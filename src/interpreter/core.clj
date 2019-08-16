(ns interpreter.core
  (:require [clojure.pprint]
            [clojure.string :as str]
            [instaparse.core :as insta]
            [slingshot.slingshot :refer [throw+]])
  (:refer-clojure :exclude [eval]))

(defn trace
  ([x] (trace identity x))
  ([f x]
   (clojure.pprint/pprint (f x))
   x))

(def clojure-instaparser
  (->> ["<FORMS> = <WHITESPACE*>"
        "        | FORM (<WHITESPACE> FORM)*"
        "SEXP = <'('> FORMS <')'>"
        "<FORM> = <WHITESPACE*> (SEXP | NUMBER | TOKEN | KEYWORD) <WHITESPACE*>"
        "NUMBER = #'\\d+'"
        "TOKEN = WORD"
        "<WORD> = #'[a-zA-Z-_+*><=]+'"
        "WHITESPACE = #'\\s'"
        "KEYWORD = <':'> WORD"]
       (str/join "\n")
       insta/parser))

(defn parse-clojure [t]
  (let [result (insta/parse clojure-instaparser t)]
    (if (insta/failure? result)
      (throw+ result)
      result)))

(comment
  (insta/parses clojure-instaparser "nil")
  (insta/parses clojure-instaparser "()")
  (insta/parses clojure-instaparser " 1")
  (insta/parses clojure-instaparser "( )")
  (insta/parses clojure-instaparser "(\t1\n#t)")
  (insta/parses clojure-instaparser "(12)")
  (insta/parses clojure-instaparser "(1 2)")
  (insta/parse clojure-instaparser "() ()")
  (insta/parse clojure-instaparser ":mykeyword")
  (clojure.pprint/pprint (parse-clojure "(+ (+ 1 2) (+ 3 4))"))
  (parse-clojure "(+ 1a -nil)") ;; error because of 1a

  )

(defn hiccup->lisp [[type_ & [arg :as rest]]]
  (case type_
    :SEXP (apply list (map hiccup->lisp rest))
    :NUMBER (Integer/parseInt arg)
    :TOKEN (case arg
             "nil" nil
             "true" true
             "false" false
             (symbol arg))
    :KEYWORD (list ::keyword (str arg))))

(defn mapM
  "Maps `f` over `exps`, threading an environment `env` forwards through the
  computation. `f` takes an environment and an expression and returns a vector
  of an environment and an expression"
  [f env exps]
  (let [[env' result-vec]
        (reduce (fn [[before-env results] sub-exp]
                  (let [[after-env result] (f env sub-exp)]
                    [after-env (conj results result)]))
                [env []]
                exps)]
    [env' (apply list result-vec)]))

;; This and functions below take `evalfn` in order to allow mutual recursion
;; with eval without a dependency cycle.
(defn eval-syntax-quote [evalfn level locals globals exp]
  (if-not (seq? exp)
    [globals exp]
    (let [[f arg] exp]
      (cond
        (= f 'unquote)
        (case level
          0 (evalfn locals globals arg)
          (let [[globals' result]
                (eval-syntax-quote evalfn (dec level) locals globals arg)]
            [globals' (list 'unquote result)]))

        (= f 'syntax-quote)
        (let [[globals' result]
              (eval-syntax-quote evalfn (inc level) locals globals arg)]
          [globals' (list 'syntax-quote result)])

        :else
        (mapM (partial eval-syntax-quote evalfn level locals) globals exp)))))

(defn eval-forms
  "Eval a sequence of forms with local bindings `locals` and global bindings
  `globals`"
  [evalfn locals globals forms]
  (reduce (fn [[globals'] form]
            (evalfn locals globals' form))
          [globals]
          forms))

(defn eval-list [evalfn locals globals [f & args :as list-exp]]
  (cond
    (empty? list-exp) [globals list-exp]
    (= f 'quote) [globals (first args)]
    (= f 'unquote) (throw+ {:type :unquote-not-in-syntax-quote
                            :unquoted-expression list-exp})
    (= f 'syntax-quote) (eval-syntax-quote evalfn 0 locals globals (first args))
    (= f 'if) (let [[condition then else] args
                    [globals' result] (evalfn locals globals condition)]
                (evalfn locals globals' (if result then else)))
    (= f 'let-one) (let [[name_ expr body] args
                         [globals' result] (evalfn locals globals expr)]
                     (evalfn (assoc locals name_ result) globals' body))
    (= f 'do) (eval-forms evalfn locals globals args)
    (= f 'fn) (let [[parameter-list body] args]
                ;; Really hard to truly emulate clojure here. For example,
                ;; clojure will complain about unresolved symbol in (fn [y] z)
                ;; if z is undefined in the environment. Here, we just capture
                ;; the current environment and deal with unresolved symbols
                ;; later. This allows some funny tricks like
                ;; (def f (fn (y) z))
                ;; (def z 1)
                ;; (f nil) => 1
                [globals (list ::closure parameter-list body locals)])
    (= f 'def)
    (let [[name_ expr] args
          [globals' result] (evalfn locals globals expr)]
      [(assoc globals' name_ result) nil])

    (= f 'defmacro)
    (let [[name_ parameter-list expr] args]
      [(assoc globals
              name_
              (list ::macro parameter-list expr locals))
       nil])

    :else
    (let [[globals' fresult] (evalfn locals globals f)]
      (if-not (seq? fresult)
        (throw+ {:type :cannot-apply
                 :f fresult})
        (case (first fresult)
          ::primitive-fn
          (let [fval (second fresult)

                [globals'' evaluated-args]
                (mapM (partial evalfn locals) globals' args)]
            (apply fval (list* locals globals'' evaluated-args)))

          ::closure
          (let [[_ [arg-name] body captured-locals] fresult
                [globals'' evaluated-arg] (evalfn locals globals' (first args))

                [globals''' result]
                (evalfn (assoc captured-locals arg-name evaluated-arg)
                        globals''
                        body)]
            [globals''' result])

          ::macro
          (let [[_ [arg-name] body captured-locals] fresult

                [globals' result]
                (evalfn (assoc captured-locals arg-name (first args))
                        globals
                        body)]
            (evalfn locals globals' result)))))))

(defn eval
  [locals globals t]
  (cond
    (contains? #{nil true false} t) [globals t]
    (number? t) [globals t]
    (symbol? t) (cond (contains? locals t) [globals (locals t)]
                      (contains? globals t) [globals (globals t)]
                      :else (throw+ {:type :unable-to-resolve-symbol
                                     :symbol t
                                     :locals locals
                                     :globals globals}))
    (= (first t) ::keyword) [globals t]
    (seq? t) (eval-list eval locals globals t)))

(defmacro construct-syms [& syms]
  (into {} (for [s syms]
             `['~s
               (list ::primitive-fn
                     (fn [_# globals# & args#]
                       [globals# (apply ~s args#)]))])))

(def runtime (merge
              (construct-syms cons list first next rest
                              + - * mod rem quot
                              < > <= >= =)
              {'divide (list ::primitive-fn
                             (fn [_ globals & args] [globals (apply / args)]))
               'eval (list ::primitive-fn eval)}))

(defn interpret-clojure [program]
  (->> program
       parse-clojure
       (map hiccup->lisp)
       (eval-forms eval {} runtime)
       second))

;; Exercises:
;;
;; - Find a bug, or at least an unexpected behavior. I'm sure there are some.
;; - Functions of arity other than one
;; - Macros of arity other than one
;; - Function overloading (multiple arities)
;; - Make fn and defmacro use [] for their arguments
;; - Keywords
;; - Strings
;; - Vectors
;; - Maps
;; - Destructuring
;; - Varargs, i.e. (fn [arg & args] (body things here))
;; - Static analysis for symbol resolution (i.e. (fn [y] z) should blow up
;;   because z is undefined)
;; - Namespaces
;; - loop recur (probably really hard to even get correct, to say nothing of
;;   actually optimizing the tail call.)
;; - atoms or refs or some other state primitive
;;
;; - Write a REPL

(comment
  ;; Haha, check out these funny names!
  (let [loop* 5]
    (loop* [if 0 recur 0]
           (if (<= recur loop*)
             (recur (+ if recur) (inc recur))
             if))))
