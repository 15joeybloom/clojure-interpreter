(ns interpreter.core
  (:require [clojure.pprint]
            [clojure.string :as str]
            [instaparse.core :as insta]
            [slingshot.slingshot :refer [throw+]])
  (:refer-clojure :exclude [eval]))

(defn trace [x]
  (clojure.pprint/pprint x)
  x)

(def clojure-instaparser
  (->> ["<TOP> = <WHITESPACE*> (SEXP | NUMBER | TOKEN) <WHITESPACE*>"
        "SEXP = <'('> ESSES <')'>"
        "<ESSES> = <WHITESPACE*>"
        "        | TOP (<WHITESPACE> TOP)*"
        "NUMBER = #'\\d+'"
        "TOKEN = #'[a-zA-Z-_+*><=]+'"
        "WHITESPACE = #'\\s'"]
       (str/join "\n")
       insta/parser))

(defn parse-clojure [t]
  (let [result (insta/parse clojure-instaparser t)]
    (if (insta/failure? result)
      result
      (first result))))

(prn (insta/parses clojure-instaparser "nil"))
(prn (insta/parses clojure-instaparser "()"))
(prn (insta/parses clojure-instaparser " 1"))
(prn (insta/parses clojure-instaparser "( )"))
(prn (insta/parses clojure-instaparser "(\t1\n#t)"))
(prn (insta/parses clojure-instaparser "(12)"))
(prn (insta/parses clojure-instaparser "(1 2)"))
(type (insta/parse clojure-instaparser "() ()"))
(clojure.pprint/pprint (parse-clojure "(+ (+ 1 2) (+ 3 4))"))
(println)
(parse-clojure "(+ 1a -nil)")

(defn hiccup->lisp [[type_ & [arg :as rest]]]
  (case type_
    :SEXP (apply list (map hiccup->lisp rest))
    :NUMBER (Integer/parseInt arg)
    :TOKEN (case arg
             "nil" nil
             "true" true
             "false" false
             (symbol arg))))

(hiccup->lisp (parse-clojure "(+ 12 (inc 6) (if true 7 8))"))

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

(defn eval-syntax-quote [evalfn level env exp]
  (if-not (seq? exp)
    [env exp]
    (let [[f arg] exp]
      (cond
        (= f 'unquote)
        (case level
          0 (evalfn env arg)
          (let [[env' result] (eval-syntax-quote evalfn (dec level) env arg)]
            [env' (list 'unquote result)]))

        (= f 'syntax-quote)
        (let [[env' result] (eval-syntax-quote evalfn (inc level) env arg)]
          [env' (list 'syntax-quote result)])

        :else (mapM (partial eval-syntax-quote evalfn level) env exp)))))

(defn eval-list [evalfn env [f & args :as list-exp]]
  (cond
    (empty? list-exp) list-exp
    (= f 'quote) [env (first args)]
    (= f 'unquote) (throw+ {:type :unquote-not-in-syntax-quote
                            :unquoted-expression list-exp})
    (= f 'syntax-quote) (eval-syntax-quote evalfn 0 env (first args))
    (= f 'if) (let [[condition then else] args
                    [env' result] (evalfn env condition)]
                (evalfn env' (if result then else)))
    (= f 'let-one) (let [[name_ expr body] args
                         [env' result] (evalfn env expr)]
                     (evalfn (assoc env' name_ result) body))
    (= f 'do) (let [[stmt expr] args
                    [env'] (evalfn env stmt)]
                (evalfn env' expr))
    (= f 'fn) (let [[arg-name body] args]
                ;; Really hard to truly emulate clojure here. For example,
                ;; clojure will complain about unresolved symbol in (fn [y] z)
                ;; if z is undefined in the environment. Here, we just capture
                ;; the current environment and deal with unresolved symbols
                ;; later. This allows some funny tricks...
                [env (list ::closure arg-name body env)])
    :else (let [[env' fresult] (evalfn env f)]
            (if-not (seq? fresult)
              (throw+ {:type :cannot-apply
                       :f fresult})
              (case (first fresult)
                ::primitive-fn
                (let [fval (second fresult)
                      [env'' evaluated-args] (mapM evalfn env' args)]
                  [env'' (apply fval evaluated-args)])

                ::closure
                (let [[_ [arg-name] body captured-env] fresult
                      [env'' evaluated-arg] (evalfn env' (first args))
                      captured-env' (assoc captured-env arg-name evaluated-arg)
                      [_ result] (evalfn captured-env' body)]
                  [env'' result]))))))

(defmacro construct-syms [& syms]
  (into {} (for [s syms] `['~s (list ::primitive-fn ~s)])))

(def runtime (merge (construct-syms cons list first next rest
                                    + - * mod rem quot
                                    < > <= >= =)
                    {'divide (list ::primitive-fn /)}))

(defn eval
  ([t] (second (eval runtime t)))
  ([env t]
   (cond
     (contains? #{nil true false} t) [env t]
     (number? t) [env t]
     (symbol? t) (if (contains? env t)
                   [env (env t)]
                   (throw+ {:type :unable-to-resolve-symbol
                            :env env
                            :symbol t}))
     (seq? t) (eval-list eval env t))))

;; Can't take value of a macro: #'clojure.core/let

(let [loop* 5]
  (loop* [if 0 recur 0]
    (if (<= recur loop*)
      (recur (+ if recur) (inc recur))
      if)))

(def interpret-clojure
  (comp eval
        hiccup->lisp
        parse-clojure))

(interpret-clojure "true")
(interpret-clojure "false")
(interpret-clojure "nil")
(interpret-clojure "12")
(interpret-clojure "(if false 1 2)")
(interpret-clojure "(if true 2 3)")
(interpret-clojure "(let-one x 3 (let-one x 4 x))")
(interpret-clojure "list")
(interpret-clojure "(syntax-quote (1 2))")
(interpret-clojure "(syntax-quote (syntax-quote asdf))")
(interpret-clojure "(let-one x 3 (syntax-quote (+ (unquote x) 2)))")
(interpret-clojure "(let-one x 3 (syntax-quote (syntax-quote (+ (unquote (unquote x)) 2))))")


;; A good technique to check that environment is plumbed correctly
(let [y (atom [])]
  (+ (do (swap! y conj (quote hi)) 1)
     (do (swap! y conj (quote its)) 2)
     (do (swap! y conj (quote joey)) 3))
  @y)

;; Exercises:
;;
;; - Keywords
;; - Strings
;; - Vectors
;; - Maps
;; - Destructuring
;;
;; - Write a REPL
