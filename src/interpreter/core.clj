(ns interpreter.core
  (:require [instaparse.core :as insta]
            [clojure.string :as str]
            [slingshot.slingshot :refer [throw+]]))

(defn trace [x]
  (clojure.pprint/pprint x)
  x)

(def clojure-instaparser
  (->> ["<TOP> = <WHITESPACE*> (SEXP | NUMBER | TOKEN) <WHITESPACE*>"
        "SEXP = <'('> ESSES <')'>"
        "<ESSES> = <WHITESPACE*>"
        "        | TOP (<WHITESPACE> TOP)*"
        "NUMBER = #'\\d+'"
        "TOKEN = #'[a-zA-Z-_+]+'"
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

(defn my-eval-syntax-quote [evalfn env level exp]
  (if (list? exp)
    (let [[f arg] exp]
      (cond
        (= f 'unquote)
        (case level
          0 (evalfn env arg)
          (let [[env' result] (my-eval-syntax-quote evalfn env (dec level) arg)]
            [env' (list 'unquote result)]))

        (= f 'syntax-quote)
        (let [[env' result] (my-eval-syntax-quote evalfn env (inc level) arg)]
          [env' (list 'syntax-quote result)])

        :else
        (let [[env' result-vec]
              (reduce (fn [[before-env results] sub-exp]
                        (let [[after-env result]
                              (my-eval-syntax-quote evalfn env level sub-exp)]
                          [after-env (conj results result)]))
                      [env []]
                      exp)]
          [env' (apply list result-vec)])))
    [env exp]))

(defn my-eval-list [evalfn env [f & args :as list-exp]]
  (cond
    (empty? list-exp) list-exp
    (= f 'quote) (first args)
    (= f 'unquote) (throw+ {:type :unquote-not-in-syntax-quote
                            :unquoted-expression list-exp})
    (= f 'syntax-quote) (my-eval-syntax-quote evalfn env 0 (first args))
    (= f 'if) (let [[condition then else] args
                    [env' result] (evalfn env condition)]
                (evalfn env' (if result then else)))
    (= f 'let-one) (let [[name_ expr body] args
                         [env' result] (evalfn env expr)]
                     (evalfn (assoc env' name_ result) body))
    (= f 'do) (let [[stmt expr] args
                    [env'] (evalfn env stmt)]
                (evalfn env' expr))))

(defmacro construct-syms [& syms]
  (into {} (for [s syms] [`'~s (list :primitive-fn s)])))

(def runtime (construct-syms cons list first next rest
                             + - * / mod rem quot))

(defn my-eval
  ([t] (second (my-eval runtime t)))
  ([env t]
   (cond
     (contains? #{nil true false} t) [env t]
     (number? t) [env t]
     (symbol? t) (if (contains? env t)
                   [env (env t)]
                   (throw+ {:type :unable-to-resolve-symbol
                            :env env
                            :symbol t}))
     (list? t) (my-eval-list my-eval env t))))

;; Can't take value of a macro: #'clojure.core/let

(let [loop* 5]
  (loop* [if 0 recur 0]
    (if (<= recur loop*)
      (recur (+ if recur) (inc recur))
      if)))

(def interpret-clojure
  (comp my-eval
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
