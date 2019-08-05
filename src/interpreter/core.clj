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

(defn my-eval-list [evalfn env [f & args]]
  (cond
    (= f 'if) (let [[condition then else] args
                    [env' result] (evalfn env condition)]
                (evalfn env' (if result then else)))
    (= f 'let-one) (let [[name_ expr body] args
                         [env' result] (evalfn env expr)]
                     (evalfn (assoc env' name_ result) body))
    (= f 'do) (let [[stmt expr] args
                    [env'] (evalfn env stmt)]
                (evalfn env' expr))))

(defn my-eval
  ([t] (second (my-eval {} t)))
  ([env t]
   (cond
     (#{nil true false} t) [env t]
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

(interpret-clojure "(if true 2 3)")
(interpret-clojure "(let-one x 3 (let-one x 4 x))")


;; Exercises:

;; - Keywords
;; - Strings
;; - Vectors
;; - Maps
;; - Destructuring
