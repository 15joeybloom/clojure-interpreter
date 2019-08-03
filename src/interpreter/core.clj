(ns interpreter.core
  (:require [instaparse.core :as insta]
            [clojure.string :as str]))

(def clojure-instaparser
  (->> ["<TOP> = INT | TOKEN | SEXP"
        "INT = #'\\d+'"
        "TOKEN = #'[a-zA-Z-_+]+'"
        "SEXP = <'('> ESSES <')'>"
        "<ESSES> = <WHITESPACE*> TOP (<WHITESPACE+> TOP)* <WHITESPACE*>"
        "WHITESPACE = #'\\s'"]
       (str/join "\n")
       insta/parser))

(defn parse-clojure [t]
  (let [result (insta/parse clojure-instaparser t)]
    (if (insta/failure? result)
      result
      (first result))))


(parse-clojure "(+ 1a -nil)")

(defn hiccup->lisp [[type_ & [arg :as rest]]]
  (case type_
    :SEXP (map hiccup->lisp rest)
    :INT (Integer/parseInt arg)
    :TOKEN (case arg
             "nil" nil
             "true" true
             "false" false
             (symbol arg))))

(hiccup->lisp (parse-clojure "(+ 12 (inc 6) (if true 7 8))"))

(defn my-eval [env [f :as t]]
  (cond
    (#{nil true false} t) [env t]
    (number? t) [env t]
    (symbol? t) (if (contains? env t)
                  [env (env t)]
                  (throw+ {:type :unable-to-resolve-symbol
                           :env env
                           :symbol t}))
    (list? t) (eval)))

;; Can't take value of a macro: #'clojure.core/let

(loop* [a 1
        b 2]
  body
  stuff)

(let [loop* 5]
  (loop* [if 0 recur 0]
    (if (<= loop recur)
      (recur (+ if loop) (inc recur))
      if)))


(comment
  Exercises:

  - Keywords
  - Strings
  - Vectors
  - Maps
  - Destructuring)
