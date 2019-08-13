(ns interpreter.core-test
  (:require [interpreter.core :as sut]
            [slingshot.test]
            [clojure.test :refer [deftest testing is]]))

(defmacro defpending [name & body]
  (let [message (str "\n" name " is pending !!")]
    `(deftest ~name (println ~message))))

(defmacro pending [name & body]
  (let [message (str "\n" name " is pending !!")]
    `(testing ~name (println ~message))))

(defn t [expected pgm]
  (is (= expected (sut/interpret-clojure pgm))))

(deftest literals
  (t true "true")
  (t false "false")
  (t nil "nil")
  (t 0 "0"))

(deftest quotes
  (t 's "(quote s)")
  (t 1 "(quote 1)")
  (pending "single-quote"
           (t (list 1 2 3) "'(1 2 3)"))
  (t ''a "(quote (quote a))")
  (t (list 1 2) "(let-one x 1
                   (let-one y 2
                     (syntax-quote
                      ((unquote x)
                       (unquote y)))))")
  (is (thrown+? [:type :unquote-not-in-syntax-quote]
                (sut/interpret-clojure "(unquote 1)")))
  (t (list 'syntax-quote (list '+ (list 'unquote 5) 1))
     "(syntax-quote (syntax-quote (+ (unquote (unquote 5)) 1)))"))

(deftest let
  (t (list 5 10)
     "(let-one x 5 (let-one y 10 (syntax-quote ((unquote x) (unquote y)))))"))

(defpending math
  (t 2 "(+ 1 1)"))