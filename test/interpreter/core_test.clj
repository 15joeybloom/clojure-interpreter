(ns interpreter.core-test
  (:require [interpreter.core :as sut]
            [slingshot.test]
            [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]))

(defmacro defpending [name & body]
  (let [message (str "\n" name " is pending !!")]
    `(deftest ~name (println ~message))))

(defmacro pending [name & body]
  (let [message (str "\n" name " is pending !!")]
    `(testing ~name (println ~message))))

(defn t [expected pgm]
  (is (= expected (sut/interpret-clojure pgm))))

(deftest literal-tests
  (t true "true")
  (t false "false")
  (t nil "nil")
  (t 0 "0"))

(deftest quote-tests
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

(deftest eval-tests
  (t 1 "(def x 1) (eval (quote x))")
  (t 5 "(eval (eval (let-one a 4 (syntax-quote (syntax-quote (+ (unquote (unquote a)) 1))))))"))

(deftest let-tests
  (t (list 5 10)
     "(let-one x 5 (let-one y 10 (syntax-quote ((unquote x) (unquote y)))))")
  (pending "Haha, oops! symbols escape their let bindings."
    (is (thrown+? [:type :unable-to-resolve-symbol]
                  (sut/interpret-clojure "(do (let-one x 5 x) x)")))))

(deftest do-tests
  (t 5 "(do 1 2 3 4 5)"))

(deftest if-tests
  (t 8 "(if true 8 9)")
  (t 9 "(if false 8 9)")
  (t 1 "(if true (def x 1) (def y 1)) x")
  (is (thrown+? [:type :unable-to-resolve-symbol]
                (sut/interpret-clojure "(if false (def x 1) (def y 1)) x"))))

(deftest math-tests
  (t 2 "(+ 1 1)")
  (t 2 "(- 3 1)")
  (t -1 "(- 1)")
  (t 60 "(* 3 4 5)")
  (t 5 "(divide 80 16)")
  (t -1 "(quot (- 5) 3)")
  (t 1 "(mod (- 5) 3)")
  (t -2 "(rem (- 5) 3)")
  (t true "(= 1 1)")
  (t false "(= 1 2)")
  (t true "(< 1 2)")
  (t false "(> 1 2)")
  (t true "(<= 1 1)")
  (t true "(>= 1 1)"))

(deftest fn-tests
  (t 2 "((fn (x) x) 2)")
  (t 3 "((fn (x) 3) 2)")
  (t '(2 2) "(let-one x 1 ((fn (x) (list x x)) 2))")
  (t 7 "(let-one y 5 ((fn (x) (+ x y)) 2))")
  (t 7 "((let-one y 5 (fn (x) (+ x y))) 2)")
  (t '(1 2 3) "(let-one f (fn (x) (fn (y) (fn (z) (list x y z))))
                 (((f 1) 2) 3))")
  (is (thrown+? [:type :unable-to-resolve-symbol
                 :symbol 'x]
                (sut/interpret-clojure "(let-one f (fn (x) x)
                                          (do (f 1)
                                              x))")))
  (pending "A not-so-anonymous function."
    (t 120 "((fn fact (n) (if (> n 1) (* n (fact (- n 1))) 1)) 5)")))

(deftest def-tests
  (t 2 "(def x 2) x")
  (t 3 "(do (def x 1) (def y 2) (+ x y))")
  (t 120 "(def fact (fn (f) (fn (n) (if (> n 1) (* n ((f f) (- n 1))) 1)))) ((fact fact) 5)")
  (t 120 (str/join " "
                   ['(def y (fn (f) ((fn (x) (f (fn (arg) ((x x) arg))))
                                     (fn (x) (f (fn (arg) ((x x) arg)))))))
                    '(def fact (y (fn (f) (fn (n) (if (> n 1) (* n (f (- n 1))) 1)))))
                    '(fact 5)]))
  (pending "weird"
    ;; This is unlike clojure. Clojure would complain about unable to resolve
    ;; symbol y. But in our little toy lisp here, we can define y after f so
    ;; long as we haven't called f yet.
    (t 3 "(def f (fn (x) y)) (def y 3) (f 1)")))

(deftest defmacro-tests
  (t 1 "(defmacro a (x) 1) (a)")
  (let [when-macro (str '(defmacro when (args)
                           (syntax-quote (if (unquote (first args))
                                           (unquote (first (rest args)))
                                           nil))))]
    (t 7 (str/join "\n" [when-macro
                         "(let-one x 4 (when ((> x 3) 7)))"]))
    (t nil (str/join "\n" [when-macro
                           "(let-one x 3 (when ((> x 3) 7)))"]))
    (let [defs
          (str/join "\n"
                    [when-macro
                     '(defmacro cond (clauses)
                        (when (clauses
                               (list (quote if) (first clauses)
                                     (if (next clauses)
                                       (first (next clauses))
                                       nil)
                                     (list (quote cond)
                                           (next (next clauses)))))))])]
      (t 1 (str defs " (cond (true 1 true 2))"))
      (t 2 (str defs " (cond (false 1 true 2))"))
      (t nil (str defs " (cond (false 1 false 2))")))))

(comment
  (def y (fn [f] ((fn [x] (f (fn [arg] ((x x) arg))))
                  (fn [x] (f (fn [arg] ((x x) arg)))))))
  (def fact (y (fn [f] (fn [n] (if (> n 1) (* n (f (- n 1))) 1)))))

  (fact 5)
  )
