(ns sparta.core-test
  (:require [clojure.test :refer :all]
            [sparta.core :refer :all]
            [sparta.analyzer :refer :all]))

(deftest analyzer
  (testing "emit-form"
    (is (= "8; " (emit-form (analyze '8 {}))))
    (is (= "8.8; " (emit-form (analyze '8.8 {}))))
    (is (= "'8'; " (emit-form (analyze "8" {}))))
    (is (= "a; " (emit-form (analyze 'a {}))))
    (is (= "':a'; " (emit-form (analyze ':a {}))))
    (is (= "list(a, b); " (emit-form (analyze '[a b] {}))))
    (is (= "{ 1; 2; 3 }; " (emit-form (analyze '(do 1 2 3) {}))))
    (is (= "{ f <- function (x, y) { 1; 2; x }; environment(f) <- .ns; f }; "
           (emit-form (analyze '(fn [x y] 1 2 x) {}))))
    (is (= "{ f <- function (x) { x }; environment(f) <- .ns; f }; "
           (emit-form (analyze '(fn [x] x) {}))))
    (is (= "{ x <- 0; { x } }; " (emit-form (analyze '(let [x 0] x) {}))))
    (is (= "{ x <- 1; y <- 8; { x+y } }; " (emit-form (analyze '(let [x 1 y 8] (+ x y)) {}))))
    (is (= "1+2; " (emit-form (analyze '(+ 1 2) {}))))
    (is (= "x <<- 8; ") (emit-form (analyze '(def x 8) {})))
    (is (= "sqrt(8); " (emit-form (analyze '(.sqrt - 8) {}))))
    (is (= "as.environment('package:stats')$sqrt(8); "
           (emit-form (analyze '(.sqrt package:stats 8) {}))))
    (is (= "{ if (!exists('core', envir=.GlobalEnv)) assign('core', new.env(parent=emptyenv()), envir=.GlobalEnv); assign('.ns', .GlobalEnv[['core']], envir=.GlobalEnv) }; "
           (emit-form (analyze '(ns core) {}))))
    (is (= "if (a) 8; " (emit-form (analyze '(if a 8) {}))))
    (is (= "if (a) 8 else 0; " (emit-form (analyze '(if a 8 0) {}))))
    ))
