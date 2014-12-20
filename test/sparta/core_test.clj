(ns sparta.core-test
  (:require [clojure.test :refer :all]
            [sparta.core :refer :all]
            [sparta.analyzer :refer :all]))

(deftest analyzer
  (testing "emit-form"
    (is (= "8" (emit-form (analyze '8 {}))))
    (is (= "8.8" (emit-form (analyze '8.8 {}))))
    (is (= "'8'" (emit-form (analyze "8" {}))))
    (is (= "a" (emit-form (analyze 'a {}))))
    (is (= "':a'" (emit-form (analyze ':a {}))))
    (is (= "list(a, b)" (emit-form (analyze '[a b] {}))))
    (is (= "{ 1; 2; 3 }" (emit-form (analyze '(do 1 2 3) {}))))
    (is (= "(function (x, y) { 1; 2; x })" (emit-form (analyze '(fn [x y] 1 2 x) {}))))
    (is (= "(function (x) { x })" (emit-form (analyze '(fn [x] x) {}))))
    (is (= "(function (x) { x })(0)" (emit-form (analyze '(let [x 0] x) {}))))
    (is (= "1+2" (emit-form (analyze '(+ 1 2) {}))))
    (is (= "x <<- 8") (emit-form (analyze '(def x 8) {})))
    ))
