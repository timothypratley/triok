(ns triok.util-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :as pprint]
            [triok.util :as u]))

(defn example-expansions []
  (println "Example expansions:")
  (pprint/pprint
    (macroexpand-1
      '(u/function f
         "f doc"
         [x]
         [int?]
         (inc x))))
  (pprint/pprint
    (macroexpand-1
      '(u/macro async
         "doc"
         [& body]
         [u/body?]
         `(do ~@body)))))

(example-expansions)

(deftest function-test
  (u/function plus-one
    "adds one"
    [x]
    [number?]
    (inc x))
  (is (= 2 (plus-one 1)))

  (u/macro plus-two
    "adds two"
    [x]
    [number?]
    `(inc ~x))
  (is (= 3 (plus-two 1))))
