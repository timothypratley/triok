(ns triok.core-test
  (:require [clojure.test :refer :all]
            [triok.core :as trio]))

(deftest examples-test
  (is (trio/async-function ainc [x]
        (inc x))
      "Defining an async function.")

  (is (thrown? Exception (ainc 1))
      "Async functions may only be called in an async context.")

  (is (= 2 (trio/run ainc 1))
      "Run an async function to provide a context... you get back the result.")

  (is (trio/current-time)
      "Time as epoch ms.")

  (is (trio/expired? 100 100))

  (is (thrown? Exception (trio/await (ainc 1)))
      "Await must be called in an async context.")

  (is (trio/async-function wait-for-it []
        (trio/await ainc 1))
      "We can define an async function that calls await on another async function.")

  (is (trio/run wait-for-it)
      "Await is like run in that it gets you back the result, but unlike run await must be called in an async context.")

  (is (trio/open-cancel-scope -1 (trio/await (ainc 1)))
      "A cancel scope is... cancellable.")

  (is (trio/cancel (trio/open-cancel-scope -1 (trio/await (ainc 1))))
      "Cancelling a cancel scope cancels it.")

  (trio/async-function move-it []
    (trio/move-on-after 1000 (println "oh hi")))

  ;; TODO: should this work? or not?
  (trio/run #(trio/await move-it))

  ;; TODO:
  #_(trio/sleep-until -1)

  #_(trio/sleep-forever))
