(ns precog.core-test
  (:require [clojure.test :refer :all]
            [precog.core :refer :all])
  (:import (clojure.lang ExceptionInfo)))

(defmethod assert-expr 'thrown-with-info? [msg form]
  (let [expected-info (nth form 1)
        expected-msg (nth form 2)
        body (nthnext form 3)]
    `(let [report# (fn [result# actual#]
                     (do-report {:type     result#, :message ~msg,
                                 :expected '~form, :actual actual#}))
           fail# (fn [actual#] (report# :fail actual#))]
       (try ~@body
            (fail# nil)
            (catch ExceptionInfo e#
              (if (not= ~expected-msg (.getMessage e#))
                (fail# e#)
                (if (not= ~expected-info (ex-data e#))
                  (fail# e#)
                  (report# :pass e#))))))))

(deftest test-nil-argument
  (testing "nil argument"
    (letfn [(test-fn [argument] (preconditions (some? argument)) :ok)]
      (is (= :ok (test-fn :foo)))
      (is (thrown-with-info?
            '{some? #{argument}, :bad-arguments #{argument}}
            "Must not be nil: argument"
            (test-fn nil))))))

(deftest test-nil-arguments
  (testing "nil argument"
    (letfn [(test-fn [argument1 argument2]
              (preconditions
                (some? argument1)
                (some? argument2))
              :ok)]
      (is (= :ok (test-fn 1 1)))
      (is (thrown-with-info?
            '{some? #{argument1}, :bad-arguments #{argument1}}
            "Must not be nil: argument1"
            (test-fn nil 1)))
      (is (thrown-with-info?
            '{some? #{argument1, argument2}, :bad-arguments #{argument1, argument2}}
            "Must not be nil: argument1, argument2"
            (test-fn nil nil))))))

(deftest test-number-arguments
  (testing "number precondition"
    (letfn [(test-fn [x y]
              (preconditions
                (number? x)
                (number? y))
              (+ x y))]
      (is (= 3 (test-fn 1 2)))
      (is (thrown-with-info?
            '{number? #{x, y}, :bad-arguments #{x, y}}
            "Must be a number: x, y"
            (test-fn nil nil))))))

(deftest test-first-failure-reported
  (testing "Only report first failure for an argument"
    (letfn [(test-fn [x]
              (preconditions
                (some? x)
                (number? x)))]
      (is (thrown-with-info?
            '{some? #{x}, :bad-arguments #{x}}
            "Must not be nil: x"
            (test-fn nil))))))

(deftest test-string-arguments
  (testing "string precondition"
    (letfn [(test-fn [x]
              (preconditions
                (string? x)))]
      (is (thrown-with-info?
            '{string? #{x}, :bad-arguments #{x}}
            "Must be a string: x"
            (test-fn nil))))))

