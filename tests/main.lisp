(defpackage random/tests/main
  (:use :cl
        :random
        :rove))
(in-package :random/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :random)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
