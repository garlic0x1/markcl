(defpackage #:markcl-test
  (:use :cl :fiveam))
(in-package :markcl-test)

(def-suite :markcl
  :description "Tests for Markcl")
(in-suite :markcl)

(test :basic
  (is (equal
       "# title

paragraph

"
       (markcl:render nil
         '(:h1 "title")
         '(:p "paragraph")))))

(test :attrs
  (is (equal
       "[name](url)
```lisp
nil
```

"
       (markcl:render nil
         '(:a :href "url" "name")
         '(:code-block :lang "lisp" "nil")))))

(test :evaluation
  (is (equal
       "[hello](world)"
       (markcl:render nil
         `(:a :href ,(format nil "world") "hello")))))

(test :ordered-list
  (is (equal
       "1. one
2. two
3. three

"
       (markcl:render nil
         `(:ol "one" "two" "three")))))
