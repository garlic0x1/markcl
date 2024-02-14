(defpackage #:markcl-test
  (:use :cl :fiveam))
(in-package :markcl-test)

;; ----------------------------------------------------------------------------
(def-suite :markcl
  :description "Tests for Markcl")
(in-suite :markcl)

;; ----------------------------------------------------------------------------
(test :basic
  (is (equal
       "# title

paragraph

"
       (markcl:render nil
         '(:h1 "title")
         '(:p "paragraph")))))

;; ----------------------------------------------------------------------------
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

;; ----------------------------------------------------------------------------
(test :evaluation
  (is (equal
       "[hello](world)"
       (markcl:render nil
         `(:a :href ,(format nil "world") "hello")))))

;; ----------------------------------------------------------------------------
(test :ordered-list
  (is (equal
       "1. one
2. two
3. three

"
       (markcl:render nil
         `(:ol "one" "two" "three")))))

;; this should emit a warning, but idk how to test for it.
;; ----------------------------------------------------------------------------
(test :unknown-tags
  (is (equal
       "# hello

world"
       (markcl:render nil
         '(:<>
           (:h1 "hello")
           (:hjkl "world"))))))

;; ----------------------------------------------------------------------------
(test :extension
  (defmethod markcl::apply-tag (out (tag (eql :lowercase-str)) body)
    (format out "~(~a~)" (car body)))

  (is (equal
       "lower"
       (markcl:render nil '(:lowercase-str "LOWER")))))
