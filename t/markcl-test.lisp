(defpackage #:markcl-test
  (:use :cl :alexandria :fiveam)
  (:import-from :serapeum :dict))
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
  ;; test custom tag
  (defmethod markcl::apply-tag (out (tag (eql :lowercase-str)) body)
    (format out "~(~a~)" (car body)))

  (is (equal
       "lower"
       (markcl:render nil '(:lowercase-str "LOWER"))))

  ;; test custom render object
  (defmethod markcl::render-form (out (sxml hash-table))
    (markcl:render out
      `(:<>
        (:thead "key" "value")
        ,@(mapcar
           (lambda (k)
             `(:tr ,k ,(gethash k sxml)))
           (hash-table-keys sxml)))))

  (is (equal
       "
| key | value |
| :---: | :---: |
| k2 | v2 |
| k1 | v1 |
"
       (markcl:render nil (dict :k1 "v1" :k2 "v2")))))

;; ----------------------------------------------------------------------------
(test :extract-attrs
  (multiple-value-bind (attrs children)
      (markcl::extract-attrs '(:k "v" :k2 "v2" "body" :body))
    (is (= 2 (length children)))
    (is (equal "v" (assoc-value attrs :k)))
    (is (equal "v2" (assoc-value attrs :k2)))))
