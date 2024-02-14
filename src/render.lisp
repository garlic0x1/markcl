(defpackage #:markcl
  (:use :cl :alexandria-2)
  (:export :render))
(in-package :markcl)

;; ----------------------------------------------------------------------------
(defun extract-attrs-and-children (body)
  (let* ((attrs (loop :for (k v) :on body :by 'cddr
                      :while (keywordp k)
                      :collect (cons k v)))
         (children (nthcdr (* 2 (length attrs)) body)))
    (values attrs children)))

;; ----------------------------------------------------------------------------
(defgeneric apply-tag (out tag body)
  (:method (out (tag (eql :h1)) body)
    (format out "# ")
    (render-forms out body)
    (format out "~%~%"))

  (:method (out (tag (eql :h2)) body)
    (format out "## ")
    (render-forms out body)
    (format out "~%~%"))

  (:method (out (tag (eql :h3)) body)
    (format out "### ")
    (render-forms out body)
    (format out "~%~%"))

  (:method (out (tag (eql :h4)) body)
    (format out "#### ")
    (render-forms out body)
    (format out "~%~%"))

  (:method (out (tag (eql :h5)) body)
    (format out "##### ")
    (render-forms out body)
    (format out "~%~%"))

  (:method (out (tag (eql :h6)) body)
    (format out "###### ")
    (render-forms out body)
    (format out "~%~%"))

  (:method (out (tag (eql :p)) body)
    (render-forms out body)
    (format out "~%~%"))

  (:method (out (tag (eql :paragraph)) body)
    (apply-tag out :p body))

  (:method (out (tag (eql :<>)) body)
    (render-forms out body))

  (:method (out (tag (eql :list)) body)
    (apply-tag out :ul body))

  (:method (out (tag (eql :ul)) body)
    (dolist (form body)
      (format out "- ")
      (render-form out form)
      (format out "~%"))
    (format out "~%"))

  (:method (out (tag (eql :ol)) body)
    (let ((c 1))
      (dolist (form body)
        (format out "~a. " c)
        (render-form out form)
        (format out "~%")
        (incf c)))
    (format out "~%"))

  (:method (out (tag (eql :br)) body)
    (format out "~%"))

  (:method (out (tag (eql :b)) body)
    (format out "**")
    (render-forms out body)
    (format out "**"))

  (:method (out (tag (eql :bold)) body)
    (apply-tag out :b body))

  (:method (out (tag (eql :i)) body)
    (format out "*")
    (render-forms out body)
    (format out "*"))

  (:method (out (tag (eql :italic)) body)
    (apply-tag out :i body))

  (:method (out (tag (eql :bold-italic)) body)
    (format out "***")
    (render-forms out body)
    (format out "***"))

  (:method (out (tag (eql :blockquote)) body)
    (format out "> ")
    (render-forms out body)
    (format out "~%"))

  (:method (out (tag (eql :code)) body)
    (format out "`")
    (render-forms out body)
    (format out "`"))

  (:method (out (tag (eql :hr)) body)
    (format out "---~%~%"))

  (:method (out (tag (eql :url)) body)
    (format out "<")
    (render-forms out body)
    (format out ">"))

  (:method (out (tag (eql :a)) body)
    (multiple-value-bind (attrs children) (extract-attrs-and-children body)
      (format out "[")
      (render-forms out children)
      (format out "](")
      (render-form out (assoc-value attrs :href))
      (format out ")")))

  (:method (out (tag (eql :link)) body)
    (apply-tag out :a body))

  (:method (out (tag (eql :code-block)) body)
    (multiple-value-bind (attrs children) (extract-attrs-and-children body)
      (format out "~%```~a~%" (assoc-value attrs :lang))
      (render-forms out children)
      (format out "~%```~%~%")))

  (:method (out (tag symbol) body)
    (warn "Unknown tag: ~a" tag)
    (render-forms out body)))

;; ----------------------------------------------------------------------------
(defgeneric render-form (out sxml)
  (:method (out (sxml symbol))
    (format out "~(~a~)" sxml))

  (:method (out (sxml number))
    (format out "~a" sxml))

  (:method (out (sxml string))
    (format out "~a" sxml))

  (:method (out (sxml list))
    (apply-tag out (car sxml) (cdr sxml))))

;; ----------------------------------------------------------------------------
(defun render-forms (out forms)
  (if out
      (dolist (f forms) (render-form out f))
      (with-output-to-string (capture)
        (funcall #'render-forms capture forms))))

;; ----------------------------------------------------------------------------
(defmacro render (output &body forms)
  `(render-forms ,output (list ,@forms)))
