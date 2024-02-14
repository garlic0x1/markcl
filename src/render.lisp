(defpackage #:markcl
  (:use :cl :alexandria-2)
  (:export :render))
(in-package :markcl)

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

  (:method (out (tag (eql :paragraph)) body)
    (dolist (form body)
      (render-form out form))
    (format out "~%~%"))

  (:method (out (tag (eql :<>)) body)
    (render-forms out body))

  (:method (out (tag (eql :list)) body)
    (apply-tag out :ul body))

  (:method (out (tag (eql :ul)) body)
    (dolist (form body)
      (format out "- ")
      (render-form out form)
      (format out "~%")))

  (:method (out (tag (eql :ol)) body)
    (let ((c 1))
      (dolist (form body)
        (format out "~a. " c)
        (render-form out form)
        (format out "~%")
        (incf c))))

  (:method (out (tag (eql :br)) body)
    (format out "~%"))

  (:method (out (tag (eql :bold)) body)
    (format out "**")
    (render-forms out body)
    (format out "**"))

  (:method (out (tag (eql :italic)) body)
    (format out "*")
    (render-forms out body)
    (format out "*"))

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
  )

(defgeneric render-form (out sxml)
  (:method (out (sxml string))
    (format out "~a" sxml))
  (:method (out (sxml list))
    (apply-tag out (car sxml) (cdr sxml))))

(defun render-forms (out forms)
  (if out
      (dolist (f forms) (render-form out f))
      (with-output-to-string (capture)
        (funcall #'render-forms capture forms))))

(defmacro render (output &body forms)
  `(render-forms ,output (list ,@forms)))
