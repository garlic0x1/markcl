# Markcl markup renderer

This is a tool to convert s-expressions into markup, based on [Hiccl](https://github.com/garlic0x1/hiccl)

# Usage

Markcl exposes one macro, `render` that accepts an output and any number of s-expressions to convert into markup.

The output argument can be `nil` if you want a string, `t` if you want stdout, or any stream.  It is the same as `format`

# Example

```lisp
(markcl:render t
  `(:<>
    (:h1 "Title")
    (:paragraph "Hello world"
     (:list "list" (:italic "in") "paragraph"))
    (:bold "i am bold")))
```

A number of tags are supported, but you can add your own by defining CLOS methods

```lisp
(defmethod markcl::apply-tag (out (tag (eql :my-tag)) body)
  (dolist (form body)
    (format "my tag form~%")
    (markcl::render-form out form))
```
