# Markcl markdown renderer

This is a tool to convert s-expressions into markdown, based on [Hiccl](https://github.com/garlic0x1/hiccl)

# Usage

Markcl exposes one macro, `render` that accepts an output and any number of s-expressions to convert into markdown.

The output argument can be `nil` if you want a string, `t` if you want stdout, or any stream.  It is the same as `format`

# Example

```lisp
(markcl:render t
  `(:<>
    (:h1 "Title")
    (:paragraph "Hello " (:bold "world"))
    (:list "list" (:italic "of") (:bold-italic "items"))
    (:br)
    (:blockquote "be me")
    (:h6 "small header")))
```

```
# Title

Hello **world**

- list
- *of*
- ***items***

> be me
###### small header

```

A number of tags are supported, but you can add your own by defining CLOS methods

```lisp
(defmethod markcl::apply-tag (out (tag (eql :my-tag)) body)
  (dolist (form body)
    (format "my tag form~%")
    (markcl::render-form out form))
```
