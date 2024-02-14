# Markcl markdown renderer

This is a tool to convert s-expressions into markdown, based on [Hiccl](https://github.com/garlic0x1/hiccl)

# Usage

Markcl exposes one macro, `render`, that accepts an output and any number of s-expressions to convert into markdown.

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

`render` can take multiple forms, I just used <> (nil tag) above to avoid superfluous quoting.

```lisp
(markcl:render nil
  '(:h1 "title")
  '(:p "content"))
```

```
"# title

content

"
```

# Tags

This might not be an exhaustive list, but the following tags are supported:

- `h1`-`h6`
- `p`
- `paragraph` (alias to `p`)
- `<>` (nil tag)
- `ul`
- `ol`
- `list` (alias to `ul`)
- `br`
- `b`
- `i`
- `bold` (alias to `b`)
- `italic` (alias to `i`)
- `bold-italic`
- `blockquote`
- `code`
- `code-block`
- `hr`
- `url`
- `a`
- `link` (alias to `a`)

Unknown tags will throw a warning and just render the body, this makes it easy if you want to copy your HTML rendering code.

# Attributes

Some tags, like `code-block` and `a`, accept attributes.  This works the same as in Hiccl, basically looking for keyword arguments after the tag.

```lisp
(render t
  '(:a :href "https://example.com" "clickme")
  '(:code-block :lang "lisp" "(+ 1 2)"))
```

````
[clickme](https://example.com)
```lisp
(+ 1 2)
```
````

# Extension

If there is a tag you want to add, you can register a method like this:

```lisp
(defmethod markcl::apply-tag (out (tag (eql :my-tag)) body)
  (dolist (form body)
    (format out "my tag form~%")
    (markcl::render-form out form)))
```

Likewise, `render-form` is a generic function you can extend if you want it to be able to handle complex types.

# TODO

- Support nested lists (might be challenging)
