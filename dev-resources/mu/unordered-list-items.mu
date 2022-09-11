- this is a list item
  + sub item
    * nesting deeper

- new list
  ;; the escapes are purely so that they don't get *tokenized* as text formatters
  ;; this should not create two newlines, test
  \-not a list item
  \*neither is this
* but this is
