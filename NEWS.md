


## rtable 0.0.2

* Redesign: `rtable` has now `header` argument instead of `col.names`. A
`header` can be created with `rheader` and is a collection of `rrow`s. If
`header` is set to `c("A", "B")` then `rtable` will create the `rheader` with a
single `rrow`  and by setting `row.name` to `NULL`.

* `format` is now by default `xx`

* `format = "xx"` calls `paste(x, collapse = ', ')`


## rtable 0.0.1

* Initial public release