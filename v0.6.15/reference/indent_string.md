# Indent strings

Used in rtables to indent row names for the ASCII output.

## Usage

``` r
indent_string(x, indent = 0, incr = 2, including_newline = TRUE)
```

## Arguments

- x:

  (`character`)  
  a character vector.

- indent:

  (`numeric`)  
  a vector of non-negative integers of length `length(x)`.

- incr:

  (`integer(1)`)  
  a non-negative number of spaces per indent level.

- including_newline:

  (`flag`)  
  whether newlines should also be indented.

## Value

`x`, indented with left-padding with `indent * incr` white-spaces.

## Examples

``` r
indent_string("a", 0)
#> [1] "a"
indent_string("a", 1)
#> [1] "  a"
indent_string(letters[1:3], 0:2)
#> [1] "a"     "  b"   "    c"
indent_string(paste0(letters[1:3], "\n", LETTERS[1:3]), 0:2)
#> [1] "a\nA"         "  b\n  B"     "    c\n    C"
```
