# Compare two rtables

Prints a matrix where `.` means cell matches, `X` means cell does not
match, `+` cell (row) is missing, and `-` cell (row) should not be
there. If `structure` is set to `TRUE`, `C` indicates column-structure
mismatch, `R` indicates row-structure mismatch, and `S` indicates
mismatch in both row and column structure.

## Usage

``` r
compare_rtables(
  object,
  expected,
  tol = 0.1,
  comp.attr = TRUE,
  structure = FALSE
)
```

## Arguments

- object:

  (`VTableTree`)  
  `rtable` to test.

- expected:

  (`VTableTree`)  
  expected `rtable`.

- tol:

  (`numeric(1)`)  
  tolerance.

- comp.attr:

  (`flag`)  
  whether to compare cell formats. Other attributes are silently
  ignored.

- structure:

  (`flag`)  
  whether structures (in the form of column and row paths to cells)
  should be compared. Currently defaults to `FALSE`, but this is subject
  to change in future versions.

## Value

A matrix of class `rtables_diff` representing the differences between
`object` and `expected` as described above.

## Note

In its current form, `compare_rtables` does not take structure into
account, only row and cell position.

## Examples

``` r
t1 <- rtable(header = c("A", "B"), format = "xx", rrow("row 1", 1, 2))
t2 <- rtable(header = c("A", "B", "C"), format = "xx", rrow("row 1", 1, 2, 3))

compare_rtables(object = t1, expected = t2)
#>   1   2   3  
#> 1 "." "." "-"
#> attr(,"info")
#> [1] "column names are not the same"
#> attr(,"class")
#> [1] "rtables_diff" "matrix"       "array"       

if (interactive()) {
  Viewer(t1, t2)
}

expected <- rtable(
  header = c("ARM A\nN=100", "ARM B\nN=200"),
  format = "xx",
  rrow("row 1", 10, 15),
  rrow(),
  rrow("section title"),
  rrow("row colspan", rcell(c(.345543, .4432423), colspan = 2, format = "(xx.xx, xx.xx)"))
)

expected
#>                 ARM A    ARM B 
#>                 N=100    N=200 
#> ———————————————————————————————
#> row 1             10       15  
#>                                
#> section title                  
#> row colspan      (0.35, 0.44)  

object <- rtable(
  header = c("ARM A\nN=100", "ARM B\nN=200"),
  format = "xx",
  rrow("row 1", 10, 15),
  rrow("section title"),
  rrow("row colspan", rcell(c(.345543, .4432423), colspan = 2, format = "(xx.xx, xx.xx)"))
)

compare_rtables(object, expected, comp.attr = FALSE)
#>   1   2  
#> 1 "." "."
#> 2 "." "."
#> 3 "X" "X"
#> 4 "-" "-"
#> attr(,"info")
#> [1] "cell attributes have not been compared"
#> [2] "row labels are not the same"           
#> attr(,"class")
#> [1] "rtables_diff" "matrix"       "array"       

object <- rtable(
  header = c("ARM A\nN=100", "ARM B\nN=200"),
  format = "xx",
  rrow("row 1", 10, 15),
  rrow(),
  rrow("section title")
)

compare_rtables(object, expected)
#>   1   2  
#> 1 "." "."
#> 2 "." "."
#> 3 "." "."
#> 4 "-" "-"
#> attr(,"info")
#> [1] "row labels are not the same"
#> attr(,"class")
#> [1] "rtables_diff" "matrix"       "array"       

object <- rtable(
  header = c("ARM A\nN=100", "ARM B\nN=200"),
  format = "xx",
  rrow("row 1", 14, 15.03),
  rrow(),
  rrow("section title"),
  rrow("row colspan", rcell(c(.345543, .4432423), colspan = 2, format = "(xx.xx, xx.xx)"))
)

compare_rtables(object, expected)
#>      [,1] [,2]
#> [1,] "X"  "." 
#> [2,] "."  "." 
#> [3,] "."  "." 
#> [4,] "X"  "X" 
#> attr(,"class")
#> [1] "rtables_diff" "matrix"       "array"       

object <- rtable(
  header = c("ARM A\nN=100", "ARM B\nN=200"),
  format = "xx",
  rrow("row 1", 10, 15),
  rrow(),
  rrow("section title"),
  rrow("row colspan", rcell(c(.345543, .4432423), colspan = 2, format = "(xx.x, xx.x)"))
)

compare_rtables(object, expected)
#>      [,1] [,2]
#> [1,] "."  "." 
#> [2,] "."  "." 
#> [3,] "."  "." 
#> [4,] "X"  "X" 
#> attr(,"class")
#> [1] "rtables_diff" "matrix"       "array"       
```
