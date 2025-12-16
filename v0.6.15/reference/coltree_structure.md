# Display column tree structure

Displays the tree structure of the columns of a table or column
structure object.

## Usage

``` r
coltree_structure(obj)
```

## Arguments

- obj:

  (`ANY`)  
  the object for the accessor to access or modify.

## Value

Nothing, called for its side effect of displaying a summary to the
terminal.

## Examples

``` r
lyt <- basic_table() %>%
  split_cols_by("ARM") %>%
  split_cols_by("STRATA1") %>%
  split_cols_by("SEX", nested = FALSE) %>%
  analyze("AGE")

tbl <- build_table(lyt, ex_adsl)
coltree_structure(tbl)
#> [root] (no pos)
#>    [ARM] (no pos)
#>      [A: Drug X] (ARM: A: Drug X)
#>        [A] (ARM: A: Drug X -> STRATA1: A)
#>        [B] (ARM: A: Drug X -> STRATA1: B)
#>        [C] (ARM: A: Drug X -> STRATA1: C)
#>      [B: Placebo] (ARM: B: Placebo)
#>        [A] (ARM: B: Placebo -> STRATA1: A)
#>        [B] (ARM: B: Placebo -> STRATA1: B)
#>        [C] (ARM: B: Placebo -> STRATA1: C)
#>      [C: Combination] (ARM: C: Combination)
#>        [A] (ARM: C: Combination -> STRATA1: A)
#>        [B] (ARM: C: Combination -> STRATA1: B)
#>        [C] (ARM: C: Combination -> STRATA1: C)
#>    [SEX] (no pos)
#>      [F] (SEX: F)
#>      [M] (SEX: M)
#>      [U] (SEX: U)
#>      [UNDIFFERENTIATED] (SEX: UNDIFFERENTIATED)
```
