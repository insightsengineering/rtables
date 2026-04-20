# Make subset expression for a split-value pair

Make subset expression for a split-value pair

## Usage

``` r
make_subset_expr(spl, val)

# S4 method for class 'VarLevelSplit'
make_subset_expr(spl, val)

# S4 method for class 'MultiVarSplit'
make_subset_expr(spl, val)

# S4 method for class 'AnalyzeVarSplit'
make_subset_expr(spl, val)

# S4 method for class 'AnalyzeColVarSplit'
make_subset_expr(spl, val)

# S4 method for class 'VarStaticCutSplit'
make_subset_expr(spl, val)

# S4 method for class 'CumulativeCutSplit'
make_subset_expr(spl, val)

# S4 method for class 'AllSplit'
make_subset_expr(spl, val)

# S4 method for class 'expression'
make_subset_expr(spl, val)

# S4 method for class 'character'
make_subset_expr(spl, val)
```

## Arguments

- spl:

  `(Split)`\
  A split object.

- val:

  `(SplitValue or string)`\
  The value, either as a `SplitValue` object or the raw value as a
  string.

## Value

A subseting expression to be used to restrict data to a particular
column during tabulation.

## Details

If `val` is a `SplitValue` object which already contains a subsetting
expression with length `>0`, that is immediately returned. Otherwise,
the appropriate subsetting expression is constructed based on the split
type of `spl` and the value `val`.

## Note

this is occasionally useful when constructing custom splitting behavior
which may used for column splitting but generally should not be called
directly by the end user.

## Examples

``` r

spl <- VarLevelSplit("ARM", split_label = "ARM")
make_subset_expr(spl, "B: Placebo")
#> expression((!is.na(ARM) & ARM %in% "B: Placebo"))
```
