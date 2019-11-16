# Developer's Guide to TableTree objects

## Read the Design Doc

Read the design document (`tabletree_design.md`) first. That will lay out definitions and basic object structure we'll need here.

## Data Values

Data values (what appears in non-empty "cells" of the table) live _only_ in `TableRow` objects. They cannot appear anywhere else.

They are stored in a `list` on the `TableRow` object and generally have no restrictions on their contents other than that the renderer is unable to


# Getting the Tabulation You Want

We can specify arbitrary functions in tabulation. This should allow us to get any cell contents we want provided they are they can be computed as a function of the "raw" data at tabulation time.

## Boilerplate

All examples will use the following column structure layout unless specified otherwise.

```
## we can re-use this over and over to
## generate a bunch of layouts. Pretty nice right?
collyt = NULL %>% add_colby_varlevels("ARM", "Arm") %>%
       add_colby_varlevels("SEX", "Gender")
```

## Standard Tabulation

Tabulation of data in one column is straighforward. Simply specify a function which returns either a scalar or vector to define a (single/multi valued) row or one that returns a list to define multiple rows.

If the list is named, the names are used as the row labels. if not, default row labels can also be specified in the `add_analyzed_var()` call. In the case of a non-anonymous function, subtsitute will be used to name a single row if no row names are specificed.

The variable specified is automatically selected from the subset of the data.frame corresponding to the row/column combination and passed to the tabulation function.

`fmt` takes a vector and is recycled as needed if a tabulation function generates multiple rows.

Examples:

One single valued row (mean)

```
layout = collyt %>%
       add_rowby_varlevels("RACE", "Ethnicity") %>%
       add_analyzed_var("AGE", "Age", afun = mean, fmt = "xx.xx")
```


One multi-valued row (mean and sd)

```
layout2 = collyt %>%
       add_rowby_varlevels("RACE", "Ethnicity") %>%
       add_analyzed_var("AGE", "Age", afun = function(x) c(mean = mean(x), sd = sd(x)), fmt = "xx.xx (xx.xx)")
```

Two rows (mean and median)

```
layout2 = collyt %>%
       add_rowby_varlevels("RACE", "Ethnicity") %>%
       add_analyzed_var("AGE", "Age", afun = function(x) list(mean = mean(x), median = median(x)), fmt = "xx.xx")

```

## Incorporating column or dataset totals

Tabulation functions which accept `.N_col` and/or `.N_total` arguments will be passed the column and overall observation counts, respectively.

### Examples

Proportion of observations in a column represented
by the current row. (silly but illustrates using `.N_col`)

```
layout2 = collyt %>%
       add_rowby_varlevels("RACE", "Ethnicity") %>%
       add_analyzed_var("AGE", "Age", afun = function(x, .N_col) list(prop = length(x) / .N_col), fmt = "xx.xx")
```


Columnwise proportion of total dataset (even sillier since we are ignoring the data itself, but again illustrative)

```
layout2 = collyt %>%
       add_rowby_varlevels("RACE", "Ethnicity") %>%
       add_analyzed_var("AGE", "Age", afun = function(x, .N_col, .N_total) list("column prop" = .N_col/.N_total), fmt = "xx.xx")
```

## Comparisons against "control" or "baseline" group (column)

We can declare one of the columns in our table layout the "baseline" or "control" column, by using `add_colby_varwbline()` instead of `add_colby_varlevels()`. Once this is done, we can declare comparisons against that group as part of the layout.

Setup
```
collyt2 = NULL %>% add_colby_varwbline("ARM", "Arm A", lbl = "Arm") %>%

```

### Comparing tabulation results

The standard way comparison cell values are generated is by performing a tabulation for both columns, and then passing them to the comparison function (defaults to `-` for simple differencing).

We do this by calling `add_analyzed_blinecomp`

Examples:

Differences in mean from 'baseline' column
```
layout = collyt2 %>%
       add_analyzed_blinecomp("AGE", afun = mean)
```

Ratio of means to baseline mean (note I am not saying this is a sane thing to do statistically)

```
layout = collyt2 %>%
       add_analyzed_blinecomp("AGE", afun = mean,
       compfun = function(a, b) list("ratio of means" = a/b))

```



### Comparisons based on contingency tables

For convenience we provide a helper function for comparisons which based on the 2xk table of (baseline vs column) against (levels of the variable): `add_2dtable_blinecomp()`.

When using this, we simply pass a comparsion function which accepts the table and returns what we want.

NB: We do NOT specify an analysis function here, as our comparison function does not accept 2 tabulated values, rather a single table.

```
layout = collyt2 %>%
       add_2dtable_blinecomp(var = "AGE",
       compfun = function(tab) list("1,1 value" = tab[1,1]))
```

See `tt_rsp()` in `R/tt_rsp.R` (specifically the generation of confidence intervals) for "real world" use of this convenience wrapper in practice.


### Comparisons based on full data vectors

Sometimes we may want to perform a comparison that is some complex function of both full data vectors (baseline and column).

To do this we simply use `identity` (or `function(x) x`) as our analysis function. In this case, both untabulated vectors will be passed directly to the comparison function.

NOTE: this is how the table-based comparison helper above is implemented.

Example (average ratio of the two vectors. Note unless this is pairwise data in the correct shared order this makes no sense at all statistically!!!)

```
layout = collyt2 %>%
       add_analyzed_blinecomp("AGE", afun = identity,
       compfun = function(a, b) list("mean of ratios" = mean(a/b)))


```

NB: This could occur when doing pairwise tests in certain data, though care would need to be taken to ensure identical ordering. 


## Content Row tabulation

Content rows are generally declared analogously to non-comparison data rows, except that `add_summary` or, often, the helper `add_summary_count()` instead of e.g., `add_analyzed_var()`.

The label is specified slightly differently: as a sprintf style format, where the single `%s` will be replaced with the current level at that split.

`add_summary_counts` adds observation counts and percent of column count in the form  `"xx (xx%)"`.

```
layout = collyt2 %>%
       add_rowby_varlevels("RACE", "Ethnicity", vlblvar = "ethn_lbl") %>%
       add_summary_count("RACE", lblfmt = "%s (n)")
```

The more general `add_summary()` allows us to specify a custom content function (the `cfun` argument) which can generate any desired content row(s) in the same way a tabulation function would generate data rows.

NOTE: `add_summary_counts()` is implemented as a custom content function passed to `add_summary()`.

## Column Counts

Column observation counts are not displayed by default (we could change this) but a call to `add_colcounts()` with no arguments is sufficient to add them.

Example
```
collyt3  = collyt2 %>% add_colcounts()
```



# Getting the Rendering You Want

## Labels

## Indenting

## Formating

### Format inheritence





