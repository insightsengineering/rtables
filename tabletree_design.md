


# Layouts

Layouts are declared /pre-data/ as the structure the table and
associated aggregation will have once it is applied to data.

This allows us to re-use layouts for different datasets (that have
the same relevant structure) and is a core design choice.

## Building up Layouts

Layouts define both the column- and row-based nesting structure
and are built up by piping together a declarative "grammar of table structure".

The core design here is that adding more structure will add nesting by default,
unless adding structure where nesting does not make sense (e.g., "total" or "all" column)
or the user indicates otherwise.

So if

`NULL %>% add_colby_varlevels("ARM")`

results in a table like 

```
       A    B
Value  5    8
```

Then

`NULL %>% add_colby_varlevels("ARM") %>% add_colby_varlevels("SEX")`

would give us something like
```
          A      B
        M  F    M  F
Value   1  5    3  3
```

This is intended to give users/developers a very convenient, intuitive
syntax for building up complex table structures using basic components.

We can always bypass the default if nesting is not what is desired, for
example

`NULL %>% add_colby_varlevels("ARM") %>% add_colby_varlevels("SEX", newtoplev = TRUE)`

Would give us a column structure like

```
       A   B   M   F
Value  5   6   4   7
```

If the user did ever want that (this type of multiple parallel top-level
sub-roots structure will be much more common in row space than column
space by my estimation).


# The Object Model
## Core Pieces - Splits

Table structure is modelled via declaring "splits", formally
modeled by the `Split` S4 virtual class and its non-virtual
subclasses.

At its core, a Split declares a piece of how the final set of
rows or columns of a table will be generated.

`Split` objects are declarative; we don't require data to
construct them. This means that they cannot know their set
of children when built and thus are pieces of blueprints
for a table structure, rather than a structure themselves.

`Split` objects can be placed together, predata to form
`SplitVector` objects (just a list of `Split`s which define
a nested (sub) structure in row or column space.

### Non-comparison Split Types
- `VarLevelSplit` - Will generate children for each subset of a categorical variable
- `AllSplit` - Will generate one child containing the full set of data (at the current level of nesting)
- `RootSplit` - An AllSplit that is explictly the root of the full row/column structure for the table
- `MultiVarSplit` - Will generate a child for each specified variable
- `VarStaticCutSplit` - Will generate children based on 'cutting' the values of a specified variable based on pre-specified points
- `VarDynCutSplit` - Will generate children based on 'cutting' the values of a specified variable via a function at aggregation time
- `AnalyzeVarSplit` - Declares that splitting is done and aggregation should occur at this stage by applying the specified function to the values of the specified variable
- `SubsetSplit` - Will generate one child based on inclusion in a subset (indicated by values of a logical variable). Additional child/children will be added for at least one of: Non-inclusion, all observations.

### Comparison Splits

Comparison splits declare that two intermediate aggregation values should be generated and then computed on together to generate the value of a cell.

E.g,

`
NULL %>% add_colby_blinecomp(var = "visit",
     	 	             baseline = "BASE",
			     comparison = '-',
			     rawvalcol = TRUE) %>%
      add_analyzed_var("BP", "Blood pressure)
`

Would give us something like

```
      	        BASE    VISIT1     (VISIT1 - BASE)    VISIT2     (VISIT2 - BASE)
Blood pressure  80      83          3                 98         15
```

*NB* These are subtractions of the aggregate values, NOT aggregates of the patient
     level differences. This honestly may not be very useful... 

XXX I have an email in to Adrian about whether this is even something we want. If we want
    aggregation of patient level comparisons and the comparison is already calculated
    this is way easier
      


## Populated Table Structure

A table tree is a nested structure of `TableTree` objects, each of which has a `content` `ElementaryTable` and a set of children (which are themselves either `TableTree` or `TableRow` objects). An ElemntaryTable is a table structure with no content slot and whose children must be rows (not nested tables).

Position (on either of the table's axes) is tracked by a `TreePos` (or subclass) object. A `TreePos` contains a list of Splits and a parallel list of `SplitValue` objects which define the child "selected" at each split to get to the current position. Each subtree and row within the full tree is aware of its position (in row space).



## Pre-Data


The table structure is declared prior to data being present (and thus can be re-used). 

This means that in the pre-data stage we cannot actually build the nesting and children yet.

### Pre-data Layouts

A `PreDataTableLayouts` object is built by a layout pipe chain (discussed below)
and contains a `PreDataColLayout` and a `PreDataRowLayout`. `PreDataColLayout` and `PreDataRowLayout` objects are both lists of one or more `SplitVector` objects declaring nesting substructures. Each also contains a `RootSplit` object though that is not likely to affect anything in most cases.

# Declaring layout

Table structure is declared (pre-data) via a set ofsequential `add_colby_*`, `add_rowby_*`, `add_analyzed_var` and `add_summary_*` calls within a layout pipeline (which currently starts with NULL as an artifact).

At each stage in the pipeline, the next action occurs *at the current level of nesting* unless otherwise indicated by the user or required definitionally. 

For example

```
layout = NULL %>% add_colby_varlevels("ARM", "Arm") %>%
       add_colby_varlevels("SEX", "Gender") %>%
       add_summary_count(lbl = "Overall (N)") %>%
       add_rowby_varlevels("RACE", "Ethnicity") %>%
       add_summary_count("RACE", lblfmt = "%s (n)") %>%
       add_analyzed_var("AGE", "Age", afun = mean, fmt = "xx.xx")
```

Will generate a layout where each arm of the trial has a column for each value of the SEX variable present (in the overall dataset, not in that arm). For row structure, it will have a top level `content` row wiith overall count, then for each level of RACE, it will have a sub count, then an `analysis` of AGE (the mean) within that ethnicity. 
```
> layout
A Pre-data Table Layout

Column-Split Structure:
ARM (lvls) -> SEX (lvls) 

Row-Split Structure:
RACE (lvls) -> AGE (** analyzed var **) 
```

Note this layout is then usable with any dataset that has the `ARM`, `SEX`, `RACE`, and `AGE` variables present.

## Variable columns

In some cases, the variable to be ultimately analyzed is most naturally defined on a column, not a row basis. 

Currently the way that row structure and column structure are handled aren't fully symmetric. As such, my current plan is to do this via something along the lines of

```
layout = NULL %>% add_colby_varlevels("ARM", "Arm") %>%
       add_colby_multivar(c("value", "pctdiff")) %>%
       add_rowby_varlevels("RACE", "ethnicity") %>%
       add_analyzed_colvars(afun = mean)
```

The add_analyzed_colvars here indicates that the variables whose data are
ultimately processed by afun are specified at the highest-depth level of nesting
in the column structure, rather than at the row level. 

We may want to change `add_analyzed_var` to `add_analyzed_rowvar` if we go this route
to ensure clarity

Open question: does it ever make sense to have columns that specify a variable AND 
analysis variables specified at the row level? I think these would always be in conflict
but need to think a bit harder first.

Note: we also probably want to change the name of `add_colby_multivar` to indicate that it is 
specifying an analysis. And we need to think about whether we would want (need) the ability to specify the analysis function on the column(s) instead of on rows.

