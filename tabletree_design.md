


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

## Compound splits

The current design calls for layouts (in both the row and column direction)  to be lists of split vectors. Max brought up the possibility of wanting to add splits at the current level of nesting, instead of either descending or jumping back up to the top. I don't want to complicate the pre-data layout structure by making it a full tree, though.

Current plan is to support this with "compound splits" which are just splits that hold 2 or more splits that should all be applied at the given level of nesting. 

NOTE: some possible issues with labeling in display.

One question is how to declare/add compound splits

### Option 1

```
NULL %>% add_colby_varlevels("ARM", "arm") %>%
     add_colby_compound(NULL %>% add_colby_all() %>% add_colby_varlevels("SEX", "sex")) %>%
     add_analyzed_var("AGE", summary)
```

Pros: no new syntax or grammar elements

Cons: I really hate the whole nesting pipes thing.
      A lot of useless work being done and undone internally here, the inner pipe would build up a PreDataLayouts object only to extract the split vector and ignore the rest.

### Option 2

```
NULL %>% add_colby_varlevels("ARM", "arm") %>%
     add_colby_compound(AllSplit(), VarLevelSplit("SEX", "gender")) %>%
     add_analyzed_var("AGE", summary)
```

Pros: no nested pipes. 
      Clear what it does. 

Cons: Not using the same declarative style grammer as everything else. 'Manual' construction of individual split objects via constructors

### Option 3

replace `newtoplevel` argument with `nestlevel` which can take:

- `"next"` (the default), 
- `"current"` (create or add to compound split at current nesting level), or
- `"top"` ( new top level)


```
NULL %>% add_colby_varlevels("ARM", "arm") %>%
     add_colby_all() %>% add_colby_varlevels("SEX", "gender", nestlevel = "current") %>%
     add_analyzed_var("AGE", summary)
```

Pros: same ux/"formfeel" as the before
      users don't need to know about the concept of compound split objects at all

Cons: adds complexity to parameter space
      non-logical argument


# Pagination

A first pass of the pagination algorithm is working and can be seen in `tt_paginate.R`

Pagination is currently defined/modeled as a _rendering_ activity, meaning it is /not/ formally modelled in the TableTree object model. Rather, it is done transiently to a TableTree object in the process of also doing other preparation for it to be printed. There are some downsides to this but it is both much simpler than formally modelling pagination and, I think, conceptually correct. I don't think we want the TableTree objects themselves to know about pagination because other output backends (e.g., html) will either not have pagination at all or will haandle it very differently.

## Definitions

- Row - a single row in the table, can be the following types
  - Label row - a row with no data tehre to display a varaible or other label for a position in the nesting structure
  - Content row - a row within the content table at a positioni within the tree, contains summary or aggregate data for that nesting level
  - Data row - a row containing the result(s) of the tabulation (e.g., mean of AGE) for a subset of the data defined by position in the tree structure
- Header lines - The multi-row defnition of the column layout and the dividing "row" displayed between the the columns and the table rows
- Repeat Row - A label or content row which must be repeated after pagination to correctly display the position/context of the first row in the new page.
- Pagination at row - The row will be the _last_ row displayed on a given page, with a page break after  it (or in the future, after any footnotes/footer material)

Row types example with 15 lines maximum per page:
(R) indicates a repeated row
(P) indicates a pagination row

```
                 [[1]]
Header                                            ARM1                          ARM2         
Header                                      M              F              F              M   
Header           ----------------------------------------------------------------------------
Content Row      Overall (N)             (N=241)        (N=265)        (N=257)        (N=237)
Label Row        Ethnicity
Content Row        WHITE (n)             (N=123)        (N=129)        (N=133)        (N=114)
Label Row          Factor2
Content Row          Level A             (N=48)         (N=42)         (N=44)         (N=36) 
Label Row            Age Analysis
Data Row               mean               51.46          52.99          55.76          54.32 
Data Row               median             48.15          51.25          56.53          52.1  
Content Row          Level B             (N=40)         (N=43)         (N=38)         (N=49) 
Label Row            Age Analysis
Data Row               mean               55.33          54.47          53.47          55.01 
(P) Data Row           median             56.64          55.13          52.24          54.88 
                 
                 [[2]]
 Header                                            ARM1                          ARM2         
 Header                                      M              F              F              M   
 Header           ----------------------------------------------------------------------------
(R) Content Row  Overall (N)             (N=241)        (N=265)        (N=257)        (N=237)
(R) Label Row    Ethnicity
Content Row         BLACK (n)             (N=118)        (N=136)        (N=124)        (N=123)
Label Row           Factor2
Content Row           Level B             (N=47)         (N=42)         (N=46)         (N=42) 
Label Row             Age Analysis
Data Row                mean               55.84          53.93          56.35          58.52 
Data Row                median             55.29          55.58          55.84          59.9  
Content Row           Level A             (N=42)         (N=47)         (N=47)         (N=37) 
Label Row             Age Analysis
Data Row                mean               56.55          57.03          56.51          54.29 
(P) Data Row            median             56.77          57.56          56.61          54.24 
                 
                 [[3]]
Header                                       ARM1                          ARM2         
Header                                 M              F              F              M   
Header           -----------------------------------------------------------------------
Label Row        Var3 Counts
Data Row           level1           (N=124)        (N=126)        (N=131)        (N=117)
Data Row           level2           (N=117)        (N=139)        (N=126)        (N=120)
                                 

```



```
[[1]]
                                                  ARM1                          ARM2         
                                            M              F              F              M   
                 ----------------------------------------------------------------------------
CR (#1)          Overall (N)             (N=241)        (N=265)        (N=257)        (N=237)
LR (#2)          Ethnicity
CR (#3)            WHITE (n)             (N=123)        (N=129)        (N=133)        (N=114)
LR (#4)            Factor2
CR (#5)              Level A             (N=48)         (N=42)         (N=44)         (N=36) 
LR (#6)              Age Analysis
DR (#7)                mean               51.46          52.99          55.76          54.32 
(P) DR (#8)            median             48.15          51.25          56.53          52.1  
                 
[[2]]
                                  ARM1                          ARM2         
                           M              F              F              M   
                 ----------------------------------------------------------------------------
 (R) CR (#1)     Overall (N)             (N=241)        (N=265)        (N=257)        (N=237)
 (R) LR (#2)     Ethnicity
 (R) CR (#3)       WHITE (n)             (N=123)        (N=129)        (N=133)        (N=114)
 (R) LR (#4)       Factor2
 CR (#9 )            Level B             (N=40)         (N=43)         (N=38)         (N=49) 
 LR (#10)            Age Analysis
 DR (#11)              mean               55.33          54.47          53.47          55.01 
 (P) DR (#12)          median             56.64          55.13          52.24          54.88 
                 
[[3]]
                                                  ARM1                          ARM2         
                                            M              F              F              M   
                 ----------------------------------------------------------------------------
(R) CR (#1)      Overall (N)             (N=241)        (N=265)        (N=257)        (N=237)
(R) LR (#2)      Ethnicity
CR (#13)           BLACK (n)             (N=118)        (N=136)        (N=124)        (N=123)
LR (#14)           Factor2
                     Level B             (N=47)         (N=42)         (N=46)         (N=42) 
                     Age Analysis
                       mean               55.84          53.93          56.35          58.52 
                       median             55.29          55.58          55.84          59.9  
                 
[[4]]
                                 ARM1                          ARM2         
                           M              F              F              M   
----------------------------------------------------------------------------
Overall (N)             (N=241)        (N=265)        (N=257)        (N=237)
Ethnicity
  BLACK (n)             (N=118)        (N=136)        (N=124)        (N=123)
  Factor2
    Level A             (N=42)         (N=47)         (N=47)         (N=37) 
    Age Analysis
      mean               56.55          57.03          56.51          54.29 
      median             56.77          57.56          56.61          54.24 

[[5]]
                            ARM1                          ARM2         
                      M              F              F              M   
-----------------------------------------------------------------------
Var3 Counts
  level1           (N=124)        (N=126)        (N=131)        (N=117)
  level2           (N=117)        (N=139)        (N=126)        (N=120)





```

## Pagination Rules

Pagination is defined to happen at (after) a row if that row will be the _last_ row on a page, with a page break directly after it (or after any footnotes which currently are not modelled, but will be). This is mostly for convenience when coding and could be changed  if it ends up being problematic.

Pagination is performed with respect to a customizable but fixed maximum number of lines per page (`lpp`). All lines are assumed to be the same height.

For a given iteration,  pagination will occur at the latest row position (`pos`) such that the following conditions are met:

1. The header lines, repeated rows, and rows between `pos` (inclusive) and the last page break (non inclusive) together require no more than `lpp` lines
2. pos points to a Data Row (i.e., NOT a Label or Content Row)
3. _Either_ the Data Row at `pos` does not have any Data Row siblings, OR pos does not point to the first Data Row data that position in the tree structure


### The Algorithm.

The algorithm in pseudocode

```

ok_pag_position = functoin(rows, pos, lastpag, reprows, lpp, nhlines) {
     ## not content row
     !is_contentrow(rows[[pos]]) &&
     	 ## not label row
         !is_labelrow(rows[[pos]]) &&
	 ## all required lines fit on page
	 (pos - lastpag + length(reprows) + nhlines) <= lpp &&
	 ## only data row at that tree position, or not first one
	 (is_datarow(rows[[pos - 1]]) || !is_datarow(rows[[pos + 1]]))
}
 	 
get_next_paginate = function(allrows, lpp, lastpag, nhlines) 
	pos = lastpag + lpp - nhlines
	reprows = getreprows(allrows, lastpag)
	while(pos > lastpag && !ok_pag_position(allrows, pos, lastpag, reprows, lpp, nhlines)) {
	     pos = pos - 1
	}
	if(pos == lastpag) stop("unable to find valid pagination positionb between rows", lastpag, "and", lastpag+lpp)
        list(pos = pos, reprows = reprows)
}
```
 