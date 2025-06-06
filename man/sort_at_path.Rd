% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tt_sort.R
\name{sort_at_path}
\alias{sort_at_path}
\title{Sorting a table at a specific path}
\usage{
sort_at_path(
  tt,
  path,
  scorefun,
  decreasing = NA,
  na.pos = c("omit", "last", "first"),
  .prev_path = character(),
  ...
)
}
\arguments{
\item{tt}{(\code{TableTree} or related class)\cr a \code{TableTree} object representing a populated table.}

\item{path}{(\code{character})\cr a vector path for a position within the structure of a \code{TableTree}. Each element
represents a subsequent choice amongst the children of the previous choice.}

\item{scorefun}{(\code{function})\cr scoring function. Should accept the type of children directly under the position
at \code{path} (either \code{VTableTree}, \code{VTableRow}, or \code{VTableNodeInfo}, which covers both) and return a numeric value
to be sorted.}

\item{decreasing}{(\code{flag})\cr whether the scores generated by \code{scorefun} should be sorted in decreasing order. If
unset (the default of \code{NA}), it is set to \code{TRUE} if the generated scores are numeric and \code{FALSE} if they are
characters.}

\item{na.pos}{(\code{string})\cr what should be done with children (sub-trees/rows) with \code{NA} scores. Defaults to
\code{"omit"}, which removes them. Other allowed values are \code{"last"}  and \code{"first"}, which indicate where \code{NA} scores
should be placed in the order.}

\item{.prev_path}{(\code{character})\cr internal detail, do not set manually.}

\item{...}{Additional (named) arguments that will be passed directly down to
\code{score_fun} \emph{if} it accepts them (or accepts \code{...} itself).}
}
\value{
A \code{TableTree} with the same structure as \code{tt} with the exception that the requested sorting has been done
at \code{path}.
}
\description{
Main sorting function to order the sub-structure of a \code{TableTree} at a particular path in the table tree.
}
\details{
\code{sort_at_path}, given a path, locates the (sub)table(s) described by the path (see below for handling of the \code{"*"}
wildcard). For each such subtable, it then calls \code{scorefun} on each direct child of the table, using the resulting
scores to determine their sorted order. \code{tt} is then modified to reflect each of these one or more sorting
operations.

\code{score_fun} can optionally accept \code{decreasing}, which will be passed the value passed
to \code{sort_at_path} automatically, and other arguments which can be set via \code{...}. The
first argument passed to \code{scorefun} will always be the table structure (subtable or row)
it is scoring.

In \code{path}, a leading \code{"root"} element will be ignored, regardless of whether this matches the object name (and thus
actual root path name) of \code{tt}. Including \code{"root"} in paths where it does not match the name of \code{tt} may mask deeper
misunderstandings of how valid paths within a \code{TableTree} object correspond to the layout used to originally declare
it, which we encourage users to avoid.

\code{path} can include the "wildcard" \code{"*"} as a step, which translates roughly to \emph{any} node/branching element and means
that each child at that step will be \emph{separately} sorted based on \code{scorefun} and the remaining \code{path} entries. This
can occur multiple times in a path.

A list of valid (non-wildcard) paths can be seen in the \code{path} column of the \code{data.frame} created by
\code{\link[formatters:make_row_df]{formatters::make_row_df()}} with the \code{visible_only} argument set to \code{FALSE}. It can also be inferred from the
summary given by \code{\link[=table_structure]{table_structure()}}.

Note that sorting needs a deeper understanding of table structure in \code{rtables}. Please consider reading the related
vignette
(\href{https://insightsengineering.github.io/rtables/latest-tag/articles/sorting_pruning.html}{Sorting and Pruning})
and explore table structure with useful functions like \code{\link[=table_structure]{table_structure()}} and \code{\link[=row_paths_summary]{row_paths_summary()}}. It is also
very important to understand the difference between "content" rows and "data" rows. The first one analyzes and
describes the split variable generally and is generated with \code{\link[=summarize_row_groups]{summarize_row_groups()}}, while the second one is
commonly produced by calling one of the various \code{\link[=analyze]{analyze()}} instances.

Built-in score functions are \code{\link[=cont_n_allcols]{cont_n_allcols()}} and \code{\link[=cont_n_onecol]{cont_n_onecol()}}. They are both working with content rows
(coming from \code{\link[=summarize_row_groups]{summarize_row_groups()}}) while a custom score function needs to be used on \code{DataRow}s. Here, some
useful descriptor and accessor functions (coming from related vignette):
\itemize{
\item \code{\link[=cell_values]{cell_values()}} - Retrieves a named list of a \code{TableRow} or \code{TableTree} object's values.
\item \code{\link[formatters:lab_name]{formatters::obj_name()}} - Retrieves the name of an object. Note this can differ from the label that is
displayed (if any is)  when printing.
\item \code{\link[formatters:lab_name]{formatters::obj_label()}} - Retrieves the display label of an object. Note this can differ from the name that
appears in the path.
\item \code{\link[=content_table]{content_table()}} - Retrieves a \code{TableTree} object's content table (which contains its summary rows).
\item \code{\link[=tree_children]{tree_children()}} - Retrieves a \code{TableTree} object's direct children (either subtables, rows or possibly a mix
thereof, though that should not happen in practice).
}
}
\examples{
# Creating a table to sort

# Function that gives two statistics per table-tree "leaf"
more_analysis_fnc <- function(x) {
  in_rows(
    "median" = median(x),
    "mean" = mean(x),
    .formats = "xx.x"
  )
}

# Main layout of the table
raw_lyt <- basic_table() \%>\%
  split_cols_by("ARM") \%>\%
  split_rows_by(
    "RACE",
    split_fun = drop_and_remove_levels("WHITE") # dropping WHITE levels
  ) \%>\%
  summarize_row_groups() \%>\%
  split_rows_by("STRATA1") \%>\%
  summarize_row_groups() \%>\%
  analyze("AGE", afun = more_analysis_fnc)

# Creating the table and pruning empty and NAs
tbl <- build_table(raw_lyt, DM) \%>\%
  prune_table()

# Peek at the table structure to understand how it is built
table_structure(tbl)

#  Sorting only ASIAN sub-table, or, in other words, sorting STRATA elements for
# the ASIAN group/row-split. This uses content_table() accessor function as it
# is a "ContentRow". In this case, we also base our sorting only on the second column.
sort_at_path(tbl, c("ASIAN", "STRATA1"), cont_n_onecol(2))

# Custom scoring function that is working on "DataRow"s
scorefun <- function(tt) {
  # Here we could use browser()
  sum(unlist(row_values(tt))) # Different accessor function
}
# Sorting mean and median for all the AGE leaves!
sort_at_path(tbl, c("RACE", "*", "STRATA1", "*", "AGE"), scorefun)

last_cat_scorefun <- function(x, decreasing, lastcat) {
  mycat <- obj_name(x)
  if (mycat == lastcat) {
    ifelse(isTRUE(decreasing), -Inf, Inf)
  } else {
    match(tolower(substr(mycat, 1, 1)), letters)
  }
}

lyt2 <- basic_table() \%>\%
  split_rows_by("SEX") \%>\%
  analyze("AGE")

tbl2 <- build_table(lyt2, DM)
sort_at_path(tbl2, "SEX", last_cat_scorefun, lastcat = "M")
sort_at_path(tbl2, "SEX", last_cat_scorefun, lastcat = "M", decreasing = FALSE)

}
\seealso{
\itemize{
\item Score functions \code{\link[=cont_n_allcols]{cont_n_allcols()}} and \code{\link[=cont_n_onecol]{cont_n_onecol()}}.
\item \code{\link[formatters:make_row_df]{formatters::make_row_df()}} and \code{\link[=table_structure]{table_structure()}} for pathing information.
\item \code{\link[=tt_at_path]{tt_at_path()}} to select a table's (sub)structure at a given path.
}
}
