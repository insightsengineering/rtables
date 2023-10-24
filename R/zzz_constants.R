# nocov start

## Empty default objects to avoid repeated calls
.onLoad <- function(libname, pkgname) {
  ns <- asNamespace(pkgname)
  delayedAssign("EmptyColInfo", InstantiatedColumnInfo(),
    assign.env = ns
  )
  delayedAssign("EmptyElTable", ElementaryTable(),
    assign.env = ns
  )
  delayedAssign("empty_table", rtable(" "),
    assign.env = ns
  )
  delayedAssign("EmptyRootSplit", RootSplit(),
    assign.env = ns
  ) ## is this used?
  delayedAssign("EmptyAllSplit", AllSplit(),
    assign.env = ns
  ) ## is this used?
  namespaceExport(ns, c("EmptyColInfo", "EmptyElTable"))
}

#' Empty table, column, split objects
#' @name EmptyColInfo
#' @aliases EmptyElTable EmptyRootSplit EmptyAllSplit
#' @description Empty objects of various types to compare against efficiently.
NULL

# nocov end
