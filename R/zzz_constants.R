## Empty default objects to avoid repeated calls
.onLoad <- function(libname, pkgname) {
    ns <- asNamespace(pkgname)
    delayedAssign("EmptyColInfo", InstantiatedColumnInfo(),
                  assign.env = ns)
    delayedAssign("EmptyElTable", ElementaryTable(),
                  assign.env = ns)
    delayedAssign("EmptyRootSplit", RootSplit(),
                  assign.env = ns) ## is this used?
    delayedAssign("EmptyAllSplit", AllSplit(),
                  assign.env = ns) ## is this used?
    namespaceExport(ns, c("EmptyColInfo", "EmptyElTable"))
}

