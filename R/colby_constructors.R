## These functions will build up a "layout" object.
##  S4

## e.g.
##
## add_colby_total() %>%>
##   add_colby("colname") %>%
##   add_colby_cumulcuts("colname", cuts) %>%
##   add_colby_collapse_levs("colname",
##                           list(c("lev1a", "lev1b"),
##                                c("lev2a", "lev2b", "lev2c")) %>%


##
## add_rowby_total() %>%>
##   add_rowby("colname") %>%
##   add_rowby_cumulcuts("colname", cuts) %>%
##   add_rowby_collapse_levs("colname",
##                           list(c("lev1a", "lev1b"),
##                                c("lev2a", "lev2b", "lev2c")) 
setClass("LayoutAxisTree", representation(level ="integer", children = "list"),
         validity = function(object) all(lapply(object@children, is, "LayoutAxisNode")))

setClass("LayoutAxisNode", contains="VIRTUAL", representation(level = "integer",
                                                              label = "character", N_count = "integer"))
## these should be VIRTUAL too
setClass("LayoutAxisBranch", contains = "LayoutAxisNode", representation(children = "LayoutAxisTree"))
setClass("LayoutAxisLeaf", contains = "LayoutAxisNode", representation(subset = "logical"))
 


setGeneric("layout_children", function(x) standardGeneric("layout_children"))
setMethod("layout_children", c(x = "LayoutAxisTree"),
          function(x) x@children)

setGeneric("layout_children<-", function(x, value) standardGeneric("layout_children<-"))
setMethod("layout_children<-", c(x = "LayoutAxisTree"),
          function(x, value){
    x@children = value
    x
})

## not worth the S4 dispatch to do it "right"... probably
n_leaves = function(tree) {
    kids = layout_children(tree)
    length(unlist(lapply(kids, function(x) if(is(x, "LayoutAxisLeaf")) TRUE else n_leaves(x))))
}

empty_dominant_axis = function(layout) {
    stopifnot(is(layout, "RTablesLayout"))
    ((is(layout, "RowDominantLayout") && n_leaves(row_tree(layout)) == 0L) ||
     (is(layout, "ColDominantLayout") && n_leaves(col_tree(layout)) == 0L))

}



setGeneric("row_tree", function(x) standardGeneric("row_tree"))
setMethod("row_tree", "RTablesLayout", function(x) x@row_layout)

setGeneric("col_tree", function(x) standardGeneric("col_tree"))
setMethod("col_tree", "RTablesLayout", function(x) x@col_layout)


setGeneric("row_tree<-", function(x, value) standardGeneric("row_tree<-"))
setMethod("row_tree<-", "RTablesLayout", function(x, value) {
    x@row_layout = value
    x
})

setGeneric("col_tree<-", function(x, value) standardGeneric("col_tree<-"))
setMethod("col_tree<-", "RTablesLayout", function(x, value) {
    x@col_layout = value
    x
})

setGeneric("add_colby_subset", function(layout = NULL, x, label = "") standardGeneric("add_colby_subset"))
setMethod("add_colby_subset", c(layout = "NULL"), function(layout, x, label) {
    lyt = rtables_layout(row_dominant = FALSE)
    add_colby_subset(lyt, x, label)
})

setMethod("add_colby_subset", c(layout = "ColDominantLayout"), function(layout, x, label) {
    stopifnot(is(x, "logical"))
    leaf = LayoutColLeaf(lab = label, sub = x, n = sum(x), lev = 1L)
    tree = col_tree(layout)
    layout_children(tree) = c(layout_children(tree), leaf)
    col_tree(layout) = tree
    layout
})

setMethod("add_colby_subset", c(layout = "RowDominantLayout"), function(layout, x, label) {
    stopifnot(is(x, "logical"))
    if(empty_dominant_axis(layout))
        stop("Attempted to add colby layout data to empty row-dominant layout")
    tree = col_tree(layout)
    leaf = LayoutColLeaf(lab = label, sub = x, n = sum(x), lev = tree@level + 1L)

    layout_children(tree) = c(layout_children(tree), leaf)
    col_tree(layout) = tree
    layout
})




setGeneric("add_colby_total", function(layout = NULL, x, label = "Total") standardGeneric("add_colby_total"))
setMethod("add_colby_total", c(layout = "NULL"), function(layout, label) {
    lyt = rtables_layout(row_dominant = FALSE)
    add_colby_total(lyt, label)
})


## XXX how do we get N_count here?!?!?
setMethod("add_colby_total", c(layout = "RowDominantLayout"), function(layout, label = "Total") {
    if(empty_dominant_axis(layout))
        stop("Attempted to add colby layout data to empty row-dominant layout")
    tree = col_tree(layout)
    leaf = LayoutColLeaf(lab = label, sub = TRUE, n = NA_integer_, lev = tree@level + 1L)
 
    layout_children(tree) = c(layout_children(tree), leaf)
    col_tree(layout) = tree
    layout
})


## XXX how do we get N_count here?!?!?
setMethod("add_colby_total", c(layout = "ColDominantLayout"), function(layout, label = "Total") {
    tree = col_tree(layout)
    leaf = LayoutColLeaf(lab = label, sub = TRUE, n = NA_integer_, lev = tree@level + 1L) 
    layout_children(tree) = c(layout_children(tree), leaf)
    col_tree(layout) = tree
    layout
})



setGeneric("add_rowby_subset", function(layout = NULL, x, label) standardGeneric("add_rowby_subset"))
setMethod("add_rowby_subset", c(layout = "NULL"), function(layout, x, label) {
    lyt = rtables_layout(row_dominant = FALSE)
    add_rowby_subset(lyt, x, label)
})

setMethod("add_rowby_subset", c(layout = "RowDominantLayout"), function(layout, x, label) {
    stopifnot(is(x, "logical"))
    leaf = LayoutRowLeaf(label = label, subset = x, N_count = sum(x), level = NA_integer_)
    tree = row_tree(layout)
    layout_children(tree) = c(layout_children(tree), leaf)
    row_tree(layout) = tree
    layout
})

setMethod("add_rowby_subset", c(layout = "ColDominantLayout"), function(layout, x, label) {
    stopifnot(is(x, "logical"))
    if(empty_dominant_axis(layout))
        stop("Attempted to add rowby layout data to empty col-dominant layout")
    leaf = LayoutRowLeaf(label = label, subset = x, N_count = sum(x), level = NA_integer_)
    tree = row_tree(layout)
    layout_children(tree) = c(layout_children(tree), leaf)
    row_tree(layout) = tree
    layout
})

## playground, once done modify rtabulate_default etc

rtabulate_layout <- function(x, layout, FUN, ...,
                              format = NULL, row.name = "", indent  = 0,
                              col_wise_args = NULL) {
  
  force(FUN)
 # check_stop_col_by(col_by, col_wise_args)
  
  column_data <- if (n_leaves(col_tree(layout)) == 0L) {
    setNames(list(x), "noname(for now FIXME)")
  } else {
      ## if (length(x) != length(col_by)) stop("dimension missmatch x and col_by")

      ## not handling nesting right now at all
      leaves = layout_children(col_tree(layout))
      setNames(lapply(leaves,
                      function(leaf) x[leaf@subset]),
               sapply(leaves,
                      function(leaf) leaf@label)
               )
  }
    
  cells <- if (is.null(col_wise_args)) {
    
    lapply(column_data, FUN, ...)
    
  } else {
    
    dots <- list(...)
    args <- lapply(seq_len(nlevels(col_by)), function(i) c(dots, lapply(col_wise_args, `[[`, i)))
    
    Map(function(xi, argsi) {
      do.call(FUN, c(list(xi), argsi))
    }, column_data, args)
  }
  
  rr <- rrowl(row.name = row.name, cells, format = format, indent = indent)
  
  rtable(header = sapply(layout_children(col_tree(layout)), function(leaf) leaf@label), rr)
}

.numsinglebracket = function(x, i, j, ...) {
    
    


}

setMethod("[[", "LayoutAxisTree",
          function(x, i, j, ...) {
    
    



})



