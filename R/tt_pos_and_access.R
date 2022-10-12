
do_recursive_replace <- function(tab, path, incontent = FALSE, value) {## rows = NULL,
                                ## cols = NULL, value) {
    ## don't want this in the recursive function
    ## so thats why we have the do_ variant
    if(is.character(path) && length(path) > 1)
        path <- as.list(path)
    if(length(path) > 0 && path[[1]] == obj_name(tab))
        path <- path[-1]
   recursive_replace(tab, path, value) ## incontent, rows, cols,value)
}


## different cases we want to support:
## 1. Replace entire children for a particular node/position in the tree
## 2. Replace entire rows at a particular (ElementaryTable) position within the
##   tree
## 3. Replace specific cell values within a set of row x column positions within
##   an ElementaryTable at a particular position within the tree
## 3. replace entire content table at a node position
## 4. replace entire rows within the content table at a particular node position
##   in the tree
## 5. replace data cell values for specific row/col positions within the content
##   table at a particular position within the tree

## XXX This is wrong, what happens if a split (or more accurately, value)
## happens more than once in the overall tree???
recursive_replace <- function(tab, path, value) { ##incontent = FALSE, rows = NULL, cols = NULL, value) {
    if(length(path) == 0) { ## done recursing
        ## if(is.null(rows) && is.null(cols)) { ## replacing whole subtree a this position
        ##     if(incontent) {
        ##         newkid = tab
        ##         content_table(newkid) = value
        ##     } else
                newkid <- value
            ## newkid has either thee content table
            ## replaced on the old kid or is the new
            ## kid
      #  } ## else { ## rows or cols (or both)  non-null
        ##     if(incontent) {
        ##         ctab = content_table(tab)
        ##         ctab[rows, cols] = value
        ##         content_table(tab) = ctab
        ##         newkid = tab

        ##     } else {
        ##         allkids = tree_children(tab)
        ##         stopifnot(are(allkids, "TableRow"))
        ##         newkid = tab
        ##         newkid[rows, cols] = value
        ##     }
        ## }
        return(newkid)
    } else if(path[[1]] == "@content") {
        ctb <- content_table(tab)
        ctb <- recursive_replace(ctb,
                                 path = path[-1],
                                 ## rows = rows,
                                 ## cols = cols,
                                 value = value)
        content_table(tab) <- ctb
        tab
    } else {## length(path) > 1, more recursing to do
        kidel <- path[[1]]
        ## broken up for debugabiliity, could be a single complex
        ## expression
        ## for now only the last step supports selecting
        ## multiple kids
        stopifnot(length(kidel) == 1,
                  is.character(kidel) || is.factor(kidel))
        knms <- names(tree_children(tab))
        if(!(kidel %in% knms))
            stop(sprintf("position element %s not in names of next level children", kidel))
        else if (sum(kidel == knms) > 1)
            stop(sprintf("position element %s appears more than once, not currently supported", kidel))
        if(is.factor(kidel)) kidel <- levels(kidel)[kidel]
        newkid <- recursive_replace(
            tree_children(tab)[[kidel]],
            path[-1],
            ## incontent = incontent,
            ## rows = rows,
            ## cols = cols,
            value)
        tree_children(tab)[[kidel]] <- newkid
        tab
    }
}

coltree_split <- function(ctree) ctree@split

col_fnotes_at_path <- function(ctree, path, fnotes) {
    if(length(path) == 0) {
        col_fnotes_here(ctree) <- fnotes
        return(ctree)
    }

    if(identical(path[1], obj_name(coltree_split(ctree))))
        path <- path[-1]
    else
        stop(paste("Path appears invalid at step:", path[1]))

    kids <- tree_children(ctree)
    kidel <- path[[1]]
    knms <- names(kids)
    stopifnot(kidel %in% knms)
    newkid <- col_fnotes_at_path(kids[[kidel]],
                       path[-1],
                       fnotes = fnotes)
    kids[[kidel]] <- newkid
    tree_children(ctree) <- kids
    ctree
}

#' Insert Row at Path
#'
#' Insert a row into an existing table directly before or directly after an existing
#' data (i.e., non-content and non-label) row, specified by its path.
#'
#' @inheritParams gen_args
#' @param after logical(1). Should `value` be added as a row directly before (`FALSE`,
#' the default) or after (`TRUE`) the row specified by `path`.
#'
#'@export
#'@examples
#'
#' lyt <- basic_table() %>%
#'   split_rows_by("COUNTRY", split_fun = keep_split_levels(c("CHN", "USA"))) %>%
#'   analyze("AGE")
#'
#' tab <- build_table(lyt, DM)
#'
#' tab2 <- insert_row_at_path(tab, c("COUNTRY", "CHN", "AGE", "Mean"),
#'                           rrow("new row", 555))
#' tab2
#' tab2 <- insert_row_at_path(tab2, c("COUNTRY", "CHN", "AGE", "Mean"),
#'                           rrow("new row redux", 888),
#'                           after = TRUE)
#' tab2
#' @seealso DataRow rrow

setGeneric("insert_row_at_path", signature = c("tt", "value"),
           function(tt, path, value, after = FALSE) {
               standardGeneric("insert_row_at_path")
           })
#' @rdname insert_row_at_path
setMethod("insert_row_at_path", c("VTableTree", "DataRow"),
          function(tt, path, value, after = FALSE) {
    if(no_colinfo(value))
        col_info(value) <- col_info(tt)
    else
        chk_compat_cinfos(tt, value)
    ## retained for debugging
    origpath <- path # nolint
    idx_row <- tt_at_path(tt, path)
    if(!is(idx_row, "DataRow"))
        stop("path must resolve fully to a non-content data row. Insertion of ",
        "rows elsewhere in the tree is not currently supported.")

    posnm <- tail(path, 1)

    path <- head(path, -1)

    subtt <- tt_at_path(tt, path)
    kids <- tree_children(subtt)
    ind <- which(names(kids) == posnm)
    if(length(ind) != 1L) {
        ## nocov start
        stop("table children do not appear to be named correctly at this ",
        "path. This should not happen, please contact the maintainer of ",
        "rtables.")
        ## nocov end
    }
    if(after)
        ind <- ind + 1

    sq <- seq_along(kids)
    tree_children(subtt) <- c(kids[sq < ind],
                              setNames(list(value), obj_name(value)),
                              kids[sq >= ind])
    tt_at_path(tt, path) <- subtt
    tt
})
#' @rdname insert_row_at_path
setMethod("insert_row_at_path", c("VTableTree", "ANY"),
          function(tt, path, value) {
              stop("Currently only insertion of DataRow objects is supported. Got ",
                   "object of class ", class(value), ". Please use rrow() or DataRow() ",
                   "to construct your row before insertion.")
          })


#' Label at Path
#'
#' Gets or sets the label at a path
#' @inheritParams gen_args
#' @details
#'
#' If `path` resolves to a single row, the label for that row
#' is retrieved or set. If, instead, `path` resolves to a subtable,
#' the text for the row-label associated with that path is retrieved
#' or set. In the subtable case, if the label text is set to a non-NA
#' value, the labelrow will be set to visible, even if it was not before.
#' Similarly, if the label row text for a subtable is set to NA,
#' the label row will bet set to non-visible, so the row will not
#' appear at all when the table is printed.
#'
#' @note When changing the row labels for content rows, it is important to
#' path all the way to the \emph{row}. Paths ending in `"@content"` will
#' not exhibit the behavior you want, and are thus an error. See
#' \code{\link{row_paths}} for help determining the full paths to content
#' rows.
#'
#' @examples
#'
#' lyt <- basic_table() %>%
#'   split_rows_by("COUNTRY", split_fun = keep_split_levels(c("CHN", "USA"))) %>%
#'   analyze("AGE")
#'
#' tab <- build_table(lyt, DM)
#'
#' label_at_path(tab, c("COUNTRY", "CHN"))
#'
#' label_at_path(tab, c("COUNTRY", "USA")) <- "United States"
#' tab
#' @export
label_at_path <- function(tt, path) {
    obj_label(tt_at_path(tt, path))
}
#' @export
#' @rdname label_at_path
`label_at_path<-` <- function(tt, path, value) {
    if(!is(tt, "VTableTree"))
        stop("tt must be a TableTree or ElementaryTable object")
    if(is.null(value) || is.na(value))
        value <- NA_character_
    subt <- tt_at_path(tt, path)
    obj_label(subt) <- value
    tt_at_path(tt, path) <- subt
    tt
}



#' Get or set table elements at specified path
#' @inheritParams gen_args
#' @param \dots unused.
#' @export
#' @rdname ttap
setGeneric("tt_at_path", function(tt, path, ...) standardGeneric("tt_at_path"))
#' @export
#' @inheritParams tt_at_path
#' @rdname int_methods
setMethod("tt_at_path", "VTableTree",
           function(tt, path, ...) {
    stopifnot(is(path, "character"),
              length(path) > 0,
              !anyNA(path))
    if(identical(path[1], "root"))
        path <- path[-1]
    ## handle pathing that hits the root split by name
    if(identical(obj_name(tt), path[1]))
        path <- path[-1]
    cur <- tt
    curpath <- path
    while(length(curpath > 0)) {
        kids <- tree_children(cur)
        curname <- curpath[1]
        if(curname == "@content")
            cur <- content_table(cur)
        else if(curname %in% names(kids)) {
            cur <- kids[[curname]]
        } else {
            stop("Path appears invalid for this tree at step ", curname)
        }
        curpath <- curpath[-1]
    }
    cur
})

#' @export
#' @rdname ttap
setGeneric("tt_at_path<-",
           function(tt, path, ..., value) standardGeneric("tt_at_path<-"))
#' @export
#' @rdname int_methods
setMethod("tt_at_path<-", c(tt = "VTableTree", value = "VTableTree"),
          function(tt, path, ..., value) {
    do_recursive_replace(tt, path = path, value = value)

})

## this one removes the child at path from the parents list of children,
## becuase thats how lists behave.
#' @export
#' @rdname int_methods
setMethod("tt_at_path<-", c(tt = "VTableTree", value = "NULL"),
          function(tt, path, ..., value) {
    do_recursive_replace(tt, path = path, value = value)

})


#' @export
#' @rdname int_methods
setMethod("tt_at_path<-", c(tt = "VTableTree", value = "TableRow"),
          function(tt, path, ..., value) {
    stopifnot(is(tt_at_path(tt = tt, path = path), "TableRow"))
    do_recursive_replace(tt, path = path, value = value)

    ## ##i <- .path_to_pos(path = path, seq_len(nrow(tt)), tt, NROW)
    ## i <- .path_to_pos(path = path, tt = tt)

    ## replace_rows(tt, i = i, value = list(value))
})


#' retrieve and assign elements of a TableTree
#'
#' @rdname brackets
#'
#' @param x TableTree
#' @param i index
#' @param j index
#' @param drop logical(1). Should the value in the cell be returned if only one
#'   cell is selected by the combination of \code{i} and \code{j}. Defaults to
#'   \code{FALSE}
#' @param \dots Includes
#' \describe{
#' \item{\emph{keep_topleft}}{ logical(1) (\code{[} only) Should the 'top-left'
#' material for the table be retained after subsetting. Defaults to \code{NA},
#' which retains the material if all rows are included (ie subsetting was by
#' column), and drops it otherwise.}
#' \item{\emph{keep_titles}}{logical(1) Should title and non-referential footer
#' information be retained. Defaults to \code{FALSE}}
#' \item{\emph{reindex_refs}}{logical(1). Should referential footnotes be
#' re-indexed as if the resulting subset is the entire table. Defaults to
#' \code{TRUE}}
#' }
#' @param value Replacement value (list, `TableRow`, or `TableTree`)
#' @return a \code{TableTree} (or \code{ElementaryTable}) object, unless a
#'   single cell was selected with \code{drop=TRUE}, in which case the (possibly
#'   multi-valued) fully stripped raw value of the selected cell.
#' @exportMethod [<-
#'
#' @examples
#' l <- basic_table() %>%
#'    split_cols_by("ARM") %>%
#'    analyze(c("SEX", "AGE"))
#'
#' tbl <- build_table(l, DM)
#'
#' tbl
#'
#' tbl[1, ]
#' tbl[1:2, 2]
#'
#' tbl[2, 1]
#' tbl[2, 1, drop = TRUE]
#'
#' tbl[, 1]
#'
#' tbl[-2, ]
#' tbl[, -1]
#'
#' tbl[2, 1] <- rcell(999)
#' tbl[2, ] <- list(rrow("FFF", 888, 666, 777))
#' tbl[3, ] <- list(-111, -222, -333)
#' tbl
#' @aliases [,VTableTree,logical,logical,ANY-method
#' [,VTableTree,logical,ANY,ANY-method
#' [,VTableTree,logical,missing,ANY-method
#' [,VTableTree,ANY,logical,ANY-method
#' [,VTableTree,ANY,missing,ANY-method
#' [,VTableTree,ANY,character,ANY-method
#' [,VTableTree,character,ANY,ANY-method
setMethod("[<-", c("VTableTree", value = "list"),
          function(x, i, j, ...,  value) {


    nr <- nrow(x)
    if(missing(i))
        i <- seq_len(NROW(x))
    else if(is(i, "character"))
        i <- .path_to_pos(i, x)
    else
        i <- .j_to_posj(i, nr)

    if(missing(j)) {
        j <- seq_along(col_exprs(col_info(x)))
    } else if(is(j, "character")) {
        j <- .path_to_pos(j, x, cols = TRUE)
    } else {
        j <- .j_to_posj(j, ncol(x))
    }

    if(length(i) > 1 && length(j) < ncol(x))
        stop("cannot modify multiple rows in not all columns.")

    if(are(value, "TableRow"))

        value <- rep(value, length.out = length(i))
    else
        value <- rep(value, length.out = length(i) * length(j))

    counter <- 0
    ## this has access to value, i, and j by scoping
    replace_rowsbynum <- function(x, i, valifnone = NULL) {
        maxi <- max(i)
        if(counter >= maxi)
            return(valifnone)

        if(labelrow_visible(x)) {
            counter <<- counter + 1
            if(counter %in% i) {
                nxtval <- value[[1]]
                if(is(nxtval, "LabelRow")) {
                    tt_labelrow(x) <- nxtval
                } else {
                    stop("can't replace label with value of class",
                         class(nxtval))
                }
                ## we're done with this one move to
                ## the next
                value <<- value[-1]
            }
        }
        if(is(x, "TableTree") && nrow(content_table(x)) > 0) {
            ctab <- content_table(x)

            content_table(x) <- replace_rowsbynum(ctab, i)
        }
        if(counter >= maxi) { #already done
            return(x)
        }
        kids <- tree_children(x)

        if(length(kids) > 0) {
            for(pos in seq_along(kids)) {
                curkid <- kids[[pos]]
                if(is(curkid, "TableRow")) {
                    counter <<- counter + 1
                    if(counter %in% i) {
                        nxtval <- value[[1]]
                        if(is(nxtval, class(curkid))) {
                            if(no_colinfo(nxtval) &&
                               length(row_values(nxtval)) == ncol(x)) {
                                col_info(nxtval) <- col_info(x)
                            }
                            stopifnot(identical(col_info(x), col_info(nxtval)))
                            curkid <- nxtval
                            value <- value[-1]
                        } else {
                            rvs <- row_values(curkid)
                            rvs[j] <- value[seq_along(j)]
                            row_values(curkid) <- rvs
                            value <- value[-(seq_along(j))]
                        }
                        kids[[pos]] <- curkid
                    }
                } else {
                    kids[[pos]] <- replace_rowsbynum(curkid, i)
                }
                if(counter >= maxi)
                    break
            }
        }
        tree_children(x) <- kids
        x
    }
    replace_rowsbynum(x, i, ...)
})

#' @exportMethod [<-
#' @rdname brackets
setMethod("[<-", c("VTableTree", value = "CellValue"),
          function(x, i, j, ...,  value) {
    x[i = i, j = j, ...] <- list(value)
    x
})



## this is going to be hard :( :( :(

### selecting/removing columns

## we have two options here: path like we do with rows and positional
## in leaf space.

setGeneric("subset_cols",
           function(tt,
                    j,
                    newcinfo = NULL,
                    keep_topleft = TRUE,
                    keep_titles = TRUE,
                    ...) {
               standardGeneric("subset_cols")
           })

setMethod("subset_cols", c("TableTree", "numeric"),
          function(tt, j, newcinfo = NULL, keep_topleft, keep_titles, ...) {
    j <- .j_to_posj(j, ncol(tt))
    if(is.null(newcinfo)) {
        cinfo <- col_info(tt)
        newcinfo <- subset_cols(cinfo, j, keep_topleft = keep_topleft, ...)
    }
    ## topleft taken care of in creation of newcinfo
    kids <- tree_children(tt)
    newkids <- lapply(kids, subset_cols, j = j, newcinfo = newcinfo,  ...)
    cont <- content_table(tt)
    newcont <- subset_cols(cont, j, newcinfo = newcinfo,  ...)
    tt2 <- tt
    col_info(tt2) <- newcinfo
    content_table(tt2) <- newcont
    tree_children(tt2) <- newkids
    tt_labelrow(tt2) <- subset_cols(tt_labelrow(tt2), j, newcinfo,  ...)

    if(isTRUE(keep_titles)) {
        main_title(tt2) <- main_title(tt)
        subtitles(tt2) <- subtitles(tt)
        main_footer(tt2) <- main_footer(tt)
        prov_footer(tt2) <- prov_footer(tt)

    } else {
        main_title(tt2) <- ""
        subtitles(tt2) <- character()
    }
    ## if(keep_topleft)
    ##     top_left(tt2) <- top_left(tt)
    ## else
    ##     top_left(tt2) <- character()
    tt2
})

setMethod("subset_cols", c("ElementaryTable", "numeric"),
          function(tt, j, newcinfo = NULL, keep_topleft, keep_titles, ...) {
    j <- .j_to_posj(j, ncol(tt))
    if(is.null(newcinfo)) {
        cinfo <- col_info(tt)
        newcinfo <- subset_cols(cinfo, j, keep_topleft = keep_topleft,
                               keep_titles = keep_titles, ...)
    }
    ## topleft handled in creation of newcinfo
    kids <- tree_children(tt)
    newkids <- lapply(kids, subset_cols, j = j, newcinfo = newcinfo,  ...)
    tt2 <- tt
    col_info(tt2) <- newcinfo
    tree_children(tt2) <- newkids
    tt_labelrow(tt2) <- subset_cols(tt_labelrow(tt2), j, newcinfo, ...)
    if(keep_titles) {
        main_title(tt2) <- main_title(tt)
        subtitles(tt2) <- subtitles(tt)
        main_footer(tt2) <- main_footer(tt)
        prov_footer(tt2) <- prov_footer(tt)

    }
    ## if(keep_topleft)
    ##     top_left(tt2) <- top_left(tt)
    tt2
})


## small utility to transform any negative
## indices into positive ones, given j
## and total length

.j_to_posj <- function(j, n) {
    ## This will work for logicals, numerics, integers
    j <- seq_len(n)[j]
    j
}


path_collapse_sep <- "`"
escape_name_padding <- function(x) {
    ret <- gsub("._[[", "\\._\\[\\[", x, fixed = TRUE)
    ret <- gsub("]]_.", "\\]\\]_\\.", ret, fixed = TRUE)
    ret
}
path_to_regex <- function(path) {
    paste(vapply(path, function(x) {
        if(identical(x, "*"))
            paste0("[^", path_collapse_sep, "]+")
        else escape_name_padding(x)
    }, ""), collapse = path_collapse_sep)
}


.path_to_pos <- function(path, tt, distinct_ok = TRUE, cols = FALSE) {
    path <- path[!grepl("^(|root)$", path)]
    if(cols)
        rowdf <- make_col_df(tt)
    else
        rowdf <- make_row_df(tt)
    if(length(path) == 0 ||
       identical(path, "*") ||
       identical(path, "root"))
        return(seq(1, nrow(rowdf)))

    paths <- rowdf$path
    pathregex <- path_to_regex(path)
    pathstrs <- vapply(paths, paste, "",  collapse = path_collapse_sep)
    allmatchs <- grep(pathregex, pathstrs)
    if(length(allmatchs) == 0)
        stop(if(cols) "column path [" else "row path [",
             paste(path, collapse = "->"),
             "] does not appear valid for this table")

    idxdiffs <- diff(allmatchs)
    if(!distinct_ok &&
       length(idxdiffs) > 0 &&
       any(idxdiffs > 1)) {
        firstnon <- min(which(idxdiffs > 1))
        ## its firstnon here because we would want firstnon-1 but
        ## the diffs are actually shifted 1 so they cancel out
        allmatchs <- allmatchs[seq(1, firstnon)]
    }
    allmatchs
}

## fix column spans that would be invalid
## after some columns are no longer there
.fix_rowcspans <- function(rw, j) {
    cspans <- row_cspans(rw)
    nc <- sum(cspans)
    j <- .j_to_posj(j, nc)
    ## this is overly complicated
    ## we need the starting indices
    ## but the first span might not be 1, so
    ## we pad with 1 and then take off the last
    start <- cumsum(c(1, head(cspans, -1)))
    ends <- c(tail(start, -1) - 1, nc)
    res <- mapply(function(st, en) {
        sum(j >= st & j <= en)
    }, st = start, en = ends)
    res <- res[res > 0]
    stopifnot(sum(res) == length(j))
    res

}

select_cells_j <- function(cells, j) {
    if(length(j) != length(unique(j)))
        stop("duplicate column selections is not currently supported")
    spans <- vapply(cells, function(x) cell_cspan(x),
                   integer(1))
    inds <- rep(seq_along(cells), times = spans)
    selinds <- inds[j]
    retcells <- cells[selinds[!duplicated(selinds)]]
    newspans <- vapply(split(selinds, selinds),
                      length,
                      integer(1))

    mapply(function(cl, sp) {
        cell_cspan(cl) <- sp
        cl
    }, cl = retcells, sp = newspans, SIMPLIFY = FALSE)
}

setMethod("subset_cols", c("ANY", "character"),
          function(tt, j, newcinfo = NULL, keep_topleft = TRUE, ...) {
    j <- .path_to_pos(path = j, tt = tt, cols = TRUE)
    subset_cols(tt, j, newcinfo = newcinfo, keep_topleft = keep_topleft,  ...)
})

setMethod("subset_cols", c("TableRow", "numeric"),
          function(tt, j, newcinfo = NULL, keep_topleft = TRUE,  ...) {
    j <- .j_to_posj(j, ncol(tt))
    if(is.null(newcinfo)) {
        cinfo <- col_info(tt)
        newcinfo <- subset_cols(cinfo, j, keep_topleft = keep_topleft,  ...)
    }
    tt2 <- tt
    row_cells(tt2) <-  select_cells_j(row_cells(tt2), j)

    if(length(row_cspans(tt2)) > 0)
        row_cspans(tt2) <- .fix_rowcspans(tt2, j)
    col_info(tt2) <- newcinfo
    tt2
})

setMethod("subset_cols", c("LabelRow", "numeric"),
          function(tt, j, newcinfo = NULL, keep_topleft = TRUE, ...) {
    j <- .j_to_posj(j, ncol(tt))
    if(is.null(newcinfo)) {
        cinfo <- col_info(tt)
        newcinfo <- subset_cols(cinfo, j, keep_topleft = keep_topleft, ...)
    }
    col_info(tt) <- newcinfo
    tt
})


setMethod("subset_cols", c("InstantiatedColumnInfo", "numeric"),
          function(tt, j, newcinfo = NULL, keep_topleft = TRUE,  ...) {
    if(!is.null(newcinfo))
        return(newcinfo)
    j <- .j_to_posj(j, length(col_exprs(tt)))
    newctree <- subset_cols(coltree(tt), j, NULL)
    newcextra <- col_extra_args(tt)[j]
    newcsubs <- col_exprs(tt)[j]
    newcounts <- col_counts(tt)[j]
    tl <- if(keep_topleft) top_left(tt) else character()
    InstantiatedColumnInfo(treelyt = newctree,
                           csubs = newcsubs,
                           extras = newcextra,
                           cnts = newcounts,
                           dispcounts = disp_ccounts(tt),
                           countformat = colcount_format(tt),
                           topleft = tl)
})

setMethod("subset_cols", c("LayoutColTree", "numeric"),
          function(tt, j, newcinfo = NULL, ...) {
    lst <- collect_leaves(tt)
    j <- .j_to_posj(j, length(lst))

    ## j has only non-negative values from
    ## this point on
    counter <- 0
    prune_children <- function(x, j) {
        kids <- tree_children(x)
        newkids <- kids
         for(i in seq_along(newkids)) {
            if(is(newkids[[i]], "LayoutColLeaf")) {
                counter <<- counter + 1
                if(!(counter %in% j))
                    newkids[[i]] <- list() ## NULL removes the position entirely
            } else {
                newkids[[i]] <- prune_children(newkids[[i]], j)
            }
        }

        newkids <- newkids[sapply(newkids, function(thing) length(thing) > 0)]
        if(length(newkids) > 0) {
            tree_children(x) <- newkids
            x
        } else {
            list()
        }
    }
    prune_children(tt, j)
})




## label rows ARE included in the count
subset_by_rownum <- function(tt,
                             i,
                             keep_topleft = NA,
                             keep_titles = TRUE,
                             ...) {
    stopifnot(is(tt, "VTableNodeInfo"))
    counter <- 0
    nr <- nrow(tt)
    i <- .j_to_posj(i, nr)
    if(length(i) == 0) {
        ret <- TableTree(cinfo = col_info(tt))
        if(isTRUE(keep_topleft))
            top_left(ret) <- top_left(tt)
        return(ret)
    }

    prune_rowsbynum <- function(x, i, valifnone = NULL) {
        maxi <- max(i)
        if(counter > maxi)
            return(valifnone)

        if(labelrow_visible(x)) {
            counter <<- counter + 1
            if(!(counter %in% i)) {
                ## XXX this should do whatever
                ## is required to 'remove' the Label Row
                ## (currently implicit based on
                ## the value of the label but
                ## that shold really probably change)
                labelrow_visible(x) <- FALSE
             }
        }
        if(is(x, "TableTree") && nrow(content_table(x)) > 0) {
            ctab <- content_table(x)

            content_table(x) <- prune_rowsbynum(ctab, i,
                                                valifnone = ElementaryTable(cinfo = col_info(ctab), iscontent = TRUE))
        }
        kids <- tree_children(x)
        if(counter > maxi) { #already done
            kids <- list()
        } else if(length(kids) > 0) {
            for(pos in seq_along(kids)) {
                if(is(kids[[pos]], "TableRow")) {
                    counter <<- counter + 1
                    if(!(counter %in% i)) {
                        kids[[pos]] <- list()
                    }
                } else {
                    kids[[pos]] <- prune_rowsbynum(kids[[pos]], i, list())
                }
            }
            kids <- kids[sapply(kids, function(x) NROW(x) > 0)]
        }
        if(length(kids) == 0 &&
           NROW(content_table(x)) == 0 &&
           !labelrow_visible(x)) {
            return(valifnone)
        } else {
            tree_children(x) <- kids
            x
        }
        ## ## if(length(kids) == 0) {
        ## ##     if(!is(x, "TableTree"))
        ## ##         return(valifnone)
        ## ## }
        ## if(is(x, "VTableTree") && nrow(x) > 0) {
        ##     x
        ## } else {
        ##     valifnone
        ## }
    }
    ret <- prune_rowsbynum(tt, i)
    if(isTRUE(keep_topleft))
        top_left(ret) <- top_left(tt)
    if(isTRUE(keep_titles) || (isTRUE(keep_topleft) && is.na(keep_titles))) {
        ret <- .copy_titles(ret, tt)
    }

    ret
}


#' @exportMethod [
#' @rdname brackets
#' @aliases [,VTableTree,logical,logical-method
setMethod("[", c("VTableTree", "logical", "logical"),
          function(x, i, j, ..., drop = FALSE) {
    i <- .j_to_posj(i, nrow(x))
    j <- .j_to_posj(j, ncol(x))
    x[i, j, ..., drop = drop]
})

#' @exportMethod [
#' @rdname brackets
#' @aliases [,VTableTree,logical,ANY-method
setMethod("[", c("VTableTree", "logical", "ANY"),
          function(x, i, j, ..., drop = FALSE) {
    i <- .j_to_posj(i, nrow(x))
    x[i, j, ..., drop = drop]
})

#' @exportMethod [
#' @rdname brackets
#' @aliases [,VTableTree,logical,missing-method
setMethod("[", c("VTableTree", "logical", "missing"),
          function(x, i, j, ..., drop = FALSE) {
    j <- seq_len(ncol(x))
    i <- .j_to_posj(i, nrow(x))
    x[i, j, ..., drop = drop]
})

#' @exportMethod [
#' @rdname brackets
#' @aliases [,VTableTree,ANY,logical-method
setMethod("[", c("VTableTree", "ANY", "logical"),
          function(x, i, j, ..., drop = FALSE) {
    j <- .j_to_posj(j, ncol(x))
    x[i, j, ..., drop = drop]
})

#' @exportMethod [
#' @rdname brackets
#' @aliases [,VTableTree,ANY,missing-method
setMethod("[", c("VTableTree", "ANY", "missing"),
          function(x, i, j, ..., drop = FALSE) {
    j <- seq_len(ncol(x))
    x[i = i, j = j, ..., drop = drop]
})

#' @exportMethod [
#' @rdname brackets
#' @aliases [,VTableTree,missing,ANY-method

setMethod("[", c("VTableTree", "missing", "ANY"),
          function(x, i, j, ..., drop = FALSE) {
    i <- seq_len(nrow(x))
    x[i = i, j = j, ..., drop = drop]
})



#' @exportMethod [
#' @rdname brackets
#' @aliases [,VTableTree,ANY,character-method
setMethod("[", c("VTableTree", "ANY", "character"),
          function(x, i, j, ..., drop = FALSE) {
    ##j <- .colpath_to_j(j, coltree(x))
    j <- .path_to_pos(path = j, tt = x, cols = TRUE)
    x[i = i, j = j, ..., drop = drop]
})

#' @exportMethod [
#' @rdname brackets
#' @aliases [,VTableTree,character,ANY-method
setMethod("[", c("VTableTree", "character", "ANY"),
          function(x, i, j, ..., drop = FALSE) {
    ##i <- .path_to_pos(i, seq_len(nrow(x)), x, NROW)
    i <- .path_to_pos(i, x)
    x[i = i, j = j, ..., drop = drop]
})

## to avoid dispatch ambiguity. Not necessary, possibly not a good idea at all
#' @exportMethod [
#' @rdname brackets
#' @aliases [,VTableTree,character,character-method
setMethod("[", c("VTableTree", "character", "character"),
          function(x, i, j, ..., drop = FALSE) {
    ##i <- .path_to_pos(i, seq_len(nrow(x)), x, NROW)
    i <- .path_to_pos(i, x)
    ##j <- .colpath_to_j(j, coltree(x))
    j <- .path_to_pos(path = j, tt = x, cols = TRUE)
    x[i = i, j = j, ..., drop = drop]
})


#' @exportMethod [
#' @rdname brackets
#' @aliases [,VTableTree,missing,numeric-method
setMethod("[", c("VTableTree", "missing", "numeric"),
          function(x, i, j, ..., drop = FALSE) {
    i <- seq_len(nrow(x))
    x[i, j, ..., drop = drop]
})


#' @exportMethod [
#' @rdname brackets
#' @aliases [,VTableTree,numeric,numeric-method
setMethod("[", c("VTableTree", "numeric", "numeric"),
          function(x, i, j, ..., drop = FALSE) {
    ## have to do it this way because we can't add an argument since we don't
    ## own the generic declaration
    keep_topleft <- list(...)[["keep_topleft"]] ## returns NULL if not presesnt
    keep_titles <- list(...)[["keep_titles"]] %||% FALSE
    reindex_refs <- list(...)[["reindex_refs"]] %||% TRUE
    if(is.null(keep_topleft))
        keep_topleft <- NA

    nr <- nrow(x)
    nc <- ncol(x)
    i <- .j_to_posj(i, nr)
    j <- .j_to_posj(j, nc)

    ##  if(!missing(i) && length(i) < nr) {
    if(length(i) < nr) { ## already populated by .j_to_posj
        keep_topleft <- isTRUE(keep_topleft)
        x <- subset_by_rownum(x, i,
                              keep_topleft = keep_topleft,
                              keep_titles = keep_titles)
    } else {
        keep_topleft <- !identical(FALSE, keep_topleft)
    }
    ##  if(!missing(j) && length(j) < nc)
    if(length(j) < nc)
        x <- subset_cols(x, j,
                         keep_topleft = keep_topleft,
                         keep_titles = keep_titles)

    if(length(j) == 1L &&
       length(i) == 1L &&
       drop) {
        rw <- collect_leaves(x, TRUE, TRUE)[[1]]
        if(is(rw, "LabelRow"))
            x <- NULL
        else
            x <- row_values(rw)[[1]]
    }
    if(!drop) {
        if(!keep_topleft)
            top_left(x) <- character()
        if(reindex_refs)
            x <- update_ref_indexing(x)
    }
    x
})

#' @importFrom utils compareVersion

setGeneric("tail", tail)
setMethod("tail", "VTableTree",
          function(x, n = 6L, ...) {
    if(compareVersion("4.0.0", as.character(getRversion())) <= 0)
        tail.matrix(x, n, keepnums = FALSE)
    else
        tail.matrix(x, n, addrownums = FALSE)
})

setGeneric("head", head)
setMethod("head", "VTableTree",
          function(x, n = 6L, ...) {
    head.matrix(x, n)
}
)

#' Retrieve cell values by row and column path
#'
#' @rdname cell_values
#'
#' @inheritParams gen_args
#' @param rowpath character. Path in row-split space to the desired row(s). Can
#'   include \code{"@content"}.
#' @param colpath character. Path in column-split space to the desired
#'   column(s). Can include \code{"*"}.
#' @param omit_labrows logical(1). Should label rows underneath \code{rowpath}
#'   be omitted (\code{TRUE}, the default), or return empty lists of cell
#'   "values" (\code{FALSE}).
#'
#' @return for \code{cell_values}, a \emph{list} (regardless of the type of
#'   value the cells hold). if \code{rowpath} defines a path to a single row,
#'   \code{cell_values} returns the list of cell values for that row, otherwise
#'   a list of such lists, one for each row captured underneath \code{rowpath}.
#'   This occurs after subsetting to \code{colpath} has occured.
#'
#'   For \code{value_at} the "unwrapped" value of a single cell, or an error, if
#'   the combination of \code{rowpath} and \code{colpath} do not define the
#'   location of a single cell in \code{tt}.
#'
#' @note \code{cell_values} will return a single cell's value wrapped in a list.
#'   Use \code{value_at} to recieve the "bare" cell value.
#'
#' @export
#'
#' @examples
#'  l <- basic_table() %>% split_cols_by("ARM") %>%
#'    split_cols_by("SEX") %>%
#'    split_rows_by("RACE") %>%
#'    summarize_row_groups() %>%
#'    split_rows_by("STRATA1") %>%
#'    analyze("AGE")
#'
#' library(dplyr) ## for mutate
#' tbl <- build_table(l, DM %>%
#'     mutate(SEX = droplevels(SEX), RACE = droplevels(RACE)))
#'
#' row_paths_summary(tbl)
#' col_paths_summary(tbl)
#'
#' cell_values(tbl, c("RACE", "ASIAN", "STRATA1", "B"),
#'             c("ARM", "A: Drug X", "SEX", "F"))
#'
#' # it's also possible to access multiple values by being less specific
#' cell_values(tbl, c("RACE", "ASIAN", "STRATA1"),
#'             c("ARM", "A: Drug X", "SEX", "F"))
#' cell_values(tbl, c("RACE", "ASIAN"), c("ARM", "A: Drug X", "SEX", "M"))
#'
#'
#' ## any arm, male columns from the ASIAN content (ie summary) row
#' cell_values(tbl, c("RACE", "ASIAN", "@content"),
#'             c("ARM", "B: Placebo", "SEX", "M"))
#' cell_values(tbl, c("RACE", "ASIAN", "@content"),
#'             c("ARM", "*", "SEX", "M"))
#'
#' ## all columns
#' cell_values(tbl,  c("RACE", "ASIAN", "STRATA1", "B"))
#'
#' ## all columns for the Combination arm
#' cell_values(tbl,  c("RACE", "ASIAN", "STRATA1", "B"),
#'             c("ARM", "C: Combination"))
#'
#' cvlist <- cell_values(tbl, c("RACE", "ASIAN", "STRATA1", "B", "AGE", "Mean"),
#'                       c("ARM", "B: Placebo", "SEX", "M"))
#' cvnolist <- value_at(tbl,  c("RACE", "ASIAN", "STRATA1", "B", "AGE", "Mean"),
#'                      c("ARM", "B: Placebo", "SEX", "M"))
#' stopifnot(identical(cvlist[[1]], cvnolist))
setGeneric("cell_values", function(tt, rowpath = NULL, colpath = NULL, omit_labrows = TRUE)
    standardGeneric("cell_values"))
#'@rdname cell_values
#' @exportMethod cell_values
setMethod("cell_values", "VTableTree",
          function(tt, rowpath, colpath = NULL, omit_labrows = TRUE) {
    .inner_cell_value(tt, rowpath = rowpath, colpath = colpath,
                      omit_labrows = omit_labrows, value_at = FALSE)
})

#'@rdname cell_values
#' @exportMethod cell_values
setMethod("cell_values", "TableRow",
          function(tt, rowpath, colpath = NULL, omit_labrows = TRUE) {
    if(!is.null(rowpath))
       stop("cell_values on TableRow objects must have NULL rowpath")
    .inner_cell_value(tt, rowpath = rowpath, colpath = colpath,
                       omit_labrows = omit_labrows, value_at = FALSE)
})

#'@rdname cell_values
#' @exportMethod cell_values
setMethod("cell_values", "LabelRow",
          function(tt, rowpath, colpath = NULL, omit_labrows = TRUE) {
    stop("calling cell_values on LabelRow is not meaningful")
})



#'@rdname cell_values
#' @export
setGeneric("value_at", function(tt, rowpath = NULL, colpath = NULL)
    standardGeneric("value_at"))
#'@rdname cell_values
#' @exportMethod value_at
setMethod("value_at", "VTableTree",
          function(tt, rowpath, colpath = NULL) {
    .inner_cell_value(tt, rowpath = rowpath, colpath = colpath,
                      omit_labrows = FALSE, value_at = TRUE)
})

#'@rdname cell_values
#' @exportMethod value_at
setMethod("value_at", "TableRow",
          function(tt, rowpath, colpath = NULL) {
    .inner_cell_value(tt, rowpath = rowpath, colpath = colpath,
                      omit_labrows = FALSE, value_at = TRUE)
})


#'@rdname cell_values
#' @exportMethod value_at
setMethod("value_at", "LabelRow",
          function(tt, rowpath, colpath = NULL) {
    stop("calling value_at for LabelRow objects is not meaningful")
})



.inner_cell_value <- function(tt,
                              rowpath,
                              colpath = NULL,
                              omit_labrows = TRUE,
                              value_at = FALSE) {
    if (is.null(rowpath))
        subtree <- tt
    else
        subtree <- tt_at_path(tt, rowpath)
    if(!is.null(colpath))
        subtree <- subset_cols(subtree, colpath)

    rows <- collect_leaves(subtree, TRUE, !omit_labrows)
    if(value_at && (ncol(subtree) != 1 || length(rows) != 1))
        stop("Combination of rowpath and colpath does not select individual cell.\n",
             "  To retrieve more than one cell value at a time use cell_values().", call. = FALSE)
    if(length(rows) == 1) {
        ret <- row_values(rows[[1]])
        if(value_at && ncol(subtree) == 1)
            ret <- ret[[1]]
        ret
    } else {
        lapply(rows, row_values)
    }
}

.copy_titles <- function(new, old) {
    main_title(new) <- main_title(old)
    subtitles(new) <- subtitles(old)
    main_footer(new) <- main_footer(old)
    prov_footer(new) <- prov_footer(old)
    new
}


.h_t_body <- function(x, res,
                            keep_topleft,
                            keep_titles,
                            reindex_refs) {

    if(keep_topleft)
        top_left(res) <- top_left(x)
    else
        top_left(res) <- character()
    if(keep_titles)
        res <- .copy_titles(res, x)
    else
        ## no titles
        res <- .copy_titles(res, rtable(" "))
    if(reindex_refs)
        res <- update_ref_indexing(res)
    res
}

#' Head and tail methods
#' @inheritParams utils::head
#' @param keep_topleft logical(1). If `TRUE` (the default),
#' top_left material for the table will be carried over to the
#' subset.
#' @param keep_titles logical(1).  If `TRUE` (the default),
#' all title and footer material for the table will be carried over to the
#' subset.
#' @param reindex_refs logical(1). Defaults to `FALSE`. If `TRUE`,
#' referential footnotes will be reindexed for the subset.
#' @docType methods
#' @export
#' @rdname head_tail
setGeneric("head")
#' @docType methods
#' @export
#' @rdname head_tail
setMethod("head", "VTableTree",
          function(x, n = 6, ..., keep_topleft = TRUE,
                   keep_titles = TRUE,
                   ## FALSE because this is a glance
                   ## more often than a subset op
                   reindex_refs = FALSE) {
    ## default
    res <- callNextMethod()
    res <- .h_t_body(x = x, res = res,
              keep_topleft = keep_topleft,
              keep_titles = keep_titles,
              reindex_refs = reindex_refs)
    res
})

#' @docType methods
#' @export
#' @rdname head_tail
setGeneric("tail")
#' @docType methods
#' @export
#' @rdname head_tail
setMethod("tail", "VTableTree",
          function(x, n = 6, ..., keep_topleft = TRUE,
                   keep_titles = TRUE,
                   ## FALSE because this is a glance
                   ## more often than a subset op
                   reindex_refs = FALSE) {

    res <- callNextMethod()
    res <- .h_t_body(x = x, res = res,
                     keep_topleft = keep_topleft,
                     keep_titles = keep_titles,
                     reindex_refs = reindex_refs)
    res
})


#' @rdname brackets
#' @name bracket_subsetting
#' @export
#' @usage
#' x[i, j, ...]
base::`[`

#' @rdname brackets
#' @export
#' @usage
#' x[i, j, ...] <- value
base::`[<-`

