## NB handling the case where there are no values is done during tabulation
## which is the only reason expression(TRUE) is ok, because otherwise
## we (sometimes) run into
## factor()[TRUE] giving <NA> (i.e. length 1)
setGeneric("make_subset_expr", function(spl, val) standardGeneric("make_subset_expr"))

setMethod(
  "make_subset_expr", "VarLevelSplit",
  function(spl, val) {
    ## this is how custom split functions will communicate the correct expression
    ## to the column modeling code
    if (length(value_expr(val)) > 0) {
      return(value_expr(val))
    }

    v <- unlist(rawvalues(val))
    ## XXX if we're including all levels should even missing be included?
    if (is(v, "AllLevelsSentinel")) {
      as.expression(bquote((!is.na(.(a))), list(a = as.name(spl_payload(spl)))))
    } else {
      as.expression(bquote((!is.na(.(a)) & .(a) %in% .(b)), list(
        a = as.name(spl_payload(spl)),
        b = v
      )))
    }
  }
)

setMethod(
  "make_subset_expr", "MultiVarSplit",
  function(spl, val) {
    ## this is how custom split functions will communicate the correct expression
    ## to the column modeling code
    if (length(value_expr(val)) > 0) {
      return(value_expr(val))
    }

    ## v = rawvalues(val)
    ## as.expression(bquote(!is.na(.(a)), list(a = v)))
    expression(TRUE)
  }
)

setMethod(
  "make_subset_expr", "AnalyzeVarSplit",
  function(spl, val) {
    if (avar_inclNAs(spl)) {
      expression(TRUE)
    } else {
      as.expression(bquote(
        !is.na(.(a)),
        list(a = as.name(spl_payload(spl)))
      ))
    }
  }
)

setMethod(
  "make_subset_expr", "AnalyzeColVarSplit",
  function(spl, val) {
    expression(TRUE)
  }
)

## XXX these are going to be ridiculously slow
## FIXME

setMethod(
  "make_subset_expr", "VarStaticCutSplit",
  function(spl, val) {
    v <- rawvalues(val)
    ##    as.expression(bquote(which(cut(.(a), breaks=.(brk), labels = .(labels),
    as.expression(bquote(
      cut(.(a),
        breaks = .(brk), labels = .(labels),
        include.lowest = TRUE
      ) == .(b),
      list(
        a = as.name(spl_payload(spl)),
        b = v,
        brk = spl_cuts(spl),
        labels = spl_cutlabels(spl)
      )
    ))
  }
)

## NB this assumes spl_cutlabels(spl) is in order!!!!!!
setMethod(
  "make_subset_expr", "CumulativeCutSplit",
  function(spl, val) {
    v <- rawvalues(val)
    ## as.expression(bquote(which(as.integer(cut(.(a), breaks=.(brk),
    as.expression(bquote(
      as.integer(cut(.(a),
        breaks = .(brk),
        labels = .(labels),
        include.lowest = TRUE
      )) <=
        as.integer(factor(.(b), levels = .(labels))),
      list(
        a = as.name(spl_payload(spl)),
        b = v,
        brk = spl_cuts(spl),
        labels = spl_cutlabels(spl)
      )
    ))
  }
)

## I think this one is unnecessary,
## build_table collapses DynCutSplits into
## static ones.
##
## XXX TODO fixme
## setMethod("make_subset_expr", "VarDynCutSplit",
##           function(spl, val) {
##     v = rawvalues(val)
##     ##   as.expression(bquote(which(.(fun)(.(a)) == .(b)),
##     as.expression(bquote(.(fun)(.(a)) == .(b)),
##                   list(a = as.name(spl_payload(spl)),
##                        b = v,
##                        fun = spl@cut_fun))
## })

setMethod(
  "make_subset_expr", "AllSplit",
  function(spl, val) expression(TRUE)
)

## probably don't need this

setMethod(
  "make_subset_expr", "expression",
  function(spl, val) spl
)

setMethod(
  "make_subset_expr", "character",
  function(spl, val) {
    newspl <- VarLevelSplit(spl, spl)
    make_subset_expr(newspl, val)
  }
)

.combine_subset_exprs <- function(ex1, ex2) {
  if (is.null(ex1) || identical(ex1, expression(TRUE))) {
    if (is.expression(ex2) && !identical(ex2, expression(TRUE))) {
      return(ex2)
    } else {
      return(expression(TRUE))
    }
  }

  ## if(is.null(ex2))
  ##     ex2 <- expression(TRUE)
  stopifnot(is.expression(ex1), is.expression(ex2))
  as.expression(bquote((.(a)) & .(b), list(a = ex1[[1]], b = ex2[[1]])))
}

make_pos_subset <- function(spls = pos_splits(pos),
                            svals = pos_splvals(pos),
                            pos) {
  expr <- NULL
  for (i in seq_along(spls)) {
    newexpr <- make_subset_expr(spls[[i]], svals[[i]])
    expr <- .combine_subset_exprs(expr, newexpr)
  }
  expr
}

get_pos_extra <- function(svals = pos_splvals(pos),
                          pos) {
  ret <- list()
  for (i in seq_along(svals)) {
    extrs <- splv_extra(svals[[i]])
    if (any(names(ret) %in% names(extrs))) {
      stop("same extra argument specified at multiple levels of nesting. Not currently supported")
    }
    ret <- c(ret, extrs)
  }
  ret
}

get_col_extras <- function(ctree) {
  leaves <- collect_leaves(ctree)
  lapply(
    leaves,
    function(x) get_pos_extra(pos = tree_pos(x))
  )
}

setGeneric(
  "make_col_subsets",
  function(lyt, df) standardGeneric("make_col_subsets")
)

setMethod(
  "make_col_subsets", "LayoutColTree",
  function(lyt, df) {
    leaves <- collect_leaves(lyt)
    lapply(leaves, make_col_subsets)
  }
)

setMethod(
  "make_col_subsets", "LayoutColLeaf",
  function(lyt, df) {
    make_pos_subset(pos = tree_pos(lyt))
  }
)

create_colinfo <- function(lyt, df, rtpos = TreePos(),
                           counts = NULL,
                           alt_counts_df = NULL,
                           total = NULL,
                           topleft = NULL) {
  ## this will work whether clayout is pre or post
  ## data
  clayout <- clayout(lyt)
  if (is.null(topleft)) {
    topleft <- top_left(lyt)
  }
  cc_format <- colcount_format(lyt) %||% "(N=xx)"

  ## do it this way for full backwards compatibility
  if (is.null(alt_counts_df)) {
    alt_counts_df <- df
  }
  ctree <- coltree(clayout, df = df, rtpos = rtpos, alt_counts_df = alt_counts_df, ccount_format = cc_format)
  if (!is.na(disp_ccounts(lyt))) {
    leaf_pths <- make_col_df(ctree, visible_only = TRUE, na_str = "", ccount_format = cc_format)$path
    for (path in leaf_pths) {
      colcount_visible(ctree, path) <- disp_ccounts(lyt)
    }
  }

  cexprs <- make_col_subsets(ctree, df)
  colextras <- col_extra_args(ctree)

  ## calculate the counts based on the df
  ## This presumes that it is called on the WHOLE dataset,
  ## NOT after any splitting has occurred. Otherwise
  ## the counts will obviously be wrong.
  if (is.null(counts)) {
    counts <- rep(NA_integer_, length(cexprs))
  } else if (length(counts) != length(cexprs)) {
    stop(
      "Length of overriding counts must equal number of columns. Got ",
      length(counts), " values for ", length(cexprs), " columns. ",
      "Use NAs to specify that the default counting machinery should be ",
      "used for that position."
    )
  }

  counts_df_name <- "alt_counts_df"
  if (identical(alt_counts_df, df)) { # is.null(alt_counts_df)) {
    alt_counts_df <- df
    counts_df_name <- "df"
  }
  calcpos <- is.na(counts)

  calccounts <- sapply(cexprs, function(ex) {
    if (identical(ex, expression(TRUE))) {
      nrow(alt_counts_df)
    } else if (identical(ex, expression(FALSE))) {
      0L
    } else {
      vec <- try(eval(ex, envir = alt_counts_df), silent = TRUE)
      if (is(vec, "numeric")) {
        length(vec)
      } else if (is(vec, "logical")) { ## sum(is.na(.)) ????
        sum(vec, na.rm = TRUE)
      }
    }
  })
  counts[calcpos] <- calccounts[calcpos]
  counts <- as.integer(counts)
  if (is.null(total)) {
    total <- sum(counts)
  }

  cpths <- col_paths(ctree)
  for (i in seq_along(cpths)) {
    facet_colcount(ctree, cpths[[i]]) <- counts[i]
  }
  InstantiatedColumnInfo(
    treelyt = ctree,
    csubs = cexprs,
    extras = colextras,
    cnts = counts,
    dispcounts = disp_ccounts(lyt),
    countformat = cc_format,
    total_cnt = total,
    topleft = topleft
  )
}
