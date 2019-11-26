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

empty_dominant_axis = function(layout) {
    stopifnot(is(layout, "RTablesLayout"))
    ((is(layout, "RowDominantLayout") && n_leaves(rowtree(layout)) == 0L) ||
     (is(layout, "ColDominantLayout") && n_leaves(coltree(layout)) == 0L))

}

# Method Definitions ----

## pre-data layouts:
## structured as lists of lists of splits
## e.g. reference table 1 would have column  proto-layout
##  list(list(VarSplit("ARM"), VarSplit("SEX")))
## and row layout
##  list(list(AllSplit()),
##       list(VarSplit("RACE"),  VarSplit("Factor2")),
##       list(AllSplit()) ## remember var3  is the variable analyzed

setMethod("c", "SplitVector", function(x, ...) {
    arglst = list(...)
    stopifnot(all(sapply(arglst, is, "Split")))
    tmp = c(unclass(x), arglst)
    SplitVector(lst = tmp)
})

## add_row_split and add_col_split are "recursive method stacks" which follow
## the general pattern of accept object -> call add_*_split on slot of object ->
## update object with value returned from slot method, return object.
##
## Thus each of the methods is idempotent, returning an updated object of the
## same class it was passed. The exception for idempotency is the NULL method
## which constructs a PreDataTableLayouts object with the specified split in the
## correct place.

## The cascading (by class) in this case is as follows for the row case:
## PreDataTableLayouts -> PreDataRowLayout -> SplitVector
setGeneric("add_row_split", function(lyt = NULL, spl, pos) standardGeneric("add_row_split"))

setMethod("add_row_split", "NULL", function(lyt, spl, pos) {
    rl = PreDataRowLayout(SplitVector(spl))
    cl = PreDataColLayout()
    PreDataTableLayouts(rlayout = rl, clayout = cl)
})

setMethod("add_row_split", "PreDataRowLayout",
          function(lyt, spl, pos) {
    stopifnot(pos >0 && pos <= length(lyt) + 1)
    tmp  = if (pos <= length(lyt)) {
               add_row_split(lyt[[pos]], spl, pos)
           } else {
               SplitVector(spl)
           }
    lyt[[pos]] = tmp
    lyt
})
setMethod("add_row_split", "SplitVector",
          function(lyt, spl, pos) {
    tmp = c(unclass(lyt), spl)
    SplitVector(lst = tmp)
})
    

setMethod("add_row_split", "PreDataTableLayouts",
          function(lyt, spl, pos){
    rlyt = lyt@row_layout
    rlyt = add_row_split(rlyt, spl, pos)
    lyt@row_layout = rlyt
    lyt
})

setMethod("add_row_split", "ANY",
          function(lyt, spl, pos) stop("nope. can't add a row split to that (", class(lyt), "). contact the maintaner.")
          )


setGeneric("cmpnd_last_rowsplit", function(lyt, spl, constructor) standardGeneric("cmpnd_last_rowsplit"))

setMethod("cmpnd_last_rowsplit", "NULL", function(lyt, spl, constructor) {
    stop("no existing splits to compound with. contact the maintainer")
})

setMethod("cmpnd_last_rowsplit", "PreDataRowLayout",
          function(lyt, spl, constructor) {
    pos = length(lyt)
    tmp = cmpnd_last_rowsplit(lyt[[pos]], spl, constructor)
    lyt[[pos]] = tmp
    lyt
})
setMethod("cmpnd_last_rowsplit", "SplitVector",
          function(lyt, spl, constructor) {
    pos = length(lyt)
    lst = lyt[[pos]]
    tmp = if(is(lst, "CompoundSplit")) {
              spl_payload(lst) = c(spl_payload(lst), spl)
              lst
          } else {
              constructor(.payload = list(lst, spl))
          }
    lyt[[pos]] = tmp
    lyt
})
    

setMethod("cmpnd_last_rowsplit", "PreDataTableLayouts",
          function(lyt, spl, constructor){
    rlyt = rlayout(lyt)
    rlyt = cmpnd_last_rowsplit(rlyt, spl, constructor)
    rlayout(lyt)= rlyt
    lyt
})

setMethod("cmpnd_last_rowsplit", "ANY",
          function(lyt, spl, constructor) stop("nope. can't do cmpnd_last_rowsplit to that (", class(lyt), "). contact the maintaner.")
          )



setGeneric("add_col_split", function(lyt = NULL, spl, pos) standardGeneric("add_col_split"))

setMethod("add_col_split", "NULL", function(lyt, spl, pos) {
    cl = PreDataColLayout(SplitVector(spl))
    rl = PreDataRowLayout()
    PreDataTableLayouts(rlayout = rl, clayout = cl)
})

setMethod("add_col_split", "PreDataColLayout",
          function(lyt, spl, pos) {
    stopifnot(pos > 0 && pos <= length(lyt) + 1)
    tmp  = if (pos <= length(lyt)) {
               add_col_split(lyt[[pos]], spl, pos)
           } else {
               SplitVector(spl)
           }
    lyt[[pos]] = tmp
    lyt
})

setMethod("add_col_split", "SplitVector",
          function(lyt, spl, pos) {
    tmp = c(lyt, spl)
    SplitVector(lst = tmp)
})

setMethod("add_col_split", "PreDataTableLayouts",
          function(lyt, spl, pos){
    rlyt = lyt@col_layout
    rlyt = add_col_split(rlyt, spl, pos)
    lyt@col_layout = rlyt
    lyt
})

setMethod("add_col_split", "ANY",
          function(lyt, spl, pos) stop("nope. can't add a col split to that (", class(lyt), "). contact the maintaner.")
          )



setGeneric("cmpnd_last_colsplit", function(lyt, spl, constructor) standardGeneric("cmpnd_last_colsplit"))

setMethod("cmpnd_last_colsplit", "NULL", function(lyt, spl, constructor) {
    stop("no existing splits to compound with. contact the maintainer")
})

setMethod("cmpnd_last_colsplit", "PreDataColLayout",
          function(lyt, spl, constructor) {
    pos = length(lyt)
    tmp = cmpnd_last_colsplit(lyt[[pos]], spl, constructor)
    lyt[[pos]] = tmp
    lyt
})
setMethod("cmpnd_last_colsplit", "SplitVector",
          function(lyt, spl, constructor) {
    pos = length(lyt)
    lst = lyt[[pos]]
    tmp = if(is(lst, "CompoundSplit")) {
              spl_payload(lst) = c(spl_payload(lst), spl)
              lst
          } else {
              constructor(.payload = list(lst, spl))
          }
    lyt[[pos]] = tmp
    lyt
})
    

setMethod("cmpnd_last_colsplit", "PreDataTableLayouts",
          function(lyt, spl, constructor){
    clyt = clayout(lyt)
    clyt = cmpnd_last_colsplit(clyt, spl, constructor)
    clayout(lyt)= clyt
    lyt
})

setMethod("cmpnd_last_colsplit", "ANY",
          function(lyt, spl, constructor) stop("nope. can't do cmpnd_last_colsplit to that (", class(lyt), "). contact the maintaner.")
          )



# constructors ----

add_new_rowtree = function(lyt, spl) {
    add_row_split(lyt, spl, next_rpos(lyt, TRUE))
}


add_new_coltree = function(lyt, spl) {
    add_col_split(lyt, spl, next_cpos(lyt, TRUE))
}


## Pipe-able functions to add the various types of splits to the current layout for both
## row and column.  These all act as wrappers to the add_col_split and add_row_split
## method stacks.


#' Declaring a column-split based on levels of a variable
#' 
#' Will generate children for each subset of a categorical variable
#' 
#' @inheritParams argument_conventions
#' 
#' @export
#'
#' @examples 
#' 
#' library(magrittr)
#' 
#' l <- NULL %>% add_colby_varlevels("ARM") 
#' l
#' 
#' # add an analysis (summary)
#' l2 <- l %>% 
#'     add_analyzed_vars("AGE", afun = lstwrap(summary) , fmt = "xx.xx")
#' l2
#' 
#' build_table(l2, DM)
#' 
#' # By default sequentially adding layouts results in nesting
#' l3 <- NULL %>% add_colby_varlevels("ARM") %>%
#'   add_colby_varlevels("SEX") %>%
#'   add_analyzed_vars("AGE", afun = lstwrap(summary), fmt = "xx.xx")
#' l3
#' 
#' build_table(l3, DM)
#' 
add_colby_varlevels = function(lyt, var, lbl = var, valuelblvar = var, splfmt = NULL, newtoplev = FALSE) {
    spl = VarLevelSplit(var = var, splbl = lbl, valuelblvar = valuelblvar, splfmt = splfmt)
    pos = next_cpos(lyt, newtoplev)
    add_col_split(lyt, spl, pos)
}



add_colby_varwbline = function(lyt, var, baseline, incl_all = FALSE, lbl, valuelblvar, splfmt = NULL, newtoplev = FALSE) {

    spl = VarLevWBaselineSplit(var = var,
                               baseline = baseline,
                               incl_all = incl_all,
                               splbl = lbl,
                               valuelblvar = valuelblvar,
                               splfmt = splfmt)
    pos = next_cpos(lyt, newtoplev)
    add_col_split(lyt, spl, pos)
}


#' Add Rows according to levels of a variable
#' 
#' 
#' @inheritParams argument_conventions
#' 
#' @export
#' 
#' @examples 
#' 
#' library(magrittr)
#' 
#' l <- NULL %>% add_colby_varlevels("ARM", "Arm") %>%
#'   add_colby_varlevels("SEX", "Gender") %>%
#'   add_summary_count(lbl = "Overall (N)") %>%
#'   add_rowby_varlevels("RACE", "Ethnicity") %>%
#'   add_summary_count("RACE", lblfmt = "%s (n)") %>%
#'   add_analyzed_vars("AGE", "Age", afun = mean, fmt = "xx.xx")
#'   
#' l
#' 
#' build_table(l, DM)
#'   
add_rowby_varlevels = function(lyt,  var, lbl = var,  vlblvar = var, splfun = NULL, fmt = NULL, newtoplev = FALSE) {
    spl = VarLevelSplit(var = var,
                        splbl = lbl,
                        valuelblvar = vlblvar,
                        splfun = splfun,
                        splfmt = fmt)
    pos = next_rpos(lyt, newtoplev)
    add_row_split(lyt, spl, pos)
}


add_colby_multivar = function(lyt, vars, lbl, varlbls = vars,
                              newtoplev = FALSE) {
    spl = MultiVarSplit(vars = vars, splbl = lbl, varlbls)
    pos = next_cpos(lyt, newtoplev)
    add_col_split(lyt, spl, pos)
}


add_rowby_multivar = function(lyt, vars, lbl, varlbls,
                              splfmt = NULL,
                              newtoplev = FALSE) {
    spl = MultiVarSplit(vars = vars, splbl = lbl, varlbls,
                        splfmt = splfmt)
    pos = next_rpos(lyt, newtoplev)
    add_row_split(lyt, spl, pos)
}

add_colby_staticcut = function(lyt, var, lbl, cuts,
                            cutlbls = NULL,
                            newtoplev = FALSE) {
    spl = VarStaticCutSplit(var, lbl, cuts, cutlbls)
    pos = next_cpos(lyt, newtoplev)
    add_col_split(lyt, spl, pos)
}

add_rowby_staticcut = function(lyt, var, lbl, cuts,
                               cutlbls = NULL,
                               splfmt = NULL,
                            newtoplev = FALSE) {
    spl = VarStaticCutSplit(var, lbl, cuts, cutlbls,
                            splfmt = splfmt)
    pos = next_rpos(lyt, newtoplev)
    add_row_split(lyt, spl, pos)
}

add_colby_dyncut = function(lyt, var, lbl, cutfun,
                            splfmt = NULL,
                            newtoplev = FALSE) {
    spl = VarDynCutSplit(var, lbl, cutfun,
                         splfmt = splfmt)
    pos = next_cpos(lyt, newtoplev)
    add_col_split(lyt, spl, pos)
}


add_rowby_dyncut = function(lyt, var, lbl, cutfun,
                            splfmt = NULL,
                            newtoplev = FALSE) {
    spl = VarDynCutSplit(var, lbl, cutfun,
                         splfmt = splfmt)
    pos = next_rpos(lyt, newtoplev)
    add_row_split(lyt, spl, pos)
}



add_analyzed_var = function(lyt, var, lbl = var, afun,
                            fmt = NULL,
                            rowlabs = "",
                            newtoplev = FALSE,
                            inclNAs = FALSE) {
    spl = AnalyzeVarSplit(var, lbl,
                          afun = afun,
                          splfmt = fmt,
                          defrowlab = rowlabs,
                          inclNAs = inclNAs)
    .Deprecated("add_analyzed_vars")
    
    if(!newtoplev &&
       (is(last_rowsplit(lyt), "AnalyzeVarSplit") ||
        is(last_rowsplit(lyt), "AnalyzeMultiVars"))) {
        cmpnd_last_rowsplit(lyt, spl)
    } else {
        pos = next_rpos(lyt, newtoplev)
        add_row_split(lyt, spl, pos)
    }
}


#' Add an anlysis split
#' 
#' This will be the end of the SplitVector at the position specified, because it defines an anlaysis that will generate
#' rows, rather than a further partition of the data.
#' 
#' @inheritParams argument_conventions
#' 
#' @export
#' 
#' @examples 
#' 
#' library(magrittr)
#' 
#' l <- NULL %>% add_colby_varlevels("ARM") %>% 
#'     add_analyzed_vars("AGE", afun = lstwrap(summary) , fmt = "xx.xx")
#' l
#' 
#' build_table(l, DM)
#' 
add_analyzed_vars = function(lyt,
                             var,
                             lbl = var,
                             afun,
                             fmt = NULL,
                             rowlabs = "",
                             newtoplev = FALSE,
                             inclNAs = FALSE) {

    subafun = substitute(afun)
    if(is.name(subafun) &&
       missing(rowlabs) &&
       is.function(afun) &&
       ## this is gross. basically testing
       ## if the symbol we have corresponds
       ## in some meaningful way to the function
       ## we will be calling.
       identical(mget(as.character(subafun),
                      mode = "function",
                      ifnotfound = list(NULL),
                      inherits = TRUE
                      )[[1]], afun)) {
        rowlabs = as.character(subafun)
    }
    spl = AnalyzeMultiVars(var, lbl,
                          afun = afun,
                          splfmt = fmt,
                          defrowlab = rowlabs,
                          inclNAs = inclNAs)
 
    if(!newtoplev &&
       (is(last_rowsplit(lyt), "AnalyzeVarSplit") ||
        is(last_rowsplit(lyt), "AnalyzeMultiVars"))) {
        cmpnd_last_rowsplit(lyt, spl, AnalyzeMultiVars)
    } else {
        pos = next_rpos(lyt, newtoplev)
        add_row_split(lyt, spl, pos)
    }
}



add_analyzed_colvars = function(lyt, lbl, afun,
                                fmt = NULL,
                                newtoplev = FALSE) {
    spl = AnalyzeVarSplit(NA_character_, lbl, afun = afun,
                          splfmt = fmt)
    pos = next_rpos(lyt, newtoplev)
    add_row_split(lyt, spl, pos)


}

add_analyzed_blinecomp = function(lyt, var = NA_character_, lbl, afun,
                                   compfun = `-`,
                                   fmt = NULL,
                                  newtoplev = FALSE) {
    if(is.character(afun)) {
        afnm = afun
        afun = get(afun, mode = "function")
    } else {
        afnm = as.character(substitute(afun))
    }
    
    if(is.character(compfun))
        compfun = get(compfun, mode = "function")
    afun2 = function(x, .baseline_data = NULL, .N_col, .N_total, ...) {
        if(is.null(.baseline_data))
            stop("did not receive baseline aggregataion value required for comparison")
        if(!is.na(var))
            blinevardat = .baseline_data[[var]]
        else
            blinevardat = .baseline_data
        if(identical(x, blinevardat))
            return(NULL) ## we are in the baseline
        
        args = list()
        if(takes_coln(afun))
            args = c(args, list(.N_col = .N_col))
        if(takes_totn(afun))
            args = c(args, list(.N_total = .N_total))
        ##XXX totdo extras
        datvalue = do.call(afun, c(list(x), args))
        blvalue = do.call(afun, c(list(blinevardat), args))
        ## TODO(?) compfun cmight need .N_col or .N_total??
        compargs = list(datvalue, blvalue)
        ret = do.call(compfun, compargs)
        if(length(ret) == 1 && is.null(names(ret)))
            names(ret) = afnm
        ret
    }

    spl = AnalyzeVarSplit(var, lbl, afun = afun2,
                          splfmt = fmt)
    pos = next_rpos(lyt, newtoplev)
    add_row_split(lyt, spl, pos)
}

add_2dtable_blinecomp = function(lyt,
                                 var = NA_character_,
                                 lbl = var,
                                 compfun,
                                 fmt = NULL,
                                 newtoplev = FALSE) {

    if(is.character(compfun)) {
        cfnm = compfun
        compfun = get(compfun, mode = "function")
    } else {
        cfnm = as.character(substitute(compfun))
    }
    

    
    compfun2 = function(colvardat, blinevardat) {
        if(identical(colvardat, blinevardat))
            return(NULL)
            
        ## TODO(?) compfun cmight need .N_col or .N_total??
        tab = .make_2xk_tab(colvardat, blinevardat)
        ret = compfun(tab)
        if(length(ret) == 1 && is.null(names(ret)))
            names(ret) = cfnm
        ret
    }
    add_analyzed_blinecomp(lyt = lyt, var = var, lbl = lbl,
                           afun = function(x) x,
                           compfun = compfun2,
                           fmt = fmt,
                           newtoplev = newtoplev)
}


## Add a total column at the next **top level** spot in
## the column layout. 
add_col_total = function(lyt, lbl) {
    spl = AllSplit(lbl)
    add_col_split(lyt,
                  spl,
                  next_cpos(lyt, TRUE))
}

setGeneric("add_summary",
           function(lyt, lbl, cfun, cfmt = NULL) standardGeneric("add_summary"))
setMethod("add_summary", "PreDataTableLayouts",
          function(lyt, lbl, cfun, cfmt = NULL) {
    tmp = add_summary(rlayout(lyt), lbl, cfun,
                      cfmt = cfmt)
    rlayout(lyt) = tmp
    lyt
})

setMethod("add_summary", "PreDataRowLayout",
          function(lyt, lbl, cfun, cfmt = NULL) {
    if(length(lyt) == 0 ||
       (length(lyt) == 1 && length(lyt[[1]]) == 0)) {
        rt = root_spl(lyt)
        rt = add_summary(rt, lbl, cfun, cfmt = cfmt)
        root_spl(lyt) = rt
    } else {
        ind = length(lyt)
        tmp = add_summary(lyt[[ind]], lbl, cfun,
                          cfmt = cfmt)
        lyt[[ind]] = tmp
    }
    lyt
})

setMethod("add_summary", "SplitVector",
          function(lyt, lbl, cfun, cfmt = NULL) {
    ind = length(lyt)
    if(ind == 0) stop("no split to add content rows at")
    spl = lyt[[ind]]
    ## if(is(spl, "AnalyzeVarSplit")) stop("can't add content rows to analyze variable split")
    tmp = add_summary(spl, lbl, cfun, cfmt = cfmt)
    lyt[[ind]] = tmp
    lyt
})

setMethod("add_summary", "Split",
          function(lyt, lbl, cfun, cfmt = NULL) {
    content_fun(lyt) = cfun
    obj_fmt(lyt) = cfmt
    lyt
})

.count_raw_constr = function(var, valfmt, lblfmt) {
    function(df, lblstr = "") {
        lbl = sprintf(lblfmt, lblstr)
        if(!is.null(var))
            cnt = sum(!is.na(df[[var]]))
        else
            cnt = nrow(df)

        ret = cnt
        
        attr(ret, "format") = valfmt
        names(ret) = lbl
        ret
    }
}

.count_wpcts_constr = function(var, valfmt, lblfmt) {
    function(df, lblstr = "", .N_col) {
        lbl = sprintf(lblfmt, lblstr)
        if(!is.null(var))
            cnt = sum(!is.na(df[[var]]))
        else
            cnt = nrow(df)

        ## the formatter does the *100 so we don't here.
        ret = list(c(cnt, cnt/.N_col))
        
        attr(ret, "format") = valfmt
        names(ret) = lbl
        ret
    }
}


#' Count number of observations
#' 
#' @inheritParams 
#' 
#' 
#' @export
#' 
#' @examples 
#' 
#' library(magrittr)
#' 
#' l <- NULL %>% add_colby_varlevels("ARM") %>% 
#'     add_rowby_varlevels("RACE") %>% 
#'     add_summary_count(lblfmt = "%s (n)") %>% 
#'     add_analyzed_vars("AGE", afun = lstwrap(summary) , fmt = "xx.xx")
#' l
#' 
#' build_table(l, DM)
#' 
add_summary_count = function(lyt, var = NULL, lblfmt = "%s", valfmt = "xx (xx.x%)" ){

    if(length(gregexpr("xx", valfmt)[[1]]) == 2)
        fun = .count_wpcts_constr(var, valfmt, lblfmt)
    else
        fun = .count_raw_constr(var,valfmt, lblfmt)
    add_summary(lyt, lbl = lbl, cfun = fun, cfmt = valfmt)
}


#' Add the column population counts
#' 
#' 
#' @inheritParams 
#' 
#' @export
#' 
#' @examples 
#' 
#' library(magrittr)
#' 
#' l <- NULL %>% add_colby_varlevels("ARM") %>% 
#'     add_colcounts() %>% 
#'     add_rowby_varlevels("RACE") %>% 
#'     add_analyzed_vars("AGE", afun = function(x) list(min = min(x), max = max(x)))
#' l
#' 
#' build_table(l, DM)
#' 
add_colcounts = function(lyt, fmt = "(N=xx)") {
    disp_ccounts(lyt) = TRUE
    colcount_fmt(lyt) = fmt
    lyt
}
    
## Currently existing tables can ONLY be added 
## as new entries at the top level, never at any
## level of nesting.
add_existing_table = function(lyt, tab) {

    lyt = add_row_split(lyt,
                        tab,
                        next_rpos(lyt, TRUE))
    lyt
}


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
  
  rtable(header = sapply(layout_children(coltree(layout)), function(leaf) leaf@label), rr)
}


takes_coln = function(f) {
    stopifnot(is(f, "function"))
    forms = names(formals(f))
    res = ".N_col" %in% forms
    res
}

takes_totn = function(f) {
    stopifnot(is(f, "function"))
    forms = names(formals(f))
    res = ".N_total" %in% forms 
    res
}


tmpfun = function(lyt, df) {

}

## use data to transform dynamic cuts to static cuts
setGeneric("fix_dyncuts", function(spl, df) standardGeneric("fix_dyncuts"))

setMethod("fix_dyncuts", "Split", function(spl, df) spl)
setMethod("fix_dyncuts", "VarDynCutSplit",
          function(spl, df) {

    var = spl_payload(spl)
    varvec = df[[var]]

    cfun = spl@cut_fun
    cuts = cfun(varvec)
    VarStaticCutSplit(var = var, splbl = spl_lbl(spl),
                      cuts = cuts, cutlbls = names(cuts))
})


.fd_helper = function(spl, df) {
    lst = lapply(spl, fix_dyncuts, df = df)
    as(lst, class(spl))
}
       
setMethod("fix_dyncuts", "PreDataAxisLayout",
          function(spl, df) {
    .fd_helper(spl, df)
})
setMethod("fix_dyncuts", "SplitVector",
          function(spl, df) {
    .fd_helper(spl, df)
})


setMethod("fix_dyncuts", "PreDataTableLayouts",
          function(spl, df) {
    rlayout(spl) = fix_dyncuts(rlayout(spl), df)
    clayout(spl) = fix_dyncuts(clayout(spl), df)
    spl
})




## Manual column construction in a simple (seeming
## to the user) way.

#' @param dots One or more vectors of levels to appear
#' in the column splace. If more than one set of levels is given, the values of the second are nested within each value of the first, and so on.
#' @examples
#' # simple one level column space
#' rows = lapply(1:5, function(i) {
#'    DataRow(rep(i, times  = 3))})
#' tab = TableTree(kids = rows, cinfo = manual_cols(split = c("a", "b", "c")))
#'
#' # manually declared nesting
#' tab2 = TableTree(kids = list(DataRow(as.list(1:4))),
#'                  cinfo = manual_cols(Arm = c("Arm A", "Arm B"),
#'                                      Gender = c("M", "F")))


manual_cols = function(..., .lst = list(...)) {
    if(is.null(names(.lst)))
        names(.lst) = paste("colsplit", seq_along(.lst))
    
    splvec = SplitVector(lst = mapply(ManualSplit, levs = .lst, lbl = names(.lst)))
    ctree = splitvec_to_coltree(data.frame(), splvec=splvec, pos = TreePos())
    InstantiatedColumnInfo(treelyt = ctree)
}


#' Returns a function that coerces the return values of f to a list
#' 
#' @export
#' 
#' @examples 
#' 
#' summary(iris$Sepal.Length)
#' 
#' library(magrittr)
#' f <- lstwrap(summary)
#' f(iris$Sepal.Length)
#' 
lstwrap = function(f) {
    function(...) as.list(f(...))
}
