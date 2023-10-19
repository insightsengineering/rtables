## Split types
## -----------
## variable: split on distinct values of a variable
## all: include all observations (root 'split')
## rawcut: cut on static values of a variable
## quantilecut: cut on quantiles of observed values for a variable
## missing: split obs based on missingness of a variable/observation. This could be used for compare to ref_group??
## multicolumn: each child analyzes a different column
## arbitrary: children are not related to each other in any systematic fashion.


## null is ok here.
check_ok_label <- function(lbl, multi_ok = FALSE) {
  if (length(lbl) == 0) {
    return(TRUE)
  }

  if (length(lbl) > 1) {
    if (multi_ok) {
      return(all(vapply(lbl, check_ok_label, TRUE)))
    }
    stop("got a label of length > 1")
  }

  if (grepl("([{}])", lbl)) {
    stop("Labels cannot contain { or } due to their use for indicating referential footnotes")
  }
  invisible(TRUE)
}

valid_lbl_pos <- c("default", "visible", "hidden", "topleft")
.labelkids_helper <- function(charval) {
  ret <- switch(charval,
    "default" = NA,
    "visible" = TRUE,
    "hidden" = FALSE,
    "topleft" = FALSE,
    stop(
      "unrecognized charval in .labelkids_helper. ",
      "this shouldn't ever happen"
    )
  )
  ret
}


setOldClass("expression")
setClassUnion("SubsetDef", c("expression", "logical", "integer", "numeric"))



setClassUnion("integerOrNULL", c("NULL", "integer"))
setClassUnion("characterOrNULL", c("NULL", "character"))





## should XXX [splits, s_values, sval_labels, subset(?)] be a data.frame?
setClass("TreePos", representation(
  splits = "list",
  s_values = "list",
  sval_labels = "character",
  subset = "SubsetDef"
),
validity = function(object) {
  nspl <- length(object@splits)
  length(object@s_values) == nspl && length(object@sval_labels) == nspl
}
)



setClassUnion("functionOrNULL", c("NULL", "function"))
setClassUnion("listOrNULL", c("NULL", "list"))
## TODO (?) make "list" more specific, e.g FormatList, or FunctionList?
setClassUnion("FormatSpec", c("NULL", "character", "function", "list"))

setClass("ValueWrapper", representation(
  value = "ANY",
  label = "characterOrNULL"
),
contains = "VIRTUAL"
)
## heavier-weight than I'd like but I think we need
## this to carry around thee subsets for
## comparison-based splits

setClass("SplitValue",
  contains = "ValueWrapper",
  representation(extra = "list")
)

SplitValue <- function(val, extr = list(), label = val) {
  if (is(val, "SplitValue")) {
    if (length(splv_extra(val)) > 0) {
      extr <- c(splv_extra(val), extr)
    }
    splv_extra(val) <- extr
    return(val)
  }
  if (!is(extr, "list")) {
    extr <- list(extr)
  }
  if (!is(label, "character")) {
    label <- as.character(label)
  }
  check_ok_label(label)
  new("SplitValue",
    value = val,
    extra = extr, label = label
  )
}

setClass("LevelComboSplitValue",
  contains = "SplitValue",
  representation(combolevels = "character")
)

## wrapped in user-facing `add_combo_facet`
LevelComboSplitValue <- function(val, extr, combolevels, label = val) {
  check_ok_label(label)
  new("LevelComboSplitValue",
    value = val,
    extra = extr,
    combolevels = combolevels,
    label = label
  )
}


setClass("Split",
  contains = "VIRTUAL",
  representation(
    payload = "ANY",
    name = "character",
    split_label = "character",
    split_format = "FormatSpec",
    split_na_str = "character",
    split_label_position = "character",
    ## NB this is the function which is applied to
    ## get the content rows for the CHILDREN of this
    ## split!!!
    content_fun = "listOrNULL", ## functionOrNULL",
    content_format = "FormatSpec",
    content_na_str = "character",
    content_var = "character",
    label_children = "logical",
    extra_args = "list",
    indent_modifier = "integer",
    content_indent_modifier = "integer",
    content_extra_args = "list",
    page_title_prefix = "character",
    child_section_div = "character"
  )
)


setClass("CustomizableSplit",
  contains = "Split",
  representation(split_fun = "functionOrNULL")
)

#' @exportClass VarLevelSplit
#' @rdname VarLevelSplit
#' @author Gabriel Becker
setClass("VarLevelSplit",
  contains = "CustomizableSplit",
  representation(
    value_label_var = "character",
    value_order = "ANY"
  )
)
#' Split on levels within a variable
#'
#' @inheritParams lyt_args
#' @return a \code{VarLevelSplit} object.
#' @inheritParams constr_args
#' @export
VarLevelSplit <- function(var,
                          split_label,
                          labels_var = NULL,
                          cfun = NULL,
                          cformat = NULL,
                          cna_str = NA_character_,
                          split_fun = NULL,
                          split_format = NULL,
                          split_na_str = NA_character_,
                          valorder = NULL,
                          split_name = var,
                          child_labels = c("default", "visible", "hidden"),
                          extra_args = list(),
                          indent_mod = 0L,
                          label_pos = c("topleft", "hidden", "visible"),
                          cindent_mod = 0L,
                          cvar = "",
                          cextra_args = list(),
                          page_prefix = NA_character_,
                          section_div = NA_character_) {
  child_labels <- match.arg(child_labels)
  if (is.null(labels_var)) {
    labels_var <- var
  }
  check_ok_label(split_label)
  new("VarLevelSplit",
    payload = var,
    split_label = split_label,
    name = split_name,
    value_label_var = labels_var,
    content_fun = cfun,
    content_format = cformat,
    content_na_str = cna_str,
    split_fun = split_fun,
    split_format = split_format,
    split_na_str = split_na_str,
    value_order = NULL,
    label_children = .labelkids_helper(child_labels),
    extra_args = extra_args,
    indent_modifier = as.integer(indent_mod),
    content_indent_modifier = as.integer(cindent_mod),
    content_var = cvar,
    split_label_position = label_pos,
    content_extra_args = cextra_args,
    page_title_prefix = page_prefix,
    child_section_div = section_div
  )
}


setClass("AllSplit", contains = "Split")

AllSplit <- function(split_label = "",
                     cfun = NULL,
                     cformat = NULL,
                     cna_str = NA_character_,
                     split_format = NULL,
                     split_na_str = NA_character_,
                     split_name = NULL,
                     extra_args = list(),
                     indent_mod = 0L,
                     cindent_mod = 0L,
                     cvar = "",
                     cextra_args = list(),
                     ...) {
  if (is.null(split_name)) {
    if (nzchar(split_label)) {
      split_name <- split_label
    } else {
      split_name <- "all obs"
    }
  }
  check_ok_label(split_label)
  new("AllSplit",
    split_label = split_label,
    content_fun = cfun,
    content_format = cformat,
    content_na_str = cna_str,
    split_format = split_format,
    split_na_str = split_na_str,
    name = split_name,
    label_children = FALSE,
    extra_args = extra_args,
    indent_modifier = as.integer(indent_mod),
    content_indent_modifier = as.integer(cindent_mod),
    content_var = cvar,
    split_label_position = "hidden",
    content_extra_args = cextra_args,
    page_title_prefix = NA_character_,
    child_section_div = NA_character_
  )
}

setClass("RootSplit", contains = "AllSplit")

RootSplit <- function(split_label = "", cfun = NULL, cformat = NULL, cna_str = NA_character_, cvar = "",
                      split_format = NULL, split_na_str = NA_character_, cextra_args = list(), ...) {
  check_ok_label(split_label)
  new("RootSplit",
    split_label = split_label,
    content_fun = cfun,
    content_format = cformat,
    content_na_str = cna_str,
    split_format = split_format,
    split_na_str = split_na_str,
    name = "root",
    label_children = FALSE,
    indent_modifier = 0L,
    content_indent_modifier = 0L,
    content_var = cvar,
    split_label_position = "hidden",
    content_extra_args = cextra_args,
    child_section_div = NA_character_
  )
}

setClass("ManualSplit",
  contains = "AllSplit",
  representation(levels = "character")
)

#' Manually defined split
#'
#' @inheritParams lyt_args
#' @inheritParams constr_args
#' @inheritParams gen_args
#' @param levels character. Levels of the split (i.e. the children of the manual
#'   split)
#' @author Gabriel Becker
#' @return A \code{ManualSplit} object.
#' @export
ManualSplit <- function(levels, label, name = "manual",
                        extra_args = list(),
                        indent_mod = 0L,
                        cindent_mod = 0L,
                        cvar = "",
                        cextra_args = list(),
                        label_pos = "visible",
                        page_prefix = NA_character_,
                        section_div = NA_character_) {
  label_pos <- match.arg(label_pos, label_pos_values)
  check_ok_label(label, multi_ok = TRUE)
  new("ManualSplit",
    split_label = label,
    levels = levels,
    name = name,
    label_children = FALSE,
    extra_args = extra_args,
    indent_modifier = 0L,
    content_indent_modifier = as.integer(cindent_mod),
    content_var = cvar,
    split_format = NULL,
    split_na_str = NA_character_,
    split_label_position = label_pos,
    page_title_prefix = page_prefix,
    child_section_div = section_div
  )
}


## splits across which variables are being analynzed
setClass("MultiVarSplit",
  contains = "CustomizableSplit", ## "Split",
  representation(
    var_labels = "character",
    var_names = "character"
  ),
  validity = function(object) {
    length(object@payload) >= 1 &&
      all(!is.na(object@payload)) &&
      (length(object@var_labels) == 0 || length(object@payload) == length(object@var_labels))
  }
)


.make_suffix_vec <- function(n) {
  c(
    "",
    sprintf(
      "._[[%d]]_.",
      seq_len(n - 1) + 1L
    )
  )
}

.make_multivar_names <- function(vars) {
  dups <- duplicated(vars)
  if (!any(dups)) {
    return(vars)
  }
  dupvars <- unique(vars[dups])
  ret <- vars
  for (v in dupvars) {
    pos <- which(ret == v)
    ret[pos] <- paste0(
      ret[pos],
      .make_suffix_vec(length(pos))
    )
  }
  ret
}
#' Split between two or more different variables
#'
#' @inheritParams lyt_args
#' @inheritParams constr_args
#' @author Gabriel Becker
#' @return A \code{MultiVarSplit} object.
#' @export
MultiVarSplit <- function(vars,
                          split_label = "",
                          varlabels = NULL,
                          varnames = NULL,
                          cfun = NULL,
                          cformat = NULL,
                          cna_str = NA_character_,
                          split_format = NULL,
                          split_na_str = NA_character_,
                          split_name = "multivars",
                          child_labels = c("default", "visible", "hidden"),
                          extra_args = list(),
                          indent_mod = 0L,
                          cindent_mod = 0L,
                          cvar = "",
                          cextra_args = list(),
                          label_pos = "visible",
                          split_fun = NULL,
                          page_prefix = NA_character_,
                          section_div = NA_character_) {
  check_ok_label(split_label)
  ## no topleft allowed
  label_pos <- match.arg(label_pos, label_pos_values[-3])
  child_labels <- match.arg(child_labels)
  if (length(vars) == 1 && grepl(":", vars)) {
    vars <- strsplit(vars, ":")[[1]]
  }
  if (length(varlabels) == 0) { ## covers NULL and character()
    varlabels <- vars
  }
  vnames <- varnames %||% .make_multivar_names(vars)
  stopifnot(length(vnames) == length(vars))
  new("MultiVarSplit",
    payload = vars,
    split_label = split_label,
    var_labels = varlabels,
    var_names = vnames,
    content_fun = cfun,
    content_format = cformat,
    content_na_str = cna_str,
    split_format = split_format,
    split_na_str = split_na_str,
    label_children = .labelkids_helper(child_labels),
    name = split_name,
    extra_args = extra_args,
    indent_modifier = as.integer(indent_mod),
    content_indent_modifier = as.integer(cindent_mod),
    content_var = cvar,
    split_label_position = label_pos,
    content_extra_args = cextra_args,
    split_fun = split_fun,
    page_title_prefix = page_prefix,
    child_section_div = section_div
  )
}


#' Splits for cutting by values of a numeric variable
#' @rdname cutsplits
#' @inheritParams lyt_args
#' @inheritParams constr_args
#' @exportClass VarStaticCutSplit
setClass("VarStaticCutSplit",
  contains = "Split",
  representation(
    cuts = "numeric",
    cut_labels = "character"
  )
)

.is_cut_lab_lst <- function(cuts) {
  is.list(cuts) && is.numeric(cuts[[1]]) &&
    is.character(cuts[[2]]) &&
    length(cuts[[1]]) == length(cuts[[2]])
}

#' Create static cut or static cumulative cut split
#' @inheritParams lyt_args
#' @inheritParams constr_args
#' @rdname cutsplits
#' @return a \code{VarStaticCutSplit}, \code{CumulativeCutSplit} object for
#'   \code{make_static_cut_split}, or a \code{VarDynCutSplit} object for
#'   \code{VarDynCutSplit()}
make_static_cut_split <- function(var,
                                  split_label,
                                  cuts,
                                  cutlabels = NULL,
                                  cfun = NULL,
                                  cformat = NULL,
                                  cna_str = NA_character_,
                                  split_format = NULL,
                                  split_na_str = NA_character_,
                                  split_name = var,
                                  child_labels = c("default", "visible", "hidden"),
                                  extra_args = list(),
                                  indent_mod = 0L,
                                  cindent_mod = 0L,
                                  cvar = "",
                                  cextra_args = list(),
                                  label_pos = "visible",
                                  cumulative = FALSE,
                                  page_prefix = NA_character_,
                                  section_div = NA_character_) {
  cls <- if (cumulative) "CumulativeCutSplit" else "VarStaticCutSplit"
  check_ok_label(split_label)

  label_pos <- match.arg(label_pos, label_pos_values)
  child_labels <- match.arg(child_labels)
  if (.is_cut_lab_lst(cuts)) {
    cutlabels <- cuts[[2]]
    cuts <- cuts[[1]]
  }
  if (is.unsorted(cuts, strictly = TRUE)) {
    stop("invalid cuts vector. not sorted unique values.")
  }

  if (is.null(cutlabels) && !is.null(names(cuts))) {
    cutlabels <- names(cuts)[-1]
  } ## XXX is this always right?

  new(cls,
    payload = var,
    split_label = split_label,
    cuts = cuts,
    cut_labels = cutlabels,
    content_fun = cfun,
    content_format = cformat,
    content_na_str = cna_str,
    split_format = split_format,
    split_na_str = split_na_str,
    name = split_name,
    label_children = .labelkids_helper(child_labels),
    extra_args = extra_args,
    indent_modifier = as.integer(indent_mod),
    content_indent_modifier = as.integer(cindent_mod),
    content_var = cvar,
    split_label_position = label_pos,
    content_extra_args = cextra_args,
    page_title_prefix = page_prefix,
    child_section_div = section_div
  )
}



#' @rdname cutsplits
#' @exportClass CumulativeCutSplit

setClass("CumulativeCutSplit", contains = "VarStaticCutSplit")

## make_static_cut_split with cumulative=TRUE is the constructor
## for CumulativeCutSplit


## do we want this to be a CustomizableSplit instead of
## taking cut_fun?
## cut_funct must take avector and no other arguments
## and return a named vector of cut points
#' @rdname cutsplits
#' @exportClass VarDynCutSplit
setClass("VarDynCutSplit",
  contains = "Split",
  representation(
    cut_fun = "function",
    cut_label_fun = "function",
    cumulative_cuts = "logical"
  )
)
#' @rdname cutsplits
#' @export
VarDynCutSplit <- function(var,
                           split_label,
                           cutfun,
                           cutlabelfun = function(x) NULL,
                           cfun = NULL,
                           cformat = NULL,
                           cna_str = NA_character_,
                           split_format = NULL,
                           split_na_str = NA_character_,
                           split_name = var,
                           child_labels = c("default", "visible", "hidden"),
                           extra_args = list(),
                           cumulative = FALSE,
                           indent_mod = 0L,
                           cindent_mod = 0L,
                           cvar = "",
                           cextra_args = list(),
                           label_pos = "visible",
                           page_prefix = NA_character_,
                           section_div = NA_character_) {
  check_ok_label(split_label)
  label_pos <- match.arg(label_pos, label_pos_values)
  child_labels <- match.arg(child_labels)
  new("VarDynCutSplit",
    payload = var,
    split_label = split_label,
    cut_fun = cutfun,
    cumulative_cuts = cumulative,
    cut_label_fun = cutlabelfun,
    content_fun = cfun,
    content_format = cformat,
    content_na_str = cna_str,
    split_format = split_format,
    split_na_str = split_na_str,
    name = split_name,
    label_children = .labelkids_helper(child_labels),
    extra_args = extra_args,
    indent_modifier = as.integer(indent_mod),
    content_indent_modifier = as.integer(cindent_mod),
    content_var = cvar,
    split_label_position = label_pos,
    content_extra_args = cextra_args,
    page_title_prefix = page_prefix,
    child_section_div = section_div
  )
}



## NB analyze splits can't have content-related things
setClass("VAnalyzeSplit",
  contains = "Split",
  representation(
    default_rowlabel = "character",
    include_NAs = "logical",
    var_label_position = "character"
  )
)

setClass("AnalyzeVarSplit",
  contains = "VAnalyzeSplit",
  representation(analysis_fun = "function")
)

setClass("AnalyzeColVarSplit",
  contains = "VAnalyzeSplit",
  representation(analysis_fun = "list")
)

#' Define a subset tabulation/analysis
#'
#' @inheritParams lyt_args
#' @inheritParams constr_args
#' @param defrowlab character. Default row labels if they are not specified by
#'   the return value of \code{afun}
#' @rdname avarspl
#' @return An \code{AnalyzeVarSplit} object.
#' @author Gabriel Becker
#' @export
AnalyzeVarSplit <- function(var,
                            split_label = var,
                            afun,
                            defrowlab = "",
                            cfun = NULL,
                            cformat = NULL,
                            split_format = NULL,
                            split_na_str = NA_character_,
                            inclNAs = FALSE,
                            split_name = var,
                            extra_args = list(),
                            indent_mod = 0L,
                            label_pos = "default",
                            cvar = "") {
  check_ok_label(split_label)
  label_pos <- match.arg(label_pos, c("default", label_pos_values))
  if (!any(nzchar(defrowlab))) {
    defrowlab <- as.character(substitute(afun))
    if (length(defrowlab) > 1 || startsWith(defrowlab, "function(")) {
      defrowlab <- ""
    }
  }
  new("AnalyzeVarSplit",
    payload = var,
    split_label = split_label,
    content_fun = cfun,
    analysis_fun = afun,
    content_format = cformat,
    split_format = split_format,
    split_na_str = split_na_str,
    default_rowlabel = defrowlab,
    include_NAs = inclNAs,
    name = split_name,
    label_children = FALSE,
    extra_args = extra_args,
    indent_modifier = as.integer(indent_mod),
    content_indent_modifier = 0L,
    var_label_position = label_pos,
    content_var = cvar,
    page_title_prefix = NA_character_,
    child_section_div = NA_character_
  ) ## no content_extra_args
}

#' Define a subset tabulation/analysis
#'
#' @inheritParams lyt_args
#' @inheritParams constr_args
#' @rdname avarspl
#' @author Gabriel Becker
#' @export
AnalyzeColVarSplit <- function(afun,
                               defrowlab = "",
                               cfun = NULL,
                               cformat = NULL,
                               split_format = NULL,
                               split_na_str = NA_character_,
                               inclNAs = FALSE,
                               split_name = "",
                               extra_args = list(),
                               indent_mod = 0L,
                               label_pos = "default",
                               cvar = "") {
  label_pos <- match.arg(label_pos, c("default", label_pos_values))
  new("AnalyzeColVarSplit",
    payload = NA_character_,
    split_label = "",
    content_fun = cfun,
    analysis_fun = afun,
    content_format = cformat,
    split_format = split_format,
    split_na_str = split_na_str,
    default_rowlabel = defrowlab,
    include_NAs = inclNAs,
    name = split_name,
    label_children = FALSE,
    extra_args = extra_args,
    indent_modifier = as.integer(indent_mod),
    content_indent_modifier = 0L,
    var_label_position = label_pos,
    content_var = cvar,
    page_title_prefix = NA_character_,
    child_section_div = NA_character_
  ) ## no content_extra_args
}

setClass("CompoundSplit",
  contains = "Split",
  validity = function(object) are(object@payload, "Split")
)


setClass("AnalyzeMultiVars", contains = "CompoundSplit")

.repoutlst <- function(x, nv) {
  if (!is.function(x) && length(x) == nv) {
    return(x)
  }
  if (!is(x, "list")) {
    x <- list(x)
  }
  rep(x, length.out = nv)
}


.uncompound <- function(csplit) {
  if (is(csplit, "list")) {
    return(unlist(lapply(csplit, .uncompound)))
  }

  if (!is(csplit, "CompoundSplit")) {
    return(csplit)
  }

  pld <- spl_payload(csplit)
  done <- all(!vapply(pld, is, TRUE, class2 = "CompoundSplit"))
  if (done) {
    pld
  } else {
    unlist(lapply(pld, .uncompound))
  }
}

strip_compound_name <- function(obj) {
  nm <- obj_name(obj)
  gsub("^ma_", "", nm)
}

make_ma_name <- function(spl, pld = spl_payload(spl)) {
  paste(
    c(
      "ma",
      vapply(pld, strip_compound_name, "")
    ),
    collapse = "_"
  )
}

#' @rdname avarspl
#' @param .payload Used internally, not intended to be set by end users.
#' @return An \code{AnalyzeMultiVars} split object.
#' @export
AnalyzeMultiVars <- function(var,
                             split_label = "",
                             afun,
                             defrowlab = "",
                             cfun = NULL,
                             cformat = NULL,
                             split_format = NULL,
                             split_na_str = NA_character_,
                             inclNAs = FALSE,
                             .payload = NULL,
                             split_name = NULL,
                             extra_args = list(),
                             indent_mod = 0L,
                             child_labels = c(
                               "default", "topleft", "visible",
                               "hidden"
                             ),
                             child_names = var,
                             cvar = "",
                             section_div = NA_character_) {
  ## NB we used to resolve to strict TRUE/FALSE for label visibillity
  ## in this function but that was too greedy for repeated
  ## analyze calls, so that now occurs in the tabulation machinery
  ## when the table is actually being built.
  ##  show_kidlabs = .labelkids_helper(match.arg(child_labels))
  child_labels <- match.arg(child_labels)
  show_kidlabs <- child_labels
  if (is.null(.payload)) {
    nv <- length(var)
    defrowlab <- .repoutlst(defrowlab, nv)
    afun <- .repoutlst(afun, nv)
    split_label <- .repoutlst(split_label, nv)
    check_ok_label(split_label, multi_ok = TRUE)
    cfun <- .repoutlst(cfun, nv)
    cformat <- .repoutlst(cformat, nv)
    ##        split_format = .repoutlst(split_format, nv)
    inclNAs <- .repoutlst(inclNAs, nv)
    pld <- mapply(AnalyzeVarSplit,
      var = var,
      split_name = child_names,
      split_label = split_label,
      afun = afun,
      defrowlab = defrowlab,
      cfun = cfun,
      cformat = cformat,
      ##                     split_format = split_format,
      inclNAs = inclNAs,
      MoreArgs = list(
        extra_args = extra_args,
        indent_mod = indent_mod,
        label_pos = show_kidlabs,
        split_format = split_format,
        split_na_str = split_na_str
      ), ## rvis),
      SIMPLIFY = FALSE
    )
  } else {
    ## we're combining existing splits here
    pld <- unlist(lapply(.payload, .uncompound))

    ## only override the childen being combined if the constructor
    ## was passed a non-default value for child_labels
    ## and the child was at NA before
    pld <- lapply(
      pld,
      function(x) {
        rvis <- label_position(x) ## labelrow_visible(x)
        if (!identical(show_kidlabs, "default")) { ## is.na(show_kidlabs)) {
          if (identical(rvis, "default")) { ## ois.na(rvis))
            rvis <- show_kidlabs
          }
        }
        label_position(x) <- rvis
        x
      }
    )
  }
  if (length(pld) == 1) {
    ret <- pld[[1]]
  } else {
    if (is.null(split_name)) {
      split_name <- paste(c("ma", vapply(pld, obj_name, "")),
        collapse = "_"
      )
    }
    ret <- new("AnalyzeMultiVars",
      payload = pld,
      split_label = "",
      split_format = NULL,
      split_na_str = split_na_str,
      content_fun = NULL,
      content_format = NULL,
      ## I beleive this is superfluous now
      ## the payloads carry aroudn the real instructions
      ## XXX
      label_children = .labelkids_helper(show_kidlabs),
      split_label_position = "hidden", ## XXX is this right?
      name = split_name,
      extra_args = extra_args,
      ## modifier applied on splits in payload
      indent_modifier = 0L,
      content_indent_modifier = 0L,
      content_var = cvar,
      page_title_prefix = NA_character_,
      child_section_div = section_div
    )
  }
  ret
}
setClass("VarLevWBaselineSplit",
  contains = "VarLevelSplit",
  representation(
    var = "character",
    ref_group_value = "character"
  )
) # ,

#' @rdname VarLevelSplit
#' @export
VarLevWBaselineSplit <- function(var,
                                 ref_group,
                                 labels_var = var,
                                 split_label,
                                 split_fun = NULL,
                                 label_fstr = "%s - %s",
                                 ## not needed I Think...
                                 cfun = NULL,
                                 cformat = NULL,
                                 cna_str = NA_character_,
                                 cvar = "",
                                 split_format = NULL,
                                 split_na_str = NA_character_,
                                 valorder = NULL,
                                 split_name = var,
                                 extra_args = list()) {
  check_ok_label(split_label)
  new("VarLevWBaselineSplit",
    payload = var,
    ref_group_value = ref_group,
    ## This will occur at the row level not on the column split, for now
    ## TODO revisit this to confirm its right
    ##        comparison_func = comparison,
    #      label_format = label_fstr,
    value_label_var = labels_var,
    split_label = split_label,
    content_fun = cfun,
    content_format = cformat,
    content_na_str = cna_str,
    split_format = split_format,
    split_na_str = split_na_str,
    split_fun = split_fun,
    name = split_name,
    label_children = FALSE,
    extra_args = extra_args,
    ## this is always a column split
    indent_modifier = 0L,
    content_indent_modifier = 0L,
    content_var = cvar,
    ## so long as this is columnspace only
    page_title_prefix = NA_character_,
    child_section_div = NA_character_
  )
}


.chkname <- function(nm) {
  if (is.null(nm)) {
    nm <- ""
  }
  if (length(nm) != 1) {
    stop("name is not of length one")
  } else if (is.na(nm)) {
    warning("Got missing value for name, converting to characters '<NA>'")
    nm <- "<NA>"
  }
  nm
}




###
### Tree Position Representation
###
### Class(es) that represent position with in a
### tree as parallel vectors of Split objects and
### values chosen at that split, plus labeling info

TreePos <- function(spls = list(),
                    svals = list(),
                    svlabels = character(),
                    sub = NULL) {
  check_ok_label(svlabels, multi_ok = TRUE)
  svals <- make_splvalue_vec(vals = svals)
  if (is.null(sub)) {
    if (length(spls) > 0) {
      sub <- make_pos_subset(
        spls = spls,
        svals = svals
      )
    } else {
      sub <- expression(TRUE)
    }
  }
  new("TreePos",
    splits = spls, s_values = svals,
    sval_labels = svlabels,
    subset = sub
  )
}

##
## Tree position convenience functions
##

make_child_pos <- function(parpos,
                           newspl,
                           newval,
                           newlab = newval,
                           newextra = list()) {
  if (!is(newval, "SplitValue")) {
    nsplitval <- SplitValue(newval, extr = newextra, label = newlab)
  } else {
    nsplitval <- newval
  }
  check_ok_label(newlab)
  newpos <- TreePos(
    spls = c(pos_splits(parpos), newspl),
    svals = c(pos_splvals(parpos), nsplitval),
    svlabels = c(pos_splval_labels(parpos), newlab),
    sub = .combine_subset_exprs(
      pos_subset(parpos),
      make_subset_expr(newspl, nsplitval)
    )
  )
  newpos
}


###
### Virtual Classes
###
### Virtual class hiearchy for the various types of
### trees in use in the S4 implementation of the TableTree
### machinery
###

## core basics
setClass("VNodeInfo",
  contains = "VIRTUAL",
  representation(
    level = "integer",
    name = "character" ## ,
    ## label = "character"
  )
)
setClass("VTree",
  contains = c("VIRTUAL", "VNodeInfo"),
  representation(children = "list")
)
setClass("VLeaf", contains = c("VIRTUAL", "VNodeInfo"))


## Layout trees
# setClass("VLayoutNode", contains= c("VIRTUAL", "VNodeInfo"))
setClass("VLayoutLeaf",
  contains = c("VIRTUAL", "VLeaf"),
  representation(
    pos_in_tree = "TreePos",
    label = "character"
  )
)
setClass("VLayoutTree",
  contains = c("VIRTUAL", "VTree"),
  representation(
    split = "Split",
    pos_in_tree = "TreePos",
    label = "character"
  )
)
setClassUnion("VLayoutNode", c("VLayoutLeaf", "VLayoutTree"))




##
## LayoutAxisTree classes
##

setOldClass("function")
setOldClass("NULL")
setClassUnion("FunctionOrNULL", c("function", "NULL"))

setClass("LayoutAxisTree",
  contains = "VLayoutTree",
  representation(summary_func = "FunctionOrNULL"),
  validity = function(object) {
    all(sapply(object@children, function(x) is(x, "LayoutAxisTree") || is(x, "LayoutAxisLeaf")))
  }
)

setClass("LayoutAxisLeaf",
  contains = "VLayoutLeaf", ## "VNodeInfo",
  representation(
    func = "function",
    col_footnotes = "list"
  )
)


setClass("LayoutColTree",
  contains = "LayoutAxisTree",
  representation(
    display_columncounts = "logical",
    columncount_format = "character",
    col_footnotes = "list"
  )
)

setClass("LayoutColLeaf", contains = "LayoutAxisLeaf")
LayoutColTree <- function(lev = 0L,
                          name = obj_name(spl),
                          label = obj_label(spl),
                          kids = list(),
                          spl = EmptyAllSplit,
                          tpos = TreePos(),
                          summary_function = NULL,
                          disp_colcounts = FALSE,
                          colcount_format = "(N=xx)",
                          footnotes = list()) { ## ,
  ## sub = expression(TRUE),
  ## svar = NA_character_,
  ## slab = NA_character_) {
  if (is.null(spl)) {
    stop(
      "LayoutColTree constructor got NULL for spl. ", # nocov
      "This should never happen. Please contact the maintainer."
    )
  } # nocov
  footnotes <- make_ref_value(footnotes)
  check_ok_label(label)
  new("LayoutColTree",
    level = lev, children = kids,
    name = .chkname(name),
    summary_func = summary_function,
    pos_in_tree = tpos,
    split = spl,
    ## subset = sub,
    ## splitvar = svar,
    label = label,
    display_columncounts = disp_colcounts,
    columncount_format = colcount_format,
    col_footnotes = footnotes
  )
}

LayoutColLeaf <- function(lev = 0L,
                          name = label,
                          label = "",
                          tpos = TreePos()) {
  check_ok_label(label)
  new("LayoutColLeaf",
    level = lev, name = .chkname(name), label = label,
    pos_in_tree = tpos ## ,
    ## subset = sub#,
    ## N_count = n,
    ## splitvar = svar
  )
}



## Instantiated column info class
##
## This is so we don't need multiple arguments
## in the recursive functions that track
## various aspects of the column layout
## once its applied to the data.

#' `InstantiatedColumnInfo`
#' @inheritParams gen_args
#' @exportClass InstantiatedColumnInfo
#' @rdname cinfo
setClass(
  "InstantiatedColumnInfo",
  representation(
    tree_layout = "VLayoutNode", ## LayoutColTree",
    subset_exprs = "list",
    cextra_args = "list",
    counts = "integer",
    total_count = "integer",
    display_columncounts = "logical",
    columncount_format = "FormatSpec",
    columncount_na_str = "character",
    top_left = "character"
  )
)

#' @rdname cinfo
#' @export
#' @param treelyt `LayoutColTree`.
#' @param csubs list. List of subsetting expressions
#' @param extras list. Extra arguments associated with the columns
#' @param cnts integer. Counts.
#' @param total_cnt integer(1). Total observations represented across all
#'   columns.
#' @param dispcounts logical(1). Should the counts be displayed as header info when
#'   the associated table is printed.
#' @param countformat character(1). Format for the counts if they are displayed
#' @param count_na_str character. NA string to be used when formatting counts. Defaults
#'   to `""`.
#' @return an \code{InstantiateadColumnInfo} object.
InstantiatedColumnInfo <- function(treelyt = LayoutColTree(),
                                   csubs = list(expression(TRUE)),
                                   extras = list(list()),
                                   cnts = NA_integer_,
                                   total_cnt = NA_integer_,
                                   dispcounts = FALSE,
                                   countformat = "(N=xx)",
                                   count_na_str = "",
                                   topleft = character()) {
  leaves <- collect_leaves(treelyt)
  nl <- length(leaves)
  extras <- rep(extras, length.out = nl)
  cnts <- rep(cnts, length.out = nl)
  csubs <- rep(csubs, length.out = nl)



  nleaves <- length(leaves)
  snas <- sum(is.na(cnts))
  if (length(csubs) != nleaves || length(extras) != nleaves || length(cnts) != nleaves) {
    stop(
      "Mismatching number of columns indicated by: csubs [",
      length(csubs), "], ",
      "treelyt [", nl, "], extras [", length(extras),
      "] and counts [", cnts, "]."
    )
  }
  if (snas != 0 && snas != nleaves) {
    warning(
      "Mixture of missing and non-missing column counts when ",
      "creating column info."
    )
  }

  new("InstantiatedColumnInfo",
    tree_layout = treelyt,
    subset_exprs = csubs,
    cextra_args = extras,
    counts = cnts,
    total_count = total_cnt,
    display_columncounts = dispcounts,
    columncount_format = countformat,
    columncount_na_str = count_na_str,
    top_left = topleft
  )
}





## TableTrees
## XXX Rowspans as implemented dont really work
## they're aren't attached to the right data structures
## during conversions.

## FIXME: if we ever actually need row spanning
setClass("VTableNodeInfo",
  contains = c("VNodeInfo", "VIRTUAL"),
  representation(
    ## col_layout = "VLayoutNode",
    col_info = "InstantiatedColumnInfo",
    format = "FormatSpec",
    na_str = "character",
    indent_modifier = "integer",
    table_inset = "integer"
  )
)

setClass("TableRow",
  contains = c("VIRTUAL", "VLeaf", "VTableNodeInfo"),
  representation(
    leaf_value = "ANY",
    var_analyzed = "character",
    ##         var_label = "character",
    label = "character",
    row_footnotes = "list"
  )
)




### TableTree Core non-virtual Classes
#' Row classes and constructors
#' @rdname rowclasses
#' @inheritParams constr_args
#' @inheritParams lyt_args
#' @param vis logical. Should the row be visible (\code{LabelRow} only).
#' @author Gabriel Becker
#' @return A formal object representing a table row of the constructed type.
#' @export
LabelRow <- function(lev = 1L,
                     label = "",
                     name = label,
                     vis = !is.na(label) && nzchar(label),
                     cinfo = EmptyColInfo,
                     indent_mod = 0L,
                     table_inset = 0L) {
  check_ok_label(label)
  new("LabelRow",
    leaf_value = list(),
    level = lev,
    label = label,
    ## XXX this means that a label row and its talbe can have the same name....
    ## XXX that is bad but how bad remains to be seen
    ## XXX
    name = .chkname(name),
    col_info = cinfo,
    visible = vis,
    indent_modifier = as.integer(indent_mod),
    table_inset = as.integer(table_inset)
  )
}

#' Row constructors and Classes
#' @rdname rowclasses
#' @exportClass DataRow
setClass("DataRow",
  contains = "TableRow",
  representation(colspans = "integer") ## ,
  ## pos_in_tree = "TableRowPos"),
  ##      validity = function(object) {
  ## lcsp = length(object@colspans)
  ## length(lcsp ==  0) || lcsp == length(object@leaf_value)
  ## }
)

#' @rdname rowclasses
#' @exportClass ContentRow
setClass("ContentRow",
  contains = "TableRow",
  representation(colspans = "integer") ## ,
  ## pos_in_tree = "TableRowPos"),
  ##      validity = function(object) {
  ## lcsp = length(object@colspans)
  ## length(lcsp ==  0) || lcsp == length(object@leaf_value)
  ## }
)

#' @rdname rowclasses
#' @exportClass LabelRow
setClass("LabelRow",
  contains = "TableRow",
  representation(visible = "logical")
)



#' @rdname rowclasses
#' @param klass Internal detail.
#'
#' @export
.tablerow <- function(vals = list(),
                      name = "",
                      lev = 1L,
                      label = name,
                      cspan = rep(1L, length(vals)),
                      cinfo = EmptyColInfo,
                      var = NA_character_,
                      format = NULL,
                      na_str = NA_character_,
                      klass,
                      indent_mod = 0L,
                      footnotes = list(),
                      table_inset = 0L) {
  if ((missing(name) || is.null(name) || is.na(name) || nchar(name) == 0) && !missing(label)) {
    name <- label
  }
  vals <- lapply(vals, rcell)
  rlabels <- unique(unlist(lapply(vals, obj_label)))
  if ((missing(label) || is.null(label) || identical(label, "")) && sum(nzchar(rlabels)) == 1) {
    label <- rlabels[nzchar(rlabels)]
  }
  if (missing(cspan) && !is.null(unlist(lapply(vals, cell_cspan)))) {
    cspan <- vapply(vals, cell_cspan, 0L)
  }

  check_ok_label(label)
  rw <- new(klass,
    leaf_value = vals,
    name = .chkname(name),
    level = lev,
    label = .chkname(label),
    colspans = cspan,
    col_info = cinfo,
    var_analyzed = var,
    ## these are set in set_format_recursive below
    format = NULL,
    na_str = NA_character_,
    indent_modifier = indent_mod,
    row_footnotes = footnotes,
    table_inset = table_inset
  )
  rw <- set_format_recursive(rw, format, na_str, FALSE)
  rw
}

#' @rdname rowclasses
#' @param \dots passed to shared constructor (\code{.tablerow}).
#' @export
DataRow <- function(...) .tablerow(..., klass = "DataRow")
#' @rdname rowclasses
#' @export
ContentRow <- function(...) .tablerow(..., klass = "ContentRow")

setClass("VTitleFooter",
  contains = "VIRTUAL",
  representation(
    main_title = "character",
    subtitles = "character",
    main_footer = "character",
    provenance_footer = "character"
  )
)


setClass("VTableTree",
  contains = c("VIRTUAL", "VTableNodeInfo", "VTree", "VTitleFooter"),
  representation(
    children = "list",
    rowspans = "data.frame",
    labelrow = "LabelRow",
    page_titles = "character",
    horizontal_sep = "character",
    trailing_section_div = "character"
  )
)

setClassUnion("IntegerOrNull", c("integer", "NULL"))
## covered because it's ElementaryTable's validity method but covr misses it
## nocov start
etable_validity <- function(object) {
  kids <- tree_children(object)
  all(sapply(
    kids,
    function(k) {
      (is(k, "DataRow") || is(k, "ContentRow"))
    }
  )) ###  &&
}
## nocov end

#' `TableTree` classes
#' @exportClass ElementaryTable
#' @author Gabriel Becker
#' @return A formal object representing a populated table.
#' @rdname tabclasses
setClass("ElementaryTable",
  contains = "VTableTree",
  representation(var_analyzed = "character"),
  validity = etable_validity ## function(object) {
)

.enforce_valid_kids <- function(lst, colinfo) {
  ## colinfo
  if (!no_colinfo(colinfo)) {
    lst <- lapply(
      lst,
      function(x) {
        if (no_colinfo(x)) {
          col_info(x) <- colinfo
        } else if (!identical(colinfo, col_info(x), ignore.environment = TRUE)) {
          ## split functions from function factories (e.g. add_combo_levels)
          ## have different environments so we can't use identical here
          ## all.equal requires the **values within the closures** to be the
          ## same but not the actual enclosing environments.
          stop(
            "attempted to add child with non-matching, non-empty ",
            "column info to an existing table"
          )
        }
        x
      }
    )
  }

  if (are(lst, "ElementaryTable") &&
    all(sapply(lst, function(tb) {
      nrow(tb) <= 1 && identical(obj_name(tb), "")
    }))) {
    lst <- unlist(lapply(lst, function(tb) tree_children(tb)[[1]]))
  }
  if (length(lst) == 0) {
    return(list())
  }
  ## names
  realnames <- sapply(lst, obj_name)
  lstnames <- names(lst)
  if (is.null(lstnames)) {
    names(lst) <- realnames
  } else if (!identical(realnames, lstnames)) {
    names(lst) <- realnames
  }

  lst
}

#' Table Constructors and Classes
#' @inheritParams constr_args
#' @inheritParams gen_args
#' @inheritParams lyt_args
#' @param rspans data.frame. Currently stored but otherwise ignored.
#' @rdname tabclasses
#' @author Gabriel Becker
#' @export
ElementaryTable <- function(kids = list(),
                            name = "",
                            lev = 1L,
                            label = "",
                            labelrow = LabelRow(
                              lev = lev,
                              label = label,
                              vis = !isTRUE(iscontent) &&
                                !is.na(label) &&
                                nzchar(label)
                            ),
                            rspans = data.frame(),
                            cinfo = NULL,
                            iscontent = NA,
                            var = NA_character_,
                            format = NULL,
                            na_str = NA_character_,
                            indent_mod = 0L,
                            title = "",
                            subtitles = character(),
                            main_footer = character(),
                            prov_footer = character(),
                            hsep = default_hsep(),
                            trailing_sep = NA_character_,
                            inset = 0L) {
  check_ok_label(label)
  if (is.null(cinfo)) {
    if (length(kids) > 0) {
      cinfo <- col_info(kids[[1]])
    } else {
      cinfo <- EmptyColInfo
    }
  }

  if (no_colinfo(labelrow)) {
    col_info(labelrow) <- cinfo
  }
  kids <- .enforce_valid_kids(kids, cinfo)
  tab <- new("ElementaryTable",
    children = kids,
    name = .chkname(name),
    level = lev,
    labelrow = labelrow,
    rowspans = rspans,
    col_info = cinfo,
    var_analyzed = var,
    ## XXX these are hardcoded, because they both get set during
    ## set_format_recursive anyway
    format = NULL,
    na_str = NA_character_,
    table_inset = 0L,
    indent_modifier = as.integer(indent_mod),
    main_title = title,
    subtitles = subtitles,
    main_footer = main_footer,
    provenance_footer = prov_footer,
    horizontal_sep = hsep,
    trailing_section_div = trailing_sep
  )
  tab <- set_format_recursive(tab, format, na_str, FALSE)
  table_inset(tab) <- as.integer(inset)
  tab
}

ttable_validity <- function(object) {
  all(sapply(
    tree_children(object),
    function(x) is(x, "VTableTree") || is(x, "TableRow")
  ))
}

.calc_cinfo <- function(cinfo, cont, kids) {
  if (!is.null(cinfo)) {
    cinfo
  } else if (!is.null(cont)) {
    col_info(cont)
  } else if (length(kids) >= 1) {
    col_info(kids[[1]])
  } else {
    EmptyColInfo
  }
}
## under this model, non-leaf nodes can have a content table where rollup
## analyses live
#' @rdname tabclasses
#' @exportClass TableTree
setClass("TableTree",
  contains = c("VTableTree"),
  representation(
    content = "ElementaryTable",
    page_title_prefix = "character"
  ),
  validity = ttable_validity
)

#' @rdname tabclasses
#' @export
TableTree <- function(kids = list(),
                      name = if (!is.na(var)) var else "",
                      cont = EmptyElTable,
                      lev = 1L,
                      label = name,
                      labelrow = LabelRow(
                        lev = lev,
                        label = label,
                        vis = nrow(cont) == 0 && !is.na(label) &&
                          nzchar(label)
                      ),
                      rspans = data.frame(),
                      iscontent = NA,
                      var = NA_character_,
                      cinfo = NULL,
                      format = NULL,
                      na_str = NA_character_,
                      indent_mod = 0L,
                      title = "",
                      subtitles = character(),
                      main_footer = character(),
                      prov_footer = character(),
                      page_title = NA_character_,
                      hsep = default_hsep(),
                      trailing_sep = NA_character_,
                      inset = 0L) {
  check_ok_label(label)
  cinfo <- .calc_cinfo(cinfo, cont, kids)

  kids <- .enforce_valid_kids(kids, cinfo)
  if (isTRUE(iscontent) && !is.null(cont) && nrow(cont) > 0) {
    stop("Got table tree with content table and content position")
  }
  if (no_colinfo(labelrow)) {
    col_info(labelrow) <- cinfo
  }
  if ((is.null(cont) || nrow(cont) == 0) && all(sapply(kids, is, "DataRow"))) {
    if (!is.na(page_title)) {
      stop("Got a page title prefix for an Elementary Table")
    }
    ## constructor takes care of recursive format application
    ElementaryTable(
      kids = kids,
      name = .chkname(name),
      lev = lev,
      labelrow = labelrow,
      rspans = rspans,
      cinfo = cinfo,
      var = var,
      format = format,
      na_str = na_str,
      indent_mod = indent_mod,
      title = title,
      subtitles = subtitles,
      main_footer = main_footer,
      prov_footer = prov_footer,
      hsep = hsep,
      trailing_sep = trailing_sep,
      inset = inset
    )
  } else {
    tab <- new("TableTree",
      content = cont,
      children = kids,
      name = .chkname(name),
      level = lev,
      labelrow = labelrow,
      rowspans = rspans,
      col_info = cinfo,
      format = NULL,
      na_str = na_str,
      table_inset = 0L,
      indent_modifier = as.integer(indent_mod),
      main_title = title,
      subtitles = subtitles,
      main_footer = main_footer,
      provenance_footer = prov_footer,
      page_title_prefix = page_title,
      horizontal_sep = "-",
      trailing_section_div = trailing_sep
    ) ## this is overridden below to get recursiveness
    tab <- set_format_recursive(tab, format, na_str, FALSE)

    ## these is recursive
    ## XXX combine thse probably
    horizontal_sep(tab) <- hsep
    table_inset(tab) <- as.integer(inset)
    tab
  }
}


###
### Pre-Data Layout Declaration Classes
###
### Notably these are NOT represented as trees
### because without data we cannot know what the
### children should be.
###

##
## Vector (ordered list) of splits.
##
## This is a vector (ordered list) of splits to be
## applied recursively to the data when provided.
##
## For convenience, if this is length 1, it can contain
## a pre-existing TableTree/ElementaryTable.
## This is used for add_existing_table in
## colby_constructors.R
##

setClass("SplitVector",
  contains = "list",
  validity = function(object) {
    if (length(object) >= 1) {
      lst <- tail(object, 1)[[1]]
    } else {
      lst <- NULL
    }
    all(sapply(head(object, -1), is, "Split")) &&
      (is.null(lst) || is(lst, "Split") || is(lst, "VTableNodeInfo"))
  }
)

SplitVector <- function(x = NULL,
                        ...,
                        lst = list(...)) {
  if (!is.null(x)) {
    lst <- unlist(c(list(x), lst), recursive = FALSE)
  }
  new("SplitVector", lst)
}

avar_noneorlast <- function(vec) {
  if (!is(vec, "SplitVector")) {
    return(FALSE)
  }
  if (length(vec) == 0) {
    return(TRUE)
  }
  isavar <- which(sapply(vec, is, "AnalyzeVarSplit"))
  (length(isavar) == 0) || (length(isavar) == 1 && isavar == length(vec))
}



setClass("PreDataAxisLayout",
  contains = "list",
  representation(root_split = "ANY"),
  validity = function(object) {
    allleafs <- unlist(object, recursive = TRUE)
    all(sapply(object, avar_noneorlast)) &&
      all(sapply(
        allleafs,
        ## remember existing table trees can be added to layouts
        ## for now...
        function(x) is(x, "Split") || is(x, "VTableTree")
      ))
  }
)



setClass("PreDataColLayout",
  contains = "PreDataAxisLayout",
  representation(
    display_columncounts = "logical",
    columncount_format = "character"
  )
)

setClass("PreDataRowLayout", contains = "PreDataAxisLayout")

PreDataColLayout <- function(x = SplitVector(),
                             rtsp = RootSplit(),
                             ...,
                             lst = list(x, ...),
                             disp_colcounts = FALSE,
                             colcount_format = "(N=xx)") {
  ret <- new("PreDataColLayout", lst,
    display_columncounts = disp_colcounts,
    columncount_format = colcount_format
  )
  ret@root_split <- rtsp
  ret
}


PreDataRowLayout <- function(x = SplitVector(),
                             root = RootSplit(),
                             ...,
                             lst = list(x, ...)) {
  new("PreDataRowLayout", lst, root_split = root)
}




setClass("PreDataTableLayouts",
  contains = "VTitleFooter",
  representation(
    row_layout = "PreDataRowLayout",
    col_layout = "PreDataColLayout",
    top_left = "character",
    table_inset = "integer"
  )
)

PreDataTableLayouts <- function(rlayout = PreDataRowLayout(),
                                clayout = PreDataColLayout(),
                                topleft = character(),
                                title = "",
                                subtitles = character(),
                                main_footer = character(),
                                prov_footer = character(),
                                table_inset = 0L) {
  new("PreDataTableLayouts",
    row_layout = rlayout,
    col_layout = clayout,
    top_left = topleft,
    main_title = title,
    subtitles = subtitles,
    main_footer = main_footer,
    provenance_footer = prov_footer,
    table_inset = table_inset
  )
}



## setClass("CellValue", contains = "ValueWrapper",
##          representation(format = "FormatSpec",
##                         colspan = "integerOrNULL",
##                         label = "characterOrNULL"),
##          prototype = list(label ="", colspan = NULL, format = NULL))

setOldClass("CellValue")

#' Length of a Cell value
#'
#' @exportMethod length
#' @return Always returns \code{1L}
#' @param x x.
setMethod(
  "length", "CellValue",
  function(x) 1L
)

setClass("RefFootnote", representation(
  value = "character",
  index = "integer",
  symbol = "character"
))


RefFootnote <- function(note, index = NA_integer_, symbol = NA_character_) {
  if (is(note, "RefFootnote")) {
    return(note)
  } else if (length(note) == 0) {
    return(NULL)
  }
  if (length(symbol) != 1L) {
    stop(
      "Referential footnote can only have a single string as its index.",
      " Got char vector of length ", length(index)
    )
  }
  if (!is.na(symbol) && (index == "NA" || grepl("[{}]", index))) {
    stop(
      "The string 'NA' and strings containing '{' or '}' cannot be used as ",
      "referential footnote index symbols. Got string '", index, "'."
    )
  }

  new("RefFootnote", value = note, index = index, symbol = symbol)
}

#' Cell Value constructor
#'
#' @inheritParams lyt_args
#' @inheritParams rcell
#' @param val ANY. value in the cell exactly as it should be passed to a
#'   formatter or returned when extracted
#' @return An object representing the value within a single cell within a
#'   populated table. The underlying structure of this object is an
#'   implementation detail and should not be relied upon beyond calling
#'   accessors for the class.
#' @export

## Class definition
## [[1]] list: cell value
## format : format for cell
## colspan: column span info for cell
## label: row label to be used for parent row
## indent_mod: indent modifier to be used for parent row
CellValue <- function(val, format = NULL, colspan = 1L, label = NULL,
                      indent_mod = NULL, footnotes = NULL,
                      align = NULL, format_na_str = NULL) {
  if (is.null(colspan)) {
    colspan <- 1L
  }
  if (!is.null(colspan) && !is(colspan, "integer")) {
    colspan <- as.integer(colspan)
  }
  ## if we're not given a label but the value has one associated with
  ## it we use that.
  ## NB: we need to be able to override a non-empty label with an empty one
  ## so we can't have "" mean "not given a label" here
  if ((is.null(label) || is.na(label)) && !is.null(obj_label(val))) {
    label <- obj_label(val)
  }
  if (!is.list(footnotes)) {
    footnotes <- lapply(footnotes, RefFootnote)
  }
  check_ok_label(label)
  ret <- structure(list(val),
    format = format, colspan = colspan,
    label = label,
    indent_mod = indent_mod, footnotes = footnotes,
    align = align,
    format_na_str = format_na_str,
    class = "CellValue"
  )
  ret
}


#' @method print CellValue
#' @export
print.CellValue <- function(x, ...) {
  cat(paste("rcell:", format_rcell(x), "\n"))
  invisible(x)
}

## too slow
# setClass("RowsVerticalSection", contains = "list",
#          representation = list(row_names = "characterOrNULL",
#                                row_labels = "characterOrNULL",
#                                row_formats = "ANY",
#                                indent_mods = "integerOrNULL"))
#

setOldClass("RowsVerticalSection")
RowsVerticalSection <- function(values,
                                names = names(values),
                                labels = NULL,
                                indent_mods = NULL,
                                formats = NULL,
                                footnotes = NULL,
                                format_na_strs = NULL) {
  stopifnot(is(values, "list"))
  ##    innernms <- value_names(values)

  if (is.null(labels)) {
    labels <- names(values)
  }
  if (is.null(names) && all(nzchar(labels))) {
    names <- labels
  } else if (is.null(labels) && !is.null(names)) {
    labels <- names
  }

  if (!is.null(indent_mods)) {
    indent_mods <- as.integer(indent_mods)
  }
  check_ok_label(labels, multi_ok = TRUE)
  structure(values,
    class = "RowsVerticalSection", row_names = names,
    row_labels = labels, indent_mods = indent_mods,
    row_formats = formats,
    row_na_strs = format_na_strs,
    row_footnotes = lapply(
      footnotes,
      ## cause each row needs to accept
      ## a *list* of row footnotes
      function(fns) lapply(fns, RefFootnote)
    )
  )
}


#' @method print RowsVerticalSection
#' @export
print.RowsVerticalSection <- function(x, ...) {
  cat("RowsVerticalSection (in_rows) object print method:\n-------------------",
    "---------\n",
    sep = ""
  )
  print(data.frame(
    row_name = attr(x, "row_names", exact = TRUE),
    formatted_cell = vapply(x, format_rcell, character(1)),
    indent_mod = indent_mod(x), ## vapply(x, indent_mod, numeric(1)),
    row_label = attr(x, "row_labels", exact = TRUE),
    stringsAsFactors = FALSE,
    row.names = NULL
  ), row.names = TRUE)
  invisible(x)
}


## ## Empty default objects to avoid repeated calls
## EmptyColInfo <- InstantiatedColumnInfo()
## EmptyElTable <- ElementaryTable()
## EmptyRootSplit <- RootSplit()
## EmptyAllSplit <- AllSplit()
