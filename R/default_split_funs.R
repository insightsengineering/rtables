# Split functions --------------------------------------------------------------
#' Split functions
#'
#' @description
#' This is a collection of useful, default split function that can help you in dividing the data, hence the
#' table rows or columns, into different parts or groups (splits). You can also create your own split function if you
#' need to create a custom division as specific as you need. Please consider reading [custom_split_funs] if
#' this is the case. Beyond this list of functions, you can also use [add_overall_level()] and [add_combo_levels()]
#' for adding or modifying levels and [trim_levels_to_map()] to provide possible level combinations to filter the split
#' with.
#'
#' @inheritParams sf_args
#' @inheritParams gen_args
#' @param vals (`ANY`)\cr for internal use only.
#' @param labels (`character`)\cr labels to use for the remaining levels instead of the existing ones.
#'
#' @returns A function that can be used to split the data accordingly. The actual function signature
#'   is similar to the one you can define when creating a fully custom one. For more details see [custom_split_funs].
#'
#' @note
#' The following parameters are also documented here but they are only the default
#' signature of a split function: `df` (data to be split), `spl` (split object), and `vals = NULL`,
#' `labels = NULL`, `trim = FALSE` (last three only for internal use). See [custom_split_funs] for more details
#' and [make_split_fun()] for a more advanced API.
#'
#' @seealso [custom_split_funs], [add_overall_level()], [add_combo_levels()], and [trim_levels_to_map()].
#'
#' @name split_funcs
NULL

# helper fncs
.get_unique_levels <- function(vec) {
  out <- if (is.factor(vec)) {
    levels(vec)
  } else {
    unique(vec)
  }

  out
}

.print_setdiff_error <- function(provided, existing) {
  paste(setdiff(provided, existing), collapse = ", ")
}

#' @describeIn split_funcs keeps only specified levels (`only`) in the split variable. If any of the specified
#'   levels is not present, an error is returned. `reorder = TRUE` (the default) orders the split levels
#'   according to the order of `only`.
#'
#' @param only (`character`)\cr levels to retain (all others will be dropped). If none of the levels is present
#'   an empty table is returned.
#' @param reorder (`flag`)\cr whether the order of `only` should be used as the order of the children of the
#'   split. Defaults to `TRUE`.
#'
#' @examples
#' # keep_split_levels keeps specified levels (reorder = TRUE by default)
#' lyt <- basic_table() %>%
#'   split_rows_by("COUNTRY",
#'     split_fun = keep_split_levels(c("USA", "CAN", "BRA"))
#'   ) %>%
#'   analyze("AGE")
#'
#' tbl <- build_table(lyt, DM)
#' tbl
#'
#' @export
keep_split_levels <- function(only, reorder = TRUE) {
  function(df, spl, vals = NULL, labels = NULL, trim = FALSE) {
    var <- spl_payload(spl)
    varvec <- df[[var]]

    # Unique values from the split variable
    unique_vals <- .get_unique_levels(varvec)

    # Error in case not all levels are present
    if (length(varvec) > 0 && !all(only %in% unique_vals)) {
      what <- ifelse(is.factor(varvec), "factor level(s)", "character value(s)")
      stop(
        "Attempted to keep ", what, " in split that are not present in data: \n",
        .print_setdiff_error(only, unique_vals)
      )
    }

    df2 <- df[varvec %in% only, ]
    if (reorder) {
      df2[[var]] <- factor(df2[[var]], levels = only)
    } else {
      # Find original order of only
      only <- unique_vals[sort(match(only, unique_vals))]
    }

    spl_child_order(spl) <- only
    .apply_split_inner(spl, df2,
      vals = only,
      labels = labels,
      trim = trim
    )
  }
}

#' @describeIn split_funcs Removes specified levels (`excl`) from the split variable. Nothing done if not in data.
#'
#' @param excl (`character`)\cr levels to be excluded (they will not be reflected in the resulting table structure
#'   regardless of presence in the data).
#'
#' @examples
#' # remove_split_levels removes specified split levels
#' lyt <- basic_table() %>%
#'   split_rows_by("COUNTRY",
#'     split_fun = remove_split_levels(c(
#'       "USA", "CAN",
#'       "CHE", "BRA"
#'     ))
#'   ) %>%
#'   analyze("AGE")
#'
#' tbl <- build_table(lyt, DM)
#' tbl
#'
#' @export
remove_split_levels <- function(excl) {
  stopifnot(is.character(excl))
  function(df, spl, vals = NULL, labels = NULL, trim = FALSE) {
    var <- spl_payload(spl)
    df2 <- df[!(df[[var]] %in% excl), ]
    if (is.factor(df2[[var]])) {
      levels <- levels(df2[[var]])
      levels <- levels[!(levels %in% excl)]
      df2[[var]] <- factor(df2[[var]], levels = levels)
    }
    .apply_split_inner(spl, df2,
      vals = vals,
      labels = labels,
      trim = trim
    )
  }
}

#' @describeIn split_funcs Drops levels that have no representation in the data.
#'
#' @examples
#' # drop_split_levels drops levels that are not present in the data
#' lyt <- basic_table() %>%
#'   split_rows_by("SEX", split_fun = drop_split_levels) %>%
#'   analyze("AGE")
#'
#' tbl <- build_table(lyt, DM)
#' tbl
#'
#' @export
drop_split_levels <- function(df,
                              spl,
                              vals = NULL,
                              labels = NULL,
                              trim = FALSE) {
  var <- spl_payload(spl)
  df2 <- df
  df2[[var]] <- factor(df[[var]])
  lblvar <- spl_label_var(spl)
  if (!is.null(lblvar)) {
    df2[[lblvar]] <- factor(df[[lblvar]])
  }

  .apply_split_inner(spl, df2,
    vals = vals,
    labels = labels,
    trim = trim
  )
}

#' @describeIn split_funcs Removes specified levels `excl` and drops all levels that are
#'   not in the data.
#'
#' @examples
#' # Removing "M" and "U" directly, then "UNDIFFERENTIATED" because not in data
#' lyt <- basic_table() %>%
#'   split_rows_by("SEX", split_fun = drop_and_remove_levels(c("M", "U"))) %>%
#'   analyze("AGE")
#'
#' tbl <- build_table(lyt, DM)
#' tbl
#'
#' @export
drop_and_remove_levels <- function(excl) {
  stopifnot(is.character(excl))
  function(df, spl, vals = NULL, labels = NULL, trim = FALSE) {
    var <- spl_payload(spl)
    df2 <- df[!(df[[var]] %in% excl), ]
    df2[[var]] <- factor(df2[[var]])
    .apply_split_inner(
      spl,
      df2,
      vals = vals,
      labels = labels,
      trim = trim
    )
  }
}

#' @describeIn split_funcs Reorders split levels following `neworder`, which needs to be of
#'   same size as the levels in data.
#'
#' @param neworder (`character`)\cr new order of factor levels. All need to be present in the data.
#'   To add empty levels, rely on pre-processing or create your [custom_split_funs].
#' @param newlabels (`character`)\cr labels for (new order of) factor levels. If named, the levels are matched.
#'   Otherwise, the order of `neworder` is used.
#' @param drlevels (`flag`)\cr whether levels that are not in `neworder` should be dropped.
#'   Default is `TRUE`. Note: `drlevels = TRUE` does not drop levels that are not originally in the data.
#'   Rely on pre-processing or use a combination of split functions with [make_split_fun()] to also drop
#'   unused levels.
#'
#' @examples
#' # Reordering levels in split variable
#' lyt <- basic_table() %>%
#'   split_rows_by(
#'     "SEX",
#'     split_fun = reorder_split_levels(
#'       neworder = c("U", "F"),
#'       newlabels = c(U = "Uu", `F` = "Female")
#'     )
#'   ) %>%
#'   analyze("AGE")
#'
#' tbl <- build_table(lyt, DM)
#' tbl
#'
#' # Reordering levels in split variable but keeping all the levels
#' lyt <- basic_table() %>%
#'   split_rows_by(
#'     "SEX",
#'     split_fun = reorder_split_levels(
#'       neworder = c("U", "F"),
#'       newlabels = c("Uu", "Female"),
#'       drlevels = FALSE
#'     )
#'   ) %>%
#'   analyze("AGE")
#'
#' tbl <- build_table(lyt, DM)
#' tbl
#'
#' @export
reorder_split_levels <- function(neworder,
                                 newlabels = neworder,
                                 drlevels = TRUE) {
  function(df, spl, trim, ...) {
    df2 <- df
    valvec <- df2[[spl_payload(spl)]]

    uni_vals <- .get_unique_levels(valvec)

    # No sense adding things that are not present -> creating unexpected NAs
    if (!all(neworder %in% uni_vals)) {
      stop(
        "Attempted to reorder factor levels in split that are not present in data:\n",
        .print_setdiff_error(neworder, uni_vals)
      )
    }

    # Keeping all levels also from before if not dropped
    diff_with_uni_vals <- setdiff(uni_vals, neworder)
    if (!drlevels && length(diff_with_uni_vals) > 0) {
      if (length(newlabels) > length(neworder)) {
        stop(
          "When keeping levels not in neworder (drlevels = FALSE), newlabels can ",
          "affect only selected neworder, and not other levels.\n",
          "Add labels for current neworder: ", paste0(neworder, collapse = ", ")
        )
      }
      neworder <- c(neworder, diff_with_uni_vals)
      if (is.null(names(newlabels))) {
        newlabels <- c(newlabels, diff_with_uni_vals)
      } else {
        newlabels <- c(newlabels, setNames(diff_with_uni_vals, diff_with_uni_vals))
      }
    }

    valvec <- factor(valvec, levels = neworder)

    # Labels
    if (!is.null(names(newlabels))) {
      if (any(!names(newlabels) %in% neworder)) {
        stop(
          "Got labels' names for levels that are not present:\n",
          setdiff(names(newlabels), neworder)
        )
      }
      # To be safe: sorting by neworder
      newlabels <- newlabels[sapply(names(newlabels), function(x) which(x == neworder))]
    } else if (length(neworder) != length(newlabels)) {
      stop(
        "Got unnamed newlabels with different length than neworder. ",
        "Please provide names or make sure they are of the same length.\n",
        "Current neworder: ", paste0(neworder, collapse = ", ")
      )
    }

    # Final values
    spl_child_order(spl) <- neworder
    df2[[spl_payload(spl)]] <- valvec
    .apply_split_inner(spl, df2,
      vals = neworder,
      labels = newlabels,
      trim = trim
    )
  }
}

#' @describeIn split_funcs Takes the split groups and removes levels of `innervar` if not present in
#'   those split groups. If you want to specify a filter of possible combinations, please
#'   consider using [trim_levels_to_map()].
#'
#' @param innervar (`string`)\cr variable whose factor levels should be trimmed (e.g. empty levels dropped)
#'   *separately within each grouping defined at this point in the structure*.
#' @param drop_outlevs (`flag`)\cr whether empty levels in the variable being split on (i.e. the "outer"
#'   variable, not `innervar`) should be dropped. Defaults to `TRUE`.
#'
#' @examples
#' # trim_levels_in_group() trims levels within each group defined by the split variable
#' dat <- data.frame(
#'   col1 = factor(c("A", "B", "C"), levels = c("A", "B", "C", "N")),
#'   col2 = factor(c("a", "b", "c"), levels = c("a", "b", "c", "x"))
#' ) # N is removed if drop_outlevs = TRUE, x is removed always
#'
#' tbl <- basic_table() %>%
#'   split_rows_by("col1", split_fun = trim_levels_in_group("col2")) %>%
#'   analyze("col2") %>%
#'   build_table(dat)
#' tbl
#'
#' @export
trim_levels_in_group <- function(innervar, drop_outlevs = TRUE) {
  myfun <- function(df, spl, vals = NULL, labels = NULL, trim = FALSE) {
    if (!drop_outlevs) {
      ret <- .apply_split_inner(spl, df,
        vals = vals,
        labels = labels, trim = trim
      )
    } else {
      ret <- drop_split_levels(
        df = df, spl = spl, vals = vals,
        labels = labels, trim = trim
      )
    }

    ret$datasplit <- lapply(ret$datasplit, function(x) {
      coldat <- x[[innervar]]
      if (is(coldat, "character")) {
        if (!is.null(vals)) {
          lvs <- vals
        } else {
          lvs <- unique(coldat)
        }
        coldat <- factor(coldat, levels = lvs) ## otherwise
      } else {
        coldat <- droplevels(coldat)
      }
      x[[innervar]] <- coldat
      x
    })
    ret$labels <- as.character(ret$labels) # TODO
    ret
  }
  myfun
}

# add_combo_levels -------------------------------------------------------------
# Dedicated docs are attached to default split functions
.add_combo_part_info <- function(part,
                                 df,
                                 valuename,
                                 levels,
                                 label,
                                 extras,
                                 first = TRUE) {
  value <- LevelComboSplitValue(valuename, extras,
    combolevels = levels,
    label = label
  )
  newdat <- setNames(list(df), valuename)
  newval <- setNames(list(value), valuename)
  newextra <- setNames(list(extras), valuename)
  if (first) {
    part$datasplit <- c(newdat, part$datasplit)
    part$values <- c(newval, part$values)
    part$labels <- c(setNames(label, valuename), part$labels)
    part$extras <- c(newextra, part$extras)
  } else {
    part$datasplit <- c(part$datasplit, newdat)
    part$values <- c(part$values, newval)
    part$labels <- c(part$labels, setNames(label, valuename))
    part$extras <- c(part$extras, newextra)
  }
  ## not needed even in custom split function case.
  ##   part = .fixupvals(part)
  part
}

#' Add overall or combination levels to split groups
#'
#' @description
#' `add_overall_level` is a split function that adds a global level to the current levels in the split. Similarly,
#' `add_combo_df` uses a user-provided `data.frame` to define the combine the levels to be added. If you need a
#' single overall column, after all splits, please check [add_overall_col()]. Consider also defining
#' your custom split function if you need more flexibility (see [custom_split_funs]).
#'
#' @inheritParams lyt_args
#' @inheritParams sf_args
#' @param valname (`string`)\cr value to be assigned to the implicit all-observations split level. Defaults to
#'   `"Overall"`.
#' @param first (`flag`)\cr whether the implicit level should appear first (`TRUE`) or last (`FALSE`). Defaults
#'   to `TRUE`.
#'
#' @return A splitting function (`splfun`) that adds or changes the levels of a split.
#'
#' @seealso [custom_split_funs] and [split_funcs].
#'
#' @examples
#' lyt <- basic_table() %>%
#'   split_cols_by("ARM", split_fun = add_overall_level("All Patients",
#'     first = FALSE
#'   )) %>%
#'   analyze("AGE")
#'
#' tbl <- build_table(lyt, DM)
#' tbl
#'
#' lyt2 <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   split_rows_by("RACE",
#'     split_fun = add_overall_level("All Ethnicities")
#'   ) %>%
#'   summarize_row_groups(label_fstr = "%s (n)") %>%
#'   analyze("AGE")
#' lyt2
#'
#' tbl2 <- build_table(lyt2, DM)
#' tbl2
#'
#' @export
add_overall_level <- function(valname = "Overall",
                              label = valname,
                              extra_args = list(),
                              first = TRUE,
                              trim = FALSE) {
  combodf <- data.frame(
    valname = valname,
    label = label,
    levelcombo = I(list(select_all_levels)),
    exargs = I(list(extra_args)),
    stringsAsFactors = FALSE
  )
  add_combo_levels(combodf,
    trim = trim, first = first
  )
}

setClass("AllLevelsSentinel", contains = "character")

# nocov start
#' @rdname add_overall_level
#' @export
select_all_levels <- new("AllLevelsSentinel")
# nocov end

#' @param combosdf (`data.frame` or `tbl_df`)\cr a data frame with columns `valname`, `label`, `levelcombo`, and
#'   `exargs`. `levelcombo` and `exargs` should be list columns. Passing the `select_all_levels` object as a value in
#'   `comblevels` column indicates that an overall/all-observations level should be created.
#' @param keep_levels (`character` or `NULL`)\cr if non-`NULL`, the levels to retain across both combination and
#'   individual levels.
#'
#' @inherit add_overall_level return
#'
#' @note
#' Analysis or summary functions for which the order matters should never be used within the tabulation framework.
#'
#' @examplesIf require(tibble)
#'
#' library(tibble)
#' combodf <- tribble(
#'   ~valname, ~label, ~levelcombo, ~exargs,
#'   "A_B", "Arms A+B", c("A: Drug X", "B: Placebo"), list(),
#'   "A_C", "Arms A+C", c("A: Drug X", "C: Combination"), list()
#' )
#'
#' lyt <- basic_table(show_colcounts = TRUE) %>%
#'   split_cols_by("ARM", split_fun = add_combo_levels(combodf)) %>%
#'   analyze("AGE")
#'
#' tbl <- build_table(lyt, DM)
#' tbl
#'
#' lyt1 <- basic_table(show_colcounts = TRUE) %>%
#'   split_cols_by("ARM",
#'     split_fun = add_combo_levels(combodf,
#'       keep_levels = c(
#'         "A_B",
#'         "A_C"
#'       )
#'     )
#'   ) %>%
#'   analyze("AGE")
#'
#' tbl1 <- build_table(lyt1, DM)
#' tbl1
#'
#' smallerDM <- droplevels(subset(DM, SEX %in% c("M", "F") &
#'   grepl("^(A|B)", ARM)))
#' lyt2 <- basic_table(show_colcounts = TRUE) %>%
#'   split_cols_by("ARM", split_fun = add_combo_levels(combodf[1, ])) %>%
#'   split_cols_by("SEX",
#'     split_fun = add_overall_level("SEX_ALL", "All Genders")
#'   ) %>%
#'   analyze("AGE")
#'
#' lyt3 <- basic_table(show_colcounts = TRUE) %>%
#'   split_cols_by("ARM", split_fun = add_combo_levels(combodf)) %>%
#'   split_rows_by("SEX",
#'     split_fun = add_overall_level("SEX_ALL", "All Genders")
#'   ) %>%
#'   summarize_row_groups() %>%
#'   analyze("AGE")
#'
#' tbl3 <- build_table(lyt3, smallerDM)
#' tbl3
#'
#' @rdname add_overall_level
#' @export
add_combo_levels <- function(combosdf,
                             trim = FALSE,
                             first = FALSE,
                             keep_levels = NULL) {
  myfun <- function(df, spl, vals = NULL, labels = NULL, ...) {
    if (is(spl, "MultiVarSplit")) {
      stop("Combining levels of a MultiVarSplit does not make sense.",
        call. = FALSE
      )
    } # nocov
    ret <- .apply_split_inner(spl, df,
      vals = vals,
      labels = labels, trim = trim
    )
    for (i in seq_len(nrow(combosdf))) {
      lcombo <- combosdf[i, "levelcombo", drop = TRUE][[1]]
      spld <- spl_payload(spl)
      if (is(lcombo, "AllLevelsSentinel")) {
        subdf <- df
      } else if (is(spl, "VarLevelSplit")) {
        subdf <- df[df[[spld]] %in% lcombo, ]
      } else { ## this covers non-var splits, e.g. Cut-based splits
        stopifnot(all(lcombo %in% c(ret$labels, ret$vals)))
        subdf <- do.call(
          rbind,
          ret$datasplit[names(ret$datasplit) %in% lcombo | ret$vals %in% lcombo]
        )
      }
      ret <- .add_combo_part_info(
        ret, subdf,
        combosdf[i, "valname", drop = TRUE],
        lcombo,
        combosdf[i, "label", drop = TRUE],
        combosdf[i, "exargs", drop = TRUE][[1]],
        first
      )
    }
    if (!is.null(keep_levels)) {
      keep_inds <- value_names(ret$values) %in% keep_levels
      ret <- lapply(ret, function(x) x[keep_inds])
    }

    ret
  }
  myfun
}

#' Trim levels to map
#'
#' This split function constructor creates a split function which trims levels of a variable to reflect restrictions
#' on the possible combinations of two or more variables which the data is split by (along the same axis) within a
#' layout.
#'
#' @param map data.frame. A data.frame defining allowed combinations of
#'   variables. Any combination at the level of this split not present in the
#'   map will be removed from the data, both for the variable being split and
#'   those present in the data but not associated with this split or any parents
#'   of it.
#'
#' @details
#' When splitting occurs, the map is subset to the values of all previously performed splits. The levels of the
#' variable being split are then pruned to only those still present within this subset of the map representing the
#' current hierarchical splitting context.
#'
#' Splitting is then performed via the [keep_split_levels()] split function.
#'
#' Each resulting element of the partition is then further trimmed by pruning values of any remaining variables
#' specified in the map to those values allowed under the combination of the previous and current split.
#'
#' @return A function that can be used as a split function.
#'
#' @seealso [trim_levels_in_group()].
#'
#' @examples
#' map <- data.frame(
#'   LBCAT = c("CHEMISTRY", "CHEMISTRY", "CHEMISTRY", "IMMUNOLOGY"),
#'   PARAMCD = c("ALT", "CRP", "CRP", "IGA"),
#'   ANRIND = c("LOW", "LOW", "HIGH", "HIGH"),
#'   stringsAsFactors = FALSE
#' )
#'
#' lyt <- basic_table() %>%
#'   split_rows_by("LBCAT") %>%
#'   split_rows_by("PARAMCD", split_fun = trim_levels_to_map(map = map)) %>%
#'   analyze("ANRIND")
#' tbl <- build_table(lyt, ex_adlb)
#'
#' @export
trim_levels_to_map <- function(map = NULL) {
  if (is.null(map) || any(sapply(map, class) != "character")) {
    stop(
      "No map dataframe was provided or not all of the columns are of ",
      "type character."
    )
  }

  myfun <- function(df,
                    spl,
                    vals = NULL,
                    labels = NULL,
                    trim = FALSE,
                    .spl_context) {
    allvars <- colnames(map)
    splvar <- spl_payload(spl)

    allvmatches <- match(.spl_context$split, allvars)
    outvars <- allvars[na.omit(allvmatches)]
    ## invars are variables present in data, but not in
    ## previous or current splits
    invars <- intersect(
      setdiff(allvars, c(outvars, splvar)),
      names(df)
    )
    ## allvarord <- c(na.omit(allvmatches), ## appear in prior splits
    ##                which(allvars == splvar), ## this split
    ##                allvars[-1*na.omit(allvmatches)]) ## "outvars"

    ## allvars <- allvars[allvarord]
    ## outvars <- allvars[-(which(allvars == splvar):length(allvars))]
    if (length(outvars) > 0) {
      indfilters <- vapply(outvars, function(ivar) {
        obsval <- .spl_context$value[match(ivar, .spl_context$split)]
        sprintf("%s == '%s'", ivar, obsval)
      }, "")

      allfilters <- paste(indfilters, collapse = " & ")
      map <- map[eval(parse(text = allfilters), envir = map), ]
    }
    map_splvarpos <- which(names(map) == splvar)
    nondup <- !duplicated(map[[splvar]])
    ksl_fun <- keep_split_levels(
      only = map[[splvar]][nondup],
      reorder = TRUE
    )
    ret <- ksl_fun(df, spl, vals, labels, trim = trim)

    if (length(ret$datasplit) == 0) {
      msg <- paste(sprintf("%s[%s]", .spl_context$split, .spl_context$value),
        collapse = "->"
      )
      stop(
        "map does not allow any values present in data for split ",
        "variable ", splvar,
        " under the following parent splits:\n\t", msg
      )
    }

    ## keep non-split (inner) variables levels
    ret$datasplit <- lapply(ret$values, function(splvar_lev) {
      df3 <- ret$datasplit[[splvar_lev]]
      curmap <- map[map[[map_splvarpos]] == splvar_lev, ]
      ## loop through inner variables
      for (iv in invars) { ## setdiff(colnames(map), splvar)) {
        iv_lev <- df3[[iv]]
        levkeep <- as.character(unique(curmap[[iv]]))
        if (is.factor(iv_lev) && !all(levkeep %in% levels(iv_lev))) {
          stop(
            "Attempted to keep invalid factor level(s) in split ",
            setdiff(levkeep, levels(iv_lev))
          )
        }

        df3 <- df3[iv_lev %in% levkeep, , drop = FALSE]

        if (is.factor(iv_lev)) {
          df3[[iv]] <- factor(as.character(df3[[iv]]),
            levels = levkeep
          )
        }
      }

      df3
    })
    names(ret$datasplit) <- ret$values
    ret
  }

  myfun
}
