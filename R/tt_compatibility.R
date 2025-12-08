#' Create an `rtable` row
#'
#' @inheritParams compat_args
#' @param ... cell values.
#' @param round_type (`"iec"`, `"iec_mod"` or `"sas"`)\cr the type of rounding to perform.
#' See [round_fmt()] for details.
#'
#' @return A row object of the context-appropriate type (label or data).
#'
#' @examples
#' rrow("ABC", c(1, 2), c(3, 2), format = "xx (xx.%)")
#' rrow("")
#'
#' @family compatibility
#' @export
rrow <- function(row.name = "", ..., format = NULL, indent = 0, inset = 0L, round_type = valid_round_type) {
  round_type <- match.arg(round_type)
  vals <- list(...)
  if (is.null(row.name)) {
    row.name <- ""
  } else if (!is(row.name, "character")) {
    stop("row.name must be NULL or a character string")
  }
  if (length(vals) == 0L) {
    LabelRow(
      lev = as.integer(indent),
      label = row.name,
      name = row.name,
      vis = TRUE,
      table_inset = 0L
    )
  } else {
    csps <- as.integer(sapply(vals, function(x) {
      attr(x, "colspan", exact = TRUE) %||% 1L
    }))
    ## we have to leave the formats on the cells and NOT the row unless we were
    ## already told to do so, because row formats get clobbered  when cbinding
    ## but cell formats do not.
    ## formats = sapply(vals, obj_format)
    ## if(is.character(formats) && length(unique(formats)) == 1L && is.null(format))
    ##     format = unique(formats)
    DataRow(
      vals = vals, lev = as.integer(indent), label = row.name,
      name = row.name, ## XXX TODO
      cspan = csps,
      format = format,
      table_inset = as.integer(inset),
      round_type = round_type
    )
  }
}

#' Create an `rtable` row from a vector or list of values
#'
#' @inheritParams compat_args
#' @param ... values in vector/list form.
#' @param round_type (`"iec"`, `"iec_mod"` or `"sas"`)\cr the type of rounding to perform.
#' See [round_fmt()] for details.
#'
#' @inherit rrow return
#'
#' @examples
#' rrowl("a", c(1, 2, 3), format = "xx")
#' rrowl("a", c(1, 2, 3), c(4, 5, 6), format = "xx")
#'
#'
#' rrowl("N", table(iris$Species))
#' rrowl("N", table(iris$Species), format = "xx")
#'
#' x <- tapply(iris$Sepal.Length, iris$Species, mean, simplify = FALSE)
#'
#' rrow(row.name = "row 1", x)
#' rrow("ABC", 2, 3)
#'
#' rrowl(row.name = "row 1", c(1, 2), c(3, 4))
#' rrow(row.name = "row 2", c(1, 2), c(3, 4))
#'
#' @family compatibility
#' @export
rrowl <- function(row.name, ..., format = NULL, indent = 0, inset = 0L, round_type = valid_round_type) {
  dots <- list(...)
  args_list <- c(list(
    row.name = row.name, format = format,
    indent = indent, inset = inset,
    round_type = round_type
  ), val = unlist(lapply(dots, as.list), recursive = FALSE))
  do.call(rrow, args_list)
}

## rcell moved to tt_afun_utils.R

## inefficient trash
paste_em_n <- function(lst, n, sep = ".") {
  ret <- lst[[1]]
  if (n > 1) {
    for (i in 2:n) {
      ret <- paste(ret, lst[[i]], sep = sep)
    }
  }
  ret
}

hrows_to_colinfo <- function(rows) {
  nr <- length(rows)
  stopifnot(nr > 0)
  cspans <- lapply(rows, row_cspans)
  vals <- lapply(rows, function(x) unlist(row_values(x)))
  unqvals <- lapply(vals, unique)
  formats <- lapply(rows, obj_format)
  counts <- NULL
  if (formats[nr] == "(N=xx)" || all(sapply(row_cells(rows[[nr]]), obj_format) == "(N=xx)")) { ## count row
    counts <- vals[[nr]]
    vals <- vals[-nr]
    cspans <- cspans[-nr]
    nr <- nr - 1
  }
  ## easiest case, one header row no counts. we're done
  ## XXX could one row but cspan ever make sense????
  ## I don't think so?
  if (nr == 1) { ## && all(cspans == 1L)) {
    ret <- manual_cols(unlist(vals[[1]]))
    if (!is.null(counts)) {
      col_counts(ret) <- counts
      disp_ccounts(ret) <- TRUE
    }
    return(ret)
  }
  ## second easiest case full repeated nestin
  repvals <- mapply(function(v, csp) rep(v, times = csp),
    v = vals, csp = cspans, SIMPLIFY = FALSE
  )

  ## nr > 1 here
  fullnest <- TRUE
  for (i in 2:nr) {
    psted <- paste_em_n(repvals, i - 1)
    spl <- split(repvals[[i]], psted)
    if (!all(sapply(spl, function(x) identical(x, spl[[1]])))) {
      fullnest <- FALSE
      break
    }
  }

  ## if its full nesting we're done, so put
  ## the counts on as necessary and return.
  if (fullnest) {
    ret <- manual_cols(.lst = unqvals)
    if (!is.null(counts)) {
      col_counts(ret) <- counts
      disp_ccounts(ret) <- TRUE
    }
    return(ret)
  }

  ## booo. the fully complex case where the multiple rows
  ## really don't represent nesting at all, each top level
  ## can have different sub labels

  ## we will build it up as if it were full nesting and then prune
  ## based on the columns we actually want.

  fullcolinfo <- manual_cols(.lst = unqvals)
  fullbusiness <- names(collect_leaves(coltree(fullcolinfo)))
  wanted <- paste_em_n(repvals, nr)
  wantcols <- match(wanted, fullbusiness)
  stopifnot(all(!is.na(wantcols)))

  subset_cols(fullcolinfo, wantcols)
}

#' Create a header
#'
#' @inheritParams compat_args
#' @param ... row specifications, either as character vectors or the output from [rrow()], [DataRow()],
#'   [LabelRow()], etc.
#'
#' @return A `InstantiatedColumnInfo` object.
#'
#' @examples
#' h1 <- rheader(c("A", "B", "C"))
#' h1
#'
#' h2 <- rheader(
#'   rrow(NULL, rcell("group 1", colspan = 2), rcell("group 2", colspan = 2)),
#'   rrow(NULL, "A", "B", "A", "B")
#' )
#' h2
#'
#' @family compatibility
#' @export
rheader <- function(..., format = "xx", .lst = NULL) {
  if (!is.null(.lst)) {
    args <- .lst
  } else {
    args <- list(...)
  }
  rrows <- if (length(args) == 1 && !is(args[[1]], "TableRow")) {
    list(rrowl(row.name = NULL, val = args[[1]], format = format))
  } else if (are(args, "TableRow")) {
    args
  }

  hrows_to_colinfo(rrows)
}

.char_to_hrows <- function(hdr) {
  nlfnd <- grep("\n", hdr, fixed = TRUE)
  if (length(nlfnd) == 0) {
    return(list(rrowl(NULL, hdr)))
  }

  stopifnot(length(nlfnd) == length(hdr))
  raw <- strsplit(hdr, "\n", fixed = TRUE)
  lens <- unique(sapply(raw, length))
  stopifnot(length(lens) == 1L)
  lapply(
    seq(1, lens),
    function(i) {
      rrowl(NULL, vapply(raw, `[`, NA_character_, i = i))
    }
  )
}

#' Create a table
#'
#' @inheritParams compat_args
#' @inheritParams gen_args
#' @param header (`TableRow`, `character`, or `InstantiatedColumnInfo`)\cr information defining the header
#'   (column structure) of the table. This can be as row objects (legacy), character vectors, or an
#'   `InstantiatedColumnInfo` object.
#' @param ... rows to place in the table.
#' @param round_type (`"iec"`, `"iec_mod"` or `"sas"`)\cr the type of rounding to perform.
#' See [round_fmt()] for details.
#'
#' @return A formal table object of the appropriate type (`ElementaryTable` or `TableTree`).
#'
#' @examples
#' rtable(
#'   header = LETTERS[1:3],
#'   rrow("one to three", 1, 2, 3),
#'   rrow("more stuff", rcell(pi, format = "xx.xx"), "test", "and more")
#' )
#'
#' # Table with multirow header
#'
#' sel <- iris$Species == "setosa"
#' mtbl <- rtable(
#'   header = rheader(
#'     rrow(
#'       row.name = NULL, rcell("Sepal.Length", colspan = 2),
#'       rcell("Petal.Length", colspan = 2)
#'     ),
#'     rrow(NULL, "mean", "median", "mean", "median")
#'   ),
#'   rrow(
#'     row.name = "All Species",
#'     mean(iris$Sepal.Length), median(iris$Sepal.Length),
#'     mean(iris$Petal.Length), median(iris$Petal.Length),
#'     format = "xx.xx"
#'   ),
#'   rrow(
#'     row.name = "Setosa",
#'     mean(iris$Sepal.Length[sel]), median(iris$Sepal.Length[sel]),
#'     mean(iris$Petal.Length[sel]), median(iris$Petal.Length[sel])
#'   )
#' )
#'
#' mtbl
#'
#' names(mtbl) # always first row of header
#'
#' # Single row header
#'
#' tbl <- rtable(
#'   header = c("Treatement\nN=100", "Comparison\nN=300"),
#'   format = "xx (xx.xx%)",
#'   rrow("A", c(104, .2), c(100, .4)),
#'   rrow("B", c(23, .4), c(43, .5)),
#'   rrow(""),
#'   rrow("this is a very long section header"),
#'   rrow("estimate", rcell(55.23, "xx.xx", colspan = 2)),
#'   rrow("95% CI", indent = 1, rcell(c(44.8, 67.4), format = "(xx.x, xx.x)", colspan = 2))
#' )
#' tbl
#'
#' row.names(tbl)
#' names(tbl)
#'
#' # Subsetting
#'
#' tbl[1, ]
#' tbl[, 1]
#'
#' tbl[1, 2]
#' tbl[2, 1]
#'
#' tbl[3, 2]
#' tbl[5, 1]
#' tbl[5, 2]
#'
#' # Data Structure methods
#'
#' dim(tbl)
#' nrow(tbl)
#' ncol(tbl)
#' names(tbl)
#'
#' # Colspans
#'
#' tbl2 <- rtable(
#'   c("A", "B", "C", "D", "E"),
#'   format = "xx",
#'   rrow("r1", 1, 2, 3, 4, 5),
#'   rrow("r2", rcell("sp2", colspan = 2), "sp1", rcell("sp2-2", colspan = 2))
#' )
#' tbl2
#'
#' @family compatibility
#' @export
rtable <- function(header, ..., format = NULL, hsep = default_hsep(),
                   inset = 0L, round_type = valid_round_type) {
  round_type <- match.arg(round_type)
  if (is.character(header)) {
    header <- .char_to_hrows(header)
  } # list(rrowl(NULL, header))
  if (is.list(header)) {
    if (are(header, "TableRow")) {
      colinfo <- hrows_to_colinfo(header)
    } else if (are(header, "list")) {
      colinfo <- do.call(rheader, header)
    }
  } else if (is(header, "InstantiatedColumnInfo")) {
    colinfo <- header
  } else if (is(header, "TableRow")) {
    colinfo <- hrows_to_colinfo(list(header))
  } else {
    stop("problems")
  }

  body <- list(...)
  ## XXX this shouldn't be needed. hacky
  if (length(body) == 1 && is.list(body[[1]])) {
    body <- body[[1]]
  }
  if (are(body, "ElementaryTable") &&
    all(sapply(body, function(tb) {
      nrow(tb) == 1 && obj_name(tb) == ""
    }))) {
    body <- lapply(body, function(tb) tree_children(tb)[[1]])
  }

  TableTree(
    kids = body, format = format, cinfo = colinfo,
    labelrow = LabelRow(lev = 0L, label = "", vis = FALSE),
    hsep = hsep, inset = inset, round_type = round_type
  )
}

#' @rdname rtable
#' @export
rtablel <- function(header, ..., format = NULL, hsep = default_hsep(), inset = 0L) {
  dots <- list(...)
  args_list <- c(list(header = header, format = format, hsep = hsep, inset = inset), unlist(lapply(
    dots,
    as.list
  ), recursive = FALSE))
  do.call(rtable, args_list)
}

# All object annotations are identical (and exist)
all_annots_identical <- function(all_annots) {
  if (!is.list(all_annots)) {
    all_annots[1] != "" && length(unique(all_annots)) == 1
  } else {
    length(all_annots[[1]]) > 0 && Reduce(identical, all_annots)
  }
}

# Only first object has annotations
only_first_annot <- function(all_annots) {
  if (!is.list(all_annots)) {
    all_annots[1] != "" && all(all_annots[-1] == "")
  } else {
    length(all_annots[[1]]) > 0 && all(sapply(all_annots, length)[-1] == 0)
  }
}

#' @param gap `r lifecycle::badge("deprecated")` ignored.
#' @param check_headers `r lifecycle::badge("deprecated")` ignored.
#'
#' @return A formal table object.
#'
#' @rdname rbind
#' @aliases rbind
#' @export
rbindl_rtables <- function(x, gap = lifecycle::deprecated(), check_headers = lifecycle::deprecated()) {
  ## nocov start
  if (lifecycle::is_present(gap)) {
    lifecycle::deprecate_warn(
      when = "0.3.2",
      what = "rbindl_rtables(gap)"
    )
  }
  if (lifecycle::is_present(check_headers)) {
    lifecycle::deprecate_warn(
      when = "0.3.2",
      what = "rbindl_rtables(check_headers)"
    )
  }
  ## nocov end

  firstcols <- col_info(x[[1]])
  i <- 1
  while (no_colinfo(firstcols) && i <= length(x)) {
    firstcols <- col_info(x[[i]])
    i <- i + 1
  }

  lapply(x, function(xi) chk_compat_cinfos(x[[1]], xi)) ## col_info(xi)))

  rbind_annot <- list(
    main_title = "",
    subtitles = character(),
    main_footer = character(),
    prov_footer = character()
  )

  # Titles/footer info are (independently) retained from first object if
  # identical or missing in all other objects
  all_titles <- sapply(x, main_title)
  if (all_annots_identical(all_titles) || only_first_annot(all_titles)) {
    rbind_annot[["main_title"]] <- all_titles[[1]]
  }

  all_sts <- lapply(x, subtitles)
  if (all_annots_identical(all_sts) || only_first_annot(all_sts)) {
    rbind_annot[["subtitles"]] <- all_sts[[1]]
  }

  all_ftrs <- lapply(x, main_footer)
  if (all_annots_identical(all_ftrs) || only_first_annot(all_ftrs)) {
    rbind_annot[["main_footer"]] <- all_ftrs[[1]]
  }

  all_pfs <- lapply(x, prov_footer)
  if (all_annots_identical(all_pfs) || only_first_annot(all_pfs)) {
    rbind_annot[["prov_footer"]] <- all_pfs[[1]]
  }

  ## if we got only ElementaryTable and
  ## TableRow objects, construct a new
  ## elementary table with all the rows
  ## instead of adding nesting.

  ## we used to check for xi not being a lable row, why?? XXX
  if (all(sapply(x, function(xi) {
    (is(xi, "ElementaryTable") && !labelrow_visible(xi)) ||
      is(xi, "TableRow")
  }))) { ## && !is(xi, "LabelRow")}))) {
    x <- unlist(lapply(x, function(xi) {
      if (is(xi, "TableRow")) {
        xi
      } else {
        lst <- tree_children(xi)
        lapply(lst, indent,
          by = indent_mod(xi)
        )
      }
    }))
  }

  TableTree(
    kids = x,
    cinfo = firstcols,
    name = "rbind_root",
    label = "",
    title = rbind_annot[["main_title"]],
    subtitles = rbind_annot[["subtitles"]],
    main_footer = rbind_annot[["main_footer"]],
    prov_footer = rbind_annot[["prov_footer"]]
  )
}

#' Row-bind `TableTree` and related objects
#'
#' @param deparse.level (`numeric(1)`)\cr currently ignored.
#' @param ... (`ANY`)\cr elements to be stacked.
#'
#' @note
#' When objects are row-bound, titles and footer information is retained from the first object (if any exists) if all
#' other objects have no titles/footers or have identical titles/footers. Otherwise, all titles/footers are removed
#' and must be set for the bound table via the [formatters::main_title()], [formatters::subtitles()],
#' [formatters::main_footer()], and [formatters::prov_footer()] functions.
#'
#' @examples
#' mtbl <- rtable(
#'   header = rheader(
#'     rrow(row.name = NULL, rcell("Sepal.Length", colspan = 2), rcell("Petal.Length", colspan = 2)),
#'     rrow(NULL, "mean", "median", "mean", "median")
#'   ),
#'   rrow(
#'     row.name = "All Species",
#'     mean(iris$Sepal.Length), median(iris$Sepal.Length),
#'     mean(iris$Petal.Length), median(iris$Petal.Length),
#'     format = "xx.xx"
#'   )
#' )
#'
#' mtbl2 <- with(subset(iris, Species == "setosa"), rtable(
#'   header = rheader(
#'     rrow(row.name = NULL, rcell("Sepal.Length", colspan = 2), rcell("Petal.Length", colspan = 2)),
#'     rrow(NULL, "mean", "median", "mean", "median")
#'   ),
#'   rrow(
#'     row.name = "Setosa",
#'     mean(Sepal.Length), median(Sepal.Length),
#'     mean(Petal.Length), median(Petal.Length),
#'     format = "xx.xx"
#'   )
#' ))
#'
#' rbind(mtbl, mtbl2)
#' rbind(mtbl, rrow(), mtbl2)
#' rbind(mtbl, rrow("aaa"), indent(mtbl2))
#'
#' @exportMethod rbind
#' @rdname rbind
setMethod(
  "rbind", "VTableNodeInfo",
  function(..., deparse.level = 1) {
    rbindl_rtables(list(...))
  }
)

#' @param y (`ANY`)\cr second element to be row-bound via `rbind2`.
#'
#' @exportMethod rbind2
#' @rdname int_methods
setMethod(
  "rbind2", c("VTableNodeInfo", "missing"),
  function(x, y) {
    TableTree(kids = list(x), cinfo = col_info(x), name = "rbind_root", label = "")
  }
)

#' @param x (`VTableNodeInfo`)\cr `TableTree`, `ElementaryTable`, or `TableRow` object.
#' @param y (`VTableNodeInfo`)\cr `TableTree`, `ElementaryTable`, or `TableRow` object.
#'
#' @exportMethod rbind2
#' @rdname rbind
setMethod(
  "rbind2", "VTableNodeInfo",
  function(x, y) {
    rbindl_rtables(list(x, y))
  }
)

EmptyTreePos <- TreePos()

## this is painful to do right but we were doing it wrong
## before and it now matters because count display information
## is in the tree which means all points in the structure
## must be pathable, which they aren't if siblings have
## identical names
fix_col_nm_recursive <- function(ct, newname, rename_obj = TRUE, oldnm) {
  if (rename_obj) {
    obj_name(ct) <- newname
  }
  if (is(ct, "LayoutColTree")) {
    kids <- tree_children(ct)
    kidnms <- names(kids)
    newkids <- lapply(kids, fix_col_nm_recursive,
      newname = newname,
      rename_obj = FALSE,
      oldnm = oldnm
    )
    names(newkids) <- kidnms
    tree_children(ct) <- newkids
  }
  mypos <- tree_pos(ct)
  if (!identical(mypos, EmptyTreePos)) {
    spls <- pos_splits(mypos)
    firstspl <- spls[[1]]
    if (obj_name(firstspl) == oldnm) {
      obj_name(firstspl) <- newname
      spls[[1]] <- firstspl
      pos_splits(mypos) <- spls
      tree_pos(ct) <- mypos
    }
  }
  if (!rename_obj) {
    spls <- pos_splits(mypos)
    splvals <- pos_splvals(mypos)
    pos_splits(mypos) <- c(
      list(AllSplit(split_name = newname)),
      spls
    )
    pos_splvals(mypos) <- c(
      list(SplitValue(NA_character_,
        sub_expr = quote(TRUE)
      )),
      splvals
    )
    tree_pos(ct) <- mypos
  }
  ct
}

fix_nms <- function(ct) {
  if (is(ct, "LayoutColLeaf")) {
    return(ct)
  }
  kids <- lapply(tree_children(ct), fix_nms)
  names(kids) <- vapply(kids, obj_name, "")
  tree_children(ct) <- kids
  ct
}

make_cbind_names <- function(num, tokens) {
  cbind_tokens <- grep("^(new_)*cbind_tbl", tokens, value = TRUE)
  ret <- paste0("cbind_tbl_", seq_len(num))
  if (length(cbind_tokens) == 0) {
    return(ret)
  }
  oldprefixes <- gsub("cbind_tbl.*", "", cbind_tokens)
  oldprefix <- oldprefixes[which.max(nchar(oldprefixes))]
  paste0("new_", oldprefix, ret)
}

combine_cinfo <- function(..., new_total = NULL, sync_count_vis) {
  tabs <- list(...)
  chk_cbindable_many(tabs)
  cinfs <- lapply(tabs, col_info)
  stopifnot(are(cinfs, "InstantiatedColumnInfo"))

  ctrees <- lapply(cinfs, coltree)
  oldnms <- nms <- vapply(ctrees, obj_name, "")
  path_els <- unique(unlist(lapply(ctrees, col_paths), recursive = TRUE))
  nms <- make_cbind_names(num = length(oldnms), tokens = path_els)

  ctrees <- mapply(function(ct, nm, oldnm) {
    ct <- fix_col_nm_recursive(ct, nm, rename_obj = TRUE, oldnm = "") # oldnm)
    ct
  }, ct = ctrees, nm = nms, oldnm = oldnms, SIMPLIFY = FALSE)
  names(ctrees) <- nms

  newctree <- LayoutColTree(kids = ctrees, colcount = NA_integer_, name = "cbind_root")
  newctree <- fix_nms(newctree)
  newcounts <- unlist(lapply(cinfs, col_counts))
  if (is.null(new_total)) {
    new_total <- sum(newcounts)
  }
  newexprs <- unlist(lapply(cinfs, col_exprs), recursive = FALSE)
  newexargs <- unlist(lapply(cinfs, col_extra_args), recursive = FALSE) %||% vector("list", length(newcounts))
  if (!sync_count_vis) {
    newdisp <- NA
  } else {
    newdisp <- any(vapply(cinfs, disp_ccounts, NA))
  }
  alltls <- lapply(cinfs, top_left)
  newtl <- character()
  if (!are(tabs, "TableRow")) {
    alltls <- alltls[vapply(alltls, function(x) length(x) > 0, NA)] ## these are already enforced to all be the same
    if (length(alltls) > 0) {
      newtl <- alltls[[1]]
    }
  }
  InstantiatedColumnInfo(
    treelyt = newctree,
    csubs = newexprs,
    extras = newexargs,
    cnts = newcounts,
    dispcounts = newdisp,
    countformat = colcount_format(cinfs[[1]]),
    total_cnt = new_total,
    topleft = newtl
  )
}

nz_len_els <- function(lst) {
  if (is(lst, "list")) {
    lst[vapply(lst, function(x) length(x) > 0, NA)]
  } else if (is(lst, "character")) {
    lst[nzchar(lst)]
  } else {
    lst
  }
}

has_one_unq <- function(x) {
  length(unique(nz_len_els(x))) <= 1
}

classvec <- function(lst, enforce_one = TRUE) {
  if (enforce_one) {
    vapply(lst, class, "")
  } else {
    lapply(lst, class)
  }
}

chk_cbindable_many <- function(lst) {
  ## we actually want is/inherits there but no easy way
  ## to figure out what the lowest base class is
  ## that I can think of right now, so we do the
  ## broken wrong thing instead :(
  if (are(lst, "TableRow")) {
    if (!has_one_unq(classvec(lst))) {
      stop("Cannot cbind different types of TableRow objects together")
    }
    return(TRUE)
  }
  ## if(!are(lst, "VTableTree")
  ##     stop("Not all elements to be bound are TableTrees or TableRows")

  nrs <- vapply(lst, NROW, 1L)
  if (!has_one_unq(nrs)) {
    stop("Not all elements to be bound have matching numbers of rows")
  }

  tls <- lapply(lst, top_left)
  if (!has_one_unq(tls[vapply(tls, function(x) length(x) > 0, NA)])) {
    stop(
      "Elements to be bound have differing top-left content: ",
      paste(which(!duplicated(tls)), collapse = " ")
    )
  }

  if (all(vapply(lst, function(x) nrow(x) == 0, NA))) {
    return(TRUE)
  }

  rns <- matrix(vapply(lst, row.names, rep("", nrs[[1]])),
    nrow = nrs[[1]]
  )
  rnsok <- apply(rns, 1, has_one_unq)
  if (!all(rnsok)) {
    stop(
      "Mismatching, non-empty row names detected in rows ",
      paste(which(!rnsok), collapse = " ")
    )
  }

  rws <- lapply(lst, collect_leaves, add.labrows = TRUE)
  rwclsmat <- matrix(unlist(lapply(rws, classvec)),
    ncol = length(lst)
  )

  rwsok <- apply(rwclsmat, 1, has_one_unq)
  if (!all(rwsok)) {
    stop(
      "Mismatching row classes found for rows: ",
      paste(which(!rwsok), collapse = " ")
    )
  }
  TRUE
}

#' Column-bind two `TableTree` objects
#'
#' @param x (`TableTree` or `TableRow`)\cr a table or row object.
#' @param ... one or more further objects of the same class as `x`.
#' @param sync_count_vis (`logical(1)`)\cr should column count
#'   visibility be synced across the new and existing columns.
#'   Currently defaults to `TRUE` for backwards compatibility but
#'   this may change in future releases.
#'
#' @inherit rbindl_rtables return
#'
#' @examples
#' x <- rtable(c("A", "B"), rrow("row 1", 1, 2), rrow("row 2", 3, 4))
#' y <- rtable("C", rrow("row 1", 5), rrow("row 2", 6))
#' z <- rtable("D", rrow("row 1", 9), rrow("row 2", 10))
#'
#' t1 <- cbind_rtables(x, y)
#' t1
#'
#' t2 <- cbind_rtables(x, y, z)
#' t2
#'
#' col_paths_summary(t1)
#' col_paths_summary(t2)
#'
#' @export
cbind_rtables <- function(x, ..., sync_count_vis = TRUE) {
  lst <- list(...)
  newcinfo <- combine_cinfo(x, ..., sync_count_vis = sync_count_vis)
  recurse_cbindl(x, cinfo = newcinfo, .list = lst)
}

setGeneric("recurse_cbindl", function(x, cinfo, .list = NULL) standardGeneric("recurse_cbindl"))

setMethod(
  "recurse_cbindl", c(
    x = "VTableNodeInfo",
    cinfo = "NULL"
  ),
  function(x, cinfo, .list = NULL) {
    recurse_cbindl(x, cinfo = combine_cinfo(.list), .list = .list)
  }
)

setMethod(
  "recurse_cbindl", c(
    x = "TableTree",
    cinfo = "InstantiatedColumnInfo"
  ),
  function(x, cinfo, .list = NULL) {
    stopifnot(are(.list, "VTableTree"))
    ## chk_cbindable(x, y)
    xcont <- content_table(x)
    lstconts <- lapply(.list, content_table)
    lcontnrows <- vapply(lstconts, NROW, 1L)
    unqnrcont <- unique(c(NROW(xcont), lcontnrows))
    if (length(unqnrcont) > 1) {
      stop(
        "Got differing numbers of content rows [",
        paste(unqnrcont, collapse = ", "),
        "]. Unable to cbind these rtables"
      )
    }

    if (unqnrcont == 0) {
      cont <- ElementaryTable(cinfo = cinfo)
    } else {
      cont <- recurse_cbindl(xcont,
        .list = lstconts,
        cinfo = cinfo
      )
    }

    kids <- lapply(
      seq_along(tree_children(x)),
      function(i) {
        recurse_cbindl(
          x = tree_children(x)[[i]],
          cinfo = cinfo,
          .list = lapply(.list, function(tt) tree_children(tt)[[i]])
        )
      }
    )
    names(kids) <- names(tree_children(x))
    TableTree(
      kids = kids, labelrow = recurse_cbindl(tt_labelrow(x),
        cinfo = cinfo,
        .list = lapply(.list, tt_labelrow)
      ),
      cont = cont,
      name = obj_name(x),
      lev = tt_level(x),
      cinfo = cinfo,
      format = obj_format(x)
    )
  }
)

setMethod(
  "recurse_cbindl", c(
    x = "ElementaryTable",
    cinfo = "InstantiatedColumnInfo"
  ),
  function(x, cinfo, .list) {
    stopifnot(are(.list, class(x)))
    ##   chk_cbindable(x,y)
    if (nrow(x) == 0 && all(vapply(.list, nrow, 1L) == 0)) {
      col_info(x) <- cinfo
      return(x) ## this needs testing... I was right, it did #136
    }
    kids <- lapply(
      seq_along(tree_children(x)),
      function(i) {
        recurse_cbindl(
          x = tree_children(x)[[i]],
          cinfo = cinfo,
          .list = lapply(.list, function(tt) tree_children(tt)[[i]])
        )
      }
    )
    names(kids) <- names(tree_children(x))

    ElementaryTable(
      kids = kids,
      labelrow = recurse_cbindl(tt_labelrow(x),
        .list = lapply(.list, tt_labelrow),
        cinfo
      ),
      name = obj_name(x),
      lev = tt_level(x),
      cinfo = cinfo,
      format = obj_format(x),
      var = obj_avar(x)
    )
  }
)

.combine_rows <- function(x, cinfo = NULL, .list) {
  stopifnot(are(.list, class(x)))

  avars <- c(obj_avar(x), unlist(lapply(.list, obj_avar), recursive = FALSE))
  avars <- avars[!is.na(avars)]

  if (length(unique(avars)) > 1) {
    stop("Got rows that don't analyze the same variable")
  }

  xlst <- c(list(x), .list)

  ncols <- vapply(xlst, ncol, 1L)
  totcols <- sum(ncols)
  cumncols <- cumsum(ncols)
  strtncols <- c(0L, head(cumncols, -1)) + 1L
  vals <- vector("list", totcols)
  cspans <- integer(totcols)
  ## vals[1:ncol(x)] <- row_values(x)
  ## cpans[1:ncol(x)] <- row_cspans(x)

  for (i in seq_along(xlst)) {
    strt <- strtncols[i]
    end <- cumncols[i]
    ## full vars are here for debugging purposes
    fullvy <- vy <- row_cells(xlst[[i]]) # nolint
    fullcspy <- cspy <- row_cspans(xlst[[i]]) # nolint

    if (
      i > 1 &&
        identical(rawvalues(vy[[1]]), rawvalues(lastval)) &&
        ##  cspy[1] == lastspn &&
        lastspn > 1
    ) {
      vy <- vy[-1]
      cspans[strt - 1L] <- lastspn + cspy[1]
      cspy <- cspy[-1]
      strt <- strt + 1L
    }
    if (length(vy) > 0) {
      vals[strt:end] <- vy
      cspans[strt:end] <- cspy
      lastval <- vy[[length(vy)]]
      lastspn <- cspy[[length(cspy)]]
    } else {
      ## lastval stays the same
      lastspn <- cspans[strtncols[i] - 1] ## already updated
    }
  }

  ## Could be DataRow or ContentRow
  ## This is ok because LabelRow is special cased
  constr_fun <- get(class(x), mode = "function")
  constr_fun(
    vals = vals,
    cspan = cspans,
    cinfo = cinfo,
    var = obj_avar(x),
    format = obj_format(x),
    name = obj_name(x),
    label = obj_label(x)
  )
}

setMethod(
  "recurse_cbindl", c(
    "TableRow",
    "InstantiatedColumnInfo"
  ),
  function(x, cinfo = NULL, .list) {
    .combine_rows(x, cinfo, .list)
  }
)

setMethod(
  "recurse_cbindl", c(
    x = "LabelRow",
    cinfo = "InstantiatedColumnInfo"
  ),
  function(x, cinfo = NULL, .list) {
    col_info(x) <- cinfo
    x
  }
)

## we don't care about the following discrepencies:
## - ci2  having NA counts when ci1 doesn't
## - mismatching display_ccounts values
## - mismatching colcount formats
##

# chk_compat_cinfos <- function(ci1, ci2) {
chk_compat_cinfos <- function(tt1, tt2) {
  nc1 <- ncol(tt1)
  nc2 <- ncol(tt2)
  if (nc1 != nc2 && nc1 > 0 && nc2 > 0) {
    stop("Column structures contain different non-zero numbers of columns: ", nc1, ", ", nc2)
  }
  if (no_colinfo(tt1) || no_colinfo(tt2)) {
    return(TRUE)
  }
  ci1 <- col_info(tt1)
  ci2 <- col_info(tt2)
  ## this will enforce same length and
  ## same names, in addition to same
  ## expressions so we dont need
  ## to check those separateley
  if (!identical(col_exprs(ci1), col_exprs(ci2))) {
    stop("Column structures not compatible: subset expression lists not identical")
  }

  if (any(!is.na(col_counts(ci2))) &&
    !identical(
      col_counts(ci1),
      col_counts(ci2)
    )) {
    stop("Column structures not compatible: 2nd column structure has non-matching, non-null column counts")
  }

  if (any(sapply(
    col_extra_args(ci2),
    function(x) length(x) > 0
  )) &&
    !identical(
      col_extra_args(ci1),
      col_extra_args(ci2)
    )) {
    stop(
      "Column structures not compatible: 2nd column structure has ",
      "non-matching, non-null extra args"
    )
  }

  if (any(nzchar(top_left(ci1))) && any(nzchar(top_left(ci2))) && !identical(top_left(ci1), top_left(ci2))) {
    stop(
      "Top-left materials not compatible: Got non-empty, non-matching ",
      "top-left materials. Clear them using top_left(x)<-character() ",
      "before binding to force compatibility."
    )
  }
  TRUE
}


#' Insert `rrow`s at (before) a specific location
#'
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated and will be removed in a future release of `rtables`. Please use
#' [insert_row_at_path()] or [label_at_path()] instead.
#'
#' @param tbl (`VTableTree`)\cr a `rtable` object.
#' @param rrow (`TableRow`)\cr an `rrow` to append to `tbl`.
#' @param at (`integer(1)`)\cr position into which to put the `rrow`, defaults to beginning (i.e. row 1).
#' @param ascontent (`flag`)\cr currently ignored.
#'
#' @return A `TableTree` of the same specific class as `tbl`.
#'
#' @note
#' Label rows (i.e. a row with no data values, only a `row.name`) can only be inserted at positions which do
#' not already contain a label row when there is a non-trivial nested row structure in `tbl`.
#'
#' @examples
#' o <- options(warn = 0)
#' lyt <- basic_table() %>%
#'   split_cols_by("Species") %>%
#'   analyze("Sepal.Length")
#'
#' tbl <- build_table(lyt, iris)
#'
#' insert_rrow(tbl, rrow("Hello World"))
#' insert_rrow(tbl, rrow("Hello World"), at = 2)
#'
#' lyt2 <- basic_table() %>%
#'   split_cols_by("Species") %>%
#'   split_rows_by("Species") %>%
#'   analyze("Sepal.Length")
#'
#' tbl2 <- build_table(lyt2, iris)
#'
#' insert_rrow(tbl2, rrow("Hello World"))
#' insert_rrow(tbl2, rrow("Hello World"), at = 2)
#' insert_rrow(tbl2, rrow("Hello World"), at = 4)
#'
#' insert_rrow(tbl2, rrow("new row", 5, 6, 7))
#'
#' insert_rrow(tbl2, rrow("new row", 5, 6, 7), at = 3)
#'
#' options(o)
#'
#' @export
insert_rrow <- function(tbl, rrow, at = 1,
                        ascontent = FALSE) {
  lifecycle::deprecate_warn(
    when = "0.4.0",
    what = "insert_rrow()",
    with = I("insert_row_at_path() or label_at_path()")
  )
  stopifnot(
    is(tbl, "VTableTree"),
    is(rrow, "TableRow"),
    at >= 1 && at <= nrow(tbl) + 1
  )
  chk_compat_cinfos(tbl, rrow)
  if (no_colinfo(rrow)) {
    col_info(rrow) <- col_info(tbl)
  }

  if (at == 1) {
    return(rbindl_rtables(list(rrow, tbl)))
  } else if (at == nrow(tbl) + 1) {
    return(rbind2(tbl, rrow))
  }

  ret <- recurse_insert(tbl, rrow,
    at = at,
    pos = 0,
    ascontent = ascontent
  )
  ret
}

.insert_helper <- function(tt, row, at, pos,
                           ascontent = FALSE) {
  islab <- is(row, "LabelRow")
  kids <- tree_children(tt)
  numkids <- length(kids)
  kidnrs <- sapply(kids, nrow)
  cumpos <- pos + cumsum(kidnrs)
  contnr <- if (is(tt, "TableTree")) {
    nrow(content_table(tt))
  } else {
    0
  }
  contnr <- contnr + as.numeric(labelrow_visible(tt))

  totnr <- nrow(tt)
  endpos <- pos + totnr
  atend <- !islab && endpos == at - 1
  if (at == pos + 1 && islab) {
    if (labelrow_visible(tt)) {
      stop("Inserting a label row at a position that already has a label row is not currently supported")
    }
    tt_labelrow(tt) <- row
    return(tt)
  }

  if (numkids == 0) {
    kids <- list(row)
  } else if (atend) {
    if (are(kids, "TableRow")) {
      kids <- c(kids, list(row))
    } else {
      kids[[numkids]] <- recurse_insert(
        kids[[numkids]],
        row = row,
        at = at,
        pos = pos + contnr + sum(kidnrs[-numkids]),
        ascontent = ascontent
      )
    }
  } else { # have >0 kids
    kidnrs <- sapply(kids, nrow)
    cumpos <- pos + cumsum(kidnrs)

    ## data rows go in the end of the
    ## preceding subtable (if applicable)
    ## label rows go in the beginning of
    ##  one at at
    ind <- min(
      which((cumpos + !islab) >= at),
      numkids
    )
    thekid <- kids[[ind]]

    if (is(thekid, "TableRow")) {
      tt_level(row) <- tt_level(thekid)
      if (ind == 1) {
        bef <- integer()
        aft <- 1:numkids
      } else if (ind == numkids) {
        bef <- 1:(ind - 1)
        aft <- ind
      } else {
        bef <- 1:ind
        aft <- (ind + 1):numkids
      }
      kids <- c(
        kids[bef], list(row),
        kids[aft]
      )
    } else { # kid is not a table row
      newpos <- if (ind == 1) {
        pos + contnr
      } else {
        cumpos[ind - 1]
      }

      kids[[ind]] <- recurse_insert(thekid,
        row,
        at,
        pos = newpos,
        ascontent = ascontent
      )
    } # end kid is not table row
  }
  tree_children(tt) <- kids
  tt
}

setGeneric("recurse_insert", function(tt, row, at, pos, ascontent = FALSE) standardGeneric("recurse_insert"))

setMethod(
  "recurse_insert", "TableTree",
  function(tt, row, at, pos, ascontent = FALSE) {
    ctab <- content_table(tt)
    contnr <- nrow(ctab)
    contpos <- pos + contnr
    islab <- is(row, "LabelRow")
    ## this will NOT insert it as
    if ((contnr > 0 || islab) && contpos > at) {
      content_table(tt) <- recurse_insert(ctab, row, at, pos, TRUE)
      return(tt)
    }

    .insert_helper(tt, row,
      at = at, pos = pos + contnr,
      ascontent = ascontent
    )
  }
)

setMethod(
  "recurse_insert", "ElementaryTable",
  function(tt, row, at, pos, ascontent = FALSE) {
    .insert_helper(tt, row,
      at = at, pos = pos,
      ascontent = FALSE
    )
  }
)
