context("Accessor tests")

test_that("various accessors work at the layout/table level", {
  ## coltree
  col_extra_args <- rtables:::col_extra_args

  l <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_cols_by_multivar(c("AGE", "BMRKR1")) %>%
    analyze_colvars(list(mean, sd))

  pred <- coltree(l, DM)
  tbl <- build_table(l, DM)
  postd <- coltree(tbl)

  expect_identical(pred, postd)
  expect_identical(
    col_extra_args(pred),
    col_extra_args(postd)
  )

  expect_identical(
    col_exprs(l, DM),
    col_exprs(col_info(tbl))
  )

  expect_identical(
    clayout(tbl),
    postd
  )


  expect_identical(
    colcount_format(tbl),
    "(N=xx)"
  )

  ## even when not displayed
  colcount_format(tbl) <- "xx"
  expect_identical(
    colcount_format(tbl),
    "xx"
  )

  ccounts <- col_counts(tbl)

  expect_identical(
    col_counts(tbl, path = c("ARM", "B: Placebo", "multivars", "AGE")),
    ccounts[3]
  )

  newccs <- rep(7L, ncol(tbl))
  col_counts(tbl) <- newccs




  expect_identical(
    newccs,
    col_counts(tbl)
  )

  col_counts(tbl, path = c("ARM", "B: Placebo", "multivars", "BMRKR1")) <- 5L
  expect_identical(
    rep(c(7L, 5L, 7L), times = c(3, 1, 2)),
    col_counts(tbl)
  )

  col_total(tbl) <- 75L
  expect_identical(
    col_total(tbl),
    75L
  )




  disp_ccounts <- rtables:::disp_ccounts
  `disp_ccounts<-` <- rtables:::`disp_ccounts<-`

  olddisp <- disp_ccounts(tbl)

  disp_ccounts(tbl) <- !olddisp

  expect_identical(
    !olddisp,
    disp_ccounts(tbl)
  )

  l2 <- l

  top_left(l2) <- "Hiya!!!"

  expect_identical(top_left(l2), "Hiya!!!")

  tbl2 <- build_table(l2, DM)

  expect_identical(top_left(tbl2), "Hiya!!!")

  expect_identical(page_titles(tbl), character())


  page_titles(tbl) <- "Woah a page title!"
  expect_identical(
    page_titles(tbl),
    "Woah a page title!"
  )


  tt_level <- rtables:::tt_level
  `tt_level<-` <- rtables:::`tt_level<-`

  expect_identical(tt_level(tbl), 1L)

  tt_level(tbl) <- 2
  expect_error(
    table_inset(tbl) <- -1,
    "invalid table_inset value"
  )
})

test_that("Accessors for Split objects work", {
  myspl <- VarLevelSplit("AGE", "My age yo",
    labels_var = "AGE2",
    cfun = list(identity),
    cformat = "xx.x",
    split_fun = identity,
    split_format = "xx.xx",
    split_name = "names_bro",
    indent_mod = 1,
    cindent_mod = 2,
    extra_args = list("hiya"),
    child_labels = "default"
  )

  expect_identical(obj_label(myspl), "My age yo")
  obj_label(myspl) <- "new label"
  expect_identical(obj_label(myspl), "new label")

  expect_identical(obj_format(myspl), "xx.xx")
  obj_format(myspl) <- "xx.x"
  expect_identical(obj_format(myspl), "xx.x")

  split_exargs <- rtables:::split_exargs
  `split_exargs<-` <- rtables:::`split_exargs<-`

  expect_identical(split_exargs(myspl), list("hiya"))
  split_exargs(myspl) <- list("whaaaaaaaaaaaaat is this")
  expect_identical(split_exargs(myspl), list("whaaaaaaaaaaaaat is this"))

  label_kids <- rtables:::label_kids
  `label_kids<-` <- rtables:::`label_kids<-`
  expect_true(is.na(label_kids(myspl)))
  label_kids(myspl) <- "hidden"
  expect_identical(label_kids(myspl), FALSE)
  label_kids(myspl) <- NA
  expect_true(is.na(label_kids(myspl)))

  varlbs <- c("age", "biomarker1")
  mvarspl <- MultiVarSplit(c("AGE", "BMRKR1"),
    "My Multivar",
    varlabels = varlbs
  )


  spl_varnames <- rtables:::spl_varnames
  `spl_varnames<-` <- rtables:::`spl_varnames<-`

  expect_identical(
    spl_varnames(mvarspl),
    c("AGE", "BMRKR1")
  )

  spl_varnames(mvarspl) <- c("stuff1", "stuff2")
  expect_identical(
    spl_varnames(mvarspl),
    paste0("stuff", 1:2)
  )

  spl_varlabels <- rtables:::spl_varlabels
  `spl_varlabels<-` <- rtables:::`spl_varlabels<-`

  expect_identical(
    spl_varlabels(mvarspl),
    varlbs
  )

  spl_varlabels(mvarspl) <- LETTERS[1:2]
  expect_identical(
    spl_varlabels(mvarspl),
    LETTERS[1:2]
  )

  mvarspl2 <- MultiVarSplit(c("A", "B"))
  expect_identical(
    spl_varnames(mvarspl2),
    spl_varlabels(mvarspl2)
  )

  spl_varnames(mvarspl2) <- c("C", "D")
  expect_identical(
    spl_varnames(mvarspl2),
    spl_varlabels(mvarspl2)
  )
})

test_that("header sep setting works", {
  dflt <- default_hsep()

  hsep_test <- function(tab, exp) {
    expect_identical(horizontal_sep(tab), exp)
    expect_identical(horizontal_sep(tab[1:5, ]), exp)
    expect_identical(horizontal_sep(tab[, 1:3]), exp)
    expect_identical(horizontal_sep(tab[1:5, 1:3]), exp)
    expect_identical(horizontal_sep(tree_children(tab)[[1]]), exp)
    TRUE
  }
  lyt <- make_big_lyt()

  tbl <- build_table(lyt, rawdat)
  hsep_test(tbl, dflt)
  tbl2 <- tbl
  horizontal_sep(tbl2) <- "="
  hsep_test(tbl2, "=")
})

# section_div tests ------------------------------------------------------------
test_structure_with_a_getter <- function(tbl, getter, val_per_lev) {
  # Main table obj
  expect_identical(tbl %>% getter(), val_per_lev$global)
  # Its labelrow (could be also not visible)
  expect_identical(tt_labelrow(tbl) %>% getter(), val_per_lev$global_labelrow)

  # First split row + its labels
  split1 <- tree_children(tbl)[[1]]
  expect_identical(split1 %>% getter(), val_per_lev$split)
  expect_identical(tt_labelrow(split1) %>% getter(), val_per_lev$split_labelrow)

  # Content table checks if there
  content_elem_tbl <- content_table(split1)
  if (nrow(content_elem_tbl) > 0) {
    expect_identical(content_elem_tbl %>% getter(), val_per_lev$content)
    expect_identical(tt_labelrow(content_elem_tbl) %>% getter(), val_per_lev$content_labelrow)
    expect_identical(tree_children(content_elem_tbl)[[1]] %>% getter(), val_per_lev$contentrow)
  }

  ## The elementary table has it?
  leaves_elementary_tbl <- tree_children(split1)[[1]]
  expect_identical(leaves_elementary_tbl %>% getter(), val_per_lev$elem_tbl)
  expect_identical(tt_labelrow(leaves_elementary_tbl) %>% getter(), val_per_lev$elem_tbl_labelrow)

  # Data rows has it?
  for (i in seq_len(nrow(leaves_elementary_tbl))) {
    expect_identical(tree_children(leaves_elementary_tbl)[[i]] %>% getter(), val_per_lev$datarow[i])
  }
}


sdf_row_ok <- function(i, rdf) {
  trail_sep <- rdf$trailing_sep[i]
  pth <- rdf$path[[i]]
  sect_div_pth <- rdf$sect_div_from_path[[i]]
  self_div <- rdf$self_section_div[i]
  paths_match <- paste(pth, collapse = "") == paste(sect_div_pth, collapse = "")
  if (rdf$node_class[i] == "LabelRow") {
    ## label rows have NA for section_div_from_path even
    ## though they technically have a path value because
    ## its wrong and doesn't actually path to that row
    ## can't path to a label row
    ret <- is.na(sect_div_pth) &&
      (is.na(trail_sep) || trail_sep == self_div)
  } else if (is.na(trail_sep)) {
    ret <- is.na(self_div) &&
      (all(is.na(sect_div_pth)) || identical(pth, sect_div_pth))
  } else if (is.na(self_div)) {
    ## trail_sep is NOT na here, so it has to inherit from somewhere else
    ret <- !paths_match
  } else {
    ## assumes we set different seps on table and last row in table
    ## ok for our unit tests but not safe hard assumption "in the wild"
    divmatch <- trail_sep == self_div ## both non na
    ## seps match if and only if paths match
    ret <- paths_match == divmatch
  }
  ret
}

sect_div_info_ok <- function(tt, sdf = section_div_info(tt)) {
  expect_true(all(vapply(seq_len(nrow(sdf)), sdf_row_ok, rdf = sdf, TRUE)))
}

test_that("section_div getter and setter works", {
  df <- data.frame(
    cat = c(
      "re", "long"
    ),
    value = c(6, 3, 10, 1)
  )
  fast_afun <- function(x) list("m" = rcell(mean(x), format = "xx."), "m/2" = max(x) / 2)

  tbl <- basic_table() %>%
    split_rows_by("cat", section_div = "~") %>%
    analyze("value", afun = fast_afun, section_div = " ") %>%
    build_table(df)

  sdf <- section_div_info(tbl)
  sect_div_info_ok(sdf = sdf)

  ## basic section_div_at_path checks
  ## this should have NO section_divs set on individual rows...


  ## label row "pathing" is stupid and I hate it
  res <- expect_silent(vapply(sdf$path, function(pth) section_div_at_path(tbl, pth, labelrow = TRUE), ""))
  expect_true(all(is.na(res)))
  expect_equal(
    section_div_at_path(tbl, c("cat", "re")),
    "~"
  )
  expect_equal(
    section_div_at_path(tbl, c("cat", "re", "value")),
    " "
  )
  expect_error(section_div_at_path(tbl, c("*", "*", "*", "*", "*")),
    "Paths including '*' wildcards are not currently supported",
    fixed = TRUE
  )
  expect_error(
    section_div_at_path(tbl, c("cat", "lol")),
    "Path appears invalid"
  )

  tbl_content <- basic_table() %>%
    split_rows_by("cat", section_div = "~") %>%
    summarize_row_groups() %>% # This makes them not visible
    analyze("value", afun = fast_afun, section_div = " ") %>%
    build_table(df)

  sect_div_info_ok(tbl_content)

  val_per_lev <- list(
    "global" = NA_character_,
    "global_labelrow" = NA_character_,
    "split" = "~",
    "split_labelrow" = NA_character_,
    "content" = NA_character_, # If there is a summarize_row_groups this is present
    "contentrow" = NA_character_,
    "content_labelrow" = NA_character_,
    "elem_tbl_labelrow" = NA_character_,
    "elem_tbl" = " ",
    "datarow" = c(NA_character_, NA_character_)
  )

  # Checks of structure - precedence is top to bottom
  test_structure_with_a_getter(tbl,
    getter = trailing_section_div,
    val_per_lev = val_per_lev
  )
  test_structure_with_a_getter(tbl_content,
    getter = trailing_section_div,
    val_per_lev = val_per_lev
  )

  # Checks that section div and printing works together
  expect_identical(section_div(tbl), make_row_df(tbl)$trailing_sep)
  expect_identical(section_div(tbl_content), make_row_df(tbl_content)$trailing_sep)

  # MAIN assignment setter - this is clean, i.e. is only node base and not real section div

  ## this behavior is changed, but I think the new behavior
  ## is correct. we now have section_div_at_path (which
  ## supports * wildcards) to set actual section divs
  ## and section_div_info to easily determine the path
  ## an effective div is coming from

  section_div(tbl) <- letters[seq_len(nrow(tbl))]
  section_div(tbl_content) <- letters[seq_len(nrow(tbl))]
  sect_div_info_ok(tbl)

  val_per_lev <- list(
    "global" = NA_character_,
    "global_labelrow" = NA_character_,
    "split" = NA_character_,
    "split_labelrow" = "a",
    "content" = NA_character_,
    "contentrow" = NA_character_,
    "content_labelrow" = NA_character_,
    "elem_tbl_labelrow" = NA_character_,
    "elem_tbl" = NA_character_, # "c",
    "datarow" = c("b", "c")
  )

  val_per_lev_cont <- list(
    "global" = NA_character_,
    "global_labelrow" = NA_character_,
    "split" = NA_character_,
    "split_labelrow" = NA_character_, # "a",
    "content" = NA_character_,
    "contentrow" = "a", # NA_character_,
    "content_labelrow" = NA_character_,
    "elem_tbl_labelrow" = NA_character_,
    "elem_tbl" = NA_character_, # "c",
    "datarow" = c("b", "c")
  )

  # Checks of structure - precedence is top to bottom
  test_structure_with_a_getter(tbl,
    getter = trailing_section_div,
    val_per_lev = val_per_lev
  )
  test_structure_with_a_getter(tbl_content,
    getter = trailing_section_div,
    val_per_lev = val_per_lev_cont
  )

  # Checks that section div and printing works together
  expect_identical(section_div(tbl), make_row_df(tbl)$trailing_sep)
  expect_identical(section_div(tbl_content), make_row_df(tbl_content)$trailing_sep)

  separators <- strsplit(toString(tbl, widths = c(4, 10)), "\n")[[1]][c(4, 6, 9, 11, 13)]
  separators2 <- strsplit(toString(tbl_content, widths = c(4, 10)), "\n")[[1]][c(4, 6, 9, 11, 13)]
  expect_identical(separators, separators2)

  tbla <- tbl
  expect_warning({
    section_div(tbla) <- letters
  })
  expect_identical(tbl, tbla)

  # -1 is the table end
  expect_true(check_all_patterns(separators, letters[seq_len(nrow(tbl) - 1)], len = 17))

  ## https://github.com/insightsengineering/rtables/issues/1023
  ## section_div<-, section_div_at_path<-
  ## 'big', ie non-trivially structured, table
  big_tbl <- build_table(make_big_lyt(), rawdat)
  tbl3 <- tbl2 <- big_tbl
  sect_div_info_ok(tbl2)
  ltrs <- rep(letters, length.out = NROW(tbl2))
  section_div(tbl2) <- ltrs
  sect_div_info_ok(tbl2)
  txtvec <- strsplit(toString(tbl2), split = "\n")[[1]]
  sepinds <- seq(from = 6, to = length(txtvec) - 1, by = 2)
  wid <- nchar(txtvec[6])
  expect_true(check_all_patterns(txtvec[sepinds], head(ltrs, -1), len = wid))

  ## when full length vector is given, divs are set on individual rows
  rdf2 <- make_row_df(tbl2)
  expect_identical(rdf2$trailing_sep, rdf2$self_section_div)
  ## set subtable section div and have it override existing row div
  ## also support for wildcard in section_div_at_path<-, including
  ## ending on a wildcard
  tbl4 <- tbl2
  section_div_at_path(tbl4, c("RACE", "*", "FACTOR2", "*", "*")) <- "-"
  sdf4 <- section_div_info(tbl4)
  sect_div_info_ok(sdf = sdf4)
  txtvec4 <- strsplit(toString(tbl4), split = "\n")[[1]]

  ## bad paths of all the various types give informative errors
  expect_error(
    {
      section_div_at_path(tbl4, c("RACE", "*", "FACTOR2", "*", "*", "@content")) <- "-"
    },
    "Unable to resolve * in path",
    fixed = TRUE
  )
  expect_error(
    {
      section_div_at_path(tbl4, c("RACE", "feline", "FACTOR2", "*", "*")) <- "-"
    },
    "Unable to find child(ren) 'feline'",
    fixed = TRUE
  )
  expect_error(
    {
      section_div_at_path(tbl4, c("RACE", "*", "FACTOR2", "*", "*", "*"), labelrow = TRUE) <- "-"
    },
    "Unable to resolve * in path",
    fixed = TRUE
  )
  expect_error(
    {
      section_div_at_path(tbl4, c("RACE", "BLACK", "FACTOR2", "A", "AGE", "mean", "*")) <- "-"
    },
    "Path continues after resolving to individual row"
  )

  expect_equal(sum(sdf4$trailing_sep == "-"), 8)
  ltrinds4 <- c(5, 7, 11, 13, 18, 20, 24, 26)
  modltrs <- ltrs
  modltrs[ltrinds4] <- "-"
  expect_true(check_all_patterns(txtvec4[sepinds], head(modltrs, -1), wid))
  expect_false(identical(sdf4$trailing_sep, sdf4$self_section_div))

  ## crazy people with their crazy ... label and content rows
  ## both visible

  lytcrazy <- basic_table() |>
    split_rows_by("ARM", child_labels = "visible") |>
    summarize_row_groups() |>
    analyze("AGE")

  ## regression for unreported bug
  tblcrazy <- build_table(lytcrazy, DM)
  section_div(tblcrazy) <- letters[seq_len(nrow(tblcrazy))]
  txtcrazy <- strsplit(toString(tblcrazy), split = "\n")[[1]]
  expect_true(
    check_all_patterns(
      txtcrazy[seq(4, length(txtcrazy), by = 2)],
      letters[1:(nrow(tblcrazy) - 1)],
      len = nchar(txtcrazy[2])
    )
  )

  expect_error({
    section_div_at_path(tbl, c("*", "*", "*", "*", "*", "*")) <- " "
  })

  ## stupid for coverage
  rws <- collect_leaves(tbl, add.labrows = TRUE)
  rws2 <- expect_silent(lapply(rws, function(r) {
    section_div(r) <- "W"
    r
  }))
  expect_equal(
    vapply(rws2, section_div, "", USE.NAMES = FALSE),
    rep("W", length(rws))
  )
})

test_that("the split only setter works", {
  fast_afun <- function(x) list("m" = rcell(mean(x), format = "xx."), "m/2" = max(x) / 2)

  tbla <- tbl <- basic_table() %>%
    split_rows_by("ARM", section_div = "=") %>%
    split_rows_by("STRATA1", section_div = "-") %>%
    analyze("BMRKR1") %>%
    build_table(DM)
  sect_div_info_ok(tbl)
  replace_sec_div <- section_div(tbl)
  replace_sec_div[replace_sec_div == "="] <- "a"
  replace_sec_div[replace_sec_div == "-"] <- "b"

  section_div(tbl) <- c("a", "b")
  sdf <- section_div_info(tbl)
  expect_identical(section_div(tbl), replace_sec_div)
  expect_identical(sdf$trailing_sep, replace_sec_div)
  ## when only_sep_sections is true, divs are set on the subtable not
  ## the row
  expect_true(all(is.na(sdf$self_section_div)))

  sect_div_info_ok(sdf = sdf)

  expect_warning(
    {
      section_div(tbla, only_sep_sections = TRUE) <- letters
    },
    "Unable to find 4 levels of nesting in table structure."
  )
  ## the above is setting "c" on the analysis tables, but that doesn't
  ## matter since they are perfectly masked by the b's from the
  ## STRATA1 split facets. That's "probably wrong behavior" (setting
  ## the invisible 'c' section divs) technically, but it doesn't
  ## make any difference in practice and I'm not convinced we can make
  ## the nesting-detection heuristic smart enough to handle that
  ## successfully.
  ##
  ## Their text renderings are identical, though...
  expect_false(identical(tbl, tbla))
  expect_identical(toString(tbl), toString(tbla))

  # multiple analyze
  tbl <- basic_table(header_section_div = " ") %>%
    split_cols_by("ARM") %>%
    split_rows_by("SEX", split_fun = drop_split_levels) %>%
    analyze("AGE") %>%
    split_rows_by("RACE", split_fun = drop_split_levels) %>%
    split_rows_by("SEX", split_fun = drop_split_levels) %>%
    analyze("AGE") %>%
    build_table(DM)
  tbl2 <- tbl
  sect_div_info_ok(tbl)
  section_div(tbl) <- c("-", NA_character_)
  sect_div_info_ok(tbl)
  section_div(tbl2) <- c("-")
  sect_div_info_ok(tbl2)
  expect_identical(
    section_div(tbl)[seq_len(6)],
    c(NA_character_, "-", NA_character_, "-", NA_character_, NA_character_)
  )
  expect_identical(
    section_div(tbl),
    section_div(tbl2)
  )
})


test_that("header_section_div works", {
  lyt <- basic_table(header_section_div = "+") %>%
    split_rows_by("STRATA1") %>%
    analyze("BMRKR1")
  expect_identical(header_section_div(lyt), "+")
  header_section_div(lyt) <- "<"
  expect_identical(header_section_div(lyt), "<")

  tbl <- lyt %>% build_table(DM)
  expect_identical(header_section_div(tbl), "<")
  header_section_div(tbl) <- "+"
  expect_identical(header_section_div(tbl), "+")
  header_sdiv <- strsplit(toString(tbl), "\n")[[1]][3]

  expect_true(check_pattern(header_sdiv, "+", nchar(header_sdiv)))
})

test_that("top_level_section_div works", {
  lyt <- basic_table(top_level_section_div = "a") %>%
    split_cols_by("ARM") %>%
    split_rows_by("SEX", split_fun = drop_split_levels) %>%
    analyze("AGE") %>%
    split_rows_by("RACE", split_fun = drop_split_levels) %>%
    split_rows_by("SEX", split_fun = drop_split_levels) %>%
    analyze("AGE")
  tbl <- build_table(lyt, DM)
  expect_identical(top_level_section_div(lyt), "a")
  top_level_section_div(lyt) <- "="
  expect_identical(top_level_section_div(lyt), "=")
  tbl <- build_table(lyt, DM)
  top_lev_div_str <- strsplit(toString(tbl), "\n")[[1]][7]
  expect_true(check_pattern(top_lev_div_str, "=", nchar(top_lev_div_str)))
})
