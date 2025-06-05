context("Pagination")


test_that("Page by splitting works", {
  lyt <- basic_table(title = "big title") %>%
    split_rows_by("SEX", page_by = TRUE) %>%
    analyze("AGE")

  tt <- build_table(lyt, DM)
  expect_true(has_force_pag(tt))

  expect_false(any(vapply(collect_leaves(tt, add.labrows = TRUE), has_force_pag, NA)))

  expect_false(has_force_pag(VarLevelSplit("AGE", "Age")))
  expect_true(has_force_pag(VarLevelSplit("AGE", "Age", page_prefix = "hi")))

  ttlst <- paginate_table(tt)
  expect_false(any(vapply(ttlst, has_force_pag, NA)))

  expect_identical(
    names(ttlst),
    levels(DM$SEX)
  )

  expect_error(
    {
      basic_table(title = "big title") %>%
        analyze("AGE") %>%
        split_rows_by("SEX", page_by = TRUE) %>%
        analyze("AGE")
    },
    "page_by splits cannot have top-level siblings"
  )

  expect_error(
    {
      basic_table() %>%
        split_rows_by("SEX") %>%
        split_rows_by("ARM", page_by = TRUE) %>%
        analyze("AGE")
    },
    "page_by splits cannot be nested within non-page_by splits"
  )

  lyt2 <- basic_table(
    title = "main title",
    subtitles = "subtitle",
    main_footer = "main footer",
    prov_footer = "provenance footer"
  ) %>%
    split_cols_by("ARM") %>%
    split_cols_by("SEX", split_fun = keep_split_levels(c("F", "M"))) %>%
    split_rows_by("STRATA1", split_fun = keep_split_levels(c("A", "B")), page_by = TRUE, page_prefix = "Stratum") %>%
    split_rows_by("RACE", split_fun = keep_split_levels(c("ASIAN", "WHITE"))) %>%
    summarize_row_groups() %>%
    analyze("AGE", afun = function(x, ...) {
      in_rows(
        "mean (sd)" = rcell(
          c(mean(x), sd(x)),
          format = "xx.x (xx.x)"
        ),
        "range" = rcell(range(x), format = "xx.x - xx.x")
      )
    })

  tbl2 <- build_table(lyt2, ex_adsl)

  ttlst2 <- paginate_table(tbl2, lpp = 16, cpp = 55)
  expect_equal(
    length(ttlst2),
    8
  )
  txt <- export_as_txt(tbl2, lpp = 16, cpp = 55)
  expect_true(grepl("Stratum: B", txt))
})

test_that("export_as_txt prints split level header correctly when using page_by", {
  # single level in page_by split
  tbl <- basic_table() %>%
    split_rows_by("PARAMCD",
      labels_var = "PARAMCD", split_label = "aaa",
      label_pos = "topleft",
      split_fun = drop_split_levels, page_by = TRUE
    ) %>%
    split_rows_by("AVISIT",
      label_pos = "topleft",
      split_fun = keep_split_levels("SCREENING")
    ) %>%
    analyze("AVAL") %>%
    build_table(ex_adlb[ex_adlb$PARAMCD == "ALT", ])
  tbl_txt <- tbl %>% export_as_txt(lpp = 100)

  expect_true(grepl("^\naaa: ALT", tbl_txt))

  # multiple levels in page_by split
  tbl <- basic_table() %>%
    split_rows_by("PARAMCD",
      labels_var = "PARAMCD", split_label = "aaa",
      label_pos = "topleft",
      split_fun = drop_split_levels, page_by = TRUE
    ) %>%
    split_rows_by("AVISIT",
      label_pos = "topleft",
      split_fun = keep_split_levels("SCREENING")
    ) %>%
    analyze("AVAL") %>%
    build_table(ex_adlb[ex_adlb$PARAMCD != "IGA", ])
  tbl_txt <- tbl %>% export_as_txt(lpp = 100)

  expect_true(grepl("^\naaa: ALT", tbl_txt))
  expect_true(grepl("\naaa: CRP", tbl_txt))
})

test_that("vertical and horizontal pagination work", {
  spoof_df <- data.frame(
    arm = factor(c("a", "b", "c", "d", "e", "f")),
    var1 = 6
  )
  simple_lyt <- basic_table() %>%
    split_cols_by("arm") %>%
    analyze("var1", function(x, ...) {
      in_rows(
        .list = replicate(30, list(1234)),
        .names = paste0("mynameis", 1:30)
      )
    })

  simple_tbl <- build_table(simple_lyt, spoof_df)

  ## rownames take up 10 char, repeated across horiz pag
  ## colheader takes up 2, repeated across vert pag
  ## all columns take up 7 (4 for content + 3 for cols sep)

  ## should be one col per page, i.e. 6 pages
  hpag1 <- paginate_table(simple_tbl, lpp = 80, cpp = 17)
  expect_equal(
    rep(1, 6),
    sapply(hpag1, ncol)
  )

  ## 16 is too small to fit any columns (After col divider)
  ## onto a page with the row names, so this is an error
  expect_error(paginate_table(simple_tbl, lpp = 80, cpp = 16))

  ## 23 is just barely still only enough to fit 1 col per page
  hpag2 <- paginate_table(simple_tbl, lpp = 80, cpp = 23)
  expect_identical(hpag1, hpag2)

  hpag3 <- paginate_table(simple_tbl, lpp = 80, cpp = 24, verbose = TRUE)
  expect_equal(
    rep(2, 3),
    sapply(hpag3, ncol)
  )

  ## preceding siblings <2
  expect_error(paginate_table(simple_tbl, lpp = 3, cpp = 120))

  vpag1 <- paginate_table(simple_tbl, lpp = 3, cpp = 120, min_siblings = 0)
  expect_equal(
    rep(1, 30),
    sapply(vpag1, nrow)
  )

  ## no lines for rows after header column
  expect_error(paginate_table(simple_tbl, lpp = 2, cpp = 120, min_siblings = 0))

  ## first lpp that allows the default min_siblings = 2 to succeed,
  ## 3 rows per page, 10 pages
  vpag2 <- paginate_table(simple_tbl, lpp = 5, cpp = 120)
  expect_equal(
    rep(3, 10),
    sapply(vpag2, nrow)
  )

  ## uneven pages, 8, 8, 8, 6 rows
  vpag3 <- paginate_table(simple_tbl, lpp = 10, cpp = 120)
  expect_equal(
    c(8, 8, 8, 6),
    sapply(vpag3, nrow)
  )

  ## combined pagination

  cpag1 <- paginate_table(simple_tbl, lpp = 5, cpp = 17)
  expect_equal(
    replicate(60, list(c(3, 1))),
    lapply(cpag1, dim)
  )
  ## ordering: horizontal pagination first then vertical in resulting list
  expect_equal(
    sapply(cpag1, names),
    rep(c("a", "b", "c", "d", "e", "f"), 10)
  )

  tt <- tt_to_export()
  main_title(tt) <- "main title"
  main_footer(tt) <- "main footer"
  subtitles(tt) <- c("sub", "titles")
  prov_footer(tt) <- "prov footer"

  res <- paginate_table(tt, lpp = NULL, cpp = 40)

  expect_identical(length(res), 3L)

  expect_identical(
    tt[, 1:2, keep_titles = TRUE, reindex_refs = FALSE],
    res[[1]]
  )

  ## this was lpp = 75, but manual line counting suggests that
  ## was a bad test, enforcing an off-by-one error.
  res2 <- paginate_table(tt, lpp = 76, cpp = 45, verbose = TRUE)
  expect_identical(length(res2), 6L)
  expect_identical(
    res2[[1]],
    tt[1:63, 1:2, keep_titles = TRUE, reindex_refs = FALSE]
  )
  expect_identical(
    res2[[2]],
    tt[1:63, 3:4, keep_titles = TRUE, reindex_refs = FALSE]
  )
  expect_identical(
    res2[[4]],
    tt[c(38, 57, 64:NROW(tt)), 1:2, keep_titles = TRUE, reindex_refs = FALSE]
  )

  expect_identical(
    main_title(tt),
    main_title(res[[1]])
  )
  expect_identical(subtitles(tt), subtitles(res[[2]]))
  expect_identical(main_footer(tt), main_footer(res[[3]]))
  expect_identical(prov_footer(tt), prov_footer(res[[1]]))

  res2b <- paginate_table(tt, lpp = 75, cpp = 45, colwidths = c(12, rep(7, times = 6)))
  ## XXX TODO do careful analuysis to ensure this is actually right.
  expect_identical(nrow(res2b[[1]]), 59L)
  expect_identical(vapply(res2b, ncol, 1L), rep(3L, 4))

  ## topleft perservation
  top_left(tt) <- "hahaha I'm a topleft"
  res3 <- paginate_table(tt, lpp = 75, cpp = 45)
  expect_identical(top_left(tt), top_left(res3[[1]]))



  lyt2 <- basic_table() %>%
    analyze(
      "mpg",
      function(x) in_rows(.list = setNames(as.list(x), as.character(seq_along(x))))
    )

  tt2 <- build_table(lyt2, cbind(mtcars, mtcars))

  main_title(tt2) <- paste(
    strrep("a", 25),
    strrep("A", 25)
  )

  subtitles(tt2) <- c(
    paste(
      strrep("a", 25),
      strrep("A", 25)
    ),
    "sub titles"
  )

  main_footer(tt2) <- paste(
    strrep("a", 25),
    strrep("A", 25)
  )

  prov_footer(tt2) <- paste(
    strrep("a", 25),
    strrep("A", 25)
  )

  res3a <- paginate_table(tt2,
    lpp = 29,
    cpp = 40, tf_wrap = TRUE
  )
  res3b <- paginate_table(tt2,
    lpp = 25,
    cpp = 40, tf_wrap = FALSE
  )
  expect_true(all(
    mapply(function(a, b) identical(dim(a), dim(b)), a = res3a, b = res3b)
  ))

  res3c <- paginate_table(tt2, lpp = 29, cpp = 40, tf_wrap = FALSE)
  expect_equal(
    nrow(res3a[[1]]),
    nrow(res3c[[1]]) - 4
  )
})

test_that("inset and pagination work together", {
  tt <- tt_to_export()
  main_title(tt) <- "main title"
  subtitles(tt) <- c("sub", "-------", "titles")
  main_footer(tt) <- "main footer"
  prov_footer(tt) <- "prov footer"
  table_inset(tt) <- 5

  res <- paginate_table(tt, lpp = NULL, cpp = 45, verbose = TRUE)

  expect_identical(length(res), 3L)

  expect_identical(
    tt[, 1:2, keep_titles = TRUE, reindex_refs = FALSE],
    res[[1]]
  )

  expect_identical(
    main_title(tt),
    main_title(res[[1]])
  )
  expect_identical(subtitles(tt), subtitles(res[[2]]))
  expect_identical(main_footer(tt), main_footer(res[[3]]))
  expect_identical(prov_footer(tt), prov_footer(res[[1]]))
})

test_that("cell and column wrapping works in pagination", {
  # Set colwidths vector
  clw <- c(5, 7, 6, 6) + 12
  lpp_tmp <- 18
  ## header mats: 5 [ 1 (title) + 2 (newline + div) + 1 (col labels) + 1 (div)]
  ## footer mats: 3 [ 1 (footer) + 2 (newline + div)
  ## adj rlpp: 10
  ## all rows fit on one page
  res1 <- pag_tt_indices(tt_for_wrap, lpp = lpp_tmp)
  expect_equal(length(res1), 1L)
  ## header mats: 7 [ 1 (title) + 2 (newline + div) + 3 (col labels) + 1 (div)]
  ## footer mats: 3 [ 1 (footer) + 2 (newline + div)
  ## adj rlpp: 8
  ## can't break after on BLACK OR AFRICAN AMERICAN row (label)
  ## 2 pages, one after full asian block, one after full BOAA block
  res2 <- pag_tt_indices(tt_for_wrap, lpp = lpp_tmp, colwidths = clw)
  expect_identical(
    res2,
    list(1:5, 6:10)
  )
  expect_identical(
    nlines(col_info(tt_for_wrap), colwidths = clw, fontspec = NULL),
    nlines(col_info(tt_for_wrap), fontspec = NULL) + 2L
  ) ## 2 new lines from wrapping

  pdf <- make_row_df(tt_for_wrap, colwidths = clw)
  expect_identical(
    pdf$self_extent,
    c(
      1L, 1L, 1L, 1L, 3L,
      2L, 1L, 1L, 1L, 3L
    )
  ) ## the 2 is row label wrap, 3s are cell wraps

  # propose_column_widths(matrix_form(tt_for_wrap, TRUE))
  pg_tbl_w_clw <- paginate_table(tt_for_wrap, lpp = lpp_tmp, cpp = NULL, colwidths = clw)
  pg_tbl_no_clw <- paginate_table(tt_for_wrap, lpp = lpp_tmp, cpp = NULL, verbose = TRUE)
  res1 <- toString(matrix_form(pg_tbl_no_clw[[1]], TRUE))
  res2 <- toString(matrix_form(tt_for_wrap, TRUE))

  # The table should have 18 lines and 10 rows when it is not cell wrapped
  expect_identical(nrow(pg_tbl_no_clw[[1]]) + nrow(tt_for_wrap), 20L)
  expect_identical(.count_chr_from_str(res1, "\n") + .count_chr_from_str(res2, "\n"), 36L)


  ## entire table takes exactly 25 lines when content is wrapped
  result <- paginate_table(tt_for_wrap, colwidths = clw, lpp = 25L, cpp = NULL)
  expect_identical(result[[1]], tt_for_wrap)

  ## paginating at 24 walks up
  ##  10 row too long with wrap -> 9 label row -> 8 ok
  ## paginates after row 8 (BOAA -> AGE-> Mean)
  result2 <- paginate_table(tt_for_wrap, colwidths = clw, lpp = 24L, cpp = NULL)
  expect_identical(
    sapply(result2, nrow),
    c(8L, 3L)
  )
  result_str <- toString(result[[1]], widths = clw)
  expect_identical(.count_chr_from_str(result_str, "\n"), 25L)

  ## Testing if the split happens with the right number of lines w/ pagination

  # Taking header and footer size (should be 10L with wrapping)
  tot_lines_w <- .count_chr_from_str(result_str, "\n") # 25L
  nr_res_w <- nrow(tt_for_wrap) + 5L # Cell (+ 2*2 l) and row values wrapping (+1 l)
  non_content_lines <- tot_lines_w - nr_res_w
  expect_identical(non_content_lines, 10L) # headers and footers with wrapping (+2 l)

  # Checking if the pages have the right amount of pages
  resw1 <- toString(result2[[1]], widths = clw)
  resw2 <- toString(result2[[2]], widths = clw) # context repetition is +2 lines
  exp_n_lines1 <- nrow(result2[[1]]) +
    non_content_lines +
    2L + # Wrap of cell value
    1L # Wrap of rowname
  exp_n_lines2 <- nrow(result2[[2]]) +
    non_content_lines +
    2L + # Wrap of cell value
    1L # Wrapping of rowname
  expect_identical(exp_n_lines1, .count_chr_from_str(resw1, "\n"))
  expect_identical(exp_n_lines2, .count_chr_from_str(resw2, "\n"))

  # Checking if the global number of lines is correctly split into pages
  paginated_lines <- exp_n_lines1 +
    exp_n_lines2 -
    non_content_lines - # Repeated header and footer
    2L # Repeated rowname for context and its wrapping
  expect_identical(paginated_lines, tot_lines_w)
})

test_that("Pagination works with section dividers", {
  lyt <- basic_table(title = "big title") %>%
    split_rows_by("SEX", section_div = "~") %>%
    split_rows_by("ARM") %>%
    analyze("AGE")

  tt <- build_table(lyt, DM)

  ttlst <- paginate_table(tt, lpp = 20)

  expect_identical(length(ttlst), 2L)

  expect_identical(
    tt[1:14, keep_titles = TRUE, reindex_refs = FALSE],
    ttlst[[1]]
  )

  expect_identical(
    export_as_txt(ttlst[[1]][7:8, keep_titles = TRUE], hsep = "-", paginate = FALSE),
    "big title\n\n--------------\n       all obs\n--------------\nMean    34.89 \n~~~~~~~~~~~~~~\nM             \n"
  )

  expect_identical(
    paste0(
      export_as_txt(tail(ttlst[[1]], 1), hsep = "-", paginate = FALSE),
      export_as_txt(head(ttlst[[2]], 1), hsep = "-", paginate = FALSE)
    ),
    paste0(
      "big title\n\n--------------\n       all obs\n--------------\nMean    34.28 \n",
      "big title\n\n-----------\n    all obs\n-----------\nU          \n"
    )
  )
})

test_that("Pagination works with non-default min_siblings", {
  lyt <- basic_table() %>%
    analyze("RACE")

  tt <- build_table(lyt, DM)

  ttlst <- expect_silent(paginate_table(tt, lpp = 3, min_siblings = 0))
  expect_identical(length(ttlst), nlevels(DM$RACE))
  expect_identical(tt[1], ttlst[[1]])

  suppressMessages(
    expect_error(
      paginate_table(tt, lpp = 3, min_siblings = 1),
      ".*Unable to find any valid pagination .*between rows 1 and 1.*"
    )
  )
})

test_that("Pagination works with wrapped titles/footers", {
  lyt <- basic_table() %>%
    split_cols_by("SEX") %>%
    analyze("RACE")

  tt <- build_table(lyt, DM)

  main_title(tt) <- "title with a\nnewline"
  main_footer(tt) <- "wrapped footer with\nnewline"

  res <- expect_silent(paginate_table(tt, cpp = 60, tf_wrap = TRUE))
  expect_identical(main_title(res[[1]]), main_title(res[[2]]))
  expect_identical(main_title(res[[1]]), main_title(tt))
  expect_identical(main_footer(res[[1]]), main_footer(res[[2]]))
  expect_identical(main_footer(res[[1]]), main_footer(tt))

  main_title(tt) <- "this is a long long table title that should be wrapped to a new line"
  main_footer(tt) <- "this is an extra long table main footer and should also be wrapped"

  res2 <- expect_silent(paginate_table(tt, cpp = 60, tf_wrap = TRUE))
  expect_equal(length(res2), 2)
  mf_res2 <- matrix_form(res2[[1]])
  nrow_res2 <- nrow(mf_strings(mf_res2)) + 5 + 4 # 5 lines tbl seps/ws + 4 lines title/footer

  res2_str1 <- toString(res2[[1]], tf_wrap = TRUE, max_width = 60)
  res2_str1_spl <- strsplit(res2_str1, split = "\n")[[1]]

  expect_equal(nrow_res2, length(res2_str1_spl))
  expect_true(all(nchar(res2_str1_spl)) <= 60)
  expect_equal(nchar(res2_str1_spl[1]), 59)
  expect_equal(nchar(res2_str1_spl[2]), 8)
  expect_equal(nchar(res2_str1_spl[nrow_res2 - 1]), 58)
  expect_equal(nchar(res2_str1_spl[nrow_res2]), 7)

  res2_str2 <- toString(res2[[2]], tf_wrap = TRUE, max_width = 60)
  res2_str2_spl <- strsplit(res2_str2, split = "\n")[[1]]

  expect_equal(nrow_res2, length(res2_str2_spl))
  expect_true(all(nchar(res2_str2_spl)) <= 60)
  expect_equal(nchar(res2_str2_spl[1]), 59)
  expect_equal(nchar(res2_str2_spl[2]), 8)
  expect_equal(nchar(res2_str2_spl[nrow_res2 - 1]), 58)
  expect_equal(nchar(res2_str2_spl[nrow_res2]), 7)
})

test_that("Pagination works with referential footnotes", {
  lyt <- basic_table(
    title = "main title",
    subtitles = "subtitle",
    main_footer = "main footer",
    prov_footer = "provenance footer"
  ) %>%
    split_cols_by("ARM") %>%
    split_cols_by("SEX", split_fun = keep_split_levels(c("F", "M"))) %>%
    split_rows_by("STRATA1", split_fun = keep_split_levels(c("A", "B")), page_by = TRUE, page_prefix = "Stratum") %>%
    split_rows_by("RACE", split_fun = keep_split_levels(c("ASIAN", "WHITE"))) %>%
    summarize_row_groups() %>%
    analyze("AGE", afun = function(x, ...) {
      in_rows(
        "mean (sd)" = rcell(
          c(mean(x), sd(x)),
          format = "xx.x (xx.x)"
        ),
        "range" = rcell(range(x), format = "xx.x - xx.x")
      )
    })

  tt <- build_table(lyt, ex_adsl)

  fnotes_at_path(tt, rowpath = c("STRATA1", "B", "RACE", "WHITE")) <- "3 Row footnote"
  fnotes_at_path(
    tt,
    rowpath = c("STRATA1", "A", "RACE", "WHITE", "AGE", "range"),
    colpath = c("ARM", "C: Combination", "SEX", "M")
  ) <- "2 Cell footnote"
  fnotes_at_path(tt, rowpath = c("STRATA1", "A", "RACE", "ASIAN")) <- "1 Row footnote"
  fnotes_at_path(
    tt,
    rowpath = c("STRATA1", "B", "RACE", "WHITE", "AGE", "mean (sd)"),
    colpath = c("ARM", "B: Placebo", "SEX", "F")
  ) <- "2 Cell footnote"

  main_title(tt) <- "title with a\nnewline"
  main_footer(tt) <- "wrapped footer with\nnewline"

  res <- expect_silent(paginate_table(tt, cpp = 60, tf_wrap = TRUE))
  expect_identical(main_title(res[[1]]), main_title(res[[2]]))
  expect_identical(main_title(res[[1]]), main_title(tt))
  expect_identical(main_footer(res[[1]]), main_footer(res[[2]]))
  expect_identical(main_footer(res[[1]]), main_footer(tt))

  main_title(tt) <- "this is a long long table title that should be wrapped to a new line"
  main_footer(tt) <- "this is an extra long table main footer and should also be wrapped"

  res <- expect_silent(paginate_table(tt, cpp = 60, tf_wrap = TRUE))
  expect_equal(length(res), 4)

  ref_fn_res1 <- matrix_form(res[[1]])$ref_fnote_df
  expect_equal(ref_fn_res1$msg, "1 Row footnote")
  expect_equal(ref_fn_res1$ref_index, 1)
  expect_equal(ref_fn_res1$symbol, "1")

  ref_fn_res2 <- matrix_form(res[[2]])$ref_fnote_df
  expect_equal(ref_fn_res2$msg, c("1 Row footnote", "2 Cell footnote"))
  expect_equal(ref_fn_res2$ref_index, 1:2)
  expect_equal(ref_fn_res2$symbol, c("1", "2"))

  ref_fn_res3 <- matrix_form(res[[3]])$ref_fnote_df
  expect_equal(ref_fn_res3$msg, c("3 Row footnote", "2 Cell footnote"))
  expect_equal(ref_fn_res3$ref_index, 1:2)
  expect_equal(ref_fn_res3$symbol, c("3", "2"))

  ref_fn_res4 <- matrix_form(res[[4]])$ref_fnote_df
  expect_equal(ref_fn_res4$msg, "3 Row footnote")
  expect_equal(ref_fn_res4$ref_index, 1)
  expect_equal(ref_fn_res4$symbol, "3")
})


test_that("setting colgap during pagination works", {
  tt <- tt_to_export()
  ## row labels take up 12, all other columns 10 + 3 (default colgap)
  ## so 2 cols per page, 3 pages total
  pags1 <- paginate_table(tt, lpp = NULL, cpp = 38)
  expect_equal(length(pags1), 3)
  ## increase col_gap by one prevents second column on each page
  ## so 6 pages
  pags2 <- paginate_table(tt, lpp = NULL, cpp = 38, col_gap = 4)
  expect_equal(length(pags2), ncol(tt))
  ## too wide a column gap, no columns fit after labels
  expect_error(suppressMessages(paginate_table(tt, lpp = NULL, cpp = 38, col_gap = 26)))
})
