context("Pagination")


test_that("Page by splitting works", {



    lyt <- basic_table(title = "big title")  %>%
        split_rows_by("SEX", page_by = TRUE) %>%
        analyze("AGE")

    tt <- build_table(lyt, DM)

    ttlst <- paginate_table(tt)

    expect_identical(names(ttlst),
                     levels(DM$SEX))

    expect_error({
        basic_table(title = "big title")  %>%
            analyze("AGE") %>%
            split_rows_by("SEX", page_by = TRUE) %>%
            analyze("AGE")},
        "page_by splits cannot have top-level siblings")

    expect_error({
        basic_table() %>%
            split_rows_by("SEX") %>%
            split_rows_by("ARM", page_by = TRUE) %>%
            analyze("AGE")},
        "page_by splits cannot be nested within non-page_by splits")
})


test_that("vertical and horizontal pagination work", {

    tt <- tt_to_export()
    main_title(tt) <- "main title"
    main_footer(tt) <- "main footer"
    subtitles(tt) <- c("sub", "titles")
    prov_footer(tt) <- "prov footer"

    res <- paginate_table(tt, lpp = NULL, cpp = 40)

    expect_identical(length(res), 3L)

    expect_identical(tt[, 1:2, keep_titles = TRUE,
                        reindex_refs = FALSE], res[[1]])

    res2 <- paginate_table(tt, lpp = 75, cpp = 45)
    expect_identical(length(res2), 6L)
    expect_identical(res2[[1]], tt[1:63, 1:2, keep_titles = TRUE,
                                   reindex_refs = FALSE])
    expect_identical(res2[[2]], tt[1:63, 3:4,
                                   keep_titles = TRUE,
                                   reindex_refs = FALSE])
    expect_identical(res2[[4]], tt[c(38, 57, 64:NROW(tt)), 1:2,
                                   keep_titles = TRUE,
                                   reindex_refs = FALSE])

    expect_identical(main_title(tt),
                     main_title(res[[1]]))
    expect_identical(subtitles(tt), subtitles(res[[2]]))
    expect_identical(main_footer(tt), main_footer(res[[3]]))
    expect_identical(prov_footer(tt), prov_footer(res[[1]]))

    res2b <- paginate_table(tt, lpp = 75, cpp = 45, colwidths = c(12, rep(7, times = 6)))
    ## XXX TODO do careful analuysis to ensure this is actually right.
    expect_identical(nrow(res2b[[1]]), 59L)
    expect_identical(vapply(res2b, ncol, 1L), rep(3L, 4))



    lyt2 <- basic_table() %>%
        analyze("mpg",
                function(x) in_rows(.list = setNames(as.list(x), as.character(seq_along(x)))))

    tt2 <- build_table(lyt2, cbind(mtcars, mtcars))

    main_title(tt2) <- paste(strrep("a", 25),
                             strrep("A", 25))

    subtitles(tt2) <- c(paste(strrep("a", 25),
                              strrep("A", 25)),
                        "sub titles")

    main_footer(tt2) <- paste(strrep("a", 25),
                             strrep("A", 25))

    prov_footer(tt2) <- paste(strrep("a", 25),
                             strrep("A", 25))

    res3a <- paginate_table(tt2, lpp = 29,
                            cpp = 40, tf_wrap = TRUE, verbose = TRUE)
    res3b <- paginate_table(tt2, lpp = 25,
                            cpp = 40, tf_wrap = FALSE, verbose = TRUE)
    expect_true(all(mapply(function(a, b) identical(dim(a), dim(b)),
                           a = res3a, b = res3b)))

    res3c <- paginate_table(tt2, lpp = 29, cpp = 40, tf_wrap = FALSE, verbose = TRUE)
    expect_equal(nrow(res3a[[1]]),
                 nrow(res3c[[1]]) - 4)

})

test_that("inset and pagination work together", {
  tt <- tt_to_export()
  main_title(tt) <- "main title"
  subtitles(tt) <- c("sub", "-------", "titles")
  main_footer(tt) <- "main footer"
  prov_footer(tt) <- "prov footer"
  table_inset(tt) <- 5

  res <- paginate_table(tt, lpp = NULL, cpp = 40)

  expect_identical(length(res), 3L)

  expect_identical(tt[, 1:2,
    keep_titles = TRUE,
    reindex_refs = FALSE
  ], res[[1]])

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

    # propose_column_widths(matrix_form(tt_for_wrap, TRUE))
    pg_tbl_w_clw <- paginate_table(tt_for_wrap, lpp = lpp_tmp, colwidths = clw)
    pg_tbl_no_clw <- paginate_table(tt_for_wrap, lpp = lpp_tmp)
    res1 <- toString(matrix_form(pg_tbl_no_clw[[1]], TRUE))
    res2 <- toString(matrix_form(tt_for_wrap, TRUE))

    # The table should have 18 lines and 10 rows when it is not cell wrapped
    expect_identical(nrow(pg_tbl_no_clw[[1]]) + nrow(tt_for_wrap), 20L)
    expect_identical(.count_chr_from_str(res1, "\n") + .count_chr_from_str(res2, "\n"), 36L)

    # With column (+ 2 lines), cell (+ 2*2 lines), and row names wrapping (+1 line) gets to 25
    result <- paginate_table(tt_for_wrap, colwidths = clw, lpp = 25L)
    result_str <- toString(result[[1]], widths = clw)
    expect_identical(.count_chr_from_str(result_str, "\n"), 25L)

    ## Testing if the split happens with the right number of lines w/ pagination

    # Taking header and footer size (should be 10L with wrapping)
    tot_lines_w <- .count_chr_from_str(result_str, "\n") # 25L
    nr_res_w <- nrow(tt_for_wrap) + 5L # Cell (+ 2*2 l) and row values wrapping (+1 l)
    non_content_lines <- tot_lines_w - nr_res_w
    expect_identical(non_content_lines, 10L) # headers and footers with wrapping (+2 l)

    # Checking if the pages have the right amount of pages
    resw1 <- toString(pg_tbl_w_clw[[1]], widths = clw)
    resw2 <- toString(pg_tbl_w_clw[[2]], widths = clw) # context repetition is +2 lines
    exp_n_lines1 <- nrow(pg_tbl_w_clw[[1]]) +
        non_content_lines +
        2L + # Wrap of cell value
        1L # Wrap of rowname
    exp_n_lines2 <- nrow(pg_tbl_w_clw[[2]]) +
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
    lyt <- basic_table(title = "big title")  %>%
        split_rows_by("SEX", section_div = "~") %>%
        split_rows_by("ARM") %>%
        analyze("AGE")

    tt <- build_table(lyt, DM)

    ttlst <- paginate_table(tt, lpp = 20)

    expect_identical(length(ttlst), 2L)

    expect_identical(tt[1:14,
                        keep_titles = TRUE,
                        reindex_refs = FALSE
    ], ttlst[[1]])

    expect_identical(
        export_as_txt(ttlst[[1]][7:8, keep_titles = TRUE]),
        "big title\n\n——————————————\n       all obs\n——————————————\nMean    34.89 \n~~~~~~~~~~~~~~\nM             \n"
    )

    expect_identical(
        paste0(export_as_txt(tail(ttlst[[1]], 1)), export_as_txt(head(ttlst[[2]], 1))),
        paste0(
            "big title\n\n——————————————\n       all obs\n——————————————\nMean    34.28 \n",
            "big title\n\n———————————\n    all obs\n———————————\nU          \n"
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
    
    expect_error(
        paginate_table(tt, lpp = 3, min_siblings = 1),
        "Unable to find any valid pagination between 1 and 1"
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
