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

    res2 <- paginate_table(tt, lpp = 75, cpp = 40)
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
        export_as_txt(ttlst[[1]][7:8, ]),
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
