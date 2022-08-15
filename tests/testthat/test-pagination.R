context("Pagination")


test_that("Page by splitting works", {



    lyt <- basic_table(title = "big title")  %>%
        split_rows_by("SEX", page_by = TRUE) %>%
        analyze("AGE")

    tt <- build_table(lyt, DM)

    ttlst <- paginate_table(tt)

    expect_identical(names(ttlst),
                     levels(DM$SEX))

    expect_error({basic_table(title = "big title")  %>%
        analyze("AGE") %>%
        split_rows_by("SEX", page_by = TRUE) %>%
            analyze("AGE")},
        "page_by splits cannot have top-level siblings")

    expect_error({basic_table() %>% split_rows_by("SEX") %>%
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

    expect_identical(tt[,1:2, keep_titles = TRUE,
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

})
