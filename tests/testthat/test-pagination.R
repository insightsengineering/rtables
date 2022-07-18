context("Pagination")


test_that("Page by splitting works", {



    lyt <- basic_table(title = "big title")  %>%
        split_rows_by("SEX", page_by = TRUE) %>%
        analyze("AGE")

    tt <- build_table(lyt, DM)

    ttlst <- paginate_table(tt)

    expect_identical(names(ttlst),
                     levels(DM$SEX))


})


test_that("vertical pagination works", {

    tt <- tt_to_export()
    res <- paginate_table(tt, lpp = NULL, cpp = 40)

    expect_identical(length(res), 3L)

    expect_identical(tt[,1:2], res[[1]])

    res2 <- paginate_table(tt, lpp = 75, cpp = 40)
    expect_identical(length(res2), 6L)
    expect_identical(res2[[1]], tt[1:71, 1:2])
    expect_identical(res2[[2]], tt[1:71, 3:4])
    expect_identical(res2[[4]], tt[c(38, 57, 64, 72:NROW(tt)), 1:2])

})
