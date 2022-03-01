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
    res <- vpaginate_table(tt)

    expect_identical(length(res), 3L)

    expect_identical(tt[,1:2], res[[1]])

})
