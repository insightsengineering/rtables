context("Summarizing table structure")

test_that("path summaries", {

    lyt <- make_big_lyt()

    tbl <- build_table(lyt, rawdat)

    cpathsum <- col_paths_summary(tbl)

    arm1tmp <- c("ARM", "ARM1")
    arm2tmp <- c("ARM", "ARM2")
    expect_identical(cpathsum,
                     data.frame(label = c("ARM1", "Male", "Female",
                                          "ARM2", "Male", "Female"),
                                path = I(list(arm1tmp,
                                              c(arm1tmp, c("SEX", "M")),
                                              c(arm1tmp, c("SEX", "F")),
                                              arm2tmp,
                                              c(arm2tmp, c("SEX", "M")),
                                              c(arm2tmp, c("SEX", "F")))),
                                stringsAsFactors = FALSE))

    cpval <- col_paths(tbl)
    ## cpval doesn't contain the non-leaf paths
    expect_identical(cpval, cpathsum$path[-c(1, 4)])

    rpathsum <- row_paths_summary(tbl)
    ## defined in setup-fakedata.R
    expect_identical(complx_lyt_rnames,
                     rpathsum$label)

    expect_identical(row_paths(tbl),
                     rpathsum$path)

})
