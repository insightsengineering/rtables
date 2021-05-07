context("headers and footers")
test_that("referential footnotes work", {
    analysisfun <- function(x, ...) {
        in_rows(row1 = 5,
                row2 = c(1, 2),
                .row_footnotes = list(row1 = "row 1 rfn"),
                .cell_footnotes = list(row2 = "row 2 cfn"))
    }

    lyt <- basic_table(title = "Title says Whaaaat", subtitles = "Oh, ok.",
                   main_footer = "ha HA! Footer!") %>%
    split_cols_by("ARM") %>%
    analyze("AGE", afun = analysisfun)

    result <-  build_table(lyt, ex_adsl)

    rdf <- make_row_df(result)
    expect_identical(rdf[2, "nrowrefs"], 0L)
    expect_identical(rdf[1, "nrowrefs"], 1L)
    expect_identical(rdf[2, "ncellrefs"], 3L)
    expect_identical(rdf[1, "ncellrefs"], 0L)
})
