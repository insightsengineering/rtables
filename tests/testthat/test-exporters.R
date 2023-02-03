context("Exporters")

test_that("tsv roundtripping for path_enriched_df", {

    tbl2 <- tt_to_export()

    df <- path_enriched_df(tbl2)

    tmptsv <- tempfile(fileext = ".tsv")

    export_as_tsv(tbl2, file = tmptsv)

    newdf <- import_from_tsv(tmptsv)

    expect_true(all(sapply(newdf, is.list)))
    expect_equal(unclass(newdf[1, 2]), #AsIs "class"
                 list(as.character(c(16,
                                     16 / sum(ex_adsl$ARM == "A: Drug X" &
                                                  ex_adsl$SEX == "M")))))
})

test_that("as_html smoke test", {

    tmpf <- tempfile(fileext = ".html")

    tbl <- tt_to_export()
    oldo <- options(viewer = identity)
    fl <- Viewer(tbl)
    xml2::read_html(fl)
    expect_true(TRUE)
    options(oldo)
})


## https://github.com/Roche/rtables/issues/308
test_that("path_enriched_df works for tables with a column that has all length 1 elements", {

    my_table <- basic_table() %>%
        split_rows_by("Species") %>%
        analyze("Petal.Length") %>%
        build_table(df = iris)
    mydf <- path_enriched_df(my_table)
    expect_identical(dim(mydf), c(3L, 2L))
})
