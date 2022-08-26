context("Exporters")

test_that("export_as_txt works with and without pagination", {

    lyt <- basic_table() %>%
        split_cols_by("ARM") %>%
        analyze(c("AGE", "BMRKR2", "COUNTRY"))

    tbl <- build_table(lyt, ex_adsl)

    tmptxtf <- tempfile()
    export_as_txt(tbl, file = tmptxtf, paginate = TRUE, lpp = 8)
    txtlns <- readLines(tmptxtf)
    expect_identical(grep("\\\\s\\\\n", txtlns),
                     c(9L, 17L))

    expect_identical(toString(tbl),
                     export_as_txt(tbl, file = NULL, paginate = FALSE))
})


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

test_that("export_as_pdf works", {

    tbl <- tt_to_export()
    tmpf <- tempfile(fileext = ".pdf")

    expect_warning(export_as_pdf(tbl, file = tmpf, width = 1, paginate = FALSE),
                   "width of page 1 exceeds the available space")
    expect_true(file.exists(tmpf))
    expect_warning(export_as_pdf(tbl, file = tmpf, height = 1, paginate = FALSE),
                   "height of page 1 exceeds the available space")

    res <- export_as_pdf(tbl, file = tmpf)

    expect_equal(res$npages, 3)

})


test_that("flextable export works", {

    analysisfun <- function(x, ...) {
        in_rows(row1 = 5,
                row2 = c(1, 2),
                .row_footnotes = list(row1 = "row 1 - row footnote"),
                .cell_footnotes = list(row2 = "row 2 - cell footnote"))
    }

    lyt <- basic_table() %>%
        split_cols_by("ARM") %>%
        split_cols_by("SEX", split_fun = keep_split_levels(c("M", "F"))) %>%
        split_rows_by("STRATA1") %>%
        summarize_row_groups() %>%
        split_rows_by("RACE", split_fun = keep_split_levels(c("WHITE", "ASIAN"))) %>%
        analyze("AGE", afun = analysisfun)


    tbl <-  build_table(lyt, ex_adsl)
    ft <- tt_to_flextable(tbl, total_width = 20)
    expect_equal(sum(unlist(nrow(ft))), 20)
    ft

    ft2 <- tt_to_flextable(tbl, paginate = TRUE, lpp = 20)
    expect_equal(length(ft2), 6)
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
