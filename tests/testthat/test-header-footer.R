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


    analysisfun2 <- function(x, cutoff,  ...) {
        mn <- mean(x)
        if(mn >= cutoff)
            cf = list(mean = "Elevated group mean")
        else
            cf = list()
        in_rows(mean = mean(x),
                range = range(x),
                .cell_footnotes = cf,
                .formats = list(mean = "xx.x", range = "xx - xx"))
    }
    lyt2 <- basic_table() %>%
        split_cols_by("ARM") %>%
        analyze("AGE", afun = analysisfun2, extra_args = list(cutoff = mean(ex_adsl$AGE)))
    tbl <- build_table(lyt2, ex_adsl)
    rdf2 <- make_row_df(tbl)
    expect_identical(rdf2$nrowrefs, c(0L, 0L))
    expect_identical(rdf2$ncellrefs, c(2L, 0L))
})


test_that("post-processing addition of referential footnotes works", {

    race_levels <- c("WHITE",
                     "BLACK OR AFRICAN AMERICAN",
                     "ASIAN",
                     "AMERICAN INDIAN OR ALASKA NATIVE",
                     "MULTIPLE")
    l1 <- basic_table() %>% split_cols_by("ARM") %>%
        split_rows_by("RACE", split_fun = keep_split_levels(race_levels)) %>%
        summarize_row_groups() %>%
        analyze("AGE", mean)


    tb1 <- build_table(l1, DM)
    fnotes_at_path(tb1, rowpath = c("RACE", "WHITE", "AGE", "mean"),
                   colpath = c("ARM", "B: Placebo")) <- "white arm b mean"

    fnotes_at_path(tb1, rowpath = c("RACE", "ASIAN", "@content", "ASIAN"),
                   colpath = c("ARM", "C: Combination")) <- "asian arm c content"

    fnotes_at_path(tb1, rowpath = c("RACE", "MULTIPLE", "@content", "MULTIPLE"),
                   colpath = NULL) <-  c("race multiple row fn 1", "race multiple row fn 2")

    rdf <- make_row_df(tb1)

    expect_identical(rdf$nrowrefs[[9]], 2L)
    expect_identical(rdf$ncellrefs,
                     c(0L, 1L, 0L, 0L, 1L, rep(0L, 5)))

})
