context("sorting and pruning")

rawtable <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_cols_by("SEX") %>%
    split_rows_by("RACE") %>%
    summarize_row_groups() %>%
    split_rows_by("STRATA1") %>%
    summarize_row_groups() %>%
    analyze("AGE") %>%
    build_table(DM)


test_that("provided pruning functions work", {
    ## all_zero_or_na
    expect_false(all_zero_or_na(rrow("hi")), "Don't trim label rows")
    expect_true(all_zero_or_na(rrow("weird", NA, NaN, 0, 0L, Inf, -Inf)))

    ## content_all_zeros_nas
    racecounts <- table(DM$RACE)
    racecounts <- setNames(as.integer(racecounts), names(racecounts))
    expect_identical(sapply(tree_children(rawtable), content_all_zeros_nas), racecounts ==  0)
})

test_that("pruning and trimming work", {
    silly_prune <- function(tt) {
        if(!is(tt, "TableRow") || is(tt, "LabelRow"))
            return(FALSE)
        all_zero_or_na(tt)
    }

    smallertab <- basic_table() %>%
        split_cols_by("ARM") %>%
        split_rows_by("SEX") %>%
        analyze("AGE") %>%
        build_table(DM)

    ptab <- prune_table(smallertab, silly_prune)
    ## ensure that empty subtables are removed when pruning

    expect_identical(prune_table(smallertab),
                     smallertab[1:4,])




    ## this one doesn't remove NA rows
    expect_identical(prune_table(smallertab, prune_zeros_only),
                     smallertab)
    expect_identical(dim(ptab), c(4L, 3L))
    ## ensure/retain structure unawareness of trim_rows
    expect_identical(dim(trim_rows(smallertab)), c(6L, 3L))

    smallertab2 <- basic_table() %>%
        split_cols_by("ARM") %>%
        split_rows_by("SEX") %>%
        summarize_row_groups() %>%
        analyze("AGE") %>%
        build_table(DM)

    expect_identical(row.names(prune_table(smallertab)),
                     row.names(prune_table(smallertab2)))

    expect_identical(prune_table(smallertab2, low_obs_pruner(60, type = "mean")),
                     smallertab2[1:2,])

    expect_identical(prune_table(smallertab2, low_obs_pruner(60, type = "mean")),
                     smallertab2[1:2,])

    expect_identical(prune_table(smallertab2, low_obs_pruner(180)),
                     smallertab2[1:2,])



})

test_that("provided score functions work", {
    smallertab2 <- basic_table() %>%
        split_cols_by("ARM") %>%
        split_rows_by("SEX") %>%
        summarize_row_groups() %>%
        analyze("AGE") %>%
        build_table(DM)
    expect_true(is.na(cont_n_allcols(smallertab2)))
    kids <- tree_children(smallertab2)
    scores <- sapply(kids, cont_n_allcols)
    counts <- table(DM$SEX)
    expect_identical(scores, setNames(as.numeric(counts), names(counts)))

    onecol_fun <- cont_n_onecol(1)
    expect_true(is.na(cont_n_onecol(1)(smallertab2)))
    expect_true(is.na(cont_n_onecol(1)(smallertab2)))
    scores2 <- sapply(kids, onecol_fun)
    dmsub<- subset(DM, ARM == "A: Drug X")
    counts2 <- table(dmsub$SEX)
    expect_identical(scores2, setNames(as.numeric(counts2), names(counts2)))

})


## todo test sorting proper


## contributed by daniel
test_that("sort_at_path just returns an empty input table", {
    silly_prune_condition <- function(tt) {
        return(TRUE)
    }
    # Note: not sure why there is a warning from below. Probably separate problem.
    emptytable <- trim_rows(rawtable, silly_prune_condition)
    expect_identical(dim(emptytable), c(0L, ncol(rawtable)))
    result <- sort_at_path(
        emptytable,
        path = c("ARM", "*", "SEX"),
        scorefun = cont_n_allcols
    )
    expect_identical(emptytable, result)
})


test_that("trim_zero_rows, trim_rows, prune do the same thing in normal cases", {


    tbl <- basic_table() %>%
        split_rows_by("RACE") %>%
        analyze("COUNTRY") %>%
        build_table(ex_adsl)

    tzr_tbl1 <- trim_zero_rows(tbl)
    tr_tbl1 <- trim_rows(tbl)

    expect_true(all(unclass(compare_rtables(tzr_tbl1, tr_tbl1)) == "."))

    tbl2 <- basic_table() %>%
        split_cols_by("ARM") %>%
        split_rows_by("RACE") %>%
        analyze("COUNTRY") %>%
        build_table(ex_adsl)

    tzr_tbl2 <- trim_zero_rows(tbl2)
    tr_tbl2 <- trim_rows(tbl2)

    expect_true(all(unclass(compare_rtables(tzr_tbl2, tr_tbl2)) == "."))

    tbl3 <- basic_table() %>%
        split_cols_by("ARM") %>%
        split_cols_by("SEX") %>%
        split_rows_by("RACE") %>%
        analyze("COUNTRY") %>%
        build_table(ex_adsl)

    tzr_tbl3 <- trim_zero_rows(tbl3)
    tr_tbl3 <- trim_rows(tbl3)

    expect_true(all(unclass(compare_rtables(tzr_tbl3, tr_tbl3)) == "."))

    bigtbl <- basic_table() %>%
        split_rows_by("RACE") %>%
        split_rows_by("COUNTRY") %>%
        analyze("AGE") %>%
        build_table(ex_adsl)

    ptbl <- prune_table(bigtbl)
    nspl <- split(ex_adsl, ex_adsl$RACE)
    num <- sum(sapply(nspl, function(df) 2*length(unique(df$COUNTRY))),
               length(unique(ex_adsl$RACE)))
    expect_equal(nrow(ptbl), num)

    tr_tbl <- trim_rows(bigtbl)
    expect_true(nrow(tr_tbl) > num)
})
