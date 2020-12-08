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
    expect_identical(dim(ptab), c(4L, 3L))
    ## ensure/retain structure unawareness of trim_rows
    expect_identical(dim(trim_rows(smallertab)), c(6L, 3L))

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
})


## todo test sorting proper


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
