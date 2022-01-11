context("Accessor tests")

test_that("various accessors work at the layout/table level", {
    ## coltree
    col_extra_args <- rtables:::col_extra_args
    clayout_splits <- rtables:::clayout_splits

    l <- basic_table() %>% split_cols_by("ARM") %>%
        split_cols_by_multivar(c("AGE", "BMRKR1")) %>%
        analyze_colvars(list(mean, sd))

    pred <- coltree(l, DM)
    tbl <- build_table(l, DM)
    postd <- coltree(tbl)
    expect_identical(col_extra_args(pred),
                     col_extra_args(postd))
    expect_identical(names(pred),
                     names(postd))
    ## expect_identical(clayout_splits(pred),
    ##                  clayout_splits(postd))

})

test_that("Accessors for Split objects work", {


    myspl <- VarLevelSplit("AGE", "My age yo",
                           labels_var = "AGE2",
                           cfun = list(identity),
                           cformat = "xx.x",
                           split_fun = identity,
                           split_format = "xx.xx",
                           split_name = "names_bro",
                           indent_mod = 1,
                           cindent_mod = 2,
                           extra_args = list("hiya"),
                           child_labels = "default")

    expect_identical(obj_label(myspl), "My age yo")
    obj_label(myspl) <- "new label"
    expect_identical(obj_label(myspl), "new label")

    expect_identical(obj_format(myspl), "xx.xx")
    obj_format(myspl) <- "xx.x"
    expect_identical(obj_format(myspl), "xx.x")

    split_exargs <- rtables:::split_exargs
    `split_exargs<-` <- rtables:::`split_exargs<-`

    expect_identical(split_exargs(myspl), list("hiya"))
    split_exargs(myspl) <- list("whaaaaaaaaaaaaat is this")
    expect_identical(split_exargs(myspl), list("whaaaaaaaaaaaaat is this"))

    label_kids <- rtables:::label_kids
    `label_kids<-` <- rtables:::`label_kids<-`
    expect_true(is.na(label_kids(myspl)))
    label_kids(myspl) <- "hidden"
    expect_identical(label_kids(myspl), FALSE)
    label_kids(myspl) <- NA
    expect_true(is.na(label_kids(myspl)))
})
