context("Accessor tests")

test_that("various accessors work at the layout/table level", {
    ## coltree
    col_extra_args <- rtables:::col_extra_args

    l <- basic_table() %>%
        split_cols_by("ARM") %>%
        split_cols_by_multivar(c("AGE", "BMRKR1")) %>%
        analyze_colvars(list(mean, sd))

    pred <- coltree(l, DM)
    tbl <- build_table(l, DM)
    postd <- coltree(tbl)

    expect_identical(pred, postd)
    expect_identical(col_extra_args(pred),
                     col_extra_args(postd))

    expect_identical(col_exprs(l, DM),
                     col_exprs(col_info(tbl)))

    expect_identical(clayout(tbl),
                     postd)


    expect_identical(colcount_format(tbl),
                     "(N=xx)")

    ## even when not displayed
    colcount_format(tbl) <- "xx"
    expect_identical(colcount_format(tbl),
                     "xx")

    ccounts <- col_counts(tbl)

    expect_identical(col_counts(tbl, path = c("ARM", "B: Placebo", "multivars", "AGE")),
                     ccounts[3])

    newccs <-  rep(7L, ncol(tbl))
    col_counts(tbl) <- newccs




    expect_identical(newccs,
                     col_counts(tbl))

    col_counts(tbl, path = c("ARM", "B: Placebo", "multivars", "BMRKR1")) <- 5L
    expect_identical(rep(c(7L, 5L, 7L), times = c(3, 1, 2)),
                     col_counts(tbl))

    col_total(tbl) <- 75L
    expect_identical(col_total(tbl),
                     75L)




    disp_ccounts <- rtables:::disp_ccounts
    `disp_ccounts<-` <- rtables:::`disp_ccounts<-`

    olddisp <- disp_ccounts(tbl)

    disp_ccounts(tbl) <- !olddisp

    expect_identical(!olddisp,
                     disp_ccounts(tbl))

    l2 <- l

    top_left(l2) <- "Hiya!!!"

    expect_identical(top_left(l2), "Hiya!!!")

    tbl2 <- build_table(l2, DM)

    expect_identical(top_left(tbl2), "Hiya!!!")

    expect_identical(page_titles(tbl), character())


    page_titles(tbl) <- "Woah a page title!"
    expect_identical(page_titles(tbl),
                     "Woah a page title!")


    tt_level <- rtables:::tt_level
    `tt_level<-` <- rtables:::`tt_level<-`

    expect_identical(tt_level(tbl), 1L)

    tt_level(tbl) <- 2
    expect_error({table_inset(tbl) <- -1},
                 "invalid table_inset value")
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

    varlbs <- c("age", "biomarker1")
    mvarspl <- MultiVarSplit(c("AGE", "BMRKR1"),
                             "My Multivar", varlabels = varlbs)


    spl_varnames <- rtables:::spl_varnames
    `spl_varnames<-` <- rtables:::`spl_varnames<-`

    expect_identical(spl_varnames(mvarspl),
                     c("AGE", "BMRKR1"))

    spl_varnames(mvarspl) <- c("stuff1", "stuff2")
    expect_identical(spl_varnames(mvarspl),
                     paste0("stuff", 1:2))

    spl_varlabels <- rtables:::spl_varlabels
    `spl_varlabels<-` <- rtables:::`spl_varlabels<-`

    expect_identical(spl_varlabels(mvarspl),
                     varlbs)

    spl_varlabels(mvarspl) <- LETTERS[1:2]
    expect_identical(spl_varlabels(mvarspl),
                     LETTERS[1:2])

    mvarspl2 <- MultiVarSplit(c("A", "B"))
    expect_identical(spl_varnames(mvarspl2),
                     spl_varlabels(mvarspl2))

    spl_varnames(mvarspl2) <- c("C", "D")
    expect_identical(spl_varnames(mvarspl2),
                     spl_varlabels(mvarspl2))

})

test_that("header sep setting works", {
    dflt <- default_hsep()

    hsep_test <- function(tab, exp) {
        expect_identical(horizontal_sep(tab), exp)
        expect_identical(horizontal_sep(tab[1:5, ]), exp)
        expect_identical(horizontal_sep(tab[, 1:3]), exp)
        expect_identical(horizontal_sep(tab[1:5, 1:3]), exp)
        expect_identical(horizontal_sep(tree_children(tab)[[1]]), exp)
        TRUE
    }
    lyt <- make_big_lyt()

    tbl <- build_table(lyt, rawdat)
    hsep_test(tbl, dflt)
    tbl2 <- tbl
    horizontal_sep(tbl2) <- "="
    hsep_test(tbl2, "=")



})
