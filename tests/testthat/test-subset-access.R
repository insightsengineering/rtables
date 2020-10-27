context("Accessing and subsetting tables")

test_that("cell_values function works as desired", {
  l <- basic_table() %>% split_cols_by("ARM") %>%
    split_cols_by("SEX") %>%
    split_rows_by("RACE") %>%
    summarize_row_groups() %>%
    split_rows_by("STRATA1") %>%
    analyze("AGE", afun = function(x, .N_col, .N_row) rcell(c(.N_row, .N_col), format = "(xx.x, xx.x)"))

  ourdat <- DM
  ourdat$SEX <- droplevels(ourdat$SEX)
  ourdat$RACE <- droplevels(ourdat$RACE)
  tbl <- build_table(l, ourdat)

  armsextab <- table(ourdat$SEX, ourdat$ARM)

  armaval = "A: Drug X"
  cvres1 = cell_values(tbl, c("RACE", "ASIAN"), c("ARM", "A: Drug X", "SEX", "M"))
  contcount = nrow(subset(ourdat, RACE == "ASIAN" & ARM == armaval & SEX == "M"))
  asianstrata <- table(subset(ourdat, RACE == "ASIAN")$STRATA1)
  expect_identical(unname(cvres1),
                   list(list("A: Drug X.M" = c(contcount, contcount/armsextab["M", armaval])),
                        list("A: Drug X.M" = c(unname(asianstrata["A"]), armsextab["M", armaval])),
                        list("A: Drug X.M" = c(unname(asianstrata["B"]), armsextab["M", armaval])),
                        list("A: Drug X.M" = c(unname(asianstrata["C"]), armsextab["M", armaval]))))



  cvres2 <- cell_values(tbl, c("RACE", "ASIAN", "STRATA1"), c("ARM", "A: Drug X", "SEX", "M"))
  expect_identical(unname(cvres1[2:4]), unname(cvres2))
 cvres3 <- cell_values(tbl, c("RACE", "ASIAN", "STRATA1", "B"), c("ARM", "A: Drug X", "SEX", "M"))
  expect_identical(cvres3, cvres1[[3]])
  ## any arm, male columns from the ASIAN content (ie summary) row
  cvres4 <- cell_values(tbl, c("RACE", "ASIAN", "@content"))
  expect_identical(cvres4[2], cvres1[[1]])

 cvres5 <- cell_values(tbl, c("RACE", "ASIAN", "@content"), c("ARM", "*", "SEX", "M"))
  expect_identical(cvres5, cvres4[seq(2, 6, by=2)])
 ## all columns
 cvres6 <- cell_values(tbl,  c("RACE", "ASIAN", "STRATA1", "B"))

 ## all columns for the Combination arm
cvres7 <-  cell_values(tbl,  c("RACE", "ASIAN", "STRATA1", "B"), c("ARM", "C: Combination"))

  expect_identical(cvres6[5:6],
                   cvres7)
})


test_that("make_pagdf gives paths which all work", {
    ## duplicated from test-lyt-tabulation.R :(
    lyt = NULL %>% split_cols_by("ARM") %>%
        ## add nested column split on SEX with value lables from gend_label
        split_cols_by("SEX", "Gender", labels_var = "gend_label") %>%
        ## No row splits have been introduced, so this adds
        ## a root split and puts summary content on it labelled Overall (N)
        ## add_colby_total(label = "All") %>%
        ##    summarize_row_groups(label = "Overall (N)", format = "(N=xx)") %>%
        add_colcounts() %>%
        ## add a new subtable that splits on RACE, value labels from ethn_label
        split_rows_by("RACE", "Ethnicity", labels_var = "ethn_label") %>%
        summarize_row_groups("RACE", label_fstr = "%s (n)") %>%
        ##
        ## Add nested row split within Race categories for FACTOR2
        ## using a split function that excludes level C
        ## value labels from fac2_label
        split_rows_by("FACTOR2", "Factor2",
                            split_fun = remove_split_levels("C"),
                            labels_var = "fac2_label") %>%
        ## Add count summary within FACTOR2 categories
        summarize_row_groups("FACTOR2") %>%
        ## Add analysis/data rows by analyzing AGE variable
        ## Note afun is a function that returns 2 values in a named list
        ## this will create 2 data rows
        analyze("AGE", "Age Analysis", afun = function(x) list(mean = mean(x),
                                                                         median = median(x)),
                          format = "xx.xx") %>%
        ## adding more analysis vars "compounds them", placing them at the same
        ## level of nesting as all previous analysis blocks, rather than
        ## attempting to further nest them
        analyze("AGE", "Age Analysis redux", afun = range, format = "xx.x - xx.x",
                table_names = "AgeRedux") %>%

        ## Note nested=TRUE, this creates a NEW subtable directly under the
        ## root split
        ## afun of table() gives us k count rows, where k is the number of
        ## levels of VAR3, in this case 2.
        analyze("VAR3", "Var3 Counts", afun = list_wrap_x(table), nested = FALSE)



    tab = build_table(lyt, rawdat)

    pdf <- make_pagdf(tab)
    res <- lapply(pdf$path, function(pth) cell_values(tab, pth))
    expect(TRUE, "some paths in visible_only pag_df did not work")
    pdf2 <- make_pagdf(tab, visible_only = FALSE)
    res2 <- lapply(pdf2$path, function(pth) cell_values(tab, pth))
    expect(TRUE, "some paths in full structure pag_df did not work")




})
