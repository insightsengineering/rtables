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
