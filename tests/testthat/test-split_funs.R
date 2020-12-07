context("Split Functions")

test_that("remove_split_levels works as expected with factor variables", {
  my_split_fun <- remove_split_levels(excl = "ASIAN")
  
  stopifnot(is.factor(DM$RACE))
  l <- basic_table() %>% 
    split_cols_by("ARM") %>%
    split_rows_by("RACE", split_fun = my_split_fun) %>%
    summarize_row_groups()
  
  tab <- build_table(l, DM)
  
  expect_false("ASIAN" %in% row.names(tab))
})

test_that("remove_split_levels works as expected with character variables", {
  my_split_fun <- remove_split_levels(excl = "ASIAN")
  
  l <- basic_table() %>% 
    split_cols_by("ARM") %>%
    split_rows_by("RACE", split_fun = my_split_fun) %>%
    summarize_row_groups()
  
  DM2 <- DM
  DM2$RACE <- as.character(DM2$RACE)
  tab <- build_table(l, DM2)
  
  expect_false("ASIAN" %in% row.names(tab))
})

test_that("drop_and_remove_levels works as expected when dropping not appearing levels", {
  my_split_fun <- drop_and_remove_levels(excl = "ASIAN")
  
  l <- basic_table() %>% 
    split_cols_by("ARM") %>%
    split_rows_by("RACE", split_fun = my_split_fun) %>%
    summarize_row_groups()
  
  tab <- build_table(l, DM)
  
  expect_setequal(
    row.names(tab),
    setdiff(unique(DM$RACE), "ASIAN")
  )
})

test_that("drop_and_remove_levels also works with character variables", {
  my_split_fun <- drop_and_remove_levels(excl = "ASIAN")
  
  l <- basic_table() %>% 
    split_cols_by("ARM") %>%
    split_rows_by("RACE", split_fun = my_split_fun) %>%
    summarize_row_groups()
  
  DM2 <- DM
  DM2$RACE <- as.character(DM2$RACE)
  tab <- build_table(l, DM2)
  
  expect_setequal(
    row.names(tab),
    setdiff(unique(DM$RACE), "ASIAN")
  )
})
