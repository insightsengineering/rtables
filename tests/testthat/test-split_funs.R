context("Split Functions")

test_that("remove_split_levels works as expected with default flags", {
  my_split_fun <- remove_split_levels(excl = "ASIAN")
  
  l <- basic_table() %>% 
    split_cols_by("ARM") %>%
    split_rows_by("RACE", split_fun = my_split_fun) %>%
    summarize_row_groups()
  
  tab <- build_table(l, DM)
  
  expect_false("ASIAN" %in% row.names(tab))
})

test_that("remove_split_levels works as expected when dropping not appearing levels", {
  my_split_fun <- remove_split_levels(excl = "ASIAN", drop_levels = TRUE)
  
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

test_that("remove_split_levels works as expected when dropping not appearing levels and keeping data order", {
  my_split_fun <- remove_split_levels(excl = "ASIAN", drop_levels = TRUE, keep_order = TRUE)
  
  l <- basic_table() %>% 
    split_cols_by("ARM") %>%
    split_rows_by("RACE", split_fun = my_split_fun) %>%
    summarize_row_groups()
  
  DM <- DM[order(DM$RACE, decreasing = TRUE), ]
  tab <- build_table(l, DM)
  
  expect_identical(
    row.names(tab),
    setdiff(unique(DM$RACE), "ASIAN")
  )
})

test_that("remove_split_levels does not allow keeping order without dropping levels", {
  expect_error(remove_split_levels(excl = "ASIAN", drop_levels = FALSE, keep_order = TRUE))
})
