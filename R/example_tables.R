
#' Return a List of Example Tables
#' 
#' These tables can be used for examples and debugging
#' 
#' 
#' @export
#' 
#' @examples 
#' get_example_tables()
get_example_tables <- function() {
  
  # prepare data & functions
  n <- 400
  
  set.seed(1)
  
  df <- tibble(
    arm = factor(sample(c("Arm A", "Arm B"), n, replace = TRUE), levels = c("Arm A", "Arm B")),
    country = factor(sample(c("CAN", "USA"), n, replace = TRUE, prob = c(.55, .45)), levels = c("CAN", "USA")),
    gender = factor(sample(c("Female", "Male"), n, replace = TRUE), levels = c("Female", "Male")),
    handed = factor(sample(c("Left", "Right"), n, prob = c(.6, .4), replace = TRUE), levels = c("Left", "Right")),
    age = rchisq(n, 30) + 10
  ) %>% mutate(
    weight = 35 * rnorm(n, sd = .5) + ifelse(gender == "Female", 140, 180)
  ) 
  
  
  

  #
  list(
    "demographic" =  {
      basic_table() %>%
        split_cols_by("ARM") %>%
        analyze(c("SEX", "AGE")) %>%
        build_table(ex_adsl)
    },
    
    "dem2" = {
      basic_table() %>%
        split_cols_by("ARM") %>%
        split_cols_by("SEX") %>%
        analyze(c("AGE")) %>%
        build_table(ex_adsl)
    },
    
    
    "handed" = {
      basic_table() %>%
        split_cols_by("arm") %>%
        split_cols_by("gender") %>%
        add_colcounts() %>%
        split_rows_by("country") %>%
        summarize_row_groups() %>%
        split_rows_by("handed") %>%
        summarize_row_groups() %>%
        analyze("age", afun = mean, format = "xx.x") %>%
        build_table(df)
    }
    
  )
  
}