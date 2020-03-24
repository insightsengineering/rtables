library(testthat)

test_results <- test_check("rtables")
saveRDS(test_results, "unit_testing_results.rds")
