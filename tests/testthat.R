library(testthat)
test_check("rtables", reporter = JunitReporter$new(file = "unit_test_results.xml"))
