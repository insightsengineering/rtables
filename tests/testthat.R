library(testthat)
library(xml2)

test_check("rtables", reporter = JunitReporter$new(file = "unit_test_results.xml"))
