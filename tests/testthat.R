library(testthat)
library(xml2)
library(tibble)

test_check("rtables", reporter = JunitReporter$new(file = "unit_test_results.xml"))
