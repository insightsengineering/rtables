library(testthat)
library(xml2)
library(tibble)
library(rtables)

test_check("rtables", reporter = JunitReporter$new(file = "unit_test_results.xml"))
