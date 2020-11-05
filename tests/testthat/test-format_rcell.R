test_that("formatting rcells works as expected", {
  expect_equal(format_rcell(3.13, "xx"), "3.13")
  expect_equal(format_rcell(4, "xx"), "4")
  
  expect_equal(format_rcell(4, "xx."), "4")
  expect_equal(format_rcell(3.13, "xx."), "3")
  
  expect_equal(format_rcell(4, "xx.x"), "4.0")
  expect_equal(format_rcell(3.13, "xx.x"), "3.1")
  expect_equal(format_rcell(3.67, "xx.x"), "3.7")
  
  expect_equal(format_rcell(4, "xx.xx"), "4.00")
  expect_equal(format_rcell(3.1, "xx.xx"), "3.10")
  expect_equal(format_rcell(3.67, "xx.xx"), "3.67")
  expect_equal(format_rcell(3.457, "xx.xx"), "3.46")
  
  expect_equal(format_rcell(4, "xx.xxx"), "4.000")
  expect_equal(format_rcell(3.1, "xx.xxx"), "3.100")
  expect_equal(format_rcell(3.67, "xx.xxx"), "3.670")
  expect_equal(format_rcell(3.457, "xx.xxx"), "3.457")
  expect_equal(format_rcell(9.8881, "xx.xxx"), "9.888")
  
  expect_equal(format_rcell(4, "xx.xxxx"), "4.0000")
  expect_equal(format_rcell(3.1, "xx.xxxx"), "3.1000")
  expect_equal(format_rcell(3.67, "xx.xxxx"), "3.6700")
  expect_equal(format_rcell(3.457, "xx.xxxx"), "3.4570")
  expect_equal(format_rcell(9.8881, "xx.xxxx"), "9.8881")
  expect_equal(format_rcell(9.88819, "xx.xxxx"), "9.8882")
  
  expect_equal(format_rcell(0.313, "xx%"), "31.3%")
  expect_equal(format_rcell(0.4, "xx%"), "40%")
  
  expect_equal(format_rcell(0.313, "xx.%"), "31%")
  expect_equal(format_rcell(0.4, "xx.%"), "40%")
  
  expect_equal(format_rcell(0.313, "xx.x%"), "31.3%")
  expect_equal(format_rcell(0.4, "xx.x%"), "40.0%")
  expect_equal(format_rcell(0.067, "xx.x%"), "6.7%")
  
  expect_equal(format_rcell(0.313, "xx.xx%"), "31.30%")
  expect_equal(format_rcell(0.4, "xx.xx%"), "40.00%")
  expect_equal(format_rcell(0.067, "xx.xx%"), "6.70%")
  expect_equal(format_rcell(0.0677, "xx.xx%"), "6.77%")
  expect_equal(format_rcell(0.06779, "xx.xx%"), "6.78%")
  
  expect_equal(format_rcell(0.313, "xx.xxx%"), "31.300%")
  expect_equal(format_rcell(0.4, "xx.xxx%"), "40.000%")
  expect_equal(format_rcell(0.067, "xx.xxx%"), "6.700%")
  expect_equal(format_rcell(0.0677, "xx.xxx%"), "6.770%")
  expect_equal(format_rcell(0.06779, "xx.xxx%"), "6.779%")
  expect_equal(format_rcell(0.067791, "xx.xxx%"), "6.779%")
  expect_equal(format_rcell(0.067799, "xx.xxx%"), "6.780%")
})
