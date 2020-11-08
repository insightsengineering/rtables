test_that("1D formats work as expected", {
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
  
  expect_equal(format_rcell(798, ">999.9"), "798.0")
  expect_equal(format_rcell(798.9, ">999.9"), "798.9")
  expect_equal(format_rcell(999.9, ">999.9"), "999.9")
  expect_equal(format_rcell(999.99, ">999.9"), ">999.9")
  expect_equal(format_rcell(1000, ">999.9"), ">999.9")
  
  expect_equal(format_rcell(798, ">999.99"), "798.00")
  expect_equal(format_rcell(798.9, ">999.99"), "798.90")
  expect_equal(format_rcell(999.99, ">999.99"), "999.99")
  expect_equal(format_rcell(999.991, ">999.99"), ">999.99")
  expect_equal(format_rcell(1000, ">999.99"), ">999.99")
  
  expect_equal(format_rcell(0.89, "x.xxxx | (<0.0001)"), "0.8900")
  expect_equal(format_rcell(0.0001, "x.xxxx | (<0.0001)"), "0.0001")
  expect_equal(format_rcell(0.00009, "x.xxxx | (<0.0001)"), "<0.0001")
  expect_equal(format_rcell(rcell(0.02, "x.xxxx | (<0.0001)")), "0.0200")
  expect_equal(format_rcell(rcell(0.0234934, "x.xxxx | (<0.0001)")), "0.0235")
  expect_equal(format_rcell(rcell(0.00000001, "x.xxxx | (<0.0001)")), "<0.0001")
})

test_that("2D formats work as expected", {
  expect_equal(format_rcell(c(3.42, 9), "xx / xx"), "3.42 / 9")
  expect_equal(format_rcell(c(3, 9.00001), "xx / xx"), "3 / 9.00001")
  
  expect_equal(format_rcell(c(3.42, 9), "xx. / xx."), "3 / 9")
  expect_equal(format_rcell(c(3, 9.00001), "xx. / xx."), "3 / 9")
  
  expect_equal(format_rcell(c(3.42, 9), "xx.x / xx.x"), "3.4 / 9.0")
  expect_equal(format_rcell(c(3, 9.00001), "xx.x / xx.x"), "3.0 / 9.0")
  
  expect_equal(format_rcell(c(3.42, 9), "xx.xx / xx.xx"), "3.42 / 9.00")
  expect_equal(format_rcell(c(3, 9.00001), "xx.xx / xx.xx"), "3.00 / 9.00")
  expect_equal(format_rcell(c(3.4, 9.997), "xx.xx / xx.xx"), "3.40 / 10.00")
  
  expect_equal(format_rcell(c(3.42, 9), "xx.xxx / xx.xxx"), "3.420 / 9.000")
  expect_equal(format_rcell(c(3, 9.00001), "xx.xxx / xx.xxx"), "3.000 / 9.000")
  expect_equal(format_rcell(c(3.4, 9.997), "xx.xxx / xx.xxx"), "3.400 / 9.997")
  expect_equal(format_rcell(c(3.4002, 9.9971), "xx.xxx / xx.xxx"), "3.400 / 9.997")
  
  expect_equal(format_rcell(c(87, 0.271), "xx (xx%)"), "87 (27.1%)")
  expect_equal(format_rcell(c(87, 0.27112), "xx (xx%)"), "87 (27.112%)")
  
  expect_equal(format_rcell(c(87, 0.271), "xx (xx.%)"), "87 (27%)")
  expect_equal(format_rcell(c(87, 0.27112), "xx (xx.%)"), "87 (27%)")
  
  expect_equal(format_rcell(c(87, 0.271), "xx (xx.x%)"), "87 (27.1%)")
  expect_equal(format_rcell(c(87, 0.27), "xx (xx.x%)"), "87 (27.0%)")
  expect_equal(format_rcell(c(87, 0.27112), "xx (xx.x%)"), "87 (27.1%)")
  
  expect_equal(format_rcell(c(87, 0.271), "xx (xx.xx%)"), "87 (27.10%)")
  expect_equal(format_rcell(c(87, 0.27), "xx (xx.xx%)"), "87 (27.00%)")
  expect_equal(format_rcell(c(87, 0.27112), "xx (xx.xx%)"), "87 (27.11%)")
  
  expect_equal(format_rcell(c(87, 0.271), "xx. (xx.%)"), "87 (27%)")
  expect_equal(format_rcell(c(87.09, 0.27), "xx. (xx.%)"), "87 (27%)")
  expect_equal(format_rcell(c(87.9, 0.278), "xx. (xx.%)"), "88 (28%)")
  
  expect_equal(format_rcell(c(87, 0.271), "xx.x (xx.x%)"), "87.0 (27.1%)")
  expect_equal(format_rcell(c(87.09, 0.27), "xx.x (xx.x%)"), "87.1 (27.0%)")
  expect_equal(format_rcell(c(87.9, 0.278), "xx.x (xx.x%)"), "87.9 (27.8%)")
  
  expect_equal(format_rcell(c(87, 0.271), "xx.xx (xx.xx%)"), "87.00 (27.10%)")
  expect_equal(format_rcell(c(87.09, 0.27), "xx.xx (xx.xx%)"), "87.09 (27.00%)")
  expect_equal(format_rcell(c(87.9, 0.278), "xx.xx (xx.xx%)"), "87.90 (27.80%)")
  
  expect_equal(format_rcell(c(0.5, 0.97621), "(xx, xx)"), "(0.5, 0.97621)")
  expect_equal(format_rcell(c(10, 14.2), "(xx, xx)"), "(10, 14.2)")
  expect_equal(format_rcell(c(100.02, 309.778), "(xx, xx)"), "(100.02, 309.778)")
  
  expect_equal(format_rcell(c(0.6, 0.97621), "(xx., xx.)"), "(1, 1)")
  expect_equal(format_rcell(c(10, 14.2), "(xx., xx.)"), "(10, 14)")
  expect_equal(format_rcell(c(100.02, 309.778), "(xx., xx.)"), "(100, 310)")
  
  expect_equal(format_rcell(c(0.6, 0.97621), "(xx.x, xx.x)"), "(0.6, 1.0)")
  expect_equal(format_rcell(c(10, 14.2), "(xx.x, xx.x)"), "(10.0, 14.2)")
  expect_equal(format_rcell(c(100.02, 309.778), "(xx.x, xx.x)"), "(100.0, 309.8)")
  
  expect_equal(format_rcell(c(0.6, 0.97621), "(xx.xx, xx.xx)"), "(0.60, 0.98)")
  expect_equal(format_rcell(c(10, 14.2), "(xx.xx, xx.xx)"), "(10.00, 14.20)")
  expect_equal(format_rcell(c(100.02, 309.778), "(xx.xx, xx.xx)"), "(100.02, 309.78)")
  
  expect_equal(format_rcell(c(0.6, 0.97621), "(xx.xxx, xx.xxx)"), "(0.600, 0.976)")
  expect_equal(format_rcell(c(10, 14.2), "(xx.xxx, xx.xxx)"), "(10.000, 14.200)")
  expect_equal(format_rcell(c(100.02, 309.778), "(xx.xxx, xx.xxx)"), "(100.020, 309.778)")
  expect_equal(format_rcell(c(100.02123, 309.77899), "(xx.xxx, xx.xxx)"), "(100.021, 309.779)")
  
  expect_equal(format_rcell(c(0.6, 0.97621), "(xx.xxxx, xx.xxxx)"), "(0.6000, 0.9762)")
  expect_equal(format_rcell(c(10, 14.2), "(xx.xxxx, xx.xxxx)"), "(10.0000, 14.2000)")
  expect_equal(format_rcell(c(100.02, 309.778), "(xx.xxxx, xx.xxxx)"), "(100.0200, 309.7780)")
  expect_equal(format_rcell(c(100.02123, 309.77899), "(xx.xxxx, xx.xxxx)"), "(100.0212, 309.7790)")
  
  expect_equal(format_rcell(c(-0.6, 0.97621), "xx - xx"), "-0.6 - 0.97621")
  expect_equal(format_rcell(c(10, 14.2), "xx - xx"), "10 - 14.2")
  expect_equal(format_rcell(c(100.02, 309.778), "xx - xx"), "100.02 - 309.778")
  
  expect_equal(format_rcell(c(0.6, 0.97621), "xx.x - xx.x"), "0.6 - 1.0")
  expect_equal(format_rcell(c(10, 14.2), "xx.x - xx.x"), "10.0 - 14.2")
  expect_equal(format_rcell(c(100.02, 309.778), "xx.x - xx.x"), "100.0 - 309.8")
  
  expect_equal(format_rcell(c(0.6, 0.97621), "xx.xx - xx.xx"), "0.60 - 0.98")
  expect_equal(format_rcell(c(-10, -14.2), "xx.xx - xx.xx"), "-10.00 - -14.20")
  expect_equal(format_rcell(c(100.02, 309.778), "xx.xx - xx.xx"), "100.02 - 309.78")
  
  expect_equal(format_rcell(c(87, 27.1), "xx.x (xx.x)"), "87.0 (27.1)")
  expect_equal(format_rcell(c(87.09, 27), "xx.x (xx.x)"), "87.1 (27.0)")
  expect_equal(format_rcell(c(87.9, 27.8), "xx.x (xx.x)"), "87.9 (27.8)")
  
  expect_equal(format_rcell(c(87, 27.1), "xx.xx (xx.xx)"), "87.00 (27.10)")
  expect_equal(format_rcell(c(87.09, 27), "xx.xx (xx.xx)"), "87.09 (27.00)")
  expect_equal(format_rcell(c(87.9, 27.8), "xx.xx (xx.xx)"), "87.90 (27.80)")
  expect_equal(format_rcell(c(87.916, 27.899), "xx.xx (xx.xx)"), "87.92 (27.90)")
  
  expect_equal(format_rcell(c(87, 27.1), "xx.x, xx.x"), "87.0, 27.1")
  expect_equal(format_rcell(c(-87.09, 27), "xx.x, xx.x"), "-87.1, 27.0")
  expect_equal(format_rcell(c(87.9, 27.8), "xx.x, xx.x"), "87.9, 27.8")
  
  expect_equal(format_rcell(c(87, 27.1), "xx.x to xx.x"), "87.0 to 27.1")
  expect_equal(format_rcell(c(87.09, 27), "xx.x to xx.x"), "87.1 to 27.0")
  expect_equal(format_rcell(c(87.9, 27.8), "xx.x to xx.x"), "87.9 to 27.8")
})

test_that("3D formats work as expected", {
  expect_equal(
    format_rcell(c(87, 80.1, 93.0234), "xx.xx (xx.xx - xx.xx)"),
    "87.00 (80.10 - 93.02)"
  )
  expect_equal(
    format_rcell(c(87.009, 80, 93.029), "xx.xx (xx.xx - xx.xx)"),
    "87.01 (80.00 - 93.03)"
  )
  expect_equal(
    format_rcell(rcell(c(0.02, -0.05, 0.0434235), "xx.xx (xx.xx - xx.xx)")),
    "0.02 (-0.05 - 0.04)"
  )
  expect_equal(
    format_rcell(rcell(c(12.34590, 3.2359, 324.2492), "xx.xx (xx.xx - xx.xx)")),
    "12.35 (3.24 - 324.25)"
  )
})


test_that("sprintf based formats work", {
  expect_equal(format_rcell(rcell(12.213743534, sprintf_format("%.3f"))), "12.214")
  expect_equal(format_rcell(rcell(12.2134543534, sprintf_format("%.3f"))), "12.213")
  expect_equal(format_rcell(rcell(c(12.21, 7.321), sprintf_format("%.1f and %.2f"))), "12.2 and 7.32")
})
