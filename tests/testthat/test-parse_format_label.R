test_that("parsing format labels works", {
  expect_error(parse_format_label("xx"), NA)
  expect_equal(parse_format_label("xx"), quote(sprintf("%s", x)))
  expect_equal(
    parse_format_label("xx (xx)"),
    quote(sprintf("%s (%s)", x[1L], x[2L]))
  )
  expect_equal(
    parse_format_label("xx (xx.x%)"),
    quote(sprintf("%s (%.1f%%)", x[1L], x[2L]*100))
  )
  expect_equal(
    parse_format_label("xx. (xx.)"),
    quote(sprintf("%.0f (%.0f)", x[1L], x[2L]))
  )
  expect_equal(
    parse_format_label("xx.x (xx.x)"),
    quote(sprintf("%.1f (%.1f)", x[1L], x[2L]))
  )
  expect_equal(
    parse_format_label("xx.xx (xx.xx)"),
    quote(sprintf("%.2f (%.2f)", x[1L], x[2L]))
  )
  expect_equal(
    parse_format_label("xx.x (xx.xx)"),
    quote(sprintf("%.1f (%.2f)", x[1L], x[2L]))
  )
  expect_equal(
    parse_format_label("xx.xx (xx.xx - xx.xx)"),
    quote(sprintf("%.2f (%.2f - %.2f)", x[1L], x[2L], x[3L]))
  )
  expect_equal(
    parse_format_label("xx. / xx."),
    quote(sprintf("%.0f / %.0f", x[1L], x[2L]))
  )
  expect_equal(
    parse_format_label("(N=xx)"),
    quote(sprintf("(N=%s)", x))
  )
  expect_equal(
    parse_format_label("xx.xxx%"),
    quote(sprintf("%.3f%%", x*100))
  )
  expect_equal(
    parse_format_label("xx%"),
    quote(sprintf("%s%%", x*100))
  )
})
