test_that("make_subset_expr works", {
  ## Only need to explicitly test split types where the
  ## corresponding split_cols_by_* call accepts split_fun,
  ## as implementing those custom split behaviors is the only
  ## external use of this. The other case are both not useful
  ## to the end user and already covered by existing corresponding
  ## tests
  ##
  ## VarLevelSplit (ie default)
  varlvlspl <- VarLevelSplit("ARM", split_label = "Trial Arm")

  expr <- make_subset_expr(varlvlspl, "B: Placebo")
  dfsub1 <- DM[eval(expr, envir = DM),]
  expect_equal(NROW(dfsub1),
               sum(DM$ARM == "B: Placebo"))
  expect_equal(as.character(unique(dfsub1$ARM)),
               "B: Placebo")

  splv <- SplitValue("B: Placebo", sub_expr = quote((!is.na(ARM) & ARM %in% "A: Drug X")))

  ## ensure it grabs existing subset expr if available on val
  expect_identical(make_subset_expr(varlvlspl, splv),
                   make_subset_expr(varlvlspl, "A: Drug X"))

  ## MultiVarSplit
  multivarspl <- MultiVarSplit(c("SEX", "ARM"), "whaaaaat is this?")

  trueexpr <- expression(TRUE)
  expect_identical(make_subset_expr(multivarspl, "SEX"),
                   trueexpr)

  expect_identical(make_subset_expr(multivarspl, splv),
                   make_subset_expr(varlvlspl, splv))

  ## AllSplit (root split among other weird situations)

  allspl <- AllSplit()

  expect_identical(make_subset_expr(allspl, "eyo what?"),
                   trueexpr)
  ## current/prviously existing behavior does NOT check for
  ## an already defined subset on val.
  ## Not convinced that's right but I guess we'll leave it
  ## for now. XXX
  expect_identical(make_subset_expr(allspl, splv),
                   trueexpr)
})
