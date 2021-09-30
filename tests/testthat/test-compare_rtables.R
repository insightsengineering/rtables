test_that("compare_rtables", {

    lyt <- make_big_lyt()

    ## hack to force all counts to "happen to be different"
    dat <- rawdat[- which(rawdat$ARM =="ARM2")[1:9],]
    tab <- build_table(lyt, dat)
    cmp <- compare_rtables(tab, tab)
    expect_true(all(cmp == "."))

    cmp_no4col <- compare_rtables(tab, tab[,-4])
    expect_true(all(cmp_no4col[, 4] == "+"))
    expect_true(all(cmp_no4col[,1:3] == "."))

    rdf <- make_row_df(tab)
    ## its not smart enought o shift columns yet...
    ## tol = .01 need for the medians to be different
    cmp_no3col <- compare_rtables(tab[,-3], tab, tol = .01)
    expect_true(all(cmp_no3col[,1:2] == "."))

    ## not checking ContentRows because of unfortunate identical counts in cols 3 and 4 :(
    expect_true(all(cmp_no3col[ rdf$node_class != "LabelRow", 3] == "X"))
    expect_true(all(cmp_no3col[,4] == "-"))
    cmp_no3colb <- compare_rtables(tab[,-3], tab, structure = TRUE)
    expect_true(all(cmp_no3colb[,1:2] == "."))
    expect_true(all(cmp_no3colb[,3] == "C"))
    expect_true(all(cmp_no3colb[,4] == "-"))

    cmp_bothsubset <- compare_rtables(tab[-5,], tab[,-2], tol = .01)

    expect_true(all(cmp_bothsubset[28,] == "-"))
    expect_true(all(cmp_bothsubset[-28,4] == "+"))
    expect_true(all(cmp_bothsubset[5:28,] != "."))

    stupid_cmp <- compare_rtables(tab, tab[,-1], tol = 1e5)
    expect_true(all(stupid_cmp[,1:3] == "."))


})
