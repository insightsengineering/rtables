context("value formatting")


test_that("sprintf_format works correctly", {


    myfun <- sprintf_format("hi there %1.4f")

    lyt <- basic_table() %>%
        split_cols_by("Species") %>%
        analyze("Sepal.Width", afun = mean, format = myfun)

    tbl <- build_table(lyt, iris)

    matform <- matrix_form(tbl)

    expect_identical(matform$strings[2, ],
                     c("mean", "hi there 3.4280",
                       myfun(2.77),
                       myfun(mean(subset(iris, Species == "virginica")$Sepal.Width))))
})



test_that("table_shell works", {

    tbl <- rtable(c("A", "B"),
                  rrow("Hiya",
                       rcell(c(2, .2),
                             format = "xx (xx.x%)"),
                       rcell(c(.1, .2)),
                       format = "xx.x - xx.x"),
                  rrow("bye", 5.2345, 17.2),
                  format = "xx.xx")


    tblsh <- rtable(c("A", "B"),
                  rrow("Hiya", "xx (xx.x%)", "xx.x - xx.x"),
                  rrow("bye", "xx.xx", "xx.xx"))

    expect_identical(toString(tblsh),
                     paste0(capture_output(table_shell(tbl)), "\n"))

    tbl2 <-  rtable(c("A", "B"),
                  rrow("Hiya",
                       rcell(c(2, .2),
                             format = function(x, ...) paste0(x)),
                       rcell(c(.1, .2)),
                       format = "xx.x - xx.x"),
                  rrow("bye", 5.2345, 17.2),
                  format = "xx.xx")

    tbl2sh <- rtable(c("A", "B"),
                    rrow("Hiya", "<fnc>", "xx.x - xx.x"),
                    rrow("bye", "xx.xx", "xx.xx"))

    expect_identical(toString(tbl2sh),
                     paste0(capture_output(table_shell(tbl2)), "\n"))

})

test_that("rcell format_na_str functionality works", {

    expect_identical(format_rcell(rcell(NA_real_,
                                        format = "xx.xx",
                                        format_na_str = "hiya")),
                     "hiya")

    ## default still works
    expect_identical(format_rcell(rcell(NA_real_, format = "xx.x")),
                     "NA")

    irs <- in_rows(val1 = NA_real_, val2 = NA_integer_,
                   .formats = list(val1 = "xx.x", val2 = "xx.x"),
                   .format_na_strs = list(val1 = "hiya", val2 = "lowdown"))
})

test_that("format_na_str functionality works in get_formatted_cells (ie printing) and make_afun", {

    DM2 <- subset(DM, COUNTRY %in% c("USA", "CAN", "CHN"))
    DM2$AGE <- NA
    DM2$AGE[1] <- 1

    s_summary <- function(x) {
        stopifnot(is.numeric(x))

        list(
            n = sum(!is.na(x)),
            mean = mean(x),
            min_max = range(x)
        )
    }

    a_summary <- make_afun(
        fun = s_summary,
        .formats = c(n = "xx", mean = "xx.xx", min_max = "xx.xx - xx.xx"),
        .labels = c(n = "n", mean = "Mean", min_max = "min - max")
    )

    a_summary3 <- make_afun(a_summary,
                            .formats = c(mean = "xx.xxx"),
                            .format_na_strs = c(mean = "Ridiculous"))

    l <- basic_table() %>%
        split_cols_by("ARM") %>%
        split_rows_by("COUNTRY", split_fun = drop_split_levels) %>%
        summarize_row_groups(label_fstr = "%s (n)") %>%
        analyze("AGE", afun = a_summary3, format = "xx.xx")

    tbl <- suppressWarnings(build_table(l, DM2))
    tbl
    expect_identical(get_formatted_cells(tbl)[3, 1, drop = TRUE],
                     "Ridiculous")
})

test_that("Test matrix obtained from get_formatted_cells as is into ASCII format", {
    
    # Table builder with rtables
    tbl <- basic_table() %>%
        split_cols_by("Species") %>%
        analyze(c("Sepal.Length", "Petal.Width"), 
                afun = mean, 
                format = NULL) %>% # NULL will be picked by format_rcell
        build_table(iris)
    
    # Get the ASCII table
    result <- get_formatted_cells(tbl) # Main function
    
    # Expected data-set is built with dplyr
    tibl <- iris %>% group_by(Species) %>% 
        summarise(m_sep = mean(Sepal.Length), 
                  m_pet = mean(Petal.Width)) %>% 
        select(-Species)
    
    # Get it into an ASCII-like format
    expected <- tibl %>% 
        mutate_all(as.character) %>% 
        mutate(a = "", b = "") %>% 
        select(a, m_sep, b, m_pet) %>% 
        t() %>% 
        as.matrix()
    
    dimnames(expected) <- NULL # fixing attributes
    
    # Check that preserve the format "as is"
    expect_identical(result, expected)
})

test_that("Test format matrix obtained from get_formatted_cells with only NULL format", {
    
    # Table builder with rtables
    tbl <- basic_table() %>%
        split_cols_by("Species") %>%
        analyze(c("Sepal.Length", "Petal.Width"), 
                afun = mean, 
                format = NULL) %>% 
        build_table(iris)
    
    # Get the ASCII table
    result <- get_formatted_cells(tbl, shell = TRUE) # Main function
    
    # Creating moking data
    col_std <- c("-", "xx", "-", "xx")
    expected <- cbind(col_std, col_std, col_std)
    
    dimnames(expected) <- NULL # fixing attributes
    
    # Check that preserve the format "as is"
    expect_identical(result, expected)
})

test_that("Test matrix obtained from get_formatted_cells with only specific format", {
    
    # Table builder with rtables
    tbl <- basic_table() %>%
        split_cols_by("Species") %>%
        analyze(c("Sepal.Length", "Petal.Width"), 
                afun = mean, 
                format = "xx.xxx") %>% 
        build_table(iris)
    
    # Get the ASCII table
    result <- get_formatted_cells(tbl, shell = TRUE) # Main function
    
    # Creating moking data
    col_std <- c("-", "xx.xxx", "-", "xx.xxx")
    expected <- cbind(col_std, col_std, col_std)
    
    dimnames(expected) <- NULL # fixing attributes
    
    # Check that preserve the specific
    expect_identical(result, expected)
})

test_that("Test matrix obtained from get_formatted_cells with only specific na values", {
    
    # Introducing NAs
    iris2 <- iris
    iris2$Sepal.Length[sample(1:nrow(iris2), size = 10)] <- NA 
    iris2$Petal.Width[sample(1:nrow(iris2), size = 10)] <- NA 
    
    # Custom summary function to add specific formats for na values to be reflected
    s_summary <- function(x) {
        stopifnot(is.numeric(x))
        list(
            mean = mean(x)
        )
    }
    
    afun <- make_afun(
        fun = s_summary,
        .formats = c(mean = "xx.xx"),
        .labels = c(mean = "Mean")
    )
    
    afun_isridiculous <- make_afun(afun,
                            .formats = c(mean = "xx.x"),
                            .format_na_strs = c(mean = "Ridiculous"))
    
    # Table builder with rtables
    tbl <- basic_table() %>%
        split_cols_by("Species") %>%
        analyze(c("Sepal.Length", "Petal.Width"), 
                afun = afun_isridiculous,
                inclNAs = TRUE,
                format = "xx.xxx"
                ) %>% 
        build_table(iris2)
    
    # Get the ASCII table
    result <- get_formatted_cells(tbl) # Main function
    
    # Creating moking data
    col_std <- c("", "Ridiculous", "", "Ridiculous")
    expected <- cbind(col_std, col_std, col_std)
    
    dimnames(expected) <- NULL # fixing attributes
    
    # Check that preserve the format "as is"
    expect_identical(result, expected)
})
