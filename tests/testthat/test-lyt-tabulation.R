context("tabulation framework")




## this 
test_that("complex layout works", {
    lyt = NULL %>% split_cols_by("ARM", "Arm") %>%
        ## add nested column split on SEX with value lables from gend_label
        split_cols_by("SEX", "Gender", labels_var = "gend_label") %>%
        ## No row splits have been introduced, so this adds
        ## a root split and puts summary content on it labelled Overall (N)
        ## add_colby_total(label = "All") %>%
        ##    summarize_row_groups(label = "Overall (N)", format = "(N=xx)") %>%
        add_colcounts() %>%
        ## add a new subtable that splits on RACE, value labels from ethn_label
        split_rows_by("RACE", "Ethnicity", labels_var = "ethn_label") %>%
        summarize_row_groups("RACE", label_fstr = "%s (n)") %>%
        ##
        ## Add nested row split within Race categories for FACTOR2
        ## using a split function that excludes level C
        ## value labels from fac2_label
        split_rows_by("FACTOR2", "Factor2",
                            split_fun = remove_split_levels("C"),
                            labels_var = "fac2_label") %>%
        ## Add count summary within FACTOR2 categories
        summarize_row_groups("FACTOR2") %>%
        ## Add analysis/data rows by analyzing AGE variable
        ## Note afun is a function that returns 2 values in a named list
        ## this will create 2 data rows
        analyze("AGE", "Age Analysis", afun = function(x) list(mean = mean(x),
                                                                         median = median(x)),
                          format = "xx.xx") %>%
        ## adding more analysis vars "compounds them", placing them at the same
        ## level of nesting as all previous analysis blocks, rather than
        ## attempting to further nest them
        analyze("AGE", "Age Analysis redux", afun = range, format = "xx.x - xx.x") %>%
        
        ## Note nested=TRUE, this creates a NEW subtable directly under the
        ## root split
        ## afun of table() gives us k count rows, where k is the number of
        ## levels of VAR3, in this case 2.
        analyze("VAR3", "Var3 Counts", afun = lstwrapx(table), nested = FALSE)
    
    
    expnames = c("Caucasian (n)", "Level A", "Age Analysis", "mean", "median",
                 "Age Analysis redux", "range", "Level B", "Age Analysis",
                 "mean", "median", "Age Analysis redux", "range",
                 "African American (n)", "Level A", "Age Analysis", "mean", "median",
                 "Age Analysis redux", "range", "Level B", "Age Analysis",
                 "mean", "median", "Age Analysis redux", "range",
                 "level1", "level2")
    tab = build_table(lyt, rawdat)
    print(tab)
    ## XXX TODO this assumes we want no var label on VAR3 subtable
    expect_identical(dim(tab), c(28L, 4L))
    expect_identical(row.names(tab), expnames)
})





test_that("existing table in layout works", {
    thing2 = NULL %>% split_cols_by("ARM", "Arm") %>%
    ## add nested column split on SEX with value labels from gend_label
    split_cols_by("SEX", "Gender", labels_var = "gend_label") %>%
    analyze(c("AGE", "AGE"), c("Age Analysis", "Age Analysis Redux"), afun = function(x) list(mean = mean(x),
                                                                    median = median(x)), format = "xx.xx")

tab2 = build_table(thing2, rawdat)


    thing3 = NULL %>% split_cols_by("ARM", "Arm") %>%
        ## add nested column split on SEX with value labels from gend_label
        split_cols_by("SEX", "Gender", labels_var = "gend_label") %>%
        split_rows_by("RACE", "Ethnicity", labels_var = "ethn_label") %>%
        summarize_row_groups("RACE", label_fstr = "%s (n)") %>%
        analyze("AGE", "Age Analysis",
                          afun = function(x) list(mean = mean(x), median = median(x)),
                          format = "xx.xx") %>%
        ## stack an existing table onto the layout and thus the generated table
        add_existing_table(tab2)

    tab3 = build_table(thing3, rawdat)
    expect_equal(nrow(tab3), 12)
    tab3
})

## It currently sometimes
test_that("labelkids parameter works", {
    yeslabellyt <- NULL %>% split_cols_by("ARM", "Arm") %>%
        split_cols_by("SEX", "Gender", labels_var = "gend_label") %>%
        split_rows_by("RACE", "Ethnicity", labels_var = "ethn_label", child_labels = "visible") %>%
        summarize_row_groups("RACE", label_fstr = "%s (n)") %>%
        split_rows_by("FACTOR2", "Factor2",
                            split_fun = remove_split_levels("C"),
                            labels_var = "fac2_label", child_labels = "visible") %>%
        analyze("AGE", "Age Analysis", afun = function(x) list(mean = mean(x),
                                                                         median = median(x)),
                          format = "xx.xx")

    tabyes <- build_table(yeslabellyt, rawdat)

    expect_identical(row.names(tabyes)[1:4],
                     c("Caucasian", "Caucasian (n)", "Level A", "Age Analysis"))

    
    misslabellyt <- NULL %>%
        split_cols_by("ARM", "Arm") %>%
        split_cols_by("SEX", "Gender", labels_var = "gend_label") %>%
        split_rows_by("RACE", "Ethnicity", labels_var = "ethn_label", child_labels = "default") %>%
        summarize_row_groups("RACE", label_fstr = "%s (n)") %>%
        split_rows_by("FACTOR2", "Factor2",
                            split_fun = remove_split_levels("C"),
                            labels_var = "fac2_label", child_labels = "default") %>%
        analyze("AGE", "Age Analysis", afun = function(x) list(mean = mean(x),
                                                                         median = median(x)),
                      format = "xx.xx") 

    tabmiss <- build_table(misslabellyt, rawdat)
    

    nolabellyt <- NULL %>%
        split_cols_by("ARM", "Arm") %>%
        split_cols_by("SEX", "Gender", labels_var = "gend_label") %>%
        split_rows_by("RACE", "Ethnicity", labels_var = "ethn_label", child_labels = "hidden") %>%
        summarize_row_groups("RACE", label_fstr = "%s (n)") %>%
        split_rows_by("FACTOR2", "Factor2",
                            split_fun = remove_split_levels("C"),
                            labels_var = "fac2_label", child_labels = "hidden") %>%
        analyze("AGE", "Age Analysis", afun = function(x) list(mean = mean(x),
                                                                         median = median(x)),
                          format = "xx.xx")


    tabno <- build_table(nolabellyt, rawdat)
    


})



test_that("ref_group comparisons work", {
    
    blthing = NULL %>% split_cols_by("ARM", ref_group = "ARM1") %>%
        analyze("AGE", mean, var_labels = "") %>%
        analyze_against_ref_group(var = "AGE",
                                  mean)
    ## function(x) list(mean = mean(x)))
    
    
    bltab = build_table(blthing, rawdat)
    print(bltab)
    expect_identical(dim(bltab), c(2L,2L))
    expect_null(bltab[2,1, drop = TRUE])
    c1 = bltab[1,1, drop = TRUE]
    c2 = bltab[1,2, drop = TRUE]
    c3 = bltab[2,2, drop = TRUE]
    expect_equivalent(c2 - c1, c3)
})

test_that("missing vars caught", {
    misscol = NULL %>% split_cols_by("ARM", "Arm") %>%
    split_cols_by("SX", "Gender") %>%
    analyze("AGE", "Age Analysis", afun = function(x) list(mean = mean(x),
                                                                    median = median(x)), format = "xx.xx")

    expect_error(build_table(misscol, rawdat))

    missrsplit =  NULL %>% split_cols_by("ARM", "Arm") %>%
    split_cols_by("SEX", "Gender") %>%
    split_rows_by("RACER", "ethn") %>%
    analyze("AGE", "Age Analysis", afun = function(x) list(mean = mean(x),
                                                                    median = median(x)), format = "xx.xx")

    expect_error(build_table(missrsplit, rawdat))

    missavar =  NULL %>% split_cols_by("ARM", "Arm") %>%
    split_cols_by("SEX", "Gender") %>%
    split_rows_by("RACE", "ethn") %>%
    analyze("AGGE", "Age Analysis", afun = function(x) list(mean = mean(x),
                                                                    median = median(x)), format = "xx.xx")

    expect_error(build_table(missavar, rawdat))
    })


    
