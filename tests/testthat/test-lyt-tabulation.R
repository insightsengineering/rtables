context("tabulation framework")



makefakedat = function(n  = 1000) {
    datadf = data.frame(stringsAsFactors = FALSE,
                        ARM = c("ARM1", sample(c("ARM1", "ARM2"), n - 1, replace = TRUE)),
                        SEX = c("M", sample(c("M", "F"), n - 1, replace = TRUE)),
                        FACTOR2 = c("A", sample(c("A", "B", "C"), n - 1, replace = TRUE)),
                        RACE = c("WHITE", sample(c("WHITE", "BLACK"), n - 1, replace = TRUE)),
                        AGE = runif(n, 40, 70),
                        VAR3 = c("level1", sample(c("level1", "level2"), n -1,
                                                  replace = TRUE)))
                        
    datadf$ethn_lbl = c(WHITE = "Caucasian", BLACK = "African American")[datadf$RACE]
    datadf$fac2_lbl = paste("Level", datadf$FACTOR2)
    datadf$gend_lbl = c(M="Male", F="Female")[datadf$SEX]
    datadf
}



makefakedat2 =  function(n  = 1000) {
    if(n%%4 != 0) {
        stop("n not multiple of 4")
    }

    many2s = rep(2, n/2)
    datadf = data.frame(stringsAsFactors = FALSE,
                        ARM = rep(c("ARM1", "ARM2"), times = rep(n/2, 2)),
                        SEX = rep(sample(c("M", "F"), n/2, replace = TRUE),
                                  many2s),
                        RACE = rep(sample(c("WHITE", "BLACK"), n/2, replace = TRUE),
                                   times = many2s),
                        PATID = rep(seq(1, n/2), many2s),
                        VISIT = rep(c("BASELINE", "FOLLOWUP"))
                        )                        
    datadf$ethn_lbl = c(WHITE = "Caucasian", BLACK = "African American")[datadf$RACE]
    datadf$gend_lbl = c(M="Male", F="Female")[datadf$SEX]
    mu = 5 + (as.integer(factor(datadf$RACE)) + as.integer(factor(datadf$ARM)) + as.integer(factor(datadf$SEX)))/2
    datadf$VALUE =  ifelse(datadf$VISIT == "BASELINE",
                           5,
                           5 + rnorm(n, mu, 4))
    datadf$PCTDIFF = NA_real_
    seconds = seq(2, n, by =2)
    datadf$PCTDIFF[seq(2, n, by=2)] = 100*(datadf$VALUE[seconds] - datadf$VALUE[seconds-1]) / datadf$VALUE[seconds - 1]
    
    datadf
}
set.seed(0)
rawdat = makefakedat()




## this 
test_that("complex layout works", {
    lyt = NULL %>% add_colby_varlevels("ARM", "Arm") %>%
        ## add nested column split on SEX with value lables from gend_lbl
        add_colby_varlevels("SEX", "Gender", vlblvar = "gend_lbl") %>%
        ## No row splits have been introduced, so this adds
        ## a root split and puts summary content on it labelled Overall (N)
        ## add_colby_total(lbl = "All") %>%
        ##    add_summary_count(lbl = "Overall (N)", fmt = "(N=xx)") %>%
        add_colcounts() %>%
        ## add a new subtable that splits on RACE, value labels from ethn_lbl
        add_rowby_varlevels("RACE", "Ethnicity", vlblvar = "ethn_lbl") %>%
        add_summary_count("RACE", lbl_fstr = "%s (n)") %>%
        ##
        ## Add nested row split within Race categories for FACTOR2
        ## using a split function that excludes level C
        ## value labels from fac2_lbl
        add_rowby_varlevels("FACTOR2", "Factor2",
                            splfun = rtables:::excl_levs_sfun("C"),
                            vlblvar = "fac2_lbl") %>%
        ## Add count summary within FACTOR2 categories
        add_summary_count("FACTOR2") %>%
        ## Add analysis/data rows by analyzing AGE variable
        ## Note afun is a function that returns 2 values in a named list
        ## this will create 2 data rows
        add_analyzed_vars("AGE", "Age Analysis", afun = function(x) list(mean = mean(x),
                                                                         median = median(x)),
                          fmt = "xx.xx") %>%
        ## adding more analysis vars "compounds them", placing them at the same
        ## level of nesting as all previous analysis blocks, rather than
        ## attempting to further nest them
        add_analyzed_vars("AGE", "Age Analysis redux", afun = range, fmt = "xx.x - xx.x") %>%
        
        ## Note newtoplev=TRUE, this creates a NEW subtable directly under the
        ## root split
        ## afun of table() gives us k count rows, where k is the number of
        ## levels of VAR3, in this case 2.
        add_analyzed_vars("VAR3", "Var3 Counts", afun = lstwrapx(table), newtoplev = TRUE)
    
    
    expnames = c("WHITE (n)", "A", "Age Analysis", "mean", "median",
                 "Age Analysis redux", "range", "B", "Age Analysis",
                 "mean", "median", "Age Analysis redux", "range",
                 "BLACK (n)", "A", "Age Analysis", "mean", "median",
                 "Age Analysis redux", "range", "B", "Age Analysis",
                 "mean", "median", "Age Analysis redux", "range",
                 "level1", "level2")
    tab = build_table(lyt, rawdat)
    print(tab)
    ## XXX TODO this assumes we want no var label on VAR3 subtable
    expect_identical(dim(tab), c(28L, 4L))
    expect_identical(row.names(tab), expnames)
})





test_that("existing table in layout works", {
    thing2 = NULL %>% add_colby_varlevels("ARM", "Arm") %>%
    ## add nested column split on SEX with value lables from gend_lbl
    add_colby_varlevels("SEX", "Gender", vlblvar = "gend_lbl") %>%
    add_analyzed_vars(c("AGE", "AGE"), c("Age Analysis", "Age Analysis Redux"), afun = function(x) list(mean = mean(x),
                                                                    median = median(x)), fmt = "xx.xx")

tab2 = build_table(thing2, rawdat)


    thing3 = NULL %>% add_colby_varlevels("ARM", "Arm") %>%
        ## add nested column split on SEX with value lables from gend_lbl
        add_colby_varlevels("SEX", "Gender", vlblvar = "gend_lbl") %>%
        add_rowby_varlevels("RACE", "Ethnicity", vlblvar = "ethn_lbl") %>%
        add_summary_count("RACE", lbl_fstr = "%s (n)") %>%
        add_analyzed_vars("AGE", "Age Analysis",
                          afun = function(x) list(mean = mean(x), median = median(x)),
                          fmt = "xx.xx") %>%
        ## stack an existing table onto the layout and thus the generated table
        add_existing_table(tab2)

    tab3 = build_table(thing3, rawdat)
    tab3
})

## It currently sometimes
test_that("lblkids parameter works", {
    yeslbllyt <- NULL %>% add_colby_varlevels("ARM", "Arm") %>%
        add_colby_varlevels("SEX", "Gender", vlblvar = "gend_lbl") %>%
        add_rowby_varlevels("RACE", "Ethnicity", vlblvar = "ethn_lbl",lblkids = TRUE) %>%
        add_summary_count("RACE", lbl_fstr = "%s (n)") %>%
        add_rowby_varlevels("FACTOR2", "Factor2",
                            splfun = rtables:::excl_levs_sfun("C"),
                            vlblvar = "fac2_lbl", lblkids = TRUE) %>%
        add_analyzed_vars("AGE", "Age Analysis", afun = function(x) list(mean = mean(x),
                                                                         median = median(x)),
                          fmt = "xx.xx")

    tabyes <- build_table(yeslbllyt, rawdat)

    expect_identical(row.names(tabyes)[1:4],
                     c("WHITE", "WHITE (n)", "Level A", "Age Analysis"))

    
    misslbllyt <- NULL %>%
        add_colby_varlevels("ARM", "Arm") %>%
        add_colby_varlevels("SEX", "Gender", vlblvar = "gend_lbl") %>%
        add_rowby_varlevels("RACE", "Ethnicity", vlblvar = "ethn_lbl",lblkids = NA) %>%
        add_summary_count("RACE", lbl_fstr = "%s (n)") %>%
        add_rowby_varlevels("FACTOR2", "Factor2",
                            splfun = rtables:::excl_levs_sfun("C"),
                            vlblvar = "fac2_lbl", lblkids = NA) %>%
        add_analyzed_vars("AGE", "Age Analysis", afun = function(x) list(mean = mean(x),
                                                                         median = median(x)),
                      fmt = "xx.xx") 

    tabmiss <- build_table(misslbllyt, rawdat)
    

    nolbllyt <- NULL %>%
        add_colby_varlevels("ARM", "Arm") %>%
        add_colby_varlevels("SEX", "Gender", vlblvar = "gend_lbl") %>%
        add_rowby_varlevels("RACE", "Ethnicity", vlblvar = "ethn_lbl",lblkids = FALSE) %>%
        add_summary_count("RACE", lbl_fstr = "%s (n)") %>%
        add_rowby_varlevels("FACTOR2", "Factor2",
                            splfun = rtables:::excl_levs_sfun("C"),
                            vlblvar = "fac2_lbl", lblkids = FALSE) %>%
        add_analyzed_vars("AGE", "Age Analysis", afun = function(x) list(mean = mean(x),
                                                                         median = median(x)),
                          fmt = "xx.xx")


    tabno <- build_table(nolbllyt, rawdat)
    


})



test_that("baseline comparisons work", {
    
    blthing = NULL %>% add_colby_varwbline("ARM", "ARM1") %>%
        add_analyzed_vars("AGE", lbl = "",## lbl = "Age v2",
                          afun = mean) %>%
        add_analyzed_blinecomp(var = "AGE",
                               afun = mean)
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
    misscol = NULL %>% add_colby_varlevels("ARM", "Arm") %>%
    add_colby_varlevels("SX", "Gender") %>%
    add_analyzed_vars("AGE", "Age Analysis", afun = function(x) list(mean = mean(x),
                                                                    median = median(x)), fmt = "xx.xx")

    expect_error(build_table(misscol, rawdat))

    missrsplit =  NULL %>% add_colby_varlevels("ARM", "Arm") %>%
    add_colby_varlevels("SEX", "Gender") %>%
    add_rowby_varlevels("RACER", "ethn") %>%
    add_analyzed_vars("AGE", "Age Analysis", afun = function(x) list(mean = mean(x),
                                                                    median = median(x)), fmt = "xx.xx")

    expect_error(build_table(missrsplit, rawdat))

    missavar =  NULL %>% add_colby_varlevels("ARM", "Arm") %>%
    add_colby_varlevels("SEX", "Gender") %>%
    add_rowby_varlevels("RACE", "ethn") %>%
    add_analyzed_vars("AGGE", "Age Analysis", afun = function(x) list(mean = mean(x),
                                                                    median = median(x)), fmt = "xx.xx")

    expect_error(build_table(missavar, rawdat))
    })


    
