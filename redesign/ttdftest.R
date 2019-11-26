library(magrittr)
library(rtables)
library(tern)
options(error=recover)



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
   
rawdat = makefakedat()








## makenfunc = function(var) function(df) sum(!is.na(df[[var]]))

## starting will NULL causes it to construct a PreDataLayouts object
## add top level column split on ARM
lyt = NULL %>% add_colby_varlevels("ARM", "Arm") %>%
    ## add nested column split on SEX with value lables from gend_lbl
    add_colby_varlevels("SEX", "Gender", valuelblvar = "gend_lbl") %>%
    ## No row splits have been introduced, so this adds
    ## a root split and puts summary content on it labelled Overall (N)
    ## add_colby_total(lbl = "All") %>%
    ##    add_summary_count(lbl = "Overall (N)", valfmt = "(N=xx)") %>%
    add_colcounts() %>%
    ## add a new subtable that splits on RACE, value labels from ethn_lbl
    add_rowby_varlevels("RACE", "Ethnicity", vlblvar = "ethn_lbl") %>%
    add_summary_count("RACE", lblfmt = "%s (n)") %>%
    ##
    ## Add nested row split within Race categories for FACTOR2
    ## using a split function that excludes level C
    ## value labels from fac2_lbl
    add_rowby_varlevels("FACTOR2", "Factor2",
                        splfun = excl_levs_sfun("C"),
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
    add_analyzed_var("VAR3", "Var3 Counts", afun = table, newtoplev = TRUE)



tab = build_table(lyt, rawdat)



## generate a little table that we want to add onto another table
## that we're going to build
thing2 = NULL %>% add_colby_varlevels("ARM", "Arm") %>%
    ## add nested column split on SEX with value lables from gend_lbl
    add_colby_varlevels("SEX", "Gender", valuelblvar = "gend_lbl") %>%
    add_analyzed_vars(c("AGE", "AGE"), c("Age Analysis", "Age Analysis Redux"), afun = function(x) list(mean = mean(x),
                                                                    median = median(x)), fmt = "xx.xx")

tab2 = build_table(thing2, rawdat)


thing3 = NULL %>% add_colby_varlevels("ARM", "Arm") %>%
    ## add nested column split on SEX with value lables from gend_lbl
    add_colby_varlevels("SEX", "Gender", valuelblvar = "gend_lbl") %>%
    add_rowby_varlevels("RACE", "Ethnicity", vlblvar = "ethn_lbl") %>%
    add_summary_count("RACE", lblfmt = "%s (n)") %>%
    add_analyzed_var("AGE", "Age Analysis", afun = function(x) list(mean = mean(x),
                                                                    median = median(x)), fmt = "xx.xx") %>%

    ## stack an existing table onto the layout and thus the generated table
    add_existing_table(tab2)


tab3 = build_table(thing3, rawdat)




### baseline stuff?????

blthing = NULL %>% add_colby_varwbline("ARM", "ARM1", lbl = "Arm") %>%
    add_analyzed_blinecomp(var = "AGE", lbl = "Age",
                           afun = mean) %>%
    add_analyzed_var("AGE", lbl = "Age v2",
                     afun = mean,
                     newtoplev = TRUE)
## function(x) list(mean = mean(x)))


bltab = build_table(blthing, rawdat)





longdat = makefakedat2()
## 'comparison' where different variables are displayed sidebyside
simplecomp = NULL %>% add_colby_varlevels("ARM", "Arm") %>%
    add_colby_varlevels("VISIT", "Visit") %>%
    add_colby_multivar(c("VALUE", "PCTDIFF"), "dummylab", varlbls = c("Raw", "Pct Diff")) %>%
    add_rowby_varlevels("RACE", "Ethnicity", vlblvar = "ethn_lbl") %>%
    add_summary_count("RACE", "%s (n)") %>%
    add_rowby_varlevels("SEX", "Gender", vlblvar="gend_lbl") %>%
    add_summary_count("SEX", "%s (n)") %>%
    add_analyzed_colvars("Mean", afun = function(x) list(mean = mean(x, na.rm = TRUE)), fmt= "xx.xx")

tab3 = build_table(simplecomp, longdat)





### reasonable errors

misscol = NULL %>% add_colby_varlevels("ARM", "Arm") %>%
    add_colby_varlevels("SX", "Gender") %>%
    add_analyzed_var("AGE", "Age Analysis", afun = function(x) list(mean = mean(x),
                                                                    median = median(x)), fmt = "xx.xx")

build_table(misscol, rawdat)


missrsplit =  NULL %>% add_colby_varlevels("ARM", "Arm") %>%
    add_colby_varlevels("SEX", "Gender") %>%
    add_rowby_varlevels("RACER", "ethn") %>%
    add_analyzed_var("AGE", "Age Analysis", afun = function(x) list(mean = mean(x),
                                                                    median = median(x)), fmt = "xx.xx")

build_table(missrsplit, rawdat)

missavar =  NULL %>% add_colby_varlevels("ARM", "Arm") %>%
    add_colby_varlevels("SEX", "Gender") %>%
    add_rowby_varlevels("RACE", "ethn") %>%
    add_analyzed_var("AGGE", "Age Analysis", afun = function(x) list(mean = mean(x),
                                                                    median = median(x)), fmt = "xx.xx")

build_table(missavar, rawdat)


## what should it look like



complyt = NULL %>% add_colby_varlevels("ARM", "Arm") %>%
    add_colby_varwbline(var = "visit",lbl = "Visit", baseline = "baseline",
                        incl_all = TRUE) %>%
    add_rowby_varlevels("RACE", "Ethnicity", vlblvar = "ethn_lbl") %>%
    add_summary_count("RACE", lblfmt = "%s (n)") %>%
    add_analyzed_blinecomp("weight", lbl = "weight", afun = mean, fmt = "xx.xx")
    



## 

 library(random.cdisc.data)
 ADSL <- radsl(cached = TRUE)

 ADRS <- radrs(cached = TRUE)
ADRS_f <- subset(ADRS, PARAMCD == "BESRSPI")

ADRS_f$rsp = ADRS_f$AVALC %in% c("CR", "PR")

 # Example 1 - ARM B as reference
 #    "NON CR/PD" response category dropped from partition section since no observations
 #     model with no stratifiaction factors, Chi-square test is performed
 tbl <- t_rsp(
   rsp = ADRS_f$AVALC %in% c("CR", "PR"),
   col_by = relevel(factor(ADRS_f$ARMCD), "ARM B"),
   partition_rsp_by = droplevels(factor(
     ADRS_f$AVALC, levels = c("CR", "PR", "SD", "NON CR/PD", "PD", "NE")
   ))
 )




 rsptab <- t_rsp(
   rsp = ADRS_f$AVALC %in% c("CR", "PR"),
   col_by = relevel(factor(ADRS_f$ARMCD), "ARM B"),
   partition_rsp_by = droplevels(factor(
       ADRS_f$AVALC, levels = c("CR", "PR", "SD", "NON CR/PD", "PD", "NE")))
##   strata_data = ADRS_f[c("RACE")]
   )


rsplyt = tt_rsp_lyt("ARMCD", "ARM B")
rsptab2  = build_table(rsplyt, ADRS_f)
to_s3compat(rsptab2)




### manual construction and manipulation

rows = lapply(1:5, function(i) {
    DataRow(rep(i, times  = 3))})
mtab = TableTree(kids = rows, cinfo = manual_cols(split = c("a", "b", "c")))
mtab

### access/replacement
mtab2 = mtab
mtab2[3:5, 2:3] = c(7, 8)
mtab2


## note we're using tab here, so need to make it above
nesttab = tab
do_recursive_replace(nesttab, list("Ethnicity", "WHITE", "Factor2", "A", "AGE"), rows = 1:2, cols = 1, value = 5)


do_recursive_replace(nesttab, list(1, "WHITE"), incontent = TRUE, cols = 3:4, value = list(c(10, 2), c(20, 7)))


## cols by absolute position
subset_cols(tab, -3)

## rows by absolute row number
subset_by_rownum(tab, -(4:8))




## compatibility layer

mtbl <- tt_rtable(
    header = tt_rheader(
        tt_rrow(row.name = NULL, tt_rcell("Sepal.Length", colspan = 2),
                tt_rcell("Petal.Length", colspan=2)),
        tt_rrow(NULL, "mean", "median", "mean", "median")
    ),
    tt_rrow(
        row.name = "All Species",
        mean(iris$Sepal.Length), median(iris$Sepal.Length),
        mean(iris$Petal.Length), median(iris$Petal.Length),
        format = "xx.xx"
    )
)
 

