library(magrittr)
library(rtables)
library(tern)
options(error=recover)

df = read.csv("tabledfex.dat", stringsAsFactors = FALSE)

res = df_to_tt(df)
dfredux = tt_to_df(res)

identical(fixup_rtable_df(df), tt_to_df(res))
## #rws = recursive_row_collect(res)





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
thing = NULL %>% add_colby_varlevels("ARM", "Arm") %>%
    ## add nested column split on SEX with value lables from gend_lbl
    add_colby_varlevels("SEX", "Gender", valuelblvar = "gend_lbl") %>%
    ## No row splits have been introduced, so this adds
    ## a root split and puts summary content on it labelled Overall (N)
    ## add_colby_total(lbl = "All") %>%
    add_summary_count(lbl = "Overall (N)", valfmt = "(N=xx)") %>%
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
    add_analyzed_var("AGE", "Age Analysis", afun = function(x) list(mean = mean(x),
                                                                    median = median(x)),
                     fmt = "xx.xx") %>%
    ## Note newtoplev=TRUE, this creates a NEW subtable directly under the
    ## root split
    ## afun of table() gives us k count rows, where k is the number of
    ## levels of VAR3, in this case 2.
    add_analyzed_var("VAR3", "Var3 Counts", afun = table, newtoplev = TRUE)



tab = build_table(thing, rawdat)

to_s3compat(tab)

## generate a little table that we want to add onto another table
## that we're going to build
thing2 = NULL %>% add_colby_varlevels("ARM", "Arm") %>%
    ## add nested column split on SEX with value lables from gend_lbl
    add_colby_varlevels("SEX", "Gender", valuelblvar = "gend_lbl") %>%
    add_analyzed_var("AGE", "Age Analysis", afun = function(x) list(mean = mean(x),
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


nms = c("rownum", "rowvar", "rowvarlbl", "valtype", "rowlbl", "r1value", "r2value", 
"r1vlbl", "r2vlbl", "ARM1___M", "ARM1___F", "ARM2___M", "ARM2___F", 
"rsp_1", "rsp_2", "rsplbl_1", "rsplbl_2", "csp_1", "csp_2", "csplbl_1", 
"csplbl_2", "csptype_1", "csptype_2", "rsptype_1", "rsptype_2")

df2 = df[,c("X",
           "rowvar",
           "rowvarlbl",
           "valtype",
           "rowlbl",
           "r1value",
           "r2value",
           "r1vlbl",
           "r2vlbl",
           "ARM1___M",
           "ARM1___F",
           "ARM2___M",
           "ARM2___F", 
           "rsp_1",
           "rsp_2",
           "rsptype_1",
           "rsptype_2",
           "rsplbl_1",
           "rsplbl_2",
           "csp_1",
           "csp_2",
           "csplbl_1", 
           "csplbl_2",
           "csptype_1",
           "csptype_2")]



blfun = function(df) {
    nonbase = unique(subset(df, cat != "base")$cat)
    
    ret = rep(list(df[df$cat == "base",]), length(unique(df[df$cat != "base", "cat"])) + 1)
    names(ret) = c("all", nonbase)
    ret
}

csetfun = function(df)
{
    nonbase = unique(subset(df, cat != "base")$cat)
    c(all = list(df), sapply(nonbase, function(i) df[df$cat == i,], simplify=FALSE))
}


compspl = ComparisonSplit(blfun, csetfun)


catvar = sample(c("base", "set1", "set2"), 100, replace = TRUE)
x = rnorm(100, as.integer(factor(catvar))*5, 10)
compdf = data.frame(val = x, cat = catvar, stringsAsFactors = FALSE)

do_split(compspl, compdf)

## what should it look like







complyt = NULL %>% add_colby_varlevels("ARM", "Arm") %>%
    add_colby_blinecomp(var = "visit", baseline = "baseline",
                        all = TRUE,
                        valuecomp = `-`) %>%
    add_rowby_varlevels("RACE", "Ethnicity", vlblvar = "ethn_lbl") %>%
    add_summary_count("RACE", lblfmt = "%s (n)") %>%
    add_analyzed_var("weight", mean, fmt = "xx.xx")
    



## 

 library(random.cdisc.data)
 ADSL <- radsl(cached = TRUE)

 ADRS <- radrs(cached = TRUE)
 ADRS_f <- subset(ADRS, PARAMCD == "BESRSPI")

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
     ADRS_f$AVALC, levels = c("CR", "PR", "SD", "NON CR/PD", "PD", "NE")
   )))
