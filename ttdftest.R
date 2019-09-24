library(magrittr)
library(rtables)
options(error=recover)

df = read.csv("tabledfex.dat", stringsAsFactors = FALSE)

res = df_to_tt(df)
dfredux = tt_to_df(res)

identical(fixup_rtable_df(df), tt_to_df(res))
## #rws = recursive_row_collect(res)


## makenfunc = function(var) function(df) sum(!is.na(df[[var]]))

## starting will NULL causes it to construct a PreDataLayouts object
## add top level column split on ARM
thing = NULL %>% add_colby_varlevels("ARM", "Arm") %>%
    ## add nested column split on SEX with value lables from gend_lbl
    add_colby_varlevels("SEX", "Gender", valuelblvar = "gend_lbl") %>%
    ## No row splits have been introduced, so this adds
    ## a root split and puts summary content on it labelled Overall (N)
    add_summary_count(lbl = "Overall (N)") %>%
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
                                                           median = median(x))) %>%
    ## Note newtoplev=TRUE, this creates a NEW subtable directly under the
    ## root split
    ## afun of table() gives us k count rows, where k is the number of
    ## levels of VAR3, in this case 2.
    add_analyzed_var("VAR3", "Var3 Counts", afun = table, newtoplev = TRUE)


makefakedat = function(n  = 1000) {
    datadf = data.frame(stringsAsFactors = FALSE,
                        ARM = c("ARM1", sample(c("ARM1", "ARM2"), n -1, replace = TRUE)),
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

rawdat = makefakedat()
                    

ctree = splitvec_to_coltree(rawdat, clayout(thing)[[1]], TreePos())

cexprs = build_splits_expr(clayout(thing)[[1]], rawdat)

## stuff = recursive_applysplit(rawdat, splvec = rlayout(thing)[[1]],
##                              treepos = TreePos(),
##                              colexprs = cexprs,
##                              coltree = ctree)


tab = build_table(thing, rawdat)

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
