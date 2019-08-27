library(magrittr)
library(rtables)

## df = read.csv("tabledfex.dat", stringsAsFactors = FALSE)
## dfrow = df[4,]
## dfrow_to_clayout(dfrow)
## res = df_to_tt(df)

## identical(df, tt_to_df(res))
## #rws = recursive_row_collect(res)


## makenfunc = function(var) function(df) sum(!is.na(df[[var]]))

thing = NULL %>% add_colby_varlevels("ARM", "Arm") %>%
    add_colby_varlevels("SEX", "Gender", valuelblvar = "gend_lbl") %>%
    add_summary_count(lbl = "Overall (N)") %>%
    add_rowby_varlevels("RACE", "Ethnicity", vlblvar = "ethn_lbl") %>%
#    add_summary_count("RACE", lblfmt = "Ethnicity (n)") %>% 
    add_rowby_varlevels("FACTOR2", "Factor2",
                        splfun = excl_levs_sfun("C"),
                        vlblvar = "fac2_lbl") %>%
    add_summary_count("FACTOR2") %>%
    add_analyzed_var("AGE", "Age", afun = function(x) list(mean = mean(x),
                                                           median = median(x))) %>%
    add_summary_count("Age") %>%
    add_analyzed_var("VAR3", "Var3", afun = table, newtoplev = TRUE)


makefakedat = function(n  = 1000) {

    datadf = data.frame(stringsAsFactors = FALSE,
                        ARM = c("ARM 1", sample(c("ARM 1", "ARM 2"), n -1, replace = TRUE)),
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

stuff = recursive_applysplit(rawdat, splvec = rlayout(thing)[[1]],
                             treepos = TreePos(),
                             colexprs = cexprs,
                             coltree = ctree)


tab = build_table(thing, rawdat)


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
