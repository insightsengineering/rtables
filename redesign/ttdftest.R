library(magrittr)
library(rtables)
library(tern)
options(error=recover)


races = c("Caucasian" = "WHITE", "African American" = "BLACK", "Latino" = "LATINO", "Eastern Asian" = "EASTASIAN", "Southern Asian" = "SOUTHASIAN", "Indian" = "INDIAN")
makefakedat = function(n  = 1000) {
    datadf = data.frame(stringsAsFactors = FALSE,
                        ARM = c("ARM1", sample(c("ARM1", "ARM2"), n - 1, replace = TRUE)),
                        SEX = c("M", sample(c("M", "F", "U"), n - 1, replace = TRUE, prob = c(.495, .495, .01))),
                        FACTOR2 = c("A", sample(c("A", "B", "C"), n - 1, replace = TRUE)),
                        RACE = c("WHITE", sample(races, n - 1, replace = TRUE)),
                        AGE = runif(n, 40, 70),
                        VAR3 = c("level1", sample(c("level1", "level2"), n -1,
                                                  replace = TRUE)))
                        
    datadf$ethn_lbl = names(races)[match(datadf$RACE, races)]
    datadf$fac2_lbl = paste("Level", datadf$FACTOR2)
    datadf$gend_lbl = c(M="Male", F="Female", U="Undetermined")[datadf$SEX]
    datadf
}

bigdat = makefakedat(10000)

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
lyt = NULL %>% split_cols_by("ARM", "Arm") %>%
    ## add nested column split on SEX with value lables from gend_lbl
    split_cols_by("SEX", "Gender", vlblvar = "gend_lbl") %>%
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



tab = build_table(lyt, rawdat)

bigtab = build_table(lyt, bigdat)
bigae

library(random.cdisc.data)

bigaelookup <-  tribble(
      ~AEBODSYS, ~AELLT,          ~AEDECOD,        ~AEHLT,        ~AEHLGT,      ~AETOXGR, ~AESOC, ~AESER, ~AREL,
      "cl A.1",  "llt A.1.1.1.1", "dcd A.1.1.1.1", "hlt A.1.1.1", "hlgt A.1.1", "1",        "cl A", "N",    "N",
      "cl A.1",  "llt A.1.1.1.2", "dcd A.1.1.1.2", "hlt A.1.1.1", "hlgt A.1.1", "2",        "cl A", "Y",    "N",
      "cl A.1",  "llt A.1.1.1.1", "dcd A.1.1.1.1", "hlt A.1.1.1", "hlgt A.1.1", "5",        "cl B", "N",    "Y",
      "cl A.2",  "llt A.2.1.2.1", "dcd A.2.1.2.1", "hlt A.2.1.2", "hlgt A.2.1", "3",        "cl B", "N",    "N",
      "cl A.2",  "llt A.2.2.3.1", "dcd A.2.2.3.1", "hlt A.2.2.3", "hlgt A.2.2", "1",        "cl B", "Y",    "N",
      "cl A.1",  "llt A.1.1.1.3", "dcd A.1.1.1.3", "hlt A.1.1.1", "hlgt A.1.1", "4",        "cl C", "N",    "Y",
      "cl A.2",  "llt A.2.1.2.1", "dcd A.2.1.2.1", "hlt A.2.1.2", "hlgt A.2.1", "2",        "cl C", "N",    "Y",
      "cl A.2",  "llt A.2.1.1.1", "dcd A.2.1.1.1", "hlt A.2.1.1", "hlgt A.2.1", "5",        "cl D", "Y",    "N",
      "cl A.2",  "llt A.2.1.4.2", "dcd A.2.1.4.2", "hlt A.2.1.4", "hlgt A.2.1", "3",        "cl D", "N",    "N",
      "cl A.2",  "llt A.2.1.5.3", "dcd A.2.1.5.3", "hlt A.2.1.5", "hlgt A.2.1", "1",        "cl D", "N",    "Y",

          "cl B.1",  "llt B.1.1.1.1", "dcd B.1.1.1.1", "hlt B.1.1.1", "hlgt B.1.1", "1",        "cl A", "N",    "N",
      "cl B.1",  "llt B.1.1.1.2", "dcd B.1.1.1.2", "hlt B.1.1.1", "hlgt B.1.1", "2",        "cl A", "Y",    "N",
      "cl B.1",  "llt B.1.1.1.1", "dcd B.1.1.1.1", "hlt B.1.1.1", "hlgt B.1.1", "5",        "cl B", "N",    "Y",
      "cl B.2",  "llt B.2.1.2.1", "dcd B.2.1.2.1", "hlt B.2.1.2", "hlgt B.2.1", "3",        "cl B", "N",    "N",
      "cl B.2",  "llt B.2.2.3.1", "dcd B.2.2.3.1", "hlt B.2.2.3", "hlgt B.2.2", "1",        "cl B", "Y",    "N",
      "cl B.1",  "llt B.1.1.1.3", "dcd B.1.1.1.3", "hlt B.1.1.1", "hlgt B.1.1", "4",        "cl C", "N",    "Y",
      "cl B.2",  "llt B.2.1.2.1", "dcd B.2.1.2.1", "hlt B.2.1.2", "hlgt B.2.1", "2",        "cl C", "N",    "Y",
      "cl B.2",  "llt B.2.1.1.1", "dcd B.2.1.1.1", "hlt B.2.1.1", "hlgt B.2.1", "5",        "cl D", "Y",    "N",
      "cl B.2",  "llt B.2.1.4.2", "dcd B.2.1.4.2", "hlt B.2.1.4", "hlgt B.2.1", "3",        "cl D", "N",    "N",
      "cl B.2",  "llt B.2.1.5.3", "dcd B.2.1.5.3", "hlt B.2.1.5", "hlgt B.2.1", "1",        "cl D", "N",    "Y",

          "cl C.1",  "llt C.1.1.1.1", "dcd C.1.1.1.1", "hlt C.1.1.1", "hlgt C.1.1", "1",        "cl A", "N",    "N",
      "cl C.1",  "llt C.1.1.1.2", "dcd C.1.1.1.2", "hlt C.1.1.1", "hlgt C.1.1", "2",        "cl A", "Y",    "N",
      "cl C.1",  "llt C.1.1.1.1", "dcd C.1.1.1.1", "hlt C.1.1.1", "hlgt C.1.1", "5",        "cl B", "N",    "Y",
      "cl C.2",  "llt C.2.1.2.1", "dcd C.2.1.2.1", "hlt C.2.1.2", "hlgt C.2.1", "3",        "cl B", "N",    "N",
      "cl C.2",  "llt C.2.2.3.1", "dcd C.2.2.3.1", "hlt C.2.2.3", "hlgt C.2.2", "1",        "cl B", "Y",    "N",
      "cl C.1",  "llt C.1.1.1.3", "dcd C.1.1.1.3", "hlt C.1.1.1", "hlgt C.1.1", "4",        "cl C", "N",    "Y",
      "cl C.2",  "llt C.2.1.2.1", "dcd C.2.1.2.1", "hlt C.2.1.2", "hlgt C.2.1", "2",        "cl C", "N",    "Y",
      "cl C.2",  "llt C.2.1.1.1", "dcd C.2.1.1.1", "hlt C.2.1.1", "hlgt C.2.1", "5",        "cl D", "Y",    "N",
      "cl C.2",  "llt C.2.1.4.2", "dcd C.2.1.4.2", "hlt C.2.1.4", "hlgt C.2.1", "3",        "cl D", "N",    "N",
      "cl C.2",  "llt C.2.1.5.3", "dcd C.2.1.5.3", "hlt C.2.1.5", "hlgt C.2.1", "1",        "cl D", "N",    "Y",

          "cl D.1",  "llt D.1.1.1.1", "dcd D.1.1.1.1", "hlt D.1.1.1", "hlgt D.1.1", "1",        "cl A", "N",    "N",
      "cl D.1",  "llt D.1.1.1.2", "dcd D.1.1.1.2", "hlt D.1.1.1", "hlgt D.1.1", "2",        "cl A", "Y",    "N",
      "cl D.1",  "llt D.1.1.1.1", "dcd D.1.1.1.1", "hlt D.1.1.1", "hlgt D.1.1", "5",        "cl B", "N",    "Y",
      "cl D.2",  "llt D.2.1.2.1", "dcd D.2.1.2.1", "hlt D.2.1.2", "hlgt D.2.1", "3",        "cl B", "N",    "N",
      "cl D.2",  "llt D.2.2.3.1", "dcd D.2.2.3.1", "hlt D.2.2.3", "hlgt D.2.2", "1",        "cl B", "Y",    "N",
      "cl D.1",  "llt D.1.1.1.3", "dcd D.1.1.1.3", "hlt D.1.1.1", "hlgt D.1.1", "4",        "cl C", "N",    "Y",
      "cl D.2",  "llt D.2.1.2.1", "dcd D.2.1.2.1", "hlt D.2.1.2", "hlgt D.2.1", "2",        "cl C", "N",    "Y",
      "cl D.2",  "llt D.2.1.1.1", "dcd D.2.1.1.1", "hlt D.2.1.1", "hlgt D.2.1", "5",        "cl D", "Y",    "N",
      "cl D.2",  "llt D.2.1.4.2", "dcd D.2.1.4.2", "hlt D.2.1.4", "hlgt D.2.1", "3",        "cl D", "N",    "N",
      "cl D.2",  "llt D.2.1.5.3", "dcd D.2.1.5.3", "hlt D.2.1.5", "hlgt D.2.1", "1",        "cl D", "N",    "Y",
)

bigadsl = radsl(N=20000, study_duration = 4, narms = 10)
bigadae = radae(bigadsl,lookup = bigaelookup, max_n_aes=  20L)




s_events_patients <- function(x, .N_col, lblstr = NULL) {
  in_rows(
    "Total number of patients with at least one event" = 
      rcell(length(unique(x)) * c(1, 1/.N_col), format = "xx (xx.xx%)"),
    
    "Total number of events" = rcell(length(x), format = "xx")
  )
}

table_count_once_per_id <- function(df, termvar = "AEDECOD", idvar = "USUBJID", na.rm = TRUE) {

  x <- df[[termvar]]
  id <- df[[idvar]]
  
  if (na.rm) x <- na.omit(x)
 
  as.list(table(x[!duplicated(id)]))
}


N_per_arm <- table(bigadsl$ARM)

biglyt <- basic_table() %>% 
    split_cols_by("ARM") %>%
  add_colcounts() %>%
    analyze("USUBJID", afun = s_events_patients) %>% 
  split_rows_by("AEBODSYS", child_labels = "visible", nested = FALSE)  %>%
    summarize_row_groups("USUBJID", cfun = s_events_patients) %>%
    analyze("AEDECOD", table_count_once_per_id, show_labels = "hidden", indent_mod = -1)


Rprof("~/bigtable.Rprof")
bigtbl <- build_table(biglyt, bigadae, col_counts = N_per_arm)
Rprof(NULL)











## generate a little table that we want to add onto another table
## that we're going to build
thing2 = NULL %>% split_cols_by("ARM", split_label = "Arm") %>%
    ## add nested column split on SEX with value lables from gend_lbl
    split_cols_by("SEX", "Gender", labels_var = "gend_lbl") %>%
    analyze(c("AGE", "AGE"), var_labels = c("Age Analysis", "Age Analysis Redux"),
            afun = function(x) {
        in_rows(mean = rcell(mean(x), format = "xx.xx"),
                median = rcell(median(x), format = "xx.xx"))
        })

tab2 = build_table(thing2, rawdat)


thing3 = NULL %>% split_cols_by("ARM", "Arm") %>%
    ## add nested column split on SEX with value lables from gend_lbl
    split_cols_by("SEX", "Gender", vlblvar = "gend_lbl") %>%
    add_rowby_varlevels("RACE", "Ethnicity", vlblvar = "ethn_lbl") %>%
    add_summary_count("RACE", lbl_fstr = "%s (n)") %>%
    add_analyzed_vars("AGE", "Age Analysis", afun = function(x) list(mean = mean(x),
                                                                    median = median(x)), fmt = "xx.xx") %>%

    ## stack an existing table onto the layout and thus the generated table
    add_existing_table(tab2)


tab3 = build_table(thing3, rawdat)




### baseline stuff?????

blthing = NULL %>% add_colby_varwbline("ARM", "ARM1") %>%
    add_analyzed_vars("AGE", lbl = "",## lbl = "Age v2",
                     afun = mean) %>%
    add_analyzed_blinecomp(var = "AGE",
                           afun = mean)
## function(x) list(mean = mean(x)))


bltab = build_table(blthing, rawdat)





longdat = makefakedat2()
## 'comparison' where different variables are displayed sidebyside
simplecomp = NULL %>% split_cols_by("ARM", "Arm") %>%
    split_cols_by("VISIT", "Visit") %>%
    add_colby_multivar(c("VALUE", "PCTDIFF"), "dummylab", varlbls = c("Raw", "Pct Diff")) %>%
    add_rowby_varlevels("RACE", "Ethnicity", vlblvar = "ethn_lbl") %>%
    add_summary_count("RACE", "%s (n)") %>%
    add_rowby_varlevels("SEX", "Gender", vlblvar="gend_lbl") %>%
    add_summary_count("SEX", "%s (n)") %>%
    add_analyzed_colvars("Mean", afun = function(x) if(all(is.na(x))) rcell(NULL) else rcell(mean(x, na.rm = TRUE), lbl="mean"), fmt= "xx.xx")

tab3 = build_table(simplecomp, longdat)





### reasonable errors

misscol = NULL %>% split_cols_by("ARM", "Arm") %>%
    split_cols_by("SX", "Gender") %>%
    add_analyzed_vars("AGE", "Age Analysis", afun = function(x) list(mean = mean(x),
                                                                    median = median(x)), fmt = "xx.xx")

build_table(misscol, rawdat)


missrsplit =  NULL %>% split_cols_by("ARM", "Arm") %>%
    split_cols_by("SEX", "Gender") %>%
    add_rowby_varlevels("RACER", "ethn") %>%
    add_analyzed_vars("AGE", "Age Analysis", afun = function(x) list(mean = mean(x),
                                                                    median = median(x)), fmt = "xx.xx")

build_table(missrsplit, rawdat)

missavar =  NULL %>% split_cols_by("ARM", "Arm") %>%
    split_cols_by("SEX", "Gender") %>%
    add_rowby_varlevels("RACE", "ethn") %>%
    add_analyzed_vars("AGGE", "Age Analysis", afun = function(x) list(mean = mean(x),
                                                                    median = median(x)), fmt = "xx.xx")

build_table(missavar, rawdat)


## what should it look like



complyt = NULL %>% split_cols_by("ARM", "Arm") %>%
    add_colby_varwbline(var = "visit",lbl = "Visit", baseline = "baseline",
                        incl_all = TRUE) %>%
    add_rowby_varlevels("RACE", "Ethnicity", vlblvar = "ethn_lbl") %>%
    add_summary_count("RACE", lbl_fstr = "%s (n)") %>%
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
 






l <- NULL %>% split_cols_by("ARM") %>%
    add_colcounts() %>%
    add_rowby_varlevels("RACE", "Ethnicity") %>%
    add_analyzed_vars("AGE", afun = range , fmt = "xx.xx - xx.xx")
l

build_table(l, DM)
