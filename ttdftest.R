library(magrittr)
library(rtables)

df = read.csv("tabledfex.dat", stringsAsFactors = FALSE)
dfrow = df[4,]
dfrow_to_clayout(dfrow)
res = df_to_tt(df)

identical(df, tt_to_df(res))
#rws = recursive_row_collect(res)



thing = NULL %>% add_colby_varlevels("ARM", "Arm") %>%
    add_colby_varlevels("SEX", "Gender") %>%
    add_rowby_varlevels("RACE", "Ethnicity") %>%
    add_rowby_varlevels("FACTOR2", "Factor2") %>%
    add_new_rowtree(AllSplit())


makefakedat = function(n  = 1000) {

    datadf = data.frame(stringsAsFactors = FALSE,
                        ARM = c("ARM 1", sample(c("ARM 1", "ARM 2"), n -1, replace = TRUE)),
                        SEX = c("M", sample(c("M", "F"), n, replace = TRUE)),
                        Factor2 = c("A", sample(c("A", "B"), n, replace = TRUE)),
                        RACE = c("WHITE", sample(c("WHITE", "BLACK"), n - 1, replace = TRUE),
                        AGE = runif(n, 40, 70))
    datadf
}
                    
                    











## fix column order so each column type are in blocks
## newcolord = c("var",
##   "varlbl",
##   "valtype",
##   "rowlbl",
##   "r1value",
##   "r2value",
##   "r1vlbl", 
##   "r2vlbl",
##   "ARM1___M",
##   "ARM1___F",
##   "ARM2___M",
##   "ARM2___F", 
##   "rsp_1",
##   "rsp_2",
##   "rsplbl_1",
##   "rsplbl_2",
##   "csp_1",
##   "csp_2",
##   "csplbl_1", 
##   "csplbl_2")


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
