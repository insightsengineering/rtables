df = read.csv("tabledfex.dat", stringsAsFactors = FALSE)
dfrow = df[4,]

library(rtables)
dfrow_to_clayout(dfrow)
res = df_to_tt(df)
res

rws = recursive_row_collect(res)


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
