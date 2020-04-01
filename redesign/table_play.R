

library(random.cdisc.data)
library(dplyr)
library(rtables)


ADSL <- radsl()



lolz = function(x) {browser(); length(x)}

lyt2 = NULL %>%
    add_colby_qrtiles("AGE") %>%
    add_analyzed_vars("AGE", afun = length, rowlabs = "silly n")

build_table(lyt2, ADSL)


lyt3  = NULL %>%
    add_colby_cmlqrtiles("AGE") %>%
    add_analyzed_vars("AGE", lbl = "", afun = length, rowlabs = "silly n")

build_table(lyt3, ADSL)

rtabulate(ADSL$AGE, by_quartile(ADSL$AGE))

rtabulate(ADSL$AGE, by_quartile(ADSL$AGE), FUN = length)
rtabulate(ADSL$AGE, by_quartile(ADSL$AGE, cumulative = TRUE), FUN = length)



asummary <- function(x) {
  
  if (is.numeric(x)) {
    list(
      "n" = sum(x),
      "Mean (sd)" = c(mean(x), sd(x)),
      "Range" = range(x)
    )
  } else if (is.factor(x)) {
    list(nlevels = nlevels(x))
  } else {
    stop("---")
  }
}


NULL %>% 
  add_colby_varlevels(var = "ARM", lbl = "ARM") %>%
  add_analyzed_vars(c("AGE", "SEX"), afun = asummary)

dm_layout <- NULL %>% 
  add_colby_varlevels(var = "ARM", lbl = "ARM") %>%
  add_analyzed_vars(var = "AGE", afun = asummary) %>%
  add_analyzed_vars(var = "SEX", afun = asummary) 
  
  
  add_analyzed_var(var = "AGE", afun =  function(x) {
    list(
      "n" = sum(x),
      "Mean (sd)" = c(mean(x), sd(x)),
      "Range" = range(x)
    )
  }, fmt = c("xx", "xx.xx (xx.xx)", "xx.xx - xx.xx"))

dm_layout <- NULL %>% 
  add_colby_varlevels(var = "ARM", lbl = "ARM") %>%
  add_colcounts() %>%
  add_analyzed_vars(var = "SEX", afun =  function(x, .N_col) {
    c(list(n = structure(sum(is.na(x)), format = "xx")),
           lapply(table(x), function(xi) xi * c(1, 1/.N_col))
      )
  }, fmt = "xx.xx (xx.xx)")



dm_layout

tbl <- build_table(dm_layout, ADSL)  
tbl











add_analyzed_var(var = c("AGE", "SEX", "BMRKR1"), afun = a_summary)


add_analyzed_var(var = c("AGE", "SEX", "BMRKR1"), afun = list(a_summary, a_summary2, a_summary))


add_analyzed_var(var = c("AGE", "SEX", "BMRKR1"), afun = a_summary)

docat(tbl)
col_info(tbl)

tbl[1, 1]
class(tbl)

class(tree_children(tbl)[[1]])

A <- tree_children(tbl)[[1]]

A[2, 1] <- list(c(5, 1111))

A[2, 1] <- list(structure(lm(1 ~ 1), format = function(x, output) "-"))
A

ElementaryTable(
  kids = TableRow(val = list(1,2,3), name = "AA" )
)

tbl <- ElementaryTable(
  kids = list(
    TableRow(list(1, 2, 3, 4), lab = "Row 1"), 
    TableRow(list(5, 6, 7, 8), lab = "Row 2")
  ),
  cinfo = manual_cols(ARM = c("ARM 1", "ARM 2"), SEX= c("M", "F"))
)

tbl@col_info@tree_layout@children

column_info <- function(x, depth = 0) {
  l <- x@col_info@tree_layout
  
  names(l@children) 
  
  rbind()
} 
cinfo(l)

   name       label          path           N     subset_expression
   ARM 1                     ARM 1      
 *    M                      ARM 1 > M      12          NA
 *    D                                     1           NA
   ARM 2                                
 *    M                                     12          NA
 *    F                                     12          NA

  
  
  
tbl[[, c("ARM 1", "M")]]


clayout(tbl)

cinfo <- function(l, depth = 0) {
  if (class(l) == "LayoutColLeaf") {
    NULL
  } else {
    Reduce(rbind, Map(function(name, child) {
      rbind(
        data.frame(depth = depth, name = name, stringsAsFactors = FALSE),
        child
      )
    }, names(l@children), lapply(l@children, cinfo, depth = depth + 1)))
  }
}

class(l@children[[1]]@children[[1]])

setGeneric("cinfo", function(x) {})

setMethod("cinfo", "LayoutColTree", function(x) {
  
})

setMethod("cinfo", "LayoutColLeaf", function(x) {
  
})
depth   name
0       ARM A
1       M
1       F

