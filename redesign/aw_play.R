
library(dplyr)
library(rtables)


# what dows tt_* stand for, e.g. tt_labelrow

# Where are variable Names ----
tbl <- NULL %>% 
  add_colby_varlevels("Species") %>%
  add_analyzed_vars("Sepal.Length", afun = mean) %>%
  build_table(iris)

tbl  

summary(tbl) # why is row_type missing

iris2 <- iris %>%
  group_by(Species) %>%
  mutate(group = as.factor(rep_len(c("a", "b"), length.out = n()))) %>%
  ungroup()

l <- NULL %>% 
  add_colby_varlevels("Species") %>%
  add_colby_varlevels("group") %>%
  add_analyzed_vars(c("Sepal.Length", "Petal.Width"), afun = lstwrapx(summary) , fmt = "xx.xx")

l

tbl <- build_table(l, iris2)

tbl

# boken as_html ----
as_html(tbl)
Viewer(tbl)

# toString issues ----
class(tbl)
toString(tbl)
tbl

# How do I get the Mean for multiple Columns ----
NULL %>%
  add_colby_multivar(c("Sepal.Length", "Petal.Length"), "") %>% 
  add_analyzed_colvars(lbl = "", afun = mean) %>%
  build_table(iris)

#            Sepal.Length       Petal.Length
#  All           5.84              3.76

# vs.
all arguments are used in examples.

#                 Sepal.Length       Petal.Length
#  versicolor
#  setosa
#  virginica


#
#          a1 | a1        c          d
#         afun(x11)   afun(x12)  afun(x13)
#         afun(x21)   afun(x22)  afun(x23)
#         afun(x31)   afun(x32)  afun(x33)


#             a         c          d
#         afun(x11)   afun(x12)  afun(x13)
#         afun(x21)   afun(x22)  afun(x23)
#         afun(x31)   afun(x32)  afun(x33)

NULL %>% ... %>% 
  add_header_split("a", c("a1", "a2"))

x <- rcell(lm(1~1))

class(x)


## rcell
##  - show method
##  - what is the label slot?
rcell(5, format = "xx.xx")

## strip rcells
obj <- s_coxph(Surv(tte, resp) ~ ARM, data = ADTE_F)

list(
  a = list(
    mod1 = rcell(...),
    mod
  )
)

# nested list of rcells?
x <- lstwrapdf(table)(iris$Species) # is invisible


x

###
#
# 
#
#

# Renaming ----
# these are only notes for now, please discuss before starting with renaming

lyt <- NULL %>% 
  add_colby_varlevels("Species") %>%
  analyze("Sepal.Length", afun = mean)



