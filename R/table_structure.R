







#' Print Table Structure
#' 
#' Tree view of table structure
#' 
#' @param tbl table object
#' 
#' 
#' @export
#' 
#' 
#' @examples 
#' 
#' ADSL <- ex_adsl
#' tbl <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   analyze(vars = c("AGE", "BMRKR1"), afun = function(x) {
#'     in_rows(
#'       mean_sd = rcell(c(mean(x), sd(x)), format = "xx.xx (xx.xx)"),
#'       range = rcell(range(x), format = "xx.xx - xx.xx"),
#'       .labels = c(mean_sd = "Mean (Sd)", range = "Range")
#'     )
#'   }) %>%
#'   build_table(ADSL)
#' 
#' 
#' table_structure(tbl)
table_structure2 <- function(tbl) {
  
  stopifnot(is(tbl, "TableTree"))
  
  structure(
    list(
      cols = col_structure2(tbl),
      rows = row_structure2(tbl)
    ),
    class = "rtable_structure"
  )
}

#' @rdname table_structure
#' 
#' @export
row_structure2 <- function(tbl) {
  stopifnot(is(tbl, "TableTree"))
  
  tblstr <- make_pagdf(tbl, visible_only = FALSE)[, c("label", "name", "abs_rownumber", "path", "indent", "node_class")]
  rownames(tblstr) <- NULL
  tblstr
}


#' @rdname table_structure
#' 
#' @export
col_structure2 <- function(tbl) {
  
}



# Doodle

#         label    name abs_rownumber         path indent      node_class
#   1                              NA                   0       TableTree
#   2               AGE            NA        , AGE      1 ElementaryTable
#   3       AGE     AGE             1        , AGE      1        LabelRow
#   4 Mean (Sd) mean_sd             2 , AGE, m....      3         DataRow
#   5     Range   range             3 , AGE, range      3         DataRow
#   6            BMRKR1            NA     , BMRKR1      1 ElementaryTable
#   7    BMRKR1  BMRKR1             4     , BMRKR1      1        LabelRow
#   8 Mean (Sd) mean_sd             5 , BMRKR1....      3         DataRow
#   9     Range   range             6 , BMRKR1....      3         DataRow
#   
#                  A: Drug X      B: Placebo    C: Combination
#   ----------------------------------------------------------
#   AGE                                                       
#     Mean (Sd)   33.77 (6.55)   35.43 (7.9)     35.43 (7.72) 
#     Range         21 - 50        21 - 62         20 - 69    
#   BMRKR1                               #                     
#     Mean (Sd)   5.97 (3.55)     5.7 (3.31)     5.62 (3.49)  
#     Range       0.41 - 17.67   0.65 - 14.24    0.17 - 21.39 
#     
#     
#   column structure:
#   
#   name                      label             indent
#   --------------------------------------------------
#   root                    
#     ARMA                    A: Drug X
#     ARMB                    B: Placebo
#     ARMC                    C: Combination
#     
#   
#   row structure:
#   
#   name                      label             indent
#   ---------------------------------------------------
#   root
#     AGE                     AGE                  1
#       mean_sd               Mean (Sd)            2
#       range                 Range                2
#     SEX                     SEX                  1
#       mean_sd               Mean (Sd)            2
#       range                 Range                2
#   
#   
#   
#   TableTree: [root] ()                           |
#     labelrow: -                                  |
#     content: -                                   |
#     children:                                    |
#       ElementaryTable: [AGE] (AGE)               |
#         labelrow: [AGE] (AGE)                    | AGE
#         children:                                |
#           DataRow: [mean_sd] (Mean (Sd))         |  Mean (Sd)
#           DataRow: [range] (Range)               |  Range
#       ElementaryTable: [BMRKR1] (BMRKR1)         |
#         labelrow: [BMRKR1] (BMRKR1)              | BMRKR1
#         children:                                |
#           DataRow: [mean_sd] (Mean (Sd))         |  Mean (Sd)
#           DataRow: [range] (Range)               |  Range
#   
#   
#   
#   
#   
#   
#   Path      | Row Print      |   |depth|indent| Tree Structure
#   ----------------------------------------------------------------------------------
#             |                |   |     |     | TableTree: [root] ()                           
#             |                |   |     |     |   labelrow: -                                  
#             |                |   |     |     |   content: -                                   
#             |                |   |     |     |   children:                                    
#   AGE       |                |   |     |     |     ElementaryTable: [AGE] (AGE)               
#             | AGE of Patient | l |     |     |       labelrow: [AGE] (AGE of Patient)                    
#             |                |   |     |     |       children:                                
#    mean_sd  |   Mean (Sd)    | d |     |     |         DataRow: [mean_sd] (Mean (Sd))         
#    range    |   Range        | d |     |     |         DataRow: [range] (Range)               
#   BMRKR1    |                |   |     |     |     ElementaryTable: [BMRKR1] (BMRKR1)         
#             | BMRKR1         | l |     |     |       labelrow: [BMRKR1] (BMRKR1)              
#             |                |   |     |     |       children:                                
#    mean_sd  |   Mean (Sd)    | d |     |     |         DataRow: [mean_sd] (Mean (Sd))         
#    range    |   Range        | d |     |     |         DataRow: [range] (Range)                



# table_paths() # need col_paths()
# row_paths()
# rowname (indented)    type (l,c,d)  row nr   path

# table_structure is str for table treestruct

#                                          path      indent      
#    AGE                  , AGE      1 
#     Mean (Sd)            AGE, m....      3      
#     Range              AGE, range      3     
#   BMRKR1             BMRKR1      1 
#     Mean (Sd)             5 , BMRKR1....      3 
#     Range              6 , BMRKR1....      3 


# rtables:::treestruct(tbl)
# rtables:::make_pagdf(tbl)
# cell_value(tbl, c("B", "Sepal.Length", "Median"), "setosa") # names, not labels

# Column Structure:
# name                label              col_count
# - setosa            Setosa Flower         10
# - versicolor        Versicolor Flower     20
# - virginica         Viginica Flower       32
# (don't) show col counts
# 
# Row Structure:
# name             label          visible    type 
# - Sepal.Length   Sepal.Length    TRUE      label
#   - Min.         Min.            TRUE      content
#   - 1st Qu.      ...             TRUE
#   - Median       ...             TRUE
#   - Mean         ...             TRUE
#   - 3rd. Qu.     ...             TRUE
#   - Max.         ...             TRUE
# - Petal.Length   ...             TRUE
#   - Min.         ...             TRUE
#   - 1st Qu.      ...             TRUE
#   - Median       ...             TRUE
#   - Mean         ...             TRUE
#   - 3rd. Qu.     ...             TRUE
#   - Max.         ...             TRUE
# 


# SOC
#  TERM           ARM A           ARM B
# -------------------------------------
# 


# 