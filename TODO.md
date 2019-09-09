
# nesting and order
NULL %>% add_colby_varlevels("ARM") %>% add_col_total() %>% add_colby_varlevels("SEX")

 A       B       Tot  M  F

NULL %>% add_colby_varlevels("ARM") %>% add_colby_varlevels("SEX") %>% add_col_total()

 A       B       
M F     M  F    Tot


#subset comparison
NULL %>% add_colby_varlevels("ARM") %>% add_colby_subset("BEP") %>% add_col_total()

  A       B        Tot
BEP ALL  BEP ALL


# rendering with gt?
tbl <- build_table(thing, data)

tbl %>% to_string_df() %>% gt() %>% fmt_...

tbl %>% paginate() %>% to_string_df() %>% foreach table do : gt() %>% fmt_...


# future: templating
# pkgname: standards.tables

lay <- s_t_dm(vars = c("AGE", "SEX", "RACE"), col_by = "ARM", version = "v0.31")

lay <- s_t_dm(version = "v0.31", col_by = "ARM")

tbl <- build_table(lay, ADSL)


lopo <- onco_standards("v.31")

# tern like ad-hoc analysis

tbl <- t_summary(ADSL %>% select(ARM, SEX, RACE), col_by = ADSL$ARM)

lay <- NULL %>% 
  add_colby_varlevels("ARM") %>% 
  add_anlvar("AGE", afun = summary) %>%
  add_anlvar("SEX", afun = table, toplevel = TRUE)


lay <- NULL %>% 
  add_colby_varlevels("ARM") %>% 
  add_anlvar(c("AGE", "SEX"))
  
  
#>                                                        ARM A                ARM B                ARM C    
#>                                                       (N=142)              (N=133)              (N=125)   
#> ----------------------------------------------------------------------------------------------------------
#> Age
#>   n                                                     142                  133                  125     
#>   Mean (SD)                                        40.34 (17.42)        39.82 (17.7)         41.1 (19.67) 
#>   Median                                                39                   37                   37      
#>   Min - Max                                           20 - 93             20 - 116              20 - 88   
#> 
#> Sex
#>   n                                                     142                  133                  125     
#>   F                                                 69 (48.59%)          67 (50.38%)          68 (54.4%)  
#>   M                                                 68 (47.89%)          63 (47.37%)          53 (42.4%)  
#>   U                                                  2 (1.41%)            3 (2.26%)            4 (3.2%)   
#>   UNDIFFERENTIATED                                   3 (2.11%)                -                    -      
#> 



t_events_per_term_grade_id(
  terms = ADAE$AEDECOD,
  id = ADAE$USUBJID,
  grade = ADAE$AETOXGR,
  col_by = ADAE$ARM,
  col_N = table(ADSL$ARM),
  total = "All Patients",
  grade_levels = 1:5
)



