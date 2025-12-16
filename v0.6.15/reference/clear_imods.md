# Clear all indent modifiers from a table

Clear all indent modifiers from a table

## Usage

``` r
clear_indent_mods(tt)

# S4 method for class 'VTableTree'
clear_indent_mods(tt)

# S4 method for class 'TableRow'
clear_indent_mods(tt)
```

## Arguments

- tt:

  (`TableTree` or related class)  
  a `TableTree` object representing a populated table.

## Value

The same class as `tt`, with all indent modifiers set to zero.

## Examples

``` r
lyt1 <- basic_table() %>%
  summarize_row_groups("STUDYID", label_fstr = "overall summary") %>%
  split_rows_by("AEBODSYS", child_labels = "visible") %>%
  summarize_row_groups("STUDYID", label_fstr = "subgroup summary") %>%
  analyze("AGE", indent_mod = -1L)

tbl1 <- build_table(lyt1, ex_adae)
tbl1
#>                           all obs   
#> ————————————————————————————————————
#> overall summary        1934 (100.0%)
#>   cl A.1                            
#>     subgroup summary    422 (21.8%) 
#>     Mean                   34.70    
#>   cl B.1                            
#>     subgroup summary    178 (9.2%)  
#>     Mean                   35.86    
#>   cl B.2                            
#>     subgroup summary    410 (21.2%) 
#>     Mean                   35.42    
#>   cl C.1                            
#>     subgroup summary    182 (9.4%)  
#>     Mean                   33.83    
#>   cl C.2                            
#>     subgroup summary    166 (8.6%)  
#>     Mean                   33.89    
#>   cl D.1                            
#>     subgroup summary    368 (19.0%) 
#>     Mean                   34.39    
#>   cl D.2                            
#>     subgroup summary    208 (10.8%) 
#>     Mean                   34.83    
clear_indent_mods(tbl1)
#>                           all obs   
#> ————————————————————————————————————
#> overall summary        1934 (100.0%)
#>   cl A.1                            
#>     subgroup summary    422 (21.8%) 
#>       Mean                 34.70    
#>   cl B.1                            
#>     subgroup summary    178 (9.2%)  
#>       Mean                 35.86    
#>   cl B.2                            
#>     subgroup summary    410 (21.2%) 
#>       Mean                 35.42    
#>   cl C.1                            
#>     subgroup summary    182 (9.4%)  
#>       Mean                 33.83    
#>   cl C.2                            
#>     subgroup summary    166 (8.6%)  
#>       Mean                 33.89    
#>   cl D.1                            
#>     subgroup summary    368 (19.0%) 
#>       Mean                 34.39    
#>   cl D.2                            
#>     subgroup summary    208 (10.8%) 
#>       Mean                 34.83    
```
