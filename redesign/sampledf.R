
library(random.cdisc.data)
library(dplyr)
ADSL <- radsl()

DM <- ADSL  %>%
  filter(SEX %in% c("M", "F"),
         RACE %in% c("ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE")) %>%  
    mutate(ID = structure(paste0("S", 1:n()), label = "subject id")) %>% 
    select(ID, AGE, SEX, RACE, COUNTRY, ARM, BMRKR1, STRATA1)
  
dim(DM)
head(DM)
