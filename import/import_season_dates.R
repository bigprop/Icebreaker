#### IMPORT SEASON DATES TABLE ####

rm(list = ls()) # clear the workspace as a precaution

require(feather)
require(tidyverse)
require(stringr)
require(dplyr)


# INSTANTIATE tibbles from feather
# setwd("C:/Users/rp/Projects/icebreaker_rp")
(season_dates <- read_feather("data/R_SEASON_DATES.feather")) # %>% View


# dimensions
nrow(season_dates); ncol(season_dates)  # 10 rows, 4 cols

colnames(season_dates)
# [1] "REGION"     "SEASON"     "BEGIN_DATE" "END_DATE"


### HOW MUCH MISSING DATA in THE TIBBLE???? ####
season_dates %>% summarise_each(funs(100*mean(is.na(.)))) # 0 NA's in the data set 



### "REGION" ###                          
# string factor
# 
(region_col <- transmute(season_dates, region = as.factor(str_to_lower(REGION))) %>% arrange(region)) # factor 

str(region_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	10 obs. of  1 variable:
#   $ season: Factor w/ 5 levels "aus","can","eur",..: 1 1 2 2 3 3 4 4 5 5

summary(region_col)
# season 
# aus:2  
# can:2  
# eur:2  
# nzl:2  
# usa:2


### "SEASON" ###                          
# string factor
# 
(season_col <- transmute(season_dates, season = as.factor(str_to_lower(SEASON))) %>% arrange(season)) # factor 

str(season_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	10 obs. of  1 variable:
#   $ season: Factor w/ 2 levels "summer","winter": 1 1 1 1 1 2 2 2 2 2

summary(season_col)
# season 
# summer:5  
# winter:5


### "BEGIN_DATE" ###       
# keep as a string so can add year and convert to a date as required
begin_date_col <- select(season_dates, begin_date = BEGIN_DATE)


### "END_DATE" ###       
# keep as a string so can add year and convert to a date as required
end_date_col <- select(season_dates, end_date = END_DATE)



### BIND INTO A NEW DATAFRAME / TIBBLE
# [1] "REGION"     "SEASON"     "BEGIN_DATE" "END_DATE"

xdf <- cbind(
      region_col,
      season_col,
      begin_date_col,
      end_date_col)

View(xdf) # lets look at the findal data frame...

str(xdf)
# $ region    : Factor w/ 5 levels "aus","can","eur",..: 1 1 2 2 3 3 4 4 5 5
# $ season    : Factor w/ 2 levels "summer","winter": 1 1 1 1 1 2 2 2 2 2
# $ begin_date: chr  "1-Aug" "1-Aug" "1-Aug" "1-Mar" ...
# $ end_date  : chr  "31-Jan" "31-Jan" "31-Jan" "14-Sep" ...

summary(xdf)
# region     season   begin_date          end_date        
# aus:2   summer:5   Length:10          Length:10         
# can:2   winter:5   Class :character   Class :character  
# eur:2              Mode  :character   Mode  :character  
# nzl:2                                                   
# usa:2
 

# write season_dates out as a feather file
write_feather(xdf,"data/season_dates.feather")
