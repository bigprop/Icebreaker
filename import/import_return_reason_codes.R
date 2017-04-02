#### IMPORT THE RETURN REASON CODES ####

rm(list = ls()) # clear the workspace as a precaution

require(feather)
require(tidyverse)
require(stringr)
require(dplyr)
# require(bit64)

# options(scipen=999) # force not to use scientific notation for number display
# options(scipen=0) # default value

# INSTANTIATE tibbles from feather
# setwd("C:/Users/rp/Projects/icebreaker_rp")
(return_reason_in <- read_feather("data/R_RETURN_REASON_CODES.feather")) %>% View # import and view the data

# dimensions
nrow(return_reason_in); ncol(return_reason_in)  # 58 rows. 3 cols

# column names
colnames(return_reason_in)
# [1] "RETURN_REASON_CODE"       "DESCRIPTION"              "RETURN_REASON_CODE_GROUP"

### HOW MUCH MISSING DATA in THE TIBBLE???? ####
return_reason_in %>% summarise_each(funs(100*mean(is.na(.)))) # 0.  No NA

# any duplicate rows that need to be removed?
(xdf <- group_by(return_reason_in, RETURN_REASON_CODE) %>% arrange(RETURN_REASON_CODE)) %>% filter(n() > 1) %>% nrow # 0. 
return_reason <- return_reason_in %>% arrange(RETURN_REASON_CODE) # No duplicates need to be removed


### "RETURN_REASON_CODE" ###
# string factor. lower case
# 
# how many distinct factors?
distinct(return_reason_in, RETURN_REASON_CODE) %>% arrange(RETURN_REASON_CODE) # %>% View   No duplicates
# 1              CANCEL
# 2             CARRIER
# 3              CLOSED
# 4            CNSM_BIG
# 5            CNSM_COL
# 6           CNSM_DESC
# 7            CNSM_FIT
# 8            CNSM_MAT
# 9           CNSM_NEED
# 10           CNSM_SML
# # ... with 48 more rows

# Need to avoid duplicates By changing everything to lower case and then convert to a factor
return_reason_col <- transmute(return_reason, return_reason_code = as.factor(str_to_lower(RETURN_REASON_CODE)))

str(return_reason_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	58 obs. of  1 variable:
#   $ return_reason_code: Factor w/ 58 levels "cancel","carrier",..: 1 2 3 4 5 6 7 8 9 10 ...

summary(return_reason_col)
# return_reason_code
# cancel   : 1      
# carrier  : 1      
# closed   : 1      
# cnsm_big : 1      
# cnsm_col : 1      
# cnsm_desc: 1      
# (Other)  :52


### "DESCRIPTION" ### 
# leave a description string - case unchanged.
# 
description_col <- select(return_reason, DESCRIPTION)


### "RETURN_REASON_CODE_GROUP" ###
# string factor. lower case
# 
# how many distinct factors?
distinct(return_reason_in, RETURN_REASON_CODE_GROUP) %>% arrange(RETURN_REASON_CODE_GROUP) # %>% View   No duplicates
# 1                CARR ISSUE
# 2                CNSM CHOIC
# 3                CUST FAULT
# 4                 DES FAULT
# 5                 FAB FAULT
# 6                 MFG FAULT
# 7                  OM ISSUE
# 8                     OTHER
# 9                PACK FAULT
# 10               PROD ISSUE
# 11               SALE ISSUE
# 12                 WH ISSUE

# Need to avoid duplicates By changing everything to lower case and then convert to a factor
return_reason_group_col <- transmute(return_reason, return_reason_group = as.factor(str_to_lower(RETURN_REASON_CODE_GROUP)))

str(return_reason_group_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	58 obs. of  1 variable:
#   $ return_reason_group: Factor w/ 12 levels "carr issue","cnsm choic",..: 7 1 7 2 2 2 2 2 2 2 ....

summary(return_reason_group_col)
# return_reason_group
# cnsm choic: 8      
# mfg fault : 8      
# fab fault : 7      
# other     : 7      
# cust fault: 6      
# wh issue  : 5      
# (Other)   :17



### CREATE THE NEW RETURN_REASON_CODES
# [1] "RETURN_REASON_CODE"       "DESCRIPTION"              "RETURN_REASON_CODE_GROUP"

xdf <- cbind(
    return_reason_col,
    description_col,
    return_reason_group_col)

View(xdf)

str(xdf)
# 'data.frame':	58 obs. of  3 variables:
# $ return_reason_code : Factor w/ 58 levels "cancel","carrier",..: 1 2 3 4 5 6 7 8 9 10 ...
# $ DESCRIPTION        : chr  "Order Cancelled" "Refused/Cartons Damaged in Transit" "Account Closed" "Consumer Choice_Fit/Size too big" ...
# $ return_reason_group: Factor w/ 12 levels "carr issue","cnsm choic",..: 7 1 7 2 2 2 2 2 2 2 ...

# write out the new dataframe as a feather file
write_feather(xdf, "data/return_reason_codes.feather")
