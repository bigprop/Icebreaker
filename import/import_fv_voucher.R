#### IMPORT THE FV_VOUCHER TABLE ####
# date 4/4/2017
## always remember to preserve the row ordering within columns...so can correctly assemble data frame from columns!!!!

rm(list = ls()) # clear the workspace as a precaution
options(stringsAsFactors = FALSE) # set option so that by default strings are left as character vector

require(methods) # Rscript wants it loaded
require(feather)
require(tidyverse)
require(stringr)
require(lubridate)


# INSTANTIATE tibbles from feather
#setwd("C:/Users/rp/Projects/icebreaker_rp")
(fv_voucher_df <- read_feather("data/R_FV_VOUCHER.feather")) # import and view the data
if (interactive()) View(fv_voucher_df)

# dimensions
nrow(fv_voucher_df); ncol(fv_voucher_df)  # 614 rows, 25 cols

colnames(fv_voucher_df)
# [1] "ACCOUNT_NUM"                 "CAN_BE_REVERSED"             "CUST_CASH_DISC_DATE"         "DUE_DATE"                    "EU_SALES_LIST"              
# [6] "EXCH_ADJUSTMENT"             "LAST_INTEREST_DATE"          "OFFSET_ACCOUNT_NUM"          "UTILISED_CASH_DISC"          "OFFSET_COMPANY"             
# [11] "OFFSET_RECID"                "OFFSET_TRANS_VOUCHER"        "PENNY_DIFF"                  "SETTLE_AMOUNT_CUR"           "SETTLE_AMOUNT_MST"          
# [16] "SETTLE_TAX1099_AMOUNT"       "SETTLE_TAX1099_STATE_AMOUNT" "TRANS_COMPANY"               "TRANS_DATE"                  "TRANS_RECID"                
# [21] "CREATE_DATE_TIME"            "CREATED_BY"                  "DATE_AREA_ID"                "REC_VERSION"                 "RECID" 



### HOW MUCH MISSING DATA in THE TIBBLE???? ####
fv_voucher_df %>% summarise_each(funs(100*mean(is.na(.)))) # %>% View # there are a few NA's in the data set.


### COLUMNS THAT WE ARE INTERESTED IN....

### ACCOUNT_NUM ###
# string integer
#
filter(fv_voucher_df, is.na(as.integer(ACCOUNT_NUM))) %>% nrow # 0. all good)

account_num_col <- transmute(fv_voucher_df, account_num = as.integer(ACCOUNT_NUM))

summary(account_num_col)  # this is correct
# account_num    
# Min.   :100116  
# 1st Qu.:100141  
# Median :100404  
# Mean   :100693  
# 3rd Qu.:100404  
# Max.   :105633

# how many distinct customer_account
distinct(account_num_col, account_num) %>% nrow # 26


# Top five customer account by number of transactions
(xdf <- group_by(account_num_col, account_num) %>% summarise(the_count = n(), log2_count = log2(the_count))) %>% arrange(desc(the_count)) %>% head(10)

# account_num the_count log_count
# <int>     <int>     <dbl>
# 1       100404       258 2.4116197
# 2       100141       178 2.2504200
# 3       100436        32 1.5051500
# 4       100269        26 1.4149733
# 5       100139        24 1.3802112


### UTILISED_CASH_DISC ###
# string numeric
# 
# how many NAs?
filter(fv_voucher_df, is.na(UTILISED_CASH_DISC)) %>% nrow # 0.  No NAs

# NEED TO REMOVE ANY $ OR , FROM THE STRING IN ORDER TO CONVERT TO NUMERIC!!!
utilised_cash_disc_col <- transmute(fv_voucher_df, utilised_cash_disc = as.numeric(gsub('[$,]','',UTILISED_CASH_DISC))) 
str(utilised_cash_disc_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	614 obs. of  1 variable:
#   $ utilised_cash_disc: num  156.58 35.2 16.5 36.56 0.03 ...

summary(utilised_cash_disc_col)
# utilised_cash_disc
# Min.   : -93.56   
# 1st Qu.:   2.79   
# Median :  15.56   
# Mean   :  55.39   
# 3rd Qu.:  66.89   
# Max.   :1677.59


### CREATE_DATE_TIME
# string date + time 7/28/2015 19:55 also 2016-10-03 23:16:00 

# test which dates dont fit the lubridate conversion
filter(fv_voucher_df, is.na(parse_date_time(CREATE_DATE_TIME, c('%m/%d/%Y %H:%M', '%Y-%m-%d %H:%M:%S'), exact = TRUE))) %>% nrow # 0. all parsed successfully

create_date_time_col <- transmute(fv_voucher_df, create_date_time = parse_date_time(CREATE_DATE_TIME, c('%m/%d/%Y %H:%M', '%Y-%m-%d %H:%M:%S'), exact = TRUE))

str(create_date_time_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	614 obs. of  1 variable:
#   $ create_date_time: POSIXct, format: "2015-07-28 09:55:00" "2015-07-31 15:34:00" "2016-06-16 09:03:00" "2015-08-17 16:53:00" ...

summary(create_date_time_col)
# create_date_time             
# Min.   :2015-01-05 09:49:00  
# 1st Qu.:2015-07-21 17:54:15  
# Median :2015-10-14 10:30:00  
# Mean   :2015-12-01 23:03:12  
# 3rd Qu.:2016-05-02 08:24:00  
# Max.   :2017-09-01 18:10:00


### BIND INTO A NEW DATAFRAME
xdf <- cbind(
      account_num_col,
      utilised_cash_disc_col,
      create_date_time_col)


if (interactive()) View(xdf) # lets look at the final data frame...

str(xdf)
# 'data.frame':	614 obs. of  3 variables:
# $ account_num       : int  100248 100161 100117 104726 100265 100117 100385 100404 100404 100404 ...
# $ utilised_cash_disc: num  156.58 35.2 16.5 36.56 0.03 ...
# $ create_date_time  : POSIXct, format: "2015-07-28 09:55:00" "2015-07-31 15:34:00" "2016-06-16 09:03:00" "2015-08-17 16:53:00" ..

summary(xdf)
# account_num     utilised_cash_disc create_date_time             
# Min.   :100116   Min.   : -93.56    Min.   :2015-01-05 09:49:00  
# 1st Qu.:100141   1st Qu.:   2.79    1st Qu.:2015-07-21 17:54:15  
# Median :100404   Median :  15.56    Median :2015-10-14 10:30:00  
# Mean   :100693   Mean   :  55.39    Mean   :2015-12-01 23:03:12  
# 3rd Qu.:100404   3rd Qu.:  66.89    3rd Qu.:2016-05-02 08:24:00  
# Max.   :105633   Max.   :1677.59    Max.   :2017-09-01 18:10:00

# write fv_voucher dataframe out as a feather file
write_feather(xdf,"data/fv_voucher.feather")
