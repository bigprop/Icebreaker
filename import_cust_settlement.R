#### IMPORT THE CUSTOMER SETTLEMENT TABLE ####

rm(list = ls()) # clear the workspace as a precaution

require(feather)
require(tidyverse)
require(stringr)
require(dplyr)
#require(bit64)

# INSTANTIATE tibbles from feather
setwd("C:/Users/rp/Projects/icebreaker_rp")
(cust_settlement <- read_feather("R_CUST_SETTLEMENT.feather")) #  %>% View

# dimensions
nrow(cust_settlement); ncol(cust_settlement)  # 197436 rows, 9 cols

colnames(cust_settlement)
# [1] "ACCOUNT"            "ACCOUNT_NUMBER"     "OFFSET_VOUCHER"     "DATE_OF_SETTLEMENT" "DUE_DATE"           "AMOUNT_SETTLED"     "SETTLED_CURRENCY"   "COMPANY_ACCOUNTS"  
# [9] "COMPANY_ACCOUNTS2"


### HOW MUCH MISSING DATA in THE TIBBLE???? ####
cust_settlement %>% summarise_each(funs(100*mean(is.na(.)))) # No NA's in the data set


### "ACCOUNT"            
# string integer
# 
# Get a count of the rows that fail conversion
cust_settlement %>% filter( is.na(as.integer(ACCOUNT))) %>% nrow # 0

account_col <- transmute(cust_settlement, account = as.integer(ACCOUNT))
summary(account_col)
# Min.   :100115              
# 1st Qu.:100293              
# Median :100468              
# Mean   :101302              
# 3rd Qu.:100647              
# Max.   :106028

# do some investigation on number of unique accounts and the aggregate counts
distinct(account_col, account) %>% nrow  # 371
 


### "ACCOUNT_NUMBER"     
# string integer
# 
# Get a count of the rows that fail conversion
cust_settlement %>% filter( is.na(as.integer(ACCOUNT_NUMBER))) %>% nrow # 0

account_number_col <- transmute(cust_settlement, account_number = as.integer(ACCOUNT_NUMBER))
summary(account_number_col)
# Min.   :100115              
# 1st Qu.:100293              
# Median :100468              
# Mean   :101302              
# 3rd Qu.:100647              
# Max.   :106028

# do some investigation on number of unique accounts and the aggregate counts
distinct(account_col, account) %>% nrow  # 371


### "OFFSET_VOUCHER"     
# prefix factor + string integer.  prefix varies PV, SCV, CIV 

# investigating gsub
# gsub("\\d+","", "PCI209") # replace the digits with "" leaving the prefix
# gsub("\\D+","", "PCI209") # replace the !digits with "" leaving the digits

transmute(cust_settlement, offset_voucher_prefix = gsub("\\d+","", OFFSET_VOUCHER)) %>% distinct(offset_voucher_prefix)
# offset_voucher_prefix
# <chr>
# 1                   CIV
# 2                   SCV
# 3                   GJI
# 4                    PV
# 5                   FCV
# 6                   GJV
# 7                 TVNMD
# 8                   FTI
# 9                  IANZ

offset_voucher_col <- transmute(cust_settlement, 
                                offset_voucher_prefix = as.factor(gsub("\\d+","", OFFSET_VOUCHER)), # remove digits
                                offset_voucher = as.integer(gsub("\\D+","", OFFSET_VOUCHER)))       # remove non digits
str(offset_voucher_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	197436 obs. of  2 variables:
# $ offset_voucher_prefix: Factor w/ 9 levels "CIV","FCV","FTI",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ offset_voucher       : int  92087 92125 92125 92126 92126 93300 93639 94466 95040 95886 ...

summary(offset_voucher_col)
# offset_voucher_prefix offset_voucher  
# CIV    :112640        Min.   :     4  
# PV     : 57430        1st Qu.: 32990  
# SCV    : 16256        Median : 71910  
# GJI    :  4871        Mean   : 76228  
# FCV    :  3167        3rd Qu.:120135  
# GJV    :  2492        Max.   :178031  
# (Other):   580 


### "DATE_OF_SETTLEMENT" 
# string date eg 31/05/15  
#
date_of_settlement_col <- transmute(cust_settlement, date_of_settlement = dmy(DATE_OF_SETTLEMENT))
str(date_of_settlement_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	197436 obs. of  1 variable:
#   $ dmy(DATE_OF_SETTLEMENT): Date, format: "2015-05-31" "2015-05-31" "2015-05-31" "2015-05-31" ...

summary(date_of_settlement_col)
# dmy(DATE_OF_SETTLEMENT)
# Min.   :2015-04-01     
# 1st Qu.:2015-08-26     
# Median :2016-01-21     
# Mean   :2016-01-28     
# 3rd Qu.:2016-06-30     
# Max.   :2016-12-31


### "DUE_DATE"          
# string date eg 31/05/15
due_date_col <- transmute(cust_settlement, due_date = dmy(DUE_DATE))
str(due_date_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	197436 obs. of  1 variable:
#   $ dmy(DATE_OF_SETTLEMENT): Date, format: "2015-05-31" "2015-05-31" "2015-05-31" "2015-05-31" ...

summary(due_date_col)
# due_date         
# Min.   :2009-09-28  
# 1st Qu.:2015-08-04  
# Median :2015-12-31  
# Mean   :2016-01-10  
# 3rd Qu.:2016-06-20  
# Max.   :2017-03-31


### "AMOUNT_SETTLED"
# string numeric
# 
# how many NAs?
filter(cust_settlement, is.na(AMOUNT_SETTLED)) %>% nrow # 0.  No NAs

# NEED TO REMOVE ANY $ OR , FROM THE STRING IN ORDER TO CONVERT TO NUMERIC!!!
amount_settled_col <- transmute(cust_settlement, amount_settled = as.numeric(gsub('[$,]','',AMOUNT_SETTLED))) 

str(amount_settled_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	197436 obs. of  1 variable:
#   $ amount_settled: num  9.16 -0.51 0.9 -3.14 5.56 ...

summary(amount_settled_col)
# amount_settled     
# Min.   :-534805.3  
# 1st Qu.:    -47.1  
# Median :      0.0  
# Mean   :      0.2  
# 3rd Qu.:     47.1  
# Max.   : 534805.3



### BIND INTO A NEW DATAFRAME / TIBBLE
# [1] "ACCOUNT"            "ACCOUNT_NUMBER"     "OFFSET_VOUCHER"     "DATE_OF_SETTLEMENT" "DUE_DATE"           "AMOUNT_SETTLED"


xdf <- cbind(
      account_col,
      account_number_col,
      offset_voucher_col,
      date_of_settlement_col,
      due_date_col,
      amount_settled_col)

View(xdf) # lets look at the findal data frame...

str(xdf)
# 'data.frame':	197436 obs. of  7 variables:
# $ account                : int  100115 100115 100115 100115 100115 100115 100115 100115 100115 100115 ...
# $ account_number         : int  100115 100115 100115 100115 100115 100115 100115 100115 100115 100115 ...
# $ offset_voucher_prefix  : Factor w/ 9 levels "CIV","FCV","FTI",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ offset_voucher         : int  92087 92125 92125 92126 92126 93300 93639 94466 95040 95886 ...
# $ dmy(DATE_OF_SETTLEMENT): Date, format: "2015-05-31" "2015-05-31" "2015-05-31" "2015-05-31" ...
# $ due_date               : Date, format: "2015-05-31" "2015-05-31" "2015-05-31" "2015-05-31" ...
# $ amount_settled         : num  9.16 -0.51 0.9 -3.14 5.56 ...

summary(xdf)
# account       account_number   offset_voucher_prefix offset_voucher   dmy(DATE_OF_SETTLEMENT)    due_date          amount_settled     
# Min.   :100115   Min.   :100115   CIV    :112640        Min.   :     4   Min.   :2015-04-01      Min.   :2009-09-28   Min.   :-534805.3  
# 1st Qu.:100188   1st Qu.:100188   PV     : 57430        1st Qu.: 32990   1st Qu.:2015-08-26      1st Qu.:2015-08-04   1st Qu.:    -47.1  
# Median :100357   Median :100357   SCV    : 16256        Median : 71910   Median :2016-01-21      Median :2015-12-31   Median :      0.0  
# Mean   :100819   Mean   :100819   GJI    :  4871        Mean   : 76228   Mean   :2016-01-28      Mean   :2016-01-10   Mean   :      0.2  
# 3rd Qu.:100524   3rd Qu.:100524   FCV    :  3167        3rd Qu.:120135   3rd Qu.:2016-06-30      3rd Qu.:2016-06-20   3rd Qu.:     47.1  
# Max.   :106028   Max.   :106028   GJV    :  2492        Max.   :178031   Max.   :2016-12-31      Max.   :2017-03-31   Max.   : 534805.3 

# write customer out as a feather file
write_feather(xdf,"cust_settlement.feather")
