#### IMPORT THE CUSTOMER SETTLEMENT TABLE ####
# date:  4/4/17
# update with data/ path
# consider using the readr package parse_* functions
# 

rm(list = ls()) # clear the workspace as a precaution

require(methods) # Rscript wants it loaded
require(feather)
require(tidyverse)
require(lubridate)
require(stringr)


# INSTANTIATE tibbles from feather
# setwd("C:/Users/rp/Projects/icebreaker_rp")
(cust_settlement <- read_feather("data/R_CUST_SETTLEMENT.feather")) #  import the data
if (interactive()) View(cust_settlement)

# dimensions
nrow(cust_settlement); ncol(cust_settlement)  # 197436 rows, 9 cols

colnames(cust_settlement)
# [1] "ACCOUNT"    "**ACCOUNT_NUMBER"     "OFFSET_VOUCHER"     "DATE_OF_SETTLEMENT" "DUE_DATE"           "AMOUNT_SETTLED"     "SETTLED_CURRENCY"   
# [8] "COMPANY_ACCOUNTS"  "COMPANY_ACCOUNTS2"

# columns that we are interested in for new data.frame
# [1] **"ACCOUNT_NUMBER"    "OFFSET_VOUCHER"     "DATE_OF_SETTLEMENT" "DUE_DATE"           "AMOUNT_SETTLED"
 


### HOW MUCH MISSING DATA in THE TIBBLE???? ####
cust_settlement %>% summarise_each(funs(100*mean(is.na(.)))) # No NA's in the data set


### "ACCOUNT"   not used as it is a dup of ACCOUNT_NUMBER         
# string integer
# 
# Get a count of the rows that fail conversion
#cust_settlement %>% filter( is.na(as.integer(ACCOUNT))) %>% nrow # 0

# CONSIDER USING 'READR' PACKAGE PARSE_* FUNCTIONS TO CREATE VECTORS AND THEN BIND THEM AND COERCE INTO A TIBBLE
# x <- parse_integer(cust_settlement$ACCOUNT)
# y <- parse_number(cust_settlement$ACCOUNT)
# xdf <- as_tibble(cbind(first_col = x, second_col = y))
# summary (x)

#account_col <- transmute(cust_settlement, customer_account = as.integer(ACCOUNT))
#summary(account_col)
# Min.   :100115              
# 1st Qu.:100293              
# Median :100468              
# Mean   :101302              
# 3rd Qu.:100647              
# Max.   :106028

# do some investigation on number of unique accounts and the aggregate counts
#distinct(account_col, account) %>% nrow  # 371
 


### **"ACCOUNT_NUMBER"
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
distinct(account_number_col, account_number) %>% nrow  # 371

# is any difference between account and account_number columns?
filter(cust_settlement, ACCOUNT != ACCOUNT_NUMBER) %>% nrow # 0. so both columns contain the same data.


### "OFFSET_VOUCHER"     
# prefix + integer. convert to a factor.  prefix varies PV, SCV, CIV.  
# generate the prefix + integer summary for information purposes
#
#
# transmute(cust_settlement, offset_voucher_prefix = gsub("\\d+","", OFFSET_VOUCHER)) %>% distinct(offset_voucher_prefix)
xdf <- transmute(cust_settlement, 
                 prefix = as.factor(gsub("\\d+","", OFFSET_VOUCHER)),     # remove digits
                 voucher = as.integer(gsub("\\D+","", OFFSET_VOUCHER)))   # remove non digits
str(xdf)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	197436 obs. of  2 variables:
# $ offset_voucher_prefix: Factor w/ 9 levels "CIV","FCV","FTI",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ offset_voucher       : int  92087 92125 92125 92126 92126 93300 93639 94466 95040 95886 ...

summary(xdf)
# offset_voucher_prefix offset_voucher  
# CIV    :112640        Min.   :     4  
# PV     : 57430        1st Qu.: 32990  
# SCV    : 16256        Median : 71910  
# GJI    :  4871        Mean   : 76228  
# FCV    :  3167        3rd Qu.:120135  
# GJV    :  2492        Max.   :178031  
# (Other):   580 

offset_voucher_col <- transmute(cust_settlement, offset_voucher = as.factor(str_to_lower(OFFSET_VOUCHER)))
str(offset_voucher_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	197436 obs. of  1 variable:
#   $ as.factor(str_to_lower(OFFSET_VOUCHER)): Factor w/ 58855 levels "civ0000175","civ0000223",..: 7360 7362 7362 7363 7363 7972 8223 8719 9106 9534 ...

summary(offset_voucher_col)
# offset_voucher  
# pv00029276:  1378  
# pv00044729:  1289  
# pv00022738:  1278  
# pv00037395:   932  
# pv00040921:   798  
# pv00025516:   707  
# (Other)   :191054


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
# [1] "ACCOUNT_NUMBER"     "OFFSET_VOUCHER"     "DATE_OF_SETTLEMENT" "DUE_DATE"           "AMOUNT_SETTLED"


xdf <- cbind(
      #account_col,         
      account_number_col,   # key: account_number for matching
      offset_voucher_col,
      date_of_settlement_col,
      due_date_col,
      amount_settled_col)

if (interactive()) View(xdf) # lets look at the final data frame...

str(xdf)
# 'data.frame':	197436 obs. of  5 variables:
# $ account_number    : int  100115 100115 100115 100115 100115 100115 100115 100115 100115 100115 ...
# $ offset_voucher    : Factor w/ 58855 levels "civ0000175","civ0000223",..: 7360 7362 7362 7363 7363 7972 8223 8719 9106 9534 ...
# $ date_of_settlement: Date, format: "2015-05-31" "2015-05-31" "2015-05-31" "2015-05-31" ...
# $ due_date          : Date, format: "2015-05-31" "2015-05-31" "2015-05-31" "2015-05-31" ...
# $ amount_settled    : num  9.16 -0.51 0.9 -3.14 5.56 ...

# write customer out as a feather file
write_feather(xdf,"data/cust_settlement.feather")
