#### IMPORT THE INVOICE JOURNAL ####
# date 4/4/17

rm(list = ls()) # clear the workspace as a precaution

require(methods) # Rscript wants it loaded
require(feather)
require(tidyverse)
require(stringr)
require(lubridate)

# options(scipen=999) # force not to use scientific notation for number display
# options(scipen=0) # default value

# INSTANTIATE tibbles from feather
# setwd("C:/Users/rp/Projects/icebreaker_rp")
(invoice_journal_in <- read_feather("data/R_CUSTOMER_INVOICE_JOURNAL.feather")) # import and view the data
if (interactive()) View(invoice_journal_in)

# dimensions
nrow(invoice_journal_in); ncol(invoice_journal_in)  # 49428 rows. 28 cols

# column names
colnames(invoice_journal_in)
# [1] "CUSTOMER_ACCOUNT"         "INVOICE_ACCOUNT"          "SALES_ORDER"              "INVOICE_DATE"             "INVOICE_NUMBER"          
# [6] "VOUCHER"                  "INVOICE_AMOUNT"           "CASH_DIS_VALUE"           "CASH_DIS_CODE"            "DELIVERY_TERMS"          
# [11] "DEPARTMENT"               "DIMENSION"                "DIMENSION_ACCOUNT"        "LINE_DISCOUNT"            "ORDER_TYPE"              
# [16] "ORDERTYPE2"               "RETURN_REASON_CODE"       "SALES_SUBTOTAL_AMOUNT"    "SALES_TAKER"              "TERMS_OF_PAYMENT"        
# [21] "INVOICE_AMOUNT_AC"        "LINE_DISCOUNT_AC"         "SALES_SUBTOTAL_AMOUNT_AC" "SALES_TAX_AC"             "CHARGES_AC"              
# [26] "CURRENCY"                 "CHARGES"                  "WAREHOUSE"

### HOW MUCH MISSING DATA in THE TIBBLE???? ####
x <- invoice_journal_in %>% summarise_each(funs(100*mean(is.na(.))))
# SALES_ORDER         2.0%
# CASH_DIS_CODE      99.3%
# DELIVERY_TERMS      1.4%
# DIMENSION_ACCOUNT   100%
# ORDER_TYPE2        79.2%
# RETURN_REASON_CODE 90.4%
# TERMS_OF_PAYMENT  0.002%
# WAREHOUSE           2.0%


###  WHAT ROWS NEED TO BE FILTERED OUT OF THE DATA SET?
###  
# 1) EXCLUDE rows where DEPARTMENT is "CONZL" or "ROANZ"
invoice_journal <- filter(invoice_journal_in, ! DEPARTMENT %in% c("CONZL", "ROANZ")) 

# new dimensions
nrow(invoice_journal); ncol(invoice_journal) # 5462 rows  28 cols



### "CUSTOMER_ACCOUNT" ###
#  string integer
# .Machine$integer.max
# [1] 214748364
# can convert all positive 8 char strings to integer. yes.
#  
#  
# Get a count of the rows that fail conversion
filter(invoice_journal, is.na(as.integer(CUSTOMER_ACCOUNT))) %>% nrow # 0. all good)

customer_account_col <- transmute(invoice_journal, customer_account = as.integer(CUSTOMER_ACCOUNT))

summary(customer_account_col)  # this is correct
# customer_account
# Min.   :100115  
# 1st Qu.:100375  
# Median :100446  
# Mean   :101060  
# 3rd Qu.:100661  
# Max.   :106028

# how many distinct customer_account
distinct(customer_account_col, customer_account) %>% nrow # 343


# Top five customer account by number of transactions
(xdf <- group_by(customer_account_col, customer_account) %>% summarise(the_count = n(), log2_count = log2(the_count))) %>% arrange(desc(the_count)) %>% head(10)

# histogram of the count
ggplot(xdf, aes(x=the_count)) + geom_histogram(bins = 40)
ggplot(xdf, aes(x=log2_count)) + geom_histogram(bins = 40)

ggplot(xdf, aes(x=the_count)) + stat_ecdf()
ggplot(xdf, aes(x=log2_count)) + stat_ecdf()

# ECDF function
P <- ecdf(xdf$the_count)
P(19)  # 0.5   50% of customers have 1..19 orders in the period

# QUANTILE IS MORE USEFUL...
quantile(P)
# 0%    25%    50%    75%   100% 
# 1.0    4.0   19.0   66.5 6047.0 
# 
(1.0 - P(6000)) * 100.0 # 0.29% have more than 6000 orders
(1.0 - P(6000)) * nrow(xdf)  # 1

# lets look at more than 2000 orders
(1.0 - P(2000)) * nrow(xdf)  # 6  this is correct

quantile(P, probs = 0.33)  # 7.    33% have between 1..7 orders.
quantile(P, probs = 0.9)   # 236.  99% have between 1..236 orders.

 
### "INVOICE_ACCOUNT" ###
# string integer.  looks like same values as CUSTOMER_ACCOUNT. but the ranges dont match!
#  
# Q: is there a 1:1 relationship between INVOICE_ACCOUNT and CUSTOMER_ACCOUNT

# Get a count of the rows that fail conversion
filter(invoice_journal, is.na(as.integer(INVOICE_ACCOUNT))) %>% nrow # 0. all good

invoice_account_col <- transmute(invoice_journal, invoice_account = as.integer(INVOICE_ACCOUNT))
summary(invoice_account_col)
# customer_account
# Min.   :100068  
# 1st Qu.:100375  
# Median :100446  
# Mean   :101060  
# 3rd Qu.:100661  
# Max.   :106028


filter(invoice_journal, as.integer(INVOICE_ACCOUNT) != as.integer(CUSTOMER_ACCOUNT)) %>% dplyr::summarise(the_count = n()) # 2189 rows dont match

# have a closer look at the relationship. is it M:1  INVOICE_ACCOUNT:CUSTOMER_ACCOUNT
by_custacct <- group_by(invoice_journal, INVOICE_ACCOUNT)
dplyr::summarise(by_custacct, the_count = n_distinct(CUSTOMER_ACCOUNT)) %>% arrange(desc(the_count)) %>% head(10) # only 6 cases
# INVOICE_ACCOUNT the_count
# <chr>     <int>
# 1           100188         8
# 2           100600         5
# 3           100137         4
# 4           100558         2
# 5           100575         2
# 6           104337         2

by_custacct <- group_by(invoice_journal, CUSTOMER_ACCOUNT)
dplyr::summarise(by_custacct, the_count = n_distinct(INVOICE_ACCOUNT)) %>% arrange(desc(the_count)) %>% head(10) # only 4 cases
# CUSTOMER_ACCOUNT the_count
# <int>     <int>
# 1            100171         2
# 2            100451         2
# 3            100536         2
# 4            105633         2



### "SALES_ORDER" ###
# test for structure SOnnnnnnnn.  contains NA's
# extract the numeric part and test convert to integer  

# count the NA
filter(invoice_journal, is.na(SALES_ORDER)) %>% nrow  # 991

# investigate the SALES_ORDER prefix.  check that SO is the only prefix
distinct(invoice_journal, prefix = str_sub(SALES_ORDER,1,2)) # NA, "SO"

# Get a count of the rows that would fail pattern match
filter(invoice_journal, !str_detect(SALES_ORDER, '^SO\\d{8}$')) %>% nrow # 0.  no failures

# test that all remaining digits will convert to integer
filter(invoice_journal, is.na(as.integer(str_sub(SALES_ORDER, 3)))) %>% nrow # 991 NAs

# strip out the 2 char to prefix and number string to an integer.  add extra sales_id col to simplify join with invoice_trans
sales_order_col <- transmute(invoice_journal, sales_order_prefix = str_sub(SALES_ORDER,1,2), sales_order = as.integer(str_sub(SALES_ORDER, 3)), sales_id = as.integer(str_sub(SALES_ORDER, 3)))

str(sales_order_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	49428 obs. of  2 variables:
# $ sales_order_prefix: chr  NA NA NA NA ...
# $ sales_order       : int  NA NA NA NA NA 81529 78167 NA NA NA ..

summary(sales_order_col)
# sales_order_prefix  sales_order      
# Length:49428       Min.   :   45988  
# Class :character   1st Qu.:   96716  
# Mode  :character   Median :  119456  
#                    Mean   :  710206  
#                    3rd Qu.:  144604  
#                    Max.   :90003867  
#                    NA's   :991


### "INVOICE_DATE"  ###
# example 2015-05-12   yyyy-mm-dd format.  use the lubridate package to convert to Date type
# x <- as.Date("2015-05-12")

# How many NA?
filter(invoice_journal, is.na(INVOICE_DATE)) %>% nrow # No NAs

# Test that all rows can be converted to Date
filter(invoice_journal, is.na(as.Date(INVOICE_DATE))) %>% nrow # 0 fail.

# new version using a Date type.  at the end use cbind to create a new dataframe/tibble
invoice_date_col <- transmute(invoice_journal, invoice_date = as.Date(INVOICE_DATE))
                  
                    
### "INVOICE_NUMBER" ###
# e.g. SCN0010751
# looks like format aaannnnnnn   3 char alphabetic prefix factor with 7 numeric
# separate into 2 separate fields
# 
filter(invoice_journal, !str_detect(invoice_journal$INVOICE_NUMBER, '^[A-Z]{3}\\d{7}')) %>% nrow # 0 failures

# investigate the prefix
distinct(invoice_journal, prefix = str_sub(INVOICE_NUMBER,1,3)) # CSI, SCN

# test that all remaining digits will convert to integer
filter(invoice_journal, is.na(as.integer(str_sub(INVOICE_NUMBER, 4)))) %>% nrow()  # 0 failures

# strip out the 3 char to invoice type factor and convert number to an integer
invoice_number_col <- transmute(invoice_journal, invoice_prefix = as.factor(str_sub(INVOICE_NUMBER,1,3)), invoice_number = as.integer(str_sub(INVOICE_NUMBER, 4)))

str(invoice_number_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	49428 obs. of  2 variables:
# $ invoice_prefix: Factor w/ 2 levels "CSI","SCN": 2 2 2 2 2 2 2 2 2 2 ...
# $ invoice_number: int  11153 11154 11155 11156 11157 11158 11159 11160 11161 11162 ...

summary(invoice_number_col)
# invoice_prefix invoice_number  
# CSI:43453      Min.   : 10526  
# SCN: 5975      1st Qu.:100176  
#                Median :125882  
#                Mean   :118287  
#                3rd Qu.:151293  
#                Max.   :179888


### "VOUCHER" ###
# e.g. FCV0001245   aaannnnnnn  3 char alphabetic  voucher_type with 7 numeric
# separate into 2 separate fields
# 
filter(invoice_journal, !str_detect(invoice_journal$VOUCHER, '^[A-Z]{3}\\d{7}')) %>% nrow # 0 failures

# investigate the prefix
distinct(invoice_journal, prefix = str_sub(VOUCHER,1,3)) # FCV, SCV, CIV, FTI

# test that all remaining digits will convert to integer
filter(invoice_journal, is.na(as.integer(str_sub(VOUCHER, 4)))) %>% nrow()  # 0 failures

# strip out the 3 char to invoice type factor and convert number to an integer
voucher_col <- transmute(invoice_journal, voucher_prefix = as.factor(str_sub(VOUCHER,1,3)), voucher = as.integer(str_sub(VOUCHER, 4)))

str(voucher_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	49428 obs. of  2 variables:
# $ voucher_prefix: Factor w/ 4 levels "CIV","FCV","FTI",..: 2 2 2 2 2 4 4 2 2 2 ...
# $ voucher       : int  1245 1246 1247 1248 1249 9909 9910 1250 1251 1252 ...

summary(voucher_col)
# voucher_prefix    voucher      
# CIV:43273      Min.   :   220  
# FCV:  811      1st Qu.: 99599  
# FTI:  180      Median :125367  
# SCV: 5164      Mean   :117125  
#                3rd Qu.:150874  
#                Max.   :179480


### "INVOICE_AMOUNT" ###
# -258.72, 0.00, 122.40.  numeric...but have negative, zero, positive!
# 
# Get a count of the rows that fail conversion
filter(invoice_journal, is.na(as.numeric(INVOICE_AMOUNT))) %>% nrow # 0 fail.

invoice_amount_col <- transmute(invoice_journal, invoice_amount = as.numeric(INVOICE_AMOUNT), credit = invoice_amount < 0.0)
str(invoice_amount_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	49428 obs. of  2 variables:
# $ invoice_amount: num  -66 -11.5 -289.8 -1650 -0.24 ...
# $ credit        : logi  TRUE TRUE TRUE TRUE TRUE TRUE ...

summary(invoice_amount_col)
# invoice_amount        credit       
# Min.   :-534805.3   Mode :logical  
# 1st Qu.:     37.9   FALSE:43501    
# Median :    140.4   TRUE :5927     
# Mean   :    866.4   NA's :0        
# 3rd Qu.:    592.3                  
# Max.   : 534805.3



### "CASH_DIS_VALUE" ###
# links to CASH_DIS_CODE
# # numeric value. expect that 0.0 linked to CASH_DIS_CODE NA. What is meaning of +/- value?
#
# Get a count of the rows that fail conversion
filter(invoice_journal, is.na(as.numeric(CASH_DIS_VALUE))) %>% nrow # 0 fail.

cash_dis_value_col <- transmute(invoice_journal, cash_dis_val = as.numeric(CASH_DIS_VALUE))
str(cash_dis_value_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	49428 obs. of  1 variable:
#   $ cash_dis_val: num  0 0 0 0 0 0 0 0 0 0 ...

summary(cash_dis_value_col)
# cash_dis_val      
# Min.   :-136.6200  
# 1st Qu.:   0.0000  
# Median :   0.0000  
# Mean   :   0.4174  
# 3rd Qu.:   0.0000  
# Max.   :1418.5000


### "CASH_DIS_CODE" ###
#
# String factor:  NA, 21D_5%  and 21D_2.5%
3
# what are the distinct CASH_DIS_CODE values?
distinct(invoice_journal, CASH_DIS_CODE)  # NA, 21D_5%  and 21D_2.5%

cash_dis_code_col <- transmute(invoice_journal, cash_dis_code = as.factor(CASH_DIS_CODE))
str(cash_dis_code_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	49428 obs. of  1 variable:
# $ as.factor(CASH_DIS_CODE): Factor w/ 2 levels "21D_2.5%","21D_5%": NA NA NA NA NA NA NA NA NA NA ...

summary(cash_dis_code_col)
# cash_dis_code  
# 21D_2.5%:   18  
# 21D_5%  :  344  
# NA's    :49066


### "DELIVERY_TERMS" ###          
#
# String factor e.g. CC, PI, PP, NA
#
# what are the distinct DELIVERY_TERMS values?
distinct(invoice_journal, DELIVERY_TERMS)

delivery_terms_col <- transmute(invoice_journal, delivery_terms = as.factor(DELIVERY_TERMS))
str(delivery_terms_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	49428 obs. of  1 variable:
# $ as.factor(DELIVERY_TERMS): Factor w/ 3 levels "CC","PI","PP": 2 2 2 2 2 2 2 2 2 2 ...

summary(delivery_terms_col)
# delivery_terms
# CC  :   11    
# PI  :47094    
# PP  : 1622    
# NA's:  701


### "DEPARTMENT" ###
# string factor :  only include : CONZL, ROANZ. already filtered
# 
department_col <- transmute(invoice_journal, department=as.factor(str_to_lower(DEPARTMENT))) # convert to a factor

str(department_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	5462 obs. of  1 variable:
#   $ department: Factor w/ 2 levels "CONZL","ROANZ": 1 1 1 1 1 1 1 1 1 1 ...

summary(department_col)
# department  
# CONZL:5458  
# ROANZ:   4 



### "DIMENSION" ###
# looks like an integer64 string e.g 5637147333
#
# convert to a factor for now with 123 levels..
#distinct(invoice_journal,DIMENSION) %>% nrow # 123

#dimension_col <- transmute(invoice_journal, dimension = as.factor(DIMENSION))
# str(dimension_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	49428 obs. of  1 variable:
# $ dimension: Factor w/ 123 levels "5637145327","5637147333",..: 2 3 3 2 8 8 2 13 13 22 ...

# summary(dimension_col)
# dimension    
# 5637150092: 7656  
# 5637145327: 6700  
# 5637213326: 4372  
# 5637147333: 3259  
# 5637215841: 2658  
# 5637215842: 2535  
# (Other)   :22248 


### "DIMENSION_ACCOUNT" ###
# all NA.  no change
# 
# distinct(invoice_journal,DIMENSION_ACCOUNT) %>% nrow #  all NA

# dimension_account_col <- select(invoice_journal, dimension_account = DIMENSION_ACCOUNT)
# str(dimension_account_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	49428 obs. of  1 variable:
# $ DIMENSION_ACCOUNT: chr  NA NA NA NA ...


### "LINE_DISCOUNT" ###
# numeric string includes 0.00 values.  some massive outliers -321,000.80 and 78,914.60
# 
# Get a count of the rows that fail conversion
filter(invoice_journal, is.na(as.numeric(LINE_DISCOUNT))) %>% nrow # 0 fail.

line_discount_col <- transmute(invoice_journal, line_discount = as.numeric(LINE_DISCOUNT))
summary(line_discount_col)
# line_discount      
# Min.   :-321000.8  
# 1st Qu.:      0.0  
# Median :      2.2  
# Mean   :    111.6  
# 3rd Qu.:     50.0  
# Max.   :  78914.6 


# "ORDER_TYPE"              
# string factor eg Sales Order: Journal, Return Order, Sales Order.   No NA.
# 
# investigate the values
distinct(invoice_journal, ORDER_TYPE) %>% nrow # 3
xdf <- filter(invoice_journal, ORDER_TYPE == "Journal")

order_type_col <- transmute(invoice_journal, order_type=as.factor(ORDER_TYPE))
str(order_type_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	49428 obs. of  1 variable:
#   $ order_type: Factor w/ 3 levels "Journal","Returned order",..: 1 1 1 1 1 2 2 1 1 1 ...

summary(order_type_col)
# order_type  
# Journal       :  41  
# Returned order: 324  
# Sales order   :5097



### "ORDERTYPE2" ###
# string factoR:  AtOnce, EOL, Indent, Promo, NA
# 
# investigate the values
distinct(invoice_journal, ORDERTYPE2) %>% nrow # 5

ordertype2_col <- transmute(invoice_journal, ordertype2=as.factor(ORDERTYPE2))
str(ordertype2_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	49428 obs. of  1 variable:
#   $ ordertype2: Factor w/ 4 levels "AtOnce","EOL",..: NA NA NA NA NA NA NA NA NA NA ...

summary(ordertype2_col)
# ordertype2   
# AtOnce: 9992  
# EOL   :   27  
# Indent:  125  
# Promo :  157  
# NA's  :39127


### "RETURN_REASON_CODE" ###
# string factor: 51 levels including NA
# 
# investigate the values
distinct(invoice_journal, RETURN_REASON_CODE) %>% nrow # 52

return_reason_code_col <- transmute(invoice_journal, return_reason_code=as.factor(str_to_lower(RETURN_REASON_CODE)))
str(return_reason_code_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	49428 obs. of  1 variable:
#   $ return_reason_code: Factor w/ 51 levels "CANCEL","CARRIER",..: NA NA NA NA NA 46 21 NA NA NA ...

summary(ordertype2_col)
# ordertype2   
# AtOnce: 9992  
# EOL   :   27  
# Indent:  125  
# Promo :  157  
# NA's  :39127  


### "SALES_SUBTOTAL_AMOUNT" ###
# numeric string  with major +/- outlier. significant number of 0.00 and -ve amounts
# 
# Get a count of the rows that fail conversion
filter(invoice_journal, is.na(as.numeric(SALES_SUBTOTAL_AMOUNT))) %>% nrow # 0. all good

sales_subtotal_amount_col <- transmute(invoice_journal, sales_subtotal_amount = as.numeric(SALES_SUBTOTAL_AMOUNT))
summary(sales_subtotal_amount_col)
# sales_subtotal_amount
# Min.   :-465048.0    
# 1st Qu.:     27.5    
# Median :    117.4    
# Mean   :    757.7    
# 3rd Qu.:    515.0    
# Max.   : 465048.0
# 
# 
# How many negative values?
filter(sales_subtotal_amount_col, sales_subtotal_amount < 0.0) %>% nrow  # 5927

# Any zero values?
filter(sales_subtotal_amount_col, sales_subtotal_amount == 0.0) %>% nrow # 451


# "SALES_TAKER"
# integer64 string 5637145600 with 0's.  No NA.
# 
# lets keep this a string factor for now...with 13 levels
# 
#distinct(invoice_journal, SALES_TAKER) %>% nrow #13

#sales_taker_col <- transmute(invoice_journal, sales_taker = as.factor(SALES_TAKER))
#str(sales_taker_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	49428 obs. of  1 variable:
# $ sales_taker: Factor w/ 13 levels "0","5637144826",..: 5 2 2 1 5 7 1 8 8 1 ...

#summary(sales_taker_col)
# sales_taker   
# 0         :45194  
# 5637145600: 1362  
# 5637144826:  774  
# 5637145609:  623  
# 5637153826:  588  
# 5637157076:  516  
# (Other)   :  371  


### "TERMS_OF_PAYMENT" ###
# string factor 18 levels:  CM+2M  including a single NA outlier

# investigate the values
distinct(invoice_journal, TERMS_OF_PAYMENT) %>% nrow # 18

terms_of_payment_col <- transmute(invoice_journal, terms_of_payment=as.factor(TERMS_OF_PAYMENT))
str(terms_of_payment_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	49428 obs. of  1 variable:
#   $ terms_of_payment: Factor w/ 17 levels "CM+1M(10)","CM+1M+20D",..: 17 4 4 8 17 17 8 4 4 8 ...

summary(terms_of_payment_col)
# terms_of_payment
# CM+20D   :34699  
# CM+30D   : 5092  
# NET0     : 2347  
# CM+1M+20D: 1725  
# N60      : 1455  
# (Other)  : 4109  
# NA's     :    1 


### "INVOICE_AMOUNT_AC" ###
# numeric string with large +/- outlier
# 
# Get a count of the rows that fail conversion
filter(invoice_journal, is.na(as.numeric(INVOICE_AMOUNT_AC))) %>% nrow # 0. all good

invoice_amount_ac_col <- transmute(invoice_journal, invoice_amount_ac = as.numeric(INVOICE_AMOUNT_AC))
summary(invoice_amount_ac_col)
# invoice_amount_ac  
# Min.   :-534805.3  
# 1st Qu.:     37.9  
# Median :    140.6  
# Mean   :    882.7  
# 3rd Qu.:    606.0  
# Max.   : 534805.3

# 
# How many negative values?
filter(invoice_amount_ac_col, invoice_amount_ac < 0.0) %>% nrow  # 5927

# Any zero values?
filter(invoice_amount_ac_col, invoice_amount_ac == 0.0) %>% nrow # 399


# "LINE_DISCOUNT_AC"
# numeric string 
# 
# Get a count of the rows that fail conversion
filter(invoice_journal, is.na(as.numeric(LINE_DISCOUNT_AC))) %>% nrow # 0. all good

line_discount_ac_col <- transmute(invoice_journal, line_discount_ac = as.numeric(LINE_DISCOUNT_AC))
summary(line_discount_ac_col)
# line_discount_ac   
# Min.   :-321000.8  
# 1st Qu.:      0.0  
# Median :      2.2  
# Mean   :    113.6  
# 3rd Qu.:     50.0  
# Max.   :  78914.6

# How many negative values?
filter(line_discount_ac_col, line_discount_ac < 0.0) %>% nrow  # 2032

# Any zero values?
filter(line_discount_ac_col, line_discount_ac == 0.0) %>% nrow # 20639


# "SALES_SUBTOTAL_AMOUNT_AC"
# Get a count of the rows that fail conversion
filter(invoice_journal, is.na(as.numeric(SALES_SUBTOTAL_AMOUNT_AC))) %>% nrow # 0. all good

sales_subtotal_amount_ac_col <- transmute(invoice_journal, sales_subtotal_amount_ac = as.numeric(SALES_SUBTOTAL_AMOUNT_AC))
summary(sales_subtotal_amount_ac_col)
# sales_subtotal_amount_ac
# Min.   :-465048.0       
# 1st Qu.:     27.5       
# Median :    119.6       
# Mean   :    772.5       
# 3rd Qu.:    527.1       
# Max.   : 465048.0

# How many negative values?
filter(sales_subtotal_amount_ac_col, sales_subtotal_amount_ac < 0.0) %>% nrow  # 5927

# Any zero values?
filter(sales_subtotal_amount_ac_col, sales_subtotal_amount_ac == 0.0) %>% nrow # 451

# Any NAs?
filter(sales_subtotal_amount_ac_col, is.na(sales_subtotal_amount_ac)) %>% nrow # No NA



# "SALES_TAX_AC"
# Get a count of the rows that fail conversion
filter(invoice_journal, is.na(as.numeric(SALES_TAX_AC))) %>% nrow # 0. all good

sales_tax_ac_col <- transmute(invoice_journal, sales_tax_ac = as.numeric(SALES_TAX_AC))
summary(sales_tax_ac_col)
# sales_tax_ac      
# Min.   :-69757.21  
# 1st Qu.:     4.95  
# Median :    17.61  
# Mean   :   105.76  
# 3rd Qu.:    70.81  
# Max.   : 69757.21

# How many negative values?
filter(sales_tax_ac_col, sales_tax_ac < 0.0) %>% nrow  # 5842

# Any zero values?
filter(sales_tax_ac_col, sales_tax_ac == 0.0) %>% nrow # 578

# Any NAs?
filter(sales_tax_ac_col, is.na(sales_tax_ac)) %>% nrow # No NA


### "CHARGES_AC" ### 
# Numeric string             
# Get a count of the rows that fail conversion
filter(invoice_journal, is.na(as.numeric(CHARGES_AC))) %>% nrow # 0. all good

charges_ac_col <- transmute(invoice_journal, charges_ac = as.numeric(CHARGES_AC))
summary(charges_ac_col)
# charges_ac      
# Min.   :-190.000  
# 1st Qu.:   0.000  
# Median :   0.000  
# Mean   :   4.453  
# 3rd Qu.:   8.000  
# Max.   :1346.320

# How many negative values?
filter(charges_ac_col, charges_ac < 0.0) %>% nrow  # 137

# Any zero values?
filter(charges_ac_col, charges_ac == 0.0) %>% nrow # 29033

# Any NAs?
filter(charges_ac_col, is.na(charges_ac)) %>% nrow # No NA


# "CURRENCY"
# string factor:  AUD, NZD
# 
# investigate the values
distinct(invoice_journal, CURRENCY)

currency_col <- transmute(invoice_journal, currency=as.factor(CURRENCY))
str(currency_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	49428 obs. of  1 variable:
#   $ terms_of_payment: Factor w/ 2 levels "AUD","NZD": 1 2 2 1 1 1 1 2 2 1 ...

summary(currency_col)
# terms_of_payment
# AUD: 7644       
# NZD:41784 

 
# "CHARGES"
# Numeric string             
# Get a count of the rows that fail conversion
filter(invoice_journal, is.na(as.numeric(CHARGES))) %>% nrow # 0. all good

charges_col <- transmute(invoice_journal, charges = as.numeric(CHARGES))
summary(charges_col)
# charges        
# Min.   :-190.000  
# 1st Qu.:   0.000  
# Median :   0.000  
# Mean   :   4.354  
# 3rd Qu.:   8.000  
# Max.   :1223.000

# How many negative values?
filter(charges_col, charges < 0.0) %>% nrow  # 137

# Any zero values?
filter(charges_col, charges == 0.0) %>% nrow # 29033

# Any NAs?
filter(charges_col, is.na(charges)) %>% nrow # No NA


# "WAREHOUSE"
# string factor: 20 level including NA eg NZ_OF
# 
# investigate the values
distinct(invoice_journal, WAREHOUSE) %>% nrow #20

warehouse_col <- transmute(invoice_journal, warehouse=as.factor(WAREHOUSE))
str(warehouse_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	49428 obs. of  1 variable:
#   $ warehouse: Factor w/ 19 levels "ANZ_DISCRP","ANZ_DS",..: NA NA NA NA NA 5 5 NA NA NA ...

summary(warehouse_col)
# warehouse    
# ANZ_MWH   :42924  
# NZ_OF     : 3529  
# AUMEL_OF  :  724  
# ANZ_DISCRP:  418  
# ANZ_DS    :  289  
# (Other)   :  551  
# NA's      :  993 


### CREATE THE NEW INVOICE_TRANS DATA FRAME
#[1] "CUSTOMER_ACCOUNT"         "INVOICE_ACCOUNT"          "SALES_ORDER"              "INVOICE_DATE"             "INVOICE_NUMBER"           "VOUCHER"                 
#[7] "INVOICE_AMOUNT"           "CASH_DIS_VALUE"           "CASH_DIS_CODE"            "DELIVERY_TERMS"           "DEPARTMENT"               "DIMENSION"               
#[13] "DIMENSION_ACCOUNT"        "LINE_DISCOUNT"            "ORDER_TYPE"               "ORDERTYPE2"               "RETURN_REASON_CODE"       "SALES_SUBTOTAL_AMOUNT"   
#[19] "SALES_TAKER"              "TERMS_OF_PAYMENT"         "INVOICE_AMOUNT_AC"        "LINE_DISCOUNT_AC"         "SALES_SUBTOTAL_AMOUNT_AC" "SALES_TAX_AC"            
#[25] "CHARGES_AC"               "CURRENCY"                 "CHARGES"                  "WAREHOUSE"

xdf <- cbind(  # bind these in the order that makes the most sense for downstream processing
    invoice_date_col,
    invoice_number_col,
    customer_account_col,
    invoice_account_col,
    sales_order_col,
    voucher_col,
    invoice_amount_col,
    cash_dis_value_col,
    cash_dis_code_col,
    delivery_terms_col,
    department_col,
    # dimension_col,          no useful data
    # dimension_account_col,  no useful data
    line_discount_col,
    order_type_col,
    ordertype2_col,
    return_reason_code_col,
    sales_subtotal_amount_col,
    # sales_taker_col,        no useful data
    terms_of_payment_col,
    invoice_amount_ac_col,
    line_discount_ac_col,
    sales_subtotal_amount_ac_col,
    sales_tax_ac_col,
    charges_ac_col,
    currency_col,
    charges_col,
    warehouse_col) %>% arrange( desc(invoice_number) )  # sort based on the invoice number. most recent first...to help with joins

if (interactive()) View(xdf)

str(xdf)
# 'data.frame':	49428 obs. of  32 variables:
# $ customer_account        : int  100171 100615 100524 100137 100161 100161 104886 100393 100393 100188 ...
# $ invoice_account         : int  100171 100615 100524 100137 100161 100161 104886 100393 100393 100188 ...
# $ sales_order_prefix      : chr  NA NA NA NA ...
# $ sales_order             : int  NA NA NA NA NA 81529 78167 NA NA NA ...
# $ invoice_date            : Date, format: "2015-05-12" "2015-05-12" "2015-05-12" "2015-05-12" ...
# $ invoice_prefix          : Factor w/ 2 levels "CSI","SCN": 2 2 2 2 2 2 2 2 2 2 ...
# $ invoice_number          : int  11153 11154 11155 11156 11157 11158 11159 11160 11161 11162 ...
# $ voucher_prefix          : Factor w/ 4 levels "CIV","FCV","FTI",..: 2 2 2 2 2 4 4 2 2 2 ...
# $ voucher                 : int  1245 1246 1247 1248 1249 9909 9910 1250 1251 1252 ...
# $ invoice_amount          : num  -66 -11.5 -289.8 -1650 -0.24 ...
# $ credit                  : logi  TRUE TRUE TRUE TRUE TRUE TRUE ...
# $ cash_dis_val            : num  0 0 0 0 0 0 0 0 0 0 ...
# $ cash_dis_code           : Factor w/ 2 levels "21D_2.5%","21D_5%": NA NA NA NA NA NA NA NA NA NA ...
# $ delivery_terms          : Factor w/ 3 levels "CC","PI","PP": 2 2 2 2 2 2 2 2 2 2 ...
# $ department              : Factor w/ 4 levels "CONZL","ROANZ",..: 3 4 4 3 3 3 3 4 4 3 ...
# $ dimension               : Factor w/ 123 levels "5637145327","5637147333",..: 2 3 3 2 8 8 2 13 13 22 ...
# $ dimension_account       : chr  NA NA NA NA ...
# $ line_discount           : num  0 0 0 0 0 0 0 0 0 0 ...
# $ order_type              : Factor w/ 3 levels "Journal","Returned order",..: 1 1 1 1 1 2 2 1 1 1 ...
# $ ordertype2              : Factor w/ 4 levels "AtOnce","EOL",..: NA NA NA NA NA NA NA NA NA NA ...
# $ return_reason_code      : Factor w/ 51 levels "CANCEL","CARRIER",..: NA NA NA NA NA 46 21 NA NA NA ...
# $ sales_subtotal_amount   : num  -60 -10 -252 -1500 -0.22 ...
# $ sales_taker             : Factor w/ 13 levels "0","5637144826",..: 5 2 2 1 5 7 1 8 8 1 ...
# $ terms_of_payment        : Factor w/ 17 levels "CM+1M(10)","CM+1M+20D",..: 17 4 4 8 17 17 8 4 4 8 ...
# $ invoice_amount_ac       : num  -69.33 -11.5 -289.8 -1733.19 -0.25 ...
# $ line_discount_ac        : num  0 0 0 0 0 0 0 0 0 0 ...
# $ sales_subtotal_amount_ac: num  -63.03 -10 -252 -1575.63 -0.23 ...
# $ sales_tax_ac            : num  -6.3 -1.5 -37.8 -157.56 -0.02 ...
# $ charges_ac              : num  0 0 0 0 0 0 0 0 0 0 ...
# $ currency                : Factor w/ 2 levels "AUD","NZD": 1 2 2 1 1 1 1 2 2 1 ...
# $ charges                 : num  0 0 0 0 0 0 0 0 0 0 ...
# $ warehouse               : Factor w/ 19 levels "ANZ_DISCRP","ANZ_DS",..: NA NA NA NA NA 5 5 NA NA NA ...



summary(xdf)
# customer_account invoice_account  sales_order_prefix  sales_order        invoice_date        invoice_prefix invoice_number   voucher_prefix    voucher       invoice_amount     
# Min.   :100068   Min.   :100068   Length:49428       Min.   :   45988   Min.   :2015-04-01   CSI:43453      Min.   : 10526   CIV:43273      Min.   :   220   Min.   :-534805.3  
# 1st Qu.:100375   1st Qu.:100375   Class :character   1st Qu.:   96716   1st Qu.:2015-08-04   SCN: 5975      1st Qu.:100176   FCV:  811      1st Qu.: 99599   1st Qu.:     37.9  
# Median :100446   Median :100446   Mode  :character   Median :  119456   Median :2016-01-28                  Median :125882   FTI:  180      Median :125367   Median :    140.4  
# Mean   :101060   Mean   :101060                      Mean   :  710206   Mean   :2016-01-27                  Mean   :118287   SCV: 5164      Mean   :117125   Mean   :    866.4  
# 3rd Qu.:100661   3rd Qu.:100661                      3rd Qu.:  144604   3rd Qu.:2016-07-11                  3rd Qu.:151293                  3rd Qu.:150874   3rd Qu.:    592.3  
# Max.   :106028   Max.   :106028                      Max.   :90003867   Max.   :2016-12-31                  Max.   :179888                  Max.   :179480   Max.   : 534805.3  
#                                                      NA's   :991
#                                                                                                                                                                      
# credit         cash_dis_val        cash_dis_code   delivery_terms department         dimension     dimension_account  line_discount                order_type     ordertype2   
# Mode :logical   Min.   :-136.6200   21D_2.5%:   18   CC  :   11     CONZL: 5458   5637150092: 7656   Length:49428       Min.   :-321000.8   Journal       :  991   AtOnce: 9992  
# FALSE:43501     1st Qu.:   0.0000   21D_5%  :  344   PI  :47094     ROANZ:    4   5637145327: 6700   Class :character   1st Qu.:      0.0   Returned order: 4768   EOL   :   27  
# TRUE :5927      Median :   0.0000   NA's    :49066   PP  : 1622     WSAUS: 7664   5637213326: 4372   Mode  :character   Median :      2.2   Sales order   :43669   Indent:  125  
# NA's :0         Mean   :   0.4174                    NA's:  701     WSNZL:36302   5637147333: 3259                      Mean   :    111.6                          Promo :  157  
#                 3rd Qu.:   0.0000                                                 5637215841: 2658                      3rd Qu.:     50.0                          NA's  :39127  
#                 Max.   :1418.5000                                                 5637215842: 2535                      Max.   :  78914.6                                        
#                 (Other)   :22248
#                                                                                                
# return_reason_code sales_subtotal_amount     sales_taker     terms_of_payment invoice_amount_ac   line_discount_ac    sales_subtotal_amount_ac  sales_tax_ac      
# FAB_HOLES: 1736    Min.   :-465048.0     0         :45194   CM+20D   :34699   Min.   :-534805.3   Min.   :-321000.8   Min.   :-465048.0        Min.   :-69757.21  
# MGMT     :  727    1st Qu.:     27.5     5637145600: 1362   CM+30D   : 5092   1st Qu.:     37.9   1st Qu.:      0.0   1st Qu.:     27.5        1st Qu.:     4.95  
# SOCKLG   :  342    Median :    117.4     5637144826:  774   NET0     : 2347   Median :    140.6   Median :      2.2   Median :    119.6        Median :    17.61  
# SAMPLE   :  337    Mean   :    757.7     5637145609:  623   CM+1M+20D: 1725   Mean   :    882.7   Mean   :    113.6   Mean   :    772.5        Mean   :   105.76  
# RTEX     :  325    3rd Qu.:    515.0     5637153826:  588   N60      : 1455   3rd Qu.:    606.0   3rd Qu.:     50.0   3rd Qu.:    527.1        3rd Qu.:    70.81  
# (Other)  : 1300    Max.   : 465048.0     5637157076:  516   (Other)  : 4109   Max.   : 534805.3   Max.   :  78914.6   Max.   : 465048.0        Max.   : 69757.21  
# NA's     :44661                          (Other)   :  371   NA's     :    1
#                                                                                        
# charges_ac          currency          charges              warehouse    
# Min.   :-190.000   AUD: 7644        Min.   :-190.000   ANZ_MWH   :42924  
# 1st Qu.:   0.000   NZD:41784        1st Qu.:   0.000   NZ_OF     : 3529  
# Median :   0.000                    Median :   0.000   AUMEL_OF  :  724  
# Mean   :   4.453                    Mean   :   4.354   ANZ_DISCRP:  418  
# 3rd Qu.:   8.000                    3rd Qu.:   8.000   ANZ_DS    :  289  
# Max.   :1346.320                    Max.   :1223.000   (Other)   :  551  
#                                                        NA's      :  993  

# write out the new dataframe as a feather file
write_feather(xdf, "data/invoice_journal.feather")
