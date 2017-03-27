rm(list = ls()) # clear the workspace as a precaution

require(feather)
require(tidyverse)
require(stringr)
require(dplyr)
require(bit64)

# INSTANTIATE tibbles from feather
setwd("C:/Users/rp/Projects/icebreaker_rp")
(invoice_trans <- read_feather("R_CUST_INVOICE_TRANS.feather")) %>% View

# dimensions
nrow(invoice_trans); ncol(invoice_trans)  # 513153 row, 11 col

# column names
colnames(invoice_trans)
# [1] "SALES_ID"                     "DOCUMENT_NUMBER__INVOICE_ID_" "CURRENCY_CURRENCY_CODE"       "ITEM_ID"                     
# [5] "STYLE_NAME"                   "LINE_AMT_PRE_DISCOUNT"        "LINE_AMT_POST_DISCOUNT"       "INVOICED_DISCOUNT_ACY"       
# [9] "COGS"                         "INVOICED_QTY"                 "SALES_PRICE"


### HOW MUCH MISSING DATA in THE TIBBLE???? ####
invoice_trans %>% summarise_each(funs(100*mean(is.na(.)))) # all data intact. No NAs.



# "SALES_ID" 
# string prefixed integer:  SO00117238.  pattern SOnnnnnnnn
# 
# Group_by Aggregate raw SALES_ID to find top/bottom 5 total SALES_ID by transactions
select(invoice_trans, SALES_ID) %>% group_by(SALES_ID) %>% dplyr::summarise(the_count = n()) %>% arrange(desc(the_count)) %>% head(5) # 1..470
# SALES_ID the_count
#         <chr>     <int>
# 1 SO00159567       470
# 2 SO00162649       470
# 3 SO00165971       449
# 4 SO00171378       449
# 5 SO00171379       449

# investigate the SALES_ID prefix.  check that SO is the only prefix
transmute(invoice_trans, prefix = str_sub(SALES_ID,1,2)) %>% distinct() # SO

# Get a count of the rows that would fail pattern match
(fail_count <- filter(invoice_trans, !str_detect(SALES_ID, '^SO\\d{8}$')) %>% nrow()) # 0.  no failures

# test that all remaining digits will convert to integer
(fail_count <- filter(invoice_trans, is.na(as.integer(str_sub(SALES_ID, 3)))) %>% nrow) # 0.  no failures.

# strip out the 2 char to prefix and number string to an integer
sales_id_col <- transmute(invoice_trans, sales_id = as.integer(str_sub(SALES_ID, 3)))

str(sales_id_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	513153 obs. of  1 variable:
#   $ sales_id: int  117238 117238 117238 117241 117241 117241 117241 117241 117241 117241 ...

summary(sales_id_col)
# sales_id       
# Min.   :   45988  
# 1st Qu.:   98660  
# Median :  119125  
# Mean   : 2507654  
# 3rd Qu.:  146429  
# Max.   :90003867

# check that this is correct.  locate "SO00045988" in the raw data. Yes.
# filter(invoice_trans, SALES_ID == "SO00117238") %>% nrow
# filter(invoice_trans, SALES_ID == "SO00045988") %>% View
# filter(invoice_trans, SALES_ID == "SO90003867") %>% View

# Whats the distribution of transaction per SALES_ID?
(xdf <- group_by(sales_id_col, sales_id) %>% dplyr::summarise(the_count = n(), log_count = log10(the_count))) %>% arrange(desc(the_count)) %>% head(10)

# Histogram of the SALES_ID aggregation
ggplot(xdf, aes(the_count)) + geom_histogram(bins = 50)
ggplot(xdf, aes(log_count)) + geom_histogram(bins = 50)  # plot of the log


# Cumulative Density function
ggplot(xdf, aes(the_count)) + stat_ecdf()
ggplot(xdf, aes(log_count)) + stat_ecdf() # makes it clear the 50% mark is 3


P <- ecdf(xdf$the_count)
quantile(P)
# 0%  25%  50%  75% 100% 
# 1    1    3   12  470

quantile(P, probs = 0.95) # 59.  95% of sales_id have 1..59 entries.  ie only 5% have more that 59 entries

# look at 100 entries as a cutoff point
(1.0 - P(100)) * nrow(xdf)  # 720 sales_id have 100+ entries.  high volume.


#"DOCUMENT_NUMBER__INVOICE_ID_" 
# string prefixed factor + integer. 
# e.g CSI0131417  aaannnnnnn

(fail_count <- filter(invoice_trans, !str_detect(DOCUMENT_NUMBER__INVOICE_ID_, '^[A-Z]{3}\\d{7}')) %>% nrow()) # 0.  all good

document_number_invoice_id_col <- transmute(invoice_trans, document_number_prefix = as.factor(str_sub(DOCUMENT_NUMBER__INVOICE_ID_,1,3)), document_number_suffix = str_sub(DOCUMENT_NUMBER__INVOICE_ID_,4))

str(document_number_invoice_id_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	513153 obs. of  2 variables:
# $ document_number_prefix: Factor w/ 2 levels "CSI","SCN": 1 1 1 1 1 1 1 1 1 1 ...
# $ document_number_suffix: chr  "0128464" "0130313" "0132360" "0128357" ..

summary(document_number_invoice_id_col)
# document_number_prefix document_number_suffix
# CSI:421045             Length:513153         
# SCN: 92108             Class :character      
#                        Mode  :character

 
### "CURRENCY_CURRENCY_CODE" ### 
# string:  only 1 value "NZD"
# 
distinct(invoice_trans, CURRENCY_CURRENCY_CODE)
currency_currency_code <- select(invoice_trans, currency_code = CURRENCY_CURRENCY_CODE)


### "ITEM_ID" ###
# Variety of formats! eg 1B6B41, 101328 etc.  too hard to sensibly separate.  
# convert strings to factors with 1535 level.
# 
# How many distinct ITEM_ID?
select(invoice_trans, ITEM_ID) %>% distinct(ITEM_ID) %>% nrow  # 1535

item_id_col <- transmute(invoice_trans, item_id = as.factor(ITEM_ID))
str(item_id_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	513153 obs. of  1 variable:
#   $ item_id: Factor w/ 1535 levels "100003","100006",..: 557 126 553 58 119 126 155 189 269 306 ...
#   
summary(item_id_col)
# item_id      
# 100476 :  5763  
# 100514 :  4906  
# IBM200 :  3893  
# IBN327 :  3885  
# 100871 :  3840  
# 100471 :  3711  
# (Other):487155

# closer look at the distribution of item_ids...

# Aggregate group_by to see distribution on style_name
(xdf <- group_by(item_id_col, item_id) %>% dplyr::summarise(the_count = n(), log_count = log10(the_count))) %>% arrange(desc(the_count)) %>% head(10)

# Histogram of the SALES_ID aggregation
ggplot(xdf, aes(the_count)) + geom_histogram(bins = 50)
ggplot(xdf, aes(log_count)) + geom_histogram(bins = 50)  # plot of the log


# Cumulative Density function
ggplot(xdf, aes(the_count)) + stat_ecdf()
ggplot(xdf, aes(log_count)) + stat_ecdf() # makes it clear the 50% mark is 3


#"STYLE_NAME"                   
# string description (not a factor)
# 
# How many distinct descriptions?
distinct(invoice_trans, STYLE_NAME) %>% nrow # 1245

# Create the new column
style_name_col <- select(invoice_trans, stlye_name = STYLE_NAME)


#"LINE_AMT_PRE_DISCOUNT"        
# string numeric
# Get a count of the rows that fail conversion
(fail_count <-invoice_trans %>% filter( is.na(as.numeric(LINE_AMT_PRE_DISCOUNT))) %>% nrow()) # 0. all good

line_amt_pre_discount_col <- transmute(invoice_trans, line_amt_pre_discount = as.numeric(LINE_AMT_PRE_DISCOUNT))
str(line_amt_pre_discount_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	513153 obs. of  1 variable:
#   $ line_amt_pre_discount: num  137 48.9 137 195.7 39.1 ...

summary(line_amt_pre_discount_col)
# line_amt_pre_discount
# Min.   :-56043.21    
# 1st Qu.:    17.12    
# Median :    58.70    
# Mean   :    82.54    
# 3rd Qu.:   146.74    
# Max.   : 62560.45

# How many negative values?
filter(line_amt_pre_discount_col, line_amt_pre_discount < 0.0) %>% nrow  # 91990

# Any zero values?
filter(line_amt_pre_discount_col, line_amt_pre_discount == 0.0) %>% nrow # 2821

# Any NAs?
filter(line_amt_pre_discount_col, is.na(line_amt_pre_discount)) %>% nrow # 0




#"LINE_AMT_POST_DISCOUNT"       
# string numeric big +/- outlier
# 
# Get a count of the rows that fail conversion
(fail_count <-invoice_trans %>% filter( is.na(as.numeric(LINE_AMT_POST_DISCOUNT))) %>% nrow()) # 0. all good

line_amt_post_discount_col <- transmute(invoice_trans, line_amt_post_discount = as.numeric(LINE_AMT_POST_DISCOUNT))
str(line_amt_post_discount_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	513153 obs. of  1 variable:
#   $ line_amt_post_discount: num  137 48.9 137 195.7 39.1 ...

summary(line_amt_post_discount_col)
# line_amt_post_discount
# Min.   :-43876.77     
# 1st Qu.:    14.15     
# Median :    48.91     
# Mean   :    72.33     
# 3rd Qu.:   136.96     
# Max.   : 43876.77

# How many negative values?
filter(line_amt_post_discount_col, line_amt_post_discount < 0.0) %>% nrow  # 86734

# Any zero values?
filter(line_amt_post_discount_col, line_amt_post_discount == 0.0) %>% nrow # 18993

# Any NAs?
filter(line_amt_post_discount_col, is.na(line_amt_post_discount)) %>% nrow # 0


### "INVOICED_DISCOUNT_ACY" ###      
# string numeric
# 
# Get a count of the rows that fail conversion
(fail_count <-invoice_trans %>% filter( is.na(as.numeric(INVOICED_DISCOUNT_ACY))) %>% nrow()) # 0. all good

invoiced_discount_acy_col <- transmute(invoice_trans, invoiced_discount_acy = as.numeric(INVOICED_DISCOUNT_ACY))
str(invoiced_discount_acy_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	513153 obs. of  1 variable:
#   $ invoiced_discount_acy: num  0 0 0 0 0 0 0 0 0 0 ...

summary(invoiced_discount_acy_col)
# invoiced_discount_acy
# Min.   :-23832.20    
# 1st Qu.:     0.00    
# Median :     0.00    
# Mean   :    10.21    
# 3rd Qu.:     2.35    
# Max.   : 26588.19

# How many negative values?
filter(invoiced_discount_acy_col, invoiced_discount_acy < 0.0) %>% nrow  # 17744

# Any zero values?
filter(invoiced_discount_acy_col, invoiced_discount_acy == 0.0) %>% nrow # 286694

# Any NAs?
filter(invoiced_discount_acy_col, is.na(invoiced_discount_acy)) %>% nrow # 0

 
### "COGS" ###                         
# string numeric
# 
# Get a count of the rows that fail conversion
(fail_count <-invoice_trans %>% filter( is.na(as.numeric(COGS))) %>% nrow()) # 0. all good

cogs_col <- transmute(invoice_trans, cogs = as.numeric(COGS))
str(cogs_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	513153 obs. of  1 variable:
#   $ cogs: num  58.9 23.4 46 91.6 17.1 ..

summary(cogs_col)
# cogs          
# Min.   :-38204.90  
# 1st Qu.:     7.04  
# Median :    23.16  
# Mean   :    36.10  
# 3rd Qu.:    65.96  
# Max.   : 38204.90

# How many negative values?
filter(cogs_col, cogs < 0.0) %>% nrow  # 91744

# Any zero values?
filter(cogs_col, cogs == 0.0) %>% nrow # 4032

# Any NAs?
filter(cogs_col, is.na(cogs)) %>% nrow # 0


### "INVOICED_QTY" ###
# string integer: lots of -ve amounts!
# 
# Get a count of the rows that fail conversion
(fail_count <-invoice_trans %>% filter( is.na(as.numeric(INVOICED_QTY))) %>% nrow ) # 0. all good

invoiced_qty_col <- transmute(invoice_trans, invoiced_qty = as.integer(INVOICED_QTY))
str(invoiced_qty_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	513153 obs. of  1 variable:
#   $ invoiced_qty: int  1 1 1 2 1 1 1 2 1 1 ...

summary(invoiced_qty_col)
# invoiced_qty     
# Min.   :-830.000  
# 1st Qu.:   1.000  
# Median :   1.000  
# Mean   :   1.695  
# 3rd Qu.:   3.000  
# Max.   :1600.000

# How many negative values?
filter(invoiced_qty_col, invoiced_qty < 0) %>% nrow  # 92106

# Any zero values?
filter(invoiced_qty_col, invoiced_qty == 0) %>% nrow # 0

# Any NAs?
filter(invoiced_qty_col, is.na(invoiced_qty)) %>% nrow # 0



### "SALES_PRICE" ###
# string numeric
# 
# Get a count of the rows that fail conversion
(fail_count <-invoice_trans %>% filter( is.na(as.numeric(SALES_PRICE))) %>% nrow ) # 0. all good

sales_price_col <- transmute(invoice_trans, sales_price = as.numeric(SALES_PRICE))
str(sales_price_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	513153 obs. of  1 variable:
#   $ sales_price: num  137 48.9 137 97.8 39.1 ...

summary(sales_price_col)
# sales_price     
# Min.   :   0.00  
# 1st Qu.:  20.81  
# Median :  47.03  
# Mean   :  58.08  
# 3rd Qu.:  72.26  
# Max.   :3000.00

# How many negative values?
filter(sales_price_col, sales_price < 0.0) %>% nrow  # 0

# Any zero values?
filter(sales_price_col, sales_price == 0.0) %>% nrow # 2821

# Any NAs?
filter(sales_price_col, is.na(sales_price)) %>% nrow # 0


### CREATE THE NEW INVOICE_TRANS DATA FRAME
# [1] "SALES_ID"                     "DOCUMENT_NUMBER__INVOICE_ID_" "CURRENCY_CURRENCY_CODE"       "ITEM_ID"                     
# [5] "STYLE_NAME"                   "LINE_AMT_PRE_DISCOUNT"        "LINE_AMT_POST_DISCOUNT"       "INVOICED_DISCOUNT_ACY"       
# [9] "COGS"                         "INVOICED_QTY"                 "SALES_PRICE"
# 
# 
xdf <- cbind(
sales_id_col,
document_number_invoice_id_col,
currency_currency_code,
item_id_col,
style_name_col,
line_amt_pre_discount_col,
line_amt_post_discount_col,
invoiced_discount_acy_col,
cogs_col,
invoiced_qty_col,
sales_price_col)

View(xdf)

str(xdf)
# 'data.frame':	513153 obs. of  12 variables:
#   $ sales_id              : int  117238 117238 117238 117241 117241 117241 117241 117241 117241 117241 ...
# $ document_number_prefix: Factor w/ 2 levels "CSI","SCN": 1 1 1 1 1 1 1 1 1 1 ...
# $ document_number_suffix: chr  "0128464" "0130313" "0132360" "0128357" ...
# $ CURRENCY_CURRENCY_CODE: chr  "NZD" "NZD" "NZD" "NZD" ...
# $ item_id               : Factor w/ 1535 levels "100003","100006",..: 557 126 553 58 119 126 155 189 269 306 ...
# $ STYLE_NAME            : chr  "Mens Sierra LS Zip" "Mens Oasis LS Crewe" "Wmns Cascade LS Zip" "Wmns Bliss Wrap" ...
# $ line_amt_pre_discount : num  137 48.9 137 195.7 39.1 ...
# $ line_amt_post_discount: num  137 48.9 137 195.7 39.1 ...
# $ invoiced_discount_acy : num  0 0 0 0 0 0 0 0 0 0 ...
# $ cogs                  : num  58.9 23.4 46 91.6 17.1 ...
# $ invoiced_qty          : int  1 1 1 2 1 1 1 2 1 1 ...
# $ sales_price           : num  137 48.9 137 97.8 39.1 ...

summary(xdf)
# sales_id        document_number_prefix document_number_suffix CURRENCY_CURRENCY_CODE    item_id        STYLE_NAME        line_amt_pre_discount
# Min.   :   45988   CSI:421045             Length:513153          Length:513153          100476 :  5763   Length:513153      Min.   :-56043.21    
# 1st Qu.:   98660   SCN: 92108             Class :character       Class :character       100514 :  4906   Class :character   1st Qu.:    17.12    
# Median :  119125                          Mode  :character       Mode  :character       IBM200 :  3893   Mode  :character   Median :    58.70    
# Mean   : 2507654                                                                        IBN327 :  3885                      Mean   :    82.54    
# 3rd Qu.:  146429                                                                        100871 :  3840                      3rd Qu.:   146.74    
# Max.   :90003867                                                                        100471 :  3711                      Max.   : 62560.45    
#                                                                                         (Other):487155
#                                            
# line_amt_post_discount invoiced_discount_acy      cogs            invoiced_qty       sales_price     
# Min.   :-43876.77      Min.   :-23832.20     Min.   :-38204.90   Min.   :-830.000   Min.   :   0.00  
# 1st Qu.:    14.15      1st Qu.:     0.00     1st Qu.:     7.04   1st Qu.:   1.000   1st Qu.:  20.81  
# Median :    48.91      Median :     0.00     Median :    23.16   Median :   1.000   Median :  47.03  
# Mean   :    72.33      Mean   :    10.21     Mean   :    36.10   Mean   :   1.695   Mean   :  58.08  
# 3rd Qu.:   136.96      3rd Qu.:     2.35     3rd Qu.:    65.96   3rd Qu.:   3.000   3rd Qu.:  72.26  
# Max.   : 43876.77      Max.   : 26588.19     Max.   : 38204.90   Max.   :1600.000   Max.   :3000.00

# write out the new dataframe as a feather file
write_feather(xdf, "invoice_trans.feather")
