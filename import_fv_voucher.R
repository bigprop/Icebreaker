#### IMPORT THE FV_VOUCHER TABLE ####

## always remember to preserve the row ordering within columns...so can correctly assemble data frame from columns!!!!

rm(list = ls()) # clear the workspace as a precaution
options(stringsAsFactors = FALSE) # set option so that by default strings are left as character vector

require(feather)
require(tidyverse)
require(stringr)
require(dplyr)
#require(bit64)

# INSTANTIATE tibbles from feather
setwd("C:/Users/rp/Projects/icebreaker_rp")
(fv_voucher_df <- read_feather("R_FV_VOUCHER.feather")) %>% View


# dimensions
nrow(fv_voucher_df); ncol(fv_voucher_df)  # 614 rows, 25 cols

colnames(fv_voucher_df)
# [1] "ACCOUNT_NUM"                 "CAN_BE_REVERSED"             "CUST_CASH_DISC_DATE"         "DUE_DATE"                    "EU_SALES_LIST"              
# [6] "EXCH_ADJUSTMENT"             "LAST_INTEREST_DATE"          "OFFSET_ACCOUNT_NUM"          "UTILISED_CASH_DISC"          "OFFSET_COMPANY"             
# [11] "OFFSET_RECID"                "OFFSET_TRANS_VOUCHER"        "PENNY_DIFF"                  "SETTLE_AMOUNT_CUR"           "SETTLE_AMOUNT_MST"          
# [16] "SETTLE_TAX1099_AMOUNT"       "SETTLE_TAX1099_STATE_AMOUNT" "TRANS_COMPANY"               "TRANS_DATE"                  "TRANS_RECID"                
# [21] "CREATE_DATE_TIME"            "CREATED_BY"                  "DATE_AREA_ID"                "REC_VERSION"                 "RECID" 



### HOW MUCH MISSING DATA in THE TIBBLE???? ####
fv_voucher_df %>% summarise_each(funs(100*mean(is.na(.)))) %>% View # there are a few NA's in the data set.


### COLUMNS THAT WE ARE INTERESTED IN....

here!!!!!



### ORDER_TYPE ###
# string factor
#
# How many distinct ORDER_TYPE?
distinct(fv_voucher_df, ORDER_TYPE) # %>% nrow  # 3
# ORDER_TYPE
# <chr>
# 1    Sales order
# 2 Returned order
# 3        Journal

order_type_col <- transmute(fv_voucher_df, order_type = as.factor(str_to_lower(ORDER_TYPE)))
str(order_type_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	83637 obs. of  1 variable:
#   $ order_type: Factor w/ 3 levels "Journal","Returned order",..: 3 3 3 3 3 3 3 3 3 3 ...
#   
summary(order_type_col)
# order_type   
# Journal       :    3  
# Returned order:10681  
# Sales order   :72953


### FV_VOUCHER ###
# test for structure SOnnnnnnnn. e.g SO00080074.  also contains sales returns... SRnnnnn.  Also orders without a prefix!
# extract the numeric part and test convert to integer  

# investigate the FV_VOUCHER prefix.  check that SO is the only prefix
distinct(fv_voucher_df, prefix = str_sub(FV_VOUCHER,1,2)) # "SO", "SR"

# Get a count of the rows that would fail pattern match
filter(fv_voucher_df, !str_detect(FV_VOUCHER, '^[A-Z]{2}\\w+')) # %>% nrow # 184  e.g. 167988

# !!!! lets put the SO prefix back in for the rows where it is missing !!! Is this the correct approach?
# xdf <- transmute(fv_voucher_df, fv_voucher= ifelse(str_detect(FV_VOUCHER,"^[A-Z]{2}"), FV_VOUCHER, paste0("SO",FV_VOUCHER)))

# perhaps a better approach is to create the prefix and fv_voucher columns separately
fv_voucher_prefix <- transmute(fv_voucher_df, fv_voucher_prefix = as.factor(ifelse(str_detect(FV_VOUCHER,"^[A-Z]{2}"), str_sub(FV_VOUCHER,1,2), NA)))
summary(fv_voucher_prefix)

fv_voucher_no <- transmute(fv_voucher_df, fv_voucher = as.integer(ifelse(str_detect(FV_VOUCHER,"^[A-Z]{2}"), str_sub(FV_VOUCHER,3), FV_VOUCHER)))
summary(fv_voucher_no)

fv_voucher_col = cbind(fv_voucher_prefix, fv_voucher_no, fv_voucher_unmodifed=fv_voucher_df$FV_VOUCHER)
str(fv_voucher_col)
# 'data.frame':	83637 obs. of  3 variables:
# $ prefix               : Factor w/ 2 levels "SO","SR": 1 1 1 1 1 1 1 1 1 1 ...
# $ fv_voucher          : int  80074 80075 80077 80078 80080 80081 80083 88725 88726 88727 ...
# $ fv_voucher_unmodifed: chr  "SO00080074" "SO00080075" "SO00080077" "SO00080078" ...


# do we expect no dups?
group_by(fv_voucher_col, fv_voucher) %>% filter( n() > 1) %>% nrow # 192 duplicates

summary(fv_voucher_col)
# prefix       fv_voucher       fv_voucher_unmodifed
# SO  :83442   Min.   :     104   Length:83637         
# SR  :   11   1st Qu.:   34216   Class :character     
# NA's:  184   Median :   74827   Mode  :character     
#              Mean   : 1837586                        
#              3rd Qu.:  123962                        
#              Max.   :90004020

 
### CUSTOMER_ORDER_GROUP ###
# string factor
# 
# How many distinct CUSTOMER_ORDER_GROUP?
distinct(fv_voucher_df, CUSTOMER_ORDER_GROUP) %>% arrange(CUSTOMER_ORDER_GROUP)# %>% nrow  # 4
# CUSTOMER_ORDER_GROUP
# <chr>
# 1               AtOnce
# 2                  EOL
# 3               Indent
# 4                Promo


cust_order_group_col <- transmute(fv_voucher_df, cust_order_group = as.factor(str_to_lower(CUSTOMER_ORDER_GROUP)))
str(cust_order_group_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	83637 obs. of  1 variable:
#   $ cust_order_group: Factor w/ 4 levels "AtOnce","EOL",..: 1 1 1 1 1 1 1 1 1 1 ....
#   
summary(cust_order_group_col)
# cust_order_group
# atonce:79003    
# eol   :  687    
# indent: 2849    
# promo : 1098

 
### SALES_RESPONSIBLE  rename to account_mgr ###
# string factor.  contains NAs
# 
# How many distinct ORDER_TYPE?
distinct(fv_voucher_df, SALES_RESPONSIBLE) %>% arrange(SALES_RESPONSIBLE) # %>% nrow  # 27
# SALES_RESPONSIBLE
# <chr>
# 1      Anna Jamieson
# 2   Chris Vanderkolk
# 3        Chuck Grubb


account_mgr_col <- transmute(fv_voucher_df, account_mgr = as.factor(str_to_lower(SALES_RESPONSIBLE)))
str(account_mgr_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	83637 obs. of  1 variable:
#  $ account_mgr: Factor w/ 26 levels "anna jamieson",..: 2 26 6 1 2 23 3 1 6 7 ...
#   
summary(account_mgr_col)
# account_mgr   
# anna jamieson      :17908  
# emily o'leary      :13039  
# misc customers (nz): 8914  
# chris vanderkolk   : 6663  
# chuck grubb        : 5100  
# (Other)            :29257  
# NA's               : 2756

 
### NAME ###
# string - need to fuzzy match?
#
name_col <- select(fv_voucher_df, name = NAME)

str(name_col)
#Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	83637 obs. of  1 variable:
#  $ name: chr  "Rebel Sport Head Office" "Rebel Sport Head Office" "R & R Sport Dunedin" "SB International - Staff Purchases" ...

 
### DELIVERY_NAME ###
# string - need to fuzzy match?
# 
delivery_name_col <- select(fv_voucher_df, delivery_name = DELIVERY_NAME)

str(delivery_name_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	83637 obs. of  1 variable:
#   $ delivery_name: chr  "Rebel Sport Hornby" "Rebel Sport Pukekohe  8070" "R & R Sport Dunedin" "SB International - Staff" ...


### STREET ###
# string - need to fuzzy match?
# 
street_col <- select(fv_voucher_df, street = STREET)

str(street_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	83637 obs. of  1 variable:
#   $ street: chr  "Unit 3, 19 Chalmers StHornby" "Unit A Pukekohe Mega centre" "70 Lower Stuart Street" "98 Waterloo Road" ...


# STATE
# string factor
# 
# How many distinct STATE?
distinct(fv_voucher_df, state = str_to_lower(STATE)) %>% arrange(state) # %>% nrow  # 9
# state
# <chr>
# 1   act
# 2   nsw
# 3    nt
# 4   qld


state_col <- transmute(fv_voucher_df, state = as.factor(str_to_lower(STATE)))
str(state_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	83637 obs. of  1 variable:
#   $ state: Factor w/ 8 levels "act","nsw","nt",..: NA NA NA NA NA NA NA NA NA NA ...
#   
summary(state_col)
# state      
# vic    : 1797  
# nsw    : 1135  
# tas    :  511  
# wa     :  403  
# qld    :  364  
# (Other):  420  
# NA's   :79007


# ZIP_POST_CODE
# string factor  contains NAs
# 
# How many distinct ZIP_POST_CODE?
distinct(fv_voucher_df, ZIP_POST_CODE) %>% arrange(ZIP_POST_CODE) # %>% nrow  # 1727
# ZIP_POST_CODE
# <chr>
# 1              0
# 2           1000
# 3           1001
# 4           1002

zip_post_code_col <- transmute(fv_voucher_df, zip_post_code = as.factor(str_to_lower(ZIP_POST_CODE)))
str(zip_post_code_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	83637 obs. of  1 variable:
#   $ zip_post_code: Factor w/ 1726 levels "0","1000","1001",..: 1723 166 1505 1418 18 243 1061 1069 1565 1214 ...
  
summary(zip_post_code_col)
# zip_post_code  
# 6011   : 5999  
# 9300   : 3439  
# 8011   : 3213  
# 2022   : 2620  
# 1011   : 2030  
# (Other):61134  
# NA's   : 5202


# CITY
# string factor
#
xdf <- transmute(fv_voucher_df, city=str_to_lower(CITY)) # convert city to lower case
distinct(xdf, city ) %>% arrange(city) # %>% nrow  # check how many city values 1453
# CITY
# <chr>
# 1  19-23 Taranaki Street,
# 2             Abbey Caves
# 3          ABERFOYLE PARK
# 4               Addington
# 5                Adelaide

# bit of a data quality problem... with 19-23 Taranaki St...
# do match for only alphabetic city names
# replace fails with NA
filter(xdf, !str_detect(city, "^[a-z]")) # 19 -23 Taranaki St

city_col <- mutate(xdf, city=as.factor(replace(city, !str_detect(city,"^[a-z]"), NA)) )
# filter(city_col, !str_detect(city, "^[a-z]")) %>% nrow # test that its been removed

str(city_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	83637 obs. of  1 variable:
#   $ city: Factor w/ 1451 levels "abbey caves",..: 231 1022 308 497 64 870 1378 845 1359 539 ....

summary(city_col)
# city      
# auckland    :12990  
# wellington  : 7792  
# christchurch: 7599  
# queenstown  : 3613  
# dunedin     : 2556  
# (Other)     :48141  
# NA's        :  946


### COUNTRY_REGION ###
# string factor  includes NAs
#
# How many distinct STATE?
distinct(fv_voucher_df, country_region = str_to_lower(COUNTRY_REGION)) %>% arrange(country_region) # %>% nrow  # 6
# country_region
# <chr>
# 1            are
# 2            aus
# 3            gbr
# 4            jpn
# 5            nzl
# 6           <NA>


country_region_col <- transmute(fv_voucher_df, country_region = as.factor(str_to_lower(COUNTRY_REGION)))
str(country_region_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	83637 obs. of  1 variable:
#   $ country_region: Factor w/ 5 levels "are","aus","gbr",..: 5 5 5 5 5 2 5 5 5 5 ...
   
summary(country_region_col)
# country_region
# are :    3    
# aus :13732    
# gbr :    2    
# jpn :    1    
# nzl :69897    
# NA's:    2



### BIND INTO A NEW DATAFRAME
xdf <- cbind(
      order_type_col,
      fv_voucher_col,
      cust_order_group_col,
      account_mgr_col,
      name_col,
      delivery_name_col,
      street_col,
      state_col,
      zip_post_code_col,
      city_col,
      country_region_col)


View(xdf) # lets look at the findal data frame...

str(xdf)

summary(xdf)

# write fv_voucher dataframe out as a feather file
write_feather(xdf,"fv_voucher.feather")
