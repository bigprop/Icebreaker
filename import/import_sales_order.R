#### IMPORT THE SALES_ORDER TABLE ####
# date 4/4/17

## always remember to preserve the row ordering within columns...so can correctly assemble data frame from columns!!!!

rm(list = ls()) # clear the workspace as a precaution
options(stringsAsFactors = FALSE) # set option so that by default strings are left as character vector

require(methods) # Rscript wants it loaded
require(feather)
require(tidyverse)
require(stringr)


# INSTANTIATE tibbles from feather
#setwd("C:/Users/rp/Projects/icebreaker_rp")
(sales_order_df <- read_feather("data/R_SALES_ORDER.feather"))
if (interactive()) View(sales_order_df)


# dimensions
nrow(sales_order_df); ncol(sales_order_df)  # 83637 rows, 35 cols

colnames(sales_order_df)
# [1] "ORDER_TYPE"             "SALES_ORDER"            "RMA_NUMBER"             "STATUS"                 "CUSTOMER_ORDER_GROUP"   "CUSTOMER_ACCOUNT"      
# [7] "CUSTOMER_GROUP"         "SALES_RESPONSIBLE"      "NAME"                   "DELIVERY_NAME"          "STREET"                 "STATE"                 
# [13] "ZIP_POST_CODE"          "CITY"                   "COUNTRY_REGION"         "ADDRESS"                "REQUESTED_SHIP_DATE"    "REQUESTED_RECEIPT_DATE"
# [19] "CUSTOMER_REQUISITION"   "CUSTOMER_REFERENCE"     "SHIP_TO_CODE"           "SHIP_TO_CODE2"          "STOPPED"                "HOLD_CODE"             
# [25] "VALIDATION_STATUS"      "HANDLING_STATUS"        "MODE_OF_DELIVERY"       "INVOICE_ACCOUNT"        "CONTRACT_CALL_OFF"      "CONTRACT_ORDER"        
# [31] "LINE_GROUP_DISCOUNT"    "INVOICED_AMOUNT"        "SALES_ORIGIN"           "DC_BYPASS"              "CREATED_DATE_TIME"



### HOW MUCH MISSING DATA in THE TIBBLE???? ####
sales_order_df %>% summarise_each(funs(100*mean(is.na(.)))) # there are lots of NA's in the data set.


### COLUMNS THAT WE ARE INTERESTED IN....

### ORDER_TYPE ###
# string factor
#
# How many distinct ORDER_TYPE?
distinct(sales_order_df, ORDER_TYPE) # %>% nrow  # 3
# ORDER_TYPE
# <chr>
# 1    Sales order
# 2 Returned order
# 3        Journal

order_type_col <- transmute(sales_order_df, order_type = as.factor(str_to_lower(ORDER_TYPE)))
str(order_type_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	83637 obs. of  1 variable:
#   $ order_type: Factor w/ 3 levels "Journal","Returned order",..: 3 3 3 3 3 3 3 3 3 3 ...
#   
summary(order_type_col)
# order_type   
# Journal       :    3  
# Returned order:10681  
# Sales order   :72953


### SALES_ORDER ###
# test for structure SOnnnnnnnn. e.g SO00080074.  also contains sales returns... SRnnnnn.  Also orders without a prefix!
# convert to a factor
# 
# there is overlap between SO and SR eg SO00017313 and SR00017313  
# 
# test that there are no duplicates when including prefix
group_by(sales_order_df, SALES_ORDER) %>% filter(n() > 1) %>% nrow # 0.  no duplicates

# investigate the SALES_ORDER prefix. 
distinct(sales_order_df, prefix = str_sub(SALES_ORDER,1,2)) # "SO", "SR".  some have no prefix

# what is the numeric range for each of SO, SR and no prefix?
(xdf <- filter(sales_order_df, str_detect(SALES_ORDER, "SO"))) %>% nrow # 83442
xdf <- transmute(xdf, so = as.integer(str_sub(SALES_ORDER, 3))) %>% arrange(so)
summarise(xdf, min(so), max(so))
# `min(so)` `max(so)`
# <int>     <int>
#   104  90004020

(ydf <- filter(sales_order_df, str_detect(SALES_ORDER, "SR"))) %>% nrow # 11
ydf <- transmute(ydf, so = as.integer(str_sub(SALES_ORDER, 3))) %>% arrange(so)
summarise(ydf, min(so), max(so))
# `min(so)` `max(so)`
# <int>     <int>
#   1     15834     17448

# need to check if there are any actual overlap numbers!  Yes.
intersect(xdf$so, ydf$so) #  17313 17368 17386 17387 17417 17425

# suggest that the best approach is to convert the sales order to a factor which will maintain the uniqueness between SO and SR with the same number
sales_order_col <- transmute(sales_order_df, sales_order = as.factor(str_to_lower(SALES_ORDER)))

str(sales_order_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	83637 obs. of  1 variable:
# $ sales_order: Factor w/ 83637 levels "107048","107050",..: 44796 44797 44798

# ORIGINAL APPROACH
# create the prefix and sales_order columns separately
#sales_order_prefix <- transmute(sales_order_df, sales_order_prefix = as.factor(ifelse(str_detect(SALES_ORDER,"^[A-Z]{2}"), str_sub(SALES_ORDER,1,2), NA)))
#summary(sales_order_prefix)
# sales_order_prefix
# SO  :83442        
# SR  :   11        
# NA's:  184 

#sales_order_no <- transmute(sales_order_df, sales_order = as.integer(ifelse(str_detect(SALES_ORDER,"^[A-Z]{2}"), str_sub(SALES_ORDER,3), SALES_ORDER)))
#summary(sales_order_no)
# sales_order      
# Min.   :     104  
# 1st Qu.:   34216  
# Median :   74827  
# Mean   : 1837586  
# 3rd Qu.:  123962  
# Max.   :90004020

#sales_order_col = cbind(sales_order_prefix, sales_order_no)
#str(sales_order_col)
# 'data.frame':	83637 obs. of  2 variables:
# $ sales_order_prefix: Factor w/ 2 levels "SO","SR": 1 1 1 1 1 1 1 1 1 1 ...
# $ sales_order       : int  80074 80075 80077 80078 80080 80081 80083 88725


### CUSTOMER_ORDER_GROUP ###
# string factor
# 
# How many distinct CUSTOMER_ORDER_GROUP?
distinct(sales_order_df, CUSTOMER_ORDER_GROUP) %>% arrange(CUSTOMER_ORDER_GROUP)# %>% nrow  # 4
# CUSTOMER_ORDER_GROUP
# <chr>
# 1               AtOnce
# 2                  EOL
# 3               Indent
# 4                Promo


cust_order_group_col <- transmute(sales_order_df, cust_order_group = as.factor(str_to_lower(CUSTOMER_ORDER_GROUP)))
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
distinct(sales_order_df, SALES_RESPONSIBLE) %>% arrange(SALES_RESPONSIBLE) # %>% nrow  # 27
# SALES_RESPONSIBLE
# <chr>
# 1      Anna Jamieson
# 2   Chris Vanderkolk
# 3        Chuck Grubb


account_mgr_col <- transmute(sales_order_df, account_mgr = as.factor(str_to_lower(SALES_RESPONSIBLE)))
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
name_col <- select(sales_order_df, name = NAME)

str(name_col)
#Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	83637 obs. of  1 variable:
#  $ name: chr  "Rebel Sport Head Office" "Rebel Sport Head Office" "R & R Sport Dunedin" "SB International - Staff Purchases" ...

 
### DELIVERY_NAME ###
# string - need to fuzzy match?
# 
delivery_name_col <- select(sales_order_df, delivery_name = DELIVERY_NAME)

str(delivery_name_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	83637 obs. of  1 variable:
#   $ delivery_name: chr  "Rebel Sport Hornby" "Rebel Sport Pukekohe  8070" "R & R Sport Dunedin" "SB International - Staff" ...


### STREET ###
# string - need to fuzzy match?
# 
street_col <- select(sales_order_df, street = STREET)

str(street_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	83637 obs. of  1 variable:
#   $ street: chr  "Unit 3, 19 Chalmers StHornby" "Unit A Pukekohe Mega centre" "70 Lower Stuart Street" "98 Waterloo Road" ...


# STATE
# string factor
# 
# How many distinct STATE?
distinct(sales_order_df, state = str_to_lower(STATE)) %>% arrange(state) # %>% nrow  # 9
# state
# <chr>
# 1   act
# 2   nsw
# 3    nt
# 4   qld


state_col <- transmute(sales_order_df, state = as.factor(str_to_lower(STATE)))
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
distinct(sales_order_df, ZIP_POST_CODE) %>% arrange(ZIP_POST_CODE) # %>% nrow  # 1727
# ZIP_POST_CODE
# <chr>
# 1              0
# 2           1000
# 3           1001
# 4           1002

zip_post_code_col <- transmute(sales_order_df, zip_post_code = as.factor(str_to_lower(ZIP_POST_CODE)))
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
xdf <- transmute(sales_order_df, city=str_to_lower(CITY)) # convert city to lower case
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
distinct(sales_order_df, country_region = str_to_lower(COUNTRY_REGION)) %>% arrange(country_region) # %>% nrow  # 6
# country_region
# <chr>
# 1            are
# 2            aus
# 3            gbr
# 4            jpn
# 5            nzl
# 6           <NA>


country_region_col <- transmute(sales_order_df, country_region = as.factor(str_to_lower(COUNTRY_REGION)))
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
      sales_order_col,
      order_type_col,
      cust_order_group_col,
      account_mgr_col,
      name_col,
      delivery_name_col,
      street_col,
      state_col,
      zip_post_code_col,
      city_col,
      country_region_col)


if (interactive()) View(xdf) # lets look at the final data frame...

str(xdf)

summary(xdf)

# write sales_order dataframe out as a feather file
write_feather(xdf,"data/sales_order.feather")
