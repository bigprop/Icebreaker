#### IMPORT THE CUSTOMER TABLE ####

rm(list = ls()) # clear the workspace as a precaution

require(feather)
require(tidyverse)
require(stringr)


# INSTANTIATE tibbles from feather
# setwd("C:/Users/rp/Projects/icebreaker_rp")
(customers <- read_feather("data/R_CUSTOMER_TABLE_FINAL.feather")) 
#     %>% View

# dimensions
nrow(customers); ncol(customers)  # 676 rows, 23 cols

colnames(customers)
# [1] "CUSTOMER_ACCOUNT"              "NAME"                          "COMPANY_CHAIN"                 "CURRENCY"                     
# [5] "CUR_CREDIT_LIMIT"              "CREDIT_LIMIT"                  "CUSTOMERS_PRICES_HANDLING"     "INVOICE_ACCOUNT"              
# [9] "LINE_DISCOUNT"                 "LINE_OF_BUSINESS"              "MAIN_CUSTOMER_ACCOUNT"  
# 
# 
# "MANDATORY_CREDIT_LIMIT"       
# [13] "PRICE_GROUP"                   "PROJECT_CODE"                  "SALES_DISTRICT"                "STATISTICS_GROUP"             
# [17] "SEGMENT"                       "SUBSEGMENT"                    "TERMS_OF_PAYMENT"              "VAS_CODES_GROUP"              
# [21] "CUSTOMER_CLASSIFICATION_GROUP" "EDI_CODE"                      "TERMS_OF_BUSINESS"


### HOW MUCH MISSING DATA in THE TIBBLE???? ####
customers %>% summarise_each(funs(100*mean(is.na(.)))) # there are NA's in the data set.

### "CUSTOMER_ACCOUNT" ###
#  string integer
# 
# Get a count of the rows that fail conversion
customers %>% filter( is.na(as.integer(customers$CUSTOMER_ACCOUNT))) %>% nrow # 0







