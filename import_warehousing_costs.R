#### IMPORT WAREHOUSING COSTS ####

rm(list = ls()) # clear the workspace as a precaution

require(feather)
require(tidyverse)
require(stringr)
require(dplyr)
# require(bit64)

# options(scipen=999) # force not to use scientific notation for number display
# options(scipen=0) # default value

# INSTANTIATE tibbles from feather
setwd("C:/Users/rp/Projects/icebreaker_rp")
(warehousing_costs_in <- read_feather("R_WAREHOUSING_COSTS.feather") %>% arrange(REGION)) %>% View # import, arrange and view the data

# dimensions
nrow(warehousing_costs_in); ncol(warehousing_costs_in)  # 58 rows. 3 cols

# column names
colnames(warehousing_costs_in)
# [1] "REGION"                     "PICKING_PU"                 "DISPATCH_PU"                "VAS_PU"                     "PACKAGING_PU"              
# [6] "WAREHOUSING_COSTS_PU"       "RETURNS_PU"                 "FREIGHT_COSTS_PU"           "SALES_ORDER_PROCESSING_PH"  "RETURN_ORDER_PROCESSING_PH"
# [11] "FINANCE_PH"                 "EDI_TRANSACTION_COSTS"      "THREE_P_FIVE_FIXED_COST"    "CS_FIXED_COST"

### HOW MUCH MISSING DATA in THE TIBBLE???? ####
warehousing_costs_in %>% summarise_each(funs(100*mean(is.na(.)))) # 0.  No NA

# any duplicate rows that need to be removed?
(xdf <- group_by(warehousing_costs_in, REGION)) %>% filter(n() > 1) %>% nrow # 0. 
warehousing_costs <- warehousing_costs_in  # No duplicates need to be removed


### "REGION" ###
# string factor. lower case
# 
# how many distinct factors?
distinct(warehousing_costs_in, REGION)  # %>% View   20. No duplicates
# REGION
# <chr>
# 1   WSAND
# 2   WSAUS
# 3   WSAUT
# 4   WSBLX
# 5   WSCAN
#     etc

# Need to avoid duplicates By changing everything to lower case and then convert to a factor
region_col <- transmute(warehousing_costs, region = as.factor(str_to_lower(REGION)))

str(region_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	20 obs. of  1 variable:
#   $ region: Factor w/ 20 levels "wsand","wsaus",..: 1 2 3 4 5 6 7 8 9 10 ...

summary(region_col)
# region  
# wsand  : 1  
# wsaus  : 1  
# wsaut  : 1  
# wsblx  : 1  
# wscan  : 1  
# wscee  : 1  
# (Other):14


### "PICKING_PU" ###
# string numeric
#
# test no conversion failures
filter(warehousing_costs,is.na(as.numeric(PICKING_PU))) %>% nrow   # 0. No failures              

picking_pu_col <- transmute(warehousing_costs, picking_pu = as.numeric(PICKING_PU))

summary(picking_pu_col)
# picking_pu    
# Min.   :0.0600  
# 1st Qu.:0.1084  
# Median :0.1671  
# Mean   :0.1839  
# 3rd Qu.:0.1952  
# Max.   :0.5784


#### "DISPATCH_PU" ###                
# string numeric
#
# test no conversion failures
filter(warehousing_costs,is.na(as.numeric(DISPATCH_PU))) %>% nrow   # 0. No failures              

dispatch_pu_col <- transmute(warehousing_costs, dispatch_pu = as.numeric(DISPATCH_PU))

summary(dispatch_pu_col)
# dispatch_pu    
# Min.   :0.0000  
# 1st Qu.:0.3807  
# Median :0.4018  
# Mean   :0.4336  
# 3rd Qu.:0.4241  
# Max.   :0.9129


### "VAS_PU" ###                     
# string numeric
#
# test no conversion failures
filter(warehousing_costs,is.na(as.numeric(VAS_PU))) %>% nrow   # 0. No failures              

vas_pu_col <- transmute(warehousing_costs, vas_pu = as.numeric(VAS_PU))

summary(vas_pu_col)
# vas_pu      
# Min.   :0.0960  
# 1st Qu.:0.0960  
# Median :0.0960  
# Mean   :0.1151  
# 3rd Qu.:0.0960  
# Max.   :0.2644


### "PACKAGING_PU" ###
# string numeric
#
# test no conversion failures
filter(warehousing_costs,is.na(as.numeric(PACKAGING_PU))) %>% nrow   # 0. No failures              

packaging_pu_col <- transmute(warehousing_costs, packaging_pu = as.numeric(PACKAGING_PU))

summary(packaging_pu_col)
# packaging_pu    
# Min.   :0.00000  
# 1st Qu.:0.04955  
# Median :0.05495  
# Mean   :0.06387  
# 3rd Qu.:0.05985  
# Max.   :0.13420
  
            
### "WAREHOUSING_COSTS_PU" ###
# string numeric
#
# test no conversion failures
filter(warehousing_costs,is.na(as.numeric(WAREHOUSING_COSTS_PU))) %>% nrow   # 0. No failures              

warehousing_costs_pu_col <- transmute(warehousing_costs, warehousing_costs = as.numeric(WAREHOUSING_COSTS_PU))

summary(warehousing_costs_pu_col) 
# warehousing_costs 
# Min.   :0.002500  
# 1st Qu.:0.003200  
# Median :0.004700  
# Mean   :0.090585  
# 3rd Qu.:0.006775  
# Max.   :0.524700


### "RETURNS_PU" ###
# string numeric
#
# test no conversion failures
filter(warehousing_costs,is.na(as.numeric(RETURNS_PU))) %>% nrow   # 0. No failures              

returns_pu_col <- transmute(warehousing_costs, returns_pu = as.numeric(RETURNS_PU))

summary(returns_pu_col)
# returns_pu    
# Min.   :0.8587  
# 1st Qu.:1.1740  
# Median :1.1740  
# Mean   :1.1484  
# 3rd Qu.:1.1740  
# Max.   :1.3247


### "FREIGHT_COSTS_PU" ###        
# string numeric
#
# test no conversion failures
filter(warehousing_costs,is.na(as.numeric(FREIGHT_COSTS_PU))) %>% nrow   # 1. NA             

freight_costs_pu_col <- transmute(warehousing_costs, freight_costs_pu = as.numeric(FREIGHT_COSTS_PU))

summary(freight_costs_pu_col)


# "SALES_ORDER_PROCESSING_PH"  
# string numeric
#
# test no conversion failures
filter(warehousing_costs,is.na(as.numeric(SALES_ORDER_PROCESSING_PH))) %>% nrow   # 0. No failures              

sales_order_processing_pu_col <- transmute(warehousing_costs, sales_order_processing_pu = as.numeric(SALES_ORDER_PROCESSING_PH))

summary(sales_order_processing_pu_col)
# sales_order_processing_pu
# Min.   :25               
# 1st Qu.:25               
# Median :25               
# Mean   :25               
# 3rd Qu.:25               
# Max.   :25


# "RETURN_ORDER_PROCESSING_PH"
# string numeric
#
# test no conversion failures
filter(warehousing_costs,is.na(as.numeric(RETURN_ORDER_PROCESSING_PH))) %>% nrow   # 0. No failures

return_order_processing_pu_col <- transmute(warehousing_costs, return_order_processing_pu = as.numeric(RETURN_ORDER_PROCESSING_PH))

summary(return_order_processing_pu_col)
# return_order_processing_pu
# Min.   :25                
# 1st Qu.:25                
# Median :25                
# Mean   :25                
# 3rd Qu.:25                
# Max.   :25


### "FINANCE_PH" ###                 
# string numeric   all NA
#
# test no conversion failures
filter(warehousing_costs,is.na(as.numeric(FINANCE_PH))) %>% nrow   # 20. All failures              

finance_ph_col <- transmute(warehousing_costs, finance_ph = as.numeric(FINANCE_PH))

summary(finance_ph_col)
# finance_ph 
# Min.   : NA  
# 1st Qu.: NA  
# Median : NA  
# Mean   :NaN  
# 3rd Qu.: NA  
# Max.   : NA  
# NA's   :20


# "EDI_TRANSACTION_COSTS"      
# string numeric
#
# test no conversion failures
filter(warehousing_costs,is.na(as.numeric(EDI_TRANSACTION_COSTS))) %>% nrow   # 0. No failures

edi_transaction_col <- transmute(warehousing_costs, edi_transaction = as.numeric(EDI_TRANSACTION_COSTS))

summary(edi_transaction_col)
# edt_transaction
# Min.   :0.1    
# 1st Qu.:0.1    
# Median :0.1    
# Mean   :0.1    
# 3rd Qu.:0.1    
# Max.   :0.1


# "THREE_P_FIVE_FIXED_COST"    
# string numeric
#
# test no conversion failures
filter(warehousing_costs,is.na(as.numeric(THREE_P_FIVE_FIXED_COST))) %>% nrow   # 0. No failures              

three_p_five_fixed_cost_col <- transmute(warehousing_costs, three_p_five_fixed_cost = as.numeric(THREE_P_FIVE_FIXED_COST))

summary(three_p_five_fixed_cost_col)
# three_p_five_fixed_cost
# Min.   :-3.221         
# 1st Qu.:-3.221         
# Median :-3.221         
# Mean   :-3.221         
# 3rd Qu.:-3.221         
# Max.   :-3.221


# "CS_FIXED_COST"
# string numeric
#
# test no conversion failures
filter(warehousing_costs,is.na(as.numeric(CS_FIXED_COST))) %>% nrow   # 0. No failures              

cs_fixed_cost_col <- transmute(warehousing_costs, cs_fixed_cost = as.numeric(CS_FIXED_COST))

summary(cs_fixed_cost_col)
# cs_fixed_cost   
# Min.   :-1.586  
# 1st Qu.:-1.586  
# Median :-1.586  
# Mean   :-1.586  
# 3rd Qu.:-1.586  
# Max.   :-1.586



### CREATE THE NEW RETURN_REASON_CODES
# [1] "REGION"                     "PICKING_PU"                 "DISPATCH_PU"                "VAS_PU"                     "PACKAGING_PU"               "WAREHOUSING_COSTS_PU"      
# [7] "RETURNS_PU"                 "FREIGHT_COSTS_PU"           "SALES_ORDER_PROCESSING_PH"  "RETURN_ORDER_PROCESSING_PH" "FINANCE_PH"                 "EDI_TRANSACTION_COSTS"     
# [13] "THREE_P_FIVE_FIXED_COST"    "CS_FIXED_COST"

xdf <- cbind(
    region_col,
    picking_pu_col,
    dispatch_pu_col,
    vas_pu_col,
    packaging_pu_col,
    warehousing_costs_pu_col,
    returns_pu_col,
    freight_costs_pu_col,
    sales_order_processing_pu_col,
    return_order_processing_pu_col,
    finance_ph_col,
    edi_transaction_col,
    three_p_five_fixed_cost_col,
    cs_fixed_cost_col)
 

View(xdf)

str(xdf)
# 'data.frame':	20 obs. of  14 variables:
# $ region                    : Factor w/ 20 levels "wsand","wsaus",..: 1 2 3 4 5 6 7 8 9 10 ...
# $ picking_pu                : num  0.0892 0.06 0.1868 0.1084 0.5784 ...
# $ dispatch_pu               : num  0.311 0.64 0.389 0.374 0 ...
# $ vas_pu                    : num  0.096 0.16 0.096 0.096 0.182 ...
# $ packaging_pu              : num  0.0385 0.1217 0.0447 0.0531 0 ...
# $ warehousing_costs         : num  0.0033 0.5 0.0025 0.0032 0.2167 ...
# $ returns_pu                : num  1.17 1 1.17 1.17 1.32 ...
# $ freight_costs_pu          : num  0 0.7 0 0.0918 0.4433 ...
# $ sales_order_processing_pu : num  25 25 25 25 25 25 25 25 25 25 ...
# $ return_order_processing_pu: num  25 25 25 25 25 25 25 25 25 25 ...
# $ finance_ph                : num  NA NA NA NA NA NA NA NA NA NA ...
# $ edi_transaction           : num  0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 ...
# $ three_p_five_fixed_cost   : num  -3.22 -3.22 -3.22 -3.22 -3.22 ...
# $ cs_fixed_cost             : num  -1.59 -1.59 -1.59 -1.59 -1.59 ...

# write out the new dataframe as a feather file
write_feather(xdf, "warehousing_costs_codes.feather")
