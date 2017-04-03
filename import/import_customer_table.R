#### IMPORT THE CUSTOMER TABLE ####
# date: 4/4/17

rm(list = ls()) # clear the workspace as a precaution

require(methods) # Rscript wants it loaded
require(feather)
require(tidyverse)
require(stringr)


# INSTANTIATE tibbles from feather
# setwd("C:/Users/rp/Projects/icebreaker_rp")
(customers <- read_feather("data/R_CUSTOMER_TABLE_FINAL.feather")) # import and view the data
if (interactive()) View(customers)

# dimensions
nrow(customers); ncol(customers)  # 676 rows, 23 cols

colnames(customers)
# [1] "CUSTOMER_ACCOUNT"              "NAME"                          "COMPANY_CHAIN"                 "CURRENCY"                     
# [5] "CUR_CREDIT_LIMIT"              "CREDIT_LIMIT"                  "CUSTOMERS_PRICES_HANDLING"     "INVOICE_ACCOUNT"              
# [9] "LINE_DISCOUNT"                 "LINE_OF_BUSINESS"              "MAIN_CUSTOMER_ACCOUNT"         "MANDATORY_CREDIT_LIMIT"       
# [13] "PRICE_GROUP"                   "PROJECT_CODE"                  "SALES_DISTRICT"                "STATISTICS_GROUP"             
# [17] "SEGMENT"                       "SUBSEGMENT"                    "TERMS_OF_PAYMENT"              "VAS_CODES_GROUP"              
# [21] "CUSTOMER_CLASSIFICATION_GROUP" "EDI_CODE"                      "TERMS_OF_BUSINESS"


### HOW MUCH MISSING DATA in THE TIBBLE???? ####
customers %>% summarise_each(funs(100*mean(is.na(.)))) # there are NA's in the data set.


### "CUSTOMER_ACCOUNT" ###
#  string integer
# 
# Get a count of the rows that fail conversion
(fail_count <-customers %>% filter( is.na(as.integer(CUSTOMER_ACCOUNT)) ) %>% count()) # 0

customer_account_col <- transmute(customers, customer_account = as.integer(CUSTOMER_ACCOUNT))
summary(customer_account_col)
# Min.   :100115              
# 1st Qu.:100293              
# Median :100468              
# Mean   :101302              
# 3rd Qu.:100647              
# Max.   :106028

# do some investigation on number of unique accounts and the aggregate counts
(unique_cnt <- distinct(customer_account_col, customer_account) %>% nrow)  # 676
# 
#                
# ### "NAME" ###                          
# string
# 
name_col <- select(customers, name = NAME)

# check how many unique name
(unique_cnt <- distinct(name_col, name) %>% nrow) # 676


# ### "COMPANY_CHAIN" ###                  
# string factor with 20 levels including NA
#
# How many distinct COMPANY_CHAIN?
(xdf <- distinct(customers, str_to_lower(COMPANY_CHAIN))) %>% nrow  # 18 distinct + NA. 21 incl dups due to capitalisation

company_chain_col <- transmute(customers, company_chain = as.factor(str_to_lower(COMPANY_CHAIN))) # lower case to remove dups
str(company_chain_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	676 obs. of  1 variable:
# $ company_chain: Factor w/ 18 levels "3point5","ballantynes",..: NA 16 NA NA NA NA NA NA NA NA ...
#   

# investigate company parent/child relationship
xdf <- group_by(company_chain_col, company_chain)
xdf %>% summarise(num_child = n()) %>% arrange(desc(num_child))
# company_chain num_child
#               <fctr>     <int>
# 1                NA       526
# 2  mountain designs        38
# 3           snowgum        28
# 4       shoe clinic        16
# 5      paddy pallin        14
# 6               r&r        12
# 7           3point5        11
# 8     purely merino        11
# 9          stirling         6
# 10     pack & pedal         4
# 11         mainpeak         2
# 12      ballantynes         1
# 13          bivouac         1
# 14          bunyips         1
# 15      david jones         1
# 16              h&j         1
# 17   outside sports         1
# 18              rd1         1
# 19      rebel sport         1

# xdf <- group_by(customers, COMPANY_CHAIN) %>% arrange(NAME)



# "CURRENCY"                     
# string factor
#
currency_col <- transmute(customers, currency = as.factor(CURRENCY))
summary(currency_col)
# currency
# AUD:293            
# NZD:383


# "CUR_CREDIT_LIMIT"              
# string numeric
#
# Get a count of the rows that fail conversion
(cur_credit_limit_col <-customers %>% filter( is.na(as.numeric(CUR_CREDIT_LIMIT))) %>% nrow()) # 0. all good

cur_credit_limit_col <- transmute(customers, cur_credit_limit = as.numeric(CUR_CREDIT_LIMIT))
str(cur_credit_limit_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	676 obs. of  1 variable:
#   $ cur_credit_limit: num  0 20000 0 0 0 10000 0 0 0 0 ...

summary(cur_credit_limit_col)
# cur_credit_limit
# Min.   :     0  
# 1st Qu.:     0  
# Median :  5000  
# Mean   : 27873  
# 3rd Qu.: 25000  
# Max.   :800000

# How many negative values?
filter(cur_credit_limit_col, cur_credit_limit < 0.0) %>% nrow  # 0

# Any zero values?
filter(cur_credit_limit_col, cur_credit_limit == 0.0) %>% nrow # 300

# Any NAs?
filter(cur_credit_limit_col, is.na(cur_credit_limit)) %>% nrow # 0

 
# "CREDIT_LIMIT"                  
# string numeric
#
# Get a count of the rows that fail conversion
(credit_limit_col <-customers %>% filter( is.na(as.numeric(CREDIT_LIMIT))) %>% nrow()) # 0. all good

credit_limit_col <- transmute(customers, credit_limit = as.numeric(CREDIT_LIMIT))
str(credit_limit_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	676 obs. of  1 variable:
#   $ credit_limit: num  0 20000 0 0 0 10000 0 0 0 0 ...

summary(credit_limit_col)
# credit_limit   
# Min.   :     0  
# 1st Qu.:     0  
# Median :  5433  
# Mean   : 29328  
# 3rd Qu.: 28179  
# Max.   :817578

# How many negative values?
filter(credit_limit_col, credit_limit < 0.0) %>% nrow  # 0

# Any zero values?
filter(credit_limit_col, credit_limit == 0.0) %>% nrow # 300

# Any NAs?
filter(credit_limit_col, is.na(credit_limit)) %>% nrow # 0


# "CUSTOMERS_PRICES_HANDLING"     
# String factor? 
#
# How many distinct COMPANY_CHAIN?
distinct(customers, CUSTOMERS_PRICES_HANDLING) %>% nrow  # 2

customers_prices_handling_col <- transmute(customers, customers_prices_handling = as.factor(CUSTOMERS_PRICES_HANDLING))
str(customers_prices_handling_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	676 obs. of  1 variable:
#   $ customers_prices_handling: Factor w/ 2 levels "None","Unexpected number of columns %1 with the value %2": 1 1 1 1 1 1 1 1 1 1 ...
#   
summary(customers_prices_handling_col)
# customers_prices_handling
# None                                             :664        
# Unexpected number of columns %1 with the value %2: 12


# "INVOICE_ACCOUNT"              
# string numeric
# 
# Get a count of the rows that fail conversion
(invoice_account_col <-customers %>% filter( is.na(as.numeric(INVOICE_ACCOUNT))) %>% nrow()) # 0. all good

invoice_account_col <- transmute(customers, invoice_account = as.numeric(INVOICE_ACCOUNT))
str(invoice_account_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	676 obs. of  1 variable:
#   $ invoice_account: num  100616 100617 100618 100620 100621 ...

summary(invoice_account_col)
# invoice_account 
# Min.   :100115  
# 1st Qu.:100271  
# Median :100462  
# Mean   :101294  
# 3rd Qu.:100646  
# Max.   :106028

# How many negative values?
filter(invoice_account_col, invoice_account < 0.0) %>% nrow  # 0

# Any zero values?
filter(invoice_account_col, invoice_account == 0.0) %>% nrow # 0

# Any NAs?
filter(invoice_account_col, is.na(invoice_account)) %>% nrow # 0


# "LINE_DISCOUNT"                 
# string factor:  20 levels including NAs
#
# How many distinct LINE_DISCOUNT?
distinct(customers, LINE_DISCOUNT) %>% nrow  # 20

line_discount_col <- transmute(customers, line_discount = as.factor(LINE_DISCOUNT))
str(line_discount_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	676 obs. of  1 variable:
#   $ line_discount: Factor w/ 19 levels "10% DISC","100% DISC",..: 7 NA NA 7 7 NA NA NA 7 7 ....
#   
summary(line_discount_col)
# 20% DISC:161  
# 5% DISC : 25  
# 2% DISC : 12  
# 4% DISC :  6  
# 10% DISC:  3  
# (Other) : 19  
# NA's    :450

 
# "LINE_OF_BUSINESS"              
# string factor
#
# How many distinct LINE_OF_BUSINESS?
distinct(customers, LINE_OF_BUSINESS) %>% nrow  # 20

line_of_business_col <- transmute(customers, line_of_business = as.factor(LINE_OF_BUSINESS))
str(line_of_business_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	676 obs. of  1 variable:
#   $ line_of_business: Factor w/ 6 levels "DIRECT","DIST(B2B)",..: 4 6 1 4 4 1 1 6 4 4 ...
#   
summary(line_of_business_col)
# line_of_business
# DIRECT    :131    
# DIST(B2B) :  2    
# ONLINE/CAT: 12    
# OTHER     :180    
# PHY&ONLINE:126    
# PHYSICAL  :225

 
# "MAIN_CUSTOMER_ACCOUNT"         
# logical
# 
# How many distinct MAIN_CUSTOMER_ACCOUNT values?
distinct(customers, MAIN_CUSTOMER_ACCOUNT) %>% nrow # No, Yes

main_customer_account_col <- transmute(customers, main_customer_account = as.logical(MAIN_CUSTOMER_ACCOUNT == "Yes"))
str(main_customer_account_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	676 obs. of  1 variable:
#   $ main_customer_account: logi  FALSE FALSE FALSE FALSE FALSE FALSE .....
#   
summary(main_customer_account_col)
# main_customer_account
# Mode :logical        
# FALSE:653            
# TRUE :23             
# NA's :0


# "MANDATORY_CREDIT_LIMIT"       
# logical
#
# How many distinct MANDATORY_CREDIT_LIMIT values?
distinct(customers, MANDATORY_CREDIT_LIMIT) %>% nrow # No, Yes

mandatory_credit_limit_col <- transmute(customers, mandatory_credit_limit = as.logical(MANDATORY_CREDIT_LIMIT == "Yes"))
str(mandatory_credit_limit_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	676 obs. of  1 variable:
#   $ mandatory_credit_limit: logi  FALSE TRUE FALSE FALSE FALSE TRUE ...
#   
summary(mandatory_credit_limit_col)
# mandatory_credit_limit
# Mode :logical         
# FALSE:279             
# TRUE :397             
# NA's :0


# "PRICE_GROUP"                   
# string factor
# 
# How many distinct PRICE_GROUP?
distinct(customers, PRICE_GROUP) %>% nrow  # 5

price_group_col <- transmute(customers, price_group = as.factor(PRICE_GROUP))
str(price_group_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	676 obs. of  1 variable:
#   $ price_group: Factor w/ 4 levels "LND","RT","WS",..: 3 3 3 3 3 3 2 3 3 3 ...
#   
summary(price_group_col)
# price_group
# LND :  1   
# RT  : 27   
# WS  :617   
# WSCO: 23   
# NA's:  8


# "PROJECT_CODE"                  
# string factor
# 
# How many distinct PROJECT_CODE?
distinct(customers, PROJECT_CODE) %>% nrow  # 20

project_code_col <- transmute(customers, project_code = as.factor(PROJECT_CODE))
str(project_code_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	676 obs. of  1 variable:
#   $ project_code: Factor w/ 19 levels "ANNAPURNA","APLINE",..: NA NA NA NA NA NA 6 NA NA NA ..
#   
summary(project_code_col)
# project_code
# CORPORATE : 21  
# APLINE    : 12  
# RANDR     : 12  
# DEPARTMENT:  7  
# PP        :  4  
# (Other)   : 18  
# NA's      :602


# "SALES_DISTRICT"                
# string factor
#
# How many distinct SALES_DISTRICT?
distinct(customers, SALES_DISTRICT) %>% nrow  # 28

sales_district_col <- transmute(customers, sales_district = as.factor(SALES_DISTRICT))
str(sales_district_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	676 obs. of  1 variable:
#   $ sales_district: Factor w/ 27 levels "ACT","BRIS","MELB",..: 11 12 12 11 11 20 20 11 11 11 ...
#   
summary(sales_district_col)
# sales_district
# NZ06   :127   
# NSW    : 52   
# SYD    : 44   
# VIC    : 40   
# MELB   : 38   
# (Other):230   
# NA's   :145


# "STATISTICS_GROUP"             
# string factor
#
# How many distinct STATISTICS_GROUP?
distinct(customers, STATISTICS_GROUP) %>% nrow  # 5

statistics_group_col <- transmute(customers, statistics_group = as.factor(STATISTICS_GROUP))
str(statistics_group_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	676 obs. of  1 variable:
#   $ statistics_group: Factor w/ 5 levels "ICEBREAKER","INDEPENDEN",..: 5 3 2 5 5 5 5 2 5 5 ....
#   
summary(statistics_group_col)
# statistics_group
# ICEBREAKER: 38    
# INDEPENDEN:234    
# NATLCHAIN :155    
# ONLINE    : 18    
# OTHER     :231

 
# "SEGMENT"                       
# string factor:   includes ICEBREAKER and staff
# 
# How many distinct SEGMENT?
distinct(customers, SEGMENT) %>% nrow  # 12

segment_col <- transmute(customers, segment = as.factor(SEGMENT))
str(segment_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	676 obs. of  1 variable:
#   $ segment: Factor w/ 12 levels "HUNTFISH","ICEBREAKER",..: 2 8 5 2 2 5 5 6 2 2 ..
#   
summary(segment_col)
# segment   
# OUTDOOR         :194  
# ICEBREAKER      :180  
# OTHER           :117  
# LIFESTYLE       : 61  
# SKI             : 30  
# OTHER SPECIALITY: 20  
# (Other)         : 74


# "SUBSEGMENT"                    
# string factor:   includes lots of ICEBREAKER staff (IB STAFF) !
# 
# How many distinct SUBSEGMENT?
distinct(customers, SUBSEGMENT) %>% nrow  # 23

subsegment_col <- transmute(customers, subsegment = as.factor(SUBSEGMENT))
str(subsegment_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	676 obs. of  1 variable:
#   $ subsegment: Factor w/ 23 levels "ALPINE","CORPORATE",..: 10 22 16 10 10 2 2 15 10 10 ...
#   
summary(subsegment_col)
# subsegment 
# OUTDOOR             :194  
# IB STAFF            :175  
# CORPORATE           : 75  
# TOURIST/TRVL/AIRPORT: 43  
# GENERAL SPORTS      : 28  
# ALPINE              : 26  
# (Other)             :135


# "TERMS_OF_PAYMENT"              
# string factor
#
# How many distinct TERMS_OF_PAYMENT?
distinct(customers, TERMS_OF_PAYMENT) %>% nrow  # 17

terms_of_payment_col <- transmute(customers, terms_of_payment = as.factor(TERMS_OF_PAYMENT))
str(terms_of_payment_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	676 obs. of  1 variable:
#   $ terms_of_payment: Factor w/ 16 levels "CM+1M(10)","CM+1M(25)",..: 16 5 5 16 16 5 15 5 16 16 ...
#   
summary(terms_of_payment_col)
# terms_of_payment
# CM+20D   :203    
# CM+30D   :164    
# NET0     :141    
# Net0     : 70    
# CM+1M(25): 37    
# (Other)  : 59    
# NA's     :  2

 
# "VAS_CODES_GROUP"              
# string factor
#
# How many distinct VAS_CODES_GROUP?
distinct(customers, VAS_CODES_GROUP) %>% nrow  # 3

vas_codes_group_col <- transmute(customers, vas_codes_group = as.factor(VAS_CODES_GROUP))
str(vas_codes_group_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	676 obs. of  1 variable:
#   $ vas_codes_group: Factor w/ 2 levels "TICKET","TICKET_RRP": NA NA NA NA NA NA NA NA NA NA ...
#   
summary(vas_codes_group_col)
# vas_codes_group
# TICKET    :  2   
# TICKET_RRP: 22   
# NA's      :652

 
# "CUSTOMER_CLASSIFICATION_GROUP" 
# string factor
#
# How many distinct CUSTOMER_CLASSIFICATION_GROUP?
distinct(customers, CUSTOMER_CLASSIFICATION_GROUP) %>% nrow  # 4

customer_classification_group_col <- transmute(customers, customer_classification_group = as.factor(CUSTOMER_CLASSIFICATION_GROUP))
str(customer_classification_group_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	676 obs. of  1 variable:
#   $ customer_classification_group: Factor w/ 4 levels "A","B","C","D": 4 4 4 4 4 4 4 4 4 4 ...
#   
summary(customer_classification_group_col)
# customer_classification_group
# A: 53                        
# B: 81                        
# C:154                        
# D:388

 
# "EDI_CODE"                      
# string factor
#
# How many distinct EDI_CODE?
distinct(customers, EDI_CODE) %>% nrow  # 17

edi_code_col <- transmute(customers, edi_code = as.factor(EDI_CODE))
str(edi_code_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	676 obs. of  1 variable:
#   $ edi_code: Factor w/ 16 levels "ANZCS","AUS3P5",..: NA NA NA NA NA NA NA NA NA NA ...
#   
summary(edi_code_col)
# edi_code  
# ANZCS     : 47  
# NZLRRSPORT: 14  
# AUSSNOW   : 10  
# NZL3P5    :  9  
# AUS3P5    :  2  
# (Other)   : 11  
# NA's      :583


# "TERMS_OF_BUSINESS"
# string factor
# 
# How many distinct TERMS_OF_BUSINESS?
distinct(customers, TERMS_OF_BUSINESS) %>% nrow  # 3

terms_of_business_col <- transmute(customers, terms_of_business = as.factor(TERMS_OF_BUSINESS))
str(terms_of_business_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	676 obs. of  1 variable:
#   $ terms_of_business: Factor w/ 3 levels "Consignment",..: 3 2 3 3 3 3 3 3 3 3 ...
#   
summary(terms_of_business_col)
# terms_of_business
# Consignment   :  5    
# Sale or Return: 38    
# Standard      :633


### BIND INTO A NEW DATAFRAME / TIBBLE
# [1] "CUSTOMER_ACCOUNT"              "NAME"                          "COMPANY_CHAIN"                 "CURRENCY"                     
# [5] "CUR_CREDIT_LIMIT"              "CREDIT_LIMIT"                  "CUSTOMERS_PRICES_HANDLING"     "INVOICE_ACCOUNT"              
# [9] "LINE_DISCOUNT"                 "LINE_OF_BUSINESS"              "MAIN_CUSTOMER_ACCOUNT"         "MANDATORY_CREDIT_LIMIT"       
# [13] "PRICE_GROUP"                   "PROJECT_CODE"                  "SALES_DISTRICT"                "STATISTICS_GROUP"             
# [17] "SEGMENT"                       "SUBSEGMENT"                    "TERMS_OF_PAYMENT"              "VAS_CODES_GROUP"              
# [21] "CUSTOMER_CLASSIFICATION_GROUP" "EDI_CODE"                      "TERMS_OF_BUSINESS

xdf <- cbind(
      customer_account_col,
      name_col,
      company_chain_col,
      currency_col,
      cur_credit_limit_col,
      credit_limit_col,
      customers_prices_handling_col,
      invoice_account_col,
      line_discount_col,
      line_of_business_col,
      main_customer_account_col,
      mandatory_credit_limit_col,
      price_group_col,
      project_code_col,
      sales_district_col,
      statistics_group_col,
      segment_col,
      subsegment_col,
      terms_of_payment_col,
      vas_codes_group_col,
      customer_classification_group_col,
      edi_code_col,
      terms_of_business_col)

if (interactive()) View(xdf) # lets look at the final data frame...

str(xdf)
# 'data.frame':	676 obs. of  23 variables:
# $ customer_account             : int  100616 100617 100618 100620 100621 100622 100623 100624 100625 100626 ...
# $ name                         : chr  "Scott McNab" "Shoe Clinic Masterton" "The Catwalk Trust - Staff" "Maya Brown (Gaebler)" ...
# $ company_chain                : Factor w/ 20 levels "3Point5","3POINT5",..: NA 18 NA NA NA NA NA NA NA NA ...
# $ currency                     : Factor w/ 2 levels "AUD","NZD": 2 2 2 2 2 2 2 2 2 2 ...
# $ cur_credit_limit             : num  0 20000 0 0 0 10000 0 0 0 0 ...
# $ credit_limit                 : num  0 20000 0 0 0 10000 0 0 0 0 ...
# $ customers_prices_handling    : Factor w/ 2 levels "None","Unexpected number of columns %1 with the value %2": 1 1 1 1 1 1 1 1 1 1 ...
# $ invoice_account              : num  100616 100617 100618 100620 100621 ...
# $ line_discount                : Factor w/ 19 levels "10% DISC","100% DISC",..: 7 NA NA 7 7 NA NA NA 7 7 ...
# $ line_of_business             : Factor w/ 6 levels "DIRECT","DIST(B2B)",..: 4 6 1 4 4 1 1 6 4 4 ...
# $ main_customer_account        : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
# $ mandatory_credit_limit       : logi  FALSE TRUE FALSE FALSE FALSE TRUE ...
# $ price_group                  : Factor w/ 4 levels "LND","RT","WS",..: 3 3 3 3 3 3 2 3 3 3 ...
# $ project_code                 : Factor w/ 19 levels "ANNAPURNA","APLINE",..: NA NA NA NA NA NA 6 NA NA NA ...
# $ sales_district               : Factor w/ 27 levels "ACT","BRIS","MELB",..: 11 12 12 11 11 20 20 11 11 11 ...
# $ statistics_group             : Factor w/ 5 levels "ICEBREAKER","INDEPENDEN",..: 5 3 2 5 5 5 5 2 5 5 ...
# $ segment                      : Factor w/ 12 levels "HUNTFISH","ICEBREAKER",..: 2 8 5 2 2 5 5 6 2 2 ...
# $ subsegment                   : Factor w/ 23 levels "ALPINE","CORPORATE",..: 10 22 16 10 10 2 2 15 10 10 ...
# $ terms_of_payment             : Factor w/ 16 levels "CM+1M(10)","CM+1M(25)",..: 16 5 5 16 16 5 15 5 16 16 ...
# $ vas_codes_group              : Factor w/ 2 levels "TICKET","TICKET_RRP": NA NA NA NA NA NA NA NA NA NA ...
# $ customer_classification_group: Factor w/ 4 levels "A","B","C","D": 4 4 4 4 4 4 4 4 4 4 ...
# $ edi_code                     : Factor w/ 16 levels "ANZCS","AUS3P5",..: NA NA NA NA NA NA NA NA NA NA ...
# $ terms_of_business            : Factor w/ 3 levels "Consignment",..: 3 2 3 3 3 3 3 3 3 3 ...

summary(xdf)
# customer_account     name                    company_chain currency            cur_credit_limit  credit_limit   
# Min.   :100115   Length:676         Mountain Designs: 38   AUD:293             Min.   :     0   Min.   :     0  
# 1st Qu.:100293   Class :character   Snowgum         : 28   NZD:383             1st Qu.:     0   1st Qu.:     0  
# Median :100468   Mode  :character   SHOE CLINIC     : 15                       Median :  5000   Median :  5433  
# Mean   :101302                      Paddy Pallin    : 14                       Mean   : 27873   Mean   : 29328  
# 3rd Qu.:100647                      R&R             : 12                       3rd Qu.: 25000   3rd Qu.: 28179  
# Max.   :106028                      (Other)         : 43                       Max.   :800000   Max.   :817578  
#                                     NA's            :526                                                        
#
# customers_prices_handling invoice_account   line_discount   line_of_business main_customer_account
# None       :664             Min.   :100115   20% DISC:161   DIRECT    :131     Mode :logical        
# Unexpected : 12             1st Qu.:100271   5% DISC : 25   DIST(B2B) :  2     FALSE:653            
#                             Median :100462   2% DISC : 12   ONLINE/CAT: 12     TRUE :23             
#                             Mean   :101294   4% DISC :  6   OTHER     :180     NA's :0              
#                             3rd Qu.:100646   10% DISC:  3   PHY&ONLINE:126                          
#                             Max.   :106028   (Other) : 19   PHYSICAL  :225                          
#                             NA's    :450  
# 
# mandatory_credit_limit price_group     project_code sales_district   statistics_group             segment                   subsegment 
# Mode :logical          LND :  1    CORPORATE : 21   NZ06   :127    ICEBREAKER: 38     OUTDOOR         :194   OUTDOOR             :194  
# FALSE:279              RT  : 27    APLINE    : 12   NSW    : 52    INDEPENDEN:234     ICEBREAKER      :180   IB STAFF            :175  
# TRUE :397              WS  :617    RANDR     : 12   SYD    : 44    NATLCHAIN :155     OTHER           :117   CORPORATE           : 75  
# NA's :0                WSCO: 23    DEPARTMENT:  7   VIC    : 40    ONLINE    : 18     LIFESTYLE       : 61   TOURIST/TRVL/AIRPORT: 43  
#                        NA's:  8    PP        :  4   MELB   : 38    OTHER     :231     SKI             : 30   GENERAL SPORTS      : 28  
#                                    (Other)   : 18   (Other):230                       OTHER SPECIALITY: 20   ALPINE              : 26  
#                                    NA's      :602   NA's   :145                       (Other)         : 74   (Other)             :135  
#
# terms_of_payment   vas_codes_group customer_classification_group       edi_code        terms_of_business
# CM+20D   :203     TICKET    :  2    A: 53                         ANZCS     : 47   Consignment   :  5    
# CM+30D   :164     TICKET_RRP: 22    B: 81                         NZLRRSPORT: 14   Sale or Return: 38    
# NET0     :141     NA's      :652    C:154                         AUSSNOW   : 10   Standard      :633    
# Net0     : 70                       D:388                         NZL3P5    :  9                         
# CM+1M(25): 37                                                     AUS3P5    :  2                         
# (Other)  : 59                                                     (Other)   : 11                         
# NA's     :  2                                                     NA's      :583 

# write customer out as a feather file
write_feather(xdf,"data/customer.feather")
