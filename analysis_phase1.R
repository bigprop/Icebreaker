rm(list=ls()) # clear the workspace as precaution

require(feather)
require(tidyverse)
require(dplyr)

# import the feather files containing the tibble data frames for the analysis
# 
setwd("C:/Users/rp/Projects/Icebreaker_rp")
customer <- read_feather("customer.feather")
invoice_journal <- read_feather("invoice_journal.feather")
invoice_trans <- read_feather("invoice_trans.feather")

### look at joining invoice_trans and invoice_journal
# start with an example of where it works...
(xdf <- invoice_trans %>% left_join(invoice_journal, c("sales_id" = "sales_id"))) %>% nrow # 899379. more than i expect. join on multiple copies in invoice_journal?
# sales_id = 117238 is on that has matches in both invoice_trans and invoice_journal
#
# cONFIRM IN CLPPLUS DIRECT TO DASHDB.  CONFIRMED.
# SELECT SALES_ID FROM CUST_INVOICE_TRANS WHERE SALES_ID = 'SO00117238';
# SELECT SALES_ORDER FROM CUSTOMER_INVOICE_JOURNAL WHERE SALES_ORDER = 'SO00117238';


# xdf <- merge(invoice_trans, invoice_journal, by.x="sales_id", by.y="sales_order") # merges gives the more control. also consider using sqldef package

### 14201 sales_id that dont have a matching sales_order !!!!
(anti_xdf <- invoice_trans %>% anti_join(invoice_journal, c("sales_id" = "sales_order"))) %>% nrow # 14201

# CONFIRMED IN THE DASHDB USING CPPLUS
# SELECT COUNT(1) FROM CUST_INVOICE_TRANS LEFT JOIN CUSTOMER_INVOICE_JOURNAL ON CUST_INVOICE_TRANS.SALES_ID = CUSTOMER_INVOICE_JOURNAL.SALES_ORDER WHERE CUSTOMER_INVOICE_JOURNAL.SALES_ORDER IS NULL;

# example sales_id 116627 which fails
filter(invoice_trans, sales_id == 116627) # returns a single row

# try and match with a sales_order in the invoice_journal
filter(invoice_journal, sales_order == 116627) # No match !
 
# cONFIRM IN CLPPLUS DIRECT TO DASHD.  CONFIRMED.
# SELECT SALES_ID FROM CUST_INVOICE_TRANS WHERE SALES_ID = 'SO00116627';
# SELECT SALES_ORDER FROM CUSTOMER_INVOICE_JOURNAL WHERE SALES_ORDER = 'SO00116627';

### 6412 sales_orders dont have any matching sales_id transaction
(anti_ydf <- invoice_journal %>% anti_join(invoice_trans, c("sales_order" = "sales_id"))) %>% nrow  # 6412

# CONFIRM IN DASHDB
#SELECT COUNT(1) FROM CUSTOMER_INVOICE_JOURNAL LEFT JOIN CUST_INVOICE_TRANS ON CUSTOMER_INVOICE_JOURNAL.SALES_ORDER = CUST_INVOICE_TRANS.SALES_ID WHERE CUST_INVOICE_TRANS.SALES_ID IS NULL;

# examples sales_order 157750
filter(invoice_journal, sales_order == 157750) # returns a single row

# try and match with a sales_id in the invoice_trans
filter(invoice_trans, sales_id == 157750) # No match !

# cONFIRM IN CLPPLUS DIRECT TO DASHD.  CONFIRMED.
# SELECT SALES_ID FROM CUST_INVOICE_TRANS WHERE SALES_ID = 'SO00157750';
# SELECT SALES_ORDER FROM CUSTOMER_INVOICE_JOURNAL WHERE SALES_ORDER = 'SO00157750';



### look at the join between invoice_trans and customer...
### 
### 62 invoice_journal invoice_accounts dont have a match in customer
(anti_zdf <- invoice_journal %>% anti_join(customer, c("invoice_account" = "invoice_account"))) %>% nrow # 62
# CONFIRMED
# SELECT COUNT(1) FROM CUSTOMER_INVOICE_JOURNAL LEFT JOIN CUSTOMER_TABLE_FINAL ON CUSTOMER_INVOICE_JOURNAL.INVOICE_ACCOUNT = CUSTOMER_TABLE_FINAL.INVOICE_ACCOUNT WHERE CUSTOMER_TABLE_FINAL.INVOICE_ACCOUNT IS NULL;

### 318 customer invoice_account dont have a matching invoice_journal entry!
(anti_adf <- customer %>% anti_join(invoice_journal, c("invoice_account" = "invoice_account"))) %>% nrow # 318
# CONFIRMED
# SELECT COUNT(1) FROM CUSTOMER_TABLE_FINAL LEFT JOIN CUSTOMER_INVOICE_JOURNAL ON CUSTOMER_TABLE_FINAL.INVOICE_ACCOUNT = CUSTOMER_INVOICE_JOURNAL.INVOICE_ACCOUNT WHERE CUSTOMER_INVOICE_JOURNAL.INVOICE_ACCOUNT IS NULL;




# example invoice_account 100485
filter(customer, invoice_account == 100485) # 1 row Brooke Riley
filter(invoice_journal, invoice_account == 100485)  # 0 No match.
filter(invoice_journal, customer_account == 100485) # 0 No match.
