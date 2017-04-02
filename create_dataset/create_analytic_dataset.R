#### CREATE_ANALYTIC_DATASET ####

rm(list = ls()) # clear the workspace as a precaution

require(feather)
require(plyr)   # load plyr before dplyr (tidyverse) so can use the merge function
require(tidyverse)
require(stringr)
# require(bit64)

# options(scipen=999) # force not to use scientific notation for number display
# options(scipen=0) # default value
# setwd("C:/Users/rp/Projects/icebreaker_rp")


### 1. JOIN TRANSACTIONS AND PRODUCT HIERARCHY on STYLE
#
# INSTANTIATE tibbles from feather
invoice_trans <- read_feather("data/invoice_trans.feather")          # pre sorted by style
nrow(invoice_trans) # 513153

product_hierarchy <- read_feather("data/product_hierarchy.feather")  # pre sorted by style
nrow(product_hierarchy) # 2340

# expect to join on "style" column in both tibbles
# do anti-join to see if there are invoice_trans styles that dont have a match in the product hierarchy
(mismatch_df1 <- invoice_trans %>% anti_join(product_hierarchy, by="style") %>% distinct(style) %>% arrange(style)) %>% nrow # !!! 199 distinct styles missing from product hierarchy!!!

# use plyr package to do a left outer join - preserves all records in the LHS transaction table to give us enriched transaction dataframe
xdf <- merge(invoice_trans, product_hierarchy, by="style", all.x=TRUE) 
invoice_trans_enriched <- xdf %>%
  select(invoice_number, document_number_prefix:sales_price, style, style_name:class) %>%
  arrange( desc(invoice_number) ) # sort by invoice number desc to assist with join

rm(xdf); rm(invoice_trans); rm(product_hierarchy) # free up memory. delete source dataframes.

nrow(invoice_trans_enriched); ncol(invoice_trans_enriched) # (513153, 17) 
colnames(invoice_trans_enriched)
# [1] "invoice_number"         "document_number_prefix" "document_number_suffix" "currency_code"          "line_amt_pre_discount"  "line_amt_post_discount"
# [7] "invoiced_discount_acy"  "cogs"                   "invoiced_qty"           "sales_price"            "style"                  "style_name"            
# [13] "gender"                 "type"                   "type_1"                 "category"               "class" 



### 2. JOIN INVOICE JOURNAL TO THE enriched TRANSACTIONS on invoice_number  
invoice_journal <- read_feather("data/invoice_journal.feather")      # pre sorted by invoice_number desc
nrow(invoice_journal) # 5462 invoices vs 513153 transactions

# check that each invoice_journal no dups?
group_by(invoice_journal, invoice_number) %>% filter( n() > 1 ) %>% nrow # 0. therefore no dups.

# test how many invoice journal entry dont have any enriched transactions
(mismatch_df2 <- invoice_journal %>% anti_join(invoice_trans_enriched, by="invoice_number") %>% distinct(invoice_number) %>% arrange(invoice_number)) %>% nrow # 4289 fail. 1173 match


# use dplyr package to do a left inner join - only keep the records in the invoice_journal that have a matching enriched transaction(s)
ydf <- merge(invoice_journal, invoice_trans_enriched, by="invoice_number", all.x=FALSE)
ydf <- inner_join(invoice_journal, invoice_trans_enriched, by="invoice_number")
distinct(ydf, invoice_number) %>% nrow


invoice_journal_enriched <- ydf %>% 
  select(invoice_date, invoice_prefix, invoice_number:class) %>% 
  arrange(desc(invoice_date), desc(invoice_number))  

write_feather(invoice_journal_enriched, "data/invoice_journal_enriched.feather") #  sorted most recent date, most recent invoice

# rm(ydf); rm(invoice_journal); rm(invoice_trans_enriched) # free up memory. delete source dataframes.



### 3. JOIN CUSTOMER TABLE entries with matching enriched INVOICE JOURNAL records on customer_account  ...or should it be invoice_account?
customer <- read_feather("data/customer.feather")      # pre sorted by customer_account desc

# do anti-join to see if there are customers that dont have any matching invoice journal records...
(mismatch_df3 <- customer %>% anti_join(invoice_journal_enriched, by="customer_account") %>% arrange(customer_account)) %>% nrow # !!! 333 !!!

# plyr package to do a left inner join - only match the customer_account that have match in the invoice_journal_enriched.
zdf <- merge(customer, invoice_journal_enriched, by="customer_account")

customer_invoice_trans <- arrange(zdf, desc(customer_account), desc(invoice_date), desc(invoice_number))

write_feather(customer_invoice_trans, "data/customer_invoice_trans.feather")

colnames(customer_invoice_trans)
# [1] "customer_account"              "name"                          "company_chain"                 "currency.x"                    "cur_credit_limit"             
# [6] "credit_limit"                  "customers_prices_handling"     "invoice_account.x"             "line_discount.x"               "line_of_business"             
# [11] "main_customer_account"         "mandatory_credit_limit"        "price_group"                   "project_code"                  "sales_district"               
# [16] "statistics_group"              "segment"                       "subsegment"                    "terms_of_payment.x"            "vas_codes_group"              
# [21] "customer_classification_group" "edi_code"                      "terms_of_business"             "invoice_date"                  "invoice_prefix"               
# [26] "invoice_number"                "invoice_account.y"             "sales_order_prefix"            "sales_order"                   "sales_id"                     
# [31] "voucher_prefix"                "voucher"                       "invoice_amount"                "credit"                        "cash_dis_val"                 
# [36] "cash_dis_code"                 "delivery_terms"                "department"                    "dimension"                     "dimension_account"            
# [41] "line_discount.y"               "order_type"                    "ordertype2"                    "return_reason_code"            "sales_subtotal_amount"        
# [46] "sales_taker"                   "terms_of_payment.y"            "invoice_amount_ac"             "line_discount_ac"              "sales_subtotal_amount_ac"     
# [51] "sales_tax_ac"                  "charges_ac"                    "currency.y"                    "charges"                       "warehouse"                    
# [56] "document_number_prefix"        "document_number_suffix"        "currency_code"                 "line_amt_pre_discount"         "line_amt_post_discount"       
# [61] "invoiced_discount_acy"         "cogs"                          "invoiced_qty"                  "sales_price"                   "style"                        
# [66] "style_name"                    "gender"                        "type"                          "type_1"                        "category"                     
# [71] "class"
