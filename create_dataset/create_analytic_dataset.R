#### CREATE_ANALYTIC_DATASET ####

rm(list = ls()) # clear the workspace as a precaution

require(feather)
require(plyr)   # load plyr before dplyr (tidyverse) so can use the merge function
require(tidyverse)
require(stringr)
# require(bit64)

# options(scipen=999) # force not to use scientific notation for number display
# options(scipen=0) # default value
setwd("C:/Users/rp/Projects/icebreaker_rp")


### 1. JOIN TRANSACTIONS AND PRODUCT HIERARCHY on STYLE
#
# INSTANTIATE tibbles from feather
invoice_trans <- read_feather("invoice_trans.feather")          # pre sorted by style
product_hierarchy <- read_feather("product_hierarchy.feather")  # pre sorted by style

# expect to join on "style" column in both tibbles
# do anti-join to see if there are invoice_trans styles that dont have a match in the product hierarchy
(mismatch_df1 <- invoice_trans %>% anti_join(product_hierarchy, by="style") %>% distinct(style) %>% arrange(style)) %>% nrow # !!! 199 distinct styles missing from product hierarchy!!!

# use plyr package to do a left outer join - preserves all records in the transaction table to give us enriched transaction dataframe
xdf <- merge(invoice_trans, product_hierarchy, by="style", all.x=TRUE) 
invoice_trans_enriched <- xdf %>%
  select(invoice_number, document_number_prefix:sales_price, style, style_name:class) %>%
  arrange( desc(invoice_number) ) # sort by invoice number desc to assist with join

rm(xdf); rm(invoice_trans); rm(product_hierarchy) # free up memory. delete source dataframes.


### 2. JOIN INVOICE JOURNAL TO THE enriched TRANSACTIONS on invoice_number  
invoice_journal <- read_feather("invoice_journal.feather")      # pre sorted by invoice_number desc

# test how many of our enriched transactions dont have a matching invoice journal entry!
(mismatch_df2 <- invoice_journal %>% anti_join(invoice_trans_enriched, by="invoice_number") %>% distinct(invoice_number) %>% arrange(invoice_number)) %>% nrow # !!! 36042 !!!

# use plyr package to do a left outer join - perserves all the records in the invoice_journal to give us an enriched transaction journal dataframe
ydf <- merge(invoice_journal, invoice_trans_enriched, by="invoice_number", all.x=TRUE)
invoice_journal_enriched <- ydf %>% 
  select(invoice_date, invoice_prefix, invoice_number:class) %>% 
  arrange(desc(invoice_date), desc(invoice_number))  

write_feather(invoice_journal_enriched, "invoice_journal_enriched.feather") #  sorted most recent date, most recent invoice

rm(ydf); rm(invoice_journal); rm(invoice_trans_enriched) # free up memory. delete source dataframes.



### 3. JOIN CUSTOMER TABLE entries with matching enriched INVOICE JOURNAL records on customer_account  ...or should it be invoice_account?
customer <- read_feather("customer.feather")      # pre sorted by customer_account desc

# do anti-join to see if there are customers that dont have any matching invoice journal records...
(mismatch_df3 <- customer %>% anti_join(invoice_journal_enriched, by="customer_account") %>% arrange(customer_account)) %>% nrow # !!! 333 !!!

# plyr package to do a left inner join - only match the customer_account that have match in the invoice_journal_enriched.
zdf <- merge(customer, invoice_journal_enriched, by="customer_account")

customer_invoice_trans <- arrange(zdf, desc(customer_account), desc(invoice_date), desc(invoice_number))

write_feather(customer_invoice_trans, "customer_invoice_trans.feather")
