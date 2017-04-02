#### IMPORT_DASHDB_TABLES ####
#
# updated 01/04/2017. takes 8 - 10min to run.
# 
# added in new tables
# 
rm(list = ls()) # clear workspace as a precaution
# setwd("C:/Users/rp/Projects/Icebreaker_rp") # location to store the files

# LIBRARIES
require(ibmdbR)
require(feather)

# Credentials and connect to Dashdb
dsn_driver <- "dashdb"     # DSN name defined on RP's supercomputer by the dashdb script
dsn_database <- "BLUDB"   
dsn_hostname <- "dashdb-entry-yp-dal09-08.services.dal.bluemix.net"  
dsn_port <- "50000"  
dsn_protocol <- "TCPIP"  
dsn_uid <- "dash8666"  
dsn_pwd <- "5c74a7ed2ecb" 
 
conn_path <- paste(dsn_driver,  
      ";DATABASE=",dsn_database,
      ";HOSTNAME=",dsn_hostname,
      ";PORT=",dsn_port,
      ";PROTOCOL=",dsn_protocol,
      ";UID=",dsn_uid,
      ";PWD=",dsn_pwd,sep="")
 
ch <- idaConnect(conn_path)  
idaInit(ch)  


# Create in-database pointers
CUSTOMER_TABLE_FINAL = ida.data.frame('CUSTOMER_TABLE_FINAL')
CUSTOMER_INVOICE_JOURNAL = ida.data.frame('CUSTOMER_INVOICE_JOURNAL')
CUST_INVOICE_TRANS = ida.data.frame('CUST_INVOICE_TRANS')
CUST_SETTLEMENT = ida.data.frame('CUST_SETTLEMENT')
FV_VOUCHER = ida.data.frame('FV_VOUCHER')
PRODUCT_HIERARCHY_MAPPING = ida.data.frame('PRODUCT_HIERARCHY_MAPPING')
RETURN_REASON_CODES = ida.data.frame('RETURN_REASON_CODES')
SALES_ORDER = ida.data.frame('SALES_ORDER')
SEASON_DATES = ida.data.frame('SEASON_DATES')
WAREHOUSING_COSTS = ida.data.frame('WAREHOUSING_COSTS')


# Change ida data.frame pointer to local R data.frame
R_CUSTOMER_TABLE_FINAL = as.data.frame(CUSTOMER_TABLE_FINAL)
R_CUSTOMER_INVOICE_JOURNAL = as.data.frame(CUSTOMER_INVOICE_JOURNAL)
R_CUST_INVOICE_TRANS = as.data.frame(CUST_INVOICE_TRANS)
R_CUST_SETTLEMENT = as.data.frame(CUST_SETTLEMENT)
R_FV_VOUCHER = as.data.frame(FV_VOUCHER)
R_PRODUCT_HIERARCHY_MAPPING = as.data.frame(PRODUCT_HIERARCHY_MAPPING)
R_RETURN_REASON_CODES = as.data.frame(RETURN_REASON_CODES)
R_SALES_ORDER = as.data.frame(SALES_ORDER)
R_SEASON_DATES = as.data.frame(SEASON_DATES)
R_WAREHOUSING_COSTS = as.data.frame(WAREHOUSING_COSTS)



# WRITE data.frames AS FEATHER FILES THAT CAN BE READ BY BOTH R AND PYTHON
write_feather(R_CUSTOMER_TABLE_FINAL, "data/R_CUSTOMER_TABLE_FINAL.feather")
write_feather(R_CUSTOMER_INVOICE_JOURNAL, "data/R_CUSTOMER_INVOICE_JOURNAL.feather")
write_feather(R_CUST_INVOICE_TRANS, "data/R_CUST_INVOICE_TRANS.feather")
write_feather(R_CUST_SETTLEMENT, "data/R_CUST_SETTLEMENT.feather")
write_feather(R_FV_VOUCHER, "data/R_FV_VOUCHER.feather")
write_feather(R_PRODUCT_HIERARCHY_MAPPING, "data/R_PRODUCT_HIERARCHY.feather")
write_feather(R_RETURN_REASON_CODES, "data/R_RETURN_REASON_CODES.feather")
write_feather(R_SALES_ORDER, "data/R_SALES_ORDER.feather")
write_feather(R_SEASON_DATES, "data/R_SEASON_DATES.feather")
write_feather(R_WAREHOUSING_COSTS, "data/R_WAREHOUSING_COSTS.feather")



# ALSO WRITE OUT AS CSV FILES
# write.csv(R_CUSTOMER_TABLE_FINAL, "data/R_CUSTOMER_TABLE_FINAL.csv", row.names = FALSE, na="")
# write.csv(R_CUSTOMER_INVOICE_JOURNAL, "data/R_CUSTOMER_INVOICE_JOURNAL.csv", row.names = FALSE, na="") # omit NA's
# write.csv(R_CUST_INVOICE_TRANS, "data/R_CUST_INVOICE_TRANS.csv", row.names = FALSE, na="")
# write.csv(R_CUST_SETTLEMENT, "data/R_CUST_SETTLEMENT.csv", row.names = FALSE, na="")
# write.cdv(R_FV_VOUCHER, "data/R_FV_VOUCHER.csv", row.names = FALSE, na="")
# write.csv(R_PRODUCT_HIERARCHY_MAPPING, "data/R_PRODUCT_HIERARCHY.csv", row.names = FALSE, na="")
# write.csv(R_RETURN_REASON_CODES, "data/R_RETURN_REASON_CODES.csv", row.names = FALSE, na="")
# write.csv(R_SALES_ORDER, "data/R_SALES_ORDER.csv", row.names = FALSE, na="")
# write.csv(R_SEASON_DATES, "data/R_SEASON_DATES.csv", row.names = FALSE, na="")
# write.csv(R_WAREHOUSING_COSTS, "data/R_WAREHOUSING_COSTS.feather", row.names = FALSE, na="")
 

### close the database connection ###
idaClose(ch)
