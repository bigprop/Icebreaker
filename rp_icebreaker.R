# updated 29/03/2017. takes 8 - 10min to run.
# 
# added in new tables
# 
rm(list = ls()) # clear workspace as a precaution
setwd("C:/Users/rp/Projects/Icebreaker_rp") # location to store the files

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
CUSTOMER_INVOICE_JOURNAL = ida.data.frame('CUSTOMER_INVOICE_JOURNAL')
CUST_INVOICE_TRANS = ida.data.frame('CUST_INVOICE_TRANS')
CUSTOMER_TABLE_FINAL = ida.data.frame('CUSTOMER_TABLE_FINAL')
RETURN_REASON_CODES = ida.data.frame('RETURN_REASON_CODES')
WAREHOUSING_COSTS = ida.data.frame('WAREHOUSING_COSTS')
PRODUCT_HIERARCHY_MAPPING = ida.data.frame('PRODUCT_HIERARCHY_MAPPING')

# Change ida data.frame pointer to local R data.frame
R_CUSTOMER_INVOICE_JOURNAL <- as.data.frame(CUSTOMER_INVOICE_JOURNAL)
R_CUST_INVOICE_TRANS <- as.data.frame(CUST_INVOICE_TRANS)
R_CUSTOMER_TABLE_FINAL = as.data.frame(CUSTOMER_TABLE_FINAL)
R_RETURN_REASON_CODES = as.data.frame(RETURN_REASON_CODES)
R_WAREHOUSING_COSTS = as.data.frame(WAREHOUSING_COSTS)
R_PRODUCT_HIERARCHY_MAPPING = as.data.frame(PRODUCT_HIERARCHY_MAPPING)



# WRITE data.frames AS FEATHER FILES THAT CAN BE READ BY BOTH R AND PYTHON
write_feather(R_CUSTOMER_INVOICE_JOURNAL, "R_CUSTOMER_INVOICE_JOURNAL.feather")
write_feather(R_CUST_INVOICE_TRANS, "R_CUST_INVOICE_TRANS.feather")
write_feather(R_CUSTOMER_TABLE_FINAL, "R_CUSTOMER_TABLE_FINAL.feather")
write_feather(R_RETURN_REASON_CODES, "R_RETURN_REASON_CODES.feather")
write_feather(R_WAREHOUSING_COSTS, "R_WAREHOUSING_COSTS.feather")
write_feather(R_PRODUCT_HIERARCHY_MAPPING, "R_PRODUCT_HIERARCHY.feather")



# ALSO WRITE OUT AS CSV FILES
# write.csv(R_CUSTOMER_INVOICE_JOURNAL, "R_CUSTOMER_INVOICE_JOURNAL.csv", row.names = FALSE, na="") # omit NA's
# write.csv(R_CUST_INVOICE_TRANS, "R_CUST_INVOICE_TRANS.csv", row.names = FALSE, na="")
# write.csv(R_CUSTOMER_TABLE_FINAL, "R_CUSTOMER_TABLE_FINAL.csv", row.names = FALSE, na="")
# write.csv(R_RETURN_REASON_CODES, "R_RETURN_REASON_CODES.feather", row.names = FALSE, na="")
# write.csv(R_WAREHOUSING_COSTS, "R_WAREHOUSING_COSTS.feather", row.names = FALSE, na="")
# write.csv(R_PRODUCT_HIERARCHY_MAPPING, "R_PRODUCT_HIERARCHY.feather", row.names = FALSE, na="")
