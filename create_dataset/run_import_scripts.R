#### RUN IMPORT SCRIPTS  ####
#### 

# !!!! TO DO modify this to be a bash shell script that calls Rscript with the name of the import_*.R script to run
# 

# script file to call all of the import data table from DashDb into feather files
# which are then converted into tibble dataframes with required types, filtering etc.
# 
x <- list.files("import/") # Vector of all the files in the import directory
file_list <- paste("import/", x, sep = "")


source(file[1]) # "import/import_cust_settlement.R"
source(file[2])
source(file[3])
source(file[4])
source(file[5])
source(file[6])
source(file[7])
source(file[8])
source(file[9])
source(file[10])

Rscript -e 'paste("import/", list.files("import/"), sep = " ")'
