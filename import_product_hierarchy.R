#### IMPORT THE PRODUCT HIERARCHY ####

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
(product_hierarchy_in <- read_feather("R_PRODUCT_HIERARCHY.feather")) %>% View # import and view the data

# dimensions
nrow(product_hierarchy_in); ncol(product_hierarchy_in)  # 2350 rows. 7 cols

# column names
colnames(product_hierarchy_in)
# [1] "STYLE"      "STYLE_NAME" "GENDER"     "TYPE"       "TYPE_1"     "CATEGORY"   "CLASS" 

### HOW MUCH MISSING DATA in THE TIBBLE???? ####
product_hierarchy_in %>% summarise_each(funs(100*mean(is.na(.)))) # 0.  No NA


### "STYLE" ###
# string mixed in with integer eg 104149 and IBQF34
# treat as string convert to factor.

# any duplicates?
(xdf <- group_by(product_hierarchy_in, STYLE) %>% arrange(STYLE)) %>% filter(n() > 1) %>% nrow # 20.   View.  safe to remove duplicates

# remove duplicates for product_hierarchy arranged by STYLE
product_hierarchy <- slice(xdf, 1:1) %>% ungroup() # choose just the first row from each group...omitting the duplicates 

style_col <- transmute(product_hierarchy, style = as.factor(STYLE))

str(style_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	2340 obs. of  1 variable:
# $ style: Factor w/ 2340 levels "100000","100014",..: 1 2 3 4 5 6 7 8 9 10 ...

summary(style_col)
# style     
# 100000 :   1  
# 100014 :   1  
# 100021 :   1  
# 100022 :   1  
# 100026 :   1  
# 100031 :   1  
# (Other):2334


### "STYLE_NAME" ### 
# leave a description string - case unchanged.
# 
style_name_col <- select(product_hierarchy, style_name = STYLE_NAME)


### "GENDER" ###
# convert string to a factor lower case
# 
# how many distinct factors?
distinct(product_hierarchy, GENDER) %>% arrange(GENDER)
# 1 Womens
# 2   Mens
# 3   Kids
# 4 Unisex
# 5   MENS
# 6   KIDS
# 7 WOMENS
# 8 UNISEX

# Need to fix duplicates By changing everything to lower case and then convert to a factor
gender_col <- transmute(product_hierarchy, gender = as.factor(str_to_lower(GENDER))) 

str(gender_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	2340 obs. of  1 variable:
#   $ gender: Factor w/ 4 levels "kids","mens",..: 4 4 4 4 4 4 2 2 2 2 ...

summary(gender_col)
# gender    
# kids  : 115  
# mens  : 937  
# unisex: 172  
# womens:1116

 
### "TYPE" ###
# string factor. lower case
# 
# how many distinct factors?
distinct(product_hierarchy, TYPE) %>% arrange(TYPE)
# 1    Adventure
# 2         Life
# 3 next to skin
# 4 Next to skin
# 5     Training

# Need to fix duplicates By changing everything to lower case and then convert to a factor
type_col <- transmute(product_hierarchy, type = as.factor(str_to_lower(TYPE)))

str(type_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	2340 obs. of  1 variable:
#   $ type: Factor w/ 4 levels "adventure","life",..: 3 2 4 4 2 2 1 1 4 4 ...

summary(type_col)
# type    
# adventure   :861  
# life        :635  
# next to skin:440  
# training    :404


### "TYPE_1" ###
# string factor. lower case
# 
# how many distinct factors?
distinct(product_hierarchy, TYPE_1) %>% arrange(TYPE_1) # Accessories, Apparel

type_1_col <- transmute(product_hierarchy, type_1 = as.factor(str_to_lower(TYPE_1)))

str(type_1_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	2340 obs. of  1 variable:
#   $ type_1: Factor w/ 2 levels "accessories",..: 2 2 2 2 2 2 2 2 2 2 ...

summary(type_1_col)
# type_1    
# accessories: 498  
# apparel    :1842


### "CATEGORY" ###
# string factor. lower case.
#
# how many distinct factors?
distinct(product_hierarchy, CATEGORY) %>% arrange(CATEGORY)
# <chr>
# 1 Accessories
# 2     Bottoms
# 3     Dresses
# 4   Outerwear
# 5        Tops
# 6        TOPS
# 7   Underwear

category_col <- transmute(product_hierarchy, category = as.factor(str_to_lower(CATEGORY)))

str(category_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	2340 obs. of  1 variable:
#   $ category: Factor w/ 6 levels "accessories",..: 5 5 5 2 4 2 5 5 5 5 ..

summary(category_col)
# category   
# accessories: 495  
# bottoms    : 236  
# dresses    :  37  
# outerwear  : 100  
# tops       :1301  
# underwear  : 171



### "CLASS" ###
# string factor. lower case.
#
# how many distinct factors?
distinct(product_hierarchy, CLASS) %>% arrange(CLASS)
# 1        Base Layers
# 2             Boxers
# 3               Bras
# 4             Briefs
# 5            Dresses
# 6             Gloves
# 7      Hats/Headwear
# 8  Insulated Jackets
# 9               Kids
# 10          Knickers
# # ... with 18 more rows

class_col <- transmute(product_hierarchy, class = as.factor(str_to_lower(CLASS)))

str(class_col)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	2340 obs. of  1 variable:
#   $ class: Factor w/ 27 levels "base layers",..: 11 12 11 1 17 1 12 25 11 11 ...

summary(class_col)
# class    
# ss tops       :518  
# socks         :321  
# ls tops       :306  
# ls base layers:173  
# tanks         :129  
# base layers   :113  
# (Other)       :780




### CREATE THE NEW PRODUCT_HIERARCHY
# [1] "STYLE"      "STYLE_NAME" "GENDER"     "TYPE"       "TYPE_1"     "CATEGORY"   "CLASS"

xdf <- cbind(
    style_col,
    style_name_col,
    gender_col,
    type_col,
    type_1_col,
    category_col,
    class_col)

View(xdf)

str(xdf)
# 'data.frame':	2340 obs. of  7 variables:
# $ style     : Factor w/ 2340 levels "100000","100014",..: 1 2 3 4 5 6 7 8 9 10 ...
# $ style_name: chr  "Wmns Oasis LS V Dusk" "Wmns Aurora LS Zip" "Wmns Quest LS Crewe" "Wmns Rush Tights" ...
# $ gender    : Factor w/ 4 levels "kids","mens",..: 4 4 4 4 4 4 2 2 2 2 ...
# $ type      : Factor w/ 4 levels "adventure","life",..: 3 2 4 4 2 2 1 1 4 4 ...
# $ type_1    : Factor w/ 2 levels "accessories",..: 2 2 2 2 2 2 2 2 2 2 ...
# $ category  : Factor w/ 6 levels "accessories",..: 5 5 5 2 4 2 5 5 5 5 ...
# $ class     : Factor w/ 27 levels "base layers",..: 11 12 11 1 17 1 12 25 11 11 ...


summary(xdf)
# style          style_name          gender               type             type_1            category               class    
# 100000 :   1   Length:2340        kids  : 115   adventure   :861   accessories: 498   accessories: 495   ss tops       :518  
# 100014 :   1   Class :character   mens  : 937   life        :635   apparel    :1842   bottoms    : 236   socks         :321  
# 100021 :   1   Mode  :character   unisex: 172   next to skin:440                      dresses    :  37   ls tops       :306  
# 100022 :   1                      womens:1116   training    :404                      outerwear  : 100   ls base layers:173  
# 100026 :   1                                                                          tops       :1301   tanks         :129  
# 100031 :   1                                                                          underwear  : 171   base layers   :113  
# (Other):2334                                                                                             (Other)       :780


# write out the new dataframe as a feather file
write_feather(xdf, "product_hierarchy.feather")
