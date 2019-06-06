# threeTables.R
# created 20190606
# Purpose of this script is to merge the three files (master1, cip_code.txt, soc_code.txt into 1 file = master2)
# cip is already embedded in master1, I am removing the merge of cip file into master

library(readr)
library(dplyr)

master1 <- read.csv("https://www.dropbox.com/s/fgty42qwpkzudwz/master1.txt?dl=1")

# set working directory
setwd("~/R/R_Training_NVS/practice scripts")

# import file into R
cip <- read_tsv("cip_code.txt")
soc <- read_tsv("soc_code.txt")

# view data
View(master1)
View(cip)
View(soc)

# variable names to lower case
names(master1) <- tolower(names(master1))
names(master1)

names(cip) <- tolower(names(cip))
names(cip)    # [1] "cip_code"     "cip_category"

names(soc) <- tolower(names(soc))
names(soc)    # [1] "soc_code"     "soc_cat_name"

# change cip_code to cip.code in cip data
colnames(cip)[colnames(cip)=="cip_code"] <- "cip.code"

# chonge soc_code to degree.code in sop data
colnames(soc)[colnames(soc)=="degree.code"] <- "soc.cat"


# merge master1 and cip ### Removing from code because CIP already in master file
# master2 <- master1 %>% left_join(cip, by = "cip.code")

# merge master1 and soc to master2
master2 <- left_join(master1, soc, by = "soc.cat")
