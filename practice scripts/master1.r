# master1 script that corresponds to master1.txt data file
# created from nvs6.r file, 20190523

#load packages
library(dplyr)
library(stringr)

# file from email, uploaded to server. How to make this available to other users?

# master1_raw <- read.csv("~/R/nvs6/data/Master1_Table.txt", header=FALSE) # no headers
# master1 <- read.csv("~/R/nvs6/data/master1_withHeaders.txt")
master1 <- read.csv("https://www.dropbox.com/s/fgty42qwpkzudwz/master1.txt?dl=1")

# put variable names in lower case
names(master1) <- tolower(names(master1))
names(master1)

# explore dataset
str(master1)
levels(master1$level_name)
glimpse(master1)
names(master1) 


# convert medwage from factor to numeric data type
as.numeric(as.character(master1$medwage))
as.character(master1$medwage)

# replace $ with blank "" in the master1$medwage column and coerce that result to numeric
master1$medwage <- as.numeric(gsub("\\$","", master1$medwage))


# Exercises below to practice filtering and sorting in R
# 1 Number of Bachelor's degrees awards offered by state
master1 %>% 
  filter(str_detect(level_name, "Bachelor's")) %>%
  group_by (stabbr) %>%
  summarize(n = n()) %>%
  arrange (desc(n))


# 2 List of institutions offering highest number of bachelor's degrees in VA
master1 %>%
  filter(stabbr == "VA",str_detect(level_name, "Bachelor's")) %>%
  select(instnm,cip2010ttl,level_name) %>%
  group_by(instnm) %>%
  summarize(n = n()) %>%
  arrange(desc(n))

# 3 Which occupation has the highest medium wage
master1 %>%
  select(occupation,medwage) %>%
  group_by(occupation) %>%
  summarize(wage = max(medwage)) %>%
  arrange(desc(wage))  

# 4 Which institutions offer the highest sum of median-wage occupations by degree in VA?
master1 %>%
  filter(stabbr == "VA") %>%
  group_by(occupation,cip2010ttl) %>%
  summarize(sum = sum(medwage)) %>%
  arrange(desc(sum))
  
