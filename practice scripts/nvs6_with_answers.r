# nvs6 factors dataset import and explore
# created 20190517, updated 20190522

library(dplyr)
library(stringr)

# file from email, uploaded to server. How to make this available to other users?

# master1_raw <- read.csv("~/R/nvs6/data/Master1_Table.txt", header=FALSE) # no headers
# master1headers_raw <- read.csv("~/R/nvs6/data/master1_withHeaders.txt")
master1headers_raw <- read.csv("https://www.dropbox.com/s/foyub5f65ccm2hk/master1_withHeaders.txt?dl=1")

# variable names to lower case
names(master1headers_raw) <- tolower(names(master1headers_raw))
names(master1headers_raw)

# copy dataset to new name
master1 <- master1headers_raw

# explore dataset
str(master1)
str(master1)
levels(master1$Label_Name)
glimpse(master1)
names(master1) 

# remove id field
master1 <- master1 %>% select (-id)

# discover duplicate fields
setequal(master1$soc2010ttl,master1$Occupation)  # = FALSE


# convert medwage from factor to numeric data type
# numeric fields = medwage
# convert currency fields from string to numeric data type


# Exercises below to practice filtering and sorting in R

# 1 Number of Bachelor's degrees awards offered by state
master1 %>%
  filter(str_detect(degree.name, "Bachelor's")) %>%
  group_by (State) %>%
  summarize(n = n()) %>%
  arrange (desc(n))

# 2 List of institutions offering highest number of bachelor's degrees in VA
master1 %>%
  filter(State == "VA",str_detect(degree.name, "Bachelor's")) %>%
  select(school.name,cip.name,degree.name) %>%
  group_by(school.name) %>%
  summarize(n = n()) %>%
  arrange(desc(n))

# 3 Which occupation is the highest medium wage

# 4 Which institutions offer the highest sum of median-wage occupations by degree in VA?

