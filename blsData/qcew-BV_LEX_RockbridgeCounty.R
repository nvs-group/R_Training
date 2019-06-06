# qcew for BV, Lex, Rockbridge County
# June 2019


library(dplyr)

qcewGetAreaData <- function(year, qtr, area) {
  url <- "http://data.bls.gov/cew/data/api/YEAR/QTR/area/AREA.csv"
  url <- sub("YEAR", year, url, ignore.case=FALSE)
  url <- sub("QTR", tolower(qtr), url, ignore.case=FALSE)
  url <- sub("AREA", toupper(area), url, ignore.case=FALSE)
  read.csv(url, header = TRUE, sep = ",", quote="\"", dec=".", na.strings=" ", skip=0)
}


# import area titles
setwd("~/Dropbox/R-Studio/R_Training/blsData")
area_titles <- read.csv("https://data.bls.gov/cew/doc/titles/area/area_titles.csv")
View(area_titles)


# import BV city data
BV_area_code <- area_titles %>% filter(area_title == "Buena Vista City, Virginia")
View(BV_area_code)

BVData <- qcewGetAreaData("2018", "1", BV_area_code$area_fips)
BVData[1,]


# import Lexington city data
LEX_area_code <- area_titles %>% filter(area_title == "Lexington City, Virginia")
View(LEX_area_code)

LEXData <- qcewGetAreaData("2018", "1", LEX_area_code$area_fips)
LEXData[1,]


# import Rockbridge County data
RB_area_code <- area_titles %>% filter(area_title == "Rockbridge County, Virginia")
View(RB_area_code)

RBData <- qcewGetAreaData("2018","1", RB_area_code$area_fips)
RBData[1,]

all3 <- bind_rows(BVData, LEXData, RBData)

ncol(BVData)
ncol(LEXData)

# EXAMPLES ----------------------------------------------------

MichiganData <- qcewGetAreaData("2015", "1", "26000")
Construction <- qcewGetIndustryData("2015", "1", "1012")
SizeData <- qcewGetSizeData("2015","6")


# Prints first line of data
MichiganData[1, ]
Construction[1, ]
SizeData[1, ]