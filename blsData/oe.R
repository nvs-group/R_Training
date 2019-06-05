# oes survey
# June 2019

# import Current OE data
oe.dataCurrent <- read.delim("https://download.bls.gov/pub/time.series/oe/oe.data.0.Current")

# import alldata dataset (from text, base)
oe.data <- read.delim("https://download.bls.gov/pub/time.series/oe/oe.data.1.AllData")

# import oe.area
oe.area <- read.delim("https://download.bls.gov/pub/time.series/oe/oe.area")

# import oe.areatype
oe.areatype <- read.delim("https://download.bls.gov/pub/time.series/oe/oe.areatype")

# import oe.datatype
oe.datatype <- read.delim("https://download.bls.gov/pub/time.series/oe/oe.datatype")

# import oe.footnote
oe.footnote <- read.delim("https://download.bls.gov/pub/time.series/oe/oe.footnote")

# import oe.industry
oe.industry <- read.delim("https://download.bls.gov/pub/time.series/oe/oe.industry")

# import oe.occupation
oe.occupation <- read.delim("https://download.bls.gov/pub/time.series/oe/oe.occupation")

# import oe.series
oe.series <- read.delim("https://download.bls.gov/pub/time.series/oe/oe.series")


summary(oe.data)
