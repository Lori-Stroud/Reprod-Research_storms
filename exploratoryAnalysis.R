# Exploratory analysis of U.S. National Oceanic and Atmospheric Administration's
# (NOAA) storm database

# This file is part of programming assignment 2 of "Reproducible Research"
# course on Coursera (december 2014)

# Setup ####

library(data.table)
library(R.utils)

# Load the data ####

data.file <- "./data/repdata_data_StormData.csv.bz2"

if (!file.exists(data.file)) {
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",data.file)
}

bunzip2(data.file, destname = "temp.csv")
data <- fread("temp.csv", sep = ",", stringsAsFactors = FALSE)
file.remove("temp.csv")
# takes around 60s to unzip and load

# Subset only valid event names ####

allowed.events <- fread("event types.txt", sep = "\t")  # load event names
allowed.events$EVTYPE <- tolower(allowed.events$EVTYPE)

data$EVTYPE <- tolower(data$EVTYPE)
  
storm.data     <- data[EVTYPE %in% allowed.events$EVTYPE,]      # test with grep to see if we get more data
discarded.data <- data[ !(EVTYPE %in% allowed.events$EVTYPE),]

# Accross the US, which types of events are most harmful with respect to population health? ####

# --> rank by injuries and fatalities
# --> plot ()

# Across the United States, which types of events have the greatest economic consequences? ####

# more dangerous place to be during strong event ?




as.data.frame(table(data$EVTYPE))