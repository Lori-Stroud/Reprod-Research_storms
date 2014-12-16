# Exploratory analysis of U.S. National Oceanic and Atmospheric Administration's
# (NOAA) storm database

# This file is part of programming assignment 2 of "Reproducible Research"
# course on Coursera (december 2014)

# Setup ####

library(data.table)
library(R.utils)

# Load the data ####

data.file <- "data/repdata_data_StormData.csv.bz2"

if (!file.exists(data.file)) {
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",data.file)
}

bunzip2(data.file, destname = "temp.csv", remove = FALSE)
data <- fread("temp.csv", sep = ",", stringsAsFactors = FALSE)
file.remove("temp.csv")

# Clean the event names ####

# the following grep commands re-maps some of the more frequent "misspelled" events.
# For example, "TSTM WIND" is assumed to be a "Thunderstorm Wind".

data[grep("marine(.*)(tstm|thunderstorm)",
          EVTYPE,ignore.case = TRUE)]$EVTYPE <- "Marine Thunderstorm Wind"
data[grep("^( ?tstm|(severe )?thunderstorms?)((.*winds?s?)|.*\\d|.*\\)|)$",
          EVTYPE,ignore.case = TRUE)]$EVTYPE <- "Thunderstorm Wind"
data[grep("^high winds?(| \\d+|/)$",
          EVTYPE,ignore.case = TRUE)]$EVTYPE <- "High Wind"
data[grep("fires?",
          EVTYPE,ignore.case = TRUE)]$EVTYPE <- "Wildfire"
data[grep("(extr|sev|rec|exces|unus|Unseas)(.*)cold(|/wind chill|/frost)$",
          EVTYPE,ignore.case = TRUE)]$EVTYPE <- "Extreme Cold/Wind Chill"
data[grep("landslide",
          EVTYPE,ignore.case = TRUE)]$EVTYPE <- "Debris Flow"
data[grep("coastal flood",
          EVTYPE,ignore.case = TRUE)]$EVTYPE <- "Coastal Flood"
data[grep("^flooding$|stream|^urban|river",
          EVTYPE,ignore.case = TRUE)]$EVTYPE <- "Flood"
data[grep("^( ?flash|flood|local).*floods?(ing)?/?$",
          EVTYPE,ignore.case = TRUE)]$EVTYPE <- "Flash Flood"
data[grep("^rip currents?$",
          EVTYPE,ignore.case = TRUE)]$EVTYPE <- "Rip Current"
data[grep("^storm surge?$",
          EVTYPE,ignore.case = TRUE)]$EVTYPE <- "Storm Surge/Tide"
data[grep("hurricane",
          EVTYPE,ignore.case = TRUE)]$EVTYPE <- "Hurricane (Typhoon)"
data[grep("^(heavy Surf|high surf)",
          EVTYPE,ignore.case = TRUE)]$EVTYPE <- "High Surf"
data[grep("^Strong Winds?$",
          EVTYPE,ignore.case = TRUE)]$EVTYPE <- "Strong Wind"
data[grep("warm|heat",
          EVTYPE,ignore.case = TRUE)]$EVTYPE <- "Heat"
data[grep("^(deep |small )?hail.*(damage|\\d+|)$",
          EVTYPE,ignore.case = TRUE)]$EVTYPE <- "Hail"
data[grep("frost|freeze",
          EVTYPE,ignore.case = TRUE)]$EVTYPE <- "Frost/Freeze"

# From the above cleaning we cover about:
# - 99.2% of the entries
# - 97.4% and 98.5% of all fatalities/injuries respectively
# - 99.2% and 99.1% of total propdmg/cropdmg respectively

# Among the most frequent data entries we do not re-map are:
#   EVTYPE (occurences)
#   WINTER WEATHER/MIX  1104
#   TSTM WIND/HAIL (1028)
#   SNOW (587)
#   FOG (538)
#   WIND (340)
#   FREEZING RAIN (250)
#   EXTREME WINDCHILL (204)
#   DRY MICROBURST (186)
#   LIGHT SNOW (154)
#   ASTRONOMICAL HIGH TIDE (103)
#   MODERATE SNOWFALL (101)

# Remarks:
# - we only re-maps events clearly belonging to a specific valid category.
# - we do not re-map events entering several categories such as "TSTM WIND/HAIL"
#   which can go either in "Thunderstorm Wind" or "Hail"
#   Same apply for "FOG" as it could be either dense or freezing fog
#   (same for "snow", "wind", ...)
# - we put the stream/urban/river- flooding events into the general flood
#   category (since there is no specific category for urban/river/stream-flood)
# - we consider fire caused by lighting as wildfire

# see also the Severe weather terminology at :
# http://en.wikipedia.org/wiki/Severe_weather_terminology_%28United_States%29

# Subset the data considering only valid event names ####
# list of valid event names is assumed to be in "event types.txt" file

valid.events <- fread("event types.txt")$EVTYPE  # load valid event names

storm.data     <- data[tolower(EVTYPE) %in% tolower(valid.events),]  # subset
# discarded.data <- data[ !(tolower(EVTYPE) %in% tolower(base.events)),]

# Analyze the data - part 1 - impact on population health ####

# --> aggregate with AVERAGE and TOTAL injuries/fatalities by event type.
# --> plot the 10 most harmfull






# Analyze the data - part 2 - impact on economy ####


# Bonus : most dangerous place to be during strong event ? ####



# look at the more important discarded event types :
t <- as.data.table(table(discarded.data$EVTYPE))
names(t) <- c("EVTYPE","count")
t[order(-count)][1:40,]

discarded.data[order(-INJURIES),list(EVTYPE,INJURIES)][1:10,]

