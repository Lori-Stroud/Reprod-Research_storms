# Exploratory analysis of U.S. National Oceanic and Atmospheric Administration's
# (NOAA) storm database

# This file is part of programming assignment 2 of "Reproducible Research"
# course on Coursera (december 2014)

# Setup ####

library(data.table)
library(R.utils)
library(lubridate)
library(ggplot2)


# Load the data ####
data.file <- "data/repdata_data_StormData.csv.bz2"
if (!file.exists(data.file)) {
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",data.file)
}
bunzip2(data.file, destname = "temp.csv", remove = FALSE)
data <- fread("temp.csv", sep = ",", stringsAsFactors = FALSE,
              colClasses = c("NULL","character",rep("NULL",5),"character", rep("NULL",14),
                             rep("numeric",3),"character","numeric","character",rep("NULL",9)))
file.remove("temp.csv")

# Convert dates with lubridate
data$BGN_DATE <- mdy_hms(data$BGN_DATE)

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


# Subset the data ####
# - only subset valid event names
# - only consider variables of interest : event type, date, injuries/fatalities, and damages
# - we clean the damage variables on the fly with the function cleanDamage()

cleanDamage <- function(dammage,attribute) {
  # returns the dammage in $ taking into account its attribute which can be :
  # h = hundred, k = thousand, m = million, b = billion
  # or a digit n considered as a factor 10^n
  # if attribute is empty to assume a factor 1
  #
  # args: 
  #   - dammage   : numeric, the dammage values (here PROPDMG or CROPDMG)
  #   - attribute : character, the dammage attribute (here PROPDMGEXP or CROPDMGEXP)
  #
  # Returns: the dammage in dollar
  #
  # Exemple : for dammage = 2 and attribute = "k", returns the numeric value 2000.
  
  attribute <- gsub("[hH]","2",attribute)
  attribute <- gsub("[kK]","3",attribute)
  attribute <- gsub("[mM]","6",attribute)
  attribute <- gsub("[bB]","9",attribute)
  attribute <- gsub("^$","0",attribute)
  dammage*10^as.numeric(attribute)
}

valid.events <- c("Astronomical Low Tide","Avalanche","Blizzard","Coastal Flood","Cold/Wind Chill",
                  "Debris Flow","Dense Fog","Dense Smoke","Drought","Dust Devil","Dust Storm",
                  "Excessive Heat","Extreme Cold/Wind Chill","Flash Flood","Flood","Frost/Freeze",
                  "Funnel Cloud","Freezing Fog","Hail","Heat","Heavy Rain","Heavy Snow","High Surf",
                  "High Wind","Hurricane (Typhoon)","Ice Storm","Lake-Effect Snow","Lakeshore Flood",
                  "Lightning","Marine Hail","Marine High Wind","Marine Strong Wind",
                  "Marine Thunderstorm Wind","Rip Current","Seiche","Sleet","Storm Surge/Tide",
                  "Strong Wind","Thunderstorm Wind","Tornado","Tropical Depression","Tropical Storm",
                  "Tsunami","Volcanic Ash","Waterspout","Wildfire","Winter Storm","Winter Weather")


storm.data   <- data[tolower(EVTYPE) %in% tolower(valid.events),
                     list(event    = EVTYPE,
                          date     = BGN_DATE,
                          injuries = INJURIES,
                          deaths   = FATALITIES,
                          damage_property = cleanDamage(PROPDMG,PROPDMGEXP),
                          damage_crop     = cleanDamage(CROPDMG,CROPDMGEXP))]

# Analyze the data - part 1 - impact on population health ####

health.data <- storm.data[, list(total.injuries = sum(injuries, na.rm = TRUE),
                                 total.deaths   = sum(deaths, na.rm = TRUE)),
                                 by = event]
setorder(health.data,-total.deaths)

# plot top 5 most harmfull in total death (also plot total injuries)
top5.health.tot <- melt(health.data[1:5,], "event", c("total.injuries","total.deaths"),
                        variable.name = "type", value.name = "count")
setorder(top5.health.tot,-type,-count)

g <- ggplot(top5.health.tot,aes(x = factor(event,levels=event),y = count))
g <- g + geom_bar(aes(fill = type), position = "dodge", stat="identity")
g <- g + scale_y_log10()
g <- g + labs(x = "Type of Event",y = "Count")
g <- g + ggtitle("Top 5 most hamfull events (according to total number of deaths)")
g <- g + theme(legend.title=element_blank(),
               plot.title = element_text(lineheight=.8, face="bold", vjust = 2),
               axis.text.x = element_text(angle = 45, hjust = 1))
g <- g + scale_fill_discrete(breaks=c("total.injuries", "total.deaths"),
                             labels=c("Total injuries", "Total deaths"))
g



# Analyze the data - part 2 - impact on economy ####

eco.data <- storm.data[, list(total.damage_property = sum(damage_property, na.rm = TRUE),
                              total.damage_crop     = sum(damage_crop, na.rm = TRUE)),
                       by = event]

eco.data[,total.damage := total.damage_property + total.damage_crop]

# sort event factor-levels with the sum of damages.
eco.data$event <- as.factor(eco.data$event)
eco.data$event <- reorder(eco.data$event, -eco.data$total.damage)

# plot top 5 with the greatest economic consequences
top5.eco.tot <- melt(eco.data[1:5,], "event", c("total.damage_property","total.damage_crop"),
                        variable.name = "type", value.name = "cost")

g <- ggplot(top5.eco.tot,aes(x = event ,y = cost/1e6))
g <- g + geom_bar(aes(fill = type), stat="identity")
g <- g + labs(x = "Type of Event",y = "Damage in Millions of Dollars")
g <- g + ggtitle("Top 5 events having the greatest economic consequences")
g <- g + theme(legend.title=element_blank(),
               plot.title = element_text(lineheight=.8, face="bold", vjust = 2),
               axis.text.x = element_text(angle = 45, hjust = 1),
               legend.position="top")
g <- g + scale_fill_discrete(breaks=c("total.damage_property", "total.damage_crop"),
                             labels=c("Property damages", "Crop damages"))
g


# look at the more important discarded event types :
t <- as.data.table(table(discarded.data$EVTYPE))
names(t) <- c("EVTYPE","count")
t[order(-count)][1:40,]

discarded.data[order(-INJURIES),list(EVTYPE,INJURIES)][1:10,]

