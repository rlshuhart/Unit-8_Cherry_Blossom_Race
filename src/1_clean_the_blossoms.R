library(dplyr)
library(tidyr)
library(stringr)

run_data <- readRDS("./data/external/run_data1990-2016.rds")

## Age Conversion - NR stands for Not Recorded, replaced with NA
run_data$AgeInt <- as.numeric(run_data$Age)

## Time Clean Up
# Some times have * after time - not sure what it means yet, but creating a column to market it
run_data$hasTimeAsterisk <- 0
run_data[grep("\\*", run_data$Time),]$hasTimeAsterisk <- 1

# Remove Asterisk
run_data$Time<-gsub("\\*", "",  run_data$Time)

# add time in seconds
# problem is the format is inconsistent, some are in mm:ss and some in hh:mm:ss
# 1. mm:ss only
# 2. hh:mm:ss only
# 3. mix of mm:ss and hh:mm:ss

m <- str_split(run_data$Time, ":", simplify = TRUE)

if (ncol(m) == 3){
  run_data$TimeInSeconds <- if_else(m[,3] == "", as.numeric(m[,1]) * 60 + as.numeric(m[,2]), 
          as.numeric(m[,1]) * 3600 + as.numeric(m[,2]) * 60 + as.numeric(m[,3]))
} else if (ncol(m) == 2){
  run_data$TimeInSeconds <- as.numeric(m[,1]) * 60 + as.numeric(m[,2])
}



#### Scratch code below ####
run_data$TimeInSeconds <- as.numeric(m[,1]) * 3600 + as.numeric(m[,2]) * 60 + as.numeric(m[,3])
run_data$TimeAsDuration <- duration(hours=as.numeric(m[,1]), 
                                    minutes=as.numeric(m[,2]), 
                                    seconds= as.numeric(m[,3]))

str(run_data)

run_data$Time[150]
duration(strptime(run_data$Time[1], "%M:%S"))

run_data$StripTime <- strptime(run_data$Time[118:125], format=guess_formats(run_data$Time[118:125], c("%H:%M:%S","%M:%S")))
duration(seconds=run_data$TimeInSeconds[1])

run_data %>% 
  transmute(StripTime = strptime(Time, 
                                 format=guess_formats(Time, c("%H:%M:%S","%M:%S")))) %>%
  mutate(TimeAsDuration = as.Date.POSIXct(duration(hours = hour(StripTime),
                                   minutes = minute(StripTime),
                                   seconds = second(StripTime)))
         )