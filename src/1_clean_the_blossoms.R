library(dplyr)
library(stringr)

external_data <- "run_data_1990-2016.rds"
run_data <- readRDS(paste("../data/external/",external_data, sep=""))

############################ Time Clean Up ############################ 
# Some years have asterisk in time - not sure what it means yet, 
# but creating a column to market it
run_data$hasTimeAsterisk <- 0

# Some 
if (sum(grep("\\*", run_data$Time)) > 0){
  run_data[grep("\\*", run_data$Time),]$hasTimeAsterisk <- 1 # Adds indicators of having asterisk
}

# Remove Asterisk from time
run_data$Time<-gsub("\\*", "",  run_data$Time)

########################### Time Conversion ###########################
# add time in minutes
# problem is the format is inconsistent, some are in mm:ss and some in hh:mm:ss
# 1. mm:ss only
# 2. hh:mm:ss only
# 3. mix of mm:ss and hh:mm:ss

convert_time <- function(time_vector){
  m <- str_split(time_vector, ":", simplify = TRUE)
  
  if (ncol(m) == 3){ # If problem #2 or #3
    time_vector <- if_else(m[,3] == "", as.numeric(m[,1]) + as.numeric(m[,2]) / 60, 
                           as.numeric(m[,1]) * 60 + as.numeric(m[,2]) + as.numeric(m[,3]) / 60)
  } else if (ncol(m) == 2){ # If problem #1
    time_vector <- as.numeric(m[,1]) + as.numeric(m[,2]) / 60
  }
  
  return(time_vector)
}

# Convert time and age from character
run_data <- run_data %>% mutate(TimeInMin = convert_time(Time),
                                PaceInMin = convert_time(Pace),
                                Age = as.numeric(Age))

# Write to processed data
saveRDS(run_data, paste("../data/processed/processed_", external_data, sep=""))
