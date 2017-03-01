library(dplyr)
library(tidyr)
library(stringr)

run_data <- readRDS("./data/external/run_data1990-2016.rds")

# Data Conversions
run_data$Age <- as.numeric(run_data$Age)

# add time in seconds
# problem is the format is inconsistent, some are in mm:ss and some in hh:mm:ss
length(str_split(run_data$Time[1], ":", simplify = TRUE))
# add pace in seconds


str(run_data)

run_data$Time[150]
duration(strptime(run_data$Time[1], "%M:%S"))

guess_formats(run_data$Time[110:140], c("%H:%M:%S","%M:%S"))
