#==================================================================================================
# Detection probability function
# Date: February 21, 2022
# Creator: Luke Henslee, ADF&G and CFOS, UAF
#
# Purpose: This script uses beacon pings within arrays to explore detection 
# probabilities throughout the 2020 and 2021 seasons
#==================================================================================================
#NOTES: Beacons were set to ping every 60 seconds 
#==================================================================================================

# Load packages ####
library(tidyverse)

det <- function(data) {
  r <- data
  # Assign rec.ID
  r$rec.ID <- substr(deparse(substitute(data)), 2, 4)
  
  r2 <- r %>% 
    # Filter for rec.ID in array
    filter(Tag.ID %in% rec[c(which(substr(rec[,1], 1, 1) == substr(r[1,5], 1, 1) &
                                     year(mdy(rec[,3])) == year(mdy(r[1,1])) & 
                                     rec[,1] != r[1,5])), 2]) %>% 
    # Filter for deploy dates only
    filter(as.Date(mdy(Date)) > as.Date(mdy(rec[which(rec[,1] == r[1,5] &
                                                        year(mdy(rec[,3])) == year(mdy(r[1,1]))),3])), 
           as.Date(mdy(Date)) < as.Date(mdy(rec[which(rec[,1] ==r[1,5] &
                                                        year(mdy(rec[,3])) == year(mdy(r[1,1]))),4])))
  # Add timestamp
  r2$unix <- as.numeric(mdy_hms(paste(r2$Date, r2$Time, sep = ' ')))
  
  # Calculate hourly number of detections
  r3 <- r2 %>% 
    mutate(hour.seq = 1 + ((unix - min(unix)) %/% 3600)) |>
    mutate(hour.seq = factor(hour.seq, levels = seq(1, max(hour.seq)))) |>
    mutate(Tag.ID = factor(Tag.ID)) |>
    group_by(hour.seq, Tag.ID, .drop = FALSE) |>
    summarize(freq = n(), .groups = "drop")
  # Calculate proportion of successful detections
  r3$prop <- r3$freq/60
  # Timestamps
  r3$unix <- min(r2$unix) + (as.numeric(r3$hour.seq) * 3600)
  r3$datetime <- as_datetime(r3$unix)
  r3$yday <- yday(r3$datetime)
  r3$hour <- hour(r3$datetime)
  # Merge with metadata
  r4 <- merge(r3, subset(rec, year(mdy(rec[,3])) == year(mdy(r[1,1]))), by = 'Tag.ID')
  # Distance from other receivers
  r4$dist <- abs(r4$dist.shore - 
                   rec[which(rec[,1] == r2[1,5] & 
                               year(mdy(rec[,3])) == year(mdy(r2[1,1]))),5])
  r4$receiver <- rep(r2[1,5], times = nrow(r4))
  return(r4)
}



