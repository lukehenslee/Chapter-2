#==================================================================================================
# Preparing data for GIS animation
# 9/12/2022
# Luke Henslee, College of Fisheries and Ocean Sciences, UAF
#
#==================================================================================================
# NOTES: This script interpolates detection data for animation using GIS 
# software.
#
#==================================================================================================

# Load packages
library(tidyverse)
library(zoo)
library(lubridate)

# Set working directory ####
setwd("H:/git_repo/Chapter-2")

# Data ####

## Load data ####

## Must be formatted as 'tag.ID', 'date.time', 'lat', 'long', then any factors
## (e.g., sex, species, stock, etc)
det <- read.csv("det_example.csv")
str(det)

## Format data ####

# Make sure date time is in POSIXct format
det$date.time <- mdy_hm(det$date.time)

# Interpolate detections ####

## We use detection location and timestamps to interpolate movement in a 
## straight line at a constant rate

### Separate data into lists by tag ID
det.list <- split(det, f = det$tag.ID)

### Now loop through each list and expand detection history into 'animate.list'
animate.list <- list() # Empty list

for(i in 1:length(det.list)){
  animate.list[[i]] <- complete(det.list[[i]], 
                             date.time = 
                               seq(min(det.list[[i]][[2]]),
                                   max(det.list[[i]][[2]]), by = '15 min')) %>% 
    # change 'by' to alter interpolation interval (default = '15 min')
    fill(tag.ID, sex) %>% 
    # This fills in your factor variables
    arrange(date.time)
  animate.list[[i]]$lat <- na.approx(animate.list[[i]][[3]]) # This interpolates latitudes 
  animate.list[[i]]$long <- na.approx(animate.list[[i]][[4]]) # This interpolates longitudes 
}

  
# Finalize and write .csv ####
animate <- do.call(rbind.data.frame, animate.list)
animate[,1] <- as.character(animate[,1])
write.csv(animate, 'animate.csv', row.names = F)
