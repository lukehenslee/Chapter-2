#==================================================================================================
# Detection probability
# Date: February 21, 2022
# Creator: Luke Henslee, ADF&G and CFOS, UAF
#
# Purpose: This script uses beacon pings within arrays to explore detection 
# probabilities throughout the 2020 and 2021 seasons
#==================================================================================================
#NOTES: Beacons were set to ping every 60 seconds 
#==================================================================================================

# Load packages ####
library(readr)
library(tidyverse)
library(data.table)
library(lubridate)
library(openxlsx)
library(magrittr)

# Set working directory ####
setwd("C:/Users/lukeh/Desktop/School/Chapter_2")

# Import data ####
## Receiver metadata
rec <- read.csv('data/rec.csv')

rec5.6_20 <- read.csv('data/2020/array_5/5.6.csv', skip = 45)
rec5.6_20 <- rec5.6_20[,c(1:2, 4, 7)]

rec5.6_21 <- read.csv('data/2021/array_5/5.6.csv', skip = 45)
rec5.6_21 <- rec5.6_21[,c(1:2, 4, 7)]


# Rec metadata
rec5 <- rec %>% 
  filter(rec.ID %in% c(5.1, 5.2, 5.3, 5.4, 5.5))

rec5_20 <- rec5 %>% filter(year(mdy(deploy.date)) == '2020')

rec5_21 <- rec5 %>% filter(year(mdy(deploy.date)) == '2021')

# Combine and filter
rec5.6 <- rbind(rec5.6_20, rec5.6_21) %>% 
  filter(Tag.ID %in% rec5$code) %>% 
  filter(Power > 20)

rec5.6$ymd_hms <- paste(rec5.6$Date, rec5.6$Time, sep = " ") 

rec5.6$ymd_hms_numeric <- as.numeric(mdy_hms(rec5.6$ymd_hms, tz = 'US/Alaska'))

rec5.6 <- rec5.6[,c(3:6)]

rec5.6$ymd_hms <- as_datetime(rec5.6$ymd_hms_numeric)

rec5.6$year <- year(rec5.6$ymd_hms)

rec5.6_20 <- rec5.6 %>% 
  filter(year == '2020')

rec5.6_21 <- rec5.6 %>% 
  filter(year == '2021')

# Merge with metadata

## 2020
rec5.6_20$code <- rec5.6_20$Tag.ID

rec5.6_20 <- merge(rec5.6_20, rec5_20, by = 'code')

rec5.6_20$dist5.6 <- abs(rec5.6_20$dist.shore - 1800)

## 2021
rec5.6_21$code <- rec5.6_21$Tag.ID

rec5.6_21 <- merge(rec5.6_21, rec5_21, by = 'code')

rec5.6_21$dist5.6 <- abs(rec5.6_21$dist.shore - 2600)
