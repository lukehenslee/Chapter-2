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
setwd("C:/Users/lukeh/Desktop/School/Thesis/Chapter_2")
setwd("C:/Users/lhhenslee/Desktop/Luke/School/Thesis/Chapter 2")

# Import data ####
## Receiver metadata
rec <- read.csv('data/rec.csv')

rec5.6_20 <- read.csv('data/range_test/blueberry_2020.csv', skip = 45)
rec5.6_20 <- rec5.6_20[,c(1:2, 4, 7)]

rec5.6_21 <- read.csv('data/range_test/blueberry_2021.csv', skip = 45)
rec5.6_21 <- rec5.6_21[,c(1:2, 4, 7)]

# Data manipulation ####

## Receiver metadata ####
codes_2020 <- rec[c(25:29), 2]

codes_2021 <- rec[c(67:71), 2]

## Filter for beacon ID's and dates ####
rec5.20 <- rec5.6_20 %>% 
  filter(Tag.ID %in% codes_2020) %>% 
  filter(as.Date(mdy(Date)) > as.Date(mdy(rec[30,3])), 
         as.Date(mdy(Date)) < as.Date(mdy(rec[30,5])))


rec5.21 <- rec5.6_21 %>% 
  filter(Tag.ID %in% codes_2021) %>% 
  filter(as.Date(mdy(Date)) > as.Date(mdy(rec[72,3])), 
         as.Date(mdy(Date)) < as.Date(mdy(rec[72,5])))

## Merge with metadata ####

## 2020
rec5.20$code <- rec5.20$Tag.ID
rec5.20$year <- rep('2020', nrow(rec5.20))

rec5.20 <- merge(rec5.20, rec, by = c('year', 'code'))

rec5.20$dist <- abs(rec5.20$dist.shore - 1800)

## 2021
rec5.21$code <- rec5.21$Tag.ID
rec5.21$year <- rep('2021', nrow(rec5.21))

rec5.21 <- merge(rec5.21, rec, by = c('year', 'code'))

rec5.21$dist <- abs(rec5.21$dist.shore - 2600)

## Need a UNIX timestamp ####
## 2020
rec5.20$ymd.hms <- paste(rec5.20$Date, rec5.20$Time, sep = ' ')

rec5.20$unix <- as.numeric(mdy_hms(rec5.20$ymd.hms))

## 2021
rec5.21$ymd.hms <- paste(rec5.21$Date, rec5.21$Time, sep = ' ')

rec5.21$unix <- as.numeric(mdy_hms(rec5.21$ymd.hms))

## For some reason, array 5 in 2021 has some timestamps from 2012

rec5.21 <- rec5.21 %>% 
  filter(unix > 1524457593)

# Analysis ####
## I want to see what proportion of pings are detected every hour
## There should be 60 pings an hour, so the proportion would be number detected
## divided by 60.

## Stack overflow question ####
## 2020 

rec5.20.det <- rec5.20 %>% 
  mutate(hour = 1 + ((unix - min(unix)) %/% 3600)) |>
  mutate(hour = factor(hour, levels = seq(1, max(hour)))) |>
  mutate(code = factor(code)) |>
  group_by(hour, code, .drop = FALSE) |>
  summarize(freq = n(), .groups = "drop")

rec5.20.det$prop <- rec5.20.det$freq/60

## 2021
rec5.21.det <- rec5.21 %>% 
  mutate(hour = 1 + ((unix - min(unix)) %/% 3600)) |>
  mutate(hour = factor(hour, levels = seq(1, max(hour)))) |>
  mutate(code = factor(code)) |>
  group_by(hour, code, .drop = FALSE) |>
  summarize(freq = n(), .groups = "drop")

rec5.21.det$prop <- rec5.21.det$freq/60

# Visualize #### 
## Combine data

rec5 <- rbind(rec5.20, rec5.21)
rec5$Power <- as.numeric(rec5$Power)

ggplot(data = rec5.20, aes(x = dist)) +
  geom_histogram() 

ggplot(data = rec5.21, aes(x = dist)) +
  geom_histogram()

ggplot(data = rec5, aes(x = dist, y = Power, col = year)) +
  geom_point(position = position_jitter(width = 50, height = 1))

## Proportion of detection by receiver

### Merge detections with metadata
rec5.20.prop <- merge(rec5.20.det, subset(rec, year == '2020'), by = 'code')
rec5.20.prop$dist <- abs(rec5.20.prop$dist.shore - 1800)

ggplot(data = rec5.20.prop, aes(x = dist, y = prop, fill = as.factor(dist))) +
  geom_violin()

#### 2021
rec5.21.prop <- merge(rec5.21.det, subset(rec, year == '2021'), by = 'code')
rec5.21.prop$dist <- abs(rec5.21.prop$dist.shore - 2600)

ggplot(data = rec5.21.prop, aes(x = dist, y = prop, fill = as.factor(dist))) +
  geom_violin()

#### Both years
rec5.prop <- rbind(rec5.20.prop, rec5.21.prop)

ggplot(data = rec5.prop, aes(x = dist, y = prop, fill = as.factor(dist))) +
  geom_violin()

ggplot(data = rec5.20.prop, aes(x = as.numeric(hour), y = prop, col = as.factor(dist))) +
  geom_line() +
  geom_smooth() +
  facet_wrap(~as.factor(dist))

ggplot(data = rec5.20.prop, aes(x = prop)) +
  geom_histogram() +
  facet_wrap(~as.factor(dist))

ggplot(data = rec5.21.prop, aes(x = as.numeric(hour), y = prop, col = as.factor(dist))) +
  geom_line() +
  geom_smooth() +
  facet_wrap(~as.factor(dist))

ggplot(data = rec5.prop, aes(x = as.numeric(hour), y = prop, col = as.factor(dist))) +
  geom_line() +
  geom_smooth() +
  facet_wrap(~as.factor(dist))
