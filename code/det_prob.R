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
setwd("C:/Users/lukeh/Desktop/School/GIT repos/Chapter-2")

# Import data ####
## Receiver metadata
rec <- read.csv('data/rec.csv')

## Receiver 5.6
rec20 <- read.csv('data/blueberry_2020.csv', skip = 45)
rec20 <- rec20[,c(1:2, 4, 7)]

rec21 <- read.csv('data/blueberry_2021.csv', skip = 45)
rec21 <- rec21[,c(1:2, 4, 7)]

# Data manipulation ####

## Filter for beacon ID's and dates ####

### 2020
rec20 <- rec20 %>% 
  filter(Tag.ID %in% rec[c(25:29), 2]) %>% 
  filter(as.Date(mdy(Date)) > as.Date(mdy(rec[30,3])), 
         as.Date(mdy(Date)) < as.Date(mdy(rec[30,5])))

### 2021
rec21 <- rec21 %>% 
  filter(Tag.ID %in% rec[c(67:71), 2]) %>% 
  filter(as.Date(mdy(Date)) > as.Date(mdy(rec[72,3])), 
         as.Date(mdy(Date)) < as.Date(mdy(rec[72,5])))

## Merge with metadata ####

rec$year <- year(mdy(rec$deploy.date))

### 2020
rec20 <- rec20 %>% 
  rename(code = Tag.ID)

rec20$year <- year(mdy(rec20$Date))

rec20 <- merge(rec20, rec, by = c('year', 'code'))

rec20$dist <- abs(rec20$dist.shore - 1800)

### 2021
rec21 <- rec21 %>% 
  rename(code = Tag.ID)

rec21$year <- year(mdy(rec21$Date))

rec21 <- merge(rec21, rec, by = c('year', 'code'))

rec21$dist <- abs(rec21$dist.shore - 2600)

## Need a UNIX timestamp ####

## 2020
rec20$ymd.hms <- paste(rec20$Date, rec20$Time, sep = ' ')

rec20$unix <- as.numeric(mdy_hms(rec20$ymd.hms))

## 2021
rec21$ymd.hms <- paste(rec21$Date, rec21$Time, sep = ' ')

rec21$unix <- as.numeric(mdy_hms(rec21$ymd.hms))

## Import temp data ####
temp <- read.csv('data/blueberry_hobo.csv')

temp$mdy.hms <- mdy_hm(temp$mdy.hms)

temp$yday <- yday(temp$mdy.hms)

temp2 <- temp %>% 
  group_by(yday) %>% 
  summarize(mean = mean(as.numeric(temp)), n = n())

## We can look at this by hour, or by day

## By hour ####

## 2020 

rec20.hr <- rec20 %>% 
  mutate(hour = 1 + ((unix - min(unix)) %/% 3600)) |>
  mutate(hour = factor(hour, levels = seq(1, max(hour)))) |>
  mutate(code = factor(code)) |>
  group_by(hour, code, .drop = FALSE) |>
  summarize(freq = n(), .groups = "drop")

rec20.hr <- rec20 %>% 
  mutate(hour = 1 + ((unix - min(unix)) %/% 3600)) |>
  mutate(unix = min(unix) + (hour * 3600)) |>
  mutate(hour = factor(hour, levels = seq(1, max(hour)))) |>
  mutate(code = factor(code)) |>
  group_by(hour, code, .drop = FALSE) |>
  summarize(freq = n(), .groups = "drop")

rec20.hr$prop <- rec20.hr$freq/60
rec20.hr$unix <- min(rec20$unix) + (as.numeric(rec20.hr$hour) * 3600)
rec20.hr$datetime <- as_datetime((rec20.hr$unix))

rec20.hr <- merge(rec20.hr, subset(rec, year == '2020'), by = 'code')
rec20.hr$dist <- abs(rec20.hr$dist.shore - 1800)

## 2021
rec21.hr <- rec21 %>% 
  mutate(hour = 1 + ((unix - min(unix)) %/% 3600)) |>
  mutate(hour = factor(hour, levels = seq(1, max(hour)))) |>
  mutate(code = factor(code)) |>
  group_by(hour, code, .drop = FALSE) |>
  summarize(freq = n(), .groups = "drop")

rec21.hr$prop <- rec21.hr$freq/60

rec21.hr <- merge(rec21.hr, subset(rec, year == '2021'), by = 'code')
rec21.hr$dist <- abs(rec21.hr$dist.shore - 2600)

## By day ####

## 2020
rec20.day <- rec20 %>% 
  mutate(yday = yday(as.POSIXlt(unix, origin = "1970-01-01"))) %>% 
  mutate(code = factor(code)) |>
  group_by(yday, code, .drop = FALSE) |>
  summarize(freq = n(), .groups = "drop")

rec20.day$prop <- rec20.day$freq/1440

rec20.day <- merge(rec20.day, subset(rec, year == '2020'), by = 'code')
rec20.day$dist <- abs(rec20.day$dist.shore - 1800)

## 2021
rec21.day <- rec21 %>% 
  mutate(yday = yday(mdy(Date))) %>% 
  mutate(code = factor(code)) |>
  group_by(yday, code, .drop = FALSE) |>
  summarize(freq = n(), .groups = "drop")

rec21.day$prop <- rec21.day$freq/1440

rec21.day <- merge(rec21.day, subset(rec, year == '2021'), by = 'code')
rec21.day$dist <- abs(rec21.day$dist.shore - 2600)

# Import temperature data ####
temp <- read.csv('data/blueberry_hobo.csv')
temp$yday <- yday(mdy_hm(temp$mdy.hms))

temp2 <- temp %>% 
  group_by(yday) %>% 
  summarize(mean = mean(as.numeric(temp)), n = n())

# Visualize #### 
## Combine data

rec5 <- rbind(rec20, rec21)
rec5$Power <- as.numeric(rec5$Power)

ggplot(data = rec20, aes(x = dist)) +
  geom_histogram() 

ggplot(data = rec21, aes(x = dist)) +
  geom_histogram()

ggplot(data = rec5, aes(x = dist, y = Power, col = year)) +
  geom_point(position = position_jitter(width = 50, height = 1))

## Proportion of detection by receiver

### Merge detections with metadata
rec20.prop <- merge(rec20.det, subset(rec, year == '2020'), by = 'code')
rec20.prop$dist <- abs(rec20.prop$dist.shore - 1800)

ggplot(data = rec20.prop, aes(x = dist, y = prop, fill = as.factor(dist))) +
  geom_violin()

#### 2021
rec21.prop <- merge(rec21.det, subset(rec, year == '2021'), by = 'code')
rec21.prop$dist <- abs(rec21.prop$dist.shore - 2600)

ggplot(data = rec21.prop, aes(x = dist, y = prop, fill = as.factor(dist))) +
  geom_violin()

#### Both years
rec5.prop <- rbind(rec20.prop, rec21.prop)

ggplot(data = rec5.prop, aes(x = dist, y = prop, fill = as.factor(dist))) +
  geom_violin()

ggplot(data = rec20.prop, aes(x = as.numeric(hour), y = prop, col = as.factor(dist))) +
  geom_line() +
  geom_smooth() +
  facet_wrap(~as.factor(dist))

ggplot(data = rec20.prop, aes(x = prop)) +
  geom_histogram() +
  facet_wrap(~as.factor(dist))

ggplot(data = rec21.prop, aes(x = as.numeric(hour), y = prop, col = as.factor(dist))) +
  geom_line() +
  geom_smooth() +
  facet_wrap(~as.factor(dist))

ggplot(data = rec5.prop, aes(x = as.numeric(hour), y = prop, col = as.factor(dist))) +
  geom_line() +
  geom_smooth() +
  facet_wrap(~as.factor(dist))

# Scrap ####

## 2020
rec20.det2 <- rec20 %>% 
  mutate(yday = yday(as.POSIXlt(unix, origin = "1970-01-01"))) %>% 
  mutate(code = factor(code)) |>
  group_by(yday, code, .drop = FALSE) |>
  summarize(freq = n(), .groups = "drop")

rec20.det2$prop <- rec20.det2$freq/1440

rec20.det2 <- merge(rec20.det2, temp2, by = 'yday')

rec20.det2 <- merge(rec20.det2, subset(rec, year == '2020'), by = 'code')
rec20.det2$dist <- abs(rec20.det2$dist.shore - 1800)

ggplot(data = rec20.det2, aes(x = mean, y = prop)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~as.factor(dist))

## 2021
rec21.det2 <- rec21 %>% 
  mutate(yday = yday(mdy(Date))) %>% 
  mutate(code = factor(code)) |>
  group_by(yday, code, .drop = FALSE) |>
  summarize(freq = n(), .groups = "drop")

rec21.det2$prop <- rec21.det2$freq/1440

rec21.det2 <- merge(rec21.det2, subset(rec, year == '2021'), by = 'code')
rec21.det2$dist <- abs(rec21.det2$dist.shore - 2600)

ggplot(data = rec21.det2, aes(x = yday, y = prop)) +
  geom_line() +
  geom_smooth() +
  facet_wrap(~as.factor(dist))
