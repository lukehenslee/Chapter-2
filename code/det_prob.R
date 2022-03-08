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
library(lubridate)

# Set working directory ####
setwd("C:/Users/lhhenslee/Desktop/Luke/School/Thesis/Chapter 2/Chapter-2") # Work
setwd("C:/Users/lukeh/Desktop/School/GIT repos/Chapter-2") # School

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


## We can look at this by hour, or by day

## By hour ####

## 2020 

rec20.hr <- rec20 %>% 
  mutate(hour.seq = 1 + ((unix - min(unix)) %/% 3600)) |>
  mutate(hour.seq = factor(hour.seq, levels = seq(1, max(hour.seq)))) |>
  mutate(code = factor(code)) |>
  group_by(hour.seq, code, .drop = FALSE) |>
  summarize(freq = n(), .groups = "drop")

rec20.hr$prop <- rec20.hr$freq/60 # Proportion of beacons detected

rec20.hr$unix <- min(rec20$unix) + (as.numeric(rec20.hr$hour.seq) * 3600) # Unix timestamp
rec20.hr$datetime <- as_datetime((rec20.hr$unix)) # Datetime
rec20.hr$yday <- yday(rec20.hr$datetime) # yday
rec20.hr$hour <- hour(rec20.hr$datetime)

rec20.hr <- merge(rec20.hr, subset(rec, year == '2020'), by = 'code')
rec20.hr$Distance <- paste(abs(rec20.hr$dist.shore - 1800), 'm')
rec20.hr$Distance <- factor(rec20.hr$Distance, levels = c('300 m', '600 m', '900 m',
                                                  '1200 m', '1500 m'))

rec20.hr <- merge(rec20.hr, temp, by = c('year', 'yday', 'hour')) # Temp

## 2021
rec21.hr <- rec21 %>% 
  mutate(hour.seq = 1 + ((unix - min(unix)) %/% 3600)) |>
  mutate(hour.seq = factor(hour.seq, levels = seq(1, max(hour.seq)))) |>
  mutate(code = factor(code)) |>
  group_by(hour.seq, code, .drop = FALSE) |>
  summarize(freq = n(), .groups = "drop")

rec21.hr$prop <- rec21.hr$freq/60 # Proportion of beacons detected

rec21.hr$unix <- min(rec21$unix) + (as.numeric(rec21.hr$hour.seq) * 3600) # Unix timestamp
rec21.hr$datetime <- as_datetime((rec21.hr$unix)) # Datetime
rec21.hr$yday <- yday(rec21.hr$datetime)
rec21.hr$hour <- hour(rec21.hr$datetime)

rec21.hr <- merge(rec21.hr, subset(rec, year == '2021'), by = 'code')
rec21.hr$Distance <- paste(abs(rec21.hr$dist.shore - 2600), 'm')
rec21.hr$Distance <- factor(rec21.hr$Distance, levels = c('500 m', '1000 m', '1500 m',
                                                          '2000 m', '2300 m'))

## By day ####

## 2020
rec20.day <- rec20 %>% 
  mutate(yday = yday(as.POSIXlt(unix, origin = "1970-01-01"))) %>% 
  mutate(code = factor(code)) |>
  group_by(yday, code, .drop = FALSE) |>
  summarize(freq = n(), .groups = "drop")

rec20.day$prop <- rec20.day$freq/1440 # Daily proportion of beacons detected

rec20.day$mdy <- as.Date(rec20.day$yday, origin = '2020-01-01') # Add mdy

rec20.day <- merge(rec20.day, subset(rec, year == '2020'), by = 'code')
rec20.day$Distance <- paste(abs(rec20.day$dist.shore - 1800), 'm')
rec20.day$Distance <- factor(rec20.day$Distance, levels = c('300 m', '600 m', '900 m',
                                                          '1200 m', '1500 m'))

## 2021
rec21.day <- rec21 %>% 
  mutate(yday = yday(mdy(Date))) %>% 
  mutate(code = factor(code)) |>
  group_by(yday, code, .drop = FALSE) |>
  summarize(freq = n(), .groups = "drop")

rec21.day$prop <- rec21.day$freq/1440

rec21.day$mdy <- as.Date(rec21.day$yday, origin = '2021-01-01') # Add mdy

rec21.day <- merge(rec21.day, subset(rec, year == '2021'), by = 'code')
rec21.day$Distance <- paste(abs(rec21.day$dist.shore - 2600), 'm')
rec21.day$Distance <- factor(rec21.day$Distance, levels = c('500 m', '1000 m', '1500 m',
                                                          '2000 m', '2300 m'))

# Import temperature data ####
temp <- read.csv('data/blueberry_hobo_2020.csv')
temp$yday <- yday(mdy_hm(temp$mdy.hms))

temp2 <- temp %>% 
  group_by(yday) %>% 
  summarize(mean = mean(as.numeric(temp)), n = n())

# Visualize #### 

library(RColorBrewer)
library(ggpubr)
library(extrafont)
library(visreg)
loadfonts(device = 'win')

## Proportion of detection by receiver ####
my.pal <- c("#E78AC3", "#66C2A5", "#FC8D62", "#8DA0CB", "#A6D854")
### By hour ####
  #### 2020
rec20.hr.plot <- ggplot(data = rec20.hr, aes(x = as.numeric(hour.seq), y = prop)) +
  geom_col(position = 'dodge', aes(fill = Distance)) +
  geom_smooth() +
  facet_wrap(~Distance, scales = 'free') +
  xlab('Date') +
  ylab('Detection proportion') +
  scale_fill_manual(values = my.pal) +
  scale_x_continuous(limits = c(0,1248), expand = c(0,0), breaks = seq(0,1200,400),
                     labels = c('Jul 15', 'Jul 31', 'Aug 17', 'Sept 3')) +
  scale_y_continuous(limits = c(0, 1), expand = c(0,0)) +
  theme_classic() +
  theme (axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
         axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
         panel.spacing = unit(1, 'lines'),
         #set the font type
         text = element_text(),
         #modify plot title, the B in this case
         plot.title = element_text(face = "bold"),
         #position the legend on the figure
         legend.position = c(0.8, 0.15),
         legend.title = element_text(size = 14),
         #adjust size of text for legend
         legend.text = element_text(size = 14),
         #margin for the plot
         plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
         strip.background = element_blank(),
         strip.text = element_blank(),
         #set size of the tick marks for y-axis
         axis.ticks.y = element_line(size = 0.5),
         #set size of the tick marks for x-axis
         axis.ticks.x = element_line(size = 0.5),
         #adjust length of the tick marks
         axis.ticks.length = unit(0.2,"cm"),
         #set size and location of the tick labels for the y axis
         axis.text.y = element_text(colour = "black", size = 10, angle = 0, vjust = 0.5, hjust = 1,
                                    margin = margin(t = 0, r = 5, b = 0, l = 0)),
         #set size and location of the tick labels for the x axis
         axis.text.x = element_text(colour = "black", size = 10, angle = 0, vjust = 0, hjust = 0.5,
                                    margin = margin(t = 5, r = 0, b = 0, l = 0)),
         #set the axis size, color, and end shape
         axis.line = element_line(colour = "black", size = 0.5, lineend = "square"))

rec20.hr.plot

ggsave(rec20.hr.plot, file = "figs/rec20.hr.png", width = 20, height = 12, units = "cm", dpi = 300)

  #### 2021
rec21.hr.plot <- ggplot(data = rec21.hr, aes(x = as.numeric(hour.seq), y = prop)) +
  geom_col(position = 'dodge', aes(fill = Distance)) +
  geom_smooth() +
  facet_wrap(~Distance, scales = 'free') +
  xlab('Date') +
  ylab('Detection proportion') +
  scale_fill_manual(values = my.pal) +
  scale_x_continuous(limits = c(0,1628), expand = c(0,0), breaks = seq(0,1600,400),
                     labels = c('Jun 26', 'Jul 12', 'Jul 29', 'Aug 15', 'Aug 31')) +
  scale_y_continuous(limits = c(0, 1), expand = c(0,0)) +
  theme_classic() +
  theme (axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
         axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
         panel.spacing = unit(1, 'lines'),
         #set the font type
         text = element_text(),
         #modify plot title, the B in this case
         plot.title = element_text(face = "bold"),
         #position the legend on the figure
         legend.position = c(0.8, 0.15),
         legend.title = element_text(size = 14),
         #adjust size of text for legend
         legend.text = element_text(size = 14),
         #margin for the plot
         plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
         strip.background = element_blank(),
         strip.text = element_blank(),
         #set size of the tick marks for y-axis
         axis.ticks.y = element_line(size = 0.5),
         #set size of the tick marks for x-axis
         axis.ticks.x = element_line(size = 0.5),
         #adjust length of the tick marks
         axis.ticks.length = unit(0.2,"cm"),
         #set size and location of the tick labels for the y axis
         axis.text.y = element_text(colour = "black", size = 10, angle = 0, vjust = 0.5, hjust = 1,
                                    margin = margin(t = 0, r = 5, b = 0, l = 0)),
         #set size and location of the tick labels for the x axis
         axis.text.x = element_text(colour = "black", size = 10, angle = 0, vjust = 0, hjust = 0.5,
                                    margin = margin(t = 5, r = 0, b = 0, l = 0)),
         #set the axis size, color, and end shape
         axis.line = element_line(colour = "black", size = 0.5, lineend = "square"))

rec21.hr.plot

ggsave(rec21.hr.plot, file = "figs/rec21.hr.png", width = 20, height = 12, units = "cm", dpi = 300)

### By day ####

#### 2020
rec20.day.plot <- ggplot(data = rec20.day, aes(x = yday, y = prop)) +
  geom_col(position = 'dodge', aes(fill = Distance)) +
  geom_smooth() +
  facet_wrap(~Distance, scales = 'free') +
  xlab('Date') +
  ylab('Detection proportion') +
  scale_fill_manual(values = my.pal) +
  scale_x_continuous(limits = c(196, 248), expand = c(0,0), breaks = seq(200,240,10),
                     labels = c('Jul 19', 'Jul 29', 'Aug 8', 'Aug 18', 'Aug 28')) +
  scale_y_continuous(limits = c(0, 1), expand = c(0,0)) +
  theme_classic() +
  theme (axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
         axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
         panel.spacing = unit(1, 'lines'),
         #set the font type
         text = element_text(),
         #modify plot title, the B in this case
         plot.title = element_text(face = "bold"),
         #position the legend on the figure
         legend.position = c(0.8, 0.15),
         legend.title = element_text(size = 14),
         #adjust size of text for legend
         legend.text = element_text(size = 14),
         #margin for the plot
         plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
         strip.background = element_blank(),
         strip.text = element_blank(),
         #set size of the tick marks for y-axis
         axis.ticks.y = element_line(size = 0.5),
         #set size of the tick marks for x-axis
         axis.ticks.x = element_line(size = 0.5),
         #adjust length of the tick marks
         axis.ticks.length = unit(0.2,"cm"),
         #set size and location of the tick labels for the y axis
         axis.text.y = element_text(colour = "black", size = 10, angle = 0, vjust = 0.5, hjust = 1,
                                    margin = margin(t = 0, r = 5, b = 0, l = 0)),
         #set size and location of the tick labels for the x axis
         axis.text.x = element_text(colour = "black", size = 10, angle = 0, vjust = 0, hjust = 0.5,
                                    margin = margin(t = 5, r = 0, b = 0, l = 0)),
         #set the axis size, color, and end shape
         axis.line = element_line(colour = "black", size = 0.5, lineend = "square"))

rec20.day.plot

ggsave(rec20.day.plot, file = "figs/rec20.day.png", width = 20, height = 12, units = "cm", dpi = 300)

#### 2021
rec21.day.plot <- ggplot(data = rec21.day, aes(x = yday, y = prop)) +
  geom_col(position = 'dodge', aes(fill = Distance)) +
  geom_smooth() +
  facet_wrap(~Distance, scales = 'free') +
  xlab('Date') +
  ylab('Detection proportion') +
  scale_fill_manual(values = my.pal) +
  scale_x_continuous(limits = c(177, 244), expand = c(0,0), breaks = seq(180,240,10),
                     labels = c('Jun 30', 'Jul 10', 'Jul 20', 'Jul 30', 'Aug 9', 'Aug 19',
                                'Aug 29')) +
  scale_y_continuous(limits = c(0, 1), expand = c(0,0)) +
  theme_classic() +
  theme (axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
         axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
         panel.spacing = unit(1, 'lines'),
         #set the font type
         text = element_text(),
         #modify plot title, the B in this case
         plot.title = element_text(face = "bold"),
         #position the legend on the figure
         legend.position = c(0.8, 0.15),
         legend.title = element_text(size = 14),
         #adjust size of text for legend
         legend.text = element_text(size = 14),
         #margin for the plot
         plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
         strip.background = element_blank(),
         strip.text = element_blank(),
         #set size of the tick marks for y-axis
         axis.ticks.y = element_line(size = 0.5),
         #set size of the tick marks for x-axis
         axis.ticks.x = element_line(size = 0.5),
         #adjust length of the tick marks
         axis.ticks.length = unit(0.2,"cm"),
         #set size and location of the tick labels for the y axis
         axis.text.y = element_text(colour = "black", size = 10, angle = 0, vjust = 0.5, hjust = 1,
                                    margin = margin(t = 0, r = 5, b = 0, l = 0)),
         #set size and location of the tick labels for the x axis
         axis.text.x = element_text(colour = "black", size = 10, angle = 0, vjust = 0, hjust = 0.5,
                                    margin = margin(t = 5, r = 0, b = 0, l = 0)),
         #set the axis size, color, and end shape
         axis.line = element_line(colour = "black", size = 0.5, lineend = "square"))

rec21.day.plot

ggsave(rec21.day.plot, file = "figs/rec21.day.png", width = 20, height = 12, units = "cm", dpi = 300)

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
