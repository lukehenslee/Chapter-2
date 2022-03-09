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

# Source function
source('code/det_prob_fun_v3.R')

# Import data ####
## Receiver metadata
rec <- read.csv('data/rec_2020.csv')

## Column classes
r_colClasses <- read.csv('data/r_colClasses.csv')

## Receiver detection data (must be named 'rX.X')

## Array 1 ####
r1.2 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/1.2.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r1.3 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/1.3.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r1.4 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/1.4.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r1.5 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/1.5.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r1.6 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/1.6.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

## Array 2 ####
r2.1 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/2.1.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r2.2 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/2.2.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r2.3 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/2.3.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r2.4 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/2.4.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r2.5 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/2.5.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r2.6 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/2.6.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

## Array 3 ####
r3.1 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/3.1.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))


r3.3 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/3.3.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))


r3.6 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/3.6.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

# Run det() function ####

## Array 1 ####
r1.2_det <- det(r1.2)
r1.3_det <- det(r1.3)
r1.4_det <- det(r1.4)
r1.5_det <- det(r1.5)
r1.6_det <- det(r1.6)

## Array 2 ####
r2.1_det <- det(r2.1)
r2.2_det <- det(r2.2)
r2.3_det <- det(r2.3)
r2.4_det <- det(r2.4)
r2.5_det <- det(r2.5)
r2.6_det <- det(r2.6)

## Array 3 ####
r3.1_det <- det(r3.1)
r3.3_det <- det(r3.3)
r3.6_det <- det(r3.6)

# Merge into array detection proportion ####

## Array 1 ####
  # Not including rec 1.3 because of data corruption
r1 <- rbind(r1.2_det, r1.4_det, r1.5_det, r1.6_det)

r1.group <- r1 %>% 
  group_by(date(datetime), yday, hour, hour.seq, dist) %>% 
  summarize(across(prop, mean))

## Array 2 ####
r2 <- rbind(r2.1_det, r2.2_det, r2.3_det, r2.4_det, r2.5_det, r2.6_det)

r2.group <- r2 %>% 
  group_by(date(datetime), yday, hour, hour.seq, dist) %>% 
  summarize(across(prop, mean))

## Array 3 ####
r3 <- rbind(r3.1_det, r3.3_det, r3.6_det)

r3.group <- r3 %>% 
  group_by(date(datetime), yday, hour, hour.seq, dist) %>% 
  summarize(across(prop, mean))

# Visualize ####
my.pal <- c("#E78AC3", "#66C2A5", "#FC8D62", "#8DA0CB", "#A6D854")
## Array 1 ####
ggplot(data = r1.group, aes(x = as.numeric(hour.seq), y = prop)) +
  geom_col(position = 'dodge') +
  geom_smooth() +
  facet_wrap(~as.factor(dist)) +
  xlab('Hour sequence') +
  ylab('Detection proportion') 

r1.group$prop[r1.group$prop > 1] <- 1 

r1.plot <- ggplot(data = r1.group, aes(x = as.numeric(hour.seq), y = prop)) +
  geom_col(position = 'dodge') +
  geom_smooth() +
  facet_wrap(~as.factor(dist)) +
  xlab('Day of year') +
  ylab('Detection proportion') +
  scale_x_continuous(breaks = c(0, 300, 600, 900, 1200),
                     labels = c(r1.group[first(which.min(as.numeric(r1.group$hour.seq))), 2],
                                r1.group[first(which(as.numeric(r1.group$hour.seq) == 300)), 2],
                                r1.group[first(which(as.numeric(r1.group$hour.seq) == 600)), 2],
                                r1.group[first(which(as.numeric(r1.group$hour.seq) == 900)), 2],
                                r1.group[first(which.max(as.numeric(r1.group$hour.seq))), 2])) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  theme(panel.spacing.x = unit(1, "lines"))

ggsave(r1.plot, file = "figs/r1_2020.png", width = 23, height = 15, units = "cm", dpi = 300)

ggplot(data = r1.group, aes(x = as.numeric(hour.seq), y = prop, col = as.factor(dist))) +
  geom_smooth() +
  xlab('Day of year') +
  ylab('Detection proportion') +
  scale_color_manual(values = my.pal) +
  scale_x_continuous(breaks = c(0, 300, 600, 900, 1200),
                     labels = c(r7.group[first(which.min(as.numeric(r7.group$hour.seq))), 2],
                                r7.group[first(which(as.numeric(r7.group$hour.seq) == 300)), 2],
                                r7.group[first(which(as.numeric(r7.group$hour.seq) == 600)), 2],
                                r7.group[first(which(as.numeric(r7.group$hour.seq) == 900)), 2],
                                r7.group[first(which.max(as.numeric(r7.group$hour.seq))), 2])) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  theme_bw()

## Array 2 ####
ggplot(data = r2.group, aes(x = as.numeric(hour.seq), y = prop)) +
  geom_col(position = 'dodge') +
  geom_smooth() +
  facet_wrap(~as.factor(dist)) +
  xlab('Hour sequence') +
  ylab('Detection proportion')

r2.group$prop[r2.group$prop > 1] <- 1 

r2.plot <- ggplot(data = r2.group, aes(x = as.numeric(hour.seq), y = prop)) +
  geom_col(position = 'dodge') +
  geom_smooth() +
  facet_wrap(~as.factor(dist)) +
  xlab('Day of year') +
  ylab('Detection proportion') +
  scale_x_continuous(breaks = c(0, 300, 600, 900, 1200),
                     labels = c(r2.group[first(which.min(as.numeric(r2.group$hour.seq))), 2],
                                r2.group[first(which(as.numeric(r2.group$hour.seq) == 300)), 2],
                                r2.group[first(which(as.numeric(r2.group$hour.seq) == 600)), 2],
                                r2.group[first(which(as.numeric(r2.group$hour.seq) == 900)), 2],
                                r2.group[first(which.max(as.numeric(r2.group$hour.seq))), 2])) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  theme(panel.spacing.x = unit(1, "lines"))

ggsave(r2.plot, file = "figs/r2_2020.png", width = 23, height = 15, units = "cm", dpi = 300)

ggplot(data = r2.group, aes(x = as.numeric(hour.seq), y = prop, col = as.factor(dist))) +
  geom_smooth() +
  xlab('Day of year') +
  ylab('Detection proportion') +
  scale_x_continuous(breaks = c(0, 300, 600, 900, 1200),
                     labels = c(r7.group[first(which.min(as.numeric(r7.group$hour.seq))), 2],
                                r7.group[first(which(as.numeric(r7.group$hour.seq) == 300)), 2],
                                r7.group[first(which(as.numeric(r7.group$hour.seq) == 600)), 2],
                                r7.group[first(which(as.numeric(r7.group$hour.seq) == 900)), 2],
                                r7.group[first(which.max(as.numeric(r7.group$hour.seq))), 2])) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) 
## Array 3 ####
ggplot(data = r3.group, aes(x = as.numeric(hour.seq), y = prop)) +
  geom_col(position = 'dodge') +
  geom_smooth() +
  facet_wrap(~as.factor(dist)) +
  xlab('Hour sequence') +
  ylab('Detection proportion')

r3.group$prop[r3.group$prop > 1] <- 1 

r3.plot <- ggplot(data = r3.group, aes(x = as.numeric(hour.seq), y = prop)) +
  geom_col(position = 'dodge') +
  geom_smooth() +
  facet_wrap(~as.factor(dist)) +
  xlab('Day of year') +
  ylab('Detection proportion') +
  scale_x_continuous(breaks = c(0, 300, 600, 900, 1200),
                     labels = c(r3.group[first(which.min(as.numeric(r3.group$hour.seq))), 2],
                                r3.group[first(which(as.numeric(r3.group$hour.seq) == 300)), 2],
                                r3.group[first(which(as.numeric(r3.group$hour.seq) == 600)), 2],
                                r3.group[first(which(as.numeric(r3.group$hour.seq) == 900)), 2],
                                r3.group[first(which.max(as.numeric(r3.group$hour.seq))), 2])) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  theme(panel.spacing.x = unit(1, "lines"))

ggsave(r3.plot, file = "figs/r3_2020.png", width = 23, height = 15, units = "cm", dpi = 300)

ggplot(data = r3.group, aes(x = as.numeric(hour.seq), y = prop, col = as.factor(dist))) +
  geom_smooth() +
  xlab('Day of year') +
  ylab('Detection proportion') +
  scale_x_continuous(breaks = c(0, 300, 600, 900, 1200),
                     labels = c(r7.group[first(which.min(as.numeric(r7.group$hour.seq))), 2],
                                r7.group[first(which(as.numeric(r7.group$hour.seq) == 300)), 2],
                                r7.group[first(which(as.numeric(r7.group$hour.seq) == 600)), 2],
                                r7.group[first(which(as.numeric(r7.group$hour.seq) == 900)), 2],
                                r7.group[first(which.max(as.numeric(r7.group$hour.seq))), 2])) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) 

# Array 4 ####
r4.1 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/4.1.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r4.2 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/4.2.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r4.3 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/4.3.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r4.4 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/4.4.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r4.5 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/4.5.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r4.6 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/4.6.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r4.1_det <- det(r4.1)
r4.2_det <- det(r4.2)
r4.3_det <- det(r4.3)
r4.4_det <- det(r4.4)
r4.5_det <- det(r4.5)
r4.6_det <- det(r4.6)

r4 <- rbind(r4.1_det, r4.2_det, r4.3_det, r4.4_det, r4.5_det, r4.6_det)

r4.group <- r4 %>% 
  group_by(date(datetime), yday, hour, hour.seq, dist) %>% 
  summarize(across(prop, mean))

ggplot(data = r4.group, aes(x = as.numeric(hour.seq), y = prop)) +
  geom_col(position = 'dodge') +
  geom_smooth() +
  facet_wrap(~as.factor(dist)) +
  xlab('Hour sequence') +
  ylab('Detection proportion')

r4.group$prop[r4.group$prop > 1] <- 1 

r4.plot <- ggplot(data = r4.group, aes(x = as.numeric(hour.seq), y = prop)) +
  geom_col(position = 'dodge') +
  geom_smooth() +
  facet_wrap(~as.factor(dist)) +
  xlab('Day of year') +
  ylab('Detection proportion') +
  scale_x_continuous(breaks = c(0, 300, 600, 900, 1200),
                     labels = c(r4.group[first(which.min(as.numeric(r4.group$hour.seq))), 2],
                                r4.group[first(which(as.numeric(r4.group$hour.seq) == 300)), 2],
                                r4.group[first(which(as.numeric(r4.group$hour.seq) == 600)), 2],
                                r4.group[first(which(as.numeric(r4.group$hour.seq) == 900)), 2],
                                r4.group[first(which.max(as.numeric(r4.group$hour.seq))), 2])) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  theme(panel.spacing.x = unit(1, "lines"))

ggsave(r4.plot, file = "figs/r4_2020.png", width = 23, height = 15, units = "cm", dpi = 300)

ggplot(data = r4.group, aes(x = as.numeric(hour.seq), y = prop, col = as.factor(dist))) +
  geom_smooth() +
  xlab('Day of year') +
  ylab('Detection proportion') +
  scale_x_continuous(breaks = c(0, 300, 600, 900, 1200),
                     labels = c(r7.group[first(which.min(as.numeric(r7.group$hour.seq))), 2],
                                r7.group[first(which(as.numeric(r7.group$hour.seq) == 300)), 2],
                                r7.group[first(which(as.numeric(r7.group$hour.seq) == 600)), 2],
                                r7.group[first(which(as.numeric(r7.group$hour.seq) == 900)), 2],
                                r7.group[first(which.max(as.numeric(r7.group$hour.seq))), 2])) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) 

# Array 5 ####
r5.1 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/5.1.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r5.2 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/5.2.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r5.3 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/5.3.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r5.4 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/5.4.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r5.5 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/5.5.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r5.6 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/5.6.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r5.1_det <- det(r5.1)
r5.2_det <- det(r5.2)
r5.3_det <- det(r5.3)
r5.4_det <- det(r5.4)
r5.5_det <- det(r5.5)
r5.6_det <- det(r5.6)

r5 <- rbind(r5.1_det, r5.2_det, r5.3_det, r5.4_det, r5.5_det, r5.6_det)

r5.group <- r5 %>% 
  group_by(date(datetime), yday, hour, hour.seq, dist) %>% 
  summarize(across(prop, mean))

ggplot(data = r5.group, aes(x = as.numeric(hour.seq), y = prop)) +
  geom_col(position = 'dodge') +
  geom_smooth() +
  facet_wrap(~as.factor(dist)) +
  xlab('Hour sequence') +
  ylab('Detection proportion')


r5.group$prop[r5.group$prop > 1] <- 1 

r5.plot <- ggplot(data = r5.group, aes(x = as.numeric(hour.seq), y = prop)) +
  geom_col(position = 'dodge') +
  geom_smooth() +
  facet_wrap(~as.factor(dist)) +
  xlab('Day of year') +
  ylab('Detection proportion') +
  scale_x_continuous(breaks = c(0, 300, 600, 900, 1200),
                     labels = c(r5.group[first(which.min(as.numeric(r5.group$hour.seq))), 2],
                                r5.group[first(which(as.numeric(r5.group$hour.seq) == 300)), 2],
                                r5.group[first(which(as.numeric(r5.group$hour.seq) == 600)), 2],
                                r5.group[first(which(as.numeric(r5.group$hour.seq) == 900)), 2],
                                r5.group[first(which.max(as.numeric(r5.group$hour.seq))), 2])) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  theme(panel.spacing.x = unit(1, "lines"))

ggsave(r5.plot, file = "figs/r5_2020.png", width = 23, height = 15, units = "cm", dpi = 300)

ggplot(data = r5.group, aes(x = as.numeric(hour.seq), y = prop, col = as.factor(dist))) +
  geom_smooth() +
  xlab('Day of year') +
  ylab('Detection proportion') +
  scale_x_continuous(breaks = c(0, 300, 600, 900, 1200),
                     labels = c(r7.group[first(which.min(as.numeric(r7.group$hour.seq))), 2],
                                r7.group[first(which(as.numeric(r7.group$hour.seq) == 300)), 2],
                                r7.group[first(which(as.numeric(r7.group$hour.seq) == 600)), 2],
                                r7.group[first(which(as.numeric(r7.group$hour.seq) == 900)), 2],
                                r7.group[first(which.max(as.numeric(r7.group$hour.seq))), 2])) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) 

# Array 6 ####
r6.1 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/6.1.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r6.2 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/6.2.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r6.3 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/6.3.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r6.4 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/6.4.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r6.5 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/6.5.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r6.6 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/6.6.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r6.1_det <- det(r6.1)
r6.2_det <- det(r6.2)
r6.3_det <- det(r6.3)
r6.4_det <- det(r6.4)
r6.5_det <- det(r6.5)
r6.6_det <- det(r6.6)

r6 <- rbind(r6.1_det, r6.2_det, r6.3_det, r6.4_det, r6.5_det, r6.6_det)

r6.group <- r6 %>% 
  group_by(date(datetime), yday, hour, hour.seq, dist) %>% 
  summarize(across(prop, mean))

r6.group$prop[r6.group$prop > 1] <- 1 

r6.plot <- ggplot(data = r6.group, aes(x = as.numeric(hour.seq), y = prop)) +
  geom_col(position = 'dodge') +
  geom_smooth() +
  facet_wrap(~as.factor(dist)) +
  xlab('Day of year') +
  ylab('Detection proportion') +
  scale_x_continuous(breaks = c(0, 300, 600, 900, 1200),
                     labels = c(r6.group[first(which.min(as.numeric(r6.group$hour.seq))), 2],
                                r6.group[first(which(as.numeric(r6.group$hour.seq) == 300)), 2],
                                r6.group[first(which(as.numeric(r6.group$hour.seq) == 600)), 2],
                                r6.group[first(which(as.numeric(r6.group$hour.seq) == 900)), 2],
                                r6.group[first(which.max(as.numeric(r6.group$hour.seq))), 2])) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0))+
  theme(panel.spacing.x = unit(1, "lines"))

ggsave(r6.plot, file = "figs/r6_2020.png", width = 23, height = 15, units = "cm", dpi = 300)

ggplot(data = r6.group, aes(x = as.numeric(hour.seq), y = prop, col = as.factor(dist))) +
  geom_smooth() +
  xlab('Day of year') +
  ylab('Detection proportion') +
  scale_x_continuous(breaks = c(0, 300, 600, 900, 1200),
                     labels = c(r7.group[first(which.min(as.numeric(r7.group$hour.seq))), 2],
                                r7.group[first(which(as.numeric(r7.group$hour.seq) == 300)), 2],
                                r7.group[first(which(as.numeric(r7.group$hour.seq) == 600)), 2],
                                r7.group[first(which(as.numeric(r7.group$hour.seq) == 900)), 2],
                                r7.group[first(which.max(as.numeric(r7.group$hour.seq))), 2])) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  theme(panel.spacing.x = unit(1, "lines"))


# Array 7 ####
r7.1 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/7.1.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r7.2 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/7.2.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r7.3 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/7.3.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r7.4 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/7.4.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r7.5 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/7.5.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r7.6 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/7.6.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r7.1_det <- det(r7.1)
r7.2_det <- det(r7.2)
r7.3_det <- det(r7.3)
r7.4_det <- det(r7.4)
r7.5_det <- det(r7.5)
r7.6_det <- det(r7.6)

r7 <- rbind(r7.1_det, r7.2_det, r7.3_det, r7.4_det, r7.5_det, r7.6_det)

r7.group <- r7 %>% 
  group_by(date(datetime), yday, hour, hour.seq, dist) %>% 
  summarize(across(prop, mean))

r7.plot <- ggplot(data = r7.group, aes(x = as.numeric(hour.seq), y = prop)) +
  geom_col(position = 'dodge') +
  geom_smooth() +
  facet_wrap(~as.factor(dist)) +
  xlab('Day of year') +
  ylab('Detection proportion') +
  scale_x_continuous(breaks = c(0, 300, 600, 900, 1200),
                     labels = c(r7.group[first(which.min(as.numeric(r7.group$hour.seq))), 2],
                                r7.group[first(which(as.numeric(r7.group$hour.seq) == 300)), 2],
                                r7.group[first(which(as.numeric(r7.group$hour.seq) == 600)), 2],
                                r7.group[first(which(as.numeric(r7.group$hour.seq) == 900)), 2],
                                r7.group[first(which.max(as.numeric(r7.group$hour.seq))), 2])) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  theme(panel.spacing.x = unit(1, "lines"))

ggsave(r7.plot, file = "figs/r7_2020.png", width = 23, height = 15, units = "cm", dpi = 300)


ggplot(data = r7.group, aes(x = as.numeric(hour.seq), y = prop, col = as.factor(dist))) +
  geom_smooth() +
  xlab('Day of year') +
  ylab('Detection proportion') +
  scale_x_continuous(breaks = c(0, 300, 600, 900, 1200),
                     labels = c(r7.group[first(which.min(as.numeric(r7.group$hour.seq))), 2],
                                r7.group[first(which(as.numeric(r7.group$hour.seq) == 300)), 2],
                                r7.group[first(which(as.numeric(r7.group$hour.seq) == 600)), 2],
                                r7.group[first(which(as.numeric(r7.group$hour.seq) == 900)), 2],
                                r7.group[first(which.max(as.numeric(r7.group$hour.seq))), 2])) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  theme(panel.spacing.x = unit(1, "lines"))

# Scrap ####

# Last detection of receivers
last1.1 <- r1.2 %>% 
  filter(Tag.ID == '45')
last1.1[which.max(mdy(last1.1$Date)),]

last3.4 <- r3.3 %>% 
  filter(Tag.ID == '19')
last3.4[which.max(mdy(last3.4$Date)),]

last3.5 <- r3.6 %>% 
  filter(Tag.ID == '33')
last3.5[which.max(mdy(last3.5$Date)),]

