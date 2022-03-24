#==================================================================================================
# Thesis chapter 2
# Date: February 21, 2022
# Creator: Luke Henslee, ADF&G and CFOS, UAF
#
#Purpose: 
#==================================================================================================
#NOTES: Multistate mark recapture detection histories
# States:
# A: Shaktoolik
# B: Unalakleet 1
# C: Unalakleet 2
# D: Unalakleet 3
# Y: Exploratory stream
# X: Spawning stream
#==================================================================================================

# Load packages ####
library(readr)
library(tidyverse)
library(data.table)
library(lubridate)
library(openxlsx)
library(magrittr)

# Set working directory ####
setwd("C:/Users/lhhenslee/Desktop/Luke/School/Thesis/Chapter 2")

# Import data ####

# Import m-code list for reference 
col <- read.csv("data/mcode_colClasses.csv", header = T)

tags <- read.csv("data/mcode.csv", colClasses = paste(col[1,]))

coho <- tags %>% 
  filter(species == 'coho')

## This analysis only uses marine receiver detection, so delete inriver det
det.2 <- det %>% 
  filter(substr(receiver.ID, 1, 1) %in% c(1:7))

## Add an array ID
det.2$array.ID <- substr(det.2$receiver.ID, 1, 1)

## Add lateral ID
det.2$lat.ID <- substr(det.2$receiver.ID, 3, 3)


ggplot(coho.det, aes(x = rec)) +
  geom_histogram(stat = 'count')

## Assign distance from shore
### Receivers had wider spacing in 2021, so we need to account for that
### In 2020, receivers were placed 300 m from shore and 300 m between 
### for a total distance of 2.1 km from shore.
### In 2021, the first receiver was 300 m from shore and the next receiver was
### 300 m from that- subsequent receivers were placed 500 meters apart for a 
### total distance of 3.1 m from shore.

### In 2020, 300, 600, 900, 1200, 1500, 1800

### In 2021, 300, 600, 1100, 1600, 2100, 2600

### First, parse mdy.hms
det.2