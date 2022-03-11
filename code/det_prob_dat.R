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
library(RColorBrewer)

# Set working directory ####
setwd("C:/Users/lhhenslee/Desktop/Luke/School/Thesis/Chapter 2/Chapter-2") # Work
setwd("C:/Users/lukeh/Desktop/School/GIT repos/Chapter-2") # School

# Source function ####
source('code/det_prob_fun.R')


# 2020 ####

# Import metadata ####
## Receiver metadata
rec <- read.csv('data/rec_2020.csv')
## Column classes
r_colClasses <- read.csv('data/r_colClasses.csv')

# Array 1 ####
## Import data ####
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

## Run det() function ####
r1.2_det <- det(r1.2)
r1.3_det <- det(r1.3)
r1.4_det <- det(r1.4)
r1.5_det <- det(r1.5)
r1.6_det <- det(r1.6)

## Merge ####
r1 <- rbind(r1.2_det, r1.3_det, r1.4_det, r1.5_det, r1.6_det)
r1$freq[r1$freq > 60] <- 60 
r1$prop[r1$prop > 1] <- 1 

rm(r1.2, r1.2_det, r1.3, r1.3_det, r1.4, r1.4_det, r1.5, r1.5_det, r1.6, r1.6_det)

# Array 2 ####
## Import data ####
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

## Run det() function ####
r2.1_det <- det(r2.1)
r2.2_det <- det(r2.2)
r2.3_det <- det(r2.3)
r2.4_det <- det(r2.4)
r2.5_det <- det(r2.5)
r2.6_det <- det(r2.6)

## Merge ####
r2 <- rbind(r2.1_det, r2.2_det, r2.3_det, r2.4_det, r2.5_det, r2.6_det)
r2$freq[r2$freq > 60] <- 60 
r2$prop[r2$prop > 1] <- 1 

rm(r2.1, r2.1_det, r2.2, r2.2_det, r2.3, r2.3_det, r2.4, r2.4_det, r2.5, r2.5_det, r2.6, r2.6_det)

# Array 3 ####
## Import data ####
r3.1 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/3.1.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))


r3.3 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/3.3.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))


r3.6 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/3.6.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

## Run det() function ####
r3.1_det <- det(r3.1)
r3.3_det <- det(r3.3)
r3.6_det <- det(r3.6)

## Merge ####
r3 <- rbind(r3.1_det, r3.3_det, r3.6_det)
r3$freq[r3$freq > 60] <- 60 
r3$prop[r3$prop > 1] <- 1 

rm(r3.1, r3.1_det, r3.2, r3.2_det, r3.3, r3.3_det, r3.4, r3.4_det, r3.5, r3.5_det, r3.6, r3.6_det)

# Array 4 ####
## Import data ####
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

## Run det() function ####
r4.1_det <- det(r4.1)
r4.2_det <- det(r4.2)
r4.3_det <- det(r4.3)
r4.4_det <- det(r4.4)
r4.5_det <- det(r4.5)
r4.6_det <- det(r4.6)

## Merge ####
r4 <- rbind(r4.1_det, r4.2_det, r4.3_det, r4.4_det, r4.5_det, r4.6_det)
r4$freq[r4$freq > 60] <- 60 
r4$prop[r4$prop > 1] <- 1 

rm(r4.1, r4.1_det, r4.2, r4.2_det, r4.3, r4.3_det, r4.4, r4.4_det, r4.5, 
   r4.5_det, r4.6, r4.6_det)

# Array 5 ####
## Import data ####
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

## Run det() function ####
r5.1_det <- det(r5.1)
r5.2_det <- det(r5.2)
r5.3_det <- det(r5.3)
r5.4_det <- det(r5.4)
r5.5_det <- det(r5.5)
r5.6_det <- det(r5.6)

## Merge ####
r5 <- rbind(r5.1_det, r5.2_det, r5.3_det, r5.4_det, r5.5_det, r5.6_det)
r5$freq[r5$freq > 60] <- 60 
r5$prop[r5$prop > 1] <- 1 

rm(r5.1, r5.1_det, r5.2, r5.2_det, r5.3, r5.3_det, r5.4, r5.4_det, r5.5, 
   r5.5_det, r5.6, r5.6_det) 

# Array 6 ####
## Import data ####
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

## Run det() function ####
r6.1_det <- det(r6.1)
r6.2_det <- det(r6.2)
r6.3_det <- det(r6.3)
r6.4_det <- det(r6.4)
r6.5_det <- det(r6.5)
r6.6_det <- det(r6.6)

## Merge ####
r6 <- rbind(r6.1_det, r6.2_det, r6.3_det, r6.4_det, r6.5_det, r6.6_det)
r6$freq[r6$freq > 60] <- 60 
r6$prop[r6$prop > 1] <- 1 

rm(r6.1, r6.1_det, r6.2, r6.2_det, r6.3, r6.3_det, r6.4, r6.4_det, r6.5, 
   r6.5_det, r6.6, r6.6_det)

# Array 7 ####
## Import data ####
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

## Run det() function ####
r7.1_det <- det(r7.1)
r7.2_det <- det(r7.2)
r7.3_det <- det(r7.3)
r7.4_det <- det(r7.4)
r7.5_det <- det(r7.5)
r7.6_det <- det(r7.6)

## Merge ####
r7 <- rbind(r7.1_det, r7.2_det, r7.3_det, r7.4_det, r7.5_det, r7.6_det)
r7$freq[r7$freq > 60] <- 60 
r7$prop[r7$prop > 1] <- 1 

rm(r7.1, r7.1_det, r7.2, r7.2_det, r7.3, r7.3_det, r7.4, r7.4_det, r7.5, 
   r7.5_det, r7.6, r7.6_det)

# All arrays ####

r1$Array <- 'Bald Head'
r2$Array <- 'Point Dexter'
r3$Array <- 'Cape Denbeigh'
r4$Array <- 'Junction Creek'
r5$Array <- 'Blueberry Creek'
r6$Array <- 'Point Creek'
r7$Array <- 'Black Point'

r20 <- rbind(r1, r2, r3, r4, r5, r6, r7)

r20$Array <- factor(r20$Array, levels = c('Bald Head', 'Point Dexter', 'Cape Denbeigh',
                                              'Junction Creek', 'Blueberry Creek',
                                              'Point Creek', 'Black Point'))

r20.group <- r20 %>% 
  group_by(Array, date(datetime), yday, hour, dist) %>% 
  summarize(across(c(freq, prop), mean)) 

write.csv(r20.group, 'data/r20_array.csv')

# 2021 ####
## Import metadata ####
rec <- read.csv('data/rec_2021.csv')
## Column classes 
r_colClasses <- read.csv('data/r_colClasses.csv')

# Array 1 ####
## Import data ####
r1.2 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/1.2.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r1.3 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/1.3.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r1.5 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/1.5.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))


## Run det() function ####
r1.2_det <- det(r1.2)
r1.3_det <- det(r1.3)
r1.5_det <- det(r1.5)

## Merge ####
r1 <- rbind(r1.2_det, r1.3_det, r1.5_det)
r1$freq[r1$freq > 60] <- 60 
r1$prop[r1$prop > 1] <- 1 

rm(r1.2, r1.2_det, r1.3, r1.3_det, r1.5, r1.5_det)

# Array 2 ####
## Import data ####
r2.1 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/2.1.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r2.2 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/2.2.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r2.4 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/2.4.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r2.5 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/2.5.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

## Run det() function ####
r2.1_det <- det(r2.1)
r2.2_det <- det(r2.2)
r2.4_det <- det(r2.4)
r2.5_det <- det(r2.5)


## Merge ####
r2 <- rbind(r2.1_det, r2.2_det, r2.4_det, r2.5_det)
r2$freq[r2$freq > 60] <- 60 
r2$prop[r2$prop > 1] <- 1 

rm(r2.1, r2.1_det, r2.2, r2.2_det, r2.4, r2.4_det, r2.5, r2.5_det)

# Array 3 ####
## Import data ####
r3.1 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/3.1.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))


r3.2 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/3.2.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))


r3.5 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/3.5.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

## Run det() function ####
r3.1_det <- det(r3.1)
r3.2_det <- det(r3.2)
r3.5_det <- det(r3.5)

## Merge ####
r3 <- rbind(r3.1_det, r3.2_det, r3.5_det)
r3$freq[r3$freq > 60] <- 60 
r3$prop[r3$prop > 1] <- 1 

rm(r3.1, r3.1_det, r3.2, r3.2_det, r3.5, r3.5_det)

# Array 4 ####
## Import data ####
r4.2 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/4.2.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r4.3 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/4.3.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r4.4 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/4.4.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))


r4.6 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/4.6.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

## Run det() function ####
r4.2_det <- det(r4.2)
r4.3_det <- det(r4.3)
r4.4_det <- det(r4.4)
r4.6_det <- det(r4.6)

## Merge ####
r4 <- rbind(r4.2_det, r4.3_det, r4.4_det, r4.6_det)
r4$freq[r4$freq > 60] <- 60 
r4$prop[r4$prop > 1] <- 1 

rm(r4.2, r4.2_det, r4.3, r4.3_det, r4.4, r4.4_det, r4.6, r4.6_det)

# Array 5 ####
## Import data ####
r5.1 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/5.1.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r5.2 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/5.2.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r5.3 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/5.3.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r5.4 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/5.4.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r5.6 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/5.6.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

## Run det() function ####
r5.1_det <- det(r5.1)
r5.2_det <- det(r5.2)
r5.3_det <- det(r5.3)
r5.4_det <- det(r5.4)
r5.6_det <- det(r5.6)

## Merge ####
r5 <- rbind(r5.1_det, r5.2_det, r5.3_det, r5.4_det, r5.6_det)
r5$freq[r5$freq > 60] <- 60 
r5$prop[r5$prop > 1] <- 1 

rm(r5.1, r5.1_det, r5.2, r5.2_det, r5.3, r5.3_det, r5.4, r5.4_det, r5.6, r5.6_det) 

# Array 6 ####
## Import data ####
r6.1 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/6.1.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r6.2 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/6.2.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r6.3 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/6.3.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r6.4 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/6.4.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r6.5 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/6.5.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r6.6 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/6.6.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

## Run det() function ####
r6.1_det <- det(r6.1)
r6.2_det <- det(r6.2)
r6.3_det <- det(r6.3)
r6.4_det <- det(r6.4)
r6.5_det <- det(r6.5)
r6.6_det <- det(r6.6)

## Merge ####
r6 <- rbind(r6.1_det, r6.2_det, r6.3_det, r6.4_det, r6.5_det, r6.6_det)
r6$freq[r6$freq > 60] <- 60 
r6$prop[r6$prop > 1] <- 1 

rm(r6.1, r6.1_det, r6.2, r6.2_det, r6.3, r6.3_det, r6.4, r6.4_det, r6.5, 
   r6.5_det, r6.6, r6.6_det)

# Array 7 ####
## Import data ####
r7.1 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/7.1.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r7.2 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/7.2.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r7.3 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/7.3.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r7.4 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/7.4.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r7.5 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/7.5.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r7.6 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/7.6.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

## Run det() function ####
r7.1_det <- det(r7.1)
r7.2_det <- det(r7.2)
r7.3_det <- det(r7.3)
r7.4_det <- det(r7.4)
r7.5_det <- det(r7.5)
r7.6_det <- det(r7.6)

## Merge ####
r7 <- rbind(r7.1_det, r7.2_det, r7.3_det, r7.4_det, r7.5_det, r7.6_det)
r7$freq[r7$freq > 60] <- 60 
r7$prop[r7$prop > 1] <- 1 

rm(r7.1, r7.1_det, r7.2, r7.2_det, r7.3, r7.3_det, r7.4, r7.4_det, r7.5, 
   r7.5_det, r7.6, r7.6_det)

# All arrays ####

r1$Array <- 'Bald Head'
r2$Array <- 'Point Dexter'
r3$Array <- 'Cape Denbeigh'
r4$Array <- 'Junction Creek'
r5$Array <- 'Blueberry Creek'
r6$Array <- 'Point Creek'
r7$Array <- 'Black Point'

r21 <- rbind(r1, r2, r3, r4, r5, r6, r7)

r21$Array <- factor(r21$Array, levels = c('Bald Head', 'Point Dexter', 'Cape Denbeigh',
                                          'Junction Creek', 'Blueberry Creek',
                                          'Point Creek', 'Black Point'))

r21.group <- r21 %>% 
  group_by(Array, date(datetime), yday, hour, dist) %>% 
  summarize(across(c(freq, prop), mean)) 

write.csv(r21.group, 'data/r21_array.csv')
