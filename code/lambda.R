#==================================================================================================
# Calculate detection probabilities 
# Date: March 22, 2022
# Creator: Luke Henslee, ADF&G and CFOS, UAF
#
# Purpose: 
#==================================================================================================
#NOTES: 
#==================================================================================================

# Load packages ####
library(tidyverse)
library(visreg)


# Set working directory ####
setwd("C:/Users/lukeh/Desktop/Git_repos/Chapter-2") # School
setwd("C:/Users/lhhenslee/Desktop/Git_repos/Chapter-2") # Work

# Import data ####

# Mcode list
col <- read.csv('data/mcode_colClasses.csv')
tags <- read.csv('data/mcode.csv', colClasses = paste(col[1,]))

## Subset
coho <- tags[which(tags[,8] == 'coho'), ]

# Detection summaries
col <- read.csv('data/detsum_colClasses.csv')
det <- read.csv('data/detsum.csv', colClasses = paste(col[1,]))

# Manipulate data ####
coho$tag.ID <- coho$mcode
detcoho <- merge(det, coho[,c(18,33)], by = 'tag.ID')

# Receiver array 2 ####
## Any fish detected by array 2
det2 <- detcoho[which(detcoho[,3] == '2'),]

## Any fish tagged south of Point Dexter, and detected north of Point Dexter
nof2.1 <- c('I', 'N', 'K')
detpast2 <- detcoho[which(detcoho[,3] %in% nof2.1),]

## Fish detected by array 2 detected beyond array 2
det2yes <- det2[which(detpast2[,1] %in% det2[,1]),]

## Any fish in 'detpast2' not in 'det2'
det2no <- det2[which(!(detpast2[,1] %in% det2[,1])),]

## Proportion detected
nrow(det2yes)/nrow(detpast2)

## 100% detection baby!

# Receiver array 3 ####
## Any fish detected by array 3
det3 <- detcoho[which(detcoho[,3] == '3'),]

## Any fish tagged south of 3, and detected north of 3
nof3.1 <- c('I', 'N', 'K', '2', '1', 'B')
nof3.2 <- c('I', 'N', 'K', '2')
detpast3 <- detcoho[which(detcoho[,3] %in% nof3.2),]

## Fish detected by array 3 detected beyond array 3
det3yes <- det3[which(detpast3[,1] %in% det3[,1]),]

det3no <- anti_join(detpast3, det3, by = 'tag.ID')

## Proportion detected
nrow(det3yes)/nrow(detpast3)

# 0.92857 if you don't include array 1, 0.76471 if you do

# Receiver array 4 ####
## Any fish detected by array 4
det4 <- detcoho[which(detcoho[,3] == '4'),]

## Fish tagged north of 4
tagn4 <- detcoho[which(detcoho[,8] == '5'),]

## Fish tagged south of 4
tags4 <- detcoho[which(detcoho[,8] %in% c('6a', '6b')),]

## Any fish tagged north of 4, and detected south of 4
sof4 <- c('E', 'U', 'G', '5', '6', '7')
detsof4 <- tagn4[which(tagn4[,3] %in% sof4),]

## Any fish tagged s of 4, and detected n of 4
nof4 <- c('T', 'S', 'N', 'I', 'K', '3', '2', '1')
detnof4 <- tags4[which(tags4[,3] %in% nof4),]

## Combine all fish known to migrate past array 4
detpast4 <- rbind(detnof4, detsof4)

## Fish detected by array 4 detected beyond array 4
det4yes <- detpast4[which(detpast4[,1] %in% det4[,1]),]

## Any fish in 'detpast4' not in 'det4'
det4no <- det4[which(!(detpast4[,1] %in% det4[,1])),]

## Proportion detected
nrow(det4yes)/nrow(detpast4)

## 0.9363296, not bad
