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

