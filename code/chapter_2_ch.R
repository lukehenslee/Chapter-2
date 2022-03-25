#==================================================================================================
# Thesis chapter 2
# Date: February 21, 2022
# Creator: Luke Henslee, ADF&G and CFOS, UAF
#
#Purpose: 
#==================================================================================================
#NOTES: Multistate mark recapture detection histories
<<<<<<< HEAD
#
# States:
# A: Norton Bay
# B: Shaktoolik
# C: Unalakleet
# Y: Outside
# X: Freshwater
=======
# States:
# A: Shaktoolik
# B: Unalakleet 1
# C: Unalakleet 2
# D: Unalakleet 3
# Y: Exploratory stream
# X: Spawning stream
>>>>>>> 173152ced44f0ae880ddbc133fe4c91709ec4c2c
#==================================================================================================

# Load packages ####
library(readr)
library(tidyverse)
library(data.table)
library(lubridate)
library(openxlsx)
library(magrittr)

# Set working directory ####
setwd("C:/Users/lukeh/Desktop/Git_repos/Chapter-2")

# Import data ##################################################################
# Import detsum
# 'colClasses.csv' is just a list for 'colClasses' argument of read.csv() 
col <- read.csv("data/detsum_colClasses.csv", header = T)
detsum <- read.csv("data/detsum.csv", colClasses = paste(col[1,])) 

# Import tag data
col <- read.csv("data/tag_colClasses.csv", header = T)
tag <- read.csv("data/tag.csv", colClasses = paste(col[1,]))

# Import recapture data
col <- read.csv("data/recap_colClasses.csv", header = T)
recap <- read.csv("data/recap.csv", colClasses = paste(col[1,]))

# Subset data ####
coho <- tag[which(tag$species == 'coho'),]

# Build detection histories ####

# First we will combine 'capture.loc', 'det.hist', and 'recap' into one history

c <- unite(tag[, 25:27], col = hist, na.rm = TRUE, sep = '')
c.hist <- str_replace_all(c$hist, '[5,6]', '')
c.hist <- str_replace_all(c.hist, '[E,U,G]', 'B')
c.hist <- str_replace_all(c.hist, '[S, T,]', 'A')
c.hist <- ifelse(substr(c.hist, 1, 2) == 'A4', str_replace(c.hist, '[4]', 'B'), c.hist)
c.hist <- ifelse(substr(c.hist, 1, 2) == 'B4', str_replace(c.hist, '[4]', 'A'), c.hist)
c.hist <- ifelse(substr(c.hist, 1, 2) == 'A3', str_replace(c.hist, '[3]', 'Y'), c.hist)
c.hist <- ifelse(substr(c.hist, 1, 2) == 'B7', str_replace(c.hist, '[7]', 'Y'), c.hist)
write.csv(cbind(tag$det.hist, c$hist, c.hist, recap1), 'ch3.csv')

# If the last two characters are the same (e.g. 'UU' for a fish detected by rec
# and then recaptured), just keep one
recap4 <- ifelse(substr(det.hist, nchar(det.hist), nchar(det.hist)) ==
                    recap3, NA, recap3)

tag$recap <- recap4

dub <- tag[which(substr(tag$c.hist, nchar(tag$c.hist), nchar(tag$c.hist)) ==
            substr(tag$c.hist, nchar(tag$c.hist)-1, nchar(tag$c.hist)-1)),]


histx <- str_replace_all(hist, '[5,6]', '')
histx <- str_replace(histx, '[8]', 'B')
histx <- str_replace(histx, '[9]', 'C')



histb <- histx[which(substr(histx, 1, 1) == 'B')]
histbff <- ifelse(substr(histb, nchar(histb), nchar(histb)) %in% c('S', 'T', 'Y', 'I', 'N', 'E', 'U', 'G', ''), str_replace(histb, '[S,T]', 'B'), histb)
histb2 <- ifelse(substr(histb, 2, 2) %in% c('S', 'T'), str_replace(histb, '[S,T]', 'B'), histb)
histb3 <- ifelse(substr(histb2, 2, 2) == '4', str_replace(histb2, '[4]', 'C'), histb2)
histb4 <- ifelse(substr(histb3, 2, 2) == '3', str_replace(histb3, '[3]', 'Y'), histb3)
histb5 <- ifelse(substr(histb4, 3, 3) %in% c('E', 'U', 'G'), str_replace(histb4, '[E,U,G]', 'C'), histb4)
histb6 <- ifelse(substr(histb5, 3, 3) == '4', str_replace(histb5, '[4]', 'B'), histb5)
histb7 <- ifelse(substr(histb6, 3, 3) == '7', str_replace(histb6, '[7]', 'Y'), histb6)


table(histb7)
write.csv(cbind(histb, histb7), 'skk.hist.1.csv')
histbx <- read.csv('skk.hist.1.csv', header = FALSE)
cohoB <- coho %>% filter(cap.loc == '8')
cohoB$ch <- str_pad(histbx$V4, width = 7, side =('right'), pad = '0')


histc <- histx[which(substr(histx, 1, 1) == 'C')]
