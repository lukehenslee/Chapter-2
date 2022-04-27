#==================================================================================================
# Thesis chapter 2
# Date: February 21, 2022
# Creator: Luke Henslee, ADF&G and CFOS, UAF
#
#Purpose: 
#==================================================================================================
#NOTES: Multistate mark recapture detection histories
#
# States:
# A: Shaktoolik
## AA: Entered stream draining into Shaktoolik subdistrict
# B: Unalakleet
## BB: Entered stream draining into Unalakleet subdistrict
# X: Outside
## XX: Entered stream draining into Norton Bay subdistrict 
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

# Build detection histories ####

# First, combine 'tag.ID' and 'capture.loc' and 'recap.hist'
ch <- data.frame(tag.ID = tag[,1], 
                 capture = tag[,25], 
                 det = tag[,26], 
                 recap = tag[,27],
                 stat.week = tag[,3],
                 year = year(mdy(tag[,2])),
                 species = tag[,12])

# Now subset for coho 
ch.coho <- ch[which(ch$species == 'coho'),]

# Now subset for coho assigned to stocks
## First read in initial ch.coho
ch.coho.v1 <- read.csv('data/ch_coho.csv')
ch.coho <- left_join(ch.coho, ch.coho.v1[,c(1,3:4)], by = 'tag.ID')

## Then subset stocks
ch.coho.stock <- ch.coho[which(is.na(ch.coho$stock) == FALSE),]

## Write it out
write.csv(ch.coho.stock, 'data/ch_coho_stock.csv')

## Get it back here
ch.coho.stock <- read.csv('ch_coho_v3.csv')

## Pad ch with zeros
ch.coho.stock$ch <- str_pad(ch.coho.stock$ch, width = 4, side = 'left', pad = 0)


## Write it back out
write.csv(ch.coho.stock, 'data/ch_coho_v3.csv')


