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
                 capture = capture.loc, 
                 det = det.hist2, 
                 recap = tag2$recap)

