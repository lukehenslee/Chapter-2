#==================================================================================================
# Thesis chapter 2
# Date: September 7, 2022
# Creator: Luke Henslee, ADF&G and CFOS, UAF
#
# Purpose: Assign detection histories to coho salmon with paired sex, stock, and
# capture timing data.
#==================================================================================================
#NOTES: Multistate mark recapture detection histories
#
# States:
# A: Shaktoolik
# V: Entered stream draining into Shaktoolik subdistrict
# B: Unalakleet
# W: Entered stream draining into Unalakleet subdistrict
# X: Outside
#
# Detection histories are unrestricted in length, but only transitions between
# two states is recorded.
# 
# The following are impossible transitions:
# X<->V
# X<->W
# V<->W
# A<->W
# B<->V
#
#==================================================================================================

# Load packages ####
library(readr)
library(tidyverse)
library(data.table)
library(lubridate)
library(openxlsx)
library(magrittr)

# Set working directory ####
setwd("C:/Users/lhhenslee/Desktop/git_repo/Chapter-2")

# Import data ##################################################################

ch <- read.csv('data/ch/ch_stock_sex_week_year.csv')

## Pad ch with zeros
ch$ch <- str_pad(ch$ch, width = 6, side = 'left', pad = 0)


## Write it back out
write.csv(ch, 'data/ch/ch_stock_sex_week_year.csv', row.names = F)


