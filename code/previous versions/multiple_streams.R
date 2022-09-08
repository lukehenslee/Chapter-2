#===============================================================================
# Multiple streams
#
# Date: April 22, 2022
# 
# Creator: Luke Henslee, ADF&G and CFOS, UAF
#
# Purpose: This code creates identifies fish that entered more than one stream.
#===============================================================================
# NOTES: 
#===============================================================================
# Load packages ################################################################
library(tidyverse)
library(data.table)
library(lubridate)
library(tools)

# Set working directory ########################################################
setwd("C:/Users/lhhenslee/Desktop/Git_repos/Chapter-2")

# Import data ##################################################################
  # Import raw tag list
    # 'colClasses.csv' is just a list for 'colClasses' argument of read.csv() 
col <- read.csv("data/mcode_colClasses.csv", header = T)

  # Import detection history summary
tag <- read.csv("data/mcode.csv")

# Identify fish in multiple streams
tag_stream <- tag[complete.cases(tag$det.hist),]

streams <- vector()

for(i in 1:nrow(tag_stream)) {
  streams[i] <- sum(!!str_count(tag_stream[i,21], LETTERS))
}

tag_stream$streams <- streams

mult_stream <- tag_stream[which(tag_stream$streams > 1 & tag_stream$species == 'coho'),]
