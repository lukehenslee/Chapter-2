#===============================================================================
# Master data 
#
# Date: March 23, 2022
# 
# Creator: Luke Henslee, ADF&G and CFOS, UAF
#
# Purpose: This code creates 'capture.loc', 'deploy.ymd.hms', 'deploy.unix', 
# 'det.hist', 'final.fate.ymd.hms', 'final.fate.unix', 'recap', 'at.large.unix',
# 'at.large.dhm', 'final.fate', 'spawn.group', and 'spawn.stream' columns and 
# adds them to the mcode list.
#===============================================================================
# NOTES: This script uses three sources of data: the raw mcode list 'mcode_raw'-
# which contains field observations from tag deploy, the detection history 
# summary 'det_sum_full'- which lists acoustic detections chronologically by 
# array/inriver receiver, and recapture data 'recap_dat'- which contains 
# information on tagged fish recaptures in subsistence, commercial, and sport 
# fisheries. 
#===============================================================================
# Load packages ################################################################
library(tidyverse)
library(data.table)
library(lubridate)
library(tools)

# Set working directory ########################################################
setwd("Q:/RESEARCH/Tagging/Github/data") # Work
setwd("C:/Users/lukeh/Desktop/Git_repos/Chapter-2") # School

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

# Capture location #############################################################

# Create empty vector
capture.loc <- vector()

# Start for() loop
for(i in 1:nrow(tag)) {
  if(tag[i,15] < 64.37609 && tag[i,15] > 64.13129) {
    capture.loc[i] <- "A"
  } else {
        capture.loc[i] <- "B"
      }
    }



tag$capture.loc <- capture.loc

  
# Deploy ymd.hms and unix timestamp ###########################################
  # Combine 'capture.date' and 'time.out' into deploy.ymd.hms
mcode$deploy.ymd.hms <- as.character(mdy_hm(paste(mcode_raw$capture.date, 
                                                   mcode_raw$time.out, 
                                                   sep = " ")))

  # Create unix timestamp
mcode$deploy.unix <- as.numeric(mdy_hm(paste(mcode_raw$capture.date, 
                                              mcode_raw$time.out, sep = " ")), 
                                 tz = 'US/Alaska')

# Detection history ############################################################
  # Create empty vector
det.hist <- aggregate(detsum[,3], detsum[1], paste, collapse = "")

  # Add new column to mcode list

tag <- left_join(tag, det.hist, by = 'tag.ID')
tag <- rename(tag, det.hist = x)

# Recapture ####################################################################

  # Create empty vector
recap.loc <- vector()

recap$recap <- ifelse(recap[,9] %in% c('6', '6a', '6b'), 'B', 
                 ifelse(recap[,9] == '5', 'A', 
                        ifelse(recap[,9] %in% c('1', '3', '7'), 'X', recap[,9])))

  # Add new column to 'tag'
tag <- left_join(tag, recap[,c(5, 13)], by = 'tag.ID')

# Final fate ###################################################################


spawn.group <- vector()

    # Start for() loop
for(i in 1:nrow(mcode)) {
  if(str_extract(mcode[i, "det.hist"], ".$") %in% ff1.3.loc |
     mcode[i, "recap"] %in% ff1.4.loc) {
    spawn.group[i] <- "3"
  } else {
  if(str_extract(mcode[i, "det.hist"], ".$") %in% ff1.4.loc |
     mcode[i, "recap"] %in% ff1.4.loc) {
    spawn.group[i] <- "4"
  } else {
    if(str_extract(mcode[i, "det.hist"], ".$") %in% ff1.5.loc |
       mcode[i, "recap"] %in% ff1.5.loc) {
      spawn.group[i] <- "5"
    } else {
      if(str_extract(mcode[i, "det.hist"], ".$") %in% ff1.6.loc |
         mcode[i, "recap"] %in% ff1.6.loc) {
        spawn.group[i] <- "6"
      } else {
        if(str_extract(mcode[i, "det.hist"], ".$") %in% ff2.N.loc |
           mcode[i, "recap"] %in% ff2.N.loc) {
          spawn.group[i] <- "N"
        } else {
          if(str_extract(mcode[i, "det.hist"], ".$") %in% ff2.S.loc |
             mcode[i, "recap"] %in% ff2.S.loc) {
            spawn.group[i] <- "S"
          } else {
            spawn.group[i] <- NA
          }
          }
        }
      }
    }
  }
}

  # Add to mcode list
mcode$spawn.group <- spawn.group

# Spawning.stream ##############################################################
  # Create vector of final fate 1's
ff1 <- c("1a", "1b", "1c")

  # Create empty vector
spawn.stream <- vector()

  # Start for() loop
for(i in 1:nrow(mcode)) {
  if(mcode[i, "final.fate"] %in% ff1) {
    if(is.na(mcode[i, "recap"]) == F) {
      spawn.stream[i] <- mcode[i, "recap"]
    } else {
      spawn.stream[i] <- str_extract(mcode[i, "det.hist"], ".$")
    }
  } else {
    spawn.stream[i] <- NA
  }
}

  # Add to mcode list
mcode$spawn.stream <- spawn.stream

# Save .csv ####################################################################
  # Export mcode
write_csv(mcode, "mcode_2021_liberal_version.csv")

