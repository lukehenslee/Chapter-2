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
<<<<<<< HEAD
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

=======
setwd("Q:/RESEARCH/Tagging/Github/data")

# Import data ##################################################################
  # Import raw mcode list
    # 'colClasses.csv' is just a list for 'colClasses' argument of read.csv() 
colClasses <- read.csv("colClasses_raw.csv", header = T)
    # Import the raw data
mcode_raw <- read.csv("mcode_raw_2021.csv") 

  # Create 'mcode' which will manipulate and add columns to 'mcode_raw'
mcode <- mcode_raw

  # Import detection history summary
det_sum <- read.csv("det_sum_2021/det_sum_2021.csv")

  # Import recapture data
recap_dat <- read.csv("recap_2021.csv")

# Mesh size ####################################################################
  # Excel converts mesh size to a date, so we'll fix that
  # Create empty vector
#mesh <- vector()

  # Start for() loop
#for(i in 1:nrow(mcode_raw)) {
#  if(mcode_raw[i, 17] == "8-Jul") {
#    mesh[i] <- "5-7/8 in" 
#  } else {
#    if(mcode_raw[i, 17] == "8-May") {
#      mesh[i] <- "5-5/8 in"
#    } else {
#      mesh[i] <- "combined"
#    }
#  }
#}

  # Add to mcode
#mcode$mesh.size <- mesh
>>>>>>> 173152ced44f0ae880ddbc133fe4c91709ec4c2c

# Capture location #############################################################
  # The study area is split into 4 marine capture regions, this script assigns
  # the capture location based on latitude 

  # Create empty vector
capture.loc <- vector()
  
  # Start for() loop
<<<<<<< HEAD
for(i in 1:nrow(tag)) {
  if(tag[i,15] < 64.37609 && tag[i,15] > 64.13129) {
    capture.loc[i] <- "5"
} else {
  if(tag[i,15] < 64.13129 && tag[i,15] > 63.95384) {
    capture.loc[i] <- "6a"
  } else {
    if(tag[i,15] < 63.95384 && tag[i,15] > 63.69065) {
=======
for(i in 1:nrow(mcode_raw)) {
  if(mcode_raw[i,15] < 64.37609 && mcode_raw[i,15] > 64.13129) {
    capture.loc[i] <- "5"
} else {
  if(mcode_raw[i,15] < 64.13129 && mcode_raw[i,15] > 63.95384) {
    capture.loc[i] <- "6a"
  } else {
    if(mcode_raw[i,15] < 63.95384 && mcode_raw[i,15] > 63.69065) {
>>>>>>> 173152ced44f0ae880ddbc133fe4c91709ec4c2c
      capture.loc[i] <- "6b"
    } else {
      capture.loc[i] <- "6c"
    }
  }
}
}

<<<<<<< HEAD
# Create empty vector
cap.loc <- vector()

# Start for() loop
for(i in 1:nrow(tag)) {
  if(tag[i,15] < 64.37609 && tag[i,15] > 64.13129) {
    cap.loc[i] <- "A"
  } else {
        cap.loc[i] <- "B"
      }
    }



tag$cap.loc <- cap.loc
=======
  # Add capture.loc to mcode list
mcode$capture.loc <- capture.loc
>>>>>>> 173152ced44f0ae880ddbc133fe4c91709ec4c2c

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
det.hist <- vector()
ff.det.ymd.hms <- vector()
ff.det.unix <- vector()

  # Start for() loop
<<<<<<< HEAD
for(i in 1:nrow(tag)) {
  if(tag[i, 1] %in% detsum[, 1]) {
    idx <- filter(detsum, detsum[, 1] == tag[i, 1])
    idx2 <- aggregate(idx[,3], idx[1], paste, collapse = "")
=======
for(i in 1:nrow(mcode_raw)) {
  if(mcode_raw[i, 1] %in% det_sum[, 1]) {
    idx <- dplyr::filter(det_sum, det_sum[, 1] == mcode_raw[i, 1])
    idx2 <- aggregate(idx[,3], idx[1], paste, collapse = ",")
>>>>>>> 173152ced44f0ae880ddbc133fe4c91709ec4c2c
    det.hist[i] <- idx2[,2]
    ff.det.ymd.hms[i] <- idx[nrow(idx), 4]
    ff.det.unix[i] <- idx[nrow(idx), 5]
  } else {
    det.hist[i] <- NA
  }
}

  # Add new column to mcode list
<<<<<<< HEAD
tag$det.hist <- det.hist

# Recapture ####################################################################
  # Create empty vector
recap1 <- vector()
=======
mcode$det.hist <- det.hist

# Recapture ####################################################################
  # Create empty vector
recap <- vector()
>>>>>>> 173152ced44f0ae880ddbc133fe4c91709ec4c2c
ff.recap.ymd.hms <- vector()
ff.recap.unix <- vector()

  # This for() loop adds the recap location or returns NA's to applicable rows
  # It also replaces final.fate.ymd.hms and final.fate.unix for recaptured fish
<<<<<<< HEAD
for(i in 1:nrow(tag)) {
  if(tag[i, 1] %in% recap[ ,5]) {
    idx <- dplyr::filter(recap, recap[ ,5] == tag[i, 1]) 
    recap1[i] <- paste0(idx[9]) 
=======
for(i in 1:nrow(mcode_raw)) {
  if(mcode_raw[i, 1] %in% recap_dat[ ,5]) {
    idx <- dplyr::filter(recap_dat, recap_dat[ ,5] == mcode_raw[i, 1]) 
    recap[i] <- paste0(idx[9]) 
>>>>>>> 173152ced44f0ae880ddbc133fe4c91709ec4c2c
    ff.recap.ymd.hms[i] <- as.character(mdy_hm(idx[7]))
    ff.recap.unix[i] <- as.numeric(mdy_hm(idx[7]), 
                                     tz = 'US/Alaska')
  } else {
<<<<<<< HEAD
    recap1[i] <- NA
=======
    recap[i] <- NA
>>>>>>> 173152ced44f0ae880ddbc133fe4c91709ec4c2c
    ff.recap.ymd.hms[i] <- NA
    ff.recap.unix[i] <- NA
  }
}

<<<<<<< HEAD
recap2 <- ifelse(recap1 %in% c('6', '6a', '6b'), '9', ifelse(recap1 == '5', '8', recap1))

recap3 <- ifelse(recap1 %in% c('6', '6a', '6b'), 'B', 
                 ifelse(recap1 == '5', 'A', 
                        ifelse(recap1 == c('1', '3', '7'), 'Y', 
                               ifelse(recap1 %in% c('S', 'T'), 'A', 
                                      ifelse(recap1 %in% c('E', 'U', 'G'), 'B', recap1)))))

  # Add new column to mcode list
tag$recap <- recap3
=======
  # Add new column to mcode list
mcode$recap <- recap
>>>>>>> 173152ced44f0ae880ddbc133fe4c91709ec4c2c

# Final fate ###################################################################

# In this version, a fish last detected by marine receiver array 2 is a 'SD4'
# stock. A fish last detected by marine receiver array 3 is a 'N' stock

  # Vector of final fate 1 locations
ff1.loc <- c("K", "T", "Y", "I", "S", "B", "U", "G", "2")

  # Vector of final fate 2 locations
ff2.loc <- c("1", "3", "7")

  # Vector of fate 3 locations
ff3.loc <- c("4", "5", "6", "6a", "6b", "6c")

  # Empty vector
final.fate <- vector()

  # Big, dumb, for() loop
    # This for() loop follows the decision tree outlined in the ADFG SOP to 
    # assign final fates
for(i in 1:nrow(mcode)) {
  if(str_extract(mcode[i, "det.hist"], ".$") %in% ff1.loc &
     is.na(mcode[i, "recap"] == T)) {
    final.fate[i] <- "1a"
  } else {
      if(is.na(mcode[i, "det.hist"]) == F &
         mcode[i, "recap"] %in% ff1.loc) {
        final.fate[i] <- "1b"
      } else {
        if(is.na(mcode[i, "det.hist"]) == T &
           mcode[i, "recap"] %in% ff1.loc) {
          final.fate[i] <- "1c"
        } else {
          if(str_extract(mcode[i, "det.hist"], ".$") %in% ff2.loc &
             is.na(mcode[i, "recap"]) == T) {
            final.fate[i] <- "2a"
          } else {
            if(is.na(mcode[i, "det.hist"]) == F &
               mcode[i, "recap"] %in% ff2.loc) {
              final.fate[i] <- "2b"
            } else {
              if(is.na(mcode[i, "det.hist"]) == T &
                 mcode[i, "recap"] %in% ff2.loc) {
                final.fate[i] <- "2c"
              } else {
                if(str_extract(mcode[i, "det.hist"], ".$") %in% ff3.loc &
                   is.na(mcode[i, "recap"]) == T) {
                  final.fate[i] <- "3a"
                } else {
                  if(is.na(mcode[i, "det.hist"]) == F &
                     mcode[i, "recap"] %in% ff3.loc) {
                    final.fate[i] <- "3b"
                  } else {
                    if(is.na(mcode[i, "det.hist"]) == T &
                       mcode[i, "recap"] %in% ff3.loc) {
                      final.fate[i] <- "3c"
                    } else {
                      final.fate[i] <- "4"
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

  # Add final.fate to mcode list
mcode$final.fate <- final.fate

# Time at large ################################################################
# This script calculates the time elapsed between tag deployment and final 
# detection

  # For final fates 1b and 2b, if the fish was recaptured AFTER inriver 
  # detection, we want to use the detection time rather than recap time
ff.det <- c("1a", "2a", "3a")
ff.det.recap <- c("1b", "2b", "3b")
ff.recap <- c("1c", "2c", "3c")

    # Create vector of fates to use detection time
    # Create empty vectors
ff.ymd.hms <- vector()
ff.unix <- vector()

for(i in 1:nrow(mcode)) {
  if(mcode[i, "final.fate"] %in% ff.det) {
    ff.ymd.hms[i] <- ff.det.ymd.hms[i]
    ff.unix[i] <- ff.det.unix[i]
  } else {
    if(mcode[i, "final.fate"] %in% ff.det.recap &
       str_extract(mcode[i, "det.hist"], ".$") %in% ff1.loc) {
      ff.ymd.hms[i] <- ff.det.ymd.hms[i]
      ff.unix[i] <- ff.det.unix[i]
    } else {
      ff.ymd.hms[i] <- ff.recap.ymd.hms[i]
      ff.unix[i] <- ff.recap.unix[i]
    }
  }
}

    # Add to mcode list
mcode$ff.ymd.hms <- ff.ymd.hms
mcode$ff.unix <- ff.unix

  # Now, simply subtract unix time of final detection from unix time of deployment
mcode$at.large.unix <- mcode$ff.unix - mcode$deploy.unix

mcode$at.large.dhm <- seconds_to_period(mcode$at.large.unix)
# Spawning group ###############################################################
  # mcode 2295 had final detection in S river but was then recaptured in marine
    # waters of SD 5, mistakenly assigned to spawn.group 5- need to fix
    # 7/25/2021

  # Fate 1 in SD3
ff1.3.loc <- c("K", "B")

  # Fate 1 in SD4
ff1.4.loc <- c("Y", "I", "2")

  # Fate 1 in SD5
ff1.5.loc <- c("S", "T")

  # Fate 1 in SD6
ff1.6.loc <- c("U", "G")

  # Vector of final fate 2 locations
ff2.N.loc <- c("1", "3")
ff2.S.loc <- "7"

  # Use for() loop to assign spawning group
    # Create empty vector
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

