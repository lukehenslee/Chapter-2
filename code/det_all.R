#==================================================================================================
# Importing and filtering marine array receiver data
# Date: May 14, 2021
# Creator: Luke Henslee, ADF&G and CFOS, UAF
#
#Purpose: To import ENS acoustic receiver data and output detection histories 
# for all tags- this differs from v1 in that it creates a separate .csv file for
# each tag ID
#==================================================================================================
#NOTES: Array 5 contained receivers 15 and 35, which are also Tag ID numbers. 
# Since each receiver sends a beacon signal to ensure proper function, it 
# obscures detection of tags 15 and 35. Therefore, we filter these Tag IDs out 
# of array 5.
#
# A similar situation arises in array 7, which contains receiver 55. We filter 
# Tag ID 55 out of array 7 detection history. Lotek is aware of the issue and 
# working on fixing it for 2021.
#
# No tags detected in Kwiniuk or Tubutulik rivers and receiver data is omitted. 
#==================================================================================================

# Load packages ####
library(readr)
library(tidyverse)
library(data.table)
library(lubridate)
library(openxlsx)
library(magrittr)

# Set working directory ####
setwd("Q:/RESEARCH/Tagging/Github/data/detections")

# Import m-code list for reference 
mcode <- read_csv("Q:/RESEARCH/Tagging/Github/data/M_code_list_raw.csv")
  # Create vector of applicable tag IDs
tagID <- mcode %>% 
  dplyr::filter(`M-code` %in% seq(75, 7995, by = 20), 
                 Species == "Coho") 

# Import receiver data and combine ####
  # Make list of file names in detection folder
temp <- list.files(pattern = "*.csv")

  # Read file content
det <- lapply(temp, read_csv, 
        col_types = cols(Date = col_date(format = "%m/%d/%Y"), 
        Power = col_double(), TOA = col_skip(), 
        `Tag ID` = col_character(), Time = col_time(format = "%H:%M:%S"), 
        Type = col_skip(), Value = col_skip()), skip = 45)

  # Read file names
filenames <- temp %>% basename() %>% as.list()

  # Combine file content list with file name list and remove ".csv"
det_named <- mapply(c, det, substr(filenames, 1, 3), SIMPLIFY = F)

  # Combine lists and lable receiver ID column
det_all <- rbindlist(det_named, fill = T)
names(det_all)[5] <- "receiver.ID"

names(det_all)[3] <- "tag.ID"

# Filter detections ####
det_all_filter <- dplyr::filter(det_all, between(Date, as.Date("2020-7-29"), as.Date("2020-9-15")), 
                       tag.ID %in% tagID$`M-code`,
                       Power > 0) %>% 
       group_by(tag.ID) 

# Modify date/time column ####
det_all_filter$ymd_hms <- paste(det_all_filter$Date, det_all_filter$Time, sep = " ") 

det_all_filter$ymd_hms_numeric <- as.numeric(ymd_hms(det_all_filter$ymd_hms, tz = 'US/Alaska'))

det_all_filter <- subset(det_all_filter, select = -c(Date, Time))

# Arrange chronologically 
det_all_filter_arrange <- arrange(det_all_filter, ymd_hms_numeric)

# Write .csv files ####

# Make separate .csv file for each tag
setwd("Q:/RESEARCH/Tagging/Github/output/species_filter_arranged")

det_all_filter_arrange %>%
  group_by(tag.ID) %>%
  group_walk(~ write_csv(.x, paste0(.y$tag.ID, ".csv")))
  
  