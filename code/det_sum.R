#===============================================================================
# Building detection summaries 
#
# Date: 18/8/2021, 2021
# Creator: Luke Henslee, ADF&G and CFOS, UAF
#
# Purpose: 
#===============================================================================
# NOTES: 
#===============================================================================

# Load packages ####
library(tidyverse)
library(data.table)
library(lubridate)
library(tools)
library(ggplot2)

# Set working directory ########################################################
setwd("Q:/RESEARCH/Tagging/Github/data")

# Import data ##################################################################
  # Make list of file names in detection folder
temp <- list.files("det_all_2021", pattern = "*.csv")

  # Read file content
det <- lapply(paste0("det_all_2021/", temp), read.csv, 
              colClasses = c("integer", "factor", "character", "integer"))

  # Read file names
filenames <- temp %>% basename() %>% as.list()

  # Combine file content list with file name list to create tag ID
det <- mapply(c, det, file_path_sans_ext(filenames), SIMPLIFY = F)

  # Combine lists and lable 'tag.ID' column
det_all <- rbindlist(det, fill = T)
names(det_all)[5] <- "tag.ID"

# Create chronological array detection marker 'det.order' ######################
  # Split 'receiver.ID' column- this allows identification of arrays rather than 
  # individual receivers 
det_all$receiver.ID <- as.character(det_all$receiver.ID)
det_all <- separate(data = det_all, col = receiver.ID,
                    into = as.character(c("array", "rec")), sep = "\\.")
det_all$array <- as.factor(det_all$array)

  # Create 'det.order' ID- this orders observation by arrays chronologically 
det_all <- det_all %>% 
  group_by(tag.ID) %>% 
  mutate(det.order = rleid(array))

# Filter milling fish detections at terminal site ##############################
  # If there is a gap of >8 hrs in the final det.order, we will assume the 
  # fish was milling in front of an inriver receiver, and keep just the first 
  # detection event
    # Create list of terminal tag detections
det_term <- det_all %>% 
  group_by(tag.ID) %>% 
  dplyr::filter(det.order == max(det.order))

  # Split into lists by 'tag.ID'
det_list_term <- split(det_term, det_term$tag.ID)

  # Loop finds gaps >8 hours and keeps only observations before that
    # Create empty list
det_list <- list() 

    # Create vector of inriver locations
ff1.loc <- c("K", "T", "Y", "I", "S", "B", "U", "G")

  # Start loop
for(i in (1:length(det_list_term))) {
  if(as.character(det_list_term[[i]][[1,2]]) %in% ff1.loc) {
    idx <- c(0,cumsum(abs(diff(x = det_list_term[[i]][[5]])) > 28800))
    if(sum(idx) > 0) { # Split by time gap and keep only initial detections
      list <- split(det_list_term[[i]], idx)
      det_list[[i]] <- list[[1]]
    } else {
      det_list[[i]] <- det_list_term[[i]]
    } 
  } else {
    det_list[[i]] <- det_list_term[[i]]
  }
}

    # Convert list to dataframe
det_sum_term <- do.call(rbind, det_list)

# Combine terminal detections with previous detections #########################
  # Create dataframe of initial detections
det_init <- det_all %>% 
  group_by(tag.ID) %>% 
  dplyr::filter(det.order != max(det.order))

  # Combine dataframes
det_sum <- rbind(det_sum_term, det_init)

# Identify multiple detections of milling fish by same marine array ############
  # Now we decide if there is a gap of >8 hours within a det_order and if there 
  # is, split the detection event into multiple groups and add an identifier 

    # Split into list by 'tag.ID' and 'det.order'
det_order <- split(det_sum, det_sum$tag.ID)
det_order <- lapply(det_order, function(x) split(x, x$det.order))

    # Create empty list
det_list <- list() 

    # Start loop
for(i in (1:length(det_order))) {
  for(t in (1:length(det_order[[i]]))) {
    idx <- c(0, cumsum(abs(diff(x = det_order[[i]][[t]][[5]])) > 28800))
    if(sum(idx) == 0) {
      det_order[[i]][[t]][[8]] <- paste0(".", 0)
      det_list[[1 + length(det_list)]] <- det_order[[i]][[t]]
    } else {
      list <- split(det_order[[i]][[t]], idx)
      for(m in 1:length(list)) {
        list[[m]][[8]] <- paste0(".", m)
        det_list[[length(det_list) + 1]] <- list[[m]]
      }
    }
  }
}

# Find max power values for each group #########################################
  # We assume that the maximum detection power indicates the closest the fish 
  # passed to an array
for(i in (1:length(det_list))) {
  det_list[[i]] <- dplyr::filter(det_list[[i]], det_list[[i]][[1]] == max(det_list[[i]][[1]]))
}

  # Combine lists into new df
det_sum_max <- do.call(rbind, det_list)

# Combine det.order and V8 into "det.order"
det_sum_max <- unite(det_sum_max, det.order, c(det.order, ...8), sep = "")

  # Then remove any dupliate max power for each group
det_sum_max <- det_sum_max[!duplicated(det_sum_max[c("tag.ID", "det.order")]),]

# Clean dataframe ##############################################################
  # Remove 'rec' and 'det.order' and order columns
det_sum_fin <- det_sum_max[ ,-c(3,7)]
det_sum_fin <- det_sum_fin %>% 
  relocate(tag.ID)

  # Arrange chronologically 
det_sum_fin <- arrange(det_sum_fin, unix) 

# Write .csv file for detection summary ########################################
  # Set destination for summaries 
setwd("Q:/RESEARCH/Tagging/Github/output/det_sum_2021")

  # Group by 'tag.ID' and create .csv files 
det_sum_fin %>%
  group_by(tag.ID) %>%
  group_walk(~ write_csv(.x, paste0(.y$tag.ID, ".csv")))

  # Create one big data frame and create .csv file
write_csv(det_sum_fin, "det_sum.csv")

# Create copies in the data forlder
setwd("Q:/RESEARCH/Tagging/Github/data/det_sum_2021")

  # Group by 'tag.ID' and create .csv files 
det_sum_fin %>%
  group_by(tag.ID) %>%
  group_walk(~ write_csv(.x, paste0(.y$tag.ID, ".csv")))

  # Create one big data frame and create .csv file
write_csv(det_sum_fin, "det_sum_2021.csv")
  