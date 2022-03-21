#==================================================================================================
# Detection probability
# Date: February 21, 2022
# Creator: Luke Henslee, ADF&G and CFOS, UAF
#
# Purpose: This script uses beacon pings within arrays to explore detection 
# probabilities throughout the 2020 and 2021 seasons
#==================================================================================================
#NOTES: Beacons were set to ping every 60 seconds 
#==================================================================================================

# Load packages ####
library(tidyverse)
library(visreg)


# Set working directory ####
setwd("C:/Users/lhhenslee/Desktop/Luke/School/Thesis/Chapter 2/Chapter-2") # Work
setwd("C:/Users/lukeh/Desktop/School/GIT repos/Chapter-2") # School

# Import data ####
