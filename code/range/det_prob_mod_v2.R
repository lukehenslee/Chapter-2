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
r20 <- read.csv('data/r20_array.csv')
r21 <- read.csv('data/r21_array.csv')

rec <- read.csv('data/rec.csv')

env_col <- read.csv('data/r_env_colClasses.csv')
env <- read.csv('data/r_env.csv', colClasses = paste(env_col[1,]))

# Manipulate data ####
r <- rbind(r20[,c(2:8)], r21[,c(2:8)])

r$year <- as.factor(year(mdy((r$mdy))))

## Round freq
env$freq <- as.integer(round(env$freq, 0))

## Add fail col
env$fail <- 60 - env$freq

# Add environmental data ####
## Temp ####
## Wind ####
## Tides ####


# Build model ####

summary(global <- glm(cbind(freq, fail) ~ dist + yday + Array + year(date.datetime.) + hour,
              family = binomial,
              data = r))

visreg(global, xvar = 'dist', scale = 'response')

# Try to assign 1 and 0
r$bi <- as.integer(round(r$prop), 0)

summary(global <- glm(bi ~ dist + yday + year + hour + Array + u + v + sea_level + temp + rnorm(n = nrow(r)),
                      family = binomial,
                      data = r))

visreg(global, xvar = 'Array', by = 'year', scale = 'response', gg = T,
       print.cond = T)
visreg(global, xvar = 'yday', scale = 'response', gg = T, print.cond = T)

# Compare env effects by array
blueberry <- glm(cbind(freq, fail) ~ u + v + temp + sea_level,
                 family = binomial, data = subset(env, Array == 'Blueberry Creek'))
summary(blueberry)

pointcrk <- glm(cbind(freq, fail) ~ u + v + temp + sea_level,
                 family = binomial, data = subset(env, Array == 'Point Creek'))
summary(pointcrk)

env %>% group_by(hour) %>% 
  summarize_at(vars(u, v), funs(mean))
