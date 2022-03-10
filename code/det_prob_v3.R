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
library(readr)
library(tidyverse)
library(data.table)
library(lubridate)
library(openxlsx)
library(magrittr)
library(lubridate)
library(RColorBrewer)

# Set working directory ####
setwd("C:/Users/lhhenslee/Desktop/Luke/School/Thesis/Chapter 2/Chapter-2") # Work
setwd("C:/Users/lukeh/Desktop/School/GIT repos/Chapter-2") # School

# Source function ####
source('code/det_prob_fun.R')


# 2020 ####

# Import metadata ####
## Receiver metadata
rec <- read.csv('data/rec_2020.csv')
## Column classes
r_colClasses <- read.csv('data/r_colClasses.csv')

# Array 1 ####
## Import data ####
r1.2 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/1.2.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r1.3 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/1.3.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r1.4 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/1.4.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r1.5 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/1.5.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r1.6 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/1.6.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

## Run det() function ####
r1.2_det <- det(r1.2)
r1.3_det <- det(r1.3)
r1.4_det <- det(r1.4)
r1.5_det <- det(r1.5)
r1.6_det <- det(r1.6)

## Merge ####
r1 <- rbind(r1.2_det, r1.3_det, r1.4_det, r1.5_det, r1.6_det)
r1$freq[r1$freq > 60] <- 60 
r1$prop[r1$prop > 1] <- 1 

rm(r1.2, r1.2_det, r1.3, r1.3_det, r1.4, r1.4_det, r1.5, r1.5_det, r1.6, r1.6_det,
   r1.7, r1.7_det,)

# Array 2 ####
## Import data ####
r2.1 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/2.1.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r2.2 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/2.2.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r2.3 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/2.3.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r2.4 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/2.4.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r2.5 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/2.5.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r2.6 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/2.6.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

## Run det() function ####
r2.1_det <- det(r2.1)
r2.2_det <- det(r2.2)
r2.3_det <- det(r2.3)
r2.4_det <- det(r2.4)
r2.5_det <- det(r2.5)
r2.6_det <- det(r2.6)

## Merge ####
r2 <- rbind(r2.1_det, r2.2_det, r2.3_det, r2.4_det, r2.5_det, r2.6_det)
r2$freq[r2$freq > 60] <- 60 
r2$prop[r2$prop > 1] <- 1 

rm(r2.1, r2.1_det, r2.2, r2.2_det, r2.3, r2.3_det, r2.4, r2.4_det, r2.5, r2.5_det, r2.6, r2.6_det,
   r2.7, r2.7_det,)

# Array 3 ####
## Import data ####
r3.1 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/3.1.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))


r3.3 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/3.3.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))


r3.6 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/3.6.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

## Run det() function ####
r3.1_det <- det(r3.1)
r3.3_det <- det(r3.3)
r3.6_det <- det(r3.6)

## Merge ####
r3 <- rbind(r3.1_det, r3.3_det, r3.6_det)
r3$freq[r3$freq > 60] <- 60 
r3$prop[r3$prop > 1] <- 1 

rm(r3.1, r3.1_det, r3.2, r3.2_det, r3.3, r3.3_det, r3.4, r3.4_det, r3.5, r3.5_det, r3.6, r3.6_det)

# Array 4 ####
## Import data ####
r4.1 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/4.1.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r4.2 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/4.2.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r4.3 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/4.3.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r4.4 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/4.4.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r4.5 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/4.5.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r4.6 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/4.6.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

## Run det() function ####
r4.1_det <- det(r4.1)
r4.2_det <- det(r4.2)
r4.3_det <- det(r4.3)
r4.4_det <- det(r4.4)
r4.5_det <- det(r4.5)
r4.6_det <- det(r4.6)

## Merge ####
r4 <- rbind(r4.1_det, r4.2_det, r4.3_det, r4.4_det, r4.5_det, r4.6_det)
r4$freq[r4$freq > 60] <- 60 
r4$prop[r4$prop > 1] <- 1 

rm(r4.1, r4.1_det, r4.2, r4.2_det, r4.3, r4.3_det, r4.4, r4.4_det, r4.5, 
   r4.5_det, r4.6, r4.6_det)

# Array 5 ####
## Import data ####
r5.1 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/5.1.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r5.2 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/5.2.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r5.3 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/5.3.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r5.4 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/5.4.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r5.5 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/5.5.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r5.6 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/5.6.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

## Run det() function ####
r5.1_det <- det(r5.1)
r5.2_det <- det(r5.2)
r5.3_det <- det(r5.3)
r5.4_det <- det(r5.4)
r5.5_det <- det(r5.5)
r5.6_det <- det(r5.6)

## Merge ####
r5 <- rbind(r5.1_det, r5.2_det, r5.3_det, r5.4_det, r5.5_det, r5.6_det)
r5$freq[r5$freq > 60] <- 60 
r5$prop[r5$prop > 1] <- 1 

rm(r5.1, r5.1_det, r5.2, r5.2_det, r5.3, r5.3_det, r5.4, r5.4_det, r5.5, 
   r5.5_det, r5.6, r5.6_det) 

# Array 6 ####
## Import data ####
r6.1 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/6.1.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r6.2 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/6.2.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r6.3 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/6.3.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r6.4 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/6.4.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r6.5 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/6.5.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r6.6 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/6.6.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

## Run det() function ####
r6.1_det <- det(r6.1)
r6.2_det <- det(r6.2)
r6.3_det <- det(r6.3)
r6.4_det <- det(r6.4)
r6.5_det <- det(r6.5)
r6.6_det <- det(r6.6)

## Merge ####
r6 <- rbind(r6.1_det, r6.2_det, r6.3_det, r6.4_det, r6.5_det, r6.6_det)
r6$freq[r6$freq > 60] <- 60 
r6$prop[r6$prop > 1] <- 1 

rm(r6.1, r6.1_det, r6.2, r6.2_det, r6.3, r6.3_det, r6.4, r6.4_det, r6.5, 
   r6.5_det, r6.6, r6.6_det)

# Array 7 ####
## Import data ####
r7.1 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/7.1.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r7.2 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/7.2.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r7.3 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/7.3.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r7.4 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/7.4.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r7.5 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/7.5.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r7.6 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections/7.6.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

## Run det() function ####
r7.1_det <- det(r7.1)
r7.2_det <- det(r7.2)
r7.3_det <- det(r7.3)
r7.4_det <- det(r7.4)
r7.5_det <- det(r7.5)
r7.6_det <- det(r7.6)

## Merge ####
r7 <- rbind(r7.1_det, r7.2_det, r7.3_det, r7.4_det, r7.5_det, r7.6_det)
r7$freq[r7$freq > 60] <- 60 
r7$prop[r7$prop > 1] <- 1 

rm(r7.1, r7.1_det, r7.2, r7.2_det, r7.3, r7.3_det, r7.4, r7.4_det, r7.5, 
   r7.5_det, r7.6, r7.6_det)

# All arrays ####

r1$Array <- 'Bald Head'
r2$Array <- 'Point Dexter'
r3$Array <- 'Cape Denbeigh'
r4$Array <- 'Junction Creek'
r5$Array <- 'Blueberry Creek'
r6$Array <- 'Point Creek'
r7$Array <- 'Black Point'

r20 <- rbind(r1, r2, r3, r4, r5, r6, r7)

r20$Array <- factor(r20$Array, levels = c('Bald Head', 'Point Dexter', 'Cape Denbeigh',
                                              'Junction Creek', 'Blueberry Creek',
                                              'Point Creek', 'Black Point'))

r20.group <- r20 %>% 
  group_by(Array, date(datetime), yday, hour, dist) %>% 
  summarize(across(c(freq, prop), mean)) 

write.csv(r20.group, 'data/r20_array.csv')

# Visualize ####
my.pal <- c("#E78AC3", "#66C2A5", "#FC8D62", "#8DA0CB", "#A6D854")

r1$Array <- 'Bald Head'
r2$Array <- 'Point Dexter'
r3$Array <- 'Cape Denbeigh'
r4$Array <- 'Junction Creek'
r5$Array <- 'Blueberry Creek'
r6$Array <- 'Point Creek'
r7$Array <- 'Black Point'

r2020 <- rbind(r1, r2, r3, r4, r5, r6, r7)
r2020$Array <- factor(r2020$Array, levels = c('Bald Head', 'Point Dexter', 'Cape Denbeigh',
                                              'Junction Creek', 'Blueberry Creek',
                                              'Point Creek', 'Black Point'))

r2020$`Distance (m)` <- as.factor(r2020$dist)

r2020.plot <- ggplot(data = r2020, aes(x = as.numeric(hour.seq), y = prop, col = `Distance (m)`)) +
  geom_smooth() +
  geom_smooth(method='lm', formula= y~x, color = 'black', linetype = 'dashed', 
              se = F) +
  xlab('Day of year') +
  ylab('Detection proportion') +
  facet_wrap(~ Array, nrow = 2) +
  scale_color_brewer(palette = 'Set1') +
  scale_x_continuous(breaks = c(0, 300, 600, 900, 1200),
                     labels = c(r7[first(which.min(as.numeric(r7$hour.seq))), 2],
                                r7[first(which(as.numeric(r7$hour.seq) == 300)), 2],
                                r7[first(which(as.numeric(r7$hour.seq) == 600)), 2],
                                r7[first(which(as.numeric(r7$hour.seq) == 900)), 2],
                                r7[first(which.max(as.numeric(r7$hour.seq))), 2])) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  theme_bw() +
  theme(legend.position = c(.85,.25))

r2020.plot

ggsave(r2020.plot, file = "figs/r_2020_v2.png", width = 23, height = 15, units = "cm", dpi = 300)

r2020.plot2 <- ggplot(data = r2020, aes(x = dist, y = prop)) +
  geom_smooth(method = 'loess', aes(col = Array)) +
  geom_smooth(method='lm', formula= y~x, color = 'black', linetype = 'dashed', 
              se = F, size = 1.5) +
  xlab('Distance (m)') +
  ylab('Detection proportion') +
  scale_color_brewer(palette = 'Set1') +
  scale_x_continuous(breaks = c(300, 600, 900, 1200, 1500)) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  theme_bw() +
  theme(legend.position = c(.75,.75),
        legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black"))

r2020.plot2

ggsave(r2020.plot2, file = "figs/r_2020_dist.png", width = 23, height = 15, units = "cm", dpi = 300)



r2020.plot3 <- ggplot(data = r2020, aes(x = dist, y = prop)) +
  geom_smooth(method = 'loess', aes(col = Array)) +
  geom_smooth(method='lm', formula= y~x, color = 'black', linetype = 'dashed', 
              se = F, size = 1.5) +
  xlab('Distance (m)') +
  ylab('Detection proportion') +
  scale_color_brewer(palette = 'Set1') +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  theme_bw() +
  theme(legend.position = c(.75,.75),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))

r2020.plot2

ggsave(r2020.plot2, file = "figs/r_2020_dist.png", width = 23, height = 15, units = "cm", dpi = 300)

r7 <- r7 %>% 
  group_by(date(datetime), yday, hour, hour.seq, depth, dist) %>% 
  summarize(across(prop, mean))

r1.depth <- r1 %>% 
  group_by(hour.seq, depth) %>% 
  summarize(across(prop,mean))
r2.depth <- r2 %>% 
  group_by(hour.seq, depth) %>% 
  summarize(across(prop, mean))
r4.depth <- r4 %>% 
  group_by(hour.seq, depth) %>% 
  summarize(across(prop, mean))
r5.depth <- r5 %>% 
  group_by(hour.seq, depth) %>% 
  summarize(across(prop, mean))
r6.depth <- r6 %>% 
  group_by(hour.seq, depth) %>% 
  summarize(across(prop, mean))
r7.depth <- r7 %>% 
  group_by(hour.seq, depth) %>% 
  summarize(across(prop, mean))

r1.depth$Array <- 'Bald Head'
r2.depth$Array <- 'Point Dexter'
r4.depth$Array <- 'Junction Creek'
r5.depth$Array <- 'Blueberry Creek'
r6.depth$Array <- 'Point Creek'
r7.depth$Array <- 'Black Point'
            
r.depth <- rbind(r1.depth, r2.depth, r4.depth, r7.depth)

ggplot(r.depth, aes(x = as.factor(depth), y = prop)) +
  geom_boxplot() +
  geom_smooth(method = 'lm', se = T) +
  scale_y_continuous(limits = c(0,1))


# Mod dat ####
r1$Array <- 'Bald Head'
r2$Array <- 'Point Dexter'
r3$Array <- 'Cape Denbeigh'
r4$Array <- 'Junction Creek'
r5$Array <- 'Blueberry Creek'
r6$Array <- 'Point Creek'
r7$Array <- 'Black Point'

r2020all <- rbind(r1, r2, r3, r4, r5, r6, r7)
r2020all$Array <- factor(r2020all$Array, levels = c('Bald Head', 'Point Dexter', 'Cape Denbeigh',
                                              'Junction Creek', 'Blueberry Creek',
                                              'Point Creek', 'Black Point'))
r2020all$prop[r2020all$prop > 1] <- 1 


# Mod ####
glm1 <- glm(prop ~ dist + dist.shore + Array + yday + depth,
            family = quasibinomial, data = r2020all)

summary(glm1)

visreg(glm1, scale = 'response')

#2021 ####

rec <- read.csv('data/rec_2021.csv')

## Column classes
r_colClasses <- read.csv('data/r_colClasses.csv')
## Receiver detection data (must be named 'rX.X')

## Array 1 ####
r1.1 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/1.1.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r1.2 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/1.2.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r1.3 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/1.3.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r1.4 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/1.4.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r1.5 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/1.5.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r1.6 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/1.6.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

## Array 2 ####
r2.1 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/2.1.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r2.2 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/2.2.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r2.3 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/2.3.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r2.4 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/2.4.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r2.5 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/2.5.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r2.6 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/2.6.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

## Array 3 ####
r3.1 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/3.1.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))


r3.2 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/3.2.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))


r3.5 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/3.5.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r3.6 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/3.6.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))
# Run det() function ####

## Array 1 ####
r1.2_det <- det(r1.2)
r1.3_det <- det(r1.3)
r1.4_det <- det(r1.4)
r1.5_det <- det(r1.5)
r1.6_det <- det(r1.6)

## Array 2 ####
r2.1_det <- det(r2.1)
r2.2_det <- det(r2.2)
r2.3_det <- det(r2.3)
r2.4_det <- det(r2.4)
r2.5_det <- det(r2.5)
r2.6_det <- det(r2.6)

## Array 3 ####

r3.2_det <- det(r3.2)
r3.5_det <- det(r3.5)

# Merge into array detection proportion ####

## Array 1 ####
# Not including rec 1.3 because of data corruption
r1 <- rbind(r1.2_det, r1.3_det, r1.5_det)

r1 <- r1 %>% 
  group_by(date(datetime), yday, hour, hour.seq, dist) %>% 
  summarize(across(prop, mean))

## Array 2 ####
r2 <- rbind(r2.1_det, r2.2_det, r2.4_det, r2.5_det)

r2 <- r2 %>% 
  group_by(date(datetime), yday, hour, hour.seq, dist) %>% 
  summarize(across(prop, mean))

## Array 3 ####
r3 <- rbind(r3.2_det, r3.5_det)

r3 <- r3 %>% 
  group_by(date(datetime), yday, hour, hour.seq, dist) %>% 
  summarize(across(prop, mean))

# Visualize ####
my.pal <- c("#E78AC3", "#66C2A5", "#FC8D62", "#8DA0CB", "#A6D854")
## Array 1 ####
ggplot(data = r1, aes(x = as.numeric(hour.seq), y = prop)) +
  geom_col(position = 'dodge') +
  geom_smooth() +
  facet_wrap(~as.factor(dist)) +
  xlab('Hour sequence') +
  ylab('Detection proportion') 

r1$prop[r1$prop > 1] <- 1 

r1.plot <- ggplot(data = r1, aes(x = as.numeric(hour.seq), y = prop)) +
  geom_col(position = 'dodge') +
  geom_smooth() +
  facet_wrap(~as.factor(dist)) +
  xlab('Day of year') +
  ylab('Detection proportion') +
  scale_x_continuous(breaks = c(0, 300, 600, 900, 1200),
                     labels = c(r1[first(which.min(as.numeric(r1$hour.seq))), 2],
                                r1[first(which(as.numeric(r1$hour.seq) == 300)), 2],
                                r1[first(which(as.numeric(r1$hour.seq) == 600)), 2],
                                r1[first(which(as.numeric(r1$hour.seq) == 900)), 2],
                                r1[first(which.max(as.numeric(r1$hour.seq))), 2])) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  theme(panel.spacing.x = unit(1, "lines"))

ggsave(r1.plot, file = "figs/r1_2020.png", width = 23, height = 15, units = "cm", dpi = 300)

ggplot(data = r1, aes(x = as.numeric(hour.seq), y = prop, col = as.factor(dist))) +
  geom_smooth() +
  xlab('Day of year') +
  ylab('Detection proportion') +
  scale_color_manual(values = my.pal) +
  scale_x_continuous(breaks = c(0, 300, 600, 900, 1200),
                     labels = c(r7[first(which.min(as.numeric(r7$hour.seq))), 2],
                                r7[first(which(as.numeric(r7$hour.seq) == 300)), 2],
                                r7[first(which(as.numeric(r7$hour.seq) == 600)), 2],
                                r7[first(which(as.numeric(r7$hour.seq) == 900)), 2],
                                r7[first(which.max(as.numeric(r7$hour.seq))), 2])) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  theme_bw()

## Array 2 ####
ggplot(data = r2, aes(x = as.numeric(hour.seq), y = prop)) +
  geom_col(position = 'dodge') +
  geom_smooth() +
  facet_wrap(~as.factor(dist)) +
  xlab('Hour sequence') +
  ylab('Detection proportion')

r2$prop[r2$prop > 1] <- 1 

r2.plot <- ggplot(data = r2, aes(x = as.numeric(hour.seq), y = prop)) +
  geom_col(position = 'dodge') +
  geom_smooth() +
  facet_wrap(~as.factor(dist)) +
  xlab('Day of year') +
  ylab('Detection proportion') +
  scale_x_continuous(breaks = c(0, 300, 600, 900, 1200),
                     labels = c(r2[first(which.min(as.numeric(r2$hour.seq))), 2],
                                r2[first(which(as.numeric(r2$hour.seq) == 300)), 2],
                                r2[first(which(as.numeric(r2$hour.seq) == 600)), 2],
                                r2[first(which(as.numeric(r2$hour.seq) == 900)), 2],
                                r2[first(which.max(as.numeric(r2$hour.seq))), 2])) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  theme(panel.spacing.x = unit(1, "lines"))

ggsave(r2.plot, file = "figs/r2_2020.png", width = 23, height = 15, units = "cm", dpi = 300)

ggplot(data = r2, aes(x = as.numeric(hour.seq), y = prop, col = as.factor(dist))) +
  geom_smooth() +
  xlab('Day of year') +
  ylab('Detection proportion') +
  scale_x_continuous(breaks = c(0, 300, 600, 900, 1200),
                     labels = c(r7[first(which.min(as.numeric(r7$hour.seq))), 2],
                                r7[first(which(as.numeric(r7$hour.seq) == 300)), 2],
                                r7[first(which(as.numeric(r7$hour.seq) == 600)), 2],
                                r7[first(which(as.numeric(r7$hour.seq) == 900)), 2],
                                r7[first(which.max(as.numeric(r7$hour.seq))), 2])) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) 
## Array 3 ####
ggplot(data = r3, aes(x = as.numeric(hour.seq), y = prop)) +
  geom_col(position = 'dodge') +
  geom_smooth() +
  facet_wrap(~as.factor(dist)) +
  xlab('Hour sequence') +
  ylab('Detection proportion')

r3$prop[r3$prop > 1] <- 1 

r3.plot <- ggplot(data = r3, aes(x = as.numeric(hour.seq), y = prop)) +
  geom_col(position = 'dodge') +
  geom_smooth() +
  facet_wrap(~as.factor(dist)) +
  xlab('Day of year') +
  ylab('Detection proportion') +
  scale_x_continuous(breaks = c(0, 300, 600, 900, 1200),
                     labels = c(r3[first(which.min(as.numeric(r3$hour.seq))), 2],
                                r3[first(which(as.numeric(r3$hour.seq) == 300)), 2],
                                r3[first(which(as.numeric(r3$hour.seq) == 600)), 2],
                                r3[first(which(as.numeric(r3$hour.seq) == 900)), 2],
                                r3[first(which.max(as.numeric(r3$hour.seq))), 2])) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  theme(panel.spacing.x = unit(1, "lines"))

ggsave(r3.plot, file = "figs/r3_2020.png", width = 23, height = 15, units = "cm", dpi = 300)

ggplot(data = r3, aes(x = as.numeric(hour.seq), y = prop, col = as.factor(dist))) +
  geom_smooth() +
  xlab('Day of year') +
  ylab('Detection proportion') +
  scale_x_continuous(breaks = c(0, 300, 600, 900, 1200),
                     labels = c(r7[first(which.min(as.numeric(r7$hour.seq))), 2],
                                r7[first(which(as.numeric(r7$hour.seq) == 300)), 2],
                                r7[first(which(as.numeric(r7$hour.seq) == 600)), 2],
                                r7[first(which(as.numeric(r7$hour.seq) == 900)), 2],
                                r7[first(which.max(as.numeric(r7$hour.seq))), 2])) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) 

# Array 4 ####
r4.1 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/4.1.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r4.2 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/4.2.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r4.3 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/4.3.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r4.4 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/4.4.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r4.5 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/4.5.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r4.6 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/4.6.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r4.1_det <- det(r4.1)
r4.2_det <- det(r4.2)
r4.3_det <- det(r4.3)
r4.4_det <- det(r4.4)
r4.5_det <- det(r4.5)
r4.6_det <- det(r4.6)

r4 <- rbind(r4.2_det, r4.3_det, r4.4_det, r4.6_det)

r4 <- r4 %>% 
  group_by(date(datetime), yday, hour, hour.seq, dist) %>% 
  summarize(across(prop, mean))

ggplot(data = r4, aes(x = as.numeric(hour.seq), y = prop)) +
  geom_col(position = 'dodge') +
  geom_smooth() +
  facet_wrap(~as.factor(dist)) +
  xlab('Hour sequence') +
  ylab('Detection proportion')

r4$prop[r4$prop > 1] <- 1 

r4.plot <- ggplot(data = r4, aes(x = as.numeric(hour.seq), y = prop)) +
  geom_col(position = 'dodge') +
  geom_smooth() +
  facet_wrap(~as.factor(dist)) +
  xlab('Day of year') +
  ylab('Detection proportion') +
  scale_x_continuous(breaks = c(0, 300, 600, 900, 1200),
                     labels = c(r4[first(which.min(as.numeric(r4$hour.seq))), 2],
                                r4[first(which(as.numeric(r4$hour.seq) == 300)), 2],
                                r4[first(which(as.numeric(r4$hour.seq) == 600)), 2],
                                r4[first(which(as.numeric(r4$hour.seq) == 900)), 2],
                                r4[first(which.max(as.numeric(r4$hour.seq))), 2])) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  theme(panel.spacing.x = unit(1, "lines"))

ggsave(r4.plot, file = "figs/r4_2020.png", width = 23, height = 15, units = "cm", dpi = 300)

ggplot(data = r4, aes(x = as.numeric(hour.seq), y = prop, col = as.factor(dist))) +
  geom_smooth() +
  xlab('Day of year') +
  ylab('Detection proportion') +
  scale_x_continuous(breaks = c(0, 300, 600, 900, 1200),
                     labels = c(r7[first(which.min(as.numeric(r7$hour.seq))), 2],
                                r7[first(which(as.numeric(r7$hour.seq) == 300)), 2],
                                r7[first(which(as.numeric(r7$hour.seq) == 600)), 2],
                                r7[first(which(as.numeric(r7$hour.seq) == 900)), 2],
                                r7[first(which.max(as.numeric(r7$hour.seq))), 2])) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) 

# Array 5 ####
r5.1 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/5.1.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r5.2 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/5.2.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r5.3 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/5.3.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r5.4 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/5.4.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r5.5 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/5.5.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r5.6 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/5.6.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r5.1_det <- det(r5.1)
r5.2_det <- det(r5.2)
r5.3_det <- det(r5.3)
r5.4_det <- det(r5.4)
r5.5_det <- det(r5.5)
r5.6_det <- det(r5.6)

r5 <- rbind(r5.1_det, r5.2_det, r5.3_det, r5.4_det, r5.6_det)

r5 <- r5 %>% 
  group_by(date(datetime), yday, hour, hour.seq, dist) %>% 
  summarize(across(prop, mean))

ggplot(data = r5, aes(x = as.numeric(hour.seq), y = prop)) +
  geom_col(position = 'dodge') +
  geom_smooth() +
  facet_wrap(~as.factor(dist)) +
  xlab('Hour sequence') +
  ylab('Detection proportion')


r5$prop[r5$prop > 1] <- 1 

r5.plot <- ggplot(data = r5, aes(x = as.numeric(hour.seq), y = prop)) +
  geom_col(position = 'dodge') +
  geom_smooth() +
  facet_wrap(~as.factor(dist)) +
  xlab('Day of year') +
  ylab('Detection proportion') +
  scale_x_continuous(breaks = c(0, 300, 600, 900, 1200),
                     labels = c(r5[first(which.min(as.numeric(r5$hour.seq))), 2],
                                r5[first(which(as.numeric(r5$hour.seq) == 300)), 2],
                                r5[first(which(as.numeric(r5$hour.seq) == 600)), 2],
                                r5[first(which(as.numeric(r5$hour.seq) == 900)), 2],
                                r5[first(which.max(as.numeric(r5$hour.seq))), 2])) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  theme(panel.spacing.x = unit(1, "lines"))

ggsave(r5.plot, file = "figs/r5_2020.png", width = 23, height = 15, units = "cm", dpi = 300)

ggplot(data = r5, aes(x = as.numeric(hour.seq), y = prop, col = as.factor(dist))) +
  geom_smooth() +
  xlab('Day of year') +
  ylab('Detection proportion') +
  scale_x_continuous(breaks = c(0, 300, 600, 900, 1200),
                     labels = c(r7[first(which.min(as.numeric(r7$hour.seq))), 2],
                                r7[first(which(as.numeric(r7$hour.seq) == 300)), 2],
                                r7[first(which(as.numeric(r7$hour.seq) == 600)), 2],
                                r7[first(which(as.numeric(r7$hour.seq) == 900)), 2],
                                r7[first(which.max(as.numeric(r7$hour.seq))), 2])) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) 

# Array 6 ####
r6.1 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/6.1.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r6.2 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/6.2.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r6.3 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/6.3.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r6.4 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/6.4.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r6.5 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/6.5.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r6.6 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/6.6.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r6.1_det <- det(r6.1)
r6.2_det <- det(r6.2)
r6.3_det <- det(r6.3)
r6.4_det <- det(r6.4)
r6.5_det <- det(r6.5)
r6.6_det <- det(r6.6)

r6 <- rbind(r6.1_det, r6.2_det, r6.3_det, r6.4_det, r6.5_det, r6.6_det)

r6 <- r6 %>% 
  group_by(date(datetime), yday, hour, hour.seq, dist) %>% 
  summarize(across(prop, mean))

r6$prop[r6$prop > 1] <- 1 

r6.plot <- ggplot(data = r6, aes(x = as.numeric(hour.seq), y = prop)) +
  geom_col(position = 'dodge') +
  geom_smooth() +
  facet_wrap(~as.factor(dist)) +
  xlab('Day of year') +
  ylab('Detection proportion') +
  scale_x_continuous(breaks = c(0, 300, 600, 900, 1200),
                     labels = c(r6[first(which.min(as.numeric(r6$hour.seq))), 2],
                                r6[first(which(as.numeric(r6$hour.seq) == 300)), 2],
                                r6[first(which(as.numeric(r6$hour.seq) == 600)), 2],
                                r6[first(which(as.numeric(r6$hour.seq) == 900)), 2],
                                r6[first(which.max(as.numeric(r6$hour.seq))), 2])) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0))+
  theme(panel.spacing.x = unit(1, "lines"))

ggsave(r6.plot, file = "figs/r6_2020.png", width = 23, height = 15, units = "cm", dpi = 300)

ggplot(data = r6, aes(x = as.numeric(hour.seq), y = prop, col = as.factor(dist))) +
  geom_smooth() +
  xlab('Day of year') +
  ylab('Detection proportion') +
  scale_x_continuous(breaks = c(0, 300, 600, 900, 1200),
                     labels = c(r7[first(which.min(as.numeric(r7$hour.seq))), 2],
                                r7[first(which(as.numeric(r7$hour.seq) == 300)), 2],
                                r7[first(which(as.numeric(r7$hour.seq) == 600)), 2],
                                r7[first(which(as.numeric(r7$hour.seq) == 900)), 2],
                                r7[first(which.max(as.numeric(r7$hour.seq))), 2])) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  theme(panel.spacing.x = unit(1, "lines"))


# Array 7 ####
r7.1 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/7.1.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r7.2 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/7.2.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r7.3 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/7.3.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r7.4 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/7.4.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r7.5 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/7.5.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r7.6 <- read.csv('Q:/RESEARCH/Tagging/Github/data/detections_2021/7.6.csv', skip = c(47),
                 col.names = names(r_colClasses), 
                 colClasses = paste(r_colClasses[1,]))

r7.1_det <- det(r7.1)
r7.2_det <- det(r7.2)
r7.3_det <- det(r7.3)
r7.4_det <- det(r7.4)
r7.5_det <- det(r7.5)
r7.6_det <- det(r7.6)

r7 <- rbind(r7.1_det, r7.3_det, r7.4_det, r7.5_det, r7.6_det)

r7 <- r7 %>% 
  group_by(date(datetime), yday, hour, hour.seq, dist) %>% 
  summarize(across(prop, mean))

r7.plot <- ggplot(data = r7, aes(x = as.numeric(hour.seq), y = prop)) +
  geom_col(position = 'dodge') +
  geom_smooth() +
  facet_wrap(~as.factor(dist)) +
  xlab('Day of year') +
  ylab('Detection proportion') +
  scale_x_continuous(breaks = c(0, 300, 600, 900, 1200),
                     labels = c(r7[first(which.min(as.numeric(r7$hour.seq))), 2],
                                r7[first(which(as.numeric(r7$hour.seq) == 300)), 2],
                                r7[first(which(as.numeric(r7$hour.seq) == 600)), 2],
                                r7[first(which(as.numeric(r7$hour.seq) == 900)), 2],
                                r7[first(which.max(as.numeric(r7$hour.seq))), 2])) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  theme(panel.spacing.x = unit(1, "lines"))

ggsave(r7.plot, file = "figs/r7_2020.png", width = 23, height = 15, units = "cm", dpi = 300)


ggplot(data = r7, aes(x = as.numeric(hour.seq), y = prop, col = as.factor(dist))) +
  geom_smooth() +
  xlab('Day of year') +
  ylab('Detection proportion') +
  scale_x_continuous(breaks = c(0, 500, 1000, 1500),
                     labels = c(r7[first(which.min(as.numeric(r7$hour.seq))), 2],
                                r7[first(which(as.numeric(r7$hour.seq) == 500)), 2],
                                r7[first(which(as.numeric(r7$hour.seq) == 1000)), 2],
                                r7[first(which.max(as.numeric(r7$hour.seq))), 2])) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  theme(panel.spacing.x = unit(1, "lines"))

# All arrays together ####

r1$Array <- 'Bald Head'
r2$Array <- 'Point Dexter'
r3$Array <- 'Cape Denbeigh'
r4$Array <- 'Junction Creek'
r5$Array <- 'Blueberry Creek'
r6$Array <- 'Point Creek'
r7$Array <- 'Black Point'

r2021 <- rbind(r1, r2, r3, r4, r5, r6, r7)
r2021$Array <- factor(r2021$Array, levels = c('Bald Head', 'Point Dexter', 'Cape Denbeigh',
                                              'Junction Creek', 'Blueberry Creek',
                                              'Point Creek', 'Black Point'))

r2021$`Distance (m)` <- as.factor(r2021$dist)

r2021.plot <- ggplot(data = r2021, aes(x = as.numeric(hour.seq), y = prop)) +
  geom_smooth(aes(col = `Distance (m)`)) +
  geom_smooth(method='lm', formula= y~x, color = 'black', linetype = 'dashed') +
  xlab('Day of year') +
  ylab('Detection proportion') +
  facet_wrap(~ Array, nrow = 2) +
  scale_color_brewer(palette = 'Set1') +
  scale_x_continuous(breaks = c(0, 500, 1000, 1500),
                     labels = c(r7[first(which.min(as.numeric(r7$hour.seq))), 2],
                                r7[first(which(as.numeric(r7$hour.seq) == 500)), 2],
                                r7[first(which(as.numeric(r7$hour.seq) == 1000)), 2],
                                r7[first(which.max(as.numeric(r7$hour.seq))), 2])) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  theme_bw() +
  theme(legend.position = c(.85,.23))

r2021.plot

ggsave(r2021.plot, file = "figs/r_2021_v2.png", width = 23, height = 15, units = "cm", dpi = 300)

# Scrap ####

# Last detection of receivers
last1.1 <- r1.2 %>% 
  filter(Tag.ID == '55')
last1.1[which.max(mdy(last1.1$Date)),]

last1.4 <- r1.5 %>% 
  filter(Tag.ID == '38')
last1.4[which.max(mdy(last1.4$Date)),]

last1.6 <- r1.5 %>% 
  filter(Tag.ID == '51')
last1.6[which.max(mdy(last1.6$Date)),]

last2.6 <- r2.5 %>% 
  filter(Tag.ID == '18')
last2.6[which.max(mdy(last2.6$Date)),]

last3.3 <- r3.2 %>% 
  filter(Tag.ID == '27')
last3.3[which.max(mdy(last3.3$Date)),]

last3.4 <- r3.5 %>% 
  filter(Tag.ID == '26')
last3.4[which.max(mdy(last3.4$Date)),]

last4.1 <- r4.2 %>% 
  filter(Tag.ID == '6')
last4.1[which.max(mdy(last4.1$Date)),]
