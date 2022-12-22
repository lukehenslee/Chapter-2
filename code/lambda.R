#==================================================================================================
# Calculate detection probabilities 
# Date: March 22, 2022
# Creator: Luke Henslee, ADF&G and CFOS, UAF
#
# Purpose: 
#==================================================================================================
#NOTES: 
#
# A -> X: 0.89
# A -> B: 0.91
# A -> A: 1.00
#
# B -> X: 1.00
# B -> A: 0.88
# B -> B: 1.00
#
# X -> A: 0.89
# X -> B: 1.00
# X -> X: 1.00
#==================================================================================================

# Load packages ####
library(tidyverse)
library(visreg)
library(lubridate)


# Set working directory ####
setwd("C:/Users/lukeh/Desktop/Git_repos/Chapter-2") # School
setwd("C:/Users/lhhenslee/Desktop/Git_repos/Chapter-2") # Work

# Import data ####

# Mcode list
col <- read.csv('data/mcode_colClasses.csv')
tags <- read.csv('data/mcode.csv', colClasses = paste(col[1,]))

## Subset
coho <- tags[which(tags[,8] == 'coho'), ]
coho20 <- tags[which(tags[,8] == 'coho' & year(mdy_hm(tags[,19])) == '2020'), ]
coho21 <- tags[which(tags[,8] == 'coho' & year(mdy_hm(tags[,19])) == '2021'), ]

# Detection summaries
col <- read.csv('data/detsum_colClasses.csv')
det <- read.csv('data/detsum.csv', colClasses = paste(col[1,]))

# Manipulate data ####
coho$tag.ID <- coho$mcode
coho20$tag.ID <- coho20$mcode
coho21$tag.ID <- coho21$mcode

detcoho <- merge(det, coho[,c(18,33)], by = 'tag.ID')
detcoho20 <- merge(det, coho20[,c(18,33)], by = 'tag.ID')
detcoho21 <- merge(det, coho21[,c(18,33)], by = 'tag.ID')


# Marine- both years ####

## Array 1 ####
det1 <- distinct(detcoho[which(detcoho[,3] == '1'),], tag.ID, array, .keep_all = TRUE)

## Receiver array 2 ####
## Any fish detected by array 2
det2 <- distinct(detcoho[which(detcoho[,3] == '2'),], tag.ID, array, .keep_all = TRUE)

## Any fish tagged south of Point Dexter, and detected north of Point Dexter
nof2.1 <- c('I', 'N', 'K')
detpast2 <- distinct(detcoho[which(detcoho[,3] %in% nof2.1),], tag.ID, .keep_all = TRUE)

## Fish detected by array 2 detected beyond array 2
det2yes <- det2[which(detpast2[,1] %in% det2[,1]),]

## Any fish in 'detpast2' not in 'det2'
det2no <- det2[which(!(detpast2[,1] %in% det2[,1])),]

## Proportion detected
nrow(det2yes)/nrow(detpast2)

## 100%

## Receiver array 3 ####
## Any fish detected by array 3
det3 <- distinct(detcoho[which(detcoho[,3] == '3'),], tag.ID, array, .keep_all = TRUE)

## Any fish tagged south of 3, and detected north of 3
nof3.1 <- c('I', 'N', 'K', '2', '1', 'B')
nof3.2 <- c('I', 'N', 'K', '2')
detpast3 <- distinct(detcoho[which(detcoho[,3] %in% nof3.2),], tag.ID, .keep_all = TRUE)

## Fish detected by array 3 detected beyond array 3
det3yes <- det3[which(det3[,1] %in% detpast3[,1]),]

det3no <- anti_join(detpast3, det3, by = 'tag.ID')

## Proportion detected
nrow(det3yes)/nrow(detpast3)

# 100%

## Receiver array 4 ####
## Any fish detected by array 4
det4 <- distinct(detcoho[which(detcoho[,3] == '4'),], tag.ID, array, .keep_all = TRUE)

## Fish tagged north of 4
tagn4 <- distinct(detcoho[which(detcoho[,7] == '5'),], tag.ID, .keep_all = TRUE)

## Fish tagged south of 4
tags4 <- distinct(detcoho[which(detcoho[,7] %in% c('6a', '6b')),], tag.ID, .keep_all = TRUE)

## Any fish tagged north of 4, and detected south of 4
sof4 <- c('E', 'U', 'G', '5', '6', '7')
detsof4 <- tagn4[which(tagn4[,3] %in% sof4),]

## Any fish tagged s of 4, and detected n of 4
nof4 <- c('T', 'S', 'N', 'I', 'K', '3', '2', '1')
detnof4 <- tags4[which(tags4[,3] %in% nof4),]

## Combine all fish known to migrate past array 4
detpast4 <- rbind(detnof4, detsof4)

## Fish detected by array 4 detected beyond array 4
det4yes <- det4[which(det4[,1] %in% detpast4[,1]),]

## Any fish in 'detpast4' not in 'det4'
det4no <- anti_join(detpast4, det4, by = 'tag.ID')

## Proportion detected
nrow(det4yes)/nrow(detpast4)

## Proportion detected going south
det4yes <- det4[which(det4[,1] %in% detsof4[,1]),]
nrow(det4yes) / nrow(detsof4)

## Proportion detected going north
det4yes <- det4[which(det4[,1] %in% detnof4[,1]),]
nrow(det4yes) / nrow(detnof4)

## 0.92

## Receiver array 5 ####
## Any fish detected by array 5
det5 <- distinct(detcoho[which(detcoho[,3] == '5'),], tag.ID, array, .keep_all = TRUE)

## Fish tagged north of 5
tagn5 <- distinct(detcoho[which(detcoho[,7] %in% c('6a', '5')),], tag.ID, .keep_all = TRUE)

## Fish tagged south of 5
tags5 <- distinct(detcoho[which(detcoho[,7] == '6b'),], tag.ID, .keep_all = TRUE)

## Any fish tagged north of 5, and detected south of 5
sof5 <- c('U', 'G', '6', '7')
detsof5 <- tagn5[which(tagn5[,3] %in% sof5),]

## Any fish tagged s of 5, and detected n of 5
nof5 <- c('E', 'T', 'S', 'N', 'I', 'K', '4', '3', '2', '1')
detnof5 <- tags5[which(tags5[,3] %in% nof5),]

## Combine all fish known to migrate past array 5
detpast5 <- rbind(detnof5, detsof5)

## Fish detected by array 5 detected beyond array 5
det5yes <- det5[which(det5[,1] %in% detpast5[,1]),]

## Any fish in 'detpast5' not in 'det5'
det5no <- anti_join(detpast5, det5, by = 'tag.ID')

## Proportion detected
nrow(det5yes)/nrow(detpast5)

# 0.943662

## Receiver array 6 ####
## Any fish detected by array 6
det6 <- distinct(detcoho[which(detcoho[,3] == '6'),], tag.ID, array, .keep_all = TRUE)

## Any fish tagged north of 6, and detected south of 6
sof6 <- c('G', '7')

detpast6 <- distinct(detcoho[which(detcoho[,3] %in% sof6),], tag.ID, .keep_all = TRUE)

## Fish detected by array 6 detected beyond array 6
det6yes <- det6[which(detpast6[,1] %in% det6[,1]),]

det6no <- anti_join(detpast6, det6, by = 'tag.ID')

## Proportion detected
nrow(det6yes)/nrow(detpast6)

# 0.9393939


# Marine 2020 ####
## Array 1 ####
det1 <- distinct(detcoho20[which(detcoho20[,3] == '1'),], tag.ID, array, .keep_all = TRUE)

sd(c(rep(1, 1), rep(0, 7)))/
  sqrt(nrow(det4yes))

## Receiver array 2 ####
## Any fish detected by array 2
det2 <- distinct(detcoho20[which(detcoho20[,3] == '2'),], tag.ID, array, .keep_all = TRUE)

## Any fish tagged south of Point Dexter, and detected north of Point Dexter
nof2.1 <- c('I', 'N', 'K')
detpast2 <- distinct(detcoho20[which(detcoho20[,3] %in% nof2.1),], tag.ID, .keep_all = TRUE)

## Fish detected by array 2 detected beyond array 2
det2yes <- det2[which(detpast2[,1] %in% det2[,1]),]

## Any fish in 'detpast2' not in 'det2'
det2no <- det2[which(!(detpast2[,1] %in% det2[,1])),]

## Proportion detected
nrow(det2yes)/nrow(detpast2)

## 100%

## Receiver array 3 ####
## Any fish detected by array 3
det3 <- distinct(detcoho20[which(detcoho20[,3] == '3'),], tag.ID, array, .keep_all = TRUE)

## Any fish tagged south of 3, and detected north of 3
nof3.1 <- c('I', 'N', 'K', '2', '1', 'B')
nof3.2 <- c('I', 'N', 'K', '2')
detpast3 <- distinct(detcoho20[which(detcoho20[,3] %in% nof3.2),], tag.ID, .keep_all = TRUE)

## Fish detected by array 3 detected beyond array 3
det3yes <- det3[which(det3[,1] %in% detpast3[,1]),]

det3no <- anti_join(detpast3, det3, by = 'tag.ID')

## Proportion detected
nrow(det3yes)/nrow(detpast3)

# 100%

## Receiver array 4 ####
## Any fish detected by array 4
det4 <- distinct(detcoho20[which(detcoho20[,3] == '4'),], tag.ID, array, .keep_all = TRUE)

## Fish tagged north of 4
tagn4 <- distinct(detcoho20[which(detcoho20[,7] == '5'),], tag.ID, .keep_all = TRUE)

## Fish tagged south of 4
tags4 <- distinct(detcoho20[which(detcoho20[,7] %in% c('6a', '6b')),], tag.ID, .keep_all = TRUE)

## Any fish tagged north of 4, and detected south of 4
sof4 <- c('E', 'U', 'G', '5', '6', '7')
detsof4 <- tagn4[which(tagn4[,3] %in% sof4),]

## Any fish tagged s of 4, and detected n of 4
nof4 <- c('T', 'S', 'N', 'I', 'K', '3', '2', '1')
detnof4 <- tags4[which(tags4[,3] %in% nof4),]

## Combine all fish known to migrate past array 4
detpast4 <- rbind(detnof4, detsof4)

## Fish detected by array 4 detected beyond array 4
det4yes <- det4[which(det4[,1] %in% detpast4[,1]),]

## Any fish in 'detpast4' not in 'det4'
det4no <- anti_join(detpast4, det4, by = 'tag.ID')

## Proportion detected
nrow(det4yes)/nrow(detpast4)

## 0.92
sd(c(rep(1, nrow(det4yes)), rep(0, nrow(detpast4))))/
  sqrt(nrow(det4yes))

## Receiver array 5 ####
## Any fish detected by array 5
det5 <- distinct(detcoho20[which(detcoho20[,3] == '5'),], tag.ID, array, .keep_all = TRUE)

## Fish tagged north of 5
tagn5 <- distinct(detcoho20[which(detcoho20[,7] == c('6a', '5')),], tag.ID, .keep_all = TRUE)

## Fish tagged south of 5
tags5 <- distinct(detcoho20[which(detcoho20[,7] %in% c('6b')),], tag.ID, .keep_all = TRUE)

## Any fish tagged north of 5, and detected south of 5
sof5 <- c('U', 'G', '6', '7')
detsof5 <- tagn5[which(tagn5[,3] %in% sof5),]

## Any fish tagged s of 5, and detected n of 5
nof5 <- c('E', 'T', 'S', 'N', 'I', 'K', '4', '3', '2', '1')
detnof5 <- tags5[which(tags5[,3] %in% nof5),]

## Combine all fish known to migrate past array 5
detpast5 <- rbind(detnof5, detsof5)

## Fish detected by array 5 detected beyond array 5
det5yes <- det5[which(det5[,1] %in% detpast5[,1]),]

## Any fish in 'detpast5' not in 'det5'
det5no <- anti_join(detpast5, det5, by = 'tag.ID')

## Proportion detected
nrow(det5yes)/nrow(detpast5)

# 0.943662
sd(c(rep(1, nrow(det5yes)), rep(0, nrow(detpast5))))/
  sqrt(nrow(det5yes))

## Receiver array 6 ####
## Any fish detected by array 6
det6 <- distinct(detcoho20[which(detcoho20[,3] == '6'),], tag.ID, array, .keep_all = TRUE)

## Any fish tagged north of 6, and detected south of 6
sof6 <- c('G', '7')

detpast6 <- distinct(detcoho20[which(detcoho20[,3] %in% sof6),], tag.ID, .keep_all = TRUE)

## Fish detected by array 6 detected beyond array 6
det6yes <- det6[which(detpast6[,1] %in% det6[,1]),]

det6no <- anti_join(detpast6, det6, by = 'tag.ID')

## Proportion detected
nrow(det6yes)/nrow(detpast6)

# 0.9393939

sd(c(rep(1, nrow(det6yes)), rep(0, nrow(detpast6))))/
  sqrt(nrow(det6yes))

## Array 7 ####

det7 <- distinct(detcoho20[which(detcoho20[,3] == '7'),], tag.ID, array, .keep_all = TRUE)

## Any fish tagged north of 6, and detected south of 6
sof6 <- c('G', '7')

detpast6 <- distinct(detcoho20[which(detcoho20[,3] %in% sof6),], tag.ID, .keep_all = TRUE)

## Fish detected by array 6 detected beyond array 6
det6yes <- det6[which(detpast6[,1] %in% det6[,1]),]

det6no <- anti_join(detpast6, det6, by = 'tag.ID')

## Proportion detected
nrow(det6yes)/nrow(detpast6)

# 0.9393939

sd(c(rep(1, nrow(det6yes)), rep(0, nrow(detpast6))))/
  sqrt(nrow(det6yes))

# Freshwater 2020 ####

# 2021 ####

## Receiver array 2 ####
det1 <- distinct(detcoho21[which(detcoho21[,3] == '1'),], tag.ID, array, .keep_all = TRUE)

## Any fish detected by array 2
det2 <- distinct(detcoho21[which(detcoho21[,3] == '2'),], tag.ID, array, .keep_all = TRUE)

## Any fish tagged south of Point Dexter, and detected north of Point Dexter
nof2.1 <- c('I', 'N', 'K')
detpast2 <- distinct(detcoho21[which(detcoho21[,3] %in% nof2.1),], tag.ID, .keep_all = TRUE)

## Fish detected by array 2 detected beyond array 2
det2yes <- det2[which(detpast2[,1] %in% det2[,1]),]

## Any fish in 'detpast2' not in 'det2'
det2no <- det2[which(!(detpast2[,1] %in% det2[,1])),]

## Proportion detected
nrow(det2yes)/nrow(detpast2)

## There were zero coho detected beyond array 2 in 2021

## Receiver array 3 ####
## Any fish detected by array 3
det3 <- distinct(detcoho21[which(detcoho21[,3] == '3'),], tag.ID, array, .keep_all = TRUE)

## Any fish tagged south of 3, and detected north of 3
nof3.1 <- c('I', 'N', 'K', '2', '1', 'B')
nof3.2 <- c('I', 'N', 'K', '2')
detpast3 <- distinct(detcoho21[which(detcoho21[,3] %in% nof3.2),], tag.ID, .keep_all = TRUE)

## Fish detected by array 3 detected beyond array 3
det3yes <- det3[which(det3[,1] %in% detpast3[,1]),]

det3no <- anti_join(detpast3, det3, by = 'tag.ID')

## Proportion detected
nrow(det3yes)/nrow(detpast3)

# 0.5 if you don't include array 1, 0.25 if you do

sd(c(rep(1, nrow(det3yes)), rep(0, nrow(detpast3))))/
  sqrt(nrow(det3yes))

## Receiver array 4 ####
## Any fish detected by array 4
det4 <- distinct(detcoho21[which(detcoho21[,3] == '4'),], tag.ID, array, .keep_all = TRUE)

## Fish tagged north of 4
tagn4 <- distinct(detcoho21[which(detcoho21[,7] == '5'),], tag.ID, .keep_all = TRUE)

## Fish tagged south of 4
tags4 <- distinct(detcoho21[which(detcoho21[,7] %in% c('6a', '6b')),], tag.ID, .keep_all = TRUE)

## Any fish tagged north of 4, and detected south of 4
sof4 <- c('E', 'U', 'G', '5', '6', '7')
detsof4 <- tagn4[which(tagn4[,3] %in% sof4),]

## Any fish tagged s of 4, and detected n of 4
nof4 <- c('T', 'S', 'N', 'I', 'K', '3', '2', '1')
detnof4 <- tags4[which(tags4[,3] %in% nof4),]

## Combine all fish known to migrate past array 4
detpast4 <- rbind(detnof4, detsof4)

## Fish detected by array 4 detected beyond array 4
det4yes <- det4[which(det4[,1] %in% detpast4[,1]),]

## Any fish in 'detpast4' not in 'det4'
det4no <- anti_join(detpast4, det4, by = 'tag.ID')

## Proportion detected
nrow(det4yes)/nrow(detpast4)

## 0.9375

sd(c(rep(1, nrow(det4yes)), rep(0, nrow(detpast4))))/
  sqrt(nrow(det4yes))

## Receiver array 5 ####
## Any fish detected by array 5
det5 <- distinct(detcoho21[which(detcoho21[,3] == '5'),], tag.ID, array, .keep_all = TRUE)

## Fish tagged north of 5
tagn5 <- distinct(detcoho21[which(detcoho21[,7] == c('6a', '5')),], tag.ID, .keep_all = TRUE)

## Fish tagged south of 5
tags5 <- distinct(detcoho21[which(detcoho21[,7] %in% c('6b')),], tag.ID, .keep_all = TRUE)

## Any fish tagged north of 5, and detected south of 5
sof5 <- c('U', 'G', '6', '7')
detsof5 <- tagn5[which(tagn5[,3] %in% sof5),]

## Any fish tagged s of 5, and detected n of 5
nof5 <- c('E', 'T', 'S', 'N', 'I', 'K', '4', '3', '2', '1')
detnof5 <- tags5[which(tags5[,3] %in% nof5),]

## Combine all fish known to migrate past array 5
detpast5 <- rbind(detnof5, detsof5)

## Fish detected by array 5 detected beyond array 5
det5yes <- det5[which(det5[,1] %in% detpast5[,1]),]

## Any fish in 'detpast5' not in 'det5'
det5no <- anti_join(detpast5, det5, by = 'tag.ID')

## Proportion detected
nrow(det5yes)/nrow(detpast5)

# 0.826087

sd(c(rep(1, nrow(det5yes)), rep(0, nrow(detpast5))))/
  sqrt(nrow(det5yes))

## Receiver array 6 ####
## Any fish detected by array 6
det6 <- distinct(detcoho21[which(detcoho21[,3] == '6'),], tag.ID, array, .keep_all = TRUE)

## Any fish tagged north of 6, and detected south of 6
sof6 <- c('G', '7')

detpast6 <- distinct(detcoho21[which(detcoho21[,3] %in% sof6),], tag.ID, .keep_all = TRUE)

## Fish detected by array 6 detected beyond array 6
det6yes <- det6[which(detpast6[,1] %in% det6[,1]),]

det6no <- anti_join(detpast6, det6, by = 'tag.ID')

## Proportion detected
nrow(det6yes)/nrow(detpast6)

# 0.8333333

sd(c(rep(1, nrow(det6yes)), rep(0, nrow(detpast6))))/
  sqrt(nrow(det6yes))

## Array 7 ####
det7 <- distinct(detcoho21[which(detcoho21[,3] == '7'),], tag.ID, array, .keep_all = TRUE)

# Scrap ####
coho <- read.csv('ch_coho_full.csv')

coho <- left_join(coho, tag[,c(1:2)], by = 'tag.ID')
coho$year <- year(mdy(coho$capture.date))
coho20 <- coho %>% filter(year == '2020')
coho21 <- coho %>% filter(year == '2021')
coho20B <- coho20[which(str_sub(coho20[,5], -2) == 'BB' & 
                          str_sub(coho20[,5], -3) != 'BBB' |
                                  str_sub(coho20[,5], -4) == 'BBBB'),]
coho21B <- coho21[which(str_sub(coho21[,5], -2) == 'BB' & 
                          str_sub(coho21[,5], -3) != 'BBB' |
                          str_sub(coho21[,5], -4) == 'BBBB'),]

nrow(coho20B[which(str_sub(coho20B[,3], -1) == 'E' | coho20B[,4] == 'E'),])
nrow(coho20B[which(str_sub(coho20B[,3], -1) == 'U' | coho20B[,4] == 'U'),])
nrow(coho20B[which(str_sub(coho20B[,3], -1) == 'G' | coho20B[,4] == 'G'),])

nrow(coho21B[which(str_sub(coho21B[,3], -1) == 'E' | coho21B[,4] == 'E'),])
nrow(coho21B[which(str_sub(coho21B[,3], -1) == 'U' | coho21B[,4] == 'U'),])
nrow(coho21B[which(str_sub(coho21B[,3], -1) == 'G' | coho21B[,4] == 'G'),])

coho20 <- coho %>% filter(year == '2020')
coho21 <- coho %>% filter(year == '2021')
coho20X <- coho20[which(str_sub(coho20[,5], -2) == 'XX' & 
                          str_sub(coho20[,5], -3) != 'XXX' |
                          str_sub(coho20[,5], -4) == 'XXXX'),]
coho21X <- coho21[which(str_sub(coho21[,5], -2) == 'XX' & 
                          str_sub(coho21[,5], -3) != 'XXX' |
                          str_sub(coho21[,5], -4) == 'XXXX'),]

nrow(coho20X[which(str_sub(coho20X[,3], -1) == 'E' | coho20X[,4] == 'E'),])
nrow(coho20X[which(str_sub(coho20X[,3], -1) == 'U' | coho20X[,4] == 'U'),])
nrow(coho20X[which(str_sub(coho20X[,3], -1) == 'G' | coho20X[,4] == 'G'),])

nrow(coho21X[which(str_sub(coho21X[,3], -1) == 'E' | coho21X[,4] == 'E'),])
nrow(coho21X[which(str_sub(coho21X[,3], -1) == 'U' | coho21X[,4] == 'U'),])
nrow(coho21X[which(str_sub(coho21X[,3], -1) == 'G' | coho21X[,4] == 'G'),])

