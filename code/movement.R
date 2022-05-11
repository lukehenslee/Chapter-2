library(tidyverse)
library(lubridate)

rec <- read.csv('data/rec_coords.csv')
detsum <- read.csv('data/detsum.csv')
detsum$rec.ID <- paste(detsum$array, detsum$rec, sep = '.')
detsum$year <- year(mdy_hm(detsum$ymd.hms))
rec$rec.ID <- rec$rec

detsum <- left_join(detsum, rec, by = c('rec.ID', 'year'))
detsum <- detsum[,c(1, 5, 10:11)]
detsum <- left_join(detsum, mcode[,c(1, 5)], by = 'tag.ID')

tag <- read.csv('data/tag.csv')
mcode <- read.csv('data/mcode.csv')
mcode <- filter(mcode, species == 'coho')
mcode$tag.ID <- mcode$mcode
mcode <- mcode[,c(33, 19, 10:11, 28)]
names(mcode)[names(mcode) == 'deploy.ymd.hms'] <- 'ymd.hms'
mcode <- mcode[,c(2:3)]

detsum2 <- rbind(detsum, mcode)

detsum3 <- detsum[complete.cases(detsum),]

write.csv(detsum3, 'data/movement.csv')
