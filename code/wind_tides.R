
# Positive u wind is from the west
# Positive v wind is from the south

wind <- read.csv('data/unk_wind.csv')
r20 <- read.csv('data/r20_array.csv')
r21 <- read.csv('data/r21_array.csv')

r <- rbind(r20, r21)
r <- r[,c(2:8)]

wind <- wind[c(2:nrow(wind)), c(1:3)]
wind$mdy.hms <- lubridate::ymd_hms(wind$time, tz = 'US/Alaska')
wind$md <- abs(as.numeric(wind$wind_from_direction) - 270)
wind$u <- as.numeric(wind$wind_speed) * cos(wind$md)
wind$v <- as.numeric(wind$wind_speed) * sin(wind$md)

unk.wind <- subset(r, Array %in% c('Blueberry Creek', 'Point Creek', 'Black Point'))

unk.wind$mdy <- ymd(unk.wind$date.datetime.)
wind$mdy <- as_date(wind$mdy.hms)
wind$hour <- hour(wind$mdy.hms)

unk.wind <- merge(unk.wind, wind, by = c('mdy', 'hour'))
unk.wind <- unk.wind[,c(1:11, 18:19)]


unk.tides <- read.csv('data/env/tides/unk_tides.csv')
unk.tides$mdy.hms <- ymd_hms(unk.tides$time)
unk.tides$mdy <- as_date(unk.tides$mdy.hms)
unk.tides$hour <- hour(unk.tides$mdy.hms)

unk.tides2 <- unk.tides %>% 
  group_by(mdy,hour) %>% 
  slice(1)

unk.env <- merge(unk.wind, unk.tides2, by = c('mdy', 'hour'))

unk.temp <- read.csv('data/env/temp/Blueberry_Creek_2020.csv')
point.temp <- read.csv('data/env/temp/Point_Creek_2020.csv')
blk.point.20 <- read.csv('data/env/temp/Black_Point_2020.csv')
blk.point.21 <- read.csv('data/env/temp/Black_Point_2021.csv')

point.20 <- subset(unk.env, Array == 'Point Creek' & year(mdy) == '2020')
point.temp$mdy <- mdy_hm(point.temp$ï..date)
point.temp$hour <- hour(point.temp$mdy)

point.20 <- merge(point.20, point.temp, by = c('mdy', 'hour'))

unk.20 <- subset(unk.env, Array == 'Blueberry Creek' & year(mdy) == '2020')
unk.temp$mdy <- mdy_hm(unk.temp$date)
unk.temp$hour <- hour(unk.temp$mdy)

unk.20 <- merge(unk.20, unk.temp, by = c('mdy', 'hour'))

blk.point.20$mdy <- mdy_hm(blk.point.20$ï..date)
blk.point.20$hour <- hour(blk.point.20$mdy)


blk.point.21$mdy <- mdy_hm(blk.point.21$ï..date)
blk.point.21$hour <- hour(blk.point.21$mdy)

blk.point <- rbind(blk.point.20[,c(1:2, 6:7)], blk.point.21[,c(1:2, 7:8)])

blkpnt <- subset(unk.env, Array == 'Black Point')
blkpnt <- merge(blkpnt, blk.point, by = c('mdy', 'hour'))

unk.21 <- subset(unk.env, Array %in% c('Blueberry Creek', 'Point Creek') & year(mdy) == '2021')

unk.21 <- merge(unk.21, blk.point.21, by = c('mdy', 'hour'))

write.csv(unk.20, 'unk_20.csv')
write.csv(point.20, 'point_20.csv')
write.csv(unk.21, 'unk_21.csv')
write.csv(point.21, 'point_21.csv')
write.csv(blkpnt, 'blackpnt.csv')

unk.20 <- read.csv('unk_20.csv')
point.20 <- read.csv('point_20.csv')
unk.21 <- read.csv('unk_21.csv')
blkpnt <- read.csv('blackpnt.csv')
unk <- rbind(unk.20, point.20, unk.21, blkpnt)

write.csv(unk, 'unk.env.csv')
