

#Unalakleet arrays: 'Blueberry Creek, Point Creek, and Black Point'

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

# Junction Creek and Cape Denbeigh ####

skk.env<- subset(r, Array %in% c('Junction Creek', 'Cape Denbeigh'))
skk.env$mdy <- (as_date(skk.env$date.datetime.))
skk.env.20 <- subset(skk.env, year(date.datetime.) == '2020')
skk.env.21 <- subset(skk.env, year(date.datetime.) == '2021')

## Temp - Denbeigh 20 and 21, Junction 20
skk.den <- subset(skk.env, Array == 'Cape Denbeigh')

den20 <- read.csv('temp/Cape_Denbeigh_2020.csv')
den20$mdy.hms <- mdy_hm(den20$ï..date)
den20$mdy <- as_date(den20$mdy.hms)
den20$hour <- hour(den20$mdy.hms) 

den21 <- read.csv('temp/Cape_Denbeigh_2021.csv')
den21$mdy.hms <- mdy_hm(den21$ï..date)
den21$mdy <- as_date(den21$mdy.hms)
den21$hour <- hour(den21$mdy.hms)

den <- rbind(den21, den20)

den <- den %>% 
  group_by(mdy, hour) %>% 
  slice(1)
skk.den <- merge(skk.den, den, by = c('mdy', 'hour'))
skk.jun.21 <- subset(skk.env.21, Array == 'Junction Creek')
skk.jun.21 <- merge(skk.jun.21, den, by = c('mdy', 'hour'))

skk.jun <- subset(skk.env.20, Array == 'Junction Creek')
jun20 <- read.csv('temp/Junction_Creek_2020.csv')
jun20$mdy.hms <- mdy_hm(jun20$ï..date)
jun20$mdy <- as_date(jun20$mdy.hms)
jun20$hour <- hour(jun20$mdy.hms)

skk.jun <- merge(skk.jun, jun20, by = c('mdy', 'hour'))

skk.env2 <- rbind(skk.jun, skk.jun.21, skk.den)

write.csv(skk.env2, 'skk.env.csv')


## Wind - SKK 2020, SKK 2021
wind.skk.20 <- read.csv('wind/SKK_wind_2020.csv')
wind.skk.21 <- read.csv('wind/SKK_wind_2021.csv')

wind.skk <- rbind(wind.skk.20, wind.skk.21)
wind.skk$ymd <- lubridate::as_date(mdy((wind.skk$Date)))
wind.skk$mdy <- wind.skk$Date
wind.skk$hour <- hour(hm(wind.skk$Time))
wind.skk$md <- abs(as.numeric(wind.skk$Wind.Direction..deg.) - 270)
wind.skk$u <- (as.numeric(wind.skk$Wind.Spd..mph.)*0.44704) * cos(wind.skk$md)
wind.skk$v <- (as.numeric(wind.skk$Wind.Spd..mph.)*0.44704) * sin(wind.skk$md)

skk.env3 <- read.csv('skk.env.csv')

skk.env4 <- merge(skk.env3, wind.skk, by = c('mdy', 'hour'))

## Tides - skk tide predictions
skk.tides <- read.csv('tides/skk_tides.csv')

skk.tides$mdy.hms <- lubridate::ymd_hms(skk.tides$time, tz = 'US/Alaska')
skk.tides$ymd <- as_date(skk.tides$mdy.hms)
skk.tides$hour <- hour(skk.tides$mdy.hms)

skk.tides2<- skk.tides %>% 
  group_by(ymd, hour) %>% 
  slice(1)

skk.env5 <- merge(skk.env4, skk.tides2, by = c('ymd', 'hour'))

write.csv(skk.env5, 'skk.env2.csv')


# Koyuk arrays - Point Dexter, Bald Head
koy.env <- subset(r, Array %in% c('Bald Head', 'Point Dexter'))
koy.env$mdy <- as_date(koy.env$date.datetime.)

## Temp- Point Dexter 2021
koy.temp <- read.csv('temp/Point_Dexter_2021.csv')

koy.temp2 <- rbind(koy.temp, den20)

den20 <- read.csv('temp/Cape_Denbeigh_2020.csv')

koy.temp2$mdy.hms <- mdy_hm(koy.temp2$ï..date)
koy.temp2$mdy <- as_date(koy.temp2$mdy.hms)
koy.temp2$hour <- hour(koy.temp2$mdy.hms) 
koy.temp2 <- koy.temp2[-c(1:2),]
koy.env2 <- merge(koy.env, koy.temp2, by = c('mdy', 'hour'))

## Wind- Koyuk wind 2020 and 2021
kwind20 <- read.csv('wind/koyuk_wind_2020.csv')
kwind21 <- read.csv('wind/koyuk_wind_2021.csv')

kwind <- rbind(kwind20, kwind21)
kwind$mdy.hms <- ymd_hms(kwind$time, tz = 'US/Alaska')
kwind$mdy <- as_date(kwind$mdy.hms)
kwind$hour <- hour(kwind$mdy.hms)

kwind$md <- abs(as.numeric(kwind$wind_from_direction) - 270)
kwind$u <- as.numeric(kwind$wind_speed) * cos(kwind$md)
kwind$v <- as.numeric(kwind$wind_speed) * sin(kwind$md)

koy.env3 <- merge(koy.env2, kwind, by =c('mdy', 'hour'))

## Tides- skk tides
koy.env4 <- merge(koy.env3, skk.tides2, by = c('mdy', 'hour'))

write.csv(koy.env4, 'koy.env.csv')


unk <- read.csv('unk.env.csv')
skk <- read.csv('skk.env2.csv')
koy <- read.csv('koy.env.csv')

r.env <- rbind(unk, skk, koy)

write.csv(r.env, 'r_env.csv')
