
# Positive u wind is from the west
# Positive v wind is from the south

wind <- read.csv('data/unk_wind.csv')
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
