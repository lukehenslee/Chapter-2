
# Positive u wind is from the west
# Positive v wind is from the south

wind <- read.csv('data/unk_wind.csv')
wind <- wind[c(2:nrow(wind)), c(1:3)]
wind$mdy.hms <- lubridate::ymd_hms(wind$time, tz = 'US/Alaska')
wind$md <- abs(as.numeric(wind$wind_from_direction) - 270)
wind$u <- as.numeric(wind$wind_speed) * cos(wind$md)
wind$v <- as.numeric(wind$wind_speed) * sin(wind$md)
