#==================================================================================================
# Preparing data for GIS animation
# 9/12/2022
# Luke Henslee, College of Fisheries and Ocean Sciences, UAF
#
#==================================================================================================
#NOTES: This script combines capture data with detection data for animation with 
# GIS software.

#==================================================================================================

# Load packages
library(tidyverse)
library(lubridate)
library(zoo)
library(data.table)


# Set working directory ####
setwd("H:/git_repo/Chapter-2")
data <- file.path(getwd(), "data")


## Load data ####
## Detection data
str <- read.csv(file=file.path(data, "detsum_colClasses.csv"))
detsum <- read.csv(file=file.path(data, "detsum.csv"), 
                   colClasses = paste(str[1,]))


## Capture data
str <- read.csv(file=file.path(data, "mcode_colClasses.csv"))
tag <- read.csv(file=file.path(data, "mcode.csv"))

## Recapture data
recap <- read.csv(file=file.path(data, "recaptures.csv"))

## Receiver coordinates 
rec <- read.csv(file=file.path(data, "rec_coords.csv"))

## Data organization ####
## We want a dataframe with each detection by tag ID

## We need to join detection data and receiver location
## Note that 'detections' include capture encounters, receiver detections, and
## physical recaptures 

## Because receivers were placed differently among years, the data must be 
## joined by receiver of detection AND year of detection

## Format detection data
### Create rec ID by combining 'array' and 'rec'
detsum$rec.ID <- paste(detsum$array, detsum$rec, sep = '.')
### Format 'year'
detsum$year <- year(mdy_hm(detsum$ymd.hms))

### Join with 'rec'
detsum <- left_join(detsum, rec, by = c('rec.ID', 'year'))


## Format recap data
recap$rec.ID <- paste(recap$array, recap$rec, sep = '.')
recap$year <- year(mdy_hm(recap$ymd.hms))

### join with 'rec'
recap <- left_join(recap, rec, by = c('rec.ID', 'year'))

### Need to remove recaps that were already detected inriver
detinriver <- detsum %>%
  group_by(tag.ID) %>% 
  arrange(mdy_hm(ymd.hms)) %>% 
  slice_tail()

detinriver <- detinriver[which(detinriver$array %in% LETTERS),]

recapdet <- recap[which(recap$tag.ID %in% detinriver$tag.ID),]

recapnodet <- anti_join(recap, recapdet, by = 'tag.ID')

### Retain tag ID, timestamp, and coordinates as well as grouping variable '
### year'
### Bind detections with recaps but only keep recaps that were not detected inriver
det <- rbind(detsum[,c(1,5,3,9:10)], recapnodet[,c(1:3, 7:8)])

### Join with 'tag' to get grouping variables 'sex','species', and 'stock'
det <- left_join(det, tag[, c(1, 5, 8, 28)], by = 'tag.ID')

### Now need to bind with capture data
cap <- tag[,c(1, 19, 10, 11, 5, 8, 28)]
### Need an 'array' col to rbind, we'll call it 'C' for capture
cap$array <- 'C'

### Only join with tags detected after capture
det <- rbind(det, subset(cap, tag.ID %in% det$tag.ID))

### We now have a row for each detection of a tagged fish, including capture
### encounter. Make sure all columns are in the format we want
str(det)
det$ymd.hms <- mdy_hm(det$ymd.hms)

## Data filtering ####
### We want to make animations of coho movement by stock, so we will remove 
### other species and any fish with no stock assignment

detstock <- det[complete.cases(det),]
detstockcoho <- filter(detstock, species == 'coho')

### Arrange chronologically 
detstockcoho <- detstockcoho %>%
  arrange(tag.ID, ymd.hms)

detstockcoho$stock <- ifelse(detstockcoho$stock == 6, 'Unalakleet', 
                               ifelse(detstockcoho$stock == 5, 'Shaktoolik', 'Transitory')) 

### Make a 'unix' column 
detstockcoho$unix <- as.numeric(detstockcoho$ymd.hms)

detstockcoho <- detstockcoho[which(!(detstockcoho$tag.ID %in% cohomultriv$tag.ID)),]
### Find time at large
at.large <- detstockcoho %>% 
  group_by(tag.ID) %>% 
  summarize(time.at.large = last(unix)-first(unix))

### Retain final detection array
detstockcoholast <- detstockcoho %>% 
  group_by(tag.ID) %>% 
  slice_tail()

checkitout <- detstockcoholast[which(detstockcoholast$array %in% c('1', '2', '5')),]

at.large <- left_join(at.large, detstockcoholast[,c(1,3)], by = 'tag.ID')

## Now we need to know the distance upriver of each receiver
##
##Y <- 9.857 * 10^3/ 0.540808
##I <- 9.19 * 10^3/ 0.540808
##N <- 5.509 * 10^3/ 0.540808
##S <- 9.288 * 10^3/ 0.540808
##T <- 10.747 * 10^3/ 0.540808
##E <- 2.893 * 10^3/ 0.540808
##U <- 6.409 * 10^3/ 0.540808
##G <- 1.303 * 10^3/ 0.540808

inriver <- data.frame(array = c('Y', 'I', 'N', 'S', 'T', 'E', 'U', 'G'), 
                      seconds = c((9.857 * 10^3)/ 0.540808, 9.19 * 10^3/ 0.540808, 
                            5.509 * 10^3/ 0.540808, 9.288 * 10^3/ 0.540808,
                            10.747 * 10^3/ 0.540808, 2.893 * 10^3/ 0.540808,
                            6.409 * 10^3/ 0.540808, 1.303 * 10^3/ 0.540808))

inriver$minutes15 <- inriver$seconds/900

inriver$hours <- inriver$seconds/3600

## combine 

at.large <- left_join(at.large, inriver, by = 'array')
at.large$seconds[is.na(at.large$seconds)] <- 0
at.large$salt.time <- at.large$time.at.large - at.large$seconds

### Make time at large col
at.large$at.large <- seconds_to_period(at.large$time.at.large)
summary(at.large$at.large)
which.max(at.large$time.at.large)

hist(at.large$time.at.large)

### Combine with tag data

at.large <- left_join(at.large, tag[,c(1,5,28)], by = 'tag.ID')

min(at.large$salt.time)/86400
max(at.large$salt.time)/86400

at.large$stock <- ifelse(at.large$stock == 6, 'Unalakleet', 
                             ifelse(at.large$stock == 5, 'Shaktoolik', 'Transitory')) 

at.large.stats <- get_summary_stats(salt.time, type = 'mean_sd', data = at.large)

at.large.stats$salt.time2 <- at.large.stats$mean/86400
at.large.stats$salt.time.sd2 <- at.large.stats$sd/86400

at.large.stats.stock <- at.large %>% 
  group_by(stock) %>% 
  get_summary_stats(salt.time, type = 'mean_sd')

at.large.stats.stock$salt.time <- seconds_to_period(at.large.stats.stock$mean)
at.large.stats.stock$salt.time.sd <- seconds_to_period(at.large.stats.stock$sd)

at.large.stats.stock$salt.time2 <- at.large.stats.stock$mean/86400
at.large.stats.stock$salt.time.sd2 <- at.large.stats.stock$sd/86400

at.large %>% 
  anova_test(salt.time ~ stock)


############
### Arrange chronologically 
det <- det %>%
  arrange(tag.ID, ymd.hms)

### Make a 'unix' column 
det$unix <- as.numeric(det$ymd.hms)

### Just fate 3
fate3 <- anti_join(det, detstockcoho, 'tag.ID')
fate3 <- fate3[which(fate3$species == 'coho'),]
## remove fish that went inrivers
fate3 <- fate3[which(!(fate3$tag.ID %in% c(3735, 150, 750, 2695))),]

### Find time at large
at.large.fate3 <- fate3 %>% 
  group_by(tag.ID) %>% 
  summarize(time.at.large = last(unix)-first(unix))

### Make time at large col
at.large.fate3$at.large.fate3 <- seconds_to_period(at.large.fate3$time.at.large)
summary(at.large.fate3$at.large.fate3)
which.max(at.large.fate3$time.at.large)

hist(at.large.fate3$time.at.large)
mean(at.large.fate3$time.at.large)/86400
sd(at.large.fate3$time.at.large)/86400

max(at.large.fate3$time.at.large)/86400
which.max(at.large.fate3$time.at.large)
min(at.large.fate3[-56,]$time.at.large)/86400
which.min(at.large.fate3[-56,]$time.at.large)

# Can we calculate the amount of time in A and B? ####

### Many have multiple inriver detections, we want to subtract this from 
### salttime 

detsum2 <- detstockcoho %>% 
  group_by(tag.ID) %>% 
  filter(array %in% LETTERS)  


detsum2 <- detsum2 %>% 
  group_by(tag.ID) %>% 
  filter(n() > 1)

cohomultriv <- detstockcoho[which(detstockcoho$tag.ID %in% detsum2$tag.ID),]

cohostock <- tag[which(tag$species == 'coho' & is.na(tag$stock) == F),]

cohostockA <- cohostock[which(cohostock$capture.loc == '5'),]
cohostockA5 <- cohostockA[which(cohostockA$stock == '5'), ]

# The fish tagged in A that were stock 5 that went right to a river
A5easy <- cohostockA5[which(cohostockA5$det.hist %in% c('S', 'T') | cohostockA5$recap %in% c('S', 'T')),]
at.largeA5easy <- at.large[which(at.large$tag.ID %in% A5easy$tag.ID),]


A5hard <- anti_join(cohostockA5, A5easy, by = 'tag.ID')
cohostockA6 <- cohostockA[which(cohostockA$stock == '6'), ]
A6easy <- cohostockA6[which(cohostockA6$det.hist %like% '4'),]
A6hard <- anti_join(cohostockA6, A6easy, by = 'tag.ID')
cohostockAZ <- cohostockA[which(cohostockA$stock %in% c('4', 'S', 'N')), ]

##

cohostockB <- cohostock[which(cohostock$capture.loc %in% c('6a', '6b')),]

# Summary stats and ANOVA ####
time <- read.csv('data/time_available_year.csv')

time <- left_join(time, tag[,c(1,2,5)], by = 'tag.ID')

# ANOVA
## We log tx the response
summary(aov(log(days) ~ Stock * Subdistrict + as.factor(stat.week), time))

## year is not significant
summary(aov(log(days) ~ Stock * Subdistrict, time))

## Stat week
summary(aov(log(days) ~ as.factor(stat.week), time))

## Sex
summary(aov(log(days) ~ sex, time))

## Overall summary
time_stats <- time %>% 
  get_summary_stats(days, type = 'mean_sd')

## summary by stock
Stock_time_stats <- time %>% 
  group_by(Stock) %>% 
  get_summary_stats(days, type = 'mean_sd')

## summary by sd
sd_time_stats <- time %>% 
  group_by(Subdistrict) %>% 
  get_summary_stats(days, type = 'mean_sd')

## summary by stock and sd
sd_Stock_time_stats <- time %>% 
  group_by(Stock, Subdistrict) %>% 
  get_summary_stats(days, type = 'mean_sd')

## summary by stock and sd
stat.week_time_stats <- time %>% 
  group_by(stat.week) %>% 
  get_summary_stats(days, type = 'mean_sd')


## visualize 
ggplot(data = time, aes(x = Subdistrict, y = log(days))) +
  geom_boxplot(aes())

ggplot(data = time, aes(x = Stock, y = days)) +
  geom_boxplot(aes())

ggplot(data = time, aes(x = as.factor(stat.week), y = days)) +
  geom_boxplot(aes(fill = Stock))


ggplot(data = time, aes(x = Subdistrict, y = days)) +
  geom_boxplot(aes(fill = Stock))

# LM with log tx ####
loglm <- lm(log(minutes) ~ Stock, time)
hist(loglm$residuals)
plot(loglm)
lm <- lm(minutes ~ Stock, time)
hist(lm$residuals)
plot(lm)
# GLM ###

summary(glm(minutes ~ as.factor(stat.week),
            data = time, family = Gamma))

# Det by array ####
detsum$doy <- yday(lubridate::mdy_hm(detsum$ymd.hms))

detcoho <- detsum[which(detsum$tag.ID %in% coho$tag.ID), ]

detbyday <- detcoho %>% 
  group_by(array, doy, tag.ID) %>% 
  count()
detbyday$tag.ID <- factor(detbyday$tag.ID)
detbyday$array <- factor(detbyday$array)

detbydaysexriver <- detbydaysex[which(detbydaysex$array %in% LETTERS), ]
detbydaysexriver$array <- factor(detbydaysexriver$array, levels = c('Y', 'I', 'N',
                                                              'S', 'T', 'E',
                                                              'U', 'G'))

detbydaysexriver <- detbydaysexriver %>% 
  group_by(array, doy, sex) %>% 
  count()

detbydaysex <- left_join(detbyday, coho[,c(1,5)], by = 'tag.ID')

detbydaysex <- detbydaysex %>% 
  group_by(array, doy, sex) %>% 
  count

ggplot(subset(detbyday, !(array %in% LETTERS)) , aes(x = doy, y = n)) +
  geom_col() +
  facet_wrap(~array)

hist(rec$dist)

detrec <- left_join(detsum, rec[,c(1:2, 5)], by = c('rec.ID', 'year'))
detrec <- detrec[which(detrec$tag.ID %in% coho$tag.ID), ]
detrecsex <- left_join(detrec, tag[,c(1,5)], by = 'tag.ID')
detrecsex <- detrecsex[which(detrecsex$tag.ID %in% coho$tag.ID), ]
detrec$tag.ID <- factor(detrec$tag.ID)
detrecstock <- left_join(detrec, unique(detstockcoho[,c(1,8)]), by = 'tag.ID')
detrecstock <- detrecstock[which(detrecstock$tag.ID %in% coho$tag.ID), ]

detrec <- left_join(detrec, unique(coho[,c(1,5)]), by = 'tag.ID')
detstockcoho$tag.ID <- factor(detstockcoho$tag.ID)
detrec <- left_join(detrec, unique(detstockcoho[,c(1,6,8)]), by = 'tag.ID')

# Figures ####
# Load packages 
library(tidyverse)
library(RColorBrewer)
library(extrafont)
loadfonts(device = 'win')
library(showtext)
windows()
font_add('Arial', 'arial.ttf')
showtext_auto()
showtext_opts(dpi = 600)


hist(time$days, freq = F)
lines(density(time$days))

daybysd<- ggplot(time, aes(x = Subdistrict, y = days)) +
  geom_boxplot() +
  xlab("Subdistrict") +
  ylab("Days at large") +
  #set the limits and tick breaks for the y-axis
  scale_y_continuous(limits = c(0,20), breaks = seq(0, 20, by = 5), expand = c(0,0)) +
  scale_x_discrete() +
  #adjust the order of the legend, make new labels, and select the symbol colors
  #makes the figure background white without grid lines
  theme_classic() +
  theme (axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
         axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
         #set the font type
         text = element_text(family = "Arial"),
         #margin for the plot
         plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
         strip.background = element_blank(),
         strip.text = element_text(size = 14),
         #set size of the tick marks for y-axis
         axis.ticks.y = element_line(size = 0.5),
         #set size of the tick marks for x-axis
         axis.ticks.x = element_blank(),
         #adjust length of the tick marks
         axis.ticks.length = unit(0.2,"cm"),
         #set size and location of the tick labels for the y axis
         axis.text.y = element_text(colour = "black", size = 12, angle = 0, vjust = 0.5, hjust = 1,
                                    margin = margin(t = 0, r = 5, b = 0, l = 0)),
         #set size and location of the tick labels for the x axis
         axis.text.x = element_text(colour = "black", size = 12, angle = 0, vjust = 0, hjust = 0.5,
                                    margin = margin(t = 5, r = 0, b = 0, l = 0)),
         #set the axis size, color, and end shape
         axis.line = element_line(colour = "black", size = 0.5, lineend = "square"),
         panel.spacing.y = unit(1, 'lines'))

daybysd
ggsave(daybysd, file = "figs/days_by_sd2.png", width = 19, height = 15, units = "cm", dpi = 600)


daybystock<- ggplot(time, aes(x = Stock, y = days)) +
  geom_boxplot() +
  xlab("Stock") +
  ylab("Days at large") +
  #set the limits and tick breaks for the y-axis
  scale_y_continuous(limits = c(0,20), breaks = seq(0, 20, by = 5), expand = c(0,0)) +
  scale_x_discrete() +
  #adjust the order of the legend, make new labels, and select the symbol colors
  #makes the figure background white without grid lines
  theme_classic() +
  theme (axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
         axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
         #set the font type
         text = element_text(family = "Arial"),
         #margin for the plot
         plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
         strip.background = element_blank(),
         strip.text = element_text(size = 14),
         #set size of the tick marks for y-axis
         axis.ticks.y = element_line(size = 0.5),
         #set size of the tick marks for x-axis
         axis.ticks.x = element_blank(),
         #adjust length of the tick marks
         axis.ticks.length = unit(0.2,"cm"),
         #set size and location of the tick labels for the y axis
         axis.text.y = element_text(colour = "black", size = 12, angle = 0, vjust = 0.5, hjust = 1,
                                    margin = margin(t = 0, r = 5, b = 0, l = 0)),
         #set size and location of the tick labels for the x axis
         axis.text.x = element_text(colour = "black", size = 12, angle = 0, vjust = 0, hjust = 0.5,
                                    margin = margin(t = 5, r = 0, b = 0, l = 0)),
         #set the axis size, color, and end shape
         axis.line = element_line(colour = "black", size = 0.5, lineend = "square"),
         panel.spacing.y = unit(1, 'lines'))

daybystock
ggsave(daybystock, file = "figs/days_by_stock.png", width = 19, height = 15, units = "cm", dpi = 600)

time$Stock <- factor(time$Stock, levels = c('Shaktoolik', 'Unalakleet', 'Transitory'))
daybystocksd<- ggplot(time, aes(x = Subdistrict, y = days, fill = Stock)) +
  geom_boxplot() +
  xlab("Subdistrict") +
  ylab("Days at large") +
  #set the limits and tick breaks for the y-axis
  scale_y_continuous(limits = c(0,20), breaks = seq(0, 20, by = 5), expand = c(0,0)) +
  scale_x_discrete() +
  #adjust the order of the legend, make new labels, and select the symbol colors
  #makes the figure background white without grid lines
  theme_classic() +
  theme (axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
         axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
         #set the font type
         text = element_text(family = "Arial"),
         legend.position = c(.20,.85),
         legend.background = element_blank(),
         legend.box.background = element_rect(colour = "black"),
         #margin for the plot
         plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
         strip.background = element_blank(),
         strip.text = element_text(size = 14),
         #set size of the tick marks for y-axis
         axis.ticks.y = element_line(size = 0.5),
         #set size of the tick marks for x-axis
         axis.ticks.x = element_blank(),
         #adjust length of the tick marks
         axis.ticks.length = unit(0.2,"cm"),
         #set size and location of the tick labels for the y axis
         axis.text.y = element_text(colour = "black", size = 12, angle = 0, vjust = 0.5, hjust = 1,
                                    margin = margin(t = 0, r = 5, b = 0, l = 0)),
         #set size and location of the tick labels for the x axis
         axis.text.x = element_text(colour = "black", size = 12, angle = 0, vjust = 0, hjust = 0.5,
                                    margin = margin(t = 5, r = 0, b = 0, l = 0)),
         #set the axis size, color, and end shape
         axis.line = element_line(colour = "black", size = 0.5, lineend = "square"),
         panel.spacing.y = unit(1, 'lines'))

daybystocksd
ggsave(daybystocksd, file = "figs/days_by_stock_sd.png", width = 19, height = 15, units = "cm", dpi = 600)


daybyweek<- ggplot(time, aes(x = as.factor(stat.week), y = days)) +
  geom_boxplot() +
  xlab("Statistical week of capture") +
  ylab("Days at large") +
  #set the limits and tick breaks for the y-axis
  scale_y_continuous(limits = c(0,20), breaks = seq(0, 20, by = 5), expand = c(0,0)) +
  scale_x_discrete() +
  #adjust the order of the legend, make new labels, and select the symbol colors
  #makes the figure background white without grid lines
  theme_classic() +
  theme (axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
         axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
         #set the font type
         text = element_text(family = "Arial"),
         legend.position = c(.20,.85),
         legend.background = element_blank(),
         legend.box.background = element_rect(colour = "black"),
         #margin for the plot
         plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
         strip.background = element_blank(),
         strip.text = element_text(size = 14),
         #set size of the tick marks for y-axis
         axis.ticks.y = element_line(size = 0.5),
         #set size of the tick marks for x-axis
         axis.ticks.x = element_blank(),
         #adjust length of the tick marks
         axis.ticks.length = unit(0.2,"cm"),
         #set size and location of the tick labels for the y axis
         axis.text.y = element_text(colour = "black", size = 12, angle = 0, vjust = 0.5, hjust = 1,
                                    margin = margin(t = 0, r = 5, b = 0, l = 0)),
         #set size and location of the tick labels for the x axis
         axis.text.x = element_text(colour = "black", size = 12, angle = 0, vjust = 0, hjust = 0.5,
                                    margin = margin(t = 5, r = 0, b = 0, l = 0)),
         #set the axis size, color, and end shape
         axis.line = element_line(colour = "black", size = 0.5, lineend = "square"),
         panel.spacing.y = unit(1, 'lines'))

daybyweek
ggsave(daybyweek, file = "figs/days_by_week.png", width = 19, height = 15, units = "cm", dpi = 600)

## Det histogram ####

array.labs <- c('Bald Head Array', 'Point Dexter array', 'Cape Denbeigh array',
                'Junction Creek array', 'Blueberry Creek array', 'Point Creek array',
                'Black Point array')
names(array.labs) <- c(seq(1:7))

detbydoysex2 <- ggplot(subset(detbydaysex, !(array %in% LETTERS)) , aes(x = doy, y = n)) +
  geom_col(aes()) +
  facet_wrap(~array, scales = 'free',
             nrow = 7,
             labeller = labeller(array = array.labs)) +
  xlab("Day of year of detection") +
  ylab("Number of detections") +
  #set the limits and tick breaks for the y-axis
  scale_y_continuous(limits = c(0,50), breaks = seq(0, 50, by = 10), expand = c(0,0)) +
  scale_x_continuous(limits = c(213,250), breaks = seq(215, 245, by = 10),expand = c(0,0)) +
  theme_classic() +
  theme (axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
         axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
         #set the font type
         text = element_text(family = "Arial"),
         legend.position = c(.20,.85),
         legend.background = element_blank(),
         legend.box.background = element_rect(colour = "black"),
         #margin for the plot
         plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
         strip.background = element_blank(),
         strip.text = element_text(size = 14),
         #set size of the tick marks for y-axis
         axis.ticks.y = element_line(size = 0.5),
         #set size of the tick marks for x-axis
         axis.ticks.x = element_blank(),
         #adjust length of the tick marks
         axis.ticks.length = unit(0.2,"cm"),
         #set size and location of the tick labels for the y axis
         axis.text.y = element_text(colour = "black", size = 12, angle = 0, vjust = 0.5, hjust = 1,
                                    margin = margin(t = 0, r = 5, b = 0, l = 0)),
         #set size and location of the tick labels for the x axis
         axis.text.x = element_text(colour = "black", size = 12, angle = 0, vjust = 0, hjust = 0.5,
                                    margin = margin(t = 5, r = 0, b = 0, l = 0)),
         #set the axis size, color, and end shape
         axis.line = element_line(colour = "black", size = 0.5, lineend = "square"),
         panel.spacing.y = unit(1, 'lines'))

detbydoysex2



ggsave(detbydoysex2, file = "figs/dets_by_doy_2.png", width = 24, height = 30, units = "cm", dpi = 600)

### by stock ####
detstockcohodoy <- detstockcoho
detstockcohodoy$doy <- yday(detstockcohodoy$ymd.hms)

detstockcohodoy <- detstockcohodoy %>% 
  group_by(array, doy, stock) %>% 
  count()

detstockcohodoy$Stock <- factor(detstockcohodoy$stock, levels = c('Shaktoolik',
                                                                  'Unalakleet',
                                                                  'Transitory'))

detbydoystock <- ggplot(subset(detstockcohodoy, !(array %in% LETTERS)) , aes(x = doy, y = n, fill = Stock)) +
  geom_col() +
  facet_wrap(~array, scales = 'free',
             nrow = 7,
             labeller = labeller(array = array.labs)) +
  xlab("Day of year of detection") +
  ylab("Number of detections") +
  #set the limits and tick breaks for the y-axis
  scale_y_continuous(limits = c(0,50), breaks = seq(0, 50, by = 10), expand = c(0,0)) +
  scale_x_continuous(limits = c(213,250), breaks = seq(215, 245, by = 10),expand = c(0,0)) +
  theme_classic() +
  theme (axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
         axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
         #set the font type
         text = element_text(family = "Arial"),
         legend.position = c(.20,.85),
         legend.background = element_blank(),
         legend.box.background = element_rect(colour = "black"),
         #margin for the plot
         plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
         strip.background = element_blank(),
         strip.text = element_text(size = 14),
         #set size of the tick marks for y-axis
         axis.ticks.y = element_line(size = 0.5),
         #set size of the tick marks for x-axis
         axis.ticks.x = element_blank(),
         #adjust length of the tick marks
         axis.ticks.length = unit(0.2,"cm"),
         #set size and location of the tick labels for the y axis
         axis.text.y = element_text(colour = "black", size = 12, angle = 0, vjust = 0.5, hjust = 1,
                                    margin = margin(t = 0, r = 5, b = 0, l = 0)),
         #set size and location of the tick labels for the x axis
         axis.text.x = element_text(colour = "black", size = 12, angle = 0, vjust = 0, hjust = 0.5,
                                    margin = margin(t = 5, r = 0, b = 0, l = 0)),
         #set the axis size, color, and end shape
         axis.line = element_line(colour = "black", size = 0.5, lineend = "square"),
         panel.spacing.y = unit(1, 'lines'))

detbydoystock



ggsave(detbydoystock, file = "figs/dets_by_doy_stock.png", width = 19, height = 30, units = "cm", dpi = 600)


### River ####
river.labs <- c('Egavik', 'Golsovia', 'Inglutalik', 'Ungalik', 'Shatkoolik', 'Tagoomenik',
                'Unalakleet', 'Koyuk')
names(river.labs) <- c('E', 'G', 'I', 'N', 'S', 'T', 'U', 'Y')



detbydoyriversex2 <- ggplot(detbydaysexriver, aes(x = doy, y = n, fill = sex)) +
  geom_col() +
  facet_wrap(~array, scales = 'free',
             nrow = 4, labeller = labeller(array = river.labs)) +
  xlab("Day of year of detection") +
  ylab("Number of detections") +
  #set the limits and tick breaks for the y-axis
  scale_y_continuous(limits = c(0,30), breaks = seq(0, 30, by = 10), expand = c(0,0)) +
  scale_x_continuous(limits = c(213,250), breaks = seq(215, 245, by = 10),expand = c(0,0)) +
  theme_classic() +
  theme (axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
         axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
         #set the font type
         text = element_text(family = "Arial"),
         legend.position = c(.20,.65),
         legend.background = element_blank(),
         legend.box.background = element_rect(colour = "black"),
         #margin for the plot
         plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
         strip.background = element_blank(),
         strip.text = element_text(size = 14),
         #set size of the tick marks for y-axis
         axis.ticks.y = element_line(size = 0.5),
         #set size of the tick marks for x-axis
         axis.ticks.x = element_blank(),
         #adjust length of the tick marks
         axis.ticks.length = unit(0.2,"cm"),
         #set size and location of the tick labels for the y axis
         axis.text.y = element_text(colour = "black", size = 12, angle = 0, vjust = 0.5, hjust = 1,
                                    margin = margin(t = 0, r = 5, b = 0, l = 0)),
         #set size and location of the tick labels for the x axis
         axis.text.x = element_text(colour = "black", size = 12, angle = 0, vjust = 0, hjust = 0.5,
                                    margin = margin(t = 5, r = 0, b = 0, l = 0)),
         #set the axis size, color, and end shape
         axis.line = element_line(colour = "black", size = 0.5, lineend = "square"),
         panel.spacing.y = unit(1, 'lines'))

detbydoyriversex2

ggsave(detbydoyriversex2, file = "figs/dets_by_doy_river_sex2.png", width = 19, height = 30, units = "cm", dpi = 600)


detbydoysex <- ggplot(subset(detbydaysex, !(array %in% LETTERS)) , aes(x = doy, y = n, fill = sex)) +
  geom_col(position = 'dodge') +
  facet_wrap(~array, scales = 'free',
             nrow = 7,
             labeller = labeller(array = array.labs)) +
  xlab("Day of year of detection") +
  ylab("Number of detections") +
  #set the limits and tick breaks for the y-axis
  scale_y_continuous(limits = c(0,30), breaks = seq(0, 30, by = 10), expand = c(0,0)) +
  scale_x_continuous(limits = c(213,250), breaks = seq(215, 245, by = 10),expand = c(0,0)) +
  theme_classic() +
  theme (axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
         axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
         #set the font type
         text = element_text(family = "Arial"),
         legend.position = c(.20,.85),
         legend.background = element_blank(),
         legend.box.background = element_rect(colour = "black"),
         #margin for the plot
         plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
         strip.background = element_blank(),
         strip.text = element_text(size = 14),
         #set size of the tick marks for y-axis
         axis.ticks.y = element_line(size = 0.5),
         #set size of the tick marks for x-axis
         axis.ticks.x = element_blank(),
         #adjust length of the tick marks
         axis.ticks.length = unit(0.2,"cm"),
         #set size and location of the tick labels for the y axis
         axis.text.y = element_text(colour = "black", size = 12, angle = 0, vjust = 0.5, hjust = 1,
                                    margin = margin(t = 0, r = 5, b = 0, l = 0)),
         #set size and location of the tick labels for the x axis
         axis.text.x = element_text(colour = "black", size = 12, angle = 0, vjust = 0, hjust = 0.5,
                                    margin = margin(t = 5, r = 0, b = 0, l = 0)),
         #set the axis size, color, and end shape
         axis.line = element_line(colour = "black", size = 0.5, lineend = "square"),
         panel.spacing.y = unit(1, 'lines'))

detbydoysex

ggsave(detbydoysex, file = "figs/dets_by_doy_sex.png", width = 19, height = 30, units = "cm", dpi = 600)

detbydoyriversex <- ggplot(detbydaysexriver, aes(x = doy, y = n)) +
  geom_col(aes(), position = 'dodge') +
  facet_wrap(~array, scales = 'free',
             nrow = 4, labeller = labeller(array = river.labs)) +
  xlab("Day of year of detection") +
  ylab("Number of detections") +
  #set the limits and tick breaks for the y-axis
  scale_y_continuous(limits = c(0,20), breaks = seq(0, 20, by = 5), expand = c(0,0)) +
  scale_x_continuous(limits = c(213,250), breaks = seq(215, 245, by = 10),expand = c(0,0)) +
  theme_classic() +
  theme (axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
         axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
         #set the font type
         text = element_text(family = "Arial"),
         legend.position = c(.20,.65),
         legend.background = element_blank(),
         legend.box.background = element_rect(colour = "black"),
         #margin for the plot
         plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
         strip.background = element_blank(),
         strip.text = element_text(size = 14),
         #set size of the tick marks for y-axis
         axis.ticks.y = element_line(size = 0.5),
         #set size of the tick marks for x-axis
         axis.ticks.x = element_blank(),
         #adjust length of the tick marks
         axis.ticks.length = unit(0.2,"cm"),
         #set size and location of the tick labels for the y axis
         axis.text.y = element_text(colour = "black", size = 12, angle = 0, vjust = 0.5, hjust = 1,
                                    margin = margin(t = 0, r = 5, b = 0, l = 0)),
         #set size and location of the tick labels for the x axis
         axis.text.x = element_text(colour = "black", size = 12, angle = 0, vjust = 0, hjust = 0.5,
                                    margin = margin(t = 5, r = 0, b = 0, l = 0)),
         #set the axis size, color, and end shape
         axis.line = element_line(colour = "black", size = 0.5, lineend = "square"),
         panel.spacing.y = unit(1, 'lines'))

detbydoyriversex

ggsave(detbydoyriversex, file = "figs/dets_by_doy_river2.png", width = 24, height = 30, units = "cm", dpi = 600)

### Distance from shore ####
detn <- left_join(detcoho, tag[,c(1,5,28)], by = 'tag.ID')

detn$Stock <- ifelse(detn$stock %in% c(4, 'N', 'S'), 'Transitory', 
                             ifelse(detn$stock == 5, 'Shaktoolik', 
                                    ifelse(detn$stock == 6, 'Unalakleet', 
                                           NA)))

detn$year <- year(mdy_hm(detn$ymd.hms))
detn$rec.ID <- paste(detn$array, detn$rec, sep = '.')
detn <- left_join(detn, rec[,c(1:2, 5)], by = c('rec.ID', 'year'))



detn2 <- detn %>% 
  group_by(array, dist, doy, sex, Stock) %>% 
  count()

library(scales)
integer_breaks <- function(n = 5, ...) {
  breaker <- pretty_breaks(n, ...)
  function(x) {
    breaks <- breaker(x)
    breaks[breaks == floor(breaks)]
  }
}

distshore <- ggplot(subset(detn2, !(array %in% LETTERS)), aes(x = dist, y = n)) +
  geom_col() +
  facet_wrap(~array, scales = 'free',
             nrow = 7,
             labeller = labeller(array = array.labs)) +
  xlab("Distance from shore") +
  ylab("Number of detections") +
  #set the limits and tick breaks for the y-axis
  scale_y_continuous(breaks = integer_breaks(), expand = c(0,0)) +
  scale_x_continuous(limits = c(200,2700), breaks = seq(200, 2600, by = 200),expand = c(0,0)) +
  theme_classic() +
  theme (axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
         axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
         #set the font type
         text = element_text(family = "Arial"),
         legend.position = c(.20,.85),
         legend.background = element_blank(),
         legend.box.background = element_rect(colour = "black"),
         #margin for the plot
         plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
         strip.background = element_blank(),
         strip.text = element_text(size = 14),
         #set size of the tick marks for y-axis
         axis.ticks.y = element_line(size = 0.5),
         #set size of the tick marks for x-axis
         axis.ticks.x = element_blank(),
         #adjust length of the tick marks
         axis.ticks.length = unit(0.2,"cm"),
         #set size and location of the tick labels for the y axis
         axis.text.y = element_text(colour = "black", size = 12, angle = 0, vjust = 0.5, hjust = 1,
                                    margin = margin(t = 0, r = 5, b = 0, l = 0)),
         #set size and location of the tick labels for the x axis
         axis.text.x = element_text(colour = "black", size = 12, angle = 0, vjust = 0, hjust = 0.5,
                                    margin = margin(t = 5, r = 0, b = 0, l = 0)),
         #set the axis size, color, and end shape
         axis.line = element_line(colour = "black", size = 0.5, lineend = "square"),
         panel.spacing.y = unit(1, 'lines'))

distshore

ggsave(distshore, file = "figs/dets_dist_shore.png", width = 24, height = 30, units = "cm", dpi = 600)

detcohon <- na.omit(detcoho) %>% 
  group_by(dist) %>% 
  count()

distshore <- ggplot(detcohon, aes(x = dist, y = n)) +
  geom_col() +
  xlab("Distance from shore (m)") +
  ylab("Number of detections") +
  #set the limits and tick breaks for the y-axis
  scale_y_continuous(limits = c(0,120), breaks = seq(0, 120, by = 20),expand = c(0,0)) +
  scale_x_continuous(limits = c(200,2700), breaks = seq(200, 2600, by = 200),expand = c(0,0)) +
  theme_classic() +
  theme (axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
         axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
         #set the font type
         text = element_text(family = "Arial"),
         legend.position = c(.20,.85),
         legend.background = element_blank(),
         legend.box.background = element_rect(colour = "black"),
         #margin for the plot
         plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
         strip.background = element_blank(),
         strip.text = element_text(size = 14),
         #set size of the tick marks for y-axis
         axis.ticks.y = element_line(size = 0.5),
         #set size of the tick marks for x-axis
         axis.ticks.x = element_blank(),
         #adjust length of the tick marks
         axis.ticks.length = unit(0.2,"cm"),
         #set size and location of the tick labels for the y axis
         axis.text.y = element_text(colour = "black", size = 12, angle = 0, vjust = 0.5, hjust = 1,
                                    margin = margin(t = 0, r = 5, b = 0, l = 0)),
         #set size and location of the tick labels for the x axis
         axis.text.x = element_text(colour = "black", size = 12, angle = 0, vjust = 0, hjust = 0.5,
                                    margin = margin(t = 5, r = 0, b = 0, l = 0)),
         #set the axis size, color, and end shape
         axis.line = element_line(colour = "black", size = 0.5, lineend = "square"),
         panel.spacing.y = unit(1, 'lines'))

distshore

ggsave(distshore, file = "figs/dets_dist_shore2.png", width = 24, height = 15, units = "cm", dpi = 600)

detn2$Sex <- detn2$sex


distshoresex <- ggplot(subset(detn2, !(array %in% LETTERS)), aes(x = dist, y = n, fill = Sex)) +
  geom_col() +
  facet_wrap(~array, scales = 'free',
             nrow = 7,
             labeller = labeller(array = array.labs)) +
  xlab("Distance from shore") +
  ylab("Number of detections") +
  #set the limits and tick breaks for the y-axis
  scale_y_continuous(breaks = integer_breaks(), expand = c(0,0)) +
  scale_x_continuous(limits = c(200,2700), breaks = seq(200, 2600, by = 200),expand = c(0,0)) +
  theme_classic() +
  theme (axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
         axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
         #set the font type
         text = element_text(family = "Arial"),
         legend.position = c(.82,.06),
         legend.background = element_blank(),
         legend.box.background = element_rect(colour = "black"),
         #margin for the plot
         plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
         strip.background = element_blank(),
         strip.text = element_text(size = 14),
         #set size of the tick marks for y-axis
         axis.ticks.y = element_line(size = 0.5),
         #set size of the tick marks for x-axis
         axis.ticks.x = element_blank(),
         #adjust length of the tick marks
         axis.ticks.length = unit(0.2,"cm"),
         #set size and location of the tick labels for the y axis
         axis.text.y = element_text(colour = "black", size = 12, angle = 0, vjust = 0.5, hjust = 1,
                                    margin = margin(t = 0, r = 5, b = 0, l = 0)),
         #set size and location of the tick labels for the x axis
         axis.text.x = element_text(colour = "black", size = 12, angle = 0, vjust = 0, hjust = 0.5,
                                    margin = margin(t = 5, r = 0, b = 0, l = 0)),
         #set the axis size, color, and end shape
         axis.line = element_line(colour = "black", size = 0.5, lineend = "square"),
         panel.spacing.y = unit(1, 'lines'))

distshoresex

ggsave(distshoresex, file = "figs/dets_dist_shoresex.png", width = 19, height = 30, units = "cm", dpi = 600)


distshorestock <- ggplot(subset(detn2, !(array %in% LETTERS)), aes(x = dist, y = n, fill = Stock)) +
  geom_col() +
  facet_wrap(~array, scales = 'free',
             nrow = 7,
             labeller = labeller(array = array.labs)) +
  xlab("Distance from shore (m)") +
  ylab("Number of detections") +
  #set the limits and tick breaks for the y-axis
  scale_y_continuous(breaks = integer_breaks(), expand = c(0,0)) +
  scale_x_continuous(limits = c(200,2700), breaks = seq(200, 2600, by = 200),expand = c(0,0)) +
  theme_classic() +
  theme (axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
         axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
         #set the font type
         text = element_text(family = "Arial"),
         legend.position = c(.82,.06),
         legend.background = element_blank(),
         legend.box.background = element_rect(colour = "black"),
         #margin for the plot
         plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
         strip.background = element_blank(),
         strip.text = element_text(size = 14),
         #set size of the tick marks for y-axis
         axis.ticks.y = element_line(size = 0.5),
         #set size of the tick marks for x-axis
         axis.ticks.x = element_blank(),
         #adjust length of the tick marks
         axis.ticks.length = unit(0.2,"cm"),
         #set size and location of the tick labels for the y axis
         axis.text.y = element_text(colour = "black", size = 12, angle = 0, vjust = 0.5, hjust = 1,
                                    margin = margin(t = 0, r = 5, b = 0, l = 0)),
         #set size and location of the tick labels for the x axis
         axis.text.x = element_text(colour = "black", size = 12, angle = 0, vjust = 0, hjust = 0.5,
                                    margin = margin(t = 5, r = 0, b = 0, l = 0)),
         #set the axis size, color, and end shape
         axis.line = element_line(colour = "black", size = 0.5, lineend = "square"),
         panel.spacing.y = unit(1, 'lines')) +
  guides(fill=guide_legend(nrow=2))

distshorestock

ggsave(distshorestock, file = "figs/dets_dist_shorestock.png", width = 24, height = 30, units = "cm", dpi = 600)

distshorestock <- ggplot(subset(detrecstockn[which(complete.cases(detrecstockn)),], !(array %in% LETTERS)), aes(x = dist, y = n, fill = Stock)) +
  geom_col() +
  facet_wrap(~array, scales = 'free',
             nrow = 7,
             labeller = labeller(array = array.labs)) +
  xlab("Distance from shore") +
  ylab("Number of detections") +
  #set the limits and tick breaks for the y-axis
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(limits = c(200,2700), breaks = seq(200, 2600, by = 200),expand = c(0,0)) +
  theme_classic() +
  theme (axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
         axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
         #set the font type
         text = element_text(family = "Arial"),
         legend.position = c(.82,.06),
         legend.background = element_blank(),
         legend.box.background = element_rect(colour = "black"),
         #margin for the plot
         plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
         strip.background = element_blank(),
         strip.text = element_text(size = 14),
         #set size of the tick marks for y-axis
         axis.ticks.y = element_line(size = 0.5),
         #set size of the tick marks for x-axis
         axis.ticks.x = element_blank(),
         #adjust length of the tick marks
         axis.ticks.length = unit(0.2,"cm"),
         #set size and location of the tick labels for the y axis
         axis.text.y = element_text(colour = "black", size = 12, angle = 0, vjust = 0.5, hjust = 1,
                                    margin = margin(t = 0, r = 5, b = 0, l = 0)),
         #set size and location of the tick labels for the x axis
         axis.text.x = element_text(colour = "black", size = 12, angle = 0, vjust = 0, hjust = 0.5,
                                    margin = margin(t = 5, r = 0, b = 0, l = 0)),
         #set the axis size, color, and end shape
         axis.line = element_line(colour = "black", size = 0.5, lineend = "square"),
         panel.spacing.y = unit(1, 'lines')) 

distshorestock

ggsave(distshorestock, file = "figs/dets_dist_shorestock3.png", width = 19, height = 30, units = "cm", dpi = 600)


## Dist stats ####
summary(aov(dist~Stock, data = na.omit(detn)))

summary(aov(dist~sex, data = detn))

summary(aov(dist~as.factor(array), data = detn))

detnstats <- 
  na.omit(detn) %>% 
  group_by(Stock) %>% 
  get_summary_stats(dist, type = 'mean_sd')

detnarraystats <- detn %>% 
  group_by(array) %>% 
  get_summary_stats(dist, type = 'mean_sd')

detnmarine <- detn[which(!(detn$array %in% LETTERS)), ]

detnriver <- detn[which(detn$array %in% LETTERS), ]

# Scrap ####
detcoho <- filter(det, species == 'coho')

detcoho$year <- year(detcoho$ymd.hms)

subset(detcoho, array %in% c('E', 'U', 'G')) %>% 
  group_by(year, array) %>% 
  count()
