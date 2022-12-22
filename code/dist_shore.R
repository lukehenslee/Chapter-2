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
tag <- read.csv(file=file.path(data, "mcode.csv"),
                colClasses = paste(str[1,]))

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
det <- rbind(detsum[,c(1,4:5,3,9:10)], recapnodet[,c(1:4, 7:8)])
det$tag.ID <- as.factor(det$tag.ID)
### Join with 'tag' to get grouping variables 'sex','species', and 'stock'
det <- left_join(det, tag[, c(1, 5, 8, 28)], by = 'tag.ID')

# Modify stock
det$stock <- ifelse(det$stock %in% c(4, 'N', 'S'), 'Transitory', 
                             ifelse(det$stock == 5, 'Shaktoolik', 
                                    ifelse(det$stock == 6, 'Unalakleet', 
                                           NA)))

det$year <- year(mdy_hm(det$ymd.hms))
det$rec.ID <- paste(det$array, det$rec, sep = '.')
det <- left_join(det, rec[,c(1:2, 5)], by = c('rec.ID', 'year'))

detcoho <- det[which(det[,8] == 'coho'),]
detcoho$stock <- factor(detcoho$stock, levels = c('Shaktoolik', 'Unalakleet',
                                                  'Transitory'))

# Summary stats
detcoho %>% 
  group_by(stock) %>% 
  get_summary_stats(dist, type = 'mean_sd')

boxplot(dist ~ stock, data = detcoho)

detcohostock <- detcoho[is.na(detcoho$stock) == F,]

diststock <- ggplot(detcohostock, aes(x = stock, y = dist)) +
  geom_boxplot() +
  xlab("Stock") +
  ylab("Distance from shore (m)") +
  #set the limits and tick breaks for the y-axis
  scale_y_continuous(limits = c(0,3000), breaks = seq(0, 3000, by = 500), expand = c(0,0)) +
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

diststock
ggsave(diststock, file = "figs/dist_by_stock.png", width = 19, height = 15, units = "cm", dpi = 600)


# Unalakleet fish ####
detcoho <- left_join(detcoho, tag[,c(1,29)])
detunk <- detcoho[which(detcoho$spawn.stream == 'U'),]
detunk <- detunk[complete.cases(detunk),]

detunk$proximity <- ifelse(detunk[,4] == '5', 'close', ifelse(detunk[,4] %in% c('4', '6'),
                                                       'mid', 'far'))
detunk$proximity <- factor(detunk$proximity, levels = c('close', 'mid', 'far'))

boxplot(dist~proximity, data = detunk)

detunkdist <- detunk %>% 
  group_by(proximity) %>% 
  get_summary_stats(dist, type = 'mean_sd')

summary(aov(dist ~ proximity, data = detunk))

distprox <- ggplot(detunk, aes(x = proximity, y = dist)) +
  geom_boxplot() +
  xlab("Relative distance from Unalakleet River mouth") +
  ylab("Distance from shore (m)") +
  #set the limits and tick breaks for the y-axis
  scale_y_continuous(limits = c(0,3000), breaks = seq(0, 3000, by = 500), expand = c(0,0)) +
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

distprox
ggsave(distprox, file = "figs/dist_by_prox.png", width = 19, height = 15, units = "cm", dpi = 600)



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
summary(aov(dist~stock, data = na.omit(detn)))

summary(aov(dist~sex, data = detn))

summary(aov(dist~as.factor(array), data = detn))

detnstats <- 
  na.omit(detn) %>% 
  group_by(stock) %>% 
  get_summary_stats(dist, type = 'mean_sd')

detnarraystats <- detn %>% 
  group_by(array) %>% 
  get_summary_stats(dist, type = 'mean_sd')

detnmarine <- detn[which(!(detn$array %in% LETTERS)), ]

detnriver <- detn[which(detn$array %in% LETTERS), ]

# Scrap ####
detsum$tag.ID <- factor(detsum$tag.ID)
det2 <- left_join(det, detsum[,c(1,3:4)])
detcoho <- filter(det2, species == 'coho')

detcoho$year <- year(detcoho$ymd.hms)

subset(detcoho, array %in% c('E', 'U', 'G')) %>% 
  group_by(year, array) %>% 
  count()
