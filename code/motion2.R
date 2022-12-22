#==================================================================================================
# Thesis chapter 2- motion statistics
# Date: September 7, 2022
# Creator: Luke Henslee, ADF&G and CFOS, UAF
#
# Purpose: Calculate motion stats

#==================================================================================================
#NOTES: 
#
#==================================================================================================

# Load packages ####

library(tidyverse)
library(rstatix)


# Set working directory ####
setwd("H:/git_repo/Chapter-2")

# Import data ####
mo <- read.csv('data/movement/movement_both_years_no_river.csv', 
               colClasses = c('factor', 'numeric','numeric', 'factor',
                              'factor', 'numeric', 'factor'))
str(mo)

str <- read.csv('data/mcode_colClasses.csv')
tag <- read.csv('data/mcode.csv',
                colClasses = paste(str[1,]))

# Remove incomplete rows
mo <- mo[complete.cases(mo),]

# Remove any with speed = 0
mo <- mo[which(mo[,6] > 0),]

# Add 'stat.week' and 'year'
mo <- left_join(mo, tag[,c(1:2,6,31)], by = 'tag.ID')

mo$stock <- factor(mo$stock, levels = c('Shaktoolik', 'Unalakleet', 'Transitory'))

# Calculate body lengths/second
mo$bl <- mo$speed/(mo$length * 0.001)
mean(mo$bl)
hist(mo$bl)
boxplot(log(mo$bl))

# Stats ####

## Average speed ####

### All fish within study area ####
mean(mo$speed) # 0.11
sd(mo$speed)

### By SD ####
mo %>% 
  group_by(subdistrict) %>% 
  get_summary_stats(speed, type = 'mean_sd')

summary(aov(log(speed) ~ subdistrict, mo))

boxplot(log(speed) ~ subdistrict, mo)

### By stock ####
mo %>% 
  group_by(stock) %>% 
  get_summary_stats(speed, type = 'mean_sd')

summary(aov(log(speed) ~ stock, mo))

boxplot(log(speed) ~ stock, mo)

### By stock by subdistrict ####
mo %>% 
  group_by(stock, subdistrict) %>% 
  get_summary_stats(speed, type = 'mean_sd')

summary(aov(log(speed) ~ subdistrict + stock, mo))

boxplot(log(speed) ~ subdistrict + stock, mo)

### By sex ####
mo %>% 
  group_by(sex) %>% 
  get_summary_stats(speed, type = 'mean_sd')

summary(aov(log(speed) ~ sex, mo))

boxplot(log(speed) ~ sex, mo)

### By stat week ####
mo %>% 
  group_by(stat.week) %>% 
  get_summary_stats(speed, type = 'mean_sd')

summary(aov(log(speed) ~ stat.week, mo))

boxplot(log(speed) ~ stat.week, mo)

### By year ####
mo %>% 
  group_by(year) %>% 
  get_summary_stats(speed, type = 'mean_sd')

summary(aov(log(speed) ~ year, mo))

boxplot(log(speed) ~ year, mo)

### By stat week + year ####
mo %>% 
  group_by(stat.week, year) %>% 
  get_summary_stats(speed, type = 'mean_sd')

summary(aov(log(speed) ~ stat.week + year, mo))

boxplot(log(speed) ~ stat.week + year, mo)

# ANOVA ####

## By SD ####
summary(aov(log(speed) ~ subdistrict, mo))

# UNK fish speed ~ proximity to UNK river mouth
unk_mo <- mo[which(mo[,5] == 'Unalakleet'),]
unk_mo <- left_join(unk_mo, tag[,c(1, 29)])
unk_mo <- unk_mo[which(unk_mo[,10] == 'U'),]

unk_mo_close <- unk_mo[which(unk_mo[,2] < 63.9546 & unk_mo[,2] > 63.6945),]
unk_mo_far <- anti_join(unk_mo, unk_mo_close)

mean(unk_mo_close$speed)
mean(unk_mo_far$speed)

unk_mo_close$mouth <- 'close'
unk_mo_far$mouth <- 'far'

unk_mo <- rbind(unk_mo_close, unk_mo_far)

boxplot(log(speed) ~ mouth, unk_mo)
summary(aov(log(speed) ~ mouth, unk_mo))

# Body lengths / second ####
### All fish within study area ####
mean(mo$bl) # 0.21
sd(mo$bl) # 0.31
max(mo$bl)
min(mo$bl)

### By SD ####
mo %>% 
  group_by(subdistrict) %>% 
  get_summary_stats(bl, type = 'mean_sd')

summary(aov(log(bl) ~ subdistrict, mo))

boxplot(bl ~ subdistrict, mo, outline = F)

### By stock ####
mo %>% 
  group_by(stock) %>% 
  get_summary_stats(bl, type = 'mean_sd')

summary(aov(log(bl) ~ stock, mo))

boxplot(log(bl) ~ stock, mo, outline = F)

### By stock by subdistrict ####
mo %>% 
  group_by(stock, subdistrict) %>% 
  get_summary_stats(bl, type = 'mean_sd')

summary(aov(log(bl) ~ subdistrict + stock, mo))

boxplot(log(bl) ~ subdistrict + stock, mo)

### By sex ####
mo %>% 
  group_by(sex) %>% 
  get_summary_stats(bl, type = 'mean_sd')

summary(aov(log(bl) ~ sex, mo))

boxplot(log(bl) ~ sex, mo)

### By stat week ####
mo %>% 
  group_by(stat.week) %>% 
  get_summary_stats(bl, type = 'mean_sd')

summary(aov(log(bl) ~ stat.week, mo))

boxplot(log(bl) ~ stat.week, mo)

### By year ####
mo %>% 
  group_by(year) %>% 
  get_summary_stats(bl, type = 'mean_sd')

summary(aov(log(bl) ~ year, mo))

boxplot(log(bl) ~ year, mo)

### By stat week + year ####
mo %>% 
  group_by(stat.week, year) %>% 
  get_summary_stats(bl, type = 'mean_sd')

summary(aov(log(bl) ~ stat.week + year, mo))

boxplot(log(bl) ~ stat.week + year, mo)

# ANOVA ####

## By SD ####
summary(aov(log(bl) ~ subdistrict, mo))

# KM/DAY ####
mo$km <- mo$speed * 86.4

### All fish within study area ####
mean(mo$km) # 0.11
sd(mo$km)

### By SD ####
mo %>% 
  group_by(subdistrict) %>% 
  get_summary_stats(km, type = 'mean_sd')

summary(aov(log(km) ~ subdistrict, mo))

boxplot(log(km) ~ subdistrict, mo)

### By stock ####
mo %>% 
  group_by(stock) %>% 
  get_summary_stats(km, type = 'mean_sd')

summary(aov(log(km) ~ stock, mo))

boxplot(log(km) ~ stock, mo)


### By stock by subdistrict ####
mo %>% 
  group_by(stock, subdistrict) %>% 
  get_summary_stats(km, type = 'mean_sd')

summary(aov(log(km) ~ subdistrict + stock, mo))

boxplot(log(km) ~ subdistrict + stock, mo)

### By sex ####
mo %>% 
  group_by(sex) %>% 
  get_summary_stats(km, type = 'mean_sd')

summary(aov(log(km) ~ sex, mo))

boxplot(log(km) ~ sex, mo)

### By stat week ####
mo %>% 
  group_by(stat.week) %>% 
  get_summary_stats(km, type = 'mean_sd')

summary(aov(log(km) ~ stat.week, mo))

boxplot(log(km) ~ stat.week, mo)

### By year ####
mo %>% 
  group_by(year) %>% 
  get_summary_stats(km, type = 'mean_sd')

summary(aov(log(km) ~ year, mo))

boxplot(log(km) ~ year, mo)

### By stat week + year ####
mo %>% 
  group_by(stat.week, year) %>% 
  get_summary_stats(km, type = 'mean_sd')

summary(aov(log(km) ~ stat.week + year, mo))

boxplot(log(km) ~ stat.week + year, mo)

# UNK fish bl ~ proximity to UNK river mouth ####
unk_mo <- mo[which(mo[,5] == 'Unalakleet'),]
unk_mo <- left_join(unk_mo, tag[,c(1, 29)])
unk_mo <- unk_mo[which(unk_mo[,13] == 'U'),]

unk_mo_close <- unk_mo[which(unk_mo[,2] < 63.9546 & unk_mo[,2] > 63.6945),]
unk_mo_far <- unk_mo[which(unk_mo[,2] < 64.1313),]
unk_mo_far <- anti_join(unk_mo_far, unk_mo_close)
unk_mo_vfar <- unk_mo[which(unk_mo[,2] > 64.1313),]

mean(unk_mo_close$bl)
mean(unk_mo_far$bl)
mean(unk_mo_vfar$bl)

unk_mo_close$mouth <- 'close'
unk_mo_far$mouth <- 'mid'
unk_mo_vfar$mouth <- 'far'

unk_mo <- rbind(unk_mo_close, unk_mo_far, unk_mo_vfar)

unk_mo$mouth <- factor(unk_mo$mouth, levels = c('close', 'mid', 'far'))

unk_mo %>% 
  group_by(mouth) %>% 
  get_summary_stats(bl, type = 'mean_sd')

boxplot(bl ~ mouth, unk_mo, outline = F)
summary(aov(log(bl) ~ mouth, unk_mo))


#LM ####
summary(lm(log(bl) ~ sex + stat.week + subdistrict + year + stock, data = mo))

# Figures ####
blbystock <- ggplot(mo, aes(x = as.factor(stock), y = km)) +
  geom_boxplot(outlier.shape = NA) +
  xlab("Stock") +
  ylab('Swim speed (km/day)') +
  #set the limits and tick breaks for the y-axis
  scale_y_continuous(limits = c(0,80), breaks = seq(0, 80, by = 20), expand = c(0,0)) +
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

blbystock
ggsave(blbystock, file = "figs/speed_stock_km.png", width = 19, height = 15, units = "cm", dpi = 600)

unkfish <- ggplot(unk_mo, aes(x = as.factor(mouth), y = bl)) +
  geom_boxplot(outlier.shape = NA) +
  xlab('Relative distance from Unalakleet River mouth') +
  ylab('Swim speed (MTF/s)') +
  #set the limits and tick breaks for the y-axis
  scale_y_continuous(limits = c(0,2), breaks = seq(0, 2, by = 0.5), expand = c(0,0)) +
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

unkfish
ggsave(unkfish, file = "figs/speed_unkfish.png", width = 19, height = 15, units = "cm", dpi = 600)

