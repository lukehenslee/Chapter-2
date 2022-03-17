#==================================================================================================
# Range test
# Date: March 15, 2022
# Creator: Luke Henslee, ADF&G and CFOS, UAF
#
# Purpose: 
#==================================================================================================
#NOTES: 
#==================================================================================================

# Load packages ####
library(tidyverse)
library(lubridate)


# Set working directory ####
setwd("C:/Users/lhhenslee/Desktop/Luke/School/Thesis/Chapter 2/Chapter-2") # Work
setwd("C:/Users/lukeh/Desktop/School/GIT repos/Chapter-2") # School

# Import data ####

mar <- read.csv('data/marine_range.csv')

# Manipulate data ####
mar$mdy.hms <- mdy_hms(paste(mar$Date, mar$Time, sep = ' '))
mar$unix <- as.numeric(mar$mdy.hms)
mar$minute2 <- minute(mar$mdy.hms)

# Detections per min ####
mar2 <- mar |>
  mutate(minute = 1 + ((unix - min(unix)) %/% 60)) |>
  mutate(minute = factor(minute, levels = seq(1, max(minute)))) |>
  mutate(Tag.ID = factor(Tag.ID)) |>
  group_by(minute, Tag.ID, .drop = FALSE) |>
  summarize(freq = n(), .groups = "drop")


mar2$prop <- mar2$freq/12
mar2$dist <- seq(75, 1000, length = nrow(mar2))


# Visualize ####

plot(x = mar2$dist, mar2$prop)

ggplot(data = mar2, aes(x = dist, y = prop)) +
  geom_line() +
  geom_hline(yintercept = 1, linetype = 'dashed') +
  scale_x_continuous(limits = c(75, 1000)) +
  scale_y_continuous(limits = c(-0.1, 3.5), expand = c(0,0)) +
  theme_classic() +
  theme (axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
         axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
         #set the font type
         text = element_text(),
         #modify plot title, the B in this case
         plot.title = element_text(face = "bold"),
         #position the legend on the figure
         legend.position = c(.65, .7),
         legend.title = element_text(size = 14),
         #adjust size of text for legend
         legend.text = element_text(size = 14),
         #margin for the plot
         plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
         strip.background = element_blank(),
         strip.text = element_text(size = 14),
         #set size of the tick marks for y-axis
         axis.ticks.y = element_line(size = 0.5),
         #set size of the tick marks for x-axis
         axis.ticks.x = element_line(size = 0.5),
         #adjust length of the tick marks
         axis.ticks.length = unit(0.2,"cm"),
         #set size and location of the tick labels for the y axis
         axis.text.y = element_text(colour = "black", size = 10, angle = 0, vjust = 0.5, hjust = 1,
                                    margin = margin(t = 0, r = 5, b = 0, l = 0)),
         #set size and location of the tick labels for the x axis
         axis.text.x = element_text(colour = "black", size = 10, angle = 0, vjust = 0, hjust = 0.5,
                                    margin = margin(t = 5, r = 0, b = 0, l = 0)),
         #set the axis size, color, and end shape
         axis.line = element_line(colour = "black", size = 0.5, lineend = "square"))

# Try with the mystery data

riv24 <- read.csv('data/range/marine_range_24.csv', skip = 45)
riv71 <- read.csv('data/range/marine_range_71.csv', skip = 45)

marx2 <- rbind(mar24, mar71)

marx2$unix <- as.numeric(mdy_hms(paste(marx2$Date, marx2$Time, sep = ' ')))

unkrange <- marx2 %>% 
  filter(unix > 1595431620, unix < 1595432407, Tag.ID %in% c('15501', '15442'))

plot(x = unkrange$unix, unkrange$Power)

unk2 <- unkrange |>
  mutate(minute = 1 + ((unix - min(unix)) %/% 60)) |>
  mutate(minute = factor(minute, levels = seq(1, max(minute)))) |>
  mutate(Tag.ID = factor(Tag.ID)) |>
  group_by(minute, Tag.ID, .drop = FALSE) |>
  summarize(freq = n(), .groups = "drop")
unk2$prop <- unk2$freq/12

unk2$dist <- seq(161, 66, length = nrow(unk2))
plot(x = unk2$dist, unk2$prop)

unk3 <- marx2 %>% filter(unix > 1595432407, unix < 1595433060, Tag.ID %in% c('15501', '15442'))

unk4 <- unk3 |>
  mutate(minute = 1 + ((unix - min(unix)) %/% 60)) |>
  mutate(minute = factor(minute, levels = seq(1, max(minute)))) |>
  mutate(Tag.ID = factor(Tag.ID)) |>
  group_by(minute, Tag.ID, .drop = FALSE) |>
  summarize(freq = n(), .groups = "drop")
unk4$prop <- unk4$freq/12
unk4$dist <- seq(46, 400, length = nrow(unk4))

plot(x = unk4$dist, y = unk4$prop)

unk <- rbind (unk2, unk4)
plot(unk$dist, unk$prop)

marine <- marx2 %>% filter(unix > 1599559662, unix < 1599562000, Tag.ID == '15536')

marine2 <- marine |>
  mutate(minute = 1 + ((unix - min(unix)) %/% 60)) |>
  mutate(minute = factor(minute, levels = seq(1, max(minute)))) |>
  mutate(Tag.ID = factor(Tag.ID)) |>
  group_by(minute, Tag.ID, .drop = FALSE) |>
  summarize(freq = n(), .groups = "drop")
marine2$prop <- marine2$freq/24
marine2$dist <- seq(50, 1000, length = nrow(marine2))

plot(x = marine2$dist, y = marine2$prop)

koy <- marx2 %>% filter(unix > 1595603404, unix < 1595604903, Tag.ID %in% c('15477', '15442'))

koy.1 <- marx2 %>% filter(unix > 1595603405, unix < 1595603729, Tag.ID %in% c('15477', '15442'))
koy.1 <- koy.1 |>
  mutate(minute = 1 + ((unix - min(unix)) %/% 60)) |>
  mutate(minute = factor(minute, levels = seq(1, max(minute)))) |>
  mutate(Tag.ID = factor(Tag.ID)) |>
  group_by(minute, Tag.ID, .drop = FALSE) |>
  summarize(freq = n(), .groups = "drop")

koy.2 <- marx2 %>% filter(unix > 1595603729, unix < 1595603883, Tag.ID %in% c('15477', '15442'))
koy.2 <- koy.2 |>
  mutate(minute = 1 + ((unix - min(unix)) %/% 60)) |>
  mutate(minute = factor(minute, levels = seq(1, max(minute)))) |>
  mutate(Tag.ID = factor(Tag.ID)) |>
  group_by(minute, Tag.ID, .drop = FALSE) |>
  summarize(freq = n(), .groups = "drop")
koy.3 <- marx2 %>% filter(unix > 1595604022, unix < 1595604078, Tag.ID %in% c('15477', '15442'))
koy.3 <- koy.3 |>
  mutate(minute = 1 + ((unix - min(unix)) %/% 60)) |>
  mutate(minute = factor(minute, levels = seq(1, max(minute)))) |>
  mutate(Tag.ID = factor(Tag.ID)) |>
  group_by(minute, Tag.ID, .drop = FALSE) |>
  summarize(freq = n(), .groups = "drop")
koy.4 <- marx2 %>% filter(unix > 1595604077, unix < 1595604253, Tag.ID %in% c('15477', '15442'))
koy.4 <- koy.4 |>
  mutate(minute = 1 + ((unix - min(unix)) %/% 60)) |>
  mutate(minute = factor(minute, levels = seq(1, max(minute)))) |>
  mutate(Tag.ID = factor(Tag.ID)) |>
  group_by(minute, Tag.ID, .drop = FALSE) |>
  summarize(freq = n(), .groups = "drop")
koy.5 <- marx2 %>% filter(unix > 1595604253, unix < 1595604842, Tag.ID %in% c('15477', '15442'))
koy.5 <- koy.5 |>
  mutate(minute = 1 + ((unix - min(unix)) %/% 60)) |>
  mutate(minute = factor(minute, levels = seq(1, max(minute)))) |>
  mutate(Tag.ID = factor(Tag.ID)) |>
  group_by(minute, Tag.ID, .drop = FALSE) |>
  summarize(freq = n(), .groups = "drop")

koy <- rbind(koy.1, koy.2, koy.3, koy.4, koy.5)

koy$prop <- koy$freq/12
koy$dist <- c(seq(350, 20, length = 20), seq(100, 400, length = 16))

plot(koy$dist, koy$prop)

koy2 <- koy |>
  mutate(minute = 1 + ((unix - min(unix)) %/% 60)) |>
  mutate(minute = factor(minute, levels = seq(1, max(minute)))) |>
  mutate(Tag.ID = factor(Tag.ID)) |>
  group_by(minute, Tag.ID, .drop = FALSE) |>
  summarize(freq = n(), .groups = "drop")
koy2$prop <- koy2$freq/12
koy2$dist <- c(seq(300, 20, length = 40), seq(30, 500, length = 10))

plot(koy2$dist, koy2$prop)

unk.plot <- ggplot(data = unk, aes(x = dist, y = prop)) +
  geom_point() +
  xlab('Distance (m)') +
  ylab('Proportion of transmissions detected') +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = F) +
  scale_x_continuous(limits = c(45, 400), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0,0), breaks = seq(0,1,.25)) +
  theme_classic() +
  theme (axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
         axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
         #set the font type
         text = element_text(),
         #modify plot title, the B in this case
         plot.title = element_text(face = "bold"),
         #position the legend on the figure
         legend.position = c(.65, .7),
         legend.title = element_text(size = 14),
         #adjust size of text for legend
         legend.text = element_text(size = 14),
         #margin for the plot
         plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
         strip.background = element_blank(),
         strip.text = element_text(size = 14),
         #set size of the tick marks for y-axis
         axis.ticks.y = element_line(size = 0.5),
         #set size of the tick marks for x-axis
         axis.ticks.x = element_line(size = 0.5),
         #adjust length of the tick marks
         axis.ticks.length = unit(0.2,"cm"),
         #set size and location of the tick labels for the y axis
         axis.text.y = element_text(colour = "black", size = 10, angle = 0, vjust = 0.5, hjust = 1,
                                    margin = margin(t = 0, r = 5, b = 0, l = 0)),
         #set size and location of the tick labels for the x axis
         axis.text.x = element_text(colour = "black", size = 10, angle = 0, vjust = 0, hjust = 0.5,
                                    margin = margin(t = 5, r = 0, b = 0, l = 0)),
         #set the axis size, color, and end shape
         axis.line = element_line(colour = "black", size = 0.5, lineend = "square"))

unk.plot

ggsave(unk.plot, file = 'figs/unk_range_gam.png', width = 12, height = 10, units = "cm", dpi = 300)

koy.plot <- ggplot(data = koy, aes(x = dist, y = prop)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = F) +
  xlab('Distance (m)') +
  ylab('Proportion of transmissions detected') +
  scale_x_continuous(limits = c(50, 400), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0,0)) +
  theme_classic() +
  theme (axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
         axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
         #set the font type
         text = element_text(),
         #modify plot title, the B in this case
         plot.title = element_text(face = "bold"),
         #position the legend on the figure
         legend.position = c(.65, .7),
         legend.title = element_text(size = 14),
         #adjust size of text for legend
         legend.text = element_text(size = 14),
         #margin for the plot
         plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
         strip.background = element_blank(),
         strip.text = element_text(size = 14),
         #set size of the tick marks for y-axis
         axis.ticks.y = element_line(size = 0.5),
         #set size of the tick marks for x-axis
         axis.ticks.x = element_line(size = 0.5),
         #adjust length of the tick marks
         axis.ticks.length = unit(0.2,"cm"),
         #set size and location of the tick labels for the y axis
         axis.text.y = element_text(colour = "black", size = 10, angle = 0, vjust = 0.5, hjust = 1,
                                    margin = margin(t = 0, r = 5, b = 0, l = 0)),
         #set size and location of the tick labels for the x axis
         axis.text.x = element_text(colour = "black", size = 10, angle = 0, vjust = 0, hjust = 0.5,
                                    margin = margin(t = 5, r = 0, b = 0, l = 0)),
         #set the axis size, color, and end shape
         axis.line = element_line(colour = "black", size = 0.5, lineend = "square"))

koy.plot

ggsave(koy.plot, file = 'figs/koy_range_gam.png', width = 12, height = 10, units = "cm", dpi = 300)


mar.plot <- ggplot(data = mar2, aes(x = dist, y = prop)) +
  geom_point() +
  geom_smooth(se = F) +
  xlab('Distance (m)') +
  ylab('Proportion of transmissions detected') +
  scale_x_continuous(limits = c(75, 1000), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0,0)) +
  theme_classic() +
  theme (axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
         axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
         #set the font type
         text = element_text(),
         #modify plot title, the B in this case
         plot.title = element_text(face = "bold"),
         #position the legend on the figure
         legend.position = c(.65, .7),
         legend.title = element_text(size = 14),
         #adjust size of text for legend
         legend.text = element_text(size = 14),
         #margin for the plot
         plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
         strip.background = element_blank(),
         strip.text = element_text(size = 14),
         #set size of the tick marks for y-axis
         axis.ticks.y = element_line(size = 0.5),
         #set size of the tick marks for x-axis
         axis.ticks.x = element_line(size = 0.5),
         #adjust length of the tick marks
         axis.ticks.length = unit(0.2,"cm"),
         #set size and location of the tick labels for the y axis
         axis.text.y = element_text(colour = "black", size = 10, angle = 0, vjust = 0.5, hjust = 1,
                                    margin = margin(t = 0, r = 5, b = 0, l = 0)),
         #set size and location of the tick labels for the x axis
         axis.text.x = element_text(colour = "black", size = 10, angle = 0, vjust = 0, hjust = 0.5,
                                    margin = margin(t = 5, r = 0, b = 0, l = 0)),
         #set the axis size, color, and end shape
         axis.line = element_line(colour = "black", size = 0.5, lineend = "square"))

mar.plot

ggsave(mar.plot, file = 'figs/marine_range.png', width = 12, height = 10, units = "cm", dpi = 300)

mar.plot2 <- ggplot(data = marine2, aes(x = dist, y = prop)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = F) +
  xlab('Distance (m)') +
  ylab('Proportion of transmissions detected') +
  scale_x_continuous(limits = c(50, 1000), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0,0)) +
  theme_classic() +
  theme (axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
         axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
         #set the font type
         text = element_text(),
         #modify plot title, the B in this case
         plot.title = element_text(face = "bold"),
         #position the legend on the figure
         legend.position = c(.65, .7),
         legend.title = element_text(size = 14),
         #adjust size of text for legend
         legend.text = element_text(size = 14),
         #margin for the plot
         plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
         strip.background = element_blank(),
         strip.text = element_text(size = 14),
         #set size of the tick marks for y-axis
         axis.ticks.y = element_line(size = 0.5),
         #set size of the tick marks for x-axis
         axis.ticks.x = element_line(size = 0.5),
         #adjust length of the tick marks
         axis.ticks.length = unit(0.2,"cm"),
         #set size and location of the tick labels for the y axis
         axis.text.y = element_text(colour = "black", size = 10, angle = 0, vjust = 0.5, hjust = 1,
                                    margin = margin(t = 0, r = 5, b = 0, l = 0)),
         #set size and location of the tick labels for the x axis
         axis.text.x = element_text(colour = "black", size = 10, angle = 0, vjust = 0, hjust = 0.5,
                                    margin = margin(t = 5, r = 0, b = 0, l = 0)),
         #set the axis size, color, and end shape
         axis.line = element_line(colour = "black", size = 0.5, lineend = "square"))

mar.plot2

ggsave(mar.plot2, file = 'figs/marine_range_gam.png', width = 17, height = 15, units = "cm", dpi = 300)

summary(mar.gam <- gam(prop ~ s(dist), data = marine2[-1,]))
marine2$pred <- c(0, predict(mar.gam))

ggplot(marine2, aes(x = dist, fill = as.factor(pred))) +
  geom_bar() +
  coord_polar(theta = 'y') +
  theme(legend.position = 'none')

marine2$pred[marine2$pred < 0] <- 0

marine3 <- marine2 %>% 
  mutate(bins = cut(pred,breaks = unique(quantile(pred,
                                                            probs=seq.int(0,1, by=1/5))), 
                                                   include.lowest=T))

marine3$Proportion <- as.factor(round(marine3$pred * 4,)/4)



range.plot <- ggplot(marine3, aes(x = dist, color = Proportion, fill = Proportion)) +
  geom_bar() +
  xlab('Distance from receiver (m)') +
  ylab('') +
  scale_x_continuous(limits = c(0, 1000)) +
  scale_fill_brewer(palette = 'GnBu') +
  scale_color_brewer(palette = 'GnBu') +
  coord_polar(theta = 'y') +
  theme_classic() +
  theme(axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
        axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
        #set the font type
        text = element_text(),
        #modify plot title, the B in this case
        plot.title = element_text(face = "bold"),
        #position the legend on the figure
        legend.title = element_text(size = 14),
        #adjust size of text for legend
        legend.text = element_text(size = 14),
        #margin for the plot
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        strip.background = element_blank(),
        strip.text = element_text(size = 14),
        #set size of the tick marks for y-axis
        axis.ticks.y = element_line(size = 0.5),
        #set size of the tick marks for x-axis
        axis.ticks.x = element_line(size = 0.5),
        #adjust length of the tick marks
        axis.ticks.length = unit(0.2,"cm"),
        #set size and location of the tick labels for the y axis
        axis.text.y = element_text(colour = "black", size = 10, angle = 0, vjust = 0.5, hjust = 1,
                                   margin = margin(t = 0, r = 5, b = 0, l = 0)),
        #set size and location of the tick labels for the x axis
        axis.text.x = element_blank(),
        #set the axis size, color, and end shape
        axis.line = element_line(colour = "black", size = 0.5, lineend = "square")) 

range.plot

ggsave(range.plot, file = 'figs/marine_range.png', width = 17, height = 15, units = "cm", dpi = 300)
