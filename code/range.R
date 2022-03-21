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
library(ggforce)
library(mgcv)
library(visreg)

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

# Try with the mystery data ####

riv24 <- read.csv('data/range/marine_range_24.csv')
riv71 <- read.csv('data/range/marine_range_71.csv', skip = 45)

marx2 <- rbind(riv24, riv71)

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

koy.mod <- gam(prop ~ s(dist), data = koy)
pred <- predict(koy.mod, se = T)
koy$pred <- pred[['fit']]
koy$se <- pred[['se.fit']]

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

unk.mod <- gam(prop ~ s(dist), data = unk)
pred <- predict(unk.mod, se = T)
unk$pred <- pred[['fit']]
unk$se <- pred[['se.fit']]

ggsave(unk.plot, file = 'figs/unk_range_gam.png', width = 12, height = 10, units = "cm", dpi = 300)

koy.plot <- ggplot(data = koy, aes(x = dist, y = prop)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = F) +
  xlab('Distance (m)') +
  ylab('Proportion of transmissions detected') +
  scale_x_continuous(limits = c(0, 400), expand = c(0,0)) +
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
  ylab('Detection proportion') +
  scale_x_continuous(limits = c(50, 1000), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0,0)) +
  theme_bw() +
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

marine2$pred[marine2$pred < 0.00] <- 0.00


marine4 <- marine2 %>% 
  mutate(bins = cut(pred, breaks = seq.int(0,1, by=1/8), include.lowest = T))

# Polar coord range plot ####
my.blues <- c("#C6DBEF", "#6BAED6", "#4292C6", "#2171B5", "#08519C") 
my.reds <- c("#FEB24C", "#FC4E2A", "#E31A1C", "#BD0026", "#800026")

range.plot <- ggplot(marine3, aes(x = dist, color = bins, fill = bins)) +
  geom_bar() +
  xlab('Distance from receiver (m)') +
  ylab('') +
  scale_x_continuous(limits = c(0, 1000), expand = c(0,0)) +
  scale_fill_manual(values = my.blues, name = 'Detection\nproportion') +
  scale_color_manual(values = my.blues, guide = 'none') +
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

marine4$r0 <- marine3$dist - 25

rangev2 <- ggplot(marine4) +
  geom_point(aes(x = 0, y = 0)) +
  stat_arc_bar(aes(x0 = 0, y0 = 0, r0 = r0, r = dist, 
                   fill = pred, start = 0, end = 2*pi), 
               color = NA,
               alpha = 0.75) +
  coord_fixed() +
  scale_fill_distiller(breaks = c(.25, .5, .75), labels = c('0.25', '0.50', '0.75'),
                       direction = 1, name = 'Detection\nproportion') +
  xlab('') +
  ylab('Distance from receiver (m)') +
  scale_x_continuous(limits = c(-1000, 1000), expand = c(0,0),
                     breaks = seq(-1000, 1000, by = 250)) +
  scale_y_continuous(limits = c(-1000, 1000), expand = c(0,0),
                     breaks = seq(-1000, 1000, by = 250),
                     labels = c('', '', '', '', '0', '250', '500', '750', '1000')) +
  theme_bw() +
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
        axis.ticks.x = element_blank(),
        #adjust length of the tick marks
        axis.ticks.length = unit(0.2,"cm"),
        #set size and location of the tick labels for the y axis
        axis.text.y = element_text(colour = "black", size = 10, angle = 0, vjust = 0.5, hjust = 1,
                                   margin = margin(t = 0, r = 5, b = 0, l = 0)),
        #set size and location of the tick labels for the x axis
        axis.text.x = element_blank(),
        #set the axis size, color, and end shape
        axis.line = element_line(colour = "black", size = 0.5, lineend = "square")) 

rangev2
 
ggsave(rangev2, file = 'figs/marine_range_v4.png', width = 17, height = 15, units = "cm", dpi = 300)

# Visreg ####

koy.gam <- visreg(koy.mod, xvar = 'dist', band = F, gg = T) + 
  geom_point() + 
  xlab('Distance (m)') +
  ylab('Detection proportion') +
  scale_x_continuous(limits = c(0, 400), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0,0)) +
  theme_bw() +
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


koy.gam
ggsave(koy.gam, file = 'figs/koy_gam_v3.png', width = 17, height = 15, units = "cm", dpi = 300)


unk.gam <- visreg(unk.mod, xvar = 'dist', band = F, gg = T) + 
  geom_point() + 
  xlab('Distance (m)') +
  ylab('Detection proportion') +
  scale_x_continuous(limits = c(0, 400), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0,0)) +
  theme_bw() +
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
unk.gam
ggsave(unk.gam, file = 'figs/unk_gam_v2.png', width = 17, height = 15, units = "cm", dpi = 300)

# Scratch ####

marine4 <- rbind(marine3, marine3,marine3,marine3,marine3,marine3)
marine4$rec <- c(rep('1', length = 39), rep('2', length = 39),rep('3', length = 39),
                     rep('4', length = 39),rep('5', length = 39),rep('6', length = 39))
marine4$shore <- c(rep(300, length = 39),rep(600, length = 39),rep(1100, length = 39),
                   rep(1600, length = 39),rep(2100, length = 39),rep(2600, length = 39))
marine4$dist.shore <- marine4$dist + marine4$shore

df20 <- data.frame(x = c(300, 600, 900, 1200, 1500, 1800), y = 0, r = 625,
                   r0 = 325, array = 'Original configuration')
df20.1 <- data.frame(x = c(600, 1200, 1500, 1800), y = 0, r = 625,
                     r0 = 325, array = 'Bald Head')

df20.3 <- data.frame(x = c(300, 900, 1800), y = 0, r = 625,
                     r0 = 325, array = 'Cape Denbeigh')

df20.tot <- rbind(df20, df20.1, df20.3)

ggplot(df20.tot) +
  geom_point(aes(x = x, y = y))+
  geom_arc_bar(aes(x0 = x, y0 = y, r0 = r0, r = r, 
                   amount = 1), 
               stat = 'pie',
               color = NA,
               fill = 'blue',
               alpha = 0.3) +
  geom_vline(xintercept = 0, linetype = 'dashed') + 
  scale_x_continuous(expand = c(0,0)) +
  coord_fixed() +
  theme_bw()+
  theme(legend.position = "none")

df21 <- data.frame(x = c(300, 600, 1100, 1600, 2100, 2600), y = 0, r = 625,
                   r0 = 325)
df21.1 <- data.frame(x = c(600, 1100, 2100), y = 1300, r = 625,
                 r0 = 325)
df21.2 <- data.frame(x = c(300, 600, 1600, 2100), y = 2600, r = 625,
                     r0 = 325)
df21.3 <- data.frame(x = c(600, 2100), y = 3900, r = 625,
                     r0 = 325)
df21.4 <- data.frame(x = c(600, 1100, 1600, 2600), y = 5200, r = 625,
                     r0 = 325)
df21.5 <- data.frame(x = c(300, 600, 1100, 1600, 2600), y = 6500, r = 625,
                     r0 = 325)

df21.tot <- rbind(df21, df21.1, df21.2, df21.3, df21.4, df21.5)

addline_format <- function(x,...){
  gsub('\\s','\n',x)
}

range.21 <- ggplot(df21.tot) +
  geom_point(aes(x = x, y = y * -1))+
  geom_arc_bar(aes(x0 = x, y0 = y * -1, r0 = r0, r = r, 
                   amount = 1), 
               stat = 'pie',
               color = NA,
               fill = 'blue',
               alpha = 0.3) +
  geom_vline(xintercept = 0, linetype = 'dashed') +
  ylab('') +
  xlab('Distance from shore (m)') +
  scale_x_continuous(expand = c(0,1), 
                     breaks = c(0, 300, 600, 1100, 1600, 2100, 2600, 3225),
                     labels = c(0, 300, 600, 1100, 1600, 2100, 2600, 3225)) +
  scale_y_continuous(expand = c(0,0),
                     breaks = unique(df21.tot$y) * -1,
                     labels = addline_format(c('Original configuration', 'Bald Head',
                                'Point Dexter', 'Cape Denbeigh',
                                'Junction Creek', 'Blueberry Creek'))) +
  coord_fixed(xlim = c(0,3225), expand = F, clip = 'on') +
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
      #set the font type
      text = element_text(),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
      #set size of the tick marks for x-axis
      axis.ticks.x = element_line(size = 0.5),
      #adjust length of the tick marks
      axis.ticks.length = unit(0.2,"cm"),
      #set size and location of the tick labels for the y axis
      axis.text.y = element_text(colour = "black", size = 12, angle = 0, vjust = 0.5, hjust = 1,
                                 margin = margin(t = 0, r = 5, b = 0, l = 0)),
      #set size and location of the tick labels for the x axis
      axis.text.x = element_text(colour = "black", size = 12, angle = 0, vjust = 0.5, hjust = 0.5,
                                margin = margin(t = 0, r = 5, b = 0, l = 0)),
      #set the axis size, color, and end shape
      axis.line = element_line(colour = "black", size = 0.5, lineend = "square")) 

range.21

ggsave(range.21, file = 'figs/range_21.png', width = 15, height = 27, units = "cm", dpi = 300)


range.20 <- ggplot(df20.tot) +
  geom_point(aes(x = x, y = y * -1))+
  geom_arc_bar(aes(x0 = x, y0 = y * -1, r0 = r0, r = r, 
                   amount = 1), 
               stat = 'pie',
               color = NA,
               fill = 'blue',
               alpha = 0.3) +
  geom_vline(xintercept = 0, linetype = 'dashed') +
  ylab('') +
  xlab('Distance from shore (m)') +
  scale_x_continuous(expand = c(0,1), 
                     breaks = c(0, 300, 600, 900, 1200, 1500, 1800, 2425),
                     labels = c(0, 300, 600, 900, 1200, 1500, 1800, 2425)) +
  scale_y_continuous(expand = c(0,0),
                     breaks = unique(df20.tot$y) * -1,
                     labels = addline_format(c('Original configuration', 'Bald Head','Cape Denbeigh'))) +
  coord_fixed(xlim = c(0,2425), expand = F, clip = 'on') +
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
        #set the font type
        text = element_text(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        #set size of the tick marks for x-axis
        axis.ticks.x = element_line(size = 0.5),
        #adjust length of the tick marks
        axis.ticks.length = unit(0.2,"cm"),
        #set size and location of the tick labels for the y axis
        axis.text.y = element_text(colour = "black", size = 12, angle = 0, vjust = 0.5, hjust = 1,
                                   margin = margin(t = 0, r = 5, b = 0, l = 0)),
        #set size and location of the tick labels for the x axis
        axis.text.x = element_text(colour = "black", size = 12, angle = 0, vjust = 0.5, hjust = 0.5,
                                   margin = margin(t = 0, r = 5, b = 0, l = 0)),
        #set the axis size, color, and end shape
        axis.line = element_line(colour = "black", size = 0.5, lineend = "square")) 

range.20

ggsave(range.20, file = 'figs/range_20.png', width = 14, height = 18, units = "cm", dpi = 300)


marine3$r0 <- marine3$dist - 25

df20.range <- df20.tot[rep(seq_len(nrow(df20.tot)), each = nrow(marine4)), ]
test.results <- do.call("rbind", replicate(nrow(df20.tot), marine4, simplify = FALSE))

range20 <- cbind(df20.range, test.results)
range20 <- range20[,-4]
range20.2 <- subset(range20, pred > 0.5)


ggplot(data = range20.2) +
  geom_point(aes(x = x, y = y)) +
  stat_arc_bar(aes(x0 = x, y0 = y, r0 = r0, r = dist, 
                   fill = pred, start = 0, end = 2*pi), 
               color = NA,
               alpha = 0.5) +
  facet_wrap(~array, nrow = 3) +
  scale_fill_distiller(direction = 1) +
  ylab('') +
  xlab('Distance from shore (m)') +
  scale_x_continuous(limits = c(0,2425), expand = c(0,0), 
                     breaks = c(0, 300, 600, 900, 1200, 1500, 1800, 2425),
                     labels = c(0, 300, 600, 900, 1200, 1500, 1800, 2425)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_fixed(xlim = c(0,2800), expand = F, clip = 'on') +
  theme_bw()+
  theme(axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
        #set the font type
        text = element_text(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        #set size of the tick marks for x-axis
        axis.ticks.x = element_line(size = 0.5),
        #adjust length of the tick marks
        axis.ticks.length = unit(0.2,"cm"),
        #set size and location of the tick labels for the y axis
        axis.text.y = element_text(colour = "black", size = 12, angle = 0, vjust = 0.5, hjust = 1,
                                   margin = margin(t = 0, r = 5, b = 0, l = 0)),
        #set size and location of the tick labels for the x axis
        axis.text.x = element_text(colour = "black", size = 12, angle = 0, vjust = 0.5, hjust = 0.5,
                                   margin = margin(t = 0, r = 5, b = 0, l = 0)),
        #set the axis size, color, and end shape
        axis.line = element_line(colour = "black", size = 0.5, lineend = "square")) 

