coho.fig.20 <- coho.fig.long[coho.fig.long$year == '2020',]
coho.fig.21 <- coho.fig.long[coho.fig.long$year == '2021',]

coho.fig.20.week <- coho.fig.20 %>% 
  mutate(stat.week = cut(julian.day, breaks = c(0, 214, 215, 221, 222, 235,236,242),
                         labels = c('32', '33', '33', '34', '34',
                                    '35', '35')))


coho.fig.21.week <- coho.fig.21 %>% 
  mutate(stat.week = cut(julian.day, breaks = c(0, 219, 220, 226, 227, 233, 234,
                                                Inf),
                         labels = c('32', '33', '33', '34', '34',
                                    '35', '35')))

coho.fig.week <- rbind(coho.fig.20.week, coho.fig.21.week)
plot(x = coho.fig.week$stat.week, y = coho.fig.week$abundance)


fig.part <- ggplot(coho.fig.week, aes(x = as.factor(stat.week), y = value, fill = Stock)) +
  geom_col() +
  facet_grid(capture.loc~year, scales = 'free_x', labeller = as_labeller(sd)) +
  xlab("Day of year of landing") +
  ylab("Coho Salmon landed") +
  scale_fill_manual(values = col.pal) +
  #set the limits and tick breaks for the y-axis
  scale_y_continuous(expand = c(0,0)) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  #adjust the order of the legend, make new labels, and select the symbol colors
  #makes the figure background white without grid lines
  theme_classic() +
  theme (axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
         axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
         #set the font type
         text = element_text(family = "Arial"),
         #modify plot title, the B in this case
         plot.title = element_text(face = "bold", family = "Times New Roman"),
         #position the legend on the figure
         
         #adjust size of text for legend
         
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
         axis.line = element_line(colour = "black", size = 0.5, lineend = "square"),
         panel.spacing.y = unit(1, 'lines'))

fig.part

# Total prop by year
coho.fig.skk <- coho.fig.week[coho.fig.week$capture.loc == '5',]

fig.part2 <- ggplot(coho.fig.skk, aes(x = as.factor(stat.week), y = value, fill = Stock)) +
  geom_col(position = 'fill') +
  facet_wrap(~year) +
  labs(title = 'Shaktoolik subdistrict landings') +
  xlab("Week of year") +
  ylab("Stock proportion") +
  scale_fill_manual(values = col.pal) +
  #set the limits and tick breaks for the y-axis
  scale_y_continuous(expand = c(0,0)) +
  scale_x_discrete() +
  #adjust the order of the legend, make new labels, and select the symbol colors
  #makes the figure background white without grid lines
  theme_classic() +
  scale_y_continuous (expand = c(0,0)) +
  #adjust the order of the legend, make new labels, and select the symbol colors
  #makes the figure background white without grid lines
  theme (plot.title = element_text(hjust = 0.5),
    axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
         axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
         #set the font type
         text = element_text(),
         #modify plot title, the B in this case
         #plot.title = element_text(face = "bold"),
         #position the legend on the figure
         legend.position = 'blank',
         #adjust size of text for legend
         legend.text = element_text(size = 10),
         legend.title = element_text(size = 14),
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


fig.part2

ggsave(fig.part2, file = "figs/partition_week_skk.png", width = 20, height = 7, units = "cm", dpi = 300)

# Total prop by year
coho.fig.unk <- coho.fig.week[coho.fig.week$capture.loc == '6',]

fig.part2 <- ggplot(coho.fig.unk, aes(x = as.factor(stat.week), y = value, fill = Stock)) +
  geom_col(position = 'fill') +
  facet_wrap(~year) +
  labs(title = 'Unalakleet subdistrict landings') +
  xlab("Week of year") +
  ylab("Stock proportion") +
  scale_fill_manual(values = col.pal) +
  #set the limits and tick breaks for the y-axis
  scale_y_continuous(expand = c(0,0)) +
  scale_x_discrete() +
  #adjust the order of the legend, make new labels, and select the symbol colors
  #makes the figure background white without grid lines
  theme_classic() +
  scale_y_continuous (expand = c(0,0)) +
  #adjust the order of the legend, make new labels, and select the symbol colors
  #makes the figure background white without grid lines
  theme (plot.title = element_text(hjust = 0.5),
    axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
         axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
         #set the font type
         text = element_text(),
         #modify plot title, the B in this case
         #plot.title = element_text(face = "bold"),
         #position the legend on the figure
         legend.position = 'blank',
         #adjust size of text for legend
         legend.text = element_text(size = 10),
         legend.title = element_text(size = 14),
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


fig.part2

ggsave(fig.part2, file = "figs/partition_week_unk.png", width = 20, height = 7, units = "cm", dpi = 300)
