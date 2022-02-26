library(ggplot2)
library(tidyverse)
library(magrittr)
library(dplyr)
library(highcharter)
library(viridis)
library(lubridate)
library(scales)
library("gridExtra")

boston <- read.csv("boston.csv")
miami <- read.csv("miami.csv")
nyc <- read.csv("nyc.csv")
seattle <- read.csv("seattle.csv")

boston <- boston %>% select('Date', 'Max.TemperatureF','Mean.TemperatureF','Min.TemperatureF') %>% rename('MaxTemperatureF'='Max.TemperatureF', 'MeanTemperatureF'='Mean.TemperatureF','MinTemperatureF'='Min.TemperatureF','date_temp'='Date') %>% mutate(date2 = as.Date(ymd(date_temp))) %>% filter(between(date2, as.Date("2015-01-01"),as.Date("2015-12-31")))
miami <- miami %>% select('Date', 'Max.TemperatureF','Mean.TemperatureF','Min.TemperatureF') %>% rename('MaxTemperatureF'='Max.TemperatureF', 'MeanTemperatureF'='Mean.TemperatureF','MinTemperatureF'='Min.TemperatureF','date_temp'='Date') %>% mutate(date2 = as.Date(ymd(date_temp))) %>% filter(between(date2, as.Date("2015-01-01"),as.Date("2015-12-31")))
nyc <- nyc %>% select('Date', 'Max.TemperatureF','Mean.TemperatureF','Min.TemperatureF') %>% rename('MaxTemperatureF'='Max.TemperatureF', 'MeanTemperatureF'='Mean.TemperatureF','MinTemperatureF'='Min.TemperatureF','date_temp'='Date') %>% mutate(date2 = as.Date(ymd(date_temp))) %>% filter(between(date2, as.Date("2015-01-01"),as.Date("2015-12-31")))
seattle <- seattle %>% select('Date', 'Max.TemperatureF','Mean.TemperatureF','Min.TemperatureF') %>% rename('MaxTemperatureF'='Max.TemperatureF', 'MeanTemperatureF'='Mean.TemperatureF','MinTemperatureF'='Min.TemperatureF','date_temp'='Date') %>% mutate(date2 = as.Date(ymd(date_temp))) %>% filter(between(date2, as.Date("2015-01-01"),as.Date("2015-12-31")))

x <- c("Min", "Mean", "Max")
y <- sprintf("{point.%s}", c("MinTemperatureF", "MeanTemperatureF", "MaxTemperatureF"))
tltip <- tooltip_table(x, y)

p1 <- ggplot(
  boston,
  aes(
    date2,
    ymin = MinTemperatureF,
    ymax = MaxTemperatureF,
    color = MeanTemperatureF
  )
) +
  geom_linerange(size = 0.7, alpha = 1) +
  scale_color_gradient2(low="#346A92", high="#ED8357", mid="#F5ECD9", midpoint=50) +
  labs(
    title = "Boston (MA)",
    x = NULL,
    y = NULL,
    color="Temperature"
  )+
  scale_y_continuous(limits = c(0, 100), breaks = seq(0,120,20))+
  theme(panel.background = element_rect(fill="white"),
        panel.grid.major.y = element_line(colour = "#EBEBEB"),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = 0.5, size=8)
        )+
  coord_polar()+
  geom_text(x = 220, y=20, label="20 F", colour = "#C5C5C5", size=3)+
  geom_text(x = 230, y=40, label="40 F", colour = "#C5C5C5", size=3)+
  geom_text(x = 230, y=60, label="60 F", colour = "#C5C5C5", size=3)+
  geom_text(x = 232, y=80, label="80 F", colour = "#C5C5C5", size=3)+
  geom_text(x = 235, y=100, label="100 F", colour = "#C5C5C5", size=3)
#ggsave("boston.pdf", width=3, height=3, unit="in")

p2 <- ggplot(
  miami,
  aes(
    date2,
    ymin = MinTemperatureF,
    ymax = MaxTemperatureF,
    color = MeanTemperatureF
  )
) +
  geom_linerange(size = 0.7, alpha = 1, show.legend = FALSE) +
  scale_color_gradient2(low="#346A92", high="#ED8357", mid="#F5ECD9", midpoint=50) +
  labs(
    title = "Miami (FL)",
    x = NULL,
    y = NULL
  )+
  scale_y_continuous(limits = c(0, 100), breaks = seq(0,120,20))+
  theme(panel.background = element_rect(fill="white"),
        panel.grid.major.y = element_line(colour = "#EBEBEB"),

        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = 0.5, size=8)
  )+
  coord_polar()+
  geom_text(x = 240, y=20, label="20 F", colour = "#C5C5C5", size=3)+
  geom_text(x = 240, y=40, label="40 F", colour = "#C5C5C5", size=3)+
  geom_text(x = 240, y=60, label="60 F", colour = "#C5C5C5", size=3)+
  geom_text(x = 240, y=80, label="80 F", colour = "#C5C5C5", size=3)+
  geom_text(x = 240, y=100, label="100 F", colour = "#C5C5C5", size=3)

#ggsave("miami.pdf", width=3, height=3, unit="in")

p3 <- ggplot(
  nyc,
  aes(
    date2,
    ymin = MinTemperatureF,
    ymax = MaxTemperatureF,
    color = MeanTemperatureF
  )
) +
  geom_linerange(size = 0.7, alpha = 1, show.legend = FALSE) +
  scale_color_gradient2(low="#346A92", high="#ED8357", mid="#F5ECD9", midpoint=50) +
  labs(
    title = "New York City (USA)",
    x = NULL,
    y = NULL
  )+
  scale_y_continuous(limits = c(0, 100), breaks = seq(0,120,20))+
  theme(panel.background = element_rect(fill="white"),
        panel.grid.major.y = element_line(colour = "#EBEBEB"),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = 0.5, size=8)
  )+
  coord_polar()+
  geom_text(x = 220, y=20, label="20 F", colour = "#C5C5C5", size=3)+
  geom_text(x = 230, y=40, label="40 F", colour = "#C5C5C5", size=3)+
  geom_text(x = 230, y=60, label="60 F", colour = "#C5C5C5", size=3)+
  geom_text(x = 232, y=80, label="80 F", colour = "#C5C5C5", size=3)+
  geom_text(x = 235, y=100, label="100 F", colour = "#C5C5C5", size=3)
#ggsave("nyc.pdf", width=3, height=3, unit="in")

p4 <- ggplot(
  seattle,
  aes(
    date2,
    ymin = MinTemperatureF,
    ymax = MaxTemperatureF,
    color = MeanTemperatureF
  )
) +
  geom_linerange(size = 0.7, alpha = 1, show.legend = FALSE) +
  scale_color_gradient2(low="#346A92", high="#ED8357", mid="#F5ECD9", midpoint=50) +
  labs(
    title = "Seattle (WA)",
    x = NULL,
    y = NULL
  )+
  scale_y_continuous(limits = c(0, 100), breaks = seq(0,120,20))+
  theme(panel.background = element_rect(fill="white"),
        panel.grid.major.y = element_line(colour = "#EBEBEB"),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = 0.5, size=8)
  )+
  coord_polar()+
  geom_text(x = 220, y=20, label="20 F", colour = "#C5C5C5", size=3)+
  geom_text(x = 230, y=40, label="40 F", colour = "#C5C5C5", size=3)+
  geom_text(x = 230, y=60, label="60 F", colour = "#C5C5C5", size=3)+
  geom_text(x = 232, y=80, label="80 F", colour = "#C5C5C5", size=3)+
  geom_text(x = 235, y=100, label="100 F", colour = "#C5C5C5", size=3)
#ggsave("seattle.pdf", width=3, height=3, unit="in")

g_legend<-function(a){
  tmp <- ggplot_gtable(ggplot_build(a))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(p1)

p5 <- grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),p2, p3,p4, nrow=2), mylegend, ncol=2, widths = c(20,5))
ggsave("result_assignment_q1.pdf", plot = p5)
