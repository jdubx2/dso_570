setwd("~/Repos/dso_570/final_project")
library(dplyr)
library(ggplot2)
library(tidyr)
library(readxl)
library(extrafont)

usage <- read.csv('room_usage.csv', stringsAsFactors = F)
capacity <- read_excel('Marshall_Room_Capacity_Chart.xlsx')

usage <- usage %>% 
  left_join(capacity, by = 'Room') %>% 
  mutate(Size = as.numeric(Size),
         Day = factor(Day, levels = c('M','T','W','H','F')))

plot <- usage %>% group_by(Term, Day, Time) %>% 
  summarise(util = sum(Registered)/sum(Size)) %>% 
  ggplot(aes(x = Day, y = -Time))+
    geom_tile(aes(fill = util), color = NA)+
    scale_y_continuous(breaks = seq(-480,-1320,-120), 
                       labels = c('8am', '10am', '12pm', '2pm', '4pm', '6pm', '8pm', '10pm'),
                       expand = c(0,0)) +
    facet_wrap(~Term) +
    scale_fill_gradientn(colors = c('#ffffd9','#edf8b1','#c7e9b4','#7fcdbb','#41b6c4','#1d91c0','#225ea8','#253494','#081d58'),
                        limits = c(0,.75), breaks = seq(0,.7,.1),
                        labels = function(x) paste0(x*100,'%')) +
    theme_minimal() +
    theme(text = element_text(family = 'Calibri Light', size = 19),
          plot.title = element_text(hjust = .5, size = 29),
          plot.subtitle = element_text(hjust = .5),
          strip.background = element_rect(fill = 'gray25', color = 'white'),
          strip.text = element_text(color = 'white'),
          axis.ticks = element_line(color='gray20'))+
    labs(y = '', fill = '', title = 'Classroom Utilization', subtitle = 'Enrollment / Classroom Capacity')+
    guides(fill = guide_colorbar(barheight = 23))

ggsave(file="util.svg", plot=plot, width=7, height=6.5, bg = 'transparent')
