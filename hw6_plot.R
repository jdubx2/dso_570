
library(ggplot2)

ggplot(data.frame(x=c(0,500)), aes(x))+
  stat_function(fun = function(x) 500-(2*x), geom = 'line')+
  stat_function(fun = function(x) (800 - 2*x)/3, geom = 'line')+
  geom_vline(xintercept = 220)+
  geom_hline(yintercept = 180) +
  coord_cartesian(ylim = c(0,500), xlim = c(0,500)) +
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  geom_point(aes(x = 175, y = 150), size = 2, color = 'red') +
  geom_segment(aes(x=0,y=0,xend=150,yend=120*150/130), arrow = arrow(length = unit(.3,"cm")), size = 1.1)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        plot.margin = margin(1,1,.5,.5, unit = 'cm'))+
  labs(x = 'Full Sized', y = 'Compact') +
  stat_function(fun= function(x) 510-(2*x), geom = 'line', linetype='longdash', color = 'blue')+
  geom_point(aes(x = 183, y = 144), size = 2, color = 'blue')

