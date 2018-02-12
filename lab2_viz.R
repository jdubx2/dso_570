library(dplyr)
library(ggplot2)
library(extrafont)
library(tidyr)

rm1 <- read.csv('rm1_data.csv')

rm1_plot <- rm1 %>% 
  filter(rev>=-3250) %>% 
  ggplot(aes(x = price1, y = price2))+
  geom_tile(aes(fill=rev)) +
  # geom_hline(yintercept=310, linetype='dashed', size=.5, color = 'black')+
  # geom_vline(xintercept=235, linetype='dashed', size=.5, color='black') +
  scale_fill_gradient2(low='#a50026', mid='#fee090', high='#313695', midpoint=0, 
                       labels = function(x) ifelse(x !=0, paste0('$ ',x/1000,'k'),x))+
  scale_x_continuous(labels = function(x) paste0('$',x)) +
  scale_y_continuous(labels = function(x) paste0('$',x)) +
  theme_minimal()+
  theme(text = element_text(family='Calibri', size = 16),
        panel.grid = element_blank(),
        axis.line = element_line(),
        axis.ticks = element_line(),
        plot.title = element_text(hjust = .5, size = 16)) +
  guides(fill = guide_colorbar(barheight=10))+
  labs(x = 'Price > 14 Days', y = 'Price <= 14 Days', title = 'Optimal Pricing for Day 14 Increase', fill='Profit')

ggsave(file="rm_1.svg", plot=rm1_plot, width=5.3, height=3.5, bg = 'transparent')

base <- read.csv('base.csv')

base_plot <- base %>% 
  filter(prices>175) %>% 
  ggplot(aes(x = prices, y = revs))+
  geom_smooth(size = 1, color = 'dodgerblue')+
  #geom_point(aes(x = 245, y = 2454.25), color = 'red', size = 2.5)+
  scale_x_continuous(labels = function(x) paste0('$',x)) +
  scale_y_continuous(labels = function(x) ifelse(x !=0, paste0('$ ',x/1000,'k'),x),
                     limits = c(-2000,3000),
                     breaks = seq(-2000,3000,1000)) +
  theme_minimal()+
  theme(text = element_text(family='Calibri', size = 16),
        panel.grid = element_blank(),
        axis.line = element_line(),
        axis.ticks = element_line(),
        plot.title = element_text(hjust = .5))+
  labs(x = 'Price', y = 'Profit', title = 'Revenue From Constant Pricing')

ggsave(file="base.svg", plot=base_plot, width=5, height=3.5, bg = 'transparent')

rm2 <- read.csv('rm2_data.csv')

rm2_plot2 <- rm2 %>% 
  group_by(quantity) %>% 
  arrange(desc(rev)) %>% 
  slice(1) %>% 
  ggplot(aes(x = quantity, y = rev))+
  geom_smooth(color = 'coral3')+
  scale_x_continuous(labels = function(x) x) +
  scale_y_continuous(labels = function(x) ifelse(x !=0, paste0('$ ',x/1000,'k'),x),
                      limits = c(0,4000))+
                     # breaks = seq(-2000,3000,1000)
  theme_minimal()+
  theme(text = element_text(family='Calibri', size = 16),
        panel.grid = element_blank(),
        axis.line = element_line(),
        axis.ticks = element_line(),
        plot.title = element_text(hjust = .5, size = 16))+
  labs(x = 'Quantity Sold Before Price Increase', y = 'Profit', title = 'Quantity Cutoffs \nat Optimal Pricing Combinations')

ggsave(file="rm2_2.svg", plot=rm2_plot2, width=4, height=3.5, bg = 'transparent')

rm2_plot <- rm2 %>% 
  group_by(price1,price2) %>% 
  arrange(desc(rev)) %>% 
  slice(1) %>% 
  ggplot(aes(x = price1, y =price2))+
  geom_tile(aes(fill=rev)) +
  # geom_hline(yintercept=310, linetype='dashed', size=.5, color = 'black')+
  # geom_vline(xintercept=235, linetype='dashed', size=.5, color='black') +
  scale_fill_gradient(low='#edf8b1', high='#0c2c84', 
                       labels = function(x) ifelse(x !=0, paste0('$ ',x/1000,'k'),x))+
  scale_x_continuous(labels = function(x) paste0('$',x),
                     breaks = seq(225,245,5)) +
  scale_y_continuous(labels = function(x) paste0('$',x)) +
  theme_minimal()+
  theme(text = element_text(family='Calibri', size = 15),
        panel.grid = element_blank(),
        axis.line = element_line(),
        axis.ticks = element_line(),
        plot.title = element_text(hjust = .5, size = 15),
        plot.subtitle = element_text(hjust = .5, size = 11)) +
  guides(fill = guide_colorbar(barheight=10))+
  labs(x = 'Price Before N Tickets Sold', y = 'Price After N Tickets Sold', title = 'Pricing Combinations \n at Optimal Quantity Cutoffs', fill='Profit')

ggsave(file="rm2.svg", plot=rm2_plot, width=5, height=3.5, bg = 'transparent')


sub_rm1 <- rm1 %>% select(price = price1, rev) %>% group_by(price) %>% summarise(rev = max(rev))
sub_rm2 <- rm2 %>% select(price = price1, rev) %>% group_by(price) %>% summarise(rev = max(rev))

# final_df <- bind_rows(
#   mutate(select(base, price = prices, rev = revs), Scenario = 'Current'),
#   mutate(sub_rm1, Scenario = 'RM1 (14 Days)'),
#   mutate(sub_rm2, Scenario = 'RM2 (N Tickets Sold)')
# )
# 
# 
# final_df %>% 
#   filter(rev > 2000) %>% 
#   ggplot(aes(x=price, y = rev, color=Scenario))+
#   geom_smooth()
library(scales)

sim <- read.csv('sim.csv')
names(sim) <- gsub('\\.',' ',names(sim))

sim_plot <- sim %>% 
  select(-X) %>% 
  gather(scenario, rev) %>% 
  mutate(scenario = factor(scenario, 
                           levels = rev(c('Increase After 59 Tickets','Increase at 14 Days Out', 'Constant Price')))) %>% 
  ggplot(aes( x = scenario, y = rev, fill = scenario))+
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = c('dodgerblue', '#66c2a5', 'coral3'))+
  scale_y_continuous(labels= function(x) paste0('$ ',x/1000,'k'),
                     breaks = seq(-10000,7500,2500),
                     limits = c(-10500,10000))+
  theme_minimal()+
  coord_flip()+
  theme(text = element_text(family='Calibri', size = 15),
        panel.grid = element_blank(),
        # panel.grid.major.x = element_line(color = 'gray90', size = .1),
        axis.line = element_line(),
        axis.ticks = element_line(),
        plot.title = element_text(hjust = .5, size = 16),
        plot.subtitle = element_text(hjust = .5, size = 11),
        legend.position = c(.3,.6)) +
  guides(fill = F) +
  labs(title = 'Estimated Profit Distriubtions', subtitle = '500 Booking Simulations at Optimal Pricing', y = 'Profit', x = '')
# facet_grid(scenario~.)

ggsave(file="sim.svg", plot=sim_plot, width=7.3, height=3.5, bg = 'transparent')
