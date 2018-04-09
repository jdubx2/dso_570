setwd("~/Repos/dso_570/final_project")
library(dplyr)
library(ggplot2)
library(tidyr)
library(readxl)
library(extrafont)
library(stringr)

usage <- read.csv('room_usage.csv', stringsAsFactors = F)
capacity <- read_excel('Marshall_Room_Capacity_Chart.xlsx')

enroll <- read_excel("Marshall_Course_Enrollment_1516_1617.xlsx")
names(enroll) <- str_replace_all(names(enroll),' ','_')


temp <- enroll %>% filter(Term == 20171) %>% 
  mutate(class_length = First_End_Time - First_Begin_Time) %>% 
  group_by(class_length) %>% summarise(n = n()) %>% arrange(desc(n))
