#Read in dataset
#install.packages("readxl")
library(readxl)
library(ggplot2)
library(cowplot)
library(ggpubr)
library(tidyverse)
library(gridExtra)
library(plyr)
library(dplyr)
library(reshape2)
library(data.table)
library(scales)
library(mosaic)
library(grid)
library(gganimate)
library(av)
library(xlsx)
#####################################################################################################################
#                                               LOAD THE DATA                                                       #
#####################################################################################################################
#Load the micro costing summaries
SA_quadrant_data <- readxl::read_xlsx("SA Incremental cost and effectiveness.xlsx", sheet = "data")
Brazil_quadrant_data <- readxl::read_xlsx("Brazil Incremental cost and effectiveness.xlsx", sheet = "data")



#####################################################################################################################
#                                               South Africa                                                       #
#####################################################################################################################

# 1. 6H vs status quo
SA_quadrant1<-SA_quadrant_data %>%
  filter(comparison =="A. 6H vs status quo")

SA_color<-SA_quadrant_data

SA_CE_1<-SA_quadrant1 %>% 
  mutate(quadrant = case_when(incremental_effectiveness > 0 & incremental_cost > 0   ~ "More expensive and more effective",
                              incremental_effectiveness <= 0 & incremental_cost > 0  ~ "More expensive and less effective",
                              incremental_effectiveness <= 0 & incremental_cost <= 0 ~ "Less costly and less effective",
                              TRUE
                              ~ "Less costly and more effective")) %>% 
  ggplot(aes(x = incremental_effectiveness,Effectiveness, y = incremental_cost, color=quadrant ))+
  scale_colour_manual(values = c('More expensive and more effective'="deepskyblue", 'More expensive and less effective'="red", 'Less costly and less effective'="orange", 'Less costly and more effective'= "darkgreen"))+
  geom_point(size = 0.05, stroke = 0, shape = 16)+
  geom_vline(xintercept = 0, linetype="dotted") + # plot vertical line
  geom_hline(yintercept = 0, linetype="dotted") + # plot horizontal line
  scale_y_continuous(labels = unit_format( unit = "M", scale = 1e-6))+
  scale_x_continuous(labels = unit_format( unit = "", scale = 1e-3))+
  facet_wrap(~Effectiveness, dir="h", scales="free_x", ncol = 3) +
  theme_classic()+
#  geom_abline(slope=3520, intercept=0, col='red') +
    geom_abline(data =  subset(SA_color, Effectiveness =="1. DALYs averted"), aes(slope=3520, intercept=0), col='red') +
  #  geom_abline(slope=18120, intercept=0, col='purple') +
  #  annotate("text", x = 200000, y = 800000000, label = "WTP - GDP", size=2, col="red")+
#  annotate("text", x = 200000, y = 1600000000, label = "WTP - GDP*3", size=2, col="purple")+
#  xlab("incremental effectiveness (In Thousands)") +
  ylab("") +
  xlab("") +
  labs(color='',title = "A. 6H vs status quo")  +
  theme(legend.position="")+
  geom_point()+
  ggsave("1. SA_CE planes 6H vs status quo.jpeg", 
         width = 25, height = 8, units = "cm")

#2. Minimal vs status quo
SA_quadrant2<-SA_quadrant_data %>%
  filter(comparison =="B. Minimal vs status quo")

SA_CE_2<-SA_quadrant2 %>% 
  mutate(quadrant = case_when(incremental_effectiveness > 0 & incremental_cost > 0   ~ "More expensive and more effective",
                              incremental_effectiveness <= 0 & incremental_cost > 0  ~ "More expensive and less effective",
                              incremental_effectiveness <= 0 & incremental_cost <= 0 ~ "Less costly and less effective",
                              TRUE
                              ~ "Less costly and more effective")) %>% 
  ggplot(aes(x = incremental_effectiveness,Effectiveness, y = incremental_cost, color=quadrant ))+
  scale_colour_manual(values = c('More expensive and more effective'="deepskyblue", 'More expensive and less effective'="red", 'Less costly and less effective'="orange", 'Less costly and more effective'= "darkgreen"))+
  geom_point(size = 0.05, stroke = 0, shape = 16)+
  geom_vline(xintercept = 0, linetype="dotted") + # plot vertical line
  geom_hline(yintercept = 0, linetype="dotted") + # plot horizontal line
  scale_y_continuous(labels = unit_format( unit = "M", scale = 1e-6))+
  scale_x_continuous(labels = unit_format( unit = "", scale = 1e-3))+
  facet_wrap(~Effectiveness, dir="h", scales="free_x", ncol = 3) +
  theme_classic()+
  geom_abline(data =  subset(SA_color, Effectiveness =="1. DALYs averted"), aes(slope=3520, intercept=0), col='red') +
  #  geom_abline(slope=18120, intercept=0, col='purple') +
  #  annotate("text", x = 200000, y = 800000000, label = "WTP - GDP", size=2, col="red")+
  #  annotate("text", x = 200000, y = 1600000000, label = "WTP - GDP*3", size=2, col="purple")+
  #  xlab("incremental effectiveness (In Thousands)") +
  ylab("") +
  xlab("") +
  labs(color='', title = "B. Minimal vs status quo")  +
  theme(legend.position="")+
  geom_point()+
  ggsave("2. SA_CE planes Minimal vs status quo.jpeg", 
         width = 25, height = 8, units = "cm")

#3. Minimal vs 6H
SA_quadrant3<-SA_quadrant_data %>%
  filter(comparison =="C. Minimal vs 6H")

SA_CE_3<-SA_quadrant3 %>% 
  mutate(quadrant = case_when(incremental_effectiveness > 0 & incremental_cost > 0   ~ "More expensive and more effective",
                              incremental_effectiveness <= 0 & incremental_cost > 0  ~ "More expensive and less effective",
                              incremental_effectiveness <= 0 & incremental_cost <= 0 ~ "Less costly and less effective",
                              TRUE
                              ~ "Less costly and more effective")) %>% 
  ggplot(aes(x = incremental_effectiveness,Effectiveness, y = incremental_cost, color=quadrant ))+
  scale_colour_manual(values = c('More expensive and more effective'="deepskyblue", 'More expensive and less effective'="red", 'Less costly and less effective'="orange", 'Less costly and more effective'= "darkgreen"))+
  geom_point(size = 0.05, stroke = 0, shape = 16)+
  geom_vline(xintercept = 0, linetype="dotted") + # plot vertical line
  geom_hline(yintercept = 0, linetype="dotted") + # plot horizontal line
  scale_y_continuous(labels = unit_format( unit = "M", scale = 1e-6))+
  scale_x_continuous(labels = unit_format( unit = "", scale = 1e-3))+
  facet_wrap(~Effectiveness, dir="h", scales="free_x", ncol = 3) +
  theme_classic()+
  geom_abline(data =  subset(SA_color, Effectiveness =="1. DALYs averted"), aes(slope=3520, intercept=0), col='red') +
  #  geom_abline(slope=18120, intercept=0, col='purple') +
  #  annotate("text", x = 200000, y = 800000000, label = "WTP - GDP", size=2, col="red")+
  #  annotate("text", x = 200000, y = 1600000000, label = "WTP - GDP*3", size=2, col="purple")+
  #  xlab("incremental effectiveness (In Thousands)") +
  ylab("Incremental cost (In millions US$)") +
  xlab("") +
  labs(color='', title = "C. Minimal vs 6H")  +
  theme(legend.position="")+
  geom_point()+
  ggsave("3. SA_CE planes Minimal vs 6H.jpeg", 
         width = 25, height = 8, units = "cm")

#4. Optimal vs minimal
SA_quadrant4<-SA_quadrant_data %>%
  filter(comparison =="D. Optimal vs minimal")

SA_CE_4<-SA_quadrant4 %>% 
  mutate(quadrant = case_when(incremental_effectiveness > 0 & incremental_cost > 0   ~ "More expensive and more effective",
                              incremental_effectiveness <= 0 & incremental_cost > 0  ~ "More expensive and less effective",
                              incremental_effectiveness <= 0 & incremental_cost <= 0 ~ "Less costly and less effective",
                              TRUE
                              ~ "Less costly and more effective")) %>% 
  ggplot(aes(x = incremental_effectiveness,Effectiveness, y = incremental_cost, color=quadrant ))+
  scale_colour_manual(values = c('More expensive and more effective'="deepskyblue", 'More expensive and less effective'="red", 'Less costly and less effective'="orange", 'Less costly and more effective'= "darkgreen"))+
  geom_point(size = 0.05, stroke = 0, shape = 16)+
  geom_vline(xintercept = 0, linetype="dotted") + # plot vertical line
  geom_hline(yintercept = 0, linetype="dotted") + # plot horizontal line
  scale_y_continuous(labels = unit_format( unit = "M", scale = 1e-6))+
  scale_x_continuous(labels = unit_format( unit = "", scale = 1e-3))+
  facet_wrap(~Effectiveness, dir="h", scales="free_x", ncol = 3) +
  theme_classic()+
  geom_abline(data =  subset(SA_color, Effectiveness =="1. DALYs averted"), aes(slope=3520, intercept=0), col='red') +
  #  geom_abline(slope=18120, intercept=0, col='purple') +
  #  annotate("text", x = 200000, y = 800000000, label = "WTP - GDP", size=2, col="red")+
  #  annotate("text", x = 200000, y = 1600000000, label = "WTP - GDP*3", size=2, col="purple")+
  #  xlab("incremental effectiveness (In Thousands)") +
  ylab("") +
  xlab("Incremental effectiveness (in thousands)") +
  labs(color='', title = "D. Optimal vs minimal")  +
  theme(legend.position="")+
  geom_point()+
  ggsave("4. SA_CE planes Optimal vs minimal.jpeg", 
         width = 25, height = 8, units = "cm")


# combine ICER cost plots multiple
SA_CE_combine<-ggarrange(SA_CE_1,SA_CE_2,SA_CE_3,SA_CE_4,nrow=4,
                         common.legend = TRUE, legend = "bottom")
SA_CE_combine
ggsave("5. SA_combined_graphs_CE.png", 
       width = 20, height = 30, units = "cm")





#####################################################################################################################
#                                               Brazil                                                       #
#####################################################################################################################

# 1. 6H vs status quo
Brazil_quadrant1<-Brazil_quadrant_data %>%
  filter(comparison =="A. 6H vs status quo")

Brazil_color<-Brazil_quadrant_data

Brazil_CE_1<-Brazil_quadrant1 %>% 
  mutate(quadrant = case_when(incremental_effectiveness > 0 & incremental_cost > 0   ~ "More expensive and more effective",
                              incremental_effectiveness <= 0 & incremental_cost > 0  ~ "More expensive and less effective",
                              incremental_effectiveness <= 0 & incremental_cost <= 0 ~ "Less costly and less effective",
                              TRUE
                              ~ "Less costly and more effective")) %>% 
  ggplot(aes(x = incremental_effectiveness,Effectiveness, y = incremental_cost, color=quadrant ))+
  scale_colour_manual(values = c('More expensive and more effective'="deepskyblue", 'More expensive and less effective'="red", 'Less costly and less effective'="orange", 'Less costly and more effective'= "darkgreen"))+
  geom_point(size = 0.05, stroke = 0, shape = 16)+
  geom_vline(xintercept = 0, linetype="dotted") + # plot vertical line
  geom_hline(yintercept = 0, linetype="dotted") + # plot horizontal line
  scale_y_continuous(labels = unit_format( unit = "M", scale = 1e-6))+
  scale_x_continuous(labels = unit_format( unit = "", scale = 1e-3))+
  facet_wrap(~Effectiveness, dir="h", scales="free_x", ncol = 3) +
  theme_classic()+
  geom_abline(data =  subset(Brazil_color, Effectiveness =="1. DALYs averted"), aes(slope=8786, intercept=0), col='red') +
  # geom_abline(slope=26151, intercept=0, col='purple') +
  #  annotate("text", x = 200000, y = 800000000, label = "WTP - GDP", size=2, col="red")+
  #  annotate("text", x = 200000, y = 1600000000, label = "WTP - GDP*3", size=2, col="purple")+
  #  xlab("incremental effectiveness (In Thousands)") +
  ylab("") +
  xlab("") +
  labs(color='',title = "A. 6H vs status quo")  +
  theme(legend.position="")+
  geom_point()+
  ggsave("1. Brazil_CE planes 6H vs status quo.jpeg", 
         width = 25, height = 8, units = "cm")

#2. Minimal vs status quo
Brazil_quadrant2<-Brazil_quadrant_data %>%
  filter(comparison =="B. Minimal vs status quo")

Brazil_CE_2<-Brazil_quadrant2 %>% 
  mutate(quadrant = case_when(incremental_effectiveness > 0 & incremental_cost > 0   ~ "More expensive and more effective",
                              incremental_effectiveness <= 0 & incremental_cost > 0  ~ "More expensive and less effective",
                              incremental_effectiveness <= 0 & incremental_cost <= 0 ~ "Less costly and less effective",
                              TRUE
                              ~ "Less costly and more effective")) %>% 
  ggplot(aes(x = incremental_effectiveness,Effectiveness, y = incremental_cost, color=quadrant ))+
  scale_colour_manual(values = c('More expensive and more effective'="deepskyblue", 'More expensive and less effective'="red", 'Less costly and less effective'="orange", 'Less costly and more effective'= "darkgreen"))+
  geom_point(size = 0.05, stroke = 0, shape = 16)+
  geom_vline(xintercept = 0, linetype="dotted") + # plot vertical line
  geom_hline(yintercept = 0, linetype="dotted") + # plot horizontal line
  scale_y_continuous(labels = unit_format( unit = "M", scale = 1e-6))+
  scale_x_continuous(labels = unit_format( unit = "", scale = 1e-3))+
  facet_wrap(~Effectiveness, dir="h", scales="free_x", ncol = 3) +
  theme_classic()+
  geom_abline(data =  subset(Brazil_color, Effectiveness =="1. DALYs averted"), aes(slope=8786, intercept=0), col='red') +
  # geom_abline(slope=26151, intercept=0, col='purple') +
  #  annotate("text", x = 200000, y = 800000000, label = "WTP - GDP", size=2, col="red")+
  #  annotate("text", x = 200000, y = 1600000000, label = "WTP - GDP*3", size=2, col="purple")+
  #  xlab("incremental effectiveness (In Thousands)") +
  ylab("") +
  xlab("") +
  labs(color='', title = "B. Minimal vs status quo")  +
  theme(legend.position="")+
  geom_point()+
  ggsave("2. Brazil_CE planes Minimal vs status quo.jpeg", 
         width = 25, height = 8, units = "cm")

#3. Minimal vs 6H
Brazil_quadrant3<-Brazil_quadrant_data %>%
  filter(comparison =="C. Minimal vs 6H")

Brazil_CE_3<-Brazil_quadrant3 %>% 
  mutate(quadrant = case_when(incremental_effectiveness > 0 & incremental_cost > 0   ~ "More expensive and more effective",
                              incremental_effectiveness <= 0 & incremental_cost > 0  ~ "More expensive and less effective",
                              incremental_effectiveness <= 0 & incremental_cost <= 0 ~ "Less costly and less effective",
                              TRUE
                              ~ "Less costly and more effective")) %>% 
  ggplot(aes(x = incremental_effectiveness,Effectiveness, y = incremental_cost, color=quadrant ))+
  scale_colour_manual(values = c('More expensive and more effective'="deepskyblue", 'More expensive and less effective'="red", 'Less costly and less effective'="orange", 'Less costly and more effective'= "darkgreen"))+
  geom_point(size = 0.05, stroke = 0, shape = 16)+
  geom_vline(xintercept = 0, linetype="dotted") + # plot vertical line
  geom_hline(yintercept = 0, linetype="dotted") + # plot horizontal line
  scale_y_continuous(labels = unit_format( unit = "M", scale = 1e-6))+
  scale_x_continuous(labels = unit_format( unit = "", scale = 1e-3))+
  facet_wrap(~Effectiveness, dir="h", scales="free_x", ncol = 3) +
  theme_classic()+
  geom_abline(data =  subset(Brazil_color, Effectiveness =="1. DALYs averted"), aes(slope=8786, intercept=0), col='red') +
  #  geom_abline(slope=18120, intercept=0, col='purple') +
  #  annotate("text", x = 200000, y = 800000000, label = "WTP - GDP", size=2, col="red")+
  #  annotate("text", x = 200000, y = 1600000000, label = "WTP - GDP*3", size=2, col="purple")+
  #  xlab("incremental effectiveness (In Thousands)") +
  ylab("Incremental cost (In millions US$)") +
  xlab("") +
  labs(color='', title = "C. Minimal vs 6H")  +
  theme(legend.position="")+
  geom_point()+
  ggsave("3. Brazil_CE planes Minimal vs 6H.jpeg", 
         width = 25, height = 8, units = "cm")

#4. Optimal vs minimal
Brazil_quadrant4<-Brazil_quadrant_data %>%
  filter(comparison =="D. Optimal vs minimal")

Brazil_CE_4<-Brazil_quadrant4 %>% 
  mutate(quadrant = case_when(incremental_effectiveness > 0 & incremental_cost > 0   ~ "More expensive and more effective",
                              incremental_effectiveness <= 0 & incremental_cost > 0  ~ "More expensive and less effective",
                              incremental_effectiveness <= 0 & incremental_cost <= 0 ~ "Less costly and less effective",
                              TRUE
                              ~ "Less costly and more effective")) %>% 
  ggplot(aes(x = incremental_effectiveness,Effectiveness, y = incremental_cost, color=quadrant ))+
  scale_colour_manual(values = c('More expensive and more effective'="deepskyblue", 'More expensive and less effective'="red", 'Less costly and less effective'="orange", 'Less costly and more effective'= "darkgreen"))+
  geom_point(size = 0.05, stroke = 0, shape = 16)+
  geom_vline(xintercept = 0, linetype="dotted") + # plot vertical line
  geom_hline(yintercept = 0, linetype="dotted") + # plot horizontal line
  scale_y_continuous(labels = unit_format( unit = "M", scale = 1e-6))+
  scale_x_continuous(labels = unit_format( unit = "", scale = 1e-3))+
  facet_wrap(~Effectiveness, dir="h", scales="free_x", ncol = 3) +
  theme_classic()+
  geom_abline(data =  subset(Brazil_color, Effectiveness =="1. DALYs averted"), aes(slope=8786, intercept=0), col='red') +
  #  geom_abline(slope=18120, intercept=0, col='purple') +
  #  annotate("text", x = 200000, y = 800000000, label = "WTP - GDP", size=2, col="red")+
  #  annotate("text", x = 200000, y = 1600000000, label = "WTP - GDP*3", size=2, col="purple")+
  #  xlab("incremental effectiveness (In Thousands)") +
  ylab("") +
  xlab("Incremental effectiveness (in thousands)") +
  labs(color='', title = "D. Optimal vs minimal")  +
  theme(legend.position="")+
  geom_point()+
  ggsave("4. Brazil_CE planes Optimal vs minimal.jpeg", 
         width = 25, height = 8, units = "cm")


# combine ICER cost plots multiple
Brazil_CE_combine<-ggarrange(Brazil_CE_1,Brazil_CE_2,Brazil_CE_3,Brazil_CE_4,nrow=4,
                         common.legend = TRUE, legend = "bottom")
Brazil_CE_combine
ggsave("5. Brazil_combined_graphs_CE.png", 
       width = 20, height = 30, units = "cm")




