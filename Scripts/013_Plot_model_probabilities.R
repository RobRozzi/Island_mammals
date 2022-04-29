# clean environment
rm(list=ls())

##############################################################
# Load libraries                                          ####
##############################################################

library(ggplot2)
library(dplyr)
library(forcats)
library(Cairo)

##############################################################
# Load data                                               ####
##############################################################

data <- read.csv('Results/PyRate_ext_rates/Plots_ext_dates')

data <- data %>% mutate(Analysis = fct_reorder(Analysis, Model, .fun='length' )) #reorder

#Set theme
theme_set(theme_light(base_size = 15))

##############################################################
# Plot model probabilities                                ####
##############################################################

plot <- ggplot(data, aes(x = fct_inorder(Model),y = P.extinction., group = Analysis, color = Analysis)) +
  geom_point(size = 4) +
  geom_line(size = 2, alpha = 0.4) +
  scale_color_manual(values=c("#ea9298", "#c25234", "#52352e"), labels = c("last 0.129 Ma", "last 2.58 Ma", "last 23.03 Ma"))+
  ylab("Probability of extinction")+
  ylim(c(0,1))+
  annotate("text", x = c("2-rate","3-rate","10-rate"), y = c(0.98,0.6,0.32), 
          label = c("0.92", "0.54", "0.26") , color=c("#ea9298", "#c25234", "#52352e"), 
          family = "Arial", size = 4, fontface="bold")+
  theme(axis.title = element_text(family = "Arial", size = 8, colour = "grey40"),
        axis.text.x = element_text(angle = 45, family = "Arial", size = 8),
        axis.text.y = element_text(family = "Arial", size = 8),
        panel.grid = element_blank(), 
        axis.title.x = element_blank(),
        legend.text=element_text(family = "Arial", size = 8, colour = "grey40"),
        legend.title = element_blank())

plot

#Save plot as pdf
ggsave(plot, filename = "Plot_model_probabilities.pdf", path = "Results/PyRate_ext_rates/Plots_ext_dates", width = 10, height = 5, device = cairo_pdf) 

#End of script