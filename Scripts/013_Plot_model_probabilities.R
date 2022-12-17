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

data <- read.csv('Results/PyRate_ext_rates/Plots_ext_rates/PyRate_model_probabilities.csv')

data <- data %>% mutate(Analysis = fct_relevel(Analysis, "from_Late_Pleistocene", "from_Pleistocene", "from_Miocene")) #reorder

#Set theme
theme_set(theme_light(base_size = 15))

##############################################################
# Plot model probabilities                                ####
##############################################################

plot <- ggplot(data, aes(x = fct_inorder(Model),y = P.extinction., group = Analysis, color = Analysis)) +
  geom_point(size = 4) +
  geom_line(size = 2, alpha = 0.4) +
  scale_color_manual(values=c("#ea9298", "#c25234", "#52352e"), labels = c("last 0.129 Ma", "last 2.58 Ma", "last 23.03 Ma"))+
  ylab("Model posterior probability")+
  ylim(c(0,1))+
  annotate("text", x = c("3-rate","3-rate","8-rate"), y = c(0.96,0.64,0.28), 
          label = c("0.91", "0.59", "0.22") , color=c("#ea9298", "#c25234", "#52352e"), 
          family = "Arial", size = 4, fontface="bold")+
  theme(axis.title = element_text(family = "Arial", size = 18, colour = "grey40"),
        axis.text.x = element_text(angle = 45, family = "Arial", size = 14),
        axis.text.y = element_text(family = "Arial", size = 14),
        panel.grid = element_blank(), 
        axis.title.x = element_blank(),
        legend.text=element_text(family = "Arial", size = 14, colour = "grey40"),
        legend.title = element_blank())

plot

#Save plot as pdf
ggsave(plot, filename = "Plot_model_probabilities.pdf", path = "Results/PyRate_ext_rates/Plots_ext_rates", width = 10, height = 5, device = cairo_pdf) 

#End of script
