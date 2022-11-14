# clean environment
rm(list=ls())

##############################################################
# Load libraries                                          ####
##############################################################

library(ggplot2)
library(bayesplot)
library(dplyr)
library(tidyr)
library(gridExtra)
library(Cairo)

#Set theme
theme_set(theme_light(base_size = 15))

######################################################
# Load data                                       ####
######################################################

data <- read.delim('Results/PyRate_MTET/Results_MTET_old_chr_from_Mio_.log')

#Get threshold values corresponding to log Bayes factor values of 2, 6, and 10

get_thresholds <- function(prior_pr){
  prior_odds = (prior_pr/(1-prior_pr)) 
  BF = c(10,6,2)
  A = exp(BF/2) * prior_odds
  thr = A/ (A+1)
  return(thr)
}

prior_prob = 0.05

significance <- get_thresholds(prior_prob) #thresholds 

log_BF_2 = significance[3]
log_BF_6 = significance[2]
log_BF_10 = significance[1]


##############################################################
# Plot effect temporal overlap with humans                ####
##############################################################

#Set theme and colour scheme

bayesplot_theme_set(theme_light(base_size = 15))

posterior <- as.matrix(data)

orange_scheme <- c("#f2ca75", "#f2ca75",
                   "#f2ca75", "#dc9e31",
                   "#c3995f", "#663d00")
color_scheme_set(orange_scheme)
color_scheme_view()

#Plot

plot_time_effect <- mcmc_areas(posterior, pars = c("sapiens_effect_log10", "pre_sapiens_effect_log10"), prob = 0.95) +
  scale_y_discrete(labels=c("Effect of island-specific human arrival"))+
  labs(y= "Posterior probability", x = "log-magnitude of rate change")+
  coord_cartesian(xlim =c(-1,4))+
          theme(axis.title = element_text(family = "Arial", size = 12, colour = "grey40"),
          axis.text.x = element_text(family = "Arial", size = 12, colour = "grey40"),
          axis.title.x = element_text(family = "Arial", size = 12, colour = "grey40"),
          axis.text.y = element_blank(),
          panel.grid = element_blank()) 

plot_time_effect

#Save plot as pdf
ggsave(plot_time_effect, filename = "Plot_overlap_effect_old_chr_from_Mio.pdf", path = "Results/PyRate_MTET", width = 3, height = 2, device = cairo_pdf)

##############################################################
# Plot bars of probability of an effect on extinction     ####
##############################################################

probs <- select(data, I_bm, I_body_size_change, I_endemism, I_island_type) 

probs <- probs%>%summarise_if(is.numeric, mean)
 
probs <- t(probs)

probs <- as.data.frame(probs)

probs <- rename(probs, mean = V1)

probs <- tibble::rownames_to_column(probs, "VALUE")

probs <- rename(probs, trait = VALUE)

theme_set(theme_light(base_size = 15))

#Plot

plot_prob_effect <- ggplot(probs, aes(x = reorder(trait, -mean),y = mean, fill = trait)) +
  geom_bar(stat = "identity")+
  geom_hline(yintercept=log_BF_2, linetype="dashed", color = "#cc8a15", size=.4)+
  geom_hline(yintercept=log_BF_6, linetype="dashed", color = "#9d3e2f", size=.4)+
  geom_hline(yintercept=log_BF_10, linetype="dashed", color = "#562014", size=.4)+
  scale_x_discrete(labels=c("endemism", "island type","body size change","body mass"))+
  scale_fill_manual(values=c("#0d574d", "#004571", "#004571", "#004571"))+
  ylab("probability of an effect on extinction")+
  theme(axis.title = element_text(family = "Arial", size = 12, colour = "grey40"),
        axis.text.x = element_text(family = "Arial", size = 12),
        axis.text.y = element_text(family = "Arial", size = 12),
        panel.grid = element_blank(), 
        axis.title.x = element_blank(),
        legend.position = "none")


plot_prob_effect

#Save plot as pdf
ggsave(plot_prob_effect, filename = "Plot_prob_effect_old_chr_from_Mio.pdf", path = "Results/PyRate_MTET", width = 5, height = 5, device = cairo_pdf)

##############################################################
# Violin plot of relative effect of BM on extinction      ####
##############################################################

BM <- select(data, m_bm_0, m_bm_1, m_bm_2, m_bm_3, m_bm_4, m_bm_5, m_bm_6) 

BM <- gather(BM, class_BM, value, m_bm_0:m_bm_6, factor_key=TRUE) 

BM

#Plot
violin_BM <- ggplot(BM, aes(x = class_BM, y = value)) + 
  geom_violin(scale = "width", alpha = 0.5, fill="#0d574d", color=FALSE)+
  stat_summary(fun=median, size = 2, geom="point", colour="#0d574d")+
  scale_fill_manual(values=c("#0d574d"))+
  labs(x = "Body mass (kg)", y = "relative effect on extinction") +
  geom_hline(yintercept=1/7, linetype="dashed", color = "grey40", size=.4)+ #value of yintercept = h=1/length(indx_trait); in this case 1/6 because we have 7 classes of BM
  scale_x_discrete(labels=c("very small", "small","small medium","medium", "large medium", "large", "very large"))+
  theme(axis.title = element_text(family = "Arial", size = 12, colour = "grey40"),
        axis.text.x = element_text(family = "Arial", size = 12),
        axis.text.y = element_text(family = "Arial", size = 12),
        panel.grid = element_blank(), 
        axis.title.x = element_text(family = "Arial", size = 16),
        legend.position = "none") 

violin_BM

#Save plot as pdf
ggsave(violin_BM, filename = "Rel_effect_BM_old_chr_from_Mio.pdf", path = "Results/PyRate_MTET", width = 5, height = 5, device = cairo_pdf) 

##############################################################
# Violin plot of relative effect of BSC on extinction     ####
##############################################################

BSC <- select(data, m_body_size_change_0, m_body_size_change_1, m_body_size_change_2, m_body_size_change_3, m_body_size_change_4, m_body_size_change_5, m_body_size_change_6) 

BSC <- gather(BSC, class_BSC, value, m_body_size_change_0:m_body_size_change_6, factor_key=TRUE)

BSC

#Plot
violin_BSC <- ggplot(BSC, aes(x = class_BSC, y = value)) + 
  geom_violin(scale = "width", alpha = 0.5, fill="#004571", color=FALSE)+
  stat_summary(fun=median, size = 2, geom="point", colour="#004571")+
  scale_fill_manual(values=c("#004571"))+
  labs(x = "Body size change", y = "relative effect on extinction") +
  geom_hline(yintercept=1/7, linetype="dashed", color = "grey40", size=.4)+ #value of yintercept = h=1/length(indx_trait); in this case 1/6 because we have 7 classes of BM
  scale_x_discrete(labels=c("strongly D", "moderately D","slightly D","no change", "slightly G", "moderately G", "strongly G"))+
  theme(axis.title = element_text(family = "Arial", size = 12, colour = "grey40"),
        axis.text.x = element_text(family = "Arial", size = 12),
        axis.text.y = element_text(family = "Arial", size = 12),
        panel.grid = element_blank(), 
        axis.title.x = element_text(family = "Arial", size = 16),
        legend.position = "none") 

violin_BSC

#Save plot as pdf
ggsave(violin_BSC, filename = "Rel_effect_BSC_old_chr_from_Mio.pdf",  path = "Results/PyRate_MTET", width = 5, height = 5, device = cairo_pdf)

################################################################
# Violin plot of relative effect of endemism on extinction  ####
################################################################

endemism <- select(data, m_endemism_0, m_endemism_1) 

endemism <- gather(endemism, class_endemism, value, m_endemism_0:m_endemism_1, factor_key=TRUE)

endemism

#Plot
violin_endemism <- ggplot(endemism, aes(x = class_endemism, y = value)) + 
  geom_violin(scale = "width", alpha = 0.5, fill="#004571", color=FALSE)+
  stat_summary(fun=median, size = 2, geom="point", colour="#004571")+
  scale_fill_manual(values=c("#004571"))+
  labs(x = "Endemism", y = "relative effect on extinction") +
  geom_hline(yintercept=1/2, linetype="dashed", color = "grey40", size=.4)+ #value of yintercept = h=1/length(indx_trait); in this case 1/6 because we have 7 classes of BM
  scale_x_discrete(labels=c("non-endemic", "endemic"))+
  theme(axis.title = element_text(family = "Arial", size = 12, colour = "grey40"),
        axis.text.x = element_text(family = "Arial", size = 12),
        axis.text.y = element_text(family = "Arial", size = 12),
        panel.grid = element_blank(), 
        axis.title.x = element_text(family = "Arial", size = 16),
        legend.position = "none") 

violin_endemism

#Save plot as pdf
ggsave(violin_endemism, filename = "Rel_effect_endemism_old_chr_from_Mio.pdf", path = "Results/PyRate_MTET", width = 5, height = 5, device = cairo_pdf)

###################################################################
# Violin plot of relative effect of island type on extinction  ####
###################################################################

island <- select(data, m_island_type_0, m_island_type_1, m_island_type_2) 

island <- gather(island, class_island, value, m_island_type_0:m_island_type_2, factor_key=TRUE) 

island

#Plot
violin_island <- ggplot(island, aes(x = class_island, y = value)) + 
  geom_violin(scale = "width", alpha = 0.5, fill="#004571", color=FALSE)+
  stat_summary(fun=median, size = 2, geom="point", colour="#004571")+
  scale_fill_manual(values=c("#004571"))+
  labs(x = "Island type", y = "relative effect on extinction") +
  geom_hline(yintercept=1/3, linetype="dashed", color = "grey40", size=.4)+ #value of yintercept = h=1/length(indx_trait); in this case 1/6 because we have 7 classes of BM
  scale_x_discrete(labels=c("continental", "oceanic","both"))+
  theme(axis.title = element_text(family = "Arial", size = 12, colour = "grey40"),
        axis.text.x = element_text(family = "Arial", size = 12),
        axis.text.y = element_text(family = "Arial", size = 12),
        panel.grid = element_blank(), 
        axis.title.x = element_text(family = "Arial", size = 16),
        legend.position = "none") 

violin_island

#Save plot as pdf
ggsave(violin_island, filename = "Rel_effect_island_type_old_chr_from_Mio.pdf", path = "Results/PyRate_MTET", width = 5, height = 5, device = cairo_pdf)

###########################################
# Combine 6 plots in one for Figure 3  ####
###########################################

lay <- rbind(c(2,2,2,1,1,1,5,5,5),
             c(6,6,6,4,4,4,3,3,3))


figure <- grid.arrange(violin_endemism, plot_prob_effect, violin_BSC, violin_island, violin_BM,plot_time_effect, ncol = 3, 
             layout_matrix = lay)

#Save figure as pdf
ggsave(figure, filename = "MTET_plots_old_chr_from_Mio.pdf", path = "Results/PyRate_MTET", width = 10, height = 6, device = cairo_pdf) 

#End of script
