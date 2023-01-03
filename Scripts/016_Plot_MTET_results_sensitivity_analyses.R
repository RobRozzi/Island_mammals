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

data_Mio_normal_young_chr <- read.delim('Results/PyRate_MTET/Results_MTET_young_chr_from_Mio_.log')
data_Mio_normal_young_chr <- rename(data_Mio_normal_young_chr, time_effect_Mio_all_yc_pre_sapiens = pre_sapiens_effect_log10)
data_Mio_normal_young_chr <- rename(data_Mio_normal_young_chr, time_effect_Mio_all_yc_sapiens = sapiens_effect_log10)
data_Mio_normal_young_chr <- select(data_Mio_normal_young_chr, "time_effect_Mio_all_yc_pre_sapiens", "time_effect_Mio_all_yc_sapiens")

data_Mio_anagen_cut_old_chr <- read.delim('Results/PyRate_MTET/Results_MTET_old_chr_from_Mio_anagen_cut_.log')
data_Mio_anagen_cut_old_chr <- rename(data_Mio_anagen_cut_old_chr, time_effect_Mio_anagen_cut_oc_pre_sapiens = pre_sapiens_effect_log10)
data_Mio_anagen_cut_old_chr <- rename(data_Mio_anagen_cut_old_chr, time_effect_Mio_anagen_cut_oc_sapiens = sapiens_effect_log10)
data_Mio_anagen_cut_old_chr <- select(data_Mio_anagen_cut_old_chr, "time_effect_Mio_anagen_cut_oc_pre_sapiens", "time_effect_Mio_anagen_cut_oc_sapiens")

data_Mio_anagen_cut_young_chr <- read.delim('Results/PyRate_MTET/Results_MTET_young_chr_from_Mio_anagen_cut_.log')
data_Mio_anagen_cut_young_chr <- rename(data_Mio_anagen_cut_young_chr, time_effect_Mio_anagen_cut_yc_pre_sapiens = pre_sapiens_effect_log10)
data_Mio_anagen_cut_young_chr <- rename(data_Mio_anagen_cut_young_chr, time_effect_Mio_anagen_cut_yc_sapiens = sapiens_effect_log10)
data_Mio_anagen_cut_young_chr <- select(data_Mio_anagen_cut_young_chr, "time_effect_Mio_anagen_cut_yc_pre_sapiens", "time_effect_Mio_anagen_cut_yc_sapiens")

data_Pleisto_normal_old_chr <- read.delim('Results/PyRate_MTET/Results_MTET_old_chr_from_Pleisto_.log')
data_Pleisto_normal_old_chr <- rename(data_Pleisto_normal_old_chr, time_effect_Pleisto_all_oc_pre_sapiens = pre_sapiens_effect_log10)
data_Pleisto_normal_old_chr <- rename(data_Pleisto_normal_old_chr, time_effect_Pleisto_all_oc_sapiens = sapiens_effect_log10)
data_Pleisto_normal_old_chr <- select(data_Pleisto_normal_old_chr, "time_effect_Pleisto_all_oc_pre_sapiens", "time_effect_Pleisto_all_oc_sapiens")

data_Pleisto_normal_young_chr <- read.delim('Results/PyRate_MTET/Results_MTET_young_chr_from_Pleisto_.log')
data_Pleisto_normal_young_chr <- rename(data_Pleisto_normal_young_chr, time_effect_Pleisto_all_yc_pre_sapiens = pre_sapiens_effect_log10)
data_Pleisto_normal_young_chr <- rename(data_Pleisto_normal_young_chr, time_effect_Pleisto_all_yc_sapiens = sapiens_effect_log10)
data_Pleisto_normal_young_chr <- select(data_Pleisto_normal_young_chr, "time_effect_Pleisto_all_yc_pre_sapiens", "time_effect_Pleisto_all_yc_sapiens")

data_Pleisto_anagen_cut_old_chr <- read.delim('Results/PyRate_MTET/Results_MTET_old_chr_from_Pleisto_anagen_cut_.log')
data_Pleisto_anagen_cut_old_chr <- rename(data_Pleisto_anagen_cut_old_chr, time_effect_Pleisto_anagen_cut_oc_pre_sapiens = pre_sapiens_effect_log10)
data_Pleisto_anagen_cut_old_chr <- rename(data_Pleisto_anagen_cut_old_chr, time_effect_Pleisto_anagen_cut_oc_sapiens = sapiens_effect_log10)
data_Pleisto_anagen_cut_old_chr <- select(data_Pleisto_anagen_cut_old_chr, "time_effect_Pleisto_anagen_cut_oc_pre_sapiens", "time_effect_Pleisto_anagen_cut_oc_sapiens")

data_Pleisto_anagen_cut_young_chr <- read.delim('Results/PyRate_MTET/Results_MTET_young_chr_from_Pleisto_anagen_cut_.log')
data_Pleisto_anagen_cut_young_chr <- rename(data_Pleisto_anagen_cut_young_chr, time_effect_Pleisto_anagen_cut_yc_pre_sapiens = pre_sapiens_effect_log10)
data_Pleisto_anagen_cut_young_chr <- rename(data_Pleisto_anagen_cut_young_chr, time_effect_Pleisto_anagen_cut_yc_sapiens = sapiens_effect_log10)
data_Pleisto_anagen_cut_young_chr <- select(data_Pleisto_anagen_cut_young_chr, "time_effect_Pleisto_anagen_cut_yc_pre_sapiens", "time_effect_Pleisto_anagen_cut_yc_sapiens")

data_Late_Pleisto_normal_old_chr <- read.delim('Results/PyRate_MTET/Results_MTET_old_chr_from_Late_Pleisto_.log')
data_Late_Pleisto_normal_old_chr <- rename(data_Late_Pleisto_normal_old_chr, time_effect_Late_Pleisto_all_oc_pre_sapiens = pre_sapiens_effect_log10)
data_Late_Pleisto_normal_old_chr <- rename(data_Late_Pleisto_normal_old_chr, time_effect_Late_Pleisto_all_oc_sapiens = sapiens_effect_log10)
data_Late_Pleisto_normal_old_chr <- select(data_Late_Pleisto_normal_old_chr, "time_effect_Late_Pleisto_all_oc_pre_sapiens", "time_effect_Late_Pleisto_all_oc_sapiens")

data_Late_Pleisto_normal_young_chr <- read.delim('Results/PyRate_MTET/Results_MTET_young_chr_from_Late_Pleisto_.log')
data_Late_Pleisto_normal_young_chr <- rename(data_Late_Pleisto_normal_young_chr, time_effect_Late_Pleisto_all_yc_pre_sapiens = pre_sapiens_effect_log10)
data_Late_Pleisto_normal_young_chr <- rename(data_Late_Pleisto_normal_young_chr, time_effect_Late_Pleisto_all_yc_sapiens = sapiens_effect_log10)
data_Late_Pleisto_normal_young_chr <- select(data_Late_Pleisto_normal_young_chr, "time_effect_Late_Pleisto_all_yc_pre_sapiens", "time_effect_Late_Pleisto_all_yc_sapiens")

#Combine all datasets
data <- cbind(data_Mio_normal_young_chr, data_Mio_anagen_cut_old_chr, data_Mio_anagen_cut_young_chr,
              data_Pleisto_normal_old_chr, data_Pleisto_normal_young_chr, data_Pleisto_anagen_cut_old_chr,
              data_Pleisto_anagen_cut_young_chr, data_Late_Pleisto_normal_old_chr, data_Late_Pleisto_normal_young_chr)

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

plot_time_effect <- mcmc_areas(posterior, prob = 0.95) +
  labs(y= "Posterior probability", x = "log-magnitude of rate change")+
  coord_cartesian(xlim =c(-3,4))+
          theme(axis.title = element_text(family = "Arial", size = 12, colour = "grey40"),
          axis.text.x = element_text(family = "Arial", size = 12, colour = "grey40"),
          axis.title.x = element_text(family = "Arial", size = 12, colour = "grey40"),
          axis.text.y = element_blank(),
          panel.grid = element_blank()) 

plot_time_effect

#Save plot as pdf
ggsave(plot_time_effect, filename = "Plot_overlap_effect_all_sensitivity.pdf", path = "Results/PyRate_MTET", width = 3, height = 10, device = cairo_pdf)

######################################################
# Load data                                       ####
######################################################

data_Mio_normal_young_chr <- read.delim('Results/PyRate_MTET/Results_MTET_young_chr_from_Mio_.log')
data_Mio_normal_young_chr <- mutate(data_Mio_normal_young_chr, data_type = "Mio_normal_young_chr")

data_Mio_anagen_cut_old_chr <- read.delim('Results/PyRate_MTET/Results_MTET_old_chr_from_Mio_anagen_cut_.log')
data_Mio_anagen_cut_old_chr <- mutate(data_Mio_anagen_cut_old_chr, data_type = "Mio_anagen_cut_old_chr")

data_Mio_anagen_cut_young_chr <- read.delim('Results/PyRate_MTET/Results_MTET_young_chr_from_Mio_anagen_cut_.log')
data_Mio_anagen_cut_young_chr <- mutate(data_Mio_anagen_cut_young_chr, data_type = "Mio_anagen_cut_young_chr")

data_Pleisto_normal_old_chr <- read.delim('Results/PyRate_MTET/Results_MTET_old_chr_from_Pleisto_.log')
data_Pleisto_normal_old_chr <- mutate(data_Pleisto_normal_old_chr, data_type = "Pleisto_normal_old_chr")

data_Pleisto_normal_young_chr <- read.delim('Results/PyRate_MTET/Results_MTET_young_chr_from_Pleisto_.log')
data_Pleisto_normal_young_chr <- mutate(data_Pleisto_normal_young_chr, data_type = "Pleisto_normal_young_chr")

data_Pleisto_anagen_cut_old_chr <- read.delim('Results/PyRate_MTET/Results_MTET_old_chr_from_Pleisto_anagen_cut_.log')
data_Pleisto_anagen_cut_old_chr <- mutate(data_Pleisto_anagen_cut_old_chr, data_type = "Pleisto_anagen_cut_old_chr")

data_Pleisto_anagen_cut_young_chr <- read.delim('Results/PyRate_MTET/Results_MTET_young_chr_from_Pleisto_anagen_cut_.log')
data_Pleisto_anagen_cut_young_chr <- mutate(data_Pleisto_anagen_cut_young_chr, data_type = "Pleisto_anagen_cut_young_chr")

data_Late_Pleisto_normal_old_chr <- read.delim('Results/PyRate_MTET/Results_MTET_old_chr_from_Late_Pleisto_.log')
data_Late_Pleisto_normal_old_chr <- mutate(data_Late_Pleisto_normal_old_chr, data_type = "Late_Pleisto_normal_old_chr")

data_Late_Pleisto_normal_young_chr <- read.delim('Results/PyRate_MTET/Results_MTET_young_chr_from_Late_Pleisto_.log')
data_Late_Pleisto_normal_young_chr <- mutate(data_Late_Pleisto_normal_young_chr, data_type = "Late_Pleisto_normal_young_chr")

#Combine all datasets
data <- rbind(data_Mio_normal_young_chr, data_Mio_anagen_cut_old_chr, data_Mio_anagen_cut_young_chr,
              data_Pleisto_normal_old_chr, data_Pleisto_normal_young_chr, data_Pleisto_anagen_cut_old_chr,
              data_Pleisto_anagen_cut_young_chr, data_Late_Pleisto_normal_old_chr, data_Late_Pleisto_normal_young_chr)

#Get threshold values corresponding to log Bayes factor values of 2, 6, and 10

get_thresholds <- function(prior_pr){
  prior_odds = (prior_pr/(1-prior_pr)) 
  BF = c(10,6,2)
  A = exp(BF/2) * prior_odds
  thr = A/ (A+1)
  return(thr)
}

prior_prob = 0.05

significance <- get_thresholds(prior_prob)

log_BF_2 = significance[3]
log_BF_6 = significance[2]
log_BF_10 = significance[1]

##############################################################
# Plot bars of probability of an effect on extinction     ####
##############################################################

probs <- select(data, I_bm, I_body_size_change, I_Endemism, I_island_type, data_type) 

probs <- probs%>%group_by(data_type)%>%summarise_if(is.numeric, mean)

probs <- probs %>% gather(trait, mean, -c(data_type))

theme_set(theme_light(base_size = 15))

#Plot

plot_prob_effect <- ggplot(probs, aes(x = reorder(trait, -mean),y = mean, fill = trait)) +
  geom_bar(stat = "identity")+
  geom_hline(yintercept=log_BF_2, linetype="dashed", color = "#cc8a15", linewidth=.4)+
  geom_hline(yintercept=log_BF_6, linetype="dashed", color = "#9d3e2f", linewidth=.4)+
  geom_hline(yintercept=log_BF_10, linetype="dashed", color = "#562014", linewidth=.4)+
  scale_x_discrete(labels=c("endemism", "island type","body size change","body mass"))+
  scale_fill_manual(values=c("#0d574d", "#004571", "#004571", "#004571"))+ 
  ylab("probability of an effect on extinction")+
  facet_wrap(~ data_type, ncol = 1)+
  theme(axis.title = element_text(family = "Arial", size = 12, colour = "grey40"),
        axis.text.x = element_text(family = "Arial", size = 12),
        axis.text.y = element_text(family = "Arial", size = 12),
        panel.grid = element_blank(), 
        axis.title.x = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill="white", size=1.5, linetype="solid"),
        strip.text.x = element_blank())

plot_prob_effect

#Save plot as pdf
ggsave(plot_prob_effect, filename = "Plot_prob_effect_all_sensitivity.pdf", path = "Results/PyRate_MTET", width = 3, height = 10, device = cairo_pdf)

##############################################################
# Violin plot of relative effect of BM on extinction      ####
##############################################################

BM <- select(data, m_bm_0, m_bm_1, m_bm_2, m_bm_3, m_bm_4, m_bm_5, m_bm_6, data_type) 

BM <- gather(BM, class_BM, value, m_bm_0:m_bm_6, -c(data_type), factor_key=TRUE)

BM

#Plot 

violin_BM <- ggplot(BM, aes(x = class_BM, y = value)) + 
  geom_violin(scale = "width", alpha = 0.5, fill="#0d574d", color=FALSE)+
  stat_summary(fun=median, size = 2, geom="point", colour="#0d574d")+
  scale_fill_manual(values=c("#0d574d"))+
  labs(x = "Body mass (kg)", y = "relative effect on extinction") +
  geom_hline(yintercept=1/7, linetype="dashed", color = "grey40", size=.4)+ #value of yintercept = h=1/length(indx_trait); in this case 1/6 because we have 7 classes of BM
  scale_x_discrete(labels=c("very small", "small","small medium","medium", "large medium", "large", "very large"))+
  facet_wrap(~ data_type, ncol = 1)+
  theme(axis.title = element_blank(),
        axis.text.x = element_text(family = "Arial", size = 12),
        axis.text.y = element_blank(),
        panel.grid = element_blank(), 
        axis.title.x = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill="white", size=1.5, linetype="solid"),
        strip.text.x = element_blank())

violin_BM

#Save plot as pdf
ggsave(violin_BM, filename = "Rel_effect_BM_all_sensitivity.pdf", path = "Results/PyRate_MTET", width = 3, height = 10, device = cairo_pdf) 

##############################################################
# Violin plot of relative effect of BSC on extinction     ####
##############################################################

BSC <- select(data, m_body_size_change_0, m_body_size_change_1, m_body_size_change_2, m_body_size_change_3, m_body_size_change_4, m_body_size_change_5, m_body_size_change_6, data_type) 

BSC <- gather(BSC, class_BSC, value, m_body_size_change_0:m_body_size_change_6, -c(data_type), factor_key=TRUE)

BSC

#Plot 

violin_BSC <- ggplot(BSC, aes(x = class_BSC, y = value)) + 
  geom_violin(scale = "width", alpha = 0.5, fill="#0d574d", color=FALSE)+
  stat_summary(fun=median, size = 2, geom="point", colour="#0d574d")+
  scale_fill_manual(values=c("#0d574d"))+
  labs(x = "Body size change", y = "relative effect on extinction") +
  geom_hline(yintercept=1/7, linetype="dashed", color = "grey40", size=.4)+ #value of yintercept = h=1/length(indx_trait); in this case 1/6 because we have 7 classes of BM
  scale_x_discrete(labels=c("strongly D", "moderately D","slightly D","no change", "slightly G", "moderately G", "strongly G"))+
  facet_wrap(~ data_type, ncol = 1)+
  theme(axis.title = element_blank(),
        axis.text.x = element_text(family = "Arial", size = 12),
        axis.text.y = element_blank(),
        panel.grid = element_blank(), 
        axis.title.x = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill="white", size=1.5, linetype="solid"),
        strip.text.x = element_blank())

violin_BSC

#Save plot as pdf
ggsave(violin_BSC, filename = "Rel_effect_BSC_all_sensitivity.pdf", path = "Results/PyRate_MTET", width = 3, height = 10, device = cairo_pdf)

################################################################
# Violin plot of relative effect of endemism on extinction  ####
################################################################

endemism <- select(data, m_Endemism_0, m_Endemism_1, data_type) 

endemism <- gather(endemism, class_Endemism, value, m_Endemism_0:m_Endemism_1, -c(data_type), factor_key=TRUE)

endemism

#Plot
violin_endemism <- ggplot(endemism, aes(x = class_Endemism, y = value)) + 
  geom_violin(scale = "width", alpha = 0.5, fill="#004571", color=FALSE)+
  stat_summary(fun=median, size = 2, geom="point", colour="#004571")+
  scale_fill_manual(values=c("##004571"))+
  labs(x = "Endemism", y = "relative effect on extinction") +
  geom_hline(yintercept=1/2, linetype="dashed", color = "grey40", size=.4)+ #value of yintercept = h=1/length(indx_trait); in this case 1/6 because we have 7 classes of BM
  scale_x_discrete(labels=c("non-endemic", "endemic"))+
  facet_wrap(~ data_type, ncol = 1)+
  theme(axis.title = element_text(family = "Arial", size = 12, colour = "grey40"),
        axis.text.x = element_text(family = "Arial", size = 12),
        axis.text.y = element_blank(),
        panel.grid = element_blank(), 
        axis.title.x = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill="white", size=1.5, linetype="solid"),
        strip.text.x = element_blank())

violin_endemism

#Save plot as pdf
ggsave(violin_endemism, filename = "Rel_effect_endemism_all_sensitivity.pdf", path = "Results/PyRate_MTET", width = 3, height = 10, device = cairo_pdf)

###################################################################
# Violin plot of relative effect of island type on extinction  ####
###################################################################

island <- select(data, m_island_type_0, m_island_type_1, m_island_type_2, data_type) 

island <- gather(island, class_island, value, m_island_type_0:m_island_type_2, -c(data_type), factor_key=TRUE)

island

#Plot 

violin_island <- ggplot(island, aes(x = class_island, y = value)) + 
  geom_violin(scale = "width", alpha = 0.5, fill="#004571", color=FALSE)+
  stat_summary(fun=median, size = 2, geom="point", colour="#004571")+
  scale_fill_manual(values=c("#004571"))+
  labs(x = "Island type", y = "relative effect on extinction") +
  geom_hline(yintercept=1/3, linetype="dashed", color = "grey40", size=.4)+ #value of yintercept = h=1/length(indx_trait); in this case 1/6 because we have 7 classes of BM
  scale_x_discrete(labels=c("continental", "oceanic","both"))+
  facet_wrap(~ data_type, ncol = 1)+
  theme(axis.title = element_blank(),
        axis.text.x = element_text(family = "Arial", size = 12),
        axis.text.y = element_blank(),
        panel.grid = element_blank(), 
        axis.title.x = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill="white", size=1.5, linetype="solid"),
        strip.text.x = element_blank())

violin_island

#Save plot as pdf
ggsave(violin_island, filename = "Rel_effect_island_type_all_sensitivity.pdf", path = "Results/PyRate_MTET", width = 3, height = 10, device = cairo_pdf)

########################################################
# Combine 6 plots in one for Extended Data Fig. 10  ####
########################################################

lay <- rbind(c(1,1,2,2,3,3,4,4,5,5,6,6),
             c(1,1,2,2,3,3,4,4,5,5,6,6),
             c(1,1,2,2,3,3,4,4,5,5,6,6),
             c(1,1,2,2,3,3,4,4,5,5,6,6),
             c(1,1,2,2,3,3,4,4,5,5,6,6),
             c(1,1,2,2,3,3,4,4,5,5,6,6),
             c(1,1,2,2,3,3,4,4,5,5,6,6),
             c(1,1,2,2,3,3,4,4,5,5,6,6),
             c(1,1,2,2,3,3,4,4,5,5,6,6),
             c(1,1,2,2,3,3,4,4,5,5,6,6))

figure <- grid.arrange(plot_time_effect, plot_prob_effect, violin_endemism, violin_island, violin_BSC, violin_BM, 
             layout_matrix = lay)

#Save figure as pdf
ggsave(figure, filename = "MTET_plots_all_sensitivity.pdf", path = "Results/PyRate_MTET", width = 12, height = 14, device = cairo_pdf)

#End of script

