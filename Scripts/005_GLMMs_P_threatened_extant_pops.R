# clean environment
rm(list=ls())

##############################################################
# Load libraries                                          ####
##############################################################

library(dplyr)
library(ggplot2)
library(forcats)
library(lme4)
library(sjPlot)
library(car)
library(DHARMa)
library(RColorBrewer)
library(ggeffects)
library(visreg)
library(extrafont)
library(Cairo)
library(IUCNpalette)

##############################################################
# Load data                                               ####
##############################################################

island_pops_ext <- read.csv('Data/Extant_mammals_populations.csv')

#remove populations with not available BM and not yet assessed
island_pops_ext <- filter(island_pops_ext, IUCN_Category != "not_assessed_yet") 
island_pops_ext <- filter(island_pops_ext, Body_mass_island_taxon != "") 

#remove populations that experienced no substantial size change
island_pops_ext <- filter(island_pops_ext, Direction_body_size_change != "no_change")

#Reorder IUCN_Category from low to high extinction risk
island_pops_ext <- island_pops_ext %>%
  mutate(IUCN_Category =
           fct_relevel(IUCN_Category,
                       "LC",
                       "NT",
                       "VU",
                       "EN",
                       "CR"))
                       
#Create threatened vs nonthreatened column
island_pops_ext <- island_pops_ext %>%
  mutate(Pr_threatened = recode_factor(IUCN_Category,
                                    CR = "threatened",
                                    EN = "threatened",
                                    VU = "threatened",
                                    NT = "nonthreatened",
                                    LC = "nonthreatened"))

#Reorder threatened vs nonthreatened column from low to high extinction risk
island_pops_ext <- island_pops_ext %>%
  mutate(Pr_threatened =
           fct_relevel(Pr_threatened,
                       "nonthreatened",
                       "threatened"))

###################################################################
# Plot populationss for each Order based on IUCN colours       ####
###################################################################

extrafont::loadfonts(device = "win")

theme_set(theme_light(base_size = 15, base_family = "Arial"))

Available_pops_per_order <- ggplot(island_pops_ext, aes(x= Order, fill = IUCN_Category)) +
                            geom_bar() +
                            scale_fill_manual(values = iucn_palettes) +
                            theme(legend.position = "none",
                                  axis.title = element_blank(),
                                  axis.text.x = element_text(family = "Arial", size = 14, angle = 45, vjust = 0.5, colour = "grey40"),
                                  axis.text.y = element_text(family = "Arial", size = 14),
                                  panel.grid = element_blank())

Available_pops_per_order

#Safe figure in pdf
ggsave(Available_pops_per_order, filename = "Available_pops_per_order.pdf", path = "/Results/Raw_data_plots", width = 8, height = 6, device = cairo_pdf)

#Scale body mass
island_pops_ext$Body_mass_island_taxon_scaled <- scale(island_pops_ext$Body_mass_island_taxon) 

##############################################################
# Fit GLMMs                                               ####
##############################################################

null <- glmer(Pr_threatened ~ (1|Order), data = island_pops_ext, family=binomial(link="logit"))

glmer1 <- glmer(Pr_threatened ~ Magnitude_body_size_change + (1|Order), data = island_pops_ext, family=binomial(link="logit"))
  
glmer2 <- glmer(Pr_threatened ~ Body_mass_island_taxon_scaled + (1|Order), data = island_pops_ext, family=binomial(link="logit"))

glmer3 <- glmer(Pr_threatened ~ Magnitude_body_size_change + Body_mass_island_taxon_scaled + (1|Order), data = island_pops_ext, family=binomial(link="logit")) 

glmer4 <- glmer(Pr_threatened ~ Magnitude_body_size_change * Body_mass_island_taxon_scaled + (1|Order), data = island_pops_ext, family=binomial(link="logit")) 

glmer5 <- glmer(Pr_threatened ~ Magnitude_body_size_change * Direction_body_size_change + (1|Order), data = island_pops_ext, family=binomial(link="logit")) 

glmer6 <- glmer(Pr_threatened ~ Magnitude_body_size_change * Direction_body_size_change + Body_mass_island_taxon_scaled + Endemism + (1|Order), data = island_pops_ext, family=binomial(link="logit")) 

glmer7 <- glmer(Pr_threatened ~ Magnitude_body_size_change * Endemism + (1|Order), data = island_pops_ext, family=binomial(link="logit")) 

glmer8 <- glmer(Pr_threatened ~ Magnitude_body_size_change * Direction_body_size_change + Endemism + (1|Order), data = island_pops_ext, family=binomial(link="logit")) 

glmer9 <- glmer(Pr_threatened ~ Magnitude_body_size_change * Direction_body_size_change * Endemism + (1|Order), data = island_pops_ext, family=binomial(link="logit")) 

#Visualize results and save table as doc file

tab_model(null, glmer1, glmer2, glmer3, glmer4, glmer5, glmer6, glmer7, glmer8, glmer9, show.aicc = TRUE, file = "Results/GLMMs/Table_models_pops_threatened.doc")

#Check VIF 
vif(glmer3) 
vif(glmer4) 
vif(glmer5) 
vif(glmer6)
vif(glmer7) 
vif(glmer8) 
vif(glmer9) 

#Perform model diagnostics with DHARMa (best model)
glmer.sim <- simulateResiduals(glmer8, integerResponse = TRUE, n = 250)
plot(glmer.sim)
testDispersion(glmer.sim) 

##############################################################
# Plot models                                             ####
############################################################## 

#Define colours
nb.cols <- 18
mycolors <- colorRampPalette(brewer.pal(12, "Paired"))(nb.cols)

#Plot single predictor model Pr_threatened ~ Magnitude

predicted_magnitude<-ggpredict(glmer1, "Magnitude_body_size_change [all]", type = "fixed") 

predicted_random <- ggpredict(glmer1, terms = c("Magnitude_body_size_change [all]", "Order"), type = "random")

Magnitude_model <- ggplot(predicted_random, aes(x = x, y = predicted, group = group)) +
  geom_line(colour = "darkgrey", size = 0.8, alpha = .5) +
  geom_line(data = subset(predicted_random, group == 'Primates'), 
            size = 0.8, color = "#c6473e") +
  geom_line(data = subset(predicted_random, group == 'Scandentia'), 
            size = 0.8, color = "#ee8c3d") +
  geom_rug(data = island_pops_ext[island_pops_ext$Pr_threatened=="nonthreatened",], aes(x = Magnitude_body_size_change), colour = "darkgrey", alpha = .5, inherit.aes = FALSE, sides = "b")+
  geom_rug(data = island_pops_ext[island_pops_ext$Pr_threatened=="threatened",], aes(x = Magnitude_body_size_change), colour = "darkgrey",  alpha = .5, sides = "t", inherit.aes = FALSE)+
  labs(x = "Magnitude of body size change", y = "P(threatened)") +
  coord_cartesian(ylim = c(0, 1))+
  theme(axis.title = element_text(family = "Arial", size = 18, colour = "grey40"),
        axis.text.x = element_text(family = "Arial", size = 14),
        axis.text.y = element_text(family = "Arial", size = 14),
        panel.grid = element_blank()) +
  geom_line(data = predicted_magnitude, aes(x = x, y = predicted), inherit.aes = FALSE, stat = 'identity', colour = "#f6c564", linetype="dotted", size = 2) +
  geom_ribbon(data = predicted_magnitude, aes(x = x, ymin = conf.low, ymax = conf.high), fill = "#f6c564", inherit.aes = FALSE, alpha = .2)

Magnitude_model

#Save figure in pdf
ggsave(Magnitude_model, filename = "P(threatened)_vs_Magnitude_pops.pdf", path = "/Results/GLMMs", width = 6, height = 6, device = cairo_pdf)

#Plot single predictor model Pr_threatened ~ Magnitude: one panel for each Order

Magnitude_model_orders <- visreg(glmer1, "Magnitude_body_size_change", by = "Order", re.form=(~1|Order), scale = "response", xlab="Magnitude of body size change", ylab="P(threatened)", overlay=TRUE, gg = TRUE, line=list(size=0.8)) + 
  guides(fill="none") + #empty the filled legend
  scale_color_manual(values = mycolors) +
  facet_wrap(~Order, scales='free', nrow = 5)+
  coord_cartesian(ylim = c(0, 1))+
  theme(axis.title = element_text(family = "Arial", size = 18, colour = "grey40"),
        axis.text.x = element_text(family = "Arial", size = 14),
        axis.text.y = element_text(family = "Arial", size = 14),
        panel.grid = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill="white", size=1.5, linetype="solid"),
        strip.text.x = element_text(size = 14, family = "Arial", colour = "grey40", face = "bold"))

Magnitude_model_orders

#Save figure in pdf
ggsave(Magnitude_model_orders, filename = "P(threatened)_vs_Magnitude_orders_pops.pdf", path = "/Results/GLMMs", width = 10, height = 12, device = cairo_pdf)

#Plot effect of interaction endemism*direction based on the model with magnitude*direction*endemism

predicted <- ggpredict(glmer15, terms = c("Magnitude_body_size_change [all]", "Direction_body_size_change", "Endemism"), type = "fixed")

predicted

# New facet label names for Endemism variable
endemism.labs <- c("endemic taxa", "non-endemic taxa")
names(endemism.labs) <- c("yes", "no")

Magnitude_dir_end_model <- ggplot(data = island_pops_ext, aes(Magnitude_size_change, Pr_threatened, colour = Direction_body_size_change)) +
                           scale_color_manual(values = c("#ee8c3d", "#f6c564"))+
                           scale_fill_manual(values = c("#ee8c3d", "#f6c564"))+
                           labs(x = "Magnitude of body size change", y = "P(threatened)") +
                           geom_line(data = predicted, aes(x = x, y = predicted, group = group, color = group), show.legend=FALSE, inherit.aes = FALSE, size = 2) +
                           geom_ribbon(data = predicted, aes(x = x, ymin = conf.low, ymax = conf.high, group = group, fill = group), inherit.aes = FALSE, show.legend=FALSE, alpha = .1)+
                           geom_rug(data = island_pops_ext[island_pops_ext$Pr_threatened=="nonthreatened",], aes(x = Magnitude_body_size_change, colour = Direction_body_size_change), inherit.aes = FALSE, sides = "b")+
                           geom_rug(data = island_pops_ext[island_pops_ext$Pr_threatened=="threatened",], aes(x = Magnitude_body_size_change, colour = Direction_body_size_change), sides = "t", inherit.aes = FALSE)+
                           coord_cartesian(ylim = c(0, 1))+
                           theme(axis.title = element_text(family = "Arial", size = 18, colour = "grey40"),
                                 axis.text.x = element_text(family = "Arial", size = 14),
                                 axis.text.y = element_text(family = "Arial", size = 14),
                                 panel.grid = element_blank(),
                                 legend.position="none",
                                 strip.background = element_rect(fill="white", size=1.5, linetype="solid"),
                                 strip.text.x = element_text(size = 14, family = "Arial", colour = "grey40", face = "bold"))+
                           facet_wrap(~facet, nrow = 2, labeller = labeller(facet = endemism.labs))

#Save figure in pdf
ggsave(Magnitude_dir_end_model, filename = "Magnitude_Direction_Endemism_pops.pdf", path = "/Results/GLMMs", width = 3, height = 6, device = cairo_pdf) 

#####################################################################################################
# Sensitivity analysis:                                                                          ####
# run GLMMs again with populations that experienced no substantial body size change included     ####
#####################################################################################################

island_pops_ext_no_change_included <- read.csv('Data/Extant_mammals_populations.csv')

island_pops_ext_no_change_included <- filter(island_pops_ext_no_change_included, IUCN_Category != "not_assessed_yet")

island_pops_ext_no_change_included <- filter(island_pops_ext_no_change_included, Size_ratio != "") 

island_pops_ext_no_change_included <- filter(island_pops_ext_no_change_included, Body_mass_island_taxon != "") 

#Reorder IUCN_Category from low to high extinction risk
island_pops_ext_no_change_included <- island_pops_ext_no_change_included %>%
  mutate(IUCN_Category =
           fct_relevel(IUCN_Category,
                       "LC",
                       "NT",
                       "VU",
                       "EN",
                       "CR"))

#Create threatened vs nonthreatened column
island_pops_ext_no_change_included <- island_pops_ext_no_change_included %>%
  mutate(Pr_threatened = recode_factor(IUCN_Category,
                                       CR = "threatened",
                                       EN = "threatened",
                                       VU = "threatened",
                                       NT = "nonthreatened",
                                       LC = "nonthreatened"))

#Reorder threatened vs nonthreatened column from low to high extinction risk
island_pops_ext_no_change_included <- island_pops_ext_no_change_included %>%
  mutate(Pr_threatened =
           fct_relevel(Pr_threatened,
                       "nonthreatened",
                       "threatened"))

#Fit GLMMs

island_pops_ext_no_change_included$Body_mass_island_taxon_scaled <- scale(island_pops_ext_no_change_included$Body_mass_island_taxon) 

null_b <- glmer(Pr_threatened ~ (1|Order), data = island_pops_ext_no_change_included, family=binomial(link="logit"))

glmer1_b <- glmer(Pr_threatened ~ Magnitude_body_size_change + (1|Order), data = island_pops_ext_no_change_included, family=binomial(link="logit"))

glmer2_b <- glmer(Pr_threatened ~ Body_mass_island_taxon_scaled + (1|Order), data = island_pops_ext_no_change_included, family=binomial(link="logit"))

glmer3_b <- glmer(Pr_threatened ~ Magnitude_body_size_change + Body_mass_island_taxon_scaled + (1|Order), data = island_pops_ext_no_change_included, family=binomial(link="logit")) 

glmer4_b <- glmer(Pr_threatened ~ Magnitude_body_size_change * Body_mass_island_taxon_scaled + (1|Order), data = island_pops_ext_no_change_included, family=binomial(link="logit")) 

glmer5_b <- glmer(Pr_threatened ~ Magnitude_body_size_change * Direction_body_size_change + (1|Order), data = island_pops_ext_no_change_included, family=binomial(link="logit")) 

glmer6_b <- glmer(Pr_threatened ~ Magnitude_body_size_change * Direction_body_size_change + Body_mass_island_taxon_scaled + Endemism + (1|Order), data = island_pops_ext_no_change_included, family=binomial(link="logit")) 

glmer7_b <- glmer(Pr_threatened ~ Magnitude_body_size_change * Endemism + (1|Order), data = island_pops_ext_no_change_included, family=binomial(link="logit")) 

glmer8_b <- glmer(Pr_threatened ~ Magnitude_body_size_change * Direction_body_size_change + Endemism + (1|Order), data = island_pops_ext_no_change_included, family=binomial(link="logit")) 

glmer9_b <- glmer(Pr_threatened ~ Magnitude_body_size_change * Direction_body_size_change * Endemism + (1|Order), data = island_pops_ext_no_change_included, family=binomial(link="logit")) 

#Results

tab_model(null_b, glmer1_b, glmer2_b, glmer3_b, glmer4_b, glmer5_b, glmer6_b, glmer7_b, glmer8_b, glmer9_b, show.aicc = TRUE, file = "Results/GLMMs/Table_models_pops_threatened_no_change_included.doc")

#End of script