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
library(RColorBrewer)
library(ggeffects)
library(visreg)
library(extrafont)
library(Cairo)

##############################################################
# Load data                                               ####
##############################################################

## I would simply name the dataset "data" or sth similar that is short, as you will save time typing the full name, which is now rather long ----
island_species_extant <- read.csv('Data/Extant_mammals_species.csv')

#remove taxa with not available size ratio
island_species_extant <- filter(island_species_extant, Size_ratio != "")
#remove taxa that experienced no substantial size change (6)
island_species_extant <- filter(island_species_extant, Direction_body_size_change != "no_change") 

## Why are there rows with no species names in the Island Ancestor_or_closest_mainland_relative column?? ----

#Reorder IUCN_Category from low to high extinction risk
## You don't need these lines of code here (to order this factor) if you are doing the analysis for threatened vs non-threatened species ----

# island_species_extant <- island_species_extant %>%
#   mutate(IUCN_Category =
#            fct_relevel(IUCN_Category,
#                        "LC",
#                        "NT",
#                        "VU",
#                        "EN",
#                        "CR"))
                       
#Create threatened vs nonthreatened column 
island_species_extant <- island_species_extant %>%
  mutate(Pr_threatened = recode_factor(IUCN_Category,
                                CR = "threatened",
                                EN = "threatened",
                                VU = "threatened",
                                NT = "nonthreatened",
                                LC = "nonthreatened"))

#Reorder threatened vs nonthreatened column from low to high extinction risk
island_species_extant <- island_species_extant %>%
  mutate(Pr_threatened =
           fct_relevel(Pr_threatened,
                       "nonthreatened",
                       "threatened"))

#Scale body mass
island_species_extant$Body_mass_island_taxon_scaled <- scale(island_species_extant$Body_mass_island_taxon) 

##############################################################
# Fit GLMMs                                               ####
############################################################## 

null <- glmer(Pr_threatened ~ (1|Order), data = island_species_extant, family=binomial(link="logit"))
glmer1 <- glmer(Pr_threatened ~ Magnitude_body_size_change + (1|Order), data = island_species_extant, family=binomial(link="logit"))
glmer2 <- glmer(Pr_threatened ~ Body_mass_island_taxon_scaled + (1|Order), data = island_species_extant, family=binomial(link="logit"))
glmer3 <- glmer(Pr_threatened ~ Magnitude_body_size_change + Body_mass_island_taxon_scaled + (1|Order), data = island_species_extant, family=binomial(link="logit")) 
glmer4 <- glmer(Pr_threatened ~ Magnitude_body_size_change * Body_mass_island_taxon_scaled + (1|Order), data = island_species_extant, family=binomial(link="logit")) 
glmer5 <- glmer(Pr_threatened ~ Magnitude_body_size_change * Direction_body_size_change + (1|Order), data = island_species_extant, family=binomial(link="logit")) 
glmer6 <- glmer(Pr_threatened ~ Magnitude_body_size_change * Direction_body_size_change + Body_mass_island_taxon_scaled + (1|Order), data = island_species_extant, family=binomial(link="logit")) 

#Visualize results and save table as doc file
tab_model(null, glmer1, glmer2, glmer3, glmer4, glmer5, glmer6, show.aicc = TRUE, file = "Results/GLMMs/Table_models_threatened.doc")

# Check best model based on AIC and BIC ----
AIC(null, glmer1, glmer2, glmer3, glmer4, glmer5, glmer6) # glmer3, followed by glmer4
BIC(null, glmer1, glmer2, glmer3, glmer4, glmer5, glmer6) # glmer1 and glmer3 similarly plausible

##############################################################
# Plot models                                             ####
############################################################## 

extrafont::loadfonts(device = "win")
theme_set(theme_light(base_size = 12, base_family = "Arial"))

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
  geom_line(data = subset(predicted_random, group == 'Didelphimorphia'), 
            size = 0.8, color = "#ee8c3d") +
  geom_rug(data = island_species_extant[island_species_extant$Pr_threatened=="nonthreatened",], aes(x = Magnitude_body_size_change), colour = "darkgrey", alpha = .5, inherit.aes = FALSE, sides = "b")+
  geom_rug(data = island_species_extant[island_species_extant$Pr_threatened=="threatened",], aes(x = Magnitude_body_size_change), colour = "darkgrey",  alpha = .5, sides = "t", inherit.aes = FALSE)+
  labs(x = "Magnitude of body size change", y = "P(threatened)") +
  theme(axis.title = element_text(family = "Arial", size = 16, colour = "grey40"),
        axis.text.x = element_text(family = "Arial", size = 12),
        axis.text.y = element_text(family = "Arial", size = 12),
        panel.grid = element_blank()) +
  geom_line(data = predicted_magnitude, aes(x = x, y = predicted), inherit.aes = FALSE, stat = 'identity', colour = "#f6c564", linetype="dotted", size = 2) +
  geom_ribbon(data = predicted_magnitude, aes(x = x, ymin = conf.low, ymax = conf.high), fill = "#f6c564", inherit.aes = FALSE, alpha = .2)

Magnitude_model

#Save figure in pdf
ggsave(Magnitude_model, filename = "P(threatened)_vs_Magnitude.pdf", path = "Results/GLMMs", width = 14, height = 8.5, device = cairo_pdf)

#Plot single predictor model Pr_threatened ~ Magnitude: one panel for each Order

Magnitude_model_orders <- visreg(glmer1, "Magnitude_body_size_change", by = "Order", re.form=(~1|Order), scale = "response", xlab="Magnitude of body size change", ylab="P(threatened)", overlay=TRUE, gg = TRUE, line=list(size=0.8)) + 
  guides(fill="none") + 
  scale_color_manual(values = mycolors) +
  facet_wrap(~Order, scales='free', nrow = 5)+
  coord_cartesian(ylim = c(0, 1))+
  theme(axis.title = element_text(family = "Arial", size = 12, colour = "grey40"),
        axis.text.x = element_text(family = "Arial", size = 8),
        axis.text.y = element_text(family = "Arial", size = 8),
        panel.grid = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill="white", size=1.5, linetype="solid"),
        strip.text.x = element_text(size = 12, family = "Arial", colour = "grey40", face = "bold"))

Magnitude_model_orders

# For this plot it would be great to have the CI as well, besides the predicted lines ------------

#Save figure in pdf
ggsave(Magnitude_model_orders, filename = "P(threatened)_vs_Magnitude_orders.pdf", path = "Results/GLMMs", width = 10, height = 12, device = cairo_pdf) 

#Plot Pr_threatened ~ Direction based on the model magnitude*direction

predictionDirsimple<-ggpredict(glmer5, "Direction_body_size_change [all]", type = "fixed") 

predictionDirsimple

Direction_vs_P_threatened <- ggplot(predictionDirsimple, aes(x, predicted)) +
  geom_pointrange(aes(x = x, ymin = conf.low, ymax = conf.high, shape = x), size = 1.5, colour = "black", position = position_dodge(3))+
  guides(fill="none") +
  coord_cartesian(ylim = c(0, 1))+
  theme(axis.title = element_text(family = "Arial", size = 18, colour = "grey40"),
        axis.text.x = element_text(family = "Arial", size = 14),
        axis.text.y = element_text(family = "Arial", size = 14),
        panel.grid = element_blank(),
        legend.position = "none") +
  labs(x = "Direction of body size change", y = "P(threatened)")

# Similar effect size and uncertainty for both directions of change ----- 
Direction_vs_P_threatened

#Save figure in pdf
ggsave(Direction_vs_P_threatened, filename = "P(threatened)_vs_Direction.pdf", path = "Results/GLMMs", width = 4, height = 4, device = cairo_pdf) 

#####################################################################################################
# Plot raw data: BM bins vs Magnitude_body_size_change for threatened vs nonthreatened species    ###
#####################################################################################################

breaks <- c(0, 0.1, 1, 10, 100, 1000, 10000, 100000,1000000, 10000000, 100000000, 1000000000)

island_species_extant$Body_mass_island_taxon <- as.numeric(island_species_extant$Body_mass_island_taxon)

#bucketing values into bins
group_tags <- cut(island_species_extant$Body_mass_island_taxon, 
                   breaks=breaks, 
                   include.lowest=TRUE, 
                   right=FALSE)
#inspect bins
summary(group_tags)

BM_groups <- factor(group_tags, ordered = TRUE)

#Plot 

Body_size_classes_vs_P_threatened <- ggplot(island_species_extant, aes(x = BM_groups, y = Magnitude_body_size_change, fill = Pr_threatened, color = Pr_threatened)) + 
  stat_summary(fun.data=mean_sdl, fun.args = list(mult = 1), size = 1, geom="pointrange", shape = 15, position = position_dodge(0.75))+
  geom_jitter(size = 2.5, alpha = 0.1, position = position_jitterdodge(0.5), set.seed(123)) +
  scale_colour_manual(values = c("#f0755d", "#742615"))+
  labs(x = "Body size (kg)", y = "Magnitude of body size change") +
  coord_flip() +
  theme(axis.title = element_text(family = "Arial", size = 12, colour = "grey40"),
        axis.text.x = element_text(family = "Arial", size = 12),
        axis.text.y = element_text(family = "Arial", size = 12),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(family = "Arial", size = 12, colour = "grey40"))

Body_size_classes_vs_P_threatened

#Save figure in pdf
ggsave(Body_size_classes_vs_P_threatened, filename = "Body_size_classes_vs_P_threatened.pdf", path = "Results/Raw_data_plots", width = 6, height = 6, device = cairo_pdf)


## what I am missing here is a plot that depict the results of the best model according to aic -- > glmer3, I guess the plot just above is meant to do that? But it is based on the empirical data, not predictions...----
# I gave it a try here below but it is definitely less appealing than the one based on empirical data.
pred_mag <- ggpredict(glmer3, terms = c("Magnitude_body_size_change [all]", "Body_mass_island_taxon_scaled[-0.11, 0.5]"), type = "fixed") 

best_model <- ggplot(pred_mag) +
  geom_line(aes(x = x, y = predicted, colour = group)) + 
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = .2) +
  geom_rug(data = island_species_extant[island_species_extant$Pr_threatened=="nonthreatened",], aes(x = Magnitude_body_size_change), colour = "darkgrey", alpha = .5, inherit.aes = FALSE, sides = "b")+
  geom_rug(data = island_species_extant[island_species_extant$Pr_threatened=="threatened",], aes(x = Magnitude_body_size_change), colour = "darkgrey",  alpha = .5, sides = "t", inherit.aes = FALSE)+
  labs(x = "Magnitude of body size change", y = "P(threatened)") +
  theme(axis.title = element_text(family = "Arial", size = 16, colour = "grey40"),
        axis.text.x = element_text(family = "Arial", size = 12),
        axis.text.y = element_text(family = "Arial", size = 12),
        panel.grid = element_blank())
 
best_model

## as I can see, the effect of body mass is really small. And if we drop it we will get model glmer1. I guess that's why you focused on that model for showing the results ----
## maybe you can just use BIC to select the model or follow the same rationale that I gave here. Probably you already do in the paper, gotta read the last version :)


#####################################################################################################
# Sensitivity analysis:                                                                          ####
# run GLMMs again with species that experienced no substantial body size change included         ####
#####################################################################################################

island_species_extant_no_change_included <- read.csv('Data/Extant_mammals_species.csv')

island_species_extant_no_change_included <- filter(island_species_extant_no_change_included, Size_ratio != "")

#Reorder IUCN_Category from low to high extinction risk
## Again, you don't need these lines of code here (to order this factor) if you are doing the analysis for threatened vs non-threatened species ----

# island_species_extant_no_change_included <- island_species_extant_no_change_included %>%
#   mutate(IUCN_Category =
#            fct_relevel(IUCN_Category,
#                        "LC",
#                        "NT",
#                        "VU",
#                        "EN",
#                        "CR"))

#Create threatened vs nonthreatened column 
island_species_extant_no_change_included <- island_species_extant_no_change_included %>%
  mutate(Pr_threatened = recode_factor(IUCN_Category,
                                CR = "threatened",
                                EN = "threatened",
                                VU = "threatened",
                                NT = "nonthreatened",
                                LC = "nonthreatened"))

#Reorder threatened vs nonthreatened column from low to high extinction risk
island_species_extant_no_change_included <- island_species_extant_no_change_included %>%
  mutate(Pr_threatened =
           fct_relevel(Pr_threatened,
                       "nonthreatened",
                       "threatened"))

#Fit GLMMs

island_species_extant_no_change_included$Body_mass_island_taxon_scaled <- scale(island_species_extant_no_change_included$Body_mass_island_taxon) #I scale (= normalizzo) BM

null_b <- glmer(Pr_threatened ~ (1|Order), data = island_species_extant_no_change_included, family=binomial(link="logit"))

glmer1_b <- glmer(Pr_threatened ~ Magnitude_body_size_change + (1|Order), data = island_species_extant_no_change_included, family=binomial(link="logit"))

glmer2_b <- glmer(Pr_threatened ~ Body_mass_island_taxon_scaled + (1|Order), data = island_species_extant_no_change_included, family=binomial(link="logit"))

glmer3_b <- glmer(Pr_threatened ~ Magnitude_body_size_change + Body_mass_island_taxon_scaled + (1|Order), data = island_species_extant_no_change_included, family=binomial(link="logit")) 

glmer4_b <- glmer(Pr_threatened ~ Magnitude_body_size_change * Body_mass_island_taxon_scaled + (1|Order), data = island_species_extant_no_change_included, family=binomial(link="logit")) 

glmer5_b <- glmer(Pr_threatened ~ Magnitude_body_size_change * Direction_body_size_change + (1|Order), data = island_species_extant_no_change_included, family=binomial(link="logit")) 

glmer6_b <- glmer(Pr_threatened ~ Magnitude_body_size_change * Direction_body_size_change + Body_mass_island_taxon_scaled + (1|Order), data = island_species_extant_no_change_included, family=binomial(link="logit")) 

## Seems that there is a problem with glmer5_b and glmer6_b --> fixed-effect model matrix is rank deficient so dropping 1 column / coefficient

#Results

tab_model(null_b, glmer1_b, glmer2_b, glmer3_b, glmer4_b, glmer5_b, glmer6_b, show.aicc = TRUE, file = "Results/GLMMs/Table_models_threatened_no_change_included.doc")

# Check best model based on AIC and BIC ----
AIC(null_b, glmer1_b, glmer2_b, glmer3_b, glmer4_b, glmer5_b, glmer6_b) # glmer3_b, followed by glmer4_b
BIC(null_b, glmer1_b, glmer2_b, glmer3_b, glmer4_b, glmer5_b, glmer6_b) # glmer1 and glmer3 similarly plausible

# Diagnostics of best models ----
# https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html
## I havent worked much with this package but I think we should check model diagnostics

library(DHARMa)

# try for the two best models: glmer1 and glmer3
glmer1_diag <- simulateResiduals(fittedModel = glmer1, plot = F)
residuals(glmer1_diag)
plot(glmer1_diag)

plotResiduals(glmer1_diag, form = island_species_extant$Magnitude_body_size_change)
testUniformity(glmer1_diag) # some deviation
testOutliers(glmer1_diag) # no outliers
testDispersion(glmer1_diag) # OK
testQuantiles(glmer1_diag) # quantile deviation

glmer3_diag <- simulateResiduals(fittedModel = glmer3, plot = F)
residuals(glmer3_diag)
plot(glmer3_diag) 

## I think there is an issue with the distribution of Magnitude_body_size_change, which is skewed. 
## Let's log-transform it prior to the modelling. Also body mass

##############################################################
# Fit GLMMs after diagnostics                             ####
############################################################## 
island_species_extant$logMag <- log10(island_species_extant$Magnitude_body_size_change)
island_species_extant$logBM <- log10(island_species_extant$Body_mass_island_taxon)

null_d <- glmer(Pr_threatened ~ (1|Order), data = island_species_extant, family=binomial(link="logit"))
glmer1_d <- glmer(Pr_threatened ~ logMag + (1|Order), data = island_species_extant, family=binomial(link="logit"))
glmer2_d <- glmer(Pr_threatened ~ logBM + (1|Order), data = island_species_extant, family=binomial(link="logit"))
glmer3_d <- glmer(Pr_threatened ~ logMag + logBM + (1|Order), data = island_species_extant, family=binomial(link="logit")) 
glmer4_d <- glmer(Pr_threatened ~ logMag * logBM + (1|Order), data = island_species_extant, family=binomial(link="logit")) 
glmer5_d <- glmer(Pr_threatened ~ logMag * Direction_body_size_change + (1|Order), data = island_species_extant, family=binomial(link="logit")) 
glmer6_d <- glmer(Pr_threatened ~ logMag * Direction_body_size_change + logBM + (1|Order), data = island_species_extant, family=binomial(link="logit")) 

#Visualize results and save table as doc file
tab_model(null_d, glmer1_d, glmer2_d, glmer3_d, glmer4_d, glmer5_d, glmer6_d, show.aicc = TRUE, file = "Results/GLMMs/Table_models_threatened_diag.doc")

# Check best model based on AIC and BIC ----
# similar results as before
AIC(null_d, glmer1_d, glmer2_d, glmer3_d, glmer4_d, glmer5_d, glmer6_d) # glmer3, followed by glmer4
BIC(null_d, glmer1_d, glmer2_d, glmer3_d, glmer4_d, glmer5_d, glmer6_d) # glmer1 and glmer3 similarly plausible

# Let's check model diagnostics

glmer1_diag <- simulateResiduals(fittedModel = glmer1_d, plot = F)
# residuals(glmer1_diag)
plot(glmer1_diag) #looks better, a bit flatter and less pattern-y than before

plotResiduals(glmer1_diag, form = island_species_extant$logMag) # definitely better
testUniformity(glmer1_diag) # some deviation
testOutliers(glmer1_diag) # Some outlier?
testDispersion(glmer1_diag) # OK
testQuantiles(glmer1_diag) # quantile deviation

glmer3_diag <- simulateResiduals(fittedModel = glmer3_d, plot = F)
# residuals(glmer3_diag)
plot(glmer3_diag) # looks better than model glmer_1. I would use log10 transfomation for Magnitude of size and Body mass

plotResiduals(glmer3_diag, form = island_species_extant$logMag) # no pattern for magnitude
plotResiduals(glmer3_diag, form = island_species_extant$logBM) # no problem for body mass
testUniformity(glmer3_diag) # pretty good overall
testOutliers(glmer3_diag) # no outliers
testDispersion(glmer3_diag) # OK
testQuantiles(glmer3_diag) # a small deviation for quantile 0.25, but overall ok. 

## I believe the best model is glmer3_d where both magnitude and body mass have been log10 transformed. Your conclusions will not change radically, but now your model structure is better defined :) ----
# let's plot it

pred_mag <- ggpredict(glmer3_d, terms = c("logMag [all]", "logBM[1,3,5]"), type = "fixed") 

# note: 1 = 10^1 = 10 g, 3 = 10^3 g, 5 = 10^5 g

best_model <- ggplot(pred_mag) +
  geom_line(aes(x = x, y = predicted, colour = group)) + 
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = .2) +
  geom_rug(data = island_species_extant[island_species_extant$Pr_threatened=="nonthreatened",], aes(x = logMag), colour = "darkgrey", alpha = .5, inherit.aes = FALSE, sides = "b")+
  geom_rug(data = island_species_extant[island_species_extant$Pr_threatened=="threatened",], aes(x = logMag), colour = "darkgrey",  alpha = .5, sides = "t", inherit.aes = FALSE)+
  labs(x = "Magnitude of body size change", y = "P(threatened)") +
  theme(axis.title = element_text(family = "Arial", size = 16, colour = "grey40"),
        axis.text.x = element_text(family = "Arial", size = 12),
        axis.text.y = element_text(family = "Arial", size = 12),
        panel.grid = element_blank())

best_model

## Now the effect of body mass is clearer. Species with large body mass that underwent through a major size change have a higher P of being threatened. ----

#End of script
