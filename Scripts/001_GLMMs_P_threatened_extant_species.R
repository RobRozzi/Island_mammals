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

island_species_extant <- read.csv('Data/Extant_mammals_species.csv')

#remove taxa with not available size ratio
island_species_extant <- filter(island_species_extant, Size_ratio != "")
#remove taxa that experienced no substantial size change (6)
island_species_extant <- filter(island_species_extant, Direction_body_size_change != "no_change") 
                       
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

Direction_vs_P_threatened

#Save figure in pdf
ggsave(Direction_vs_P_threatened, filename = "P(threatened)_vs_Direction.pdf", path = "Results/GLMMs", width = 4, height = 4, device = cairo_pdf) 

#####################################################################################################
# Plot raw data: BM bins vs Magnitude_body_size_change for threatened vs nonthreatened species   ####
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
  coord_flip(ylim = c(NA, 1.2)) +
  theme(axis.title = element_text(family = "Arial", size = 12, colour = "grey40"),
        axis.text.x = element_text(family = "Arial", size = 12),
        axis.text.y = element_text(family = "Arial", size = 12),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(family = "Arial", size = 12, colour = "grey40"))

Body_size_classes_vs_P_threatened

#Save figure in pdf
ggsave(Body_size_classes_vs_P_threatened, filename = "Body_size_classes_vs_P_threatened.pdf", path = "Results/Raw_data_plots", width = 6, height = 6, device = cairo_pdf)


#####################################################################################################
# Sensitivity analysis:                                                                          ####
# run GLMMs again with species that experienced no substantial body size change included         ####
#####################################################################################################

island_species_extant_no_change_included <- read.csv('Data/Extant_mammals_species.csv')

island_species_extant_no_change_included <- filter(island_species_extant_no_change_included, Size_ratio != "")

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

#Results

tab_model(null_b, glmer1_b, glmer2_b, glmer3_b, glmer4_b, glmer5_b, glmer6_b, show.aicc = TRUE, file = "Results/GLMMs/Table_models_threatened_no_change_included.doc")

#End of script
