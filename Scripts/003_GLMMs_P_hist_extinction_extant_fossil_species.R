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

island_species_extant <- select(island_species_extant, -c("Source_Body_mass_and_size_ratio", "Source_Ancestor_or_closest_mainland_relative"))

island_species_fossil <- read.csv('Data/Fossil_mammals_species.csv')

island_species_fossil <- select(island_species_fossil, -c("Source"))

#Combine extant and fossil datasets

island_species_all <- rbind(island_species_extant, island_species_fossil)

#remove taxa with not available BM and size ratio
island_species_all <- filter(island_species_all, Body_mass_island_taxon != "") 
island_species_all <- filter(island_species_all, Size_ratio != "") 
#remove taxa that experienced no substantial size change
island_species_all <- filter(island_species_all, Direction_body_size_change != "no_change")

#Dataset with only historical extinctions
island_species_historical_ext <- filter(island_species_all, IUCN_Category != "EP") 

#Create extinct vs extant column
island_species_historical_ext <- island_species_historical_ext %>%
  mutate(Pr_hist_extinction = recode_factor(IUCN_Category,
                                EX = "extinct",
                                CR = "extant",
                                EN = "extant",
                                VU = "extant",
                                NT = "extant",
                                LC = "extant"))

#Reorder hist extinct vs extant column from low to high extinction risk
island_species_historical_ext <- island_species_historical_ext %>%
  mutate(Pr_hist_extinction =
           fct_relevel(Pr_hist_extinction,
                       "extant",
                       "extinct"))

#Scale body mass
island_species_historical_ext$Body_mass_island_taxon_scaled <- scale(island_species_historical_ext$Body_mass_island_taxon) 

##############################################################
# Fit GLMMs                                               ####
##############################################################

null <- glmer(Pr_hist_extinction ~ (1|Order), data = island_species_historical_ext, family=binomial(link="logit"))
glmer1 <- glmer(Pr_hist_extinction ~ Magnitude_body_size_change + (1|Order), data = island_species_historical_ext, family=binomial(link="logit"))
glmer2 <- glmer(Pr_hist_extinction ~ Body_mass_island_taxon_scaled + (1|Order), data = island_species_historical_ext, family=binomial(link="logit"))
glmer3 <- glmer(Pr_hist_extinction ~ Magnitude_body_size_change + Body_mass_island_taxon_scaled + (1|Order), data = island_species_historical_ext, family=binomial(link="logit")) 
glmer4 <- glmer(Pr_hist_extinction ~ Magnitude_body_size_change * Body_mass_island_taxon_scaled + (1|Order), data = island_species_historical_ext, family=binomial(link="logit")) 
glmer5 <- glmer(Pr_hist_extinction ~ Magnitude_body_size_change * Direction_body_size_change + (1|Order), data = island_species_historical_ext, family=binomial(link="logit")) 
glmer6 <- glmer(Pr_hist_extinction ~ Magnitude_body_size_change * Direction_body_size_change + Body_mass_island_taxon_scaled + (1|Order), data = island_species_historical_ext, family=binomial(link="logit")) 

#Visualize results and save table as doc file

tab_model(null, glmer1, glmer2, glmer3, glmer4, glmer5, glmer6, show.aicc = TRUE, file = "Results/GLMMs/Table_models_hist_extinct.doc")

##############################################################
# Plot models                                             ####
##############################################################

extrafont::loadfonts(device = "win")

theme_set(theme_light(base_size = 12, base_family = "Arial"))

#Define colours
nb.cols <- 19
mycolors <- colorRampPalette(brewer.pal(12, "Paired"))(nb.cols)

#Plot single predictor model Pr_hist_extinction ~ Magnitude

predicted_magnitude<-ggpredict(glmer1, "Magnitude_body_size_change [all]", type = "fixed") 

predicted_random <- ggpredict(glmer1, terms = c("Magnitude_body_size_change [all]", "Order"), type = "random")

Magnitude_model <- ggplot(predicted_random, aes(x = x, y = predicted, group = group)) +
  geom_line(colour = "darkgrey", size = 0.8, alpha = .5) +
  geom_line(data = subset(predicted_random, group == 'Rodentia'), 
            size = 0.8, color = "#9e728d") +
  geom_line(data = subset(predicted_random, group == 'Eulipotyphla'), 
            size = 0.8, color = "#e7b9ca") +
  geom_rug(data = island_species_historical_ext[island_species_historical_ext$Pr_hist_extinction=="extant",], aes(x = Magnitude_body_size_change), colour = "darkgrey", alpha = .5, inherit.aes = FALSE, sides = "b")+
  geom_rug(data = island_species_historical_ext[island_species_historical_ext$Pr_hist_extinction=="extinct",], aes(x = Magnitude_body_size_change), colour = "darkgrey",  alpha = .5, sides = "t", inherit.aes = FALSE)+
  labs(x = "Magnitude of body size change", y = "P(extinct)") +
  theme(axis.title = element_text(family = "Arial", size = 18, colour = "grey40"),
        axis.text.x = element_text(family = "Arial", size = 14),
        axis.text.y = element_text(family = "Arial", size = 14),
        panel.grid = element_blank()) +
  geom_line(data = predicted_magnitude, aes(x = x, y = predicted), inherit.aes = FALSE, stat = 'identity', colour = "#f6c564", linetype="dotted", size = 2) +
  geom_ribbon(data = predicted_magnitude, aes(x = x, ymin = conf.low, ymax = conf.high), fill = "#f6c564", inherit.aes = FALSE, alpha = .2)

Magnitude_model

#Save figure in pdf
ggsave(Magnitude_model, filename = "P(hist_extinct)_vs_Magnitude.pdf", path = "Results/GLMMs", width = 6, height = 6, device = cairo_pdf)

#Direction
#Plot direction based on the model magnitude*direction

predictionDirsimple<-ggpredict(glmer5, "Direction_body_size_change [all]", type = "fixed") 

predictionDirsimple

Direction_vs_P_hist_extinct <- ggplot(predictionDirsimple, aes(x, predicted)) +
  geom_pointrange(aes(x = x, ymin = conf.low, ymax = conf.high, shape = x), size = 1.5, colour = "black", position = position_dodge(3))+
  guides(fill="none") + #empty the filled legend
  coord_cartesian(ylim = c(0, 1))+
  theme(axis.title = element_text(family = "Arial", size = 18, colour = "grey40"),
        axis.text.x = element_text(family = "Arial", size = 14),
        axis.text.y = element_text(family = "Arial", size = 14),
        panel.grid = element_blank(),
        legend.position = "none") +
  labs(x = "Direction of body size change", y = "P(hist_extinct)")

Direction_vs_P_hist_extinct

ggsave(Direction_vs_P_hist_extinct, filename = "P(hist_extinct)_vs_Direction.pdf", path = "Results/GLMMs", width = 4, height = 4, device = cairo_pdf) 

#Plot single predictor model Pr_hist_extinction ~ Magnitude: one panel for each Order

Magnitude_model_orders <- visreg(glmer1, "Magnitude_body_size_change", by = "Order", re.form=(~1|Order), scale = "response", xlab="Magnitude of body size change", ylab="P(hist_extinct)", overlay=TRUE, gg = TRUE, line=list(size=0.8)) + 
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
ggsave(Magnitude_model_orders, filename = "P(hist_extinct)_vs_Magnitude_orders.pdf", path = "Results/GLMMs", width = 10, height = 12, device = cairo_pdf)

##################################################################################################
# Plot raw data: BM bins vs Magnitude_body_size_change for hist extinct vs extant species     ####
##################################################################################################

breaks <- c(0, 0.1, 1, 10, 100, 1000, 10000, 100000,1000000, 10000000, 100000000, 1000000000)

island_species_historical_ext$Body_mass_island_taxon <- as.numeric(island_species_historical_ext$Body_mass_island_taxon)

# bucketing values into bins
group_tags <- cut(island_species_historical_ext$Body_mass_island_taxon, 
                   breaks=breaks, 
                   include.lowest=TRUE, 
                   right=FALSE)
# inspect bins
summary(group_tags)

BM_groups <- factor(group_tags, ordered = TRUE) 

# Plot 

Body_size_classes_vs_P_hist_extinct <- ggplot(island_species_historical_ext, aes(x = BM_groups, y = Magnitude_body_size_change, fill = Pr_hist_extinction, color = Pr_hist_extinction)) + 
  stat_summary(fun.data=mean_sdl, fun.args = list(mult = 1), size = 1, geom="pointrange", shape = 15, position = position_dodge(0.75))+
  geom_jitter(size = 2.5, alpha = 0.1, position = position_jitterdodge(0.5), set.seed(123)) +
  scale_colour_manual(values = c("#89b790","#06485e"))+
  labs(x = "Body size (kg)", y = "Magnitude of body size change") +
  coord_flip() +
  theme(axis.title = element_text(family = "Arial", size = 12, colour = "grey40"),
        axis.text.x = element_text(family = "Arial", size = 12),
        axis.text.y = element_text(family = "Arial", size = 12),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(family = "Arial", size = 12, colour = "grey40"))

Body_size_classes_vs_P_hist_extinct

#Save figure in pdf
ggsave(Body_size_classes_vs_P_hist_extinct, filename = "Body_size_classes_vs_P_hist_extinct.pdf", path = "Results/Raw_data_plots", width = 6, height = 6, device = cairo_pdf) 

#End of script
