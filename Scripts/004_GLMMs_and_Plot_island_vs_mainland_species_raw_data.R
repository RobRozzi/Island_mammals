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

##############################################################
# Load data                                               ####
##############################################################

#Import data island species

island_species_extant <- read.csv('Data/Extant_mammals_species.csv')
island_species_extant <- select(island_species_extant, -c("Source_Body_mass_and_size_ratio", "Source_Ancestor_or_closest_mainland_relative"))
island_species_fossil <- read.csv('Data/Fossil_mammals_species.csv')
island_species_fossil <- select(island_species_fossil, -c("Source"))

#Combine extant and fossil datasets

island_species_all <- rbind(island_species_extant, island_species_fossil)
island_species_all <- select(island_species_all, -c("Island","Ancestor_or_closest_mainland_relative", "Size_ratio", 
                                                    "Log_Size_ratio","Body_mass_mainland_taxon", "Magnitude_body_size_change",
                                                    "Direction_body_size_change", "Endemism", "Notes"))

island_species_all <- rename(island_species_all, Body_mass = Body_mass_island_taxon)
island_species_all <- mutate(island_species_all, Island_or_mainland = "island")

#Import data mainland species
mainland_species <- read.csv('Data/Mainland_species_PHYLACINE.csv')

#Combine island and mainland datasets

island_sp_mainlandvsisland <- rbind(island_species_all, mainland_species)
island_sp_mainlandvsisland <- filter(island_sp_mainlandvsisland, IUCN_Category != "DD") 
island_sp_mainlandvsisland <- filter(island_sp_mainlandvsisland, Body_mass != "") 

#Reorder IUCN_Category from low to high extinction risk
island_sp_mainlandvsisland <- island_sp_mainlandvsisland %>%
  mutate(IUCN_Category =
           fct_relevel(IUCN_Category,
                       "LC",
                       "NT",
                       "VU",
                       "EN",
                       "CR",
                       "EW",
                       "EX",
                       "EP"))
                       
#Create extinct vs extant column
island_sp_mainlandvsisland <- island_sp_mainlandvsisland %>%
  mutate(Pr_extinction = recode_factor(IUCN_Category,
                                    EP = "extinct",
                                    EW = "extinct",
                                    EX = "extinct",
                                    CR = "extant",
                                    EN = "extant",
                                    VU = "extant",
                                    NT = "extant",
                                    LC = "extant"))

#Reorder extinct vs extant column from low to high extinction risk
island_sp_mainlandvsisland <- island_sp_mainlandvsisland %>%
  mutate(Pr_extinction =
           fct_relevel(Pr_extinction,
                       "extant",
                       "extinct"))

#########################################################################
# Calculate proportions and numbers of extinct taxa per BM class     ####
#########################################################################

#Create BM bins
island_sp_mainlandvsisland$Body_mass <- as.numeric(island_sp_mainlandvsisland$Body_mass)

breaks <- c(0, 0.1, 1, 10, 100, 1000, 10000, 100000,1000000, 10000000, 100000000, 1000000000)

#bucketing values into bins
island_sp_mainlandvsisland$group_tags <- cut(island_sp_mainlandvsisland$Body_mass, 
                   breaks=breaks, 
                   include.lowest=TRUE, 
                   right=FALSE)
#inspect bins
summary(island_sp_mainlandvsisland$group_tags)

#order bins
island_sp_mainlandvsisland$BM_groups <- factor(island_sp_mainlandvsisland$group_tags, ordered = TRUE)

#Calculate number of extinct taxa per BM class on islands vs mainland

extinct_subset_island_sp_mainlandvsisland <- filter(island_sp_mainlandvsisland, Pr_extinction != "extant") 

#bucketing values into bins
group_tags_extinct <- cut(extinct_subset_island_sp_mainlandvsisland$Body_mass, 
                   breaks=breaks, 
                   include.lowest=TRUE, 
                   right=FALSE)

#inspect bins
summary(group_tags_extinct)

#order bins
BM_groups_extinct <- factor(group_tags_extinct, ordered = TRUE)

#Calculate proportion of total extinct species on the islands vs mainland (per BM class)

prop_extinct <- island_sp_mainlandvsisland %>%
                group_by(BM_groups, Island_or_mainland, Pr_extinction) %>%
                summarise(n = n()) %>%
                mutate(freq = n / sum(n))

#Create a column with counts of extinct and extant species on island and on the mainland per each BM class

island_sp_mainlandvsisland <- island_sp_mainlandvsisland %>%
                group_by(Island_or_mainland, Pr_extinction, BM_groups) %>%
                mutate(number_extinct_per_size_class_island_vs_mainland = length(Pr_extinction))

#Create another subset without extant data

island_sp_mainlandvsisland_only_extinct <- filter(island_sp_mainlandvsisland, Pr_extinction != "extant") 

###################################################################################################
# Plot raw data: percentage and number of extinct taxa per BM class on islands vs mainland     ####
###################################################################################################

extrafont::loadfonts(device = "win")

theme_set(theme_light(base_size = 15, base_family = "Arial"))

plot1 <- ggplot() +
  geom_bar(data = island_sp_mainlandvsisland, aes(x=BM_groups, fill = factor(Pr_extinction)), position = "fill") +
  labs(x = "Body mass (kg)") +
  facet_wrap(~Island_or_mainland, nrow = 2)+
  scale_fill_manual(values = c("#89b790","#06485e"))+
  scale_y_continuous("Percentage extinct", limits = c(0,1), sec.axis = sec_axis(~. *100 , name="Number extinct")) +
  geom_point(data = island_sp_mainlandvsisland_only_extinct, aes(x=BM_groups, y = number_extinct_per_size_class_island_vs_mainland/100), inherit.aes = FALSE, colour = "black", size = 4, shape = "square")+
  geom_line(data = island_sp_mainlandvsisland_only_extinct, aes(x=BM_groups, y = number_extinct_per_size_class_island_vs_mainland/100, group = 1), color = "black", size = 1.5, inherit.aes = FALSE)+
  theme(axis.title = element_text(family = "Arial", size = 18, colour = "grey40"),
        axis.text.x = element_text(family = "Arial", size = 14, angle = 90),
        axis.text.y = element_text(family = "Arial", size = 14),
        axis.title.y.right = element_text(family = "Arial", size = 14, colour = "black", face = "bold"),
        axis.text.y.right = element_text(family = "Arial", size = 14, colour = "black"),
        panel.grid = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(family = "Arial", size = 12, colour = "grey40"),
        strip.background = element_rect(fill="white", size=1.5, linetype="solid"),
        strip.text.x = element_text(size = 12, family = "Arial", colour = "grey40", face = "bold"))

plot1

#Save figure in pdf
ggsave(plot1, filename = "Percentage&number_extinct_per_size_class_island_vs_mainland.pdf", path = "/Results/Raw_data_plots", width = 6, height = 6, device = cairo_pdf)

##############################################################
# Fit GLMM: Pr_extinct and BM islands vs mainland         ####
##############################################################

island_sp_mainlandvsisland$Body_mass_log <- log(island_sp_mainlandvsisland$Body_mass) 

glmer1 <- glmer(Pr_extinction ~ Body_mass_log * Island_or_mainland + (1|Order), data = island_sp_mainlandvsisland, family=binomial(link="logit"))

#Check VIF
vif(glmer1)

#Visualize results and save table as doc file
tab_model(glmer1, show.aicc = TRUE, file = "Results/GLMMs/Table_models_ext_main_vs_isl.doc")

#Perform model diagnostics with DHARMa
glmer.sim <- simulateResiduals(glmer1, integerResponse = TRUE, n = 250)
plot(glmer.sim)
testDispersion(glmer.sim)

#######################
# Plot model       ####
#######################

predicted <- ggpredict(glmer1, terms = c("Body_mass_log [all]", "Island_or_mainland"), type = "fixed")

plot2 <- ggplot(data = island_sp_mainlandvsisland, aes(Body_mass_log, Pr_extinction, colour = Island_or_mainland)) +
         scale_color_manual(values = c("#e1be6d", "#A9A9A9"))+
         scale_fill_manual(values = c("#e1be6d", "#A9A9A9"))+
         labs(x = "Log body mass", y = "P(extinct)") +
         geom_line(data = predicted, aes(x = x, y = predicted, group = group, color = group), show.legend=FALSE, inherit.aes = FALSE, size = 1) +
         geom_ribbon(data = predicted, aes(x = x, ymin = conf.low, ymax = conf.high, group = group, fill = group), inherit.aes = FALSE, show.legend=FALSE, alpha = .1)+
         geom_rug(data = island_sp_mainlandvsisland[island_sp_mainlandvsisland$Pr_extinction=="extinct",], aes(x = Body_mass_log, colour = Island_or_mainland), inherit.aes = FALSE, sides = "t")+
         geom_rug(data = island_sp_mainlandvsisland[island_sp_mainlandvsisland$Pr_extinction=="extant",], aes(x = Body_mass_log, colour = Island_or_mainland), inherit.aes = FALSE, sides = "b")+
         theme(axis.title = element_text(family = "Arial", size = 18, colour = "grey40"),
               axis.text.x = element_text(family = "Arial", size = 14),
               axis.text.y = element_text(family = "Arial", size = 14),
               panel.grid = element_blank(),
               legend.position = "none")

plot2

#Save figure in pdf
ggsave(plot2, filename = "P(extinct)_BM_island_vs_mainland.pdf", path = "/Results/GLMMs", width = 2.5, height = 2.5, device = cairo_pdf) 

###################################################################
# Rerun analyses by including only historical extinctions      ####
###################################################################

island_sp_mainlandvsisland <- filter(island_sp_mainlandvsisland, IUCN_Category != "EP")

#Create extant vs hist_extinct column 
island_sp_mainlandvsisland <- island_sp_mainlandvsisland %>%
  mutate(Pr_hist_extinction = recode_factor(IUCN_Category,
                                            EW = "extinct",
                                            EX = "extinct",
                                            CR = "extant",
                                            EN = "extant",
                                            VU = "extant",
                                            NT = "extant",
                                            LC = "extant"))

#Reorder hist extinct vs extant column from low to high extinction risk
island_sp_mainlandvsisland <- island_sp_mainlandvsisland %>%
  mutate(Pr_hist_extinction =
           fct_relevel(Pr_hist_extinction,
                       "extant",
                       "extinct"))

##############################################################################
# Calculate proportions and numbers of hist extinct taxa per BM class     ####
##############################################################################

hist_extinct_subset_island_sp_mainlandvsisland <- filter(island_sp_mainlandvsisland, Pr_hist_extinction != "extant") #taglia tutte le righe in cui Pr_extinction = extant

#bucketing values into bins
group_tags_hist_extinct <- cut(hist_extinct_subset_island_sp_mainlandvsisland$Body_mass, 
                               breaks=breaks, 
                               include.lowest=TRUE, 
                               right=FALSE)

#inspect bins
summary(group_tags_hist_extinct)

#Order bins
BM_groups_hist_extinct <- factor(group_tags_hist_extinct, ordered = TRUE) 

#Calculate proportion of total hist extinct species on the islands vs mainland (per BM class)
prop_hist_extinct <- island_sp_mainlandvsisland %>%
  group_by(BM_groups, Island_or_mainland, Pr_hist_extinction) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

#Create a column with counts of hist extinct and extant species on island and on the mainland per each BM class

island_sp_mainlandvsisland <- island_sp_mainlandvsisland %>%
  group_by(Island_or_mainland, Pr_hist_extinction, BM_groups) %>%
  mutate(number_hist_extinct_per_size_class_island_vs_mainland = length(Pr_hist_extinction))

#Create another subset without extant data

island_sp_mainlandvsisland_only_hist_extinct <- filter(island_sp_mainlandvsisland, Pr_hist_extinction != "extant") 

########################################################################################################
# Plot raw data: percentage and number of hist extinct taxa per BM class on islands vs mainland     ####
########################################################################################################

plot3 <- ggplot() +
  geom_bar(data = island_sp_mainlandvsisland, aes(x=BM_groups, fill = factor(Pr_hist_extinction)), position = "fill") +
  labs(x = "Body mass (kg)") +
  facet_wrap(~Island_or_mainland, nrow = 2)+
  scale_fill_manual(values = c("#89b790","#06485e"))+
  scale_y_continuous("Percentage hist extinct", limits = c(0,1), sec.axis = sec_axis(~. *100 , name="Number hist extinct")) +
  geom_point(data = island_sp_mainlandvsisland_only_hist_extinct, aes(x=BM_groups, y = number_hist_extinct_per_size_class_island_vs_mainland/100), inherit.aes = FALSE, colour = "black", size = 4, shape = "square")+
  geom_line(data = island_sp_mainlandvsisland_only_hist_extinct, aes(x=BM_groups, y = number_hist_extinct_per_size_class_island_vs_mainland/100, group = 1), color = "black", size = 1.5, inherit.aes = FALSE)+
  theme(axis.title = element_text(family = "Arial", size = 18, colour = "grey40"),
        axis.text.x = element_text(family = "Arial", size = 14, angle = 90),
        axis.text.y = element_text(family = "Arial", size = 14),
        axis.title.y.right = element_text(family = "Arial", size = 14, colour = "black", face = "bold"),
        axis.text.y.right = element_text(family = "Arial", size = 14, colour = "black"),
        panel.grid = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(family = "Arial", size = 14, colour = "grey40"),
        strip.background = element_rect(fill="white", size=1.5, linetype="solid"),
        strip.text.x = element_text(size = 12, family = "Arial", colour = "grey40", face = "bold"))

plot3

#Save figure in pdf
ggsave(plot3, filename = "Percentage&number_hist_extinct_per_size_class_island_vs_mainland.pdf", path = "/Results/Raw_data_plots", width = 6, height = 6, device = cairo_pdf)

###################################################################
# Fit GLMM: Pr_hist_extinct and BM islands vs mainland         ####
###################################################################

glmer2 <- glmer(Pr_hist_extinction ~ Body_mass_log * Island_or_mainland + (1|Order), data = island_sp_mainlandvsisland, family=binomial(link="logit"))

#Check VIF
vif(glmer2)

#Visualize results and save table as doc file
tab_model(glmer2, show.aicc = TRUE, file = "Results/GLMMs/Table_models_hist_ext_main_vs_isl.doc")

#Perform model diagnostics with DHARMa
glmer.sim <- simulateResiduals(glmer2, integerResponse = TRUE, n = 250)
plot(glmer.sim)
testDispersion(glmer.sim)

#######################
# Plot model       ####
####################### 

predicted <- ggpredict(glmer2, terms = c("Body_mass_log [all]", "Island_or_mainland"), type = "fixed")

plot4 <- ggplot(data = island_sp_mainlandvsisland, aes(Body_mass_log, Pr_hist_extinction, colour = Island_or_mainland)) +
         scale_color_manual(values = c("#e1be6d", "#A9A9A9"))+
         scale_fill_manual(values = c("#e1be6d", "#A9A9A9"))+
         labs(x = "Log body mass", y = "P(extinct)") +
         geom_line(data = predicted, aes(x = x, y = predicted, group = group, color = group), show.legend=FALSE, inherit.aes = FALSE, size = 1) +
         geom_ribbon(data = predicted, aes(x = x, ymin = conf.low, ymax = conf.high, group = group, fill = group), inherit.aes = FALSE, show.legend=FALSE, alpha = .1)+
         geom_rug(data = island_sp_mainlandvsisland[island_sp_mainlandvsisland$Pr_hist_extinction=="extinct",], aes(x = Body_mass_log, colour = Island_or_mainland), inherit.aes = FALSE, sides = "t")+
         geom_rug(data = island_sp_mainlandvsisland[island_sp_mainlandvsisland$Pr_hist_extinction=="extant",], aes(x = Body_mass_log, colour = Island_or_mainland), inherit.aes = FALSE, sides = "b")+
         theme(axis.title = element_text(family = "Arial", size = 18, colour = "grey40"),
               axis.text.x = element_text(family = "Arial", size = 14),
               axis.text.y = element_text(family = "Arial", size = 14),
               panel.grid = element_blank(),
               legend.position = "none")

plot4

#Save figure in pdf
ggsave(plot4, filename = "P(hist_extinct)_BM_island_vs_mainland.pdf", path = "/Results/GLMMs", width = 2.5, height = 2.5, device = cairo_pdf) 

##############################################################
# Clean data                                              ####
##############################################################

island_sp_mainlandvsisland <- filter(island_sp_mainlandvsisland, IUCN_Category != "EX") 
island_sp_mainlandvsisland <- filter(island_sp_mainlandvsisland, IUCN_Category != "EW") 
island_sp_mainlandvsisland <- filter(island_sp_mainlandvsisland, IUCN_Category != "EP") 

#Create threatened vs nonthreatened column
island_sp_mainlandvsisland <- island_sp_mainlandvsisland %>%
  mutate(Pr_threatened = recode_factor(IUCN_Category,
                                CR = "threatened",
                                EN = "threatened",
                                VU = "threatened",
                                NT = "nonthreatened",
                                LC = "nonthreatened"))

#Reorder threatened vs nonthreatened column from low to high extinction risk
island_sp_mainlandvsisland <- island_sp_mainlandvsisland %>%
  mutate(Pr_threatened =
           fct_relevel(Pr_threatened,
                       "nonthreatened",
                       "threatened"))

#########################################################################
# Calculate proportions and numbers of threatened taxa per BM class  ####
#########################################################################

# bucketing values into bins
island_sp_mainlandvsisland$group_tags5 <- cut(island_sp_mainlandvsisland$Body_mass, 
                   breaks=breaks, 
                   include.lowest=TRUE, 
                   right=FALSE)
# inspect bins
summary(island_sp_mainlandvsisland$group_tags5)

#Order bins
island_sp_mainlandvsisland$BM_groups5 <- factor(island_sp_mainlandvsisland$group_tags5, ordered = TRUE) 

#Calculate proportion of total threatened species on the islands vs mainland (per BM class)
prop_threat <- island_sp_mainlandvsisland %>%
  group_by(BM_groups5, Island_or_mainland, Pr_threatened) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

#Create a column with counts of threatened and nonthreatened species on island and on the mainland per each BM class
island_sp_mainlandvsisland <- island_sp_mainlandvsisland %>%
  group_by(Island_or_mainland, Pr_threatened, BM_groups5) %>%
  mutate(number_threatened_per_size_class_island_vs_mainland = length(Pr_threatened))

#Create another dataset without nonthreatened data
island_sp_mainlandvsisland_only_threatened <- filter(island_sp_mainlandvsisland, Pr_threatened != "nonthreatened") #taglia tutte le righe in cui IUCN_Category è DD

###################################################################################################
# Plot raw data: percentage and number of threatened taxa per BM class on islands vs mainland  ####
###################################################################################################

plot5 <- ggplot() +
  geom_bar(data = island_sp_mainlandvsisland, aes(x=BM_groups5, fill = factor(Pr_threatened)), position = "fill") +
  labs(x = "Body mass (kg)") +
  facet_wrap(~Island_or_mainland, nrow = 2)+
  scale_fill_manual(values = c("#f0755d", "#742615"))+
  scale_y_continuous("Percentage threatened", limits = c(0,1), sec.axis = sec_axis(~. *210 , name="Number threatened")) +
  geom_point(data = island_sp_mainlandvsisland_only_threatened, aes(x=BM_groups5, y = number_threatened_per_size_class_island_vs_mainland/210), inherit.aes = FALSE, colour = "black", size = 4, shape = "square")+
  geom_line(data = island_sp_mainlandvsisland_only_threatened, aes(x=BM_groups5, y = number_threatened_per_size_class_island_vs_mainland/210, group = 1), color = "black", size = 1.5, inherit.aes = FALSE)+
  theme(axis.title = element_text(family = "Arial", size = 18, colour = "grey40"),
        axis.text.x = element_text(family = "Arial", size = 14, angle = 90),
        axis.text.y = element_text(family = "Arial", size = 14),
        axis.title.y.right = element_text(family = "Arial", size = 14, colour = "black", face = "bold"),
        axis.text.y.right = element_text(family = "Arial", size = 14, colour = "black"),
        panel.grid = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(family = "Arial", size = 14, colour = "grey40"),
        strip.background = element_rect(fill="white", size=1.5, linetype="solid"),
        strip.text.x = element_text(size = 14, family = "Arial", colour = "grey40", face = "bold"))

plot5

#Save figure in pdf
ggsave(plot5, filename = "Percentage&number_threatened_per_size_class_island_vs_mainland.pdf", path = "/Results/Raw_data_plots", width = 6, height = 6, device = cairo_pdf) 

##############################################################
# Fit GLMM: Pr_threatened and BM islands vs mainland      ####
##############################################################

glmer3 <- glmer(Pr_threatened ~ Body_mass_log * Island_or_mainland + (1|Order), data = island_sp_mainlandvsisland, family=binomial(link="logit"))

#Check VIF
vif(glmer3) 

#Visualize results and save table as doc file
tab_model(glmer3, show.aicc = TRUE, file = "Results/GLMMs/Table_models_thr_main_vs_isl.doc") 

#Perform model diagnostics with DHARMa
glmer.sim <- simulateResiduals(glmer2, integerResponse = TRUE, n = 250)
plot(glmer.sim)
testDispersion(glmer.sim)

#######################
# Plot model       ####
#######################

predicted2 <- ggpredict(glmer3, terms = c("Body_mass_log [all]", "Island_or_mainland"), type = "fixed")

plot6 <- ggplot(data = island_sp_mainlandvsisland, aes(Body_mass_log, Pr_threatened, colour = Island_or_mainland)) +
         scale_color_manual(values = c("#e1be6d", "#A9A9A9"))+
         scale_fill_manual(values = c("#e1be6d", "#A9A9A9"))+
         labs(x = "Log body mass", y = "P(threatened)") +
         geom_line(data = predicted2, aes(x = x, y = predicted, group = group, color = group), show.legend=FALSE, inherit.aes = FALSE, size = 1) +
         geom_ribbon(data = predicted2, aes(x = x, ymin = conf.low, ymax = conf.high, group = group, fill = group), inherit.aes = FALSE, show.legend=FALSE, alpha = .1)+
         geom_rug(data = island_sp_mainlandvsisland[island_sp_mainlandvsisland$Pr_threatened=="nonthreatened",], aes(x = Body_mass_log, colour = Island_or_mainland), inherit.aes = FALSE, sides = "b")+
         geom_rug(data = island_sp_mainlandvsisland[island_sp_mainlandvsisland$Pr_threatened=="threatened",], aes(x = Body_mass_log, colour = Island_or_mainland), sides = "t", inherit.aes = FALSE)+
         coord_cartesian(ylim = c(0, 1))+
         theme(axis.title = element_text(family = "Arial", size = 18, colour = "grey40"),
               axis.text.x = element_text(family = "Arial", size = 14),
               axis.text.y = element_text(family = "Arial", size = 14),
               panel.grid = element_blank(),
               legend.position = "none")

plot6

#Save figure in pdf
ggsave(plot6, filename = "P(threatened)_BM_island_vs_mainland.pdf", path = "/Results/GLMMs", width = 2.5, height = 2.5, device = cairo_pdf)

#End of script