# clean environment
rm(list=ls())

##############################################################
# Load libraries                                          ####
##############################################################

library(dplyr)
library(forcats)
library(ape)
library(sensiPhy)
library(ggplot2)
library(phylolm)
library(ggtree)
library(caper)
library(phytools)
library(extrafont)
library(ggpubr)
library(Cairo)

##############################################################
# Load data                                               ####
##############################################################

db_fossil <- read.csv('Data/Fossil_mammals_species_PLRs.csv')
db_extant <- read.csv('Data/Extant_mammals_species_PLRs.csv')

#Combine extant and fossil datasets

db_fossil <- dplyr::select(db_fossil, Order:IUCN_Category)
db_extant <- dplyr::select(db_extant, Order:IUCN_Category)

db_total <- rbind(db_fossil, db_extant)

db_total <- filter(db_total, Body_mass_island_taxon != "") 
db_total <- filter(db_total, Size_ratio != "") 
db_total <- filter(db_total, Direction_body_size_change != "no_change")

#Reorder IUCN_Category from low to high extinction risk

db_total <- db_total %>%
  mutate(IUCN_Category =
           fct_relevel(IUCN_Category,
                       "LC",
                       "NT",
                       "VU",
                       "EN",
                       "CR",
                       "EX",
                       "EP"))

#Create extinct vs extant column

db_total <- db_total %>%
  mutate(Pr_extinction = recode_factor(IUCN_Category,
                                EP = "extinct",
                                EX = "extinct",
                                CR = "extant",
                                EN = "extant",
                                VU = "extant",
                                NT = "extant",
                                LC = "extant"))

#Creates extant vs extinct column as a binary output 
#0 = extant; 1 = extinct

db_total <- db_total %>%
  mutate(Pr_extinction_bin = recode(Pr_extinction,
                                    extant = 0L, 
                                    extinct = 1L))

#Read the complete mammal trees including fossil species obtained by TACT

trees_complete_TACT <-read.nexus('Results/TACT/100trees_outputs_TACT.trees')

# Assign rownames to dataset
rownames(db_total)=db_total$Latin_binomial 

#Prune the first tree so that includes only species in the dataset

tree1 <- trees_complete_TACT[[1]]
  
comp_data <- match_dataphy(Pr_extinction_bin ~ Magnitude_body_size_change, data = db_total, phy = tree1, track = FALSE)

comp.data.tree <- comp_data$phy

#Save the pruned tree in Newick and nexus format
write.tree(comp.data.tree, file = 'Results/PLRs/Pruned_tree_dwarfs_giants.tree')
write.nexus(comp.data.tree, file = 'Results/PLRs/Pruned_tree_dwarfs_giants.nex')

####################################################################
# Calculate phylo signal (K) for Magnitude size change and      ####
# sensitivity analysis across 100 complete TACT trees           ####
####################################################################

tree_K <- tree_physig(trait.col = "Magnitude_body_size_change", data = db_total, phy = trees_complete_TACT, method = "K", track=FALSE)
summary(tree_K) 
sensi_plot(tree_K, graphs = 1)
sensi_plot(tree_K, graphs = 2)
sensi_plot(tree_K) 

#Extract summary stats in a table
phylosignK <- tree_K$stats
phylosignK <- phylosignK %>% mutate_if(is.numeric, round, digits=8)
rownames(phylosignK)[rownames(phylosignK) == "estimate"] <- "estimate_K"
rownames(phylosignK)[rownames(phylosignK) == "pval"] <- "pval_K"

####################################################################
# Calculate phylo signal (lambda) for Magnitude size change and ####
# sensitivity analysis across 100 complete TACT trees           ####
####################################################################

tree_lambda <- tree_physig(trait.col = "Magnitude_body_size_change", data = db_total, phy = trees_complete_TACT, method = "lambda", track=FALSE)
summary(tree_lambda)
sensi_plot(tree_lambda, graphs = 1)
sensi_plot(tree_lambda, graphs = 2)
sensi_plot(tree_lambda) 

#Extract summary stats in a table
phylosignlambda <- tree_lambda$stats
phylosignlambda <- phylosignlambda %>% mutate_if(is.numeric, round, digits=8)
rownames(phylosignlambda)[rownames(phylosignlambda) == "estimate"] <- "estimate_lambda"
rownames(phylosignlambda)[rownames(phylosignlambda) == "pval"] <- "pval_lambda"

####################################################################
# Calculate phylo signal (D) for Direction size change and      ####
# sensitivity analysis across 100 complete TACT trees           ####
####################################################################

#Set up lists to store results
results_D <- list(length = trees_complete_TACT) 
results_Pval1 <- list(length = trees_complete_TACT) 
results_Pval0 <- list(length = trees_complete_TACT) 

for(i in 1:length(trees_complete_TACT)) {
  currentTree <- trees_complete_TACT[[i]]
  direction <- comparative.data(phy = currentTree, data = db_total, names.col = Latin_binomial, vcv = TRUE, na.omit = FALSE, warn.dropped = TRUE)
  output <- phylo.d(data=direction, binvar = Direction_body_size_change, permut = 1000)
  results_D[i] <- output[["DEstimate"]]
  results_Pval0[i] <- output[["Pval0"]]
  results_Pval1[i] <- output[["Pval1"]]
}

#Tranform list in data frame
results_D_df <- data.frame(matrix(unlist(results_D), nrow=length(results_D), byrow=TRUE))

results_D_df <- rename(results_D_df, values = matrix.unlist.results_D...nrow...length.results_D...byrow...TRUE.)

#Calculate mean, max, min, and CIs of D

D_summary <- sapply(results_D_df, function(results_D_df) c( "min" = min(results_D_df), 
                                                            "max"= max(results_D_df),
                                                            "mean" = mean(results_D_df)))

Mean_ci_D <- mean_ci(results_D_df$values)
Mean_ci_D <- dplyr::rename(Mean_ci_D, mean = y)
Mean_ci_D <- dplyr::rename(Mean_ci_D, CI_low = ymin)
Mean_ci_D <- dplyr::rename(Mean_ci_D, CI_high = ymax)
rownames(Mean_ci_D)[rownames(Mean_ci_D) == "1"] <- "values"
D_summary <- t(D_summary)
D_summary <- as.data.frame(D_summary)
D_summary <- merge(D_summary,Mean_ci_D,by="mean")
rownames(D_summary)[rownames(D_summary) == "1"] <- "estimate_D"

#Calculate mean, max, min, and CIs of Pval0

results_Pval0_df <- data.frame(matrix(unlist(results_Pval0), nrow=length(results_Pval0), byrow=TRUE))

results_Pval0_df <- dplyr::rename(results_Pval0_df, values = matrix.unlist.results_Pval0...nrow...length.results_Pval0...byrow...TRUE.)

Pval0_summary <- sapply(results_Pval0_df, function(results_Pval0_df) c( "min" = min(results_Pval0_df), 
                                                            "max"= max(results_Pval0_df),
                                                            "mean" = mean(results_Pval0_df)))

Mean_ci_Pval0 <- mean_ci(results_Pval0_df$values)
Mean_ci_Pval0 <- dplyr::rename(Mean_ci_Pval0, mean = y)
Mean_ci_Pval0 <- dplyr::rename(Mean_ci_Pval0, CI_low = ymin)
Mean_ci_Pval0 <- dplyr::rename(Mean_ci_Pval0, CI_high = ymax)
rownames(Mean_ci_Pval0)[rownames(Mean_ci_Pval0) == "1"] <- "values"
Pval0_summary <- t(Pval0_summary)
Pval0_summary <- as.data.frame(Pval0_summary)
Pval0_summary <- merge(Pval0_summary,Mean_ci_Pval0,by="mean")
rownames(Pval0_summary)[rownames(Pval0_summary) == "1"] <- "pval0_D"

#Calculate mean, max, min, and CIs of Pval1

results_Pval1_df <- data.frame(matrix(unlist(results_Pval1), nrow=length(results_Pval1), byrow=TRUE))

results_Pval1_df <- dplyr::rename(results_Pval1_df, values = matrix.unlist.results_Pval1...nrow...length.results_Pval1...byrow...TRUE.)

Pval1_summary <- sapply(results_Pval1_df, function(results_Pval1_df) c( "min" = min(results_Pval1_df), 
                                                                        "max"= max(results_Pval1_df),
                                                                        "mean" = mean(results_Pval1_df)))

Mean_ci_Pval1 <- mean_ci(results_Pval1_df$values)
Mean_ci_Pval1 <- dplyr::rename(Mean_ci_Pval1, mean = y)
Mean_ci_Pval1 <- dplyr::rename(Mean_ci_Pval1, CI_low = ymin)
Mean_ci_Pval1 <- dplyr::rename(Mean_ci_Pval1, CI_high = ymax)
rownames(Mean_ci_Pval1)[rownames(Mean_ci_Pval1) == "1"] <- "values"
Pval1_summary <- t(Pval1_summary)
Pval1_summary <- as.data.frame(Pval1_summary)
Pval1_summary <- merge(Pval1_summary,Mean_ci_Pval1,by="mean")
rownames(Pval1_summary)[rownames(Pval1_summary) == "1"] <- "pval1_D"

#Add D and p values to the other summ stats and convert them in a table

stats_phylo_sign <- rbind(phylosignK, phylosignlambda, D_summary, Pval0_summary, Pval1_summary)
stats_phylo_sign <- add_rownames(stats_phylo_sign, "VALUE")
stats_phylo_sign

table <- gt(stats_phylo_sign)

table <- table %>% tab_header(
  title = "Phylogenetic signal",
  subtitle = "traits = MBSC (K, lambda); DBSC (D)")

table

#Save the table
gtsave(table, filename = "Table_phylogenetic_signal.png", path = "/Results/PLRs")

#######################################################
# Plot phylo signal for Magnitude (K, lambda) and  ####
# Direction size change                            ####
#######################################################

K <- tree_K$tree.physig.estimates$estimate
K_df <- as.data.frame(K)
K_df <- dplyr::rename(K_df, estimate = K)
K_df <- K_df %>% mutate(phy_sign = "K", trait = "magnitude")

lambda <- tree_lambda$tree.physig.estimates$estimate
lambda_df <- as.data.frame(lambda)
lambda_df <- dplyr::rename(lambda_df, estimate = lambda)
lambda_df <- lambda_df %>% mutate(phy_sign = "lambda", trait = "magnitude")

results_D_df <- dplyr::rename(results_D_df, estimate = values)
results_D_df <- results_D_df %>% mutate(phy_sign = "D", trait = "direction")

phylo_sign_estimates <- rbind(K_df, lambda_df, results_D_df)

extrafont::loadfonts(device = "win")

theme_set(theme_light(base_size = 12, base_family = "Arial"))

Plot_phylo_signal <- ggplot(phylo_sign_estimates, aes(x = phy_sign, y = estimate, fill = trait, color = trait)) + 
  stat_summary(fun.data=mean_sdl, fun.args = list(mult = 1), size = 1, geom="pointrange", shape = 15, position = position_dodge(0.75))+
  geom_jitter(size = 2.5, alpha = 0.1, position = position_jitterdodge(0.5), set.seed(123)) +
  scale_colour_manual(values = c("#52352e", "#c25234"))+
  labs(x = "", y = "Estimate") +
  annotate("text", label = "0.01", x = "K", y = -0.06, family = "Arial", size = 6, colour = "#c25234")+
  annotate("text", label = "0.80", x = "lambda", y = 0.69, family = "Arial", size = 6, colour = "#c25234")+ 
  annotate("text", label = "0.59", x = "D", y = 0.5, family = "Arial", size = 6, colour = "#52352e")+ 
  annotate("text", label = "p = 0.31", x = "K", y = 0.12, family = "Arial", size = 6, colour = "grey40", fontface = "italic")+ 
  annotate("text", label = "p < 0.001", x = "lambda", y = 0.92, family = "Arial", size = 6, colour = "grey40", fontface = "italic")+ 
  annotate("text", label = "p < 0.001", x = "D", y = 0.69, family = "Arial", size = 6, colour = "grey40", fontface = "italic")+ 
  ylim(-0.25,1.25) +
  theme(axis.title = element_text(family = "Arial", size = 18, colour = "grey40"),
        axis.text.x = element_text(family = "Arial", size = 14),
        axis.text.y = element_text(family = "Arial", size = 14),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        legend.text = element_text(family = "Arial", size = 14, colour = "grey40"))

Plot_phylo_signal

#Save figure in pdf
ggsave(Plot_phylo_signal, filename = "Phylo_sign_estimates_plot.pdf", path = "/Results/PLRs", width = 3, height = 5, device = cairo_pdf)


############################################################
# Plot phylogeny with Direction size change at the tips ####
############################################################

p1 <- ggtree(comp.data.tree, layout="circular") 
plot(p1)

#Find node names and select each Order
findMRCA(comp.data.tree, tips=c("Elasmodontomys_obliquus","Mastomys_natalensis"), type=c("node"))

#Carnivora; node: 985
#Pholidota; node: 983
#Artiodactyla; node: 1078
#Perissodactyla; node: 1077
#Chiroptera; node: 1157
#Eulipotyphla; node: 900
#Rodentia; node: 1411
#Lagomorpha; node: 1394
#Primates; node: 1329
#Scandentia; node: 1319
#Dermoptera; node: 1317
#Proboscidea; node: 874
#Pilosa; node: 873
#Diprotodontia; node: 846
#Peramelemorphia; node: 838
#Dasyuromorphia; node: 843 

p2 <- p1 + 
  geom_cladelabel(985, "Carnivora", offset=15, barsize=1, angle=0, offset.text=50, hjust=0.5, fontsize=3, color = "#8d96a3") +
  geom_cladelabel(983, "Pholidota", offset=15, barsize=1, angle=0, offset.text=50, hjust=0.5, fontsize=3, color = "black") +
  geom_cladelabel(1078, "Artiodactyla", offset=15, barsize=1, angle=0, offset.text=50, hjust=0.2, fontsize=3, color = "#8d96a3")+
  geom_cladelabel(1077, "Perissodactyla", offset=15, barsize=1, angle=0, offset.text=50, hjust=0.5, fontsize=3, color = "black")+ 
  geom_cladelabel(1157, "Chiroptera", offset=15, barsize=1, angle=0, offset.text=50, hjust=0.5, fontsize=3, color = "#8d96a3")+
  geom_cladelabel(900, "Eulipotyphla", offset=15, barsize=1, angle=0, offset.text=80, hjust=0.5, fontsize=3, color = "black")+
  geom_cladelabel(1411, "Rodentia", offset=15, barsize=1, angle=0, offset.text=50, hjust=0.5, fontsize=3, color = "#8d96a3")+
  geom_cladelabel(1394, "Lagomorpha", offset=15, barsize=1, angle=0, offset.text=50, hjust=0.5, fontsize=3, color = "black")+ 
  geom_cladelabel(1329, "Primates", offset=15, barsize=1, angle=0, offset.text=50, hjust=0.5, fontsize=3, color = "#8d96a3")+
  geom_cladelabel(1319, "Scandentia", offset=15, barsize=1, angle=0, offset.text=50, hjust=0.5, fontsize=3, color = "black")+
  geom_cladelabel(1317, "Dermoptera", offset=15, barsize=1, angle=0, offset.text=50, hjust=0, fontsize=3, color ="#8d96a3")+
  geom_cladelabel(874, "Proboscidea", offset=15, barsize=1, angle=0, offset.text=70, hjust=0.5, fontsize=3, color = "black")+
  geom_cladelabel(873, "Pilosa", offset=15, barsize=1, angle=0, offset.text=50, hjust=0.5, fontsize=3, color = "#8d96a3")+ 
  geom_cladelabel(846, "Diprotodontia", offset=15, barsize=1, angle=0, offset.text=80, hjust=0.5, fontsize=3, color = "black")+ 
  geom_cladelabel(838, "Peramelemorphia", offset=15, barsize=1, angle=0, offset.text=120, hjust=0.5, fontsize=3, color = "#8d96a3")+
  geom_cladelabel(843, "Dasyuromorphia", offset=15, barsize=1, angle=0, offset.text=120, hjust=0.5, fontsize=3, color = "black")+
  geom_strip("Dromiciops_gliroides", "Didelphis_marsupialis", label = "Microbiotheria + Didelphimorphia", offset=15, barsize=1, angle=0, offset.text=120, hjust=0.2, fontsize=3, color = "#8d96a3")+ 
  geom_strip("Didelphis_marsupialis", "Ornithorhynchus_anatinus", label = "Monotremata", offset=15, barsize=1, angle=0, offset.text=100, hjust=0.5, fontsize=3, color = "black")

plot(p2)

direction <- dplyr::select(db_total, Direction_body_size_change)

p3 <-  gheatmap(p2, direction, color = FALSE, offset=0.2, width=0.05, low="#ee8c3d", high="#66a182", colnames = FALSE) +
       scale_fill_manual(values = c("#edae49", "#66a182"))

p3

#Save figure in pdf
ggsave(p3, filename = "Plot_phylogeny_dwarfs_giants.pdf", path = "/Results/PLRs", width = 8, height = 8, device = cairo_pdf)

#End of script
