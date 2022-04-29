# clean environment
rm(list=ls())

##############################################################
# Load libraries                                          ####
##############################################################

library(dplyr)
library(ggplot2)
library(extrafont)
library(Cairo)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(Cairo)

##############################################################
# Load data                                               ####
##############################################################

world_land <- ne_download(scale = "large", category = 'physical', type = 'land', returnclass = "sf" )

data <- read.csv('Data/Fossil_occurrences.csv')

coordinates <- read.csv('Data/Coordinates_islands.csv')

#Assign coordinates to each occurrence
data_coordinates <- left_join(data, coordinates)

#Transform max and min ages in years
data_coordinates <- mutate(data_coordinates, MinAge = MinAge*1000000)
data_coordinates <- mutate(data_coordinates, MaxAge = MaxAge*1000000)

#Add median ages
data_coordinates$Median_age_years_BP = apply(data_coordinates[7:8], 1, median)

#Define geologic epochs
data_coordinates$geo_epoch <- ifelse(data_coordinates$Median_age_years_BP<= 11700, "Holocene",
                              ifelse(c(data_coordinates$Median_age_years_BP > 11700 & data_coordinates$Median_age_years_BP <= 2588000), "Pleistocene",
                              ifelse(c(data_coordinates$Median_age_years_BP > 2588000 & data_coordinates$Median_age_years_BP <= 5333000), "Pliocene",
                                         "Miocene")))

#Converting data to Simple Features
spdata<- st_as_sf(data_coordinates, coords = c('longitude', 'latitude'), remove = FALSE,
                  crs= "init=epsg:4326 
                   +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" )

#Group by island as there are several cases per island
spdata_group <- spdata %>%
  group_by(Island, geo_epoch) %>%
  summarize(n_occ = n(),
            x = first(longitude),
            y = first(latitude))

spdata_group

#Reorder epochs
spdata_group <- spdata_group %>%
  mutate(geo_epoch =
           fct_relevel(geo_epoch,
                       "Holocene",
                       "Pleistocene",
                       "Pliocene",
                       "Miocene"))

##############################################################
# Plot maps                                               ####
##############################################################

theme_set(theme_bw())

map1 <- ggplot(data = world_land) +
  geom_sf(fill = "light grey", color = NA) +
  geom_point(data = spdata_group, aes(x = x, y = y, colour = geo_epoch, size = n_occ, alpha = .4)) +
  scale_colour_brewer(palette = "Spectral", direction=-1, name="Epoch")+
  guides(alpha = "none", size=guide_legend(title="Num. fossil occurrences"))+
  theme(panel.background = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 14, family = "Arial", colour = "grey40", face = "bold"),
        strip.text.y = element_text(size = 14, family = "Arial", colour = "grey40"),
        legend.title=element_text(size = 14, family = "Arial", colour = "grey40"),
        legend.text=element_text(size = 14, family = "Arial", colour = "grey40"))

map1

#Save map in pdf
ggsave(map1, filename = "Map_fossil_occurrences1.pdf", path = "/Results/Raw_data_plots", width = 10, height = 5, device = cairo_pdf)

map2 <- ggplot(data = world_land) +
  geom_sf(fill = "light grey", color = NA) +
  geom_point(data = spdata_group, aes(x = x, y = y, colour = geo_epoch, size = n_occ, alpha = .4)) +
  scale_colour_brewer(palette = "Spectral", direction=-1, name="Epoch")+
  guides(alpha = "none", size=guide_legend(title="Num. fossil occurrences"))+
  facet_wrap(~geo_epoch, nrow = 4)+
  theme(panel.background = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 14, family = "Arial", colour = "grey40", face = "bold"),
        strip.text.y = element_text(size = 14, family = "Arial", colour = "grey40"),
        legend.title=element_text(size = 14, family = "Arial", colour = "grey40"),
        legend.text=element_text(size = 14, family = "Arial", colour = "grey40"))

map2

#Save map in pdf
ggsave(map2, filename = "Map_fossil_occurrences2.pdf",  path = "/Results/Raw_data_plots", width = 10, height = 8, device = cairo_pdf)

#End of script