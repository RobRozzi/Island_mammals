#clean environment
rm(list =ls())

##############################################################
# Load libraries                                         ####
##############################################################
library(sf)
library(dplyr)
library(rnaturalearth)
library(extrafont)
library(ggplot2)
library(forcats)
library(Cairo)

##############################################################
# Load data                                               ####
##############################################################

#207 taxa; 89 dwarfs; 118 giants
data<-read.csv('Results/PyRate_ext_rates/Map_extinctions_dwarfs_giants/Input_map_extinctions_dwarfs_giants.csv')

#Define geologic ages
data$geo_epoch <- ifelse(data$TE_PyRate_years_BP<= 11700, "Holocene",
                         ifelse(c(data$TE_PyRate_years_BP > 11700 & data$TE_PyRate_years_BP <= 2588000), "Pleistocene",
                                       "Miocene+Pliocene"))

##############################################################
# Converting data to spatial data                         ####
##############################################################

#Converting data to Simple Features
spdata<- st_as_sf(data, coords = c('longitude', 'latitude'), remove = FALSE,
                  crs= "init=epsg:4326 
                   +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" )

#Group by island as there are several cases per island
spdata_group <- spdata %>%
  group_by(Island, geo_epoch, Direction_body_size_change) %>%
  summarize(nsp = n(),
            x = first(longitude),
            y = first(latitude))

spdata_group

#Download world land mass from Natural Earth
world_land <- ne_download(scale = "large", category = 'physical', type = 'land', returnclass = "sf" )


######################################
# Plot map                        ####
######################################

extrafont::loadfonts(device = "win")

theme_set(theme_light(base_size = 15, base_family = "Arial"))

#Reorder facets

spdata_group <- spdata_group %>%
  mutate(geo_epoch =
           fct_relevel(geo_epoch,
                       "Holocene",
                       "Pleistocene",
                       "Miocene+Pliocene"))


map <- ggplot(data = world_land) +
  geom_sf(fill = "light grey", color = NA) +
  geom_point(data = spdata_group, aes(x = x, y = y, colour = nsp, alpha = .4), stroke = 0, shape = 16, size = 4) +
  scale_colour_distiller(palette = "Spectral", direction=-1, trans = "log10", name="Num. species")+
  facet_grid(cols = vars(Direction_body_size_change), rows = vars(geo_epoch), switch = "y")+
  guides(size = "none", alpha = "none", colour = guide_colourbar(barwidth = 0.5))+
  theme(panel.background = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 10, family = "Arial", colour = "grey40", face = "bold"),
        strip.text.y = element_text(size = 10, family = "Arial", colour = "grey40"),
        legend.title=element_text(size = 10, family = "Arial", colour = "grey40"),
        legend.text=element_text(size = 10, family = "Arial", colour = "grey40"))


map

#Save map as pdf
ggsave(map, filename = "Map_extinctions_dwarfs_giants.pdf", path = "Results/PyRate_ext_rates/Map_extinctions_dwarfs_giants", width = 10, height = 20, device = cairo_pdf) 

#End of script
