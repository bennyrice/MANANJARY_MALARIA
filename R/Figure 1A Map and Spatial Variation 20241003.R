library(tidyverse)
library(patchwork)
library(ggrepel)
library(geosphere)
library(sf)
library(terra)
library(scales)

####################################################################################################
## Read in shape files, elevation model and site coordinates
####################################################################################################

#Read in District (ADM2) boundaries from shape file
mg.adm2 <- st_read("/Users/blrice/Documents/GitHub/MANANJARY_MALARIA/data/mapping_madagascar_admin/mdg_adm_bngrc_ocha_20181031_shp/mdg_admbnda_adm2_BNGRC_OCHA_20181031.shp")
#Read in Country (ADM0) boundaries for Madagascar
mg.adm0 <- st_read("/Users/blrice/Documents/GitHub/MANANJARY_MALARIA/data/mapping_madagascar_admin/mdg_adm_bngrc_ocha_20181031_shp/mdg_admbnda_adm0_BNGRC_OCHA_20181031.shp")
#read in MNJ locality data
df.mnj.points <- readr::read_csv("/Users/blrice/Documents/GitHub/MANANJARY_MALARIA/data/mapping_site_gps/site_midpoints 20230316.csv")

#read in elev data from: 
#European Space Agency, Sinergise (2021). Copernicus Global Digital Elevation Model. Distributed by OpenTopography. https://doi.org/10.5069/G9028PQB. Accessed: 2024-03-22
#https://portal.opentopography.org/raster?opentopoID=OTSDEM.032021.4326.3
#Using copernicus to get coastal water bodies (elev < 0.5m), then using geom_tile to show on solid base map

#For publication, **undo aggregation to get max resolution of maps

#SE Madagascar elevation
r.elev.SE <- rast("/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2023_cyclones/digital_elevation_models/copernicus_elevation_model/SE Madagasccar/output_COP30.tif")
# r.elev.SE <- aggregate(r.elev.SE, 1)
r.elev.SE <- aggregate(r.elev.SE, 4)

#Madagascar elevation: Use copernicus 90m but make it slightly lower resolution using aggregate()
r.elev.mada <- rast("/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2023_cyclones/digital_elevation_models/copernicus_elevation_model/Madagascar/output_COP90.tif")
# r.elev.mada <- aggregate(r.elev.mada, 3)
r.elev.mada <- aggregate(r.elev.mada, 6)

#convert to data frame and clean
df.elev.mada <- as.data.frame(r.elev.mada, xy = TRUE) %>%
  rename(elev_m = names(r.elev.mada)[1]) %>%
  mutate(elev_m = ifelse(elev_m < 0.001, NA, elev_m)) %>%
  filter(!is.na(elev_m)) %>%
  #dropping Comoros
  filter(x > 47 | y < -14)

#convert to data frame and clean
df.elev.SE <- as.data.frame(r.elev.SE, xy = TRUE) %>% 
  #Renaming column for clarity
  rename(elev_m = names(r.elev.SE)[1]) %>%
  mutate(elev.cat = case_when(
    elev_m == 0                  ~ NA_character_,
    elev_m <  0                  ~ "<0.5",
    elev_m >  0   & elev_m < 0.5 ~ "<0.5",
    elev_m >= 0.5 & elev_m < 1.0 ~ "0.5-1.0",
    elev_m >= 1.0 & elev_m < 2.0 ~ "1.0-2.0",
    elev_m >= 2.0 & elev_m < 5.0 ~ "2.0-5.0",
    elev_m >= 5.0 & elev_m < 10  ~ "5.0-10",
    elev_m >= 10  & elev_m < 100 ~ "10-100",
    elev_m >= 100 & elev_m < 250 ~ "100-250",
    elev_m >= 250                 ~ "250+")) %>%
  mutate(elev.cat = factor(elev.cat, levels = c("<0.5", "0.5-1.0", "1.0-2.0", "2.0-5.0", "5.0-10", "10-100", "100-250", "250+"))) %>%
  filter(!is.na(elev.cat))


####################################################################################################
## MADAGSACAR MAP: Solid fill, highlighting MNJ district
####################################################################################################

p.MADA <- ggplot() + 
  #Base layer: Madagascar, fill color set to match end of cividis color scale
  geom_sf(data = mg.adm0, fill = "#FEC98DFF", alpha = 1, color = "white", linewidth = 0) +
  #Elevation profile
  geom_tile(data = df.elev.mada, aes(x = x, y = y, fill = elev_m), alpha = 1) +
  #MNJ District Boundary
  geom_sf(data = mg.adm2 %>% filter(ADM2_EN == "Mananjary"), fill = "black", alpha = 0, color = "white", linewidth = 0.4) +
  #POINTS
  #MNJ District Capital
  geom_point(data = df.mnj.points %>% filter(Code == "MNJ"), 
             aes(x = LON, y = LAT),
             color = "black", alpha = 0.9, size = 2.5, shape = 0, stroke = 1) +
  #LABELS
  #MNJ District Capital Label
  geom_label_repel(data = df.mnj.points %>% filter(Code == "MNJ"), 
                   aes(x = LON, y = LAT, label = code.new),
                   color = "black",
                   nudge_x = 50.5 - subset(df.mnj.points, Code == "MNJ")$LON, 
                   segment.size = 0.3,
                   direction = "y") +
  #SCALE
  scale_fill_viridis_c(option = "magma", name = "Elevation (m)", direction = -1, end = 0.9) +
  #COORDS
  coord_sf() +
  theme(legend.position = "left",
        panel.grid = element_blank(),
        axis.title = element_blank(), 
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"))
p.MADA

####################################################################################################
## SE MADAGSACAR MAP (MNJ DISTRICT): Fill by elevation bin
####################################################################################################

#Showing color scales to match fill
# viridis_pal(option = "magma")(10)

#Note the plot takes approx. 2 minutes to render when at low resolution
#(change the aggregate factor above to make higher resolution when needed)
p.SE <- ggplot() + 
  #Base layer: Madagascar
  geom_sf(data = mg.adm0, fill = "#FCFDBFFF", alpha = 1, color = "white", linewidth = 0) +
  #Elevation profile
  geom_tile(data = df.elev.SE, aes(x = x, y = y, fill = elev.cat), alpha = 1) +
  #MNJ District Boundary
  geom_sf(data = mg.adm2 %>% filter(ADM2_EN == "Mananjary"), fill = "black", alpha = 0, color = "white", linewidth = 0.4) +
  #POINTS
  #Study Sites
  geom_point(data = df.mnj.points %>% filter(Code != "MNJ"), 
             aes(x = LON, y = LAT),
             color = "black", fill = "grey90", alpha = 0.75, size = 4, shape = 21, stroke = 1) +
  #MNJ District Capital
  geom_point(data = df.mnj.points %>% filter(Code == "MNJ"), 
             aes(x = LON, y = LAT),
             color = "black", alpha = 0.9, size = 4, shape = 0, stroke = 1.5) +
  #LABELS
  #Study Site Labels
  geom_label_repel(data = df.mnj.points %>% filter(Code != "MNJ"), 
                   aes(x = LON, y = LAT, label = code.new),
                   color = "black",
                   nudge_x = 48.6-subset(df.mnj.points, Code != "MNJ")$LON, 
                   hjust = 0.5,
                   segment.size = 0.3,
                   direction = "y",
                   point.padding = 0.3,
                   segment.curvature = -0.1,
                   segment.ncp = 3,
                   segment.angle = 90) +
  #MNJ District Capital Label
  geom_label_repel(data = df.mnj.points %>% filter(Code == "MNJ"), 
                   aes(x = LON, y = LAT, label = code.new),
                   color = "black",
                   nudge_x = 48.5-subset(df.mnj.points, Code == "MNJ")$LON, 
                   hjust = 0.5,
                   segment.size = 0.3,
                   direction = "y",
                   point.padding = 0.2,
                   segment.curvature = -0.1,
                   segment.ncp = 3,
                   segment.angle = 90) +
  #Color scale
  scale_fill_viridis_d(option = "magma", name = "Elevation (m)", direction = -1, end = 0.95, begin = 0.15) +
  #COORDS
  coord_sf(xlim = c(47.5, 48.8), ylim = c(-20.7, -21.8)) +
  theme(legend.position = "left",
    panel.grid = element_blank(),
    axis.title = element_blank(), 
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "white"))
p.SE





