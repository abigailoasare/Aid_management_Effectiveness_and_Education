# Aid management quality and Educational outcomes
#
# written by: Asare

# Setup -------------------------------------------------------------------
## Load Packages -----------------------------------------------------------
library("tidyverse") # data wrangling
library("sf") # general spatial functionality
library("raster") # raster data functionality 
library("tmap") # static and interactive maps
library("leaflet") # interactive maps
library("lubridate") #dates
library("ggplot2")
library("ggspatial")
library("geodata")
library("ggpattern")
library(rnaturalearth)     # package with detailed information about country &
library(rnaturalearthdata) # state/province borders, and geographical features
library(rnaturalearthhires) # Hi-Resolution Natural Earth
library(cowplot)


maps_dir    <- "./output/maps/"

# DHS and Aid Locatons Map ----------------------------------------------------
## Map Shape File ----------------------------------------------------------
data("World")
africa <- World |> 
  dplyr::filter(continent == "Africa")


## Grids -------------------------------------------------------------------
grid <- st_make_grid(africa, cellsize = 0.25) |> 
  st_as_sf() |> 
  dplyr::mutate(cellid = row_number())

centroids <- st_centroid(grid) |> 
  dplyr::mutate(cellid = row_number()) |> 
  st_filter(africa)

grid <- grid |> 
  dplyr::filter(cellid %in% unique(centroids$cellid))


country_sample <- read_csv("./data-r4r/country_sample.csv") 


ccodes <- geodata::country_codes() 

country_sample <- country_sample |> 
  left_join(ccodes, by = c("dhscc2" = "ISO2")) 

countries <- country_sample |> 
  pull(ISO3)

## DHS Project Locations ---------------------------------------------------
dhs_sf <- "./data-processed/dhs_clusters.shp" |> 
  sf::st_read() |> 
  st_as_sf(coords=c("longnum","latnum"), crs=4326) 

dhs_count <- st_join(grid, dhs_sf) |> 
  st_drop_geometry()|> 
  dplyr::filter(!is.na(DHSID)) |> 
  group_by(cellid) |> 
  summarise(dhs_cluster = n())


## Aid Locations -----------------------------------------------------------
aid_sf <- "./data-processed/aid_locations.shp" |> 
  sf::st_read() |> 
  st_as_sf(coords=c("long","lat"), crs=4326) |>
  dplyr::select(pro_loc_id,country)

aid_count <- st_join(grid, aid_sf) |> 
  st_drop_geometry() |> 
  dplyr::filter(!is.na(pro_loc_id)) |> 
  group_by(cellid) |> 
  summarise(aid_num = n())

## Aid Ratings and Project Info --------------------------------------------
ieg <- "./data-processed/rating_aid_map.csv" |> 
  readr::read_csv() |> 
  dplyr::mutate(
    ieg_outcome_edu= ifelse(ieg_outcome_edu==0,NA_real_,ieg_outcome_edu),
    ieg_labels = ifelse(ieg_outcome_edu == 1,"Highly Unsatisfactory",NA),
    ieg_labels = ifelse(ieg_outcome_edu == 2,"Unsatisfactory",ieg_labels),
    ieg_labels = ifelse(ieg_outcome_edu == 3,"Moderately Unsatisfactory",ieg_labels),
    ieg_labels = ifelse(ieg_outcome_edu == 4,"Moderately Satisfactory",ieg_labels),
    ieg_labels = ifelse(ieg_outcome_edu == 5,"Satisfactory",ieg_labels),
    ieg_labels = ifelse(ieg_outcome_edu == 6,"Highly Satisfactory",ieg_labels),
    ieg_labels = ifelse(is.na(ieg_outcome_edu),"other aid/Missing",ieg_labels)
  )

ieg_sf <- ieg |> 
  dplyr::select(project_location_id,IEG_EvalFY,ieg_outcome_code,ieg_outcome_edu,ieg_labels) |> 
  dplyr::left_join(aid_sf, by=c("project_location_id"="pro_loc_id")) |> 
  st_as_sf(crs=4326) 


ieg_mean <- st_join(grid, ieg_sf) |> 
  st_drop_geometry() |> 
  dplyr::filter(!is.na(project_location_id)) |> 
  group_by(cellid) |> 
  summarise(avieg_outcome_code = mean(ieg_outcome_code, na.rm=TRUE),
            avieg_outcome_edu = mean(ieg_outcome_edu, na.rm=TRUE)
  ) 

ieg_mean[ieg_mean=="NaN"] <- 0

grid <- grid |> 
  left_join(aid_count, by = "cellid") |> 
  left_join(ieg_mean, by = "cellid") |> 
  left_join(dhs_count, by = "cellid") |> 
  replace_na(list(dhs_cluster = 0, aid_num = 0, avieg_outcome_code = 0,
                  avieg_outcome_edu= 0))


grid <- grid |>
  dplyr::mutate(fillcolor = case_when(
    (dhs_cluster > 0 & aid_num == 0) ~ 1,
    (aid_num  > 0 & dhs_cluster == 0) ~ 2,
    (aid_num  > 0 & dhs_cluster > 0) ~ 3,
    TRUE ~ NA_real_
  )) |>
  dplyr::mutate(fillcolor = factor(
    fillcolor,
    levels = c(1, 2, 3),
    labels = c(
      "DHS Survey Locations",
      "Aid locations",
      "DHS Survey Locations and Aid Locations"
    )
  ))

## Map of Aid locations ---------------------------------------------------
ggplot()+
  geom_sf(data = subset(grid, aid_num > 0), aes(fill = aid_num), lwd = 0)+
  geom_sf(data = africa, fill = NA, color = "black")+
  geom_sf(data = africa |> dplyr::filter(iso_a3 %in% countries), fill = NA, color = "lightgreen", lwd=0.8) +
  scale_fill_viridis_c()


plot <- ggplot()+
  geom_sf(data = africa |> dplyr::filter(iso_a3 %in% countries), fill = "#fff0c2", color = NA, lwd=0)+
  #geom_sf(data = grid, aes(fill = fillcolor), lwd = 0, color = NA)+
  geom_sf(data = subset(st_filter(grid, africa |> dplyr::filter(iso_a3 %in% countries)), 
                        !is.na(fillcolor)), aes(fill = fillcolor), lwd = 0, color = NA)+
  geom_sf(data = africa |> dplyr::filter(!(iso_a3 %in% countries)), fill = "gray", color = "#6b6b6b", lwd = 0.4)+
  geom_sf(data = africa |> dplyr::filter(iso_a3 %in% countries), fill = NA, color = "#000000", lwd=0.5)+
  ggspatial::annotation_north_arrow(
    location = "bl", which_north = "true", pad_y = unit(0.4, "in"), pad_x = unit(0.4, "in"),
    style = ggspatial::north_arrow_fancy_orienteering())+
  annotation_scale(location = "bl")+
  xlab("Longitude") + ylab("Latitude") +
  scale_fill_manual(name = "Spatial Points",
                    values = c("#2cb67b","red","#50191c"),
                    na.value = "NA",
                    guide = guide_legend(nrow = 1, byrow = T, title.position = "top"))+
  theme(legend.position = "bottom",
        plot.title = element_text(
          face = "bold", size = 100, color = "grey90", hjust = .05,
          vjust = -100
        ))+
  labs(
    x = "©2024 Author | Data: ©Aiddata and DHS program",
    y = NULL,
    title = "DHS Clusters and World Bank Aid Locations in Africa",
    subtitle = "",
    caption = ""
  )

plot

## Save map ------------------------------------
ggsave(filename = file.path(maps_dir,"africa_dhs_aid.png"),
       plot = plot,device = "png",width = 6000, height = 6000, units = "px", dpi = 800)


# ggsave(filename = file.path(maps_dir,"africa_dhs_aid.eps"),
#        plot = plot,device = "eps",width = 6000, height = 6000, units = "px", dpi = 800)


# Spatial Match Approach ----------------------------------------------


### OSM -----------------------------------------------------------------


roads <- geodata::osm("GHA",var = "highways", path = tempdir()) |> 
  st_as_sf() |> 
  dplyr::mutate(legend = case_when(
    highway == "primary" ~ "Primary Road",
    highway == "tertiary" ~ "Secondary Road",
    highway == "secondary" ~ "Tertiary Road",
    TRUE ~ "Something else."
  )) |> 
  st_transform("epsg:4326") 

places <- geodata::osm("GHA",var = "places", path = tempdir()) |> 
  st_as_sf() |> 
  dplyr::mutate(legend = "Village or Settlement",
                type = "places",
                label = name) |> 
  dplyr::select(legend, label, type) |> 
  st_transform("epsg:4326")

rail <- geodata::osm("GHA",var = "railway", path = tempdir()) |> 
  st_as_sf() |> 
  st_transform("epsg:4326")


### Aid locations -------------------------------------------------------------
aid_sf <- "./data-processed/aid_locations.shp" |> 
  sf::st_read() |> 
  st_as_sf(coords=c("long","lat"), crs=4326) 


dhs_sf<- dhs_sf |> 
  group_by(DHSCC, version_GE) 

aid_gh <- aid_sf |> 
  dplyr::filter(country=="Ghana") 


## Map ---------------------------------------------------------------------
dhs_gh <- dhs_sf |> 
  dplyr::filter(DHSCC=="GH") |> 
  dplyr::mutate(legends = "DHS Location",
                type = "DHS") |> 
  dplyr::filter(DHSYEAR == 2019) 

selection_xlim=c(-4,3)
selection_ylim=c(9.7,10)

buffer_gh <- st_buffer(dhs_gh %>% st_crop(xmin = selection_xlim[1],
                                          xmax = selection_xlim[2],
                                          ymin = selection_ylim[1],
                                          ymax = selection_ylim[2]),
                       units::as_units(50000, "m"))

dhs_gh_1 <- dhs_sf |> 
  dplyr::filter(DHSCC=="GH") |> 
  dplyr::mutate(legends = "DHS Location",
                type = "DHS") |> 
  dplyr::filter(DHSYEAR == 2019) %>% 
  st_crop(xmin = selection_xlim[1],
          xmax = selection_xlim[2],
          ymin = selection_ylim[1],
          ymax = selection_ylim[2])

#country <- africa |> dplyr::filter(iso_a3 == "GHA") 
ghana <- ne_states(country='Ghana', returnclass = 'sf')|> 
  st_transform("epsg:4326")


aid_gh <- aid_gh |>   
  dplyr::mutate(
    distance = st_is_within_distance(aid_gh, dhs_gh, dist = units::as_units(50, "km")),
    within50km = sapply(distance, length) >= 1) 


ieg_gh <- ieg_sf |> 
  dplyr::filter(country=="Ghana") %>% 
  dplyr:: filter(IEG_EvalFY<2019) %>% 
  dplyr:: filter(!is.na(ieg_outcome_edu))


# Map

bs_map <- ggplot() +
  geom_sf(data = ghana, fill = "#fff0c2", color = "#000000") +
  geom_sf(data = buffer_gh, aes(colour = "50km Buffer"), alpha = 0.1) +
  geom_sf(data = dhs_gh_1, aes(shape = "Spatial Points"), color = "#4f4f4f",
          stroke = 0.15, fill = "#25c43b", size = 3) +
  geom_sf(data = ieg_gh, aes(fill = factor(ieg_outcome_edu)), 
          shape = 22, size = 3) +
  geom_sf(data = roads, aes(linetype = legend), linewidth = 0.15) +
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true", pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_fancy_orienteering()
  ) +
  annotation_scale(location = "br") +
  xlab("Longitude") + ylab("Latitude") +
  scale_fill_manual(name = "Education WB IEG Rating before 2019",
                    values = c("red","#ff7b00","#8bb455","#1e5224","#55b476"),
                    guide = guide_legend(nrow = 3,title.position = "top"),
                    labels =c("Unsatisfactory = 2","Moderately Unsatisfactory = 3","Moderately satisfactory = 4",
                              "Satisfactory = 5")
  )+
  scale_shape_manual(
    name = "Spatial Points",
    values = c("Spatial Points" = 24),
    guide = guide_legend(nrow = 1, byrow = T, title.position = "top"),
    labels = c("Spatial Points" = "2019 DHS Survey Locations")
  ) +
  scale_colour_manual(
    name = "Buffer",
    values = "blue",
    guide = guide_legend(nrow = 1, byrow = T, title.position = "top"),
    labels = c("50km Buffer" = "50km Buffer")
  ) +
  scale_linetype_manual(name = "Road Network", 
                        values = c("dashed","twodash","dotted"), 
                        guide = guide_legend(nrow = 4, byrow = T, title.position = "top")
  )+
  theme(legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12)
  )

bs_map

## Save map ------------------------------------
ggsave(filename = file.path(maps_dir,"dhs_aid_gh.png"),
       plot = bs_map,device = "png",width = 9500, height = 9000, units = "px", dpi = 800)




## Zoomed in ------------------------------------------------------

xlim=c(-1.3,-0.4)
ylim=c(9.3,10.2)

zm_map <-ggplot() +
  geom_sf(data = ghana, fill = "#fff0c2", color = "#000000") +
  geom_sf(data = buffer_gh, aes(colour = "50km Buffer"), alpha = 0.1) +
  geom_sf(data = dhs_gh_1, aes(shape = "Spatial Points"), color = "#4f4f4f",
          stroke = 0.15, fill = "#25c43b", size = 5) +
  geom_sf(data = ieg_gh, aes(fill = factor(ieg_outcome_edu)), 
          shape = 22, size = 4) +
  #scale_size_area() +
  geom_sf(data = roads, aes(linetype = legend), linewidth = 0.15) +
  coord_sf(xlim=xlim, ylim=ylim)+
  # ggspatial::annotation_north_arrow(
  #   location = "br", which_north = "true", pad_y = unit(0.4, "in"),
  #   style = ggspatial::north_arrow_fancy_orienteering()
  # ) +
  # annotation_scale(location = "br") +
  xlab("Longitude") + ylab("Latitude") +
  scale_fill_manual(name = "World Bank IEG Rating",
                    values = c("red","#ff7b00","#8bb455","#1e5224","#55b476"),
                    labels =c("Unsatisfactory = 2","Moderately Unsatisfactory = 3","Moderately satisfactory = 4",
                              "Satisfactory = 5"),
                    guide = guide_legend(nrow = 3, byrow = T, title.position = "top")
  )+
  scale_shape_manual(
    name = "Spatial Points",
    values = c("Spatial Points" = 24),
    guide = guide_legend(nrow = 1, byrow = T, title.position = "top"),
    labels = c("Spatial Points" = "2019 DHS Survey Locations")
  ) +
  scale_colour_manual(
    name = "Buffer",
    values = "blue",
    guide = guide_legend(nrow = 1, byrow = T, title.position = "top"),
    labels = c("50km Buffer" = "50km Buffer")
  ) +
  scale_linetype_manual(name = "Road Network", 
                        values = c("dashed","twodash","dotted"), 
                        guide = guide_legend(nrow = 3, byrow = T, title.position = "top")
  )+
  theme(legend.position = "none",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12)
  )

zm_map


## Save map ------------------------------------
ggsave(filename = file.path(maps_dir,"plot_zoom.png"),
       plot = zm_map,device = "png",width = 9500, height = 9000, units = "px", dpi = 800)

# Combine 

arrowA <- data.frame(x1 = 11.5, x2 = 19.5, y1 = 16, y2 = 14)

full_map <- ggdraw(xlim = c(0, 28), ylim = c(0, 20)) +
  draw_plot(bs_map, x = 0, y = 0, width = 20, height = 20) +
  draw_plot(zm_map, x = 17, y = 8, width = 9, height = 9) +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = arrowA, 
               arrow = arrow(), lineend = "round", colour="red") 


plot(full_map)





rm(list = ls())

