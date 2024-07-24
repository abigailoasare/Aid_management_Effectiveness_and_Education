library("sf")
library("tidyverse")
library("geodata")
library("exactextractr")

africa <- rnaturalearth::ne_countries(returnclass = "sf") |> 
  dplyr::filter(continent == "Africa") |> 
  sf::st_make_valid()

sf::sf_use_s2(FALSE) 

## DHS Project Locations ---------------------------------------------------
dhs <- "./data-processed/dhs_clusters.shp" |> 
  sf::st_read() |> 
  sf::st_filter(africa) |> 
  sf::st_set_crs(4326) #|> 
#sf::st_transform(3857) # Project to web mercator

# 
# # Define Buffer
# buffer_dhs <- dhs |> 
#   st_buffer(units::as_units(50000, "m")) |> 
#   dplyr::mutate(
#     buffer_id = row_number()
#   )
# 
# # Centroids of Buffers
# buffer_centroids <-  st_centroid(buffer_dhs)

## GADM Data -----------------------------------------------------------
### GADM-1
ccodes <- geodata::country_codes()|>
  dplyr::filter(continent == "Africa")|>
  dplyr::select(NAME,ISO3,ISO2,UNREGION1)|>
  dplyr::filter(!ISO2 %in% c("TF", "YT", "RE", "SH"))

t0 <- Sys.time()
africa_1 <- lapply(ccodes$ISO3, function(c) {
  geodata::gadm(country = c,
                level = 1, path = tempdir())|>
    st_as_sf()
})
t1 <- Sys.time()
print(t1-t0)


africa_1 <- africa_1 |>
  bind_rows()|>
  st_make_valid() 


all(st_is_valid(africa_1))


ggplot()+
  geom_sf(data = africa_1, fill = NA)+
  geom_sf(data = dhs)


# join to buffer and GADM, select relevant names

t0 <- Sys.time()
gadm_1 <- dhs |>
  dplyr::select(DHSID, DHSYEAR) |>
  sf::st_join(africa_1) |>
  sf::st_drop_geometry() |>
  dplyr::filter(!is.na(GID_1))|>
  dplyr::select(DHSID,
                COUNTRY,
                GID_0, GID_1)|>
  dplyr::rename(
    country = COUNTRY,
    iso3code=GID_0
  )
t1 <- Sys.time()
print(t1-t0)


### GADM-2

t0 <- Sys.time()
africa_2 <- lapply(ccodes$ISO3, function(c) {
  gadm <- geodata::gadm(country = c,
                        level = 2,
                        path = tempdir()) 
  
  if (is.null(gadm)) return(NULL)
  
  gadm |>
    st_as_sf()
})
t1 <- Sys.time()
print(t1-t0)


africa_2 <- africa_2 |>
  bind_rows()|>
  st_make_valid() 


all(st_is_valid(africa_2))


t0 <- Sys.time()
gadm_2 <- dhs |>
  dplyr::select(DHSID, DHSYEAR) |>
  sf::st_join(africa_2) |>
  sf::st_drop_geometry() |>
  dplyr::filter(!is.na(GID_2))|>
  dplyr::select(DHSID,
                COUNTRY,
                GID_0, GID_1, GID_2)|>
  dplyr::rename(
    country = COUNTRY,
    iso3code=GID_0
  )
t1 <- Sys.time()
print(t1-t0)


# Merge gadm 1 and 2 -----------------------------

gadm_12 <- gadm_1 |>
  left_join(gadm_2)


# Saving regions Data ---------------------------------------------------------

write_csv(gadm_12, "./data-intermediate/gadm_regions_1_2.csv")



rm(list = ls())
