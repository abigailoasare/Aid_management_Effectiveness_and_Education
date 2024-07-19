# Load packages ----------------------------------------------------------------
library(haven)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(DescTools)
library(data.table)
library(Hmisc)
library(labelled)
library(sjlabelled)
library(expss)
library(naniar)
#load geo_packages
library("sf")
library("sp")
library("rgdal")
library("raster") # For Raster Data
library("rgeos")
library("tidyverse")
library("ncdf4")
library("ggspatial")
library("parallel")
library("data.table")
library("overviewR")
library(kableExtra)
library(gt)
library(flextable)
library(pastecs)
library(Hmisc)
library("huxtable")
library(mapedit)
library(tmap)
library(mapview)


#Set Directories ---------------------------------------------------------------
DHS_GPS               <<- "../GPS"
DHS_GPS_INPUT_DATA    <<- "../output/input/"
DHS_GPS_OUTPUT        <<- "../output"
DHS_GPS_CODE          <<- "../code"



# Load overview data -----------------------------------------------------------

overview <- read_dta(file.path(DHS_GPS_INPUT_DATA , "overview.dta"))

overview <- overview %>%
  filter(datacategory == "GIS", dcode == "GE")

ccode <- c(overview$ccode)
version_GE <- c(overview$version_GE)

variables <-
  c(
    "OBJECTID",
    "DHSID",
    "DHSCC" ,
    "DHSYEAR",
    "DHSCLUST",
    "CCFIPS",
    "ADM1FIPS",
    "ADM1FIPSNA",
    "ADM1SALBNA",
    "ADM1SALBCO",
    "ADM1DHS",
    "ADM1NAME" ,
    "DHSREGCO",
    "DHSREGNA",
    "SOURCE" ,
    "URBAN_RURA",
    "LATNUM",
    "LONGNUM",
    "ALT_GPS",
    "ALT_DEM",
    "DATUM",
    "geometry",
    "iso2code",
    "version_GE"
  )

# Separating Shape  files ------------------------------------------------------

shp_files <-
  list.files(
    DHS_GPS_INPUT_DATA ,
    pattern = ".shp$",
    recursive = T,
    ignore.case = T
  ) %>%
  as_tibble() %>%
  #dplyr::filter(value != "*.shp.xml") %>%
  dplyr::mutate(
    version_GE = stringr::str_sub(value, -8, -7),
    type = stringr::str_sub(value, -10, -9),
    ccode = stringr::str_sub(value, -12, -11)
  )



list_1 <-
  c(
    "AO",
    "BJ",
    "BT",
    "MZ",
    "BF",
    "BU",
    "CM",
    "CV",
    "CF",
    "CD",
    "TD",
    "CG",
    "CI",
    "ST",
    "EG",
    "SL",
    "EK",
    "ER",
    "ET",
    "GA",
    "GM",
    "GH",
    "GN",
    "TG",
    "TN",
    "UG",
    "KE",
    "KM",
    "LS",
    "LB",
    "MD",
    "ML",
    "MW",
    "MA",
    "NI",
    "NG",
    "MZ",
    "OS",
    "RW",
    "NM",
    "RW",
    "SN",
    "ZA",
    "SD",
    "SZ",
    "TZ",
    "ZM",
    "ZW"
  )


list_shp <- shp_files %>%
  dplyr::filter(ccode %in% list_1) %>%
  dplyr::select(value) %>%
  as.list() %>%
  unlist()



## Loop across all countries and Versions for shape files-----------------------

out_shapefile <- lapply(list_shp, function(x) {
  file <- sf::read_sf(paste(DHS_GPS_INPUT_DATA, x, sep = "/")) %>%
    sf::st_transform(4326) %>%  # Transform to WGS84 (EPSG:4326)
    as_tibble()
  
  for (f in variables[!(variables %in% names(file))]) {
    #print(f)
    f <- stringr::str_trim(f, side = "both")
    file <- file %>%
      dplyr::mutate(!!f := NA_real_)
  }
  
  file <- file %>%
    dplyr::select(DHSID,
                  DHSCC,
                  DHSYEAR,
                  DHSCLUST,
                  CCFIPS,
                  LATNUM,
                  LONGNUM,
                  geometry) %>%
    dplyr::filter(!LATNUM == 0 & !LONGNUM == 0) %>%
    mutate(# Add Information on Country and Version/Phase
      iso2code = str_sub(x, 1, 2),
      version_GE = str_sub(x, 5, 6))
  
  
  return(file)
})


## Appending all countries and versions ----------------------------------------
full_gps_shp <- out_shapefile %>%
  bind_rows()


## Saving dhs_GPS_full Data ----------------------------------------------------


dhs_clusters <- full_gps_shp %>%
  dplyr::select(DHSID,
                DHSCC,
                DHSYEAR,
                DHSCLUST,
                LATNUM,
                LONGNUM,
                geometry,
                iso2code,
                version_GE) %>%
  dplyr::mutate(dhs_id = row_number())



## Export as CSV and shapefile -------------------------------------------------

st_write(
  dhs_clusters,
  dsn = file.path(DHS_GPS_OUTPUT, "dhs_clusters.shp"),
  layer = "dhs_clusters.shp",
  driver = "ESRI Shapefile",
  append = FALSE
)

write_csv(dhs_clusters, file.path(DHS_GPS_OUTPUT, "dhs_clusters.csv"))
