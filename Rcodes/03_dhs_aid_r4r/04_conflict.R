# Load packages -----------------------------------------------------------
library("haven")
library("foreign")
library("dplyr")
library("readr")
library("lubridate")
library("zoo")
library("ggplot2")
library("Hmisc")
library("sf")
library("terra")
library("tidyverse")
library("data.table")
library("exactextractr")
library("lubridate")
library("parallel")
library("expss")
# Setup -------------------------------------------------------------------
source("./00_setup.R")



# Load conflict data set into R -------------------------------------------------

gedevent<- read_csv("./data-raw/conflict/GEDEvent_v22_1.csv")


## keep variables

ged <- gedevent %>%
  dplyr::select(
    id,
    relid,
    year,
    active_year,
    type_of_violence,
    where_prec,
    latitude,
    longitude,
    deaths_civilians,
    deaths_unknown,
    event_clarity,
    date_prec,
    date_start,
    best,
    high,
    low
  ) %>%
  dplyr::rename(
    ged_id=id,
    ged_relid = relid,
    ged_active_year = active_year,
    ged_type_of_violence = type_of_violence,
    ged_where_prec = where_prec,
    ged_latitude = latitude,
    ged_longitude = longitude,
    ged_deaths_civilians=deaths_civilians,
    ged_deaths_unknown=deaths_unknown,
    ged_event_clarity = event_clarity,
    ged_date_prec = date_prec,
    ged_date_start = date_start,
    ged_best = best,
    ged_high = high,
    ged_low = low
  ) %>%
  dplyr::mutate(
    ged_date = format(as.Date(substr(
      ged_date_start, 1, 10
    ), "%Y-%m-%d")),
    mdate = as.yearmon(ged_date, "%Y-%m"),
    ged_year = substr(ged_date_start, 1, 4),
    ged_month = substr(ged_date_start, 6, 7),
    ged_Tstart = 1
  ) %>%
  expss::apply_labels(ged_Tstart = "Indicator of actual start of conflict event (=1), and zero otherwise")

## Generate Event Type Variables 
ged <- ged %>%
  dplyr::mutate(ged_violence_type_1 = case_when(ged_type_of_violence == 1~1,TRUE~0),
                ged_violence_type_2 = case_when(ged_type_of_violence == 2~1,TRUE~0),
                ged_violence_type_3 = case_when(ged_type_of_violence == 3~1,TRUE~0))


## Generate Final Data Set 

ged <- ged %>%
  dplyr::select(ged_id,
                ged_relid,
                year,
                ged_year,
                ged_active_year,
                ged_type_of_violence,
                ged_where_prec,
                ged_latitude,
                ged_longitude,
                ged_deaths_civilians,
                ged_deaths_unknown,
                ged_best,
                ged_high,
                ged_low,
                ged_violence_type_1,
                ged_violence_type_2,
                ged_violence_type_3,
                ged_Tstart) 



# Transform as sf object ---------------------------------------------------
ged_loca <- ged %>%
  st_as_sf(coords = c("ged_longitude", "ged_latitude"),
           crs = 4326)



## save as shapefile
sf::st_write(ged_loca, dsn=file.path(dir[["data-processed"]],"ged_loca.shp"),
             layer ="ged_loca", driver = "ESRI Shapefile", append=FALSE)




rm(list = ls())
