# Load packages ----------------------------------------------------------------
library("haven")
library("foreign")
library("readr")
library("tidyverse")
library("dplyr")
library("data.table")
library("Hmisc")#variable labeling
library("expss")# giving variable labels for each variable
library("naniar") # Replacing missing with NAs
library("stringr")
library("forcats")
library("srvyr") #for survey
library("survey")

##Set Directories --------------------------------------------------------------
DHS_INPUT_DATA    <- "../input/"
DHS_OUTPUT        <- "../output"
DHS_PR_TEMP       <- "../temporary"
DHS_OUTPUT_LONG   <- "../long_survey"
DHS_OUTPUT_DATA   <- "../output/raw_survey"
DHS_CODE          <- "../code"

# Load overview data -----------------------------------------------------------

#dhs_fulla <- read_csv(file.path(DHS_OUTPUT,"dhs_full.csv"))
dhs_fulla <-
  expss::read_labelled_csv(file.path(DHS_OUTPUT, "dhs_full.csv"))


# Generate Variables -----------------------------------------------------------
# School indicators--Age range: 6-24
# // Currently in school: edu_inschool (hv110), attendsch_current (hv121), school_status (hv129)

# General schooling -----------------------------------------------------------

dhs_fulla <- dhs_fulla %>%
  dplyr::mutate(
    I_persons = 1,
    I_in_sch_6_24 = NA,
    I_in_sch_6_24 = ifelse(
      is.na(I_in_sch_6_24) &
        (
          school_status == 0 |
            school_status == 4 |
            school_status == 5 |
            school_status == 8
        ) &  age_member >= 6 & age_member <= 24,
      0,
      I_in_sch_6_24
    ),
    I_in_sch_6_24 = ifelse(
      is.na(I_in_sch_6_24) &
        (school_status == 1 |
           school_status == 2 |
           school_status == 3) &
        age_member >= 6 & age_member <= 24,
      1,
      I_in_sch_6_24
    ),
    I_in_sch_6_24 = ifelse(
      is.na(I_in_sch_6_24) &
        attendsch_current == 0 &
        age_member >= 6 & age_member <= 24,
      0,
      I_in_sch_6_24
    ),
    I_in_sch_6_24 = ifelse(
      is.na(I_in_sch_6_24) &
        attendsch_current == 2 &
        age_member >= 6 & age_member <= 24,
      1,
      I_in_sch_6_24
    ),
    I_in_sch_6_24 = ifelse(
      is.na(I_in_sch_6_24) &
        attendsch_current == 1 &
        age_member >= 6 & age_member <= 24,
      1,
      I_in_sch_6_24
    ),
    I_in_sch_6_24 = ifelse(
      is.na(I_in_sch_6_24) &
        edu_inschool == 0 &
        age_member >= 6 & age_member <= 24,
      0,
      I_in_sch_6_24
    ),
    I_in_sch_6_24 = ifelse(
      is.na(I_in_sch_6_24) &
        edu_inschool == 1 &
        age_member >= 6 & age_member <= 24,
      1,
      I_in_sch_6_24
    ),
    I_no_sch_6_24 = 1 - I_in_sch_6_24,
  )





# Save data --------------------------------------------------------------
dhs_fulla <- as.data.table(dhs_fulla)

write_csv(dhs_fulla, file.path(DHS_OUTPUT, "dhs_noagg.csv"))

#
#
#
# # Aggregating Survey to Cluster locations --------------------------------------


dhs_fullb <- dhs_fulla %>%
  srvyr::as_survey(weights = c(dhs_weights)) %>%
  dplyr::group_by(dhs_dhscc, dhs_version_pr, dhs_clust, dhs_year) %>%
  dplyr::summarise(
    num_ppl = sum(I_persons, na.rm = TRUE),
    num_schoolage_6_24 = sum(schoolage_6_24, na.rm = TRUE),
    num_ppl_nosch_6_24 = sum(I_no_sch_6_24, na.rm = TRUE),
    num_ppl_in_sch = sum(I_in_sch_6_24, na.rm = TRUE),
    num_ppl_other_animals = sum(other_animals, na.rm = TRUE),
    num_ppl_agric_land = sum(agric_land, na.rm = TRUE),
    num_ppl_livestock = sum(livestock, na.rm = TRUE),
    num_ppl_electric = sum(electric, na.rm = TRUE),
    num_ppl_radio = sum(radio, na.rm = TRUE),
    num_ppl_tv = sum(tv, na.rm = TRUE),
    num_ppl_fridge = sum(fridge, na.rm = TRUE),
    num_ppl_bike = sum(bike, na.rm = TRUE),
    num_ppl_phone = sum(phone, na.rm = TRUE),
    num_ppl_car_truck = sum(car_truck, na.rm = TRUE),
    num_ppl_motor_scooter = sum(motorcyle_scooter, na.rm =
                                  TRUE),
    num_ppl_pipe_water = sum(pipe_water, na.rm = TRUE),
    num_ppl_ff_roof = sum(finished_roof, na.rm = TRUE),
    num_ppl_ff_wall = sum(finished_wall, na.rm = TRUE),
    num_ppl_ff_floor = sum(finished_floor, na.rm = TRUE),
    num_ppl_sleep_net = sum(sleep_net, na.rm = TRUE),
    num_ppl_toilet = sum(toilet, na.rm = TRUE),
    num_ppl_bank_access = sum(bank_access, na.rm = TRUE),
    num_ppl_sex_headm = sum(sex_headm, na.rm = TRUE),
    num_ppl_sex_mem = sum(sex_mem, na.rm = TRUE),
    num_ppl_son_daughter = sum(son_daughter, na.rm = TRUE),
    area_rural = first(area, na.rm = TRUE),
    num_ppl_adopted_foster = sum(adopted_foster, na.rm =
                                   TRUE),
    num_ppl_grandchild = sum(grandchild, na.rm = TRUE),
    num_ppl_father_alive = sum(father_alive, na.rm = TRUE),
    num_ppl_mother_alive = sum(mother_alive, na.rm = TRUE),
    av_edu_years_25 = mean(edu_years_25, na.rm = TRUE),
    av_yearsofeduc_24 = mean(yearsofeduc_24, na.rm = TRUE),
    av_age_mm = mean(age_member, na.rm = TRUE),
    av_age_head = mean(age_head, na.rm = TRUE),
    av_size_hh = mean(size_household, na.rm = TRUE),
    num_ppl_size_hh = mean(size_household, na.rm = TRUE),
    av_yr_edu = mean(edu_years, na.rm = TRUE),
    av_ll_edu = mean(level_edu, na.rm = TRUE),
    av_water_time = mean(water_time, na.rm = TRUE)
    
  ) %>%
  dplyr::ungroup()


# Replace NaN with NA using dplyr
dhs_fullb <- dhs_fullb %>%
  dplyr::mutate_all( ~ na_if(., NaN))


# Save data
dhs_fullb <- as.data.table(dhs_fullb)

write_csv(dhs_fullb, file.path(DHS_OUTPUT, "dhs_agg.csv"))


rm(list = ls())



