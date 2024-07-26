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

# Setup -------------------------------------------------------------------
source("./00_setup.R")

# Load overview data -----------------------------------------------------------


dhs_fulla <-
  expss::read_labelled_csv(file.path(dir[["data-processed"]], "dhs_full.csv"))


# Generate Variables -----------------------------------------------------------
# School indicators--Age range: 6-24
# // Illiteracy: edu_inschool (hv110), attendsch_current (hv121),
# school_status (hv129),  edu_attainment (hv109), level_edu(hv106)

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
  ) %>% 
  dplyr::mutate(
    neversch_6_24 = NA,
    neversch_6_24 = ifelse(
      is.na(neversch_6_24) & 
        (school_status == 0| 
           school_status == 4 |
           school_status == 5 |
           school_status == 8) & 
        level_edu==0 & 
        (edu_attainment==0|edu_attainment==1) &  
        age_member >= 6 & age_member <= 24,1,0
    ),
    neversch_6_24 = ifelse(
      is.na(neversch_6_24) 
      & (I_in_sch_6_24 == 1) 
      &  age_member >= 6 & age_member <= 24,0,neversch_6_24
    ),
    neversch_6_24 = ifelse(
      is.na(I_in_sch_6_24), NA_real_, neversch_6_24
    ),
    neversch_6_24 = ifelse(
      is.na(neversch_6_24) &
        (level_edu==0|level_edu==1) & 
        (edu_attainment==0|edu_attainment==1) &
        age_member >= 6 & age_member <= 24, 1, neversch_6_24
    ),
    neversch_6_24 = ifelse(
      I_in_sch_6_24!=1 &
        (level_edu==0|level_edu==1) & 
        (edu_attainment==0|edu_attainment==1) &
        age_member >= 6 & age_member <= 24, 1, neversch_6_24
    )
  )



# Repeating and drop out -------------------------------------------------

dhs_fulla <-dhs_fulla %>% 
  dplyr::mutate(
    repeatsch_6_24 = NA,
    repeatsch_6_24 = ifelse(
      is.na(repeatsch_6_24) & (school_status == 3) &  age_member >= 6 & age_member <= 24,1,0
    ),
    repeatsch_6_24 = ifelse(
      is.na(repeatsch_6_24) & (I_in_sch_6_24 == 1) &  age_member >= 6 & age_member <= 24,0,repeatsch_6_24
    ),
    repeatsch_6_24 = ifelse(
      is.na(I_in_sch_6_24), NA_real_, repeatsch_6_24
    ),
    dropsch_6_24 = NA,
    dropsch_6_24 = ifelse(
      is.na(dropsch_6_24) & (school_status == 4) &  age_member >= 6 & age_member <= 24,1,0
    ),
    dropsch_6_24 = ifelse(
      is.na(dropsch_6_24) & (I_in_sch_6_24 == 1) &  age_member >= 6 & age_member <= 24,0,dropsch_6_24
    ),
    dropsch_6_24 = ifelse(
      is.na(I_in_sch_6_24), NA_real_, dropsch_6_24
    )
  )

#crosscheck
test<-dhs_fulla %>% 
  select(age_member, school_status,level_edu,edu_attainment,edu_inschool,
         attendsch_current,I_in_sch_6_24,I_no_sch_6_24,neversch_6_24,
         repeatsch_6_24,dropsch_6_24) %>% 
  #filter(neversch_6_24==1) %>% 
  #filter(level_edu==0|edu_attainment==0|edu_attainment==1) %>% 
  filter(age_member >= 6 & age_member <= 24) %>% 
  filter(!is.na(neversch_6_24)) 


# # Aggregating Survey to Cluster locations --------------------------------------


dhs_fullb <- dhs_fulla %>%
  srvyr::as_survey(weights = c(dhs_weights)) %>%
  dplyr::group_by(dhs_dhscc, dhs_version_pr, dhs_clust, dhs_year) %>%
  dplyr::summarise(
    num_ppl = sum(I_persons, na.rm = TRUE),
    num_schoolage_6_24 = sum(schoolage_6_24, na.rm = TRUE),
    num_dhs_survey = n_distinct(dhs_version_pr, na.rm = TRUE),
    num_ppl_neversch_6_24 = sum(neversch_6_24, na.rm = TRUE),
    num_ppl_nosch_6_24 = sum(I_no_sch_6_24, na.rm = TRUE),
    num_ppl_repeatsch_6_24 = sum(repeatsch_6_24, na.rm = TRUE),
    num_ppl_dropsch_6_24 = sum(dropsch_6_24, na.rm = TRUE),
    num_ppl_in_sch = sum(I_in_sch_6_24, na.rm = TRUE),
    num_ppl_sex_headm = sum(sex_headm, na.rm = TRUE),
    num_ppl_sex_mem = sum(sex_mem, na.rm = TRUE),
    num_ppl_son_daughter = sum(son_daughter, na.rm = TRUE),
    area_rural = first(area, na.rm = TRUE),
    av_age_mm = mean(age_member, na.rm = TRUE),
    av_age_head = mean(age_head, na.rm = TRUE),
    av_size_hh = mean(size_household, na.rm = TRUE),
    num_ppl_size_hh = mean(size_household, na.rm = TRUE),
    av_yr_edu = mean(edu_years, na.rm = TRUE),
    av_ll_edu = mean(level_edu, na.rm = TRUE)
  ) %>%
  dplyr::ungroup()


# Replace NaN with NA using dplyr
dhs_fullb <- dhs_fullb %>%
  dplyr::mutate_all( ~ na_if(., NaN))


# Save data
dhs_fullb <- as.data.table(dhs_fullb)

write_csv(dhs_fullb, file.path(dir[["data-processed"]], "dhs_agg.csv"))


rm(list = ls())




