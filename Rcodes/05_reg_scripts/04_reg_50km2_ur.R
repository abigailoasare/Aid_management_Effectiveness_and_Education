# Load packages ----------------------------------------------------------------
library("haven")
library("dplyr")
library("foreign")
library("tidyverse")
library("ggplot2")
library("data.table")
library("expss")
library("Hmisc")
library("fixest")

table_dir    <- "./output/tables/"

# Read data --------------------------------------------------------------------


# Read the gzipped CSV file
reg_df <- fread("./data-r4r/data_50km.csv.gz")


# Generate dummies for IEG 

reg_df <- reg_df %>%
  dplyr::mutate(
    dm_comp_aid_edu = ifelse(aid_complete_edu > 0, 1, 0),
    dm_comp_ieg_edu_hs = ifelse(comp_ieg_edu_hs > 0, 1, 0),
    dm_comp_ieg_edu_ms = ifelse(comp_ieg_edu_ms > 0, 1, 0),
    dm_comp_ieg_edu_ls = ifelse(comp_ieg_edu_ls > 0, 1, 0),
  ) %>% 
  mutate(gpw_sum=gpw_ip_sum/100000, 
         shr_nosch_6_24=shr_nosch_6_24*100,
         GID_2=ifelse(is.na(GID_2),iso2code,GID_2)
  )  

# Set Baseline Sample --------------------------------

reg_obs = feols(
  shr_nosch_6_24 ~  aid_complete_edu +
    conflict_5y_n + 
    I(conflict_5y_n == 0) +
    gpw_sum +
    sol_mean +
    pre_mean + tmp_mean + spei_mean+
    av_age_mm + av_size_hh + shr_son_daughter 
  | DHSYEAR +
    GID_2
  , data = reg_df
)

etable(
  reg_obs,
  cluster = ~ DHSID,
  signif.code = c("*" = .1, "**" = .05,  "***" = 0.01)
  
)


# Preliminaries ----------------------------------------------------

## Summary Statistic Regression Sample --------------------------------

# Remove Obs that are not part of sample from original data set
sample_dhs <- reg_df[unlist(reg_obs$obs_selection), ]

# Split Sample for Rural Clusters ----------------------------------------------
reg_ur <- sample_dhs %>%
  dplyr::filter(area_rural==0)


reg_obs = feols(
  shr_nosch_6_24 ~  aid_complete_edu +
    conflict_5y_n + 
    I(conflict_5y_n == 0) +
    gpw_sum +
    sol_mean +
    pre_mean + tmp_mean + spei_mean+
    av_age_mm + av_size_hh + shr_son_daughter 
  | DHSYEAR +
    GID_2
  , data = reg_ur
)

etable(
  reg_obs,
  cluster = ~ DHSID,
  signif.code = c("*" = .1, "**" = .05,  "***" = 0.01)
  
)



## other statistics ---------------------------------------
# Number of Clusters
unique_clust<- function(x) {
  unique_elements <- unique(reg_ur$DHSID)
  length(unique_elements)
}
# We register a function
extralines_register("num_clust", unique_clust, "clust_num")

# Number of Clusters with Aid
treated_clust <- function(x) {
  unique_elements <-
    unique(reg_ur %>% dplyr::filter(wbAid > 0) %>% pull(DHSID))
  length(unique_elements)
}

# We register a function
extralines_register("tt_clust", treated_clust, "clust_tt")

# Number of Clusters with Completed Aid
comp_clust <- function(x) {
  unique_elements <-
    unique(reg_ur %>% dplyr::filter(aid_complete > 0) %>% pull(DHSID))
  length(unique_elements)
}

# We register a function
extralines_register("comp_aid", comp_clust, "aid_comp")



# Number of Clusters with Educational Aid

nontreated_clust <- function(x) {
  unique_elements <-
    unique(reg_ur %>% dplyr::filter(edu > 0) %>% pull(DHSID))
  length(unique_elements)
}

# We register a function
extralines_register("ntt_clust", nontreated_clust , "num_clust")


# Number of Clusters with Completed Educational Aid

comp_edu <- function(x) {
  unique_elements <-
    unique(reg_ur %>% dplyr::filter(aid_complete_edu > 0) %>% pull(DHSID))
  length(unique_elements)
}

# We register a function
extralines_register("edu_aid", comp_edu , "aid_edu")



# Edu. Aid ------------------------------------------------------


# shr_nosch_6_24 = "Share of ID 6--24yrs with no School"

reg_edu = feols(
  shr_nosch_6_24 ~ aid_complete_edu +
    csw0(
      conflict_5y_n + 
        I(conflict_5y_n == 0) +
        gpw_sum,
      sol_mean,
      pre_mean + tmp_mean + spei_mean+
        av_age_mm + av_size_hh + shr_son_daughter 
    )
  | DHSYEAR +
    GID_2
  , data = reg_ur,
  subset = obs(reg_obs)
)


etable(
  reg_edu,
  cluster = ~ DHSID,
  signif.code = c("*" = .1, "**" = .05,  "***" = 0.01)
)


etable(
  reg_edu,
  cluster = ~ DHSID,
  signif.code = c("*" = .1, "**" = .05,  "***" = 0.01),
  title = "Share of HH 6--24yrs with no School",
  fitstat = ~ . + ivfall + ivwaldall.p,
  tex = TRUE,
  file = file.path(table_dir, "nosch624_edu_50km2_ur.tex"),
  replace = TRUE,
  extralines = list(
    "_^Number of DHS Clusters" =  ~ num_clust,
    "_^Number of DHS Clusters with Aid" =  ~ tt_clust,
    "_^Number of DHS Clusters with Educational Aid" =  ~ ntt_clust,
    "_^Number of DHS Clusters with Completed Aid" =  ~ comp_aid,
    "_^Number of DHS Clusters with Completed Educational Aid" =  ~ edu_aid
    
  )
  
)




## All together ------------------------------------------------------


reg_edu = feols(
  shr_nosch_6_24 ~ comp_ieg_edu_hs +
    csw0(comp_ieg_edu_ms,  comp_ieg_edu_ls, 
         conflict_5y_n + 
           I(conflict_5y_n == 0) +
           gpw_sum,
         sol_mean,
         pre_mean + tmp_mean + spei_mean+
           av_age_mm + av_size_hh + shr_son_daughter 
    )
  | DHSYEAR +
    GID_2
  , data = reg_ur,
  subset = obs(reg_obs)
)


etable(
  reg_edu,
  cluster = ~ DHSID,
  signif.code = c("*" = .1, "**" = .05,  "***" = 0.01)
)


etable(
  reg_edu,
  cluster = ~ DHSID,
  signif.code = c("*" = .1, "**" = .05,  "***" = 0.01),
  title = "Share of HH 6--24yrs with no School",
  fitstat = ~ . + ivfall + ivwaldall.p,
  tex = TRUE,
  file = file.path(table_dir, "nosch624_hmlieg_50km2_ur.tex"),
  replace = TRUE,
  extralines = list(
    "_^Number of DHS Clusters" =  ~ num_clust,
    "_^Number of DHS Clusters with Aid" =  ~ tt_clust,
    "_^Number of DHS Clusters with Educational Aid" =  ~ ntt_clust,
    "_^Number of DHS Clusters with Completed Aid" =  ~ comp_aid,
    "_^Number of DHS Clusters with Completed Educational Aid" =  ~ edu_aid
    
  )
  
)


# Interactions (counts)--------------------------------------------------------- 


reg_edu = feols(
  shr_nosch_6_24 ~ comp_ieg_edu_hs + 
    csw0(comp_ieg_edu_ms,comp_ieg_edu_ls,
         I(comp_ieg_edu_hs*comp_ieg_edu_ms),
         I(comp_ieg_edu_hs*comp_ieg_edu_ls),
         I(comp_ieg_edu_ms*comp_ieg_edu_ls),
         conflict_5y_n + 
           I(conflict_5y_n == 0) +
           gpw_sum,
         sol_mean,
         pre_mean + tmp_mean + spei_mean+
           av_age_mm + av_size_hh + shr_son_daughter 
    )
  | DHSYEAR +
    GID_2
  , data = reg_ur,
  subset = obs(reg_obs)
)


etable(
  reg_edu,
  cluster = ~ DHSID,
  signif.code = c("*" = .1, "**" = .05,  "***" = 0.01)
)


etable(
  reg_edu,
  cluster = ~ DHSID,
  signif.code = c("*" = .1, "**" = .05,  "***" = 0.01),
  title = "Share of HH 6--24yrs with no School",
  fitstat = ~ . + ivfall + ivwaldall.p,
  tex = TRUE,
  file = file.path(table_dir, "nosch624_intxhmlieg_50km2_ur.tex"),
  replace = TRUE,
  extralines = list(
    "_^Number of DHS Clusters" =  ~ num_clust,
    "_^Number of DHS Clusters with Aid" =  ~ tt_clust,
    "_^Number of DHS Clusters with Educational Aid" =  ~ ntt_clust,
    "_^Number of DHS Clusters with Completed Aid" =  ~ comp_aid,
    "_^Number of DHS Clusters with Completed Educational Aid" =  ~ edu_aid
    
  )
  
)


# Dummies IEG  ------------------------------------------------------

## Any Edu aid  ------

reg_edu = feols(
  shr_nosch_6_24 ~ dm_comp_aid_edu +
    csw0(
      conflict_5y_n + 
        I(conflict_5y_n == 0) +
        gpw_sum,
      sol_mean,
      pre_mean + tmp_mean + spei_mean+
        av_age_mm + av_size_hh + shr_son_daughter 
    )
  | DHSYEAR +
    GID_2
  , data = reg_ur,
  subset = obs(reg_obs)
)


etable(
  reg_edu,
  cluster = ~ DHSID,
  signif.code = c("*" = .1, "**" = .05,  "***" = 0.01)
)


etable(
  reg_edu,
  cluster = ~ DHSID,
  signif.code = c("*" = .1, "**" = .05,  "***" = 0.01),
  title = "Share of HH 6--24yrs with no School",
  fitstat = ~ . + ivfall + ivwaldall.p,
  tex = TRUE,
  file = file.path(table_dir, "nosch624_dedu_50km2_ur.tex"),
  replace = TRUE,
  extralines = list(
    "_^Number of DHS Clusters" =  ~ num_clust,
    "_^Number of DHS Clusters with Aid" =  ~ tt_clust,
    "_^Number of DHS Clusters with Educational Aid" =  ~ ntt_clust,
    "_^Number of DHS Clusters with Completed Aid" =  ~ comp_aid,
    "_^Number of DHS Clusters with Completed Educational Aid" =  ~ edu_aid
  )
  
)

## Together --------------
reg_edu = feols(
  shr_nosch_6_24 ~ dm_comp_ieg_edu_hs+
    csw0(dm_comp_ieg_edu_ms, dm_comp_ieg_edu_ls,
         conflict_5y_n + 
           I(conflict_5y_n == 0) +
           gpw_sum,
         sol_mean,
         pre_mean + tmp_mean + spei_mean+
           av_age_mm + av_size_hh + shr_son_daughter 
    )
  | DHSYEAR +
    GID_2
  , data = reg_ur,
  subset = obs(reg_obs)
)


etable(
  reg_edu,
  cluster = ~ DHSID,
  signif.code = c("*" = .1, "**" = .05,  "***" = 0.01)
)


etable(
  reg_edu,
  cluster = ~ DHSID,
  signif.code = c("*" = .1, "**" = .05,  "***" = 0.01),
  title = "Share of HH 6--24yrs with no School",
  fitstat = ~ . + ivfall + ivwaldall.p,
  tex = TRUE,
  file = file.path(table_dir, "nosch624_dhmlieg_50km2_ur.tex"),
  replace = TRUE,
  extralines = list(
    "_^Number of DHS Clusters" =  ~ num_clust,
    "_^Number of DHS Clusters with Aid" =  ~ tt_clust,
    "_^Number of DHS Clusters with Educational Aid" =  ~ ntt_clust,
    "_^Number of DHS Clusters with Completed Aid" =  ~ comp_aid,
    "_^Number of DHS Clusters with Completed Educational Aid" =  ~ edu_aid
    
  )
)



## interactions (dummies)--------------------------------------------------------- 


reg_edu = feols(
  shr_nosch_6_24 ~ dm_comp_ieg_edu_hs + 
    csw0(dm_comp_ieg_edu_ms,  dm_comp_ieg_edu_ls,
         I(dm_comp_ieg_edu_hs*dm_comp_ieg_edu_ms),
         I(dm_comp_ieg_edu_hs*dm_comp_ieg_edu_ls),
         I(dm_comp_ieg_edu_ms*dm_comp_ieg_edu_ls),
         conflict_5y_n + 
           I(conflict_5y_n == 0) +
           gpw_sum,
         sol_mean,
         pre_mean + tmp_mean + spei_mean+
           av_age_mm + av_size_hh + shr_son_daughter 
    )
  | DHSYEAR +
    GID_2
  , data = reg_ur,
  subset = obs(reg_obs)
)


etable(
  reg_edu,
  cluster = ~ DHSID,
  signif.code = c("*" = .1, "**" = .05,  "***" = 0.01)
)


etable(
  reg_edu,
  cluster = ~ DHSID,
  signif.code = c("*" = .1, "**" = .05,  "***" = 0.01),
  title = "Share of HH 6--24yrs with no School",
  fitstat = ~ . + ivfall + ivwaldall.p,
  tex = TRUE,
  file = file.path(table_dir, "nosch624_dintxhmlieg_50km2_ur.tex"),
  replace = TRUE,
  extralines = list(
    "_^Number of DHS Clusters" =  ~ num_clust,
    "_^Number of DHS Clusters with Aid" =  ~ tt_clust,
    "_^Number of DHS Clusters with Educational Aid" =  ~ ntt_clust,
    "_^Number of DHS Clusters with Completed Aid" =  ~ comp_aid,
    "_^Number of DHS Clusters with Completed Educational Aid" =  ~ edu_aid
    
  )
)







rm(list = ls())

