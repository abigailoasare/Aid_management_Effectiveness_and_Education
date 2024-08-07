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


# Set Baseline Sample --------------------------------

reg_obs = feols(
  shr_neversch_6_24 ~  aid_complete_edu +
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


# Remove Obs that are not part of sample from original data set
sample_dhs <- reg_df[unlist(reg_obs$obs_selection), ]

# Split Sample for Rural Clusters ----------------------------------------------
reg_rr <- sample_dhs %>%
  dplyr::filter(area_rural==1)


reg_obs = feols(
  shr_neversch_6_24 ~  aid_complete_edu +
    conflict_5y_n + 
    I(conflict_5y_n == 0) +
    gpw_sum +
    sol_mean +
    pre_mean + tmp_mean + spei_mean+
    av_age_mm + av_size_hh + shr_son_daughter 
  | DHSYEAR +
    GID_2
  , data = reg_rr
)

etable(
  reg_obs,
  cluster = ~ DHSID,
  signif.code = c("*" = .1, "**" = .05,  "***" = 0.01)
  
)



## other statistics ---------------------------------------
# Number of Clusters
unique_clust<- function(x) {
  unique_elements <- unique(reg_rr$DHSID)
  length(unique_elements)
}
# We register a function
extralines_register("num_clust", unique_clust, "clust_num")

# Number of Clusters with Aid
treated_clust <- function(x) {
  unique_elements <-
    unique(reg_rr %>% dplyr::filter(wbAid > 0) %>% pull(DHSID))
  length(unique_elements)
}

# We register a function
extralines_register("tt_clust", treated_clust, "clust_tt")

# Number of Clusters with Completed Aid
comp_clust <- function(x) {
  unique_elements <-
    unique(reg_rr %>% dplyr::filter(aid_complete > 0) %>% pull(DHSID))
  length(unique_elements)
}

# We register a function
extralines_register("comp_aid", comp_clust, "aid_comp")



# Number of Clusters with Educational Aid

nontreated_clust <- function(x) {
  unique_elements <-
    unique(reg_rr %>% dplyr::filter(edu > 0) %>% pull(DHSID))
  length(unique_elements)
}

# We register a function
extralines_register("ntt_clust", nontreated_clust , "num_clust")


# Number of Clusters with Completed Educational Aid

comp_edu <- function(x) {
  unique_elements <-
    unique(reg_rr %>% dplyr::filter(aid_complete_edu > 0) %>% pull(DHSID))
  length(unique_elements)
}

# We register a function
extralines_register("edu_aid", comp_edu , "aid_edu")


# Dummies IEG  ------------------------------------------------------

##  Edu aid (dummy)  --------------------------------------

reg_edu = feols(
  shr_neversch_6_24 ~ dm_comp_aid_edu +
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
  , data = reg_rr,
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
  file = file.path(table_dir, "nosch624_dedu_50km2_rr.tex"),
  replace = TRUE,
  extralines = list(
    "_^Number of DHS Clusters" =  ~ num_clust,
    "_^Number of DHS Clusters with Aid" =  ~ tt_clust,
    "_^Number of DHS Clusters with Educational Aid" =  ~ ntt_clust,
    "_^Number of DHS Clusters with Completed Aid" =  ~ comp_aid,
    "_^Number of DHS Clusters with Completed Educational Aid" =  ~ edu_aid
  )
  
)

## IEG (dummy) ----------------------------------------------------------
reg_edu = feols(
  shr_neversch_6_24 ~ dm_comp_ieg_edu_hs+
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
  , data = reg_rr,
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
  file = file.path(table_dir, "nosch624_dhmlieg_50km2_rr.tex"),
  replace = TRUE,
  extralines = list(
    "_^Number of DHS Clusters" =  ~ num_clust,
    "_^Number of DHS Clusters with Aid" =  ~ tt_clust,
    "_^Number of DHS Clusters with Educational Aid" =  ~ ntt_clust,
    "_^Number of DHS Clusters with Completed Aid" =  ~ comp_aid,
    "_^Number of DHS Clusters with Completed Educational Aid" =  ~ edu_aid
    
  )
)



## Interactions (dummies)--------------------------------------------------------- 


reg_edu = feols(
  shr_neversch_6_24 ~ dm_comp_ieg_edu_hs + 
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
  , data = reg_rr,
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
  file = file.path(table_dir, "nosch624_dintxhmlieg_50km2_rr.tex"),
  replace = TRUE,
  extralines = list(
    "_^Number of DHS Clusters" =  ~ num_clust,
    "_^Number of DHS Clusters with Aid" =  ~ tt_clust,
    "_^Number of DHS Clusters with Educational Aid" =  ~ ntt_clust,
    "_^Number of DHS Clusters with Completed Aid" =  ~ comp_aid,
    "_^Number of DHS Clusters with Completed Educational Aid" =  ~ edu_aid
    
  )
)




# Counts ------------------------------------------------------------

## Edu. Aid (counts) ------------------------------------------------------


# shr_neversch_6_24 = "Share of ID 6--24yrs with no School"

reg_edu = feols(
  shr_neversch_6_24 ~ aid_complete_edu +
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
  , data = reg_rr,
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
  file = file.path(table_dir, "nosch624_edu_50km2_rr.tex"),
  replace = TRUE,
  extralines = list(
    "_^Number of DHS Clusters" =  ~ num_clust,
    "_^Number of DHS Clusters with Aid" =  ~ tt_clust,
    "_^Number of DHS Clusters with Educational Aid" =  ~ ntt_clust,
    "_^Number of DHS Clusters with Completed Aid" =  ~ comp_aid,
    "_^Number of DHS Clusters with Completed Educational Aid" =  ~ edu_aid
    
  )
  
)




## IEG (Counts) ------------------------------------------------------


reg_edu = feols(
  shr_neversch_6_24 ~ comp_ieg_edu_hs +
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
  , data = reg_rr,
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
  file = file.path(table_dir, "nosch624_hmlieg_50km2_rr.tex"),
  replace = TRUE,
  extralines = list(
    "_^Number of DHS Clusters" =  ~ num_clust,
    "_^Number of DHS Clusters with Aid" =  ~ tt_clust,
    "_^Number of DHS Clusters with Educational Aid" =  ~ ntt_clust,
    "_^Number of DHS Clusters with Completed Aid" =  ~ comp_aid,
    "_^Number of DHS Clusters with Completed Educational Aid" =  ~ edu_aid
    
  )
  
)


## Interactions (counts)--------------------------------------------------------- 


reg_edu = feols(
  shr_neversch_6_24 ~ comp_ieg_edu_hs + 
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
  , data = reg_rr,
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
  file = file.path(table_dir, "nosch624_intxhmlieg_50km2_rr.tex"),
  replace = TRUE,
  extralines = list(
    "_^Number of DHS Clusters" =  ~ num_clust,
    "_^Number of DHS Clusters with Aid" =  ~ tt_clust,
    "_^Number of DHS Clusters with Educational Aid" =  ~ ntt_clust,
    "_^Number of DHS Clusters with Completed Aid" =  ~ comp_aid,
    "_^Number of DHS Clusters with Completed Educational Aid" =  ~ edu_aid
    
  )
  
)

rm(reg_obs,sample_dhs)

# Disbursement ----------------------------------------------------------

## Preliminaries ----------------------------------------------------

### Set Estimation Sample --------------------------------

reg_obs = feols(
  shr_neversch_6_24 ~  dbxr2011_comp_edu1 +
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



# Remove Obs that are not part of sample from original data set
sample_dhs <- reg_df[unlist(reg_obs$obs_selection), ]

### Split Sample for Rural Clusters ----------------------------------------------
reg_rr <- sample_dhs %>%
  dplyr::filter(area_rural==1)


reg_obs = feols(
  shr_neversch_6_24 ~  dbxr2011_comp_edu1 +
    conflict_5y_n + 
    I(conflict_5y_n == 0) +
    gpw_sum +
    sol_mean +
    pre_mean + tmp_mean + spei_mean+
    av_age_mm + av_size_hh + shr_son_daughter 
  | DHSYEAR +
    GID_2
  , data = reg_rr
)


etable(
  reg_obs,
  cluster = ~ DHSID,
  signif.code = c("*" = .1, "**" = .05,  "***" = 0.01)
  
)


# Remove Obs that are not part of Rural from original data set
sample_dhs <- reg_rr[unlist(reg_obs$obs_selection), ]



### other statistics ---------------------------------------
# Number of Clusters
unique_clust<- function(x) {
  unique_elements <- unique(reg_rr$DHSID)
  length(unique_elements)
}
# We register a function
extralines_register("num_clust", unique_clust, "clust_num")

# Number of Clusters with Aid
treated_clust <- function(x) {
  unique_elements <-
    unique(reg_rr %>% dplyr::filter(wbAid > 0) %>% pull(DHSID))
  length(unique_elements)
}

# We register a function
extralines_register("tt_clust", treated_clust, "clust_tt")

# Number of Clusters with Completed Aid
comp_clust <- function(x) {
  unique_elements <-
    unique(reg_rr %>% dplyr::filter(aid_complete > 0) %>% pull(DHSID))
  length(unique_elements)
}

# We register a function
extralines_register("comp_aid", comp_clust, "aid_comp")



# Number of Clusters with Educational Aid

nontreated_clust <- function(x) {
  unique_elements <-
    unique(reg_rr %>% dplyr::filter(edu > 0) %>% pull(DHSID))
  length(unique_elements)
}

# We register a function
extralines_register("ntt_clust", nontreated_clust , "num_clust")


# Number of Clusters with Completed Educational Aid

comp_edu <- function(x) {
  unique_elements <-
    unique(reg_rr %>% dplyr::filter(aid_complete_edu > 0) %>% pull(DHSID))
  length(unique_elements)
}

# We register a function
extralines_register("edu_aid", comp_edu , "aid_edu")



### Total Value (disb) ------------------------------------------------------

reg_edu = feols(
  shr_neversch_6_24 ~ dbxr2011_comp_edu1 +
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
  , data = reg_rr,
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
  file = file.path(table_dir, "nosch624_edu_ndislog_50km2_rr.tex"),
  replace = TRUE,
  extralines = list(
    "_^Number of DHS Clusters" =  ~ num_clust,
    "_^Number of DHS Clusters with Aid" =  ~ tt_clust,
    "_^Number of DHS Clusters with Educational Aid" =  ~ ntt_clust,
    "_^Number of DHS Clusters with Completed Aid" =  ~ comp_aid,
    "_^Number of DHS Clusters with Completed Educational Aid" =  ~ edu_aid
    
  )
  
)

### IEG (disb) ------------------------------------------------------


reg_edu = feols(
  shr_neversch_6_24 ~ dbxr2011_comp_ieg_edu_hs1 +
    csw0(dbxr2011_comp_ieg_edu_ms1,  dbxr2011_comp_ieg_edu_ls1, 
         conflict_5y_n + 
           I(conflict_5y_n == 0) +
           gpw_sum,
         sol_mean,
         pre_mean + tmp_mean + spei_mean+
           av_age_mm + av_size_hh + shr_son_daughter 
    )
  | DHSYEAR +
    GID_2
  
  , data = reg_rr,
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
  file = file.path(table_dir, "nosch624_hmlieg_ndislog_50km2_rr.tex"),
  replace = TRUE,
  extralines = list(
    "_^Number of DHS Clusters" =  ~ num_clust,
    "_^Number of DHS Clusters with Aid" =  ~ tt_clust,
    "_^Number of DHS Clusters with Educational Aid" =  ~ ntt_clust,
    "_^Number of DHS Clusters with Completed Aid" =  ~ comp_aid,
    "_^Number of DHS Clusters with Completed Educational Aid" =  ~ edu_aid
    
  )
  
)


### Interactions (disb)--------------------------------------------------------- 


reg_edu = feols(
  shr_neversch_6_24 ~ dbxr2011_comp_ieg_edu_hs1 + 
    csw0(dbxr2011_comp_ieg_edu_ms1,dbxr2011_comp_ieg_edu_ls1,
         I(dbxr2011_comp_ieg_edu_hs1*dbxr2011_comp_ieg_edu_ms1),
         I(dbxr2011_comp_ieg_edu_hs1*dbxr2011_comp_ieg_edu_ls1),
         I(dbxr2011_comp_ieg_edu_ms1*dbxr2011_comp_ieg_edu_ls1),
         conflict_5y_n + 
           I(conflict_5y_n == 0) +
           gpw_sum,
         sol_mean,
         pre_mean + tmp_mean + spei_mean+
           av_age_mm + av_size_hh + shr_son_daughter 
    )
  | DHSYEAR +
    GID_2
  , data = reg_rr,
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
  file = file.path(table_dir, "nosch624_intxhmlieg_ndislog_50km2_rr.tex"),
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

