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

# Set directories
table_dir <- "./output/tables/"
data_dir <- "./data-r4r/"

# Define a function to perform the analysis for a given distance
run_analysis <- function(distance) {
  # Read data --------------------------------------------------------------------
  data_file <- paste0(data_dir, "data_", distance, "km.csv.gz")
  reg_df <- fread(data_file)
  
  # Generate dummies for IEG 
  reg_df <- reg_df %>%
    mutate(
      dm_comp_aid_edu = ifelse(aid_complete_edu > 0, 1, 0),
      dm_comp_ieg_edu_hs = ifelse(comp_ieg_edu_hs > 0, 1, 0),
      dm_comp_ieg_edu_ms = ifelse(comp_ieg_edu_ms > 0, 1, 0),
      dm_comp_ieg_edu_ls = ifelse(comp_ieg_edu_ls > 0, 1, 0),
      dbxr2011_comp_edu1 = log(0.01+dbxr2011_comp_edu),
      dbxr2011_comp_ieg_edu_hs1 = log(0.01+dbxr2011_comp_ieg_edu_hs),
      dbxr2011_comp_ieg_edu_ms1 = log(0.01+dbxr2011_comp_ieg_edu_ms),
      dbxr2011_comp_ieg_edu_ls1 = log(0.01+dbxr2011_comp_ieg_edu_ls),
      gpw_sum = gpw_ip_sum / 100000,
      shr_neversch_6_24=shr_neversch_6_24*100,
      GID_2=ifelse(is.na(GID_2),iso2code,GID_2)
    )
  
  # Set Estimation Sample --------------------------------
  reg_obs <- feols(
    shr_neversch_6_24 ~ aid_complete_edu +
      conflict_5y_n + 
      I(conflict_5y_n == 0) +
      gpw_sum +
      sol_mean +
      pre_mean + tmp_mean + spei_mean +
      av_age_mm + av_size_hh + shr_son_daughter 
    | DHSYEAR + GID_2,
    data = reg_df
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
  
  ## other statistics ---------------------------------------
  # Number of Clusters
  unique_clust<- function(x) {
    unique_elements <- unique(sample_dhs$DHSID)
    length(unique_elements)
  }
  # We register a function
  extralines_register("num_clust", unique_clust, "clust_num")
  
  # Number of Clusters with Aid
  treated_clust <- function(x) {
    unique_elements <-
      unique(sample_dhs %>% dplyr::filter(wbAid > 0) %>% pull(DHSID))
    length(unique_elements)
  }
  
  # We register a function
  extralines_register("tt_clust", treated_clust, "clust_tt")
  
  # Number of Clusters with Completed Aid
  comp_clust <- function(x) {
    unique_elements <-
      unique(sample_dhs %>% dplyr::filter(aid_complete > 0) %>% pull(DHSID))
    length(unique_elements)
  }
  
  # We register a function
  extralines_register("comp_aid", comp_clust, "aid_comp")
  
  
  
  # Number of Clusters with Educational Aid
  
  nontreated_clust <- function(x) {
    unique_elements <-
      unique(sample_dhs %>% dplyr::filter(edu > 0) %>% pull(DHSID))
    length(unique_elements)
  }
  
  # We register a function
  extralines_register("ntt_clust", nontreated_clust , "num_clust")
  
  
  # Number of Clusters with Completed Educational Aid
  
  comp_edu <- function(x) {
    unique_elements <-
      unique(sample_dhs %>% dplyr::filter(aid_complete_edu > 0) %>% pull(DHSID))
    length(unique_elements)
  }
  
  # We register a function
  extralines_register("edu_aid", comp_edu , "aid_edu")
  
  
  # Dummies IEG  ------------------------------------------------------
  
  ## Edu aid (dummies) -------------------------------------------------------
  
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
    , data = reg_df,
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
    file = file.path(table_dir, paste0("nosch624_dedu_", distance, "km2.tex")),
    replace = TRUE,
    extralines = list(
      "_^Number of DHS Clusters" =  ~ num_clust,
      "_^Number of DHS Clusters with Aid" =  ~ tt_clust,
      "_^Number of DHS Clusters with Educational Aid" =  ~ ntt_clust,
      "_^Number of DHS Clusters with Completed Aid" =  ~ comp_aid,
      "_^Number of DHS Clusters with Completed Educational Aid" =  ~ edu_aid
    )
    
  )
  
  ## IEG (dummies) ----------------------------------------------------------------
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
    , data = reg_df,
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
    file = file.path(table_dir, paste0("nosch624_dhmlieg_", distance, "km2.tex")),
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
    , data = reg_df,
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
    file = file.path(table_dir, paste0("nosch624_dintxhmlieg_", distance, "km2.tex")),
    replace = TRUE,
    extralines = list(
      "_^Number of DHS Clusters" =  ~ num_clust,
      "_^Number of DHS Clusters with Aid" =  ~ tt_clust,
      "_^Number of DHS Clusters with Educational Aid" =  ~ ntt_clust,
      "_^Number of DHS Clusters with Completed Aid" =  ~ comp_aid,
      "_^Number of DHS Clusters with Completed Educational Aid" =  ~ edu_aid
      
    )
  )
  
  
  # Counts ------------------------------------------------------
  
  
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
    , data = reg_df,
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
    file = file.path(table_dir, paste0("nosch624_edu_", distance, "km2.tex")),
    replace = TRUE,
    extralines = list(
      "_^Number of DHS Clusters" =  ~ num_clust,
      "_^Number of DHS Clusters with Aid" =  ~ tt_clust,
      "_^Number of DHS Clusters with Educational Aid" =  ~ ntt_clust,
      "_^Number of DHS Clusters with Completed Aid" =  ~ comp_aid,
      "_^Number of DHS Clusters with Completed Educational Aid" =  ~ edu_aid
      
    )
    
  )
  
  
  
  ## IEG (counts) ------------------------------------------------------
  
  
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
    , data = reg_df,
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
    file = file.path(table_dir, paste0("nosch624_hmlieg_", distance, "km2.tex")),
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
      csw0(comp_ieg_edu_ms,  comp_ieg_edu_ls,
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
    , data = reg_df,
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
    file = file.path(table_dir, paste0("nosch624_intxhmlieg_", distance, "km2.tex")),
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
  
  # Disbursement ---------------------------------------------------
  
  ## Set Estimation Sample --------------------------------
  
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
  
  ## Preliminaries ----------------------------------------------------
  
  # Remove Obs that are not part of sample from original data set
  sample_dhs <- reg_df[unlist(reg_obs$obs_selection), ]
  
  
  
  ## other statistics ---------------------------------------
  # Number of Clusters
  unique_clust<- function(x) {
    unique_elements <- unique(sample_dhs$DHSID)
    length(unique_elements)
  }
  # We register a function
  extralines_register("num_clust", unique_clust, "clust_num")
  
  # Number of Clusters with Aid
  treated_clust <- function(x) {
    unique_elements <-
      unique(sample_dhs %>% dplyr::filter(wbAid > 0) %>% pull(DHSID))
    length(unique_elements)
  }
  
  # We register a function
  extralines_register("tt_clust", treated_clust, "clust_tt")
  
  # Number of Clusters with Completed Aid
  comp_clust <- function(x) {
    unique_elements <-
      unique(sample_dhs %>% dplyr::filter(aid_complete > 0) %>% pull(DHSID))
    length(unique_elements)
  }
  
  # We register a function
  extralines_register("comp_aid", comp_clust, "aid_comp")
  
  
  
  # Number of Clusters with Educational Aid
  
  nontreated_clust <- function(x) {
    unique_elements <-
      unique(sample_dhs %>% dplyr::filter(edu > 0) %>% pull(DHSID))
    length(unique_elements)
  }
  
  # We register a function
  extralines_register("ntt_clust", nontreated_clust , "num_clust")
  
  
  # Number of Clusters with Completed Educational Aid
  
  comp_edu <- function(x) {
    unique_elements <-
      unique(sample_dhs %>% dplyr::filter(aid_complete_edu > 0) %>% pull(DHSID))
    length(unique_elements)
  }
  
  # We register a function
  extralines_register("edu_aid", comp_edu , "aid_edu")
  
  
  
  
  
  ## Total Value  ------------------------------------------------------
  
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
    , data = reg_df,
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
    file = file.path(table_dir, paste0("nosch624_edu_ndislog_", distance, "km2.tex")),
    replace = TRUE,
    extralines = list(
      "_^Number of DHS Clusters" =  ~ num_clust,
      "_^Number of DHS Clusters with Aid" =  ~ tt_clust,
      "_^Number of DHS Clusters with Educational Aid" =  ~ ntt_clust,
      "_^Number of DHS Clusters with Completed Aid" =  ~ comp_aid,
      "_^Number of DHS Clusters with Completed Educational Aid" =  ~ edu_aid
      
    )
    
  )
  
  
  
  ## IEG (disbursement) ------------------------------------------------------
  
  
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
    
    , data = reg_df,
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
    file = file.path(table_dir, paste0("nosch624_hmlieg_ndislog_", distance, "km2.tex")),
    replace = TRUE,
    extralines = list(
      "_^Number of DHS Clusters" =  ~ num_clust,
      "_^Number of DHS Clusters with Aid" =  ~ tt_clust,
      "_^Number of DHS Clusters with Educational Aid" =  ~ ntt_clust,
      "_^Number of DHS Clusters with Completed Aid" =  ~ comp_aid,
      "_^Number of DHS Clusters with Completed Educational Aid" =  ~ edu_aid
      
    )
    
  )
  
  
  ## Interactions (disbursements)--------------------------------------------------------- 
  
  
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
    , data = reg_df,
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
    file = file.path(table_dir, paste0("nosch624_intxhmlieg_ndislog_", distance, "km2.tex")),
    replace = TRUE,
    extralines = list(
      "_^Number of DHS Clusters" =  ~ num_clust,
      "_^Number of DHS Clusters with Aid" =  ~ tt_clust,
      "_^Number of DHS Clusters with Educational Aid" =  ~ ntt_clust,
      "_^Number of DHS Clusters with Completed Aid" =  ~ comp_aid,
      "_^Number of DHS Clusters with Completed Educational Aid" =  ~ edu_aid
      
    )
    
  )
  
  
  # Clean up
  rm(list = ls())
}

# Run Regressions for different buffer sizes ---------------------------------------
distances <- c(50, 45, 40, 35,30,25,20)
for (distance in distances) {
  run_analysis(distance)
}

rm(list = ls())



