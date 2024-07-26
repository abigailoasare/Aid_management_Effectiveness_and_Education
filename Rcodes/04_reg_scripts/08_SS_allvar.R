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
library("stargazer")

table_dir    <- "./output/descriptives/"

# Read data --------------------------------------------------------------------


# Read the gzipped CSV file
reg_df <- fread("./data-r4r/data_50km.csv.gz")


# Set Estimation Sample --------------------------------

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





## Summary Statistic Regression Sample --------------------------------

# Remove Obs that are not part of sample from original data set
sample_dhs <- reg_df[unlist(reg_obs$obs_selection), ]

stargazer(
  sample_dhs,
  summary = T,
  type = "latex" ,
  out = file.path(table_dir, "SS_reg_sample.tex")
)

# Save regression Sample ---------------------------------
sample_dhs <- sample_dhs |>
  readr::write_csv(paste0("./data-r4r/reg_data_", distance, "km.csv.gz"))




### For regression variables ------------------------------------------------
selected_column <- dplyr::select(sample_dhs, shr_neversch_6_24,aid_complete,
                                 comp_ieg_edu_hs,comp_ieg_edu_ms,comp_ieg_edu_ls,
                                 dm_comp_ieg_edu_hs,dm_comp_ieg_edu_ms,
                                 dm_comp_ieg_edu_ls,tt_disb,dbxr2011_comp_edu,
                                 dbxr2011_comp_ieg_edu_hs,dbxr2011_comp_ieg_edu_ms,
                                 dbxr2011_comp_ieg_edu_ls,dbxr2011_comp_edu1,
                                 dbxr2011_comp_ieg_edu_hs1,dbxr2011_comp_ieg_edu_ms1,
                                 dbxr2011_comp_ieg_edu_ls1)

column_matrix <- as.matrix(selected_column)

regression_variables <- model.matrix(reg_obs)

# Add the selected column to the model matrix
model_sum <- cbind(selected_column, regression_variables)

stargazer(
  model_sum,
  summary = T,
  type = "latex",
  out = file.path(table_dir, "SS_reg_var.tex")
)


# Select only non Zero disbursement ------------------------------------------

# education disbursement logged
no_Zdisb_edu1 <- model_sum %>% 
  filter(dbxr2011_comp_edu1>0) %>% 
  dplyr::select(dbxr2011_comp_edu1)

stargazer(
  no_Zdisb_edu1,
  summary = T,
  type = "latex",
  out = file.path(table_dir, "SS_noZdisb_edu1_reg_var.tex")
)


# High disbursement logged
no_Zdisb_hs1 <- model_sum %>% 
  filter(dbxr2011_comp_ieg_edu_hs1>0) %>% 
  dplyr::select(dbxr2011_comp_ieg_edu_hs1)

stargazer(
  no_Zdisb_hs1,
  summary = T,
  type = "latex",
  out = file.path(table_dir, "SS_noZdisb_hs1_reg_var.tex")
)

# Medium disbursement logged
no_Zdisb_ms1 <- model_sum %>% 
  filter(dbxr2011_comp_ieg_edu_ms1>0) %>% 
  dplyr::select(dbxr2011_comp_ieg_edu_ms1)

stargazer(
  no_Zdisb_ms1,
  summary = T,
  type = "latex",
  out = file.path(table_dir, "SS_noZdisb_ms1_reg_var.tex")
)

# low disbursement logged
no_Zdisb_ls1 <- model_sum %>% 
  filter(dbxr2011_comp_ieg_edu_ls1>0) %>% 
  dplyr::select(dbxr2011_comp_ieg_edu_ls1)

stargazer(
  no_Zdisb_ls1,
  summary = T,
  type = "latex",
  out = file.path(table_dir, "SS_noZdisb_ls1_reg_var.tex")
)

rm(list = ls())






