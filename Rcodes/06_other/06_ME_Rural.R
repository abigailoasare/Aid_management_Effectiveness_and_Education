# Authors: Abigail O. Asare
# Date:12/03/2026
# This script is used for calculating the Marginal effects using
# Equation (5)-(7)
# See main table for the respective coefficients

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
library(xtable)

table_dir    <- "./output/descriptives/"

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
    dbxr2011_comp_edu1 = log(0.01 + dbxr2011_comp_edu),
    dbxr2011_comp_ieg_edu_hs1 = log(0.01 + dbxr2011_comp_ieg_edu_hs),
    dbxr2011_comp_ieg_edu_ms1 = log(0.01 + dbxr2011_comp_ieg_edu_ms),
    dbxr2011_comp_ieg_edu_ls1 = log(0.01 + dbxr2011_comp_ieg_edu_ls),
  ) %>%
  mutate(
    gpw_sum = gpw_ip_sum / 100000,
    shr_neversch_6_24 = shr_neversch_6_24 * 100,
    GID_2 = ifelse(is.na(GID_2), iso2code, GID_2)
  )

# Set Baseline Sample --------------------------------

reg_obs = feols(
  shr_neversch_6_24 ~  aid_complete_edu +
    conflict_5y_n + I(conflict_5y_n == 0) +
    gpw_sum + sol_mean + pre_mean + tmp_mean + spei_mean +
    av_age_mm + av_age_head + av_size_hh + shr_son_daughter
  | DHSYEAR + GID_2,
  data = reg_df
)

etable(
  reg_obs,
  cluster = ~ DHSID,
  signif.code = c("*" = .1, "**" = .05,  "***" = 0.01)
  
)


# Preliminaries ----------------------------------------------------


# Remove Obs that are not part of sample from original data set
sample_dhs <- reg_df[unlist(reg_obs$obs_selection),]

# Split Sample for Urban Clusters ----------------------------------------------
reg_rr <- sample_dhs %>%
  dplyr::filter(area_rural == 1)


reg_obs = feols(
  shr_neversch_6_24 ~  aid_complete_edu +
    conflict_5y_n + I(conflict_5y_n == 0) +
    gpw_sum + sol_mean + pre_mean + tmp_mean + spei_mean +
    av_age_mm + av_age_head + av_size_hh + shr_son_daughter
  | DHSYEAR + GID_2,
  data = reg_rr
)

etable(
  reg_obs,
  cluster = ~ DHSID,
  signif.code = c("*" = .1, "**" = .05,  "***" = 0.01)
  
)


### For regression variables ------------------------------------------------
selected_column <-
  dplyr::select(
    reg_rr,
    DHSID,
    DHSCC,
    DHSYEAR,
    DHSCLUST,
    iso3code,
    shr_neversch_6_24,
    aid_complete,
    comp_ieg_edu_hs,
    comp_ieg_edu_ms,
    comp_ieg_edu_ls,
    dm_comp_ieg_edu_hs,
    dm_comp_ieg_edu_ms,
    dm_comp_ieg_edu_ls,
    tt_disb,
    dbxr2011_comp_edu,
    dbxr2011_comp_ieg_edu_hs,
    dbxr2011_comp_ieg_edu_ms,
    dbxr2011_comp_ieg_edu_ls,
    dbxr2011_comp_edu1,
    dbxr2011_comp_ieg_edu_hs1,
    dbxr2011_comp_ieg_edu_ms1,
    dbxr2011_comp_ieg_edu_ls1
  )

column_matrix <- as.matrix(selected_column)

regression_variables <- model.matrix(reg_obs)

# Add the selected column to the model matrix
model_sum <- cbind(selected_column, regression_variables)

# 1_Indicator Marginal Effects ---------------------------------------------------


## Unique combinations and Marginal Effects ------------------------------------

combi_me_ind <- model_sum |>
  dplyr::select(dm_comp_ieg_edu_hs,
                dm_comp_ieg_edu_ms,
                dm_comp_ieg_edu_ls,
                DHSID) |>
  group_by(DHSID) |>
  dplyr::summarise(across(
    c(dm_comp_ieg_edu_hs, dm_comp_ieg_edu_ms, dm_comp_ieg_edu_ls),
    ~ sum(.x, na.rm = TRUE)
  )) |>
  dplyr::group_by(dm_comp_ieg_edu_hs, dm_comp_ieg_edu_ms, dm_comp_ieg_edu_ls) |>
  dplyr::summarise(n = n()) |>
  ungroup() |>
  dplyr::filter((dm_comp_ieg_edu_hs + dm_comp_ieg_edu_ls + dm_comp_ieg_edu_ms) >
                  0) |>
  dplyr::mutate(
    size_bin = case_when(
      n %in% 0:49 ~ "0-49",
      n %in% 50:99 ~ "50-99",
      n %in% 100:499 ~ "100-499",
      n %in% 500:999 ~ "500-999",
      n %in% 1000:1999 ~ "1000-1999",
      n >= 2000 ~ ">2000"
    ),
    name = paste0(
      dm_comp_ieg_edu_hs,
      "H",
      dm_comp_ieg_edu_ms,
      "M",
      dm_comp_ieg_edu_ls,
      "L"
    ),
    n_percent = (n / sum(n)) * 100
  ) |>
  dplyr::mutate(
    meff_hs = ifelse(
      dm_comp_ieg_edu_hs >= 1,
      0 - 3.582 * dm_comp_ieg_edu_ms - 5.844 * dm_comp_ieg_edu_ls,
      "--"
    ),
    meff_ms = ifelse(
      dm_comp_ieg_edu_ms >= 1,
      1.515 - 3.582 * dm_comp_ieg_edu_hs - 5.006 * dm_comp_ieg_edu_ls,
      "--"
    ),
    meff_ls = ifelse(
      dm_comp_ieg_edu_ls >= 1,
      9.225 - 5.844 * dm_comp_ieg_edu_hs - 5.006 * dm_comp_ieg_edu_ms,
      "--"
    )
  ) |>
  dplyr::arrange(-n_percent) |>
  dplyr::mutate(cumsum_n = cumsum(n_percent),)

# select relevant columns

combi_me_ind_rd <- combi_me_ind |>
  dplyr::select(
    dm_comp_ieg_edu_hs,
    dm_comp_ieg_edu_ms,
    dm_comp_ieg_edu_ls,
    n,
    n_percent,
    meff_hs,
    meff_ms,
    meff_ls
  ) |>
  rename(
    HS = dm_comp_ieg_edu_hs,
    MS = dm_comp_ieg_edu_ms,
    LS = dm_comp_ieg_edu_ls,
    Frequency = n,
    Percentage_Frequency = n_percent,
    HS_Marginal_Effect = meff_hs,
    MS_Marginal_Effect = meff_ms,
    LS_Marginal_Effect = meff_ls
  )

## Save----------------------------------------------------------------
me_ind_table <- xtable(combi_me_ind_rd,
                       caption = "Indicator Marginal Effects")

# Save the LaTeX table to the file
print(me_ind_table, file = file.path(table_dir, "ME_ind_rr-R1.tex"))


# 2_Counts Marginal Effects ---------------------------------------------------


## Unique combinations and Marginal Effects ------------------------------------
combi_me_counts <- model_sum |>
  dplyr::select(comp_ieg_edu_hs, comp_ieg_edu_ms, comp_ieg_edu_ls, DHSID) |>
  group_by(DHSID) |>
  dplyr::summarise(across(
    c(comp_ieg_edu_hs, comp_ieg_edu_ms, comp_ieg_edu_ls),
    ~ sum(.x, na.rm = TRUE)
  )) |>
  dplyr::group_by(comp_ieg_edu_hs, comp_ieg_edu_ms, comp_ieg_edu_ls) |>
  dplyr::summarise(n = n()) |>
  ungroup() |>
  dplyr::filter((comp_ieg_edu_hs + comp_ieg_edu_ls + comp_ieg_edu_ms) >
                  0) |>
  dplyr::mutate(
    size_bin = case_when(
      n %in% 0:49 ~ "0-49",
      n %in% 50:99 ~ "50-99",
      n %in% 100:499 ~ "100-499",
      n %in% 500:999 ~ "500-999",
      n %in% 1000:1999 ~ "1000-1999",
      n >= 2000 ~ ">2000"
    ),
    name = paste0(
      comp_ieg_edu_hs,
      "H",
      comp_ieg_edu_ms,
      "M",
      comp_ieg_edu_ls,
      "L"
    ),
    n_percent = (n / sum(n)) * 100
  ) |>
  dplyr::mutate(
    meff_hs = ifelse(
      comp_ieg_edu_hs >= 1,
      -0.3291 - 0.1640 * comp_ieg_edu_ms - 0.7466 * comp_ieg_edu_ls,
      "--"
    ),
    meff_ms = ifelse(
      comp_ieg_edu_ms >= 1,
      0.3329 - 0.1640 * comp_ieg_edu_hs - 0.3058 * comp_ieg_edu_ls,
      "--"
    ),
    meff_ls = ifelse(
      comp_ieg_edu_ls >= 1,
      4.332 - 0.7466 * comp_ieg_edu_hs - 0.3058 * comp_ieg_edu_ms,
      "--"
    )
  ) |>
  dplyr::arrange(-n_percent) |>
  dplyr::mutate(cumsum_n = cumsum(n_percent),)

# select relevant columns and rows(based of Percentage_Frequency only > 1%)
# Because we have 222 unique combination.

combi_me_counts_rd <- combi_me_counts |>
  dplyr::select(
    comp_ieg_edu_hs,
    comp_ieg_edu_ms,
    comp_ieg_edu_ls,
    n,
    n_percent,
    meff_hs,
    meff_ms,
    meff_ls
  ) |>
  dplyr::rename(
    HS = comp_ieg_edu_hs,
    MS = comp_ieg_edu_ms,
    LS = comp_ieg_edu_ls,
    Frequency = n,
    Percentage_Frequency = n_percent,
    HS_Marginal_Effect = meff_hs,
    MS_Marginal_Effect = meff_ms,
    LS_Marginal_Effect = meff_ls
  ) |>
  dplyr::filter(Percentage_Frequency > 1)

## Save----------------------------------------------------------------
me_counts_table <- xtable(combi_me_counts_rd,
                          caption = "Counts Marginal Effects")

# Save the LaTeX table to the file
print(me_counts_table, file = file.path(table_dir, "ME_counts_rr-R1.tex"))


# 3_Disbursement Marginal Effects ---------------------------------------------------


## Evaluation of Marginal Effects at the mean ------------------------------------
#It is evaluated at the mean
# mean values are reported only for non zero disbursements
# They are already in logs (see main paper for means)

# coefficients
coeff_disb <- data.frame(
  term = c("alpha1", "alpha2", "alpha3", "alpha4", "alpha5", "alpha6"),
  value = c(-0.1121, 0, 0.3612,-0.0080,-0.0151,-0.0131)  # Zero for insignificant coefficients
)

# HS Marginal Effects
calc_me_hs <- function(ms, ls, coeff) {
  coeff$value[coeff$term == "alpha1"] +
    coeff$value[coeff$term == "alpha4"] * ms +
    coeff$value[coeff$term == "alpha5"] * ls
}

me_hs <- calc_me_hs(15.355, 14.315, coeff_disb)

me_hs




# MS Marginal Effects
calc_me_ms <- function(hs, ls, coeff) {
  coeff$value[coeff$term == "alpha2"] +
    coeff$value[coeff$term == "alpha4"] * hs +
    coeff$value[coeff$term == "alpha6"] * ls
}

me_ms <- calc_me_ms(15.946, 14.315, coeff_disb)
me_ms


# LS Marginal Effects
calc_me_ls <- function(hs, ms, coeff) {
  coeff$value[coeff$term == "alpha3"] +
    coeff$value[coeff$term == "alpha5"] * hs +
    coeff$value[coeff$term == "alpha6"] * ms
}

me_ls <- calc_me_ls(15.946, 15.355, coeff_disb)
me_ls




## Save----------------------------------------------------------------

me_disb_table <- as.data.frame(cbind(me_hs, me_ms, me_ls))

me_disb_table <-  xtable(me_disb_table,
                         caption = "Disbursement Marginal Effects")

# Save the LaTeX table to the file
print(me_disb_table , file = file.path(table_dir, "ME_disb_rr-R1.tex"))


rm(list = ls())
