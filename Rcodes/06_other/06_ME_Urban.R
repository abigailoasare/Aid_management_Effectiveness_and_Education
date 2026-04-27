# Authors: Abigail O. Asare
# Date:27/04/2026 
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

reg_df <- reg_df |>
  dplyr::mutate(
    dm_comp_aid_edu = ifelse(aid_complete_edu > 0, 1, 0),
    dm_comp_ieg_edu_hs = ifelse(comp_ieg_edu_hs > 0, 1, 0),
    dm_comp_ieg_edu_ms = ifelse(comp_ieg_edu_ms > 0, 1, 0),
    dm_comp_ieg_edu_ls = ifelse(comp_ieg_edu_ls > 0, 1, 0),
    dbxr2011_comp_edu1 = log(0.01 + dbxr2011_comp_edu),
    dbxr2011_comp_ieg_edu_hs1 = log(0.01 + dbxr2011_comp_ieg_edu_hs),
    dbxr2011_comp_ieg_edu_ms1 = log(0.01 + dbxr2011_comp_ieg_edu_ms),
    dbxr2011_comp_ieg_edu_ls1 = log(0.01 + dbxr2011_comp_ieg_edu_ls),
  ) |>
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
reg_ur <- sample_dhs |>
  dplyr::filter(area_rural == 0)


reg_obs = feols(
  shr_neversch_6_24 ~  aid_complete_edu +
    conflict_5y_n + I(conflict_5y_n == 0) +
    gpw_sum + sol_mean + pre_mean + tmp_mean + spei_mean +
    av_age_mm + av_age_head + av_size_hh + shr_son_daughter
  | DHSYEAR + GID_2,
  data = reg_ur
)

etable(
  reg_obs,
  cluster = ~ DHSID,
  signif.code = c("*" = .1, "**" = .05,  "***" = 0.01)
  
)


### For regression variables ------------------------------------------------
selected_column <-
  dplyr::select(
    reg_ur,
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



# Indicator Marginal Effects ---------------------------------------------------
reg_ind = feols(
  shr_neversch_6_24 ~ dm_comp_ieg_edu_hs*dm_comp_ieg_edu_ms +
    dm_comp_ieg_edu_hs*dm_comp_ieg_edu_ls +
    dm_comp_ieg_edu_ms*dm_comp_ieg_edu_ls +
    conflict_5y_n + I(conflict_5y_n == 0) +
    gpw_sum + sol_mean + pre_mean + tmp_mean + spei_mean +
    av_age_mm + av_age_head + av_size_hh + shr_son_daughter 
  | DHSYEAR +
    GID_2, 
  data = reg_ur,
  subset = obs(reg_obs)
)

etable(
  reg_ind,
  cluster = ~ DHSID,
  signif.code = c("*" = .1, "**" = .05,  "***" = 0.01)
)



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
  )


## 1)  clustered coefs + p-values, zero-out insignificant ones ---------
alpha <- 0.10

ct <- summary(reg_ind, cluster = ~ DHSID)$coeftable
# ct is a matrix with columns: Estimate, Std. Error, t value, Pr(>|t|)

coefs_sig <- setNames(
  ifelse(ct[, "Pr(>|t|)"] < alpha, ct[, "Estimate"], 0),
  rownames(ct)
)

# small helper to safely fetch a coef by name, default 0 if absent
getc <- function(nm) if (nm %in% names(coefs_sig)) coefs_sig[[nm]] else 0

## 2) Mapping specific coefficients -----------------------------------
b_hs    <- getc("dm_comp_ieg_edu_hs")
b_ms    <- getc("dm_comp_ieg_edu_ms")
b_ls    <- getc("dm_comp_ieg_edu_ls")

# interaction names in fixest use ":"; order is usually the same
b_hs_ms <- getc("dm_comp_ieg_edu_hs:dm_comp_ieg_edu_ms"); if (b_hs_ms == 0) b_hs_ms <- getc("dm_comp_ieg_edu_ms:dm_comp_ieg_edu_hs")
b_hs_ls <- getc("dm_comp_ieg_edu_hs:dm_comp_ieg_edu_ls"); if (b_hs_ls == 0) b_hs_ls <- getc("dm_comp_ieg_edu_ls:dm_comp_ieg_edu_hs")
b_ms_ls <- getc("dm_comp_ieg_edu_ms:dm_comp_ieg_edu_ls"); if (b_ms_ls == 0) b_ms_ls <- getc("dm_comp_ieg_edu_ls:dm_comp_ieg_edu_ms")

## 3) Add marginal effects to comb table --------------------------------

combi_me_ind <- combi_me_ind |>
  mutate(
    meff_hs = if_else(
      dm_comp_ieg_edu_hs >= 1,
      # ME of increasing HS, holding others at their combo values:
      b_hs + (b_hs_ms * dm_comp_ieg_edu_ms) + (b_hs_ls * dm_comp_ieg_edu_ls),
      NA_real_
    ),
    meff_ms = if_else(
      dm_comp_ieg_edu_ms >= 1,
      b_ms + (b_hs_ms * dm_comp_ieg_edu_hs) + (b_ms_ls * dm_comp_ieg_edu_ls),
      NA_real_
    ),
    meff_ls = if_else(
      dm_comp_ieg_edu_ls >= 1,
      b_ls + (b_hs_ls * dm_comp_ieg_edu_hs) + (b_ms_ls * dm_comp_ieg_edu_ms),
      NA_real_
    )
  )
# "--" instead of NA, coerce to character:
combi_me_ind <- combi_me_ind |>
  dplyr::mutate(
    across(c(meff_hs, meff_ms, meff_ls),
           ~ ifelse(is.na(.x), "--", formatC(.x, digits = 2, format = "f")))
  )|>
  dplyr::arrange(-n_percent) |>
  dplyr::mutate(cumsum_n = cumsum(n_percent))


# select relevant columns

combi_me_ind_rd <- combi_me_ind |>
  dplyr::mutate(n_percent= round(n_percent, 2)) |> 
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
print(me_ind_table, file = file.path(table_dir, "ME_ind_ur.tex"))


# 2_Counts Marginal Effects ---------------------------------------------------

reg_counts = feols(
  shr_neversch_6_24 ~ comp_ieg_edu_hs*comp_ieg_edu_ms +
    comp_ieg_edu_hs*comp_ieg_edu_ls +
    comp_ieg_edu_ms*comp_ieg_edu_ls +
    conflict_5y_n + I(conflict_5y_n == 0) +
    gpw_sum + sol_mean + pre_mean + tmp_mean + spei_mean +
    av_age_mm + av_age_head + av_size_hh + shr_son_daughter 
  | DHSYEAR +
    GID_2, 
  data = reg_ur,
  subset = obs(reg_obs)
)

etable(
  reg_counts,
  cluster = ~ DHSID,
  signif.code = c("*" = .1, "**" = .05,  "***" = 0.01)
)



## Unique combinations and Marginal Effects ------------------------------------

combi_me_counts <- model_sum |>
  dplyr::select(comp_ieg_edu_hs,
                comp_ieg_edu_ms,
                comp_ieg_edu_ls,
                DHSID) |>
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
  )


## 1)  clustered coefs + p-values, zero-out insignificant ones ---------
alpha <- 0.10

ct <- summary(reg_counts, cluster = ~ DHSID)$coeftable
# ct is a matrix with columns: Estimate, Std. Error, t value, Pr(>|t|)

coefs_sig <- setNames(
  ifelse(ct[, "Pr(>|t|)"] < alpha, ct[, "Estimate"], 0),
  rownames(ct)
)

# small helper to safely fetch a coef by name, default 0 if absent
getc <- function(nm) if (nm %in% names(coefs_sig)) coefs_sig[[nm]] else 0

## 2) Mapping specific coefficients -----------------------------------
b_hs    <- getc("comp_ieg_edu_hs")
b_ms    <- getc("comp_ieg_edu_ms")
b_ls    <- getc("comp_ieg_edu_ls")

# interaction names in fixest use ":"; order is usually the same
b_hs_ms <- getc("comp_ieg_edu_hs:comp_ieg_edu_ms"); if (b_hs_ms == 0) b_hs_ms <- getc("comp_ieg_edu_ms:comp_ieg_edu_hs")
b_hs_ls <- getc("comp_ieg_edu_hs:comp_ieg_edu_ls"); if (b_hs_ls == 0) b_hs_ls <- getc("comp_ieg_edu_ls:comp_ieg_edu_hs")
b_ms_ls <- getc("comp_ieg_edu_ms:comp_ieg_edu_ls"); if (b_ms_ls == 0) b_ms_ls <- getc("comp_ieg_edu_ls:comp_ieg_edu_ms")

## 3) Add marginal effects to comb table --------------------------------

combi_me_counts <- combi_me_counts |>
  mutate(
    meff_hs = if_else(
      comp_ieg_edu_hs >= 1,
      # ME of increasing HS, holding others at their combo values:
      b_hs + (b_hs_ms * comp_ieg_edu_ms) + (b_hs_ls * comp_ieg_edu_ls),
      NA_real_
    ),
    meff_ms = if_else(
      comp_ieg_edu_ms >= 1,
      b_ms + (b_hs_ms * comp_ieg_edu_hs) + (b_ms_ls * comp_ieg_edu_ls),
      NA_real_
    ),
    meff_ls = if_else(
      comp_ieg_edu_ls >= 1,
      b_ls + (b_hs_ls * comp_ieg_edu_hs) + (b_ms_ls * comp_ieg_edu_ms),
      NA_real_
    )
  )
# "--" instead of NA, coerce to character:
combi_me_counts <- combi_me_counts |>
  dplyr::mutate(
    across(c(meff_hs, meff_ms, meff_ls),
           ~ ifelse(is.na(.x), "--", formatC(.x, digits = 2, format = "f")))
  )|>
  dplyr::arrange(-n_percent) |>
  dplyr::mutate(cumsum_n = cumsum(n_percent))


# select relevant columns

combi_me_counts_rd <- combi_me_counts |>
  dplyr::mutate(n_percent= round(n_percent, 2)) |> 
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
  rename(
    HS = comp_ieg_edu_hs,
    MS = comp_ieg_edu_ms,
    LS = comp_ieg_edu_ls,
    Frequency = n,
    Percentage_Frequency = n_percent,
    HS_Marginal_Effect = meff_hs,
    MS_Marginal_Effect = meff_ms,
    LS_Marginal_Effect = meff_ls
  )|>
  dplyr::filter(Percentage_Frequency > 1)

## Save----------------------------------------------------------------
me_counts_table <- xtable(combi_me_counts_rd,
                          caption = "Counts Marginal Effects")

# Save the LaTeX table to the file
print(me_counts_table, file = file.path(table_dir, "ME_counts_ur.tex"))


# 3_Disbursement Marginal Effects ---------------------------------------------------

reg_db = feols(
  shr_neversch_6_24 ~ dbxr2011_comp_ieg_edu_hs1*dbxr2011_comp_ieg_edu_ms1 +
    dbxr2011_comp_ieg_edu_hs1*dbxr2011_comp_ieg_edu_ls1 +
    dbxr2011_comp_ieg_edu_ms1*dbxr2011_comp_ieg_edu_ls1 +
    conflict_5y_n + I(conflict_5y_n == 0) +
    gpw_sum + sol_mean + pre_mean + tmp_mean + spei_mean +
    av_age_mm + av_age_head + av_size_hh + shr_son_daughter 
  | DHSYEAR +
    GID_2, 
  data = reg_ur,
  subset = obs(reg_obs)
)


etable(
  reg_db,
  cluster = ~ DHSID,
  signif.code = c("*" = .1, "**" = .05,  "***" = 0.01)
)

## 1)  clustered coefs + p-values, zero-out insignificant ones ---------
alpha <- 0.10

ct <- summary(reg_db, cluster = ~ DHSID)$coeftable
# ct is a matrix with columns: Estimate, Std. Error, t value, Pr(>|t|)

coefs_sig <- setNames(
  ifelse(ct[, "Pr(>|t|)"] < alpha, ct[, "Estimate"], 0),
  rownames(ct)
)

#fetch a coef by name, default 0 if absent
getc <- function(nm) if (nm %in% names(coefs_sig)) coefs_sig[[nm]] else 0

## 2) Mapping specific coefficients -----------------------------------
b_hs    <- getc("dbxr2011_comp_ieg_edu_hs1")
b_ms    <- getc("dbxr2011_comp_ieg_edu_ms1")
b_ls    <- getc("dbxr2011_comp_ieg_edu_ls1")

# interaction names in fixest use ":"; order is usually the same, but be safe:
b_hs_ms <- getc("dbxr2011_comp_ieg_edu_hs1:dbxr2011_comp_ieg_edu_ms1"); if (b_hs_ms == 0) b_hs_ms <- getc("dbxr2011_comp_ieg_edu_ms1:dbxr2011_comp_ieg_edu_hs1")
b_hs_ls <- getc("dbxr2011_comp_ieg_edu_hs1:dbxr2011_comp_ieg_edu_ls1"); if (b_hs_ls == 0) b_hs_ls <- getc("dbxr2011_comp_ieg_edu_ls1:dbxr2011_comp_ieg_edu_hs1")
b_ms_ls <- getc("dbxr2011_comp_ieg_edu_ms1:dbxr2011_comp_ieg_edu_ls1"); if (b_ms_ls == 0) b_ms_ls <- getc("dbxr2011_comp_ieg_edu_ls1:dbxr2011_comp_ieg_edu_ms1")


## Evaluation of Marginal Effects at the mean ------------------------------------
# Means from statitics table 1
# HS Marginal Effects
calc_me_hs <- function(ms, ls, coeff) {
  b_hs +
    b_hs_ms * ms +
    b_hs_ls * ls
}

me_hs <- calc_me_hs(15.355, 14.315, coeff_disb)

me_hs




# MS Marginal Effects
calc_me_ms <- function(hs, ls, coeff) {
  b_ms +
    b_hs_ms* hs +
    b_ms_ls * ls
}

me_ms <- calc_me_ms(15.946, 14.315, coeff_disb)
me_ms


# LS Marginal Effects
calc_me_ls <- function(hs, ms, coeff) {
  b_ls +
    b_hs_ls * hs +
    b_ms_ls* ms
}

me_ls <- calc_me_ls(15.946, 15.355, coeff_disb)
me_ls




## Save----------------------------------------------------------------

me_disb_table <- as.data.frame(cbind(me_hs, me_ms, me_ls))

me_disb_table <-  xtable(me_disb_table,
                         caption = "Disbursement Marginal Effects")

# Save the LaTeX table to the file
print(me_disb_table , file = file.path(table_dir, "ME_disb_ur.tex"))


rm(list = ls())

