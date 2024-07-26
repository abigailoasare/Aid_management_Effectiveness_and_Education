library("sf")
library("tidyverse")
library("geodata")
library("terra")
library("exactextractr")
library("parallel")
library("data.table")

cores <- ifelse("Windows" %in% Sys.info(), 1, parallel::detectCores())

# Function to perform the analysis for a given distance
run_analysis <- function(distance) {
  
  # Load Data --------------------------------------------------------------------
  africa <- rnaturalearth::ne_countries(returnclass = "sf") |> 
    dplyr::filter(continent == "Africa") |> 
    sf::st_make_valid()
  
  sf::sf_use_s2(FALSE) 
  
  ## DHS Project Locations ---------------------------------------------------
  dhs <- "./data-processed/dhs_clusters.shp" |> 
    sf::st_read() |> 
    sf::st_filter(africa)|> 
    sf::st_transform(3857) # Project to web mercator
  
  
  ## Aid Locations -----------------------------------------------------------
  aid <- "./data-processed/aid_locations.shp" |> 
    sf::st_read() |> 
    sf::st_filter(africa)|> 
    sf::st_transform(3857) # Project to web mercator
  
  
  ## Aid Ratings and Project Info --------------------------------------------
  aid_ieg <- "./data-processed/rating_aid_map.csv" |> 
    readr::read_csv()
  
  aid_projects <- "./data-processed/Final_WB_Aid_Projects_bf.csv" |> 
    readr::read_csv() |>
    dplyr::mutate(
      ad_sector_codes = sapply(ad_sector_codes, function(s) str_split(s, "\\|"))
    ) |>
    dplyr::select(-latitude,-longitude ,-TEAM_LEAD,-starts_with(c("ieg_","wb_ieg_" ,ignore.case = T)))
  
  for (i in unique(unlist(aid_projects$ad_sector_codes))) {
    varname <- paste0("ad_", i)
    aid_projects <- aid_projects |> 
      dplyr::mutate(
        !!varname := sapply(ad_sector_codes, function(s) i %in% s)
      )
  }
  
  
  # Disbursement ---------------------------------------------
  # Split total disbursement according to number of projects
  
  aid_projects <- aid_projects |>
    dplyr::group_by(project_id,year) |> 
    dplyr::mutate(count_project_id=n()) |> 
    dplyr::ungroup()
  
  
  aid_projects <- aid_projects |>
    dplyr::mutate(tt_disb = total_disbursements/count_project_id)
  
  
  # Join
  aid <- aid |> 
    dplyr::left_join(aid_projects, by = c("pro_id" = "project_id",
                                          "pro_loc_id" = "project_location_id")) |> 
    dplyr::left_join(aid_ieg, by = c("pro_id" = "project_id",
                                     "pro_loc_id" = "project_location_id"))
  
  
  
  rm(aid_projects, aid_ieg)
  
  # Spatial Operations ------------------------------------------------------
  ## Build buffers --------------------------------------------------
  t0 <- Sys.time()
  buffer <- sf::st_buffer(aid, units::as_units(distance * 1000, "m"))|> 
    sf::st_transform(4326) 
  t1 <- Sys.time()
  print(t1-t0)
  
  ## Spatial Join Aid/DHS ------------------------------------------------------------
  
  dhs <- dhs |> 
    sf::st_transform(4326)
  
  t0 <- Sys.time()
  join <- sf::st_join(dhs, buffer)
  t1 <- Sys.time()
  print(t1-t0)
  
  
  ## Temporal "Join" Aid/DHS -----------------------------------------------------
  t0 <- Sys.time()
  aid_df <- join |>
    sf::st_drop_geometry() 
  t1 <- Sys.time()
  print(t1-t0)
  
  # Generate sector Specific aid projects --------------------------------------
  t0 <- Sys.time()
  aid_df <- aid_df |>
    dplyr::rowwise() |>
    dplyr::mutate(
      edu = any(ad_111, ad_112,ad_113,ad_114),
      health = any(ad_120,ad_121),
      eco_is = any(ad_210,ad_220,ad_230,ad_240),
      prod = any(ad_310,ad_311,ad_312),
      indus_mc = any(ad_321, ad_322,ad_331),
      multi = any(ad_410,ad_430)
    ) |>
    dplyr::ungroup() 
  t1 <- Sys.time()
  print(t1-t0)
  
  
  
  # Generate completed projects in the past before dhs survey date ---------------
  
  aid_df <- aid_df |>
    dplyr::mutate(
      water_ss = ad_140,
      gov_sc = ad_151,
      other_sis = ad_160,
      aid_complete = (DHSYEAR > transactions_end_year),
      aid_complete_edu = aid_complete & edu
    ) 
  
  
  # Generate completed projects ratings in the past before dhs survey date ---------------
  
  aid_df <- aid_df |>
    dplyr::mutate(
      comp_ieg_edu_1 = ifelse(aid_complete_edu & wb_ieg_edu_1, 1, 0),
      comp_ieg_edu_2 = ifelse(aid_complete_edu & wb_ieg_edu_2, 1, 0),
      comp_ieg_edu_3 = ifelse(aid_complete_edu & wb_ieg_edu_3, 1, 0),
      comp_ieg_edu_4 = ifelse(aid_complete_edu & wb_ieg_edu_4, 1, 0),
      comp_ieg_edu_5 = ifelse(aid_complete_edu & wb_ieg_edu_5, 1, 0),
      comp_ieg_edu_6 = ifelse(aid_complete_edu & wb_ieg_edu_6, 1, 0),
      comp_ieg_1 = ifelse(aid_complete & wb_ieg_code_1, 1, 0),
      comp_ieg_2 = ifelse(aid_complete & wb_ieg_code_2, 1, 0),
      comp_ieg_3 = ifelse(aid_complete & wb_ieg_code_3, 1, 0),
      comp_ieg_4 = ifelse(aid_complete & wb_ieg_code_4, 1, 0),
      comp_ieg_5 = ifelse(aid_complete & wb_ieg_code_5, 1, 0),
      comp_ieg_6 = ifelse(aid_complete & wb_ieg_code_6, 1, 0)
    ) 
  
  
  
  
  # Generate completed projects disbursement in the past before dhs survey date ---------------
  
  aid_df <- aid_df |>
    dplyr::mutate(
      dbxr2011_comp_edu = ifelse(aid_complete_edu,tt_disb,0)
    ) 
  
  
  # Generate completed projects disbursement by IEG in the past before dhs survey date ---------------
  
  aid_df <- aid_df |>
    dplyr::mutate(
      dbxr2011_comp_ieg_code_1 = ifelse(aid_complete & wb_ieg_code_1,tt_disb,0),
      dbxr2011_comp_ieg_code_2 = ifelse(aid_complete & wb_ieg_code_2,tt_disb,0),
      dbxr2011_comp_ieg_code_3 = ifelse(aid_complete & wb_ieg_code_3,tt_disb,0),
      dbxr2011_comp_ieg_code_4 = ifelse(aid_complete & wb_ieg_code_4,tt_disb,0),
      dbxr2011_comp_ieg_code_5 = ifelse(aid_complete & wb_ieg_code_5,tt_disb,0),
      dbxr2011_comp_ieg_code_6 = ifelse(aid_complete & wb_ieg_code_6,tt_disb,0),
      dbxr2011_comp_ieg_edu_1 = ifelse(aid_complete_edu & wb_ieg_edu_1, tt_disb, 0),
      dbxr2011_comp_ieg_edu_2 = ifelse(aid_complete_edu & wb_ieg_edu_2, tt_disb, 0),
      dbxr2011_comp_ieg_edu_3 = ifelse(aid_complete_edu & wb_ieg_edu_3, tt_disb, 0),
      dbxr2011_comp_ieg_edu_4 = ifelse(aid_complete_edu & wb_ieg_edu_4, tt_disb, 0),
      dbxr2011_comp_ieg_edu_5 = ifelse(aid_complete_edu & wb_ieg_edu_5, tt_disb, 0),
      dbxr2011_comp_ieg_edu_6 = ifelse(aid_complete_edu & wb_ieg_edu_6, tt_disb, 0)
    ) 
  
  # Summarized to DHS Cluster ---------------------------------------------------
  
  aid_df <- aid_df |>
    dplyr::group_by(DHSID) |>
    dplyr::summarise(
      wbAid = n_distinct(pl_id),
      edu = sum(edu, na.rm = TRUE),
      health = sum(health, na.rm = TRUE),
      water_ss = sum(water_ss, na.rm = TRUE),
      gov_sc = sum(gov_sc, na.rm = TRUE),
      other_sis = sum(other_sis, na.rm = TRUE),
      eco_is = sum(eco_is, na.rm = TRUE),
      prod = sum(prod, na.rm = TRUE),
      indus_mc = sum(indus_mc, na.rm = TRUE),
      multi = sum(multi, na.rm = TRUE),
      across(starts_with(
        c( 
          "total_disbursements",
          "tt_disb",
          "aid_complete",
          "aid_complete_",
          "comp_ieg",
          "comp_ieg_",
          "wb_ad_sector_",
          "wbAid_dur_",
          "wbAid_dur_time",
          "dbxr2011",
          "dbxr2011_",
          "wb_ieg_"
        )
      )
      ,  ~ sum(.x, na.rm = TRUE))
    ) |>
    dplyr::ungroup()  
  
  
  
  
  
  # Categories IEG Ratings into high, medium and low -----------------------------
  
  aid_df <- aid_df |> 
    dplyr::mutate(
      comp_ieg_edu_hs = comp_ieg_edu_5 + comp_ieg_edu_6,
      comp_ieg_edu_ms = comp_ieg_edu_4 + comp_ieg_edu_3,
      comp_ieg_edu_ls = comp_ieg_edu_1 + comp_ieg_edu_2,
      comp_ieg_edu_4h = comp_ieg_edu_4 + comp_ieg_edu_5 + comp_ieg_edu_6,
      comp_ieg_edu_3l = comp_ieg_edu_1 + comp_ieg_edu_2 + comp_ieg_edu_3,
      comp_ieg_hs = comp_ieg_5 + comp_ieg_6,
      comp_ieg_ms = comp_ieg_4 + comp_ieg_3,
      comp_ieg_ls = comp_ieg_1 + comp_ieg_2,
      comp_ieg_4h = comp_ieg_4 + comp_ieg_5 + comp_ieg_6,
      comp_ieg_3l = comp_ieg_1 + comp_ieg_2 + comp_ieg_3
    ) 
  
  
  # Categories Disbursement by IEG Ratings into high, medium and low -----------------------------
  aid_df <- aid_df |>
    dplyr::mutate(
      dbxr2011_comp_ieg_code_hs = dbxr2011_comp_ieg_code_5 + dbxr2011_comp_ieg_code_6,
      dbxr2011_comp_ieg_code_ms = dbxr2011_comp_ieg_code_3 + dbxr2011_comp_ieg_code_4,
      dbxr2011_comp_ieg_code_ls = dbxr2011_comp_ieg_code_1 + dbxr2011_comp_ieg_code_2,
      dbxr2011_comp_ieg_code_4h = dbxr2011_comp_ieg_code_4 + dbxr2011_comp_ieg_code_5 + dbxr2011_comp_ieg_code_6,
      dbxr2011_comp_ieg_code_3l = dbxr2011_comp_ieg_code_1 + dbxr2011_comp_ieg_code_2 + dbxr2011_comp_ieg_code_3,
      dbxr2011_comp_ieg_edu_hs = dbxr2011_comp_ieg_edu_5 + dbxr2011_comp_ieg_edu_6,
      dbxr2011_comp_ieg_edu_ms = dbxr2011_comp_ieg_edu_4 + dbxr2011_comp_ieg_edu_3,
      dbxr2011_comp_ieg_edu_ls = dbxr2011_comp_ieg_edu_1 + dbxr2011_comp_ieg_edu_2,
      dbxr2011_comp_ieg_edu_4h = dbxr2011_comp_ieg_edu_4 + dbxr2011_comp_ieg_edu_5 + dbxr2011_comp_ieg_edu_6,
      dbxr2011_comp_ieg_edu_3l = dbxr2011_comp_ieg_edu_1 + dbxr2011_comp_ieg_edu_2 + dbxr2011_comp_ieg_edu_3
    )
  
  
  aid_df[aid_df == "NaN"] <- NA
  
  
  # Merge Survey-data -------------------------------------------------------
  sum(is.na(dhs$version_GE))
  
  dhs_survey <- "./data-processed/dhs_agg.csv" |> 
    readr::read_csv() 
  sum(is.na(dhs_survey$dhs_version_pr))
  
  #no duplicates
  dups <- dhs_survey |> 
    group_by(dhs_dhscc,dhs_version_pr, dhs_clust,dhs_year) |>
    mutate(
      n= n()
    ) |>
    ungroup() |>
    filter(n>1) |>
    distinct(dhs_dhscc,dhs_version_pr, dhs_clust,dhs_year, .keep_all = T)
  
  dhs_link <-
    "./data-processed/DHS_link_PR_GEO-LK_2.xlsx" |>
    readxl::read_xlsx(sheet = "filenames") |>
    dplyr::mutate(
      iso2code = substr(filename_geodata, 1, 2),
      version_GE = substr(filename_geodata, 5, 6),
      version = substr(filename_survey_PR, 5, 6)
    ) |>
    dplyr::select(version_GE, version, iso2code) |>
    dplyr::distinct()
  sum(is.na(dhs_link$version))
  sum(is.na(dhs_link$version_GE))
  
  
  dups <- dhs_link |> 
    group_by(version_GE,version, iso2code) |>
    mutate(
      n= n()
    ) |>
    ungroup() |>
    filter(n>1) |>
    distinct(version_GE,version, iso2code, .keep_all = T)
  
  # no duplicates
  
  dhs_full <- dhs |>
    sf::st_drop_geometry() |>
    dplyr::select(DHSID, DHSCC, DHSYEAR,version_GE, DHSCLUST) |> 
    dplyr::left_join(dhs_link, by = c("DHSCC" = "iso2code", "version_GE")) |>
    dplyr::left_join(
      dhs_survey,
      by = c(
        "DHSCC" = "dhs_dhscc",
        "version" = "dhs_version_pr",
        "DHSCLUST" = "dhs_clust"
      )
    ) |> 
    dplyr::select(-version_GE, -DHSCC, -DHSCLUST, -dhs_year) 
  
  
# Check where these duplicates in DHSID are coming from
# coming from surveys that spanned more than a year (e.g 2005-06)
  
  # dups <- dhs_full |> 
  #   group_by(DHSID) |> 
  #   mutate(
  #     n= n()
  #   ) |> 
  #   ungroup() |> 
  #   select(n, DHSID, everything()) |> 
  #   filter(n>1) |> 
  #   distinct(DHSID, .keep_all = T)  
  
  
  
  ## Generate shares ----------------------------------------------------
  
  dhs_shr <- dhs_full |>
    dplyr::group_by(DHSID) |> 
    dplyr::mutate(
      shr_neversch_6_24 = num_ppl_neversch_6_24 / num_schoolage_6_24,
      shr_nosch_6_24 = num_ppl_nosch_6_24 / num_schoolage_6_24,
      shr_repeatsch_6_24 = num_ppl_repeatsch_6_24 / num_schoolage_6_24,
      shr_dropsch_6_24 = num_ppl_dropsch_6_24 / num_schoolage_6_24,
      shr_in_sch = num_ppl_in_sch / num_schoolage_6_24,
      shr_sex_headm = num_ppl_sex_headm / num_ppl,
      shr_sex_mem = num_ppl_sex_mem / num_ppl,
      shr_son_daughter = num_ppl_son_daughter / num_ppl,
      area_rural = first(area_rural, na_rm = TRUE),
      av_age_mm = mean(av_age_mm, na.rm = TRUE),
      av_age_head = mean(av_age_head, na.rm = TRUE),
      av_size_hh = mean(av_size_hh, na.rm = TRUE),
      av_yr_edu = mean(av_yr_edu, na.rm = TRUE),
      av_ll_edu = mean(av_ll_edu, na.rm = TRUE)
    ) |>
    dplyr::ungroup() 
  
  
  rm(dhs_full)
  
  dhs_shr[dhs_shr == "NaN"] <- NA
  dhs_shr[dhs_shr == "Inf"] <- NA
  
  # Load Raster data -------------------------------------------------------------
  
  # Read the gzipped CSV file
  raster_df <- paste0("./data-r4r/data_raster_", distance, "km.csv.gz")
  raster_df <- fread(raster_df)
  
  # Replace conflict NAs with zero
  
  raster_df <- raster_df |>
    mutate(across(starts_with("conflict"), ~replace_na(., 0))) |> 
    dplyr::select(-gpw_sum,-gpw_mean)
  
  
  # Join Data Sets  --------------------------------------------------------------
  
  
  full_df <- raster_df |>
    dplyr::left_join(aid_df, by = "DHSID") 
  
  
  full_df <- full_df|>
    dplyr::left_join(dhs_shr, by = c("DHSID", "DHSYEAR")) |> 
    sf::st_drop_geometry() 
  
  # Save data -------------------------------------------------------
  
  full_df[full_df == "NaN"] <- NA
  
  full_df[full_df == "Inf"] <- NA
  
  full_df <- full_df |>
    readr::write_csv(paste0("./data-r4r/data_", distance, "km.csv.gz"))

  # Generate variables ------------------------------------------------------------------------------

  full_df <- full_df |>
        dplyr::mutate(
          dm_comp_aid_edu = ifelse(aid_complete_edu > 0, 1, 0),
          dm_comp_ieg_edu_hs = ifelse(comp_ieg_edu_hs > 0, 1, 0),
          dm_comp_ieg_edu_ms = ifelse(comp_ieg_edu_ms > 0, 1, 0),
          dm_comp_ieg_edu_ls = ifelse(comp_ieg_edu_ls > 0, 1, 0),
          dbxr2011_comp_edu1 = log(0.01+dbxr2011_comp_edu),
          dbxr2011_comp_ieg_edu_hs1 = log(0.01+dbxr2011_comp_ieg_edu_hs),
          dbxr2011_comp_ieg_edu_ms1 = log(0.01+dbxr2011_comp_ieg_edu_ms),
          dbxr2011_comp_ieg_edu_ls1 = log(0.01+dbxr2011_comp_ieg_edu_ls),
        ) |>
        mutate(gpw_sum=gpw_ip_sum/100000, 
         shr_neversch_6_24=shr_neversch_6_24*100,
         GID_2=ifelse(is.na(GID_2),iso2code,GID_2)
              )
  
  
  rm(list = ls())
}

# Run analysis for different distances ---------------------------------------------------------------------
distances <- c(50, 45, 40, 35,30,25,20)
for (distance in distances) {
  message("Running analysis for distance: ", distance)
  tt0 <- Sys.time()
  run_analysis(distance)
  tt1 <- Sys.time()
  message("Finished analysis for distance: ", distance, " in ", tt1 - tt0)
}

rm(list = ls())




