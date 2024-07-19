library("sf")
library("tidyverse")
library("geodata")
library("terra")
library("exactextractr")
library("parallel")
library("data.table")

cores <- ifelse("Windows" %in% Sys.info(), 1, parallel::detectCores())
# Load Data --------------------------------------------------------------------
africa <- rnaturalearth::ne_countries(returnclass = "sf") |> 
  dplyr::filter(continent == "Africa") |> 
  sf::st_make_valid()

sf::sf_use_s2(FALSE) 

## DHS Project Locations ---------------------------------------------------
dhs <- "../DO/GRID_025/1_DATA_PROCESSING/2_DHS_SURVEY/PR/output/dhs_clusters.shp" |> 
  sf::st_read() |> 
  sf::st_filter(africa)|> 
  sf::st_transform(3857) # Project to web mercator


## Aid Locations -----------------------------------------------------------
aid <- "../DO/GRID_025/1_DATA_PROCESSING/1_AidData/output/aid_locations.shp" |> 
  sf::st_read() |> 
  sf::st_filter(africa)|> 
  sf::st_transform(3857) # Project to web mercator


## Aid Ratings and Project Info --------------------------------------------
aid_ieg <- "../DO/GRID_025/1_DATA_PROCESSING/1_AidData/output/rating_aid_map.csv" |> 
  readr::read_csv()

aid_projects <- "../DO/GRID_025/1_DATA_PROCESSING/1_AidData/output/Final_WB_Aid_Projects_bf.csv" |> 
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


# test <- aid_projects |> 
#   select(project_id,project_location_id,datestart,total_disbursements) |> 
#   filter(datestart>=2013)

aid_projects <- aid_projects |>
  dplyr::group_by(project_id,year) |> 
  dplyr::mutate(count_project_id=n()) |> 
  dplyr::ungroup()


aid_projects <- aid_projects |>
  dplyr::mutate(tt_disb = total_disbursements/count_project_id)

# test <- aid_projects |> 
#   select(project_id,project_location_id,datestart,count_project_id,
#          total_disbursements,even_split_disbursements,tt_disb)




# Join
aid <- aid |> 
  dplyr::left_join(aid_projects, by = c("pro_id" = "project_id",
                                        "pro_loc_id" = "project_location_id")) |> 
  dplyr::left_join(aid_ieg, by = c("pro_id" = "project_id",
                                   "pro_loc_id" = "project_location_id"))


# aid |> 
#   sf::st_drop_geometry() |> 
#   group_by(ieg_outcome_code) |> 
#   summarise(disb = sum(total_disbursements, na.rm=TRUE),
#             mean_disb = mean(total_disbursements, na.rm=TRUE),
#             n = n())

# 
# aid |> 
#   st_drop_geometry() |> 
#   filter(is.na(ieg_outcome_code)) |> 
#   group_by(transactions_end_year) |> 
#   summarise(n = n())
# #Check why rating is missing for obs before ~2020
# # truely missing IEG score (941)
# 
# tt <- aid |> 
#   st_drop_geometry() |> 
#   filter(is.na(ieg_outcome_code)) |> 
#   dplyr::select(pro_id) |> 
#   distinct()


rm(aid_projects, aid_ieg)

# Spatial Operations ------------------------------------------------------
## Build buffers --------------------------------------------------
t0 <- Sys.time()
buffer <- sf::st_buffer(aid, units::as_units(50000, "m"))|> 
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

# Using `>=` for selecting complete aid. (give ~14k additional obs)
aid_df <- aid_df |>
  dplyr::mutate(
    water_ss = ad_140,
    gov_sc = ad_151,
    other_sis = ad_160,
    aid_complete = (DHSYEAR > transactions_end_year),
    aid_complete_edu = aid_complete & edu
  ) 

# test <- aid_df |>
#   dplyr::select(DHSYEAR,transactions_end_year,edu,aid_complete,aid_complete_edu)|>
#   filter(aid_complete==TRUE)


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
# test <- aid_df |>
#   dplyr::select(DHSYEAR,transactions_end_year,edu,aid_complete,aid_complete_edu,starts_with("dbxr2011"))|>
#   filter(aid_complete==TRUE)


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


# test <- aid_df |>
#   dplyr::select(DHSYEAR,transactions_end_year,edu,aid_complete,tt_disb,
#                 total_disbursements,aid_complete_edu,
#                 starts_with("dbxr2011_comp_"))|>
#   filter(aid_complete==TRUE)

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

### View Map interactive -------------------------------------------------

# x <- aid_df2 |> 
#   dplyr::filter(aid_complete > 50) |> 
#   pull(DHSID)
# leaflet::leaflet() |> 
#   leaflet::addTiles() |> 
#   leaflet::addCircleMarkers(data = dhs |> dplyr::filter(DHSID %in% x ) |> sf::st_transform(4326),
#                             popup = ~DHSYEAR, clusterOptions = leaflet::markerClusterOptions()) |> 
#   leaflet::addScaleBar()




#  Generate disbursement Variables for Main Sector Codings ----------------

aid_df <- aid_df |> 
  dplyr::rowwise() |> 
  dplyr::mutate(
    dbxr2011_edu = dbxr2011_111 + dbxr2011_112 + dbxr2011_113 + dbxr2011_114,
    dbxr2011_health = dbxr2011_120+ dbxr2011_121,
    dbxr2011_waterss = dbxr2011_140,
    dbxr2011_govsc = dbxr2011_151,
    dbxr2011_othersis = dbxr2011_160,
    dbxr2011_ecois = dbxr2011_210+dbxr2011_220+dbxr2011_230+dbxr2011_240,
    dbxr2011_prod = dbxr2011_310+dbxr2011_311+ dbxr2011_312,
    dbxr2011_indusmc =dbxr2011_321+ dbxr2011_322+dbxr2011_331,
    dbxr2011_multi= dbxr2011_410+dbxr2011_430
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


# test <- aid_df |>
#   dplyr::select(edu,aid_complete_edu,tt_disb,
#                 total_disbursements,comp_ieg_edu_hs,comp_ieg_edu_ms,comp_ieg_edu_ls,
#                 starts_with("dbxr2011_comp_"))|>
#   filter(aid_complete_edu==TRUE) %>% 
#   filter(comp_ieg_edu_ls>0) 

# Merge Survey-data -------------------------------------------------------
sum(is.na(dhs$version_GE))

dhs_survey <- "../DO/GRID_025/1_DATA_PROCESSING/2_DHS_SURVEY/PR/output/dhs_agg.csv" |> 
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
  "../DO/GRID_025/1_DATA_PROCESSING/2_DHS_SURVEY/PR/output/DHS_link_PR_GEO-LK_2.xlsx" |>
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
  dplyr::select(-version_GE, -DHSCC, -DHSCLUST, -version, -dhs_year) 


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
    shr_nosch_6_24 = num_ppl_nosch_6_24 / num_schoolage_6_24,
    shr_in_sch = num_ppl_in_sch / num_schoolage_6_24,
    shr_agric_land = num_ppl_agric_land / num_ppl,
    shr_livestock = num_ppl_livestock / num_ppl,
    shr_electric = num_ppl_electric / num_ppl,
    shr_radio = num_ppl_radio / num_ppl,
    shr_tv = num_ppl_tv / num_ppl,
    shr_fridge = num_ppl_fridge / num_ppl,
    shr_bike = num_ppl_bike / num_ppl,
    shr_phone = num_ppl_phone / num_ppl,
    shr_car_truck = num_ppl_car_truck / num_ppl,
    shr_motor_scooter = num_ppl_motor_scooter / num_ppl,
    shr_pipe_water = num_ppl_pipe_water / num_ppl,
    shr_ff_roof = num_ppl_ff_roof / num_ppl,
    shr_ff_wall = num_ppl_ff_wall / num_ppl,
    shr_ff_floor = num_ppl_ff_floor / num_ppl,
    shr_toilet = num_ppl_toilet / num_ppl,
    shr_bank_access = num_ppl_bank_access / num_ppl,
    shr_sex_headm = num_ppl_sex_headm / num_ppl,
    shr_sex_mem = num_ppl_sex_mem / num_ppl,
    shr_son_daughter = num_ppl_son_daughter / num_ppl,
    area_rural = first(area_rural, na_rm = TRUE),
    shr_adopted_foster = num_ppl_adopted_foster / num_ppl,
    shr_grandchild = num_ppl_grandchild / num_ppl,
    shr_father_alive = num_ppl_father_alive / num_ppl,
    shr_mother_alive = num_ppl_mother_alive / num_ppl,
    shr_sleep_net = num_ppl_sleep_net / num_ppl,
    av_edu_years_25 = mean(av_edu_years_25, na.rm = TRUE),
    av_yrsch_24 = mean(av_yearsofeduc_24, na.rm = TRUE),
    av_age_mm = mean(av_age_mm, na.rm = TRUE),
    av_age_head = mean(av_age_head, na.rm = TRUE),
    av_size_hh = mean(av_size_hh, na.rm = TRUE),
    av_yr_edu = mean(av_yr_edu, na.rm = TRUE),
    av_ll_edu = mean(av_ll_edu, na.rm = TRUE),
    av_water_time = mean(av_water_time, na.rm = TRUE)
  ) |>
  dplyr::ungroup() 


rm(dhs_full)

dhs_shr[dhs_shr == "NaN"] <- NA
dhs_shr[dhs_shr == "Inf"] <- NA

# Load Raster data -------------------------------------------------------------

# Read the gzipped CSV file
raster_df <- fread("./data-r4r/data_raster_50km.csv.gz")

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
  readr::write_csv("./data-r4r/data_50km.csv.gz")
#beepr::beep(3)



rm(list = ls())

# Labels

# expss::apply_labels(
#   shr_nosch_6_24 = "Share of HH 6--24yrs with no School",
#   shr_in_sch = "Share of HH in School",
#   shr_livestock = "Share of HH with livestock",
#   shr_electric = "Share of HH with electricity",
#   shr_radio = "Share of HH with radio",
#   shr_tv = "Share of HH with TV",
#   shr_fridge = "Share of HH with fridge",
#   shr_bike = "Share of HH with bike",
#   shr_phone = "Share of HH with phone",
#   shr_car_truck = "Share of HH with car/truck",
#   shr_motor_scooter = "Share of HH with motor scooter",
#   shr_pipe_water = "Share of HH with pipe water",
#   shr_ff_roof = "Share of HH with finished roof",
#   shr_ff_wall = "Share of HH with finished wall",
#   shr_ff_floor = "Share of HH with finished floor",
#   shr_toilet = "Share of HH with toilet",
#   shr_bank_access = "Share of HH with back account",
#   shr_sex_headm = "Share of Households with Male Head",
#   shr_sex_mem = "Share of Households with Male members",
#   shr_son_daughter = "Share of HH with livestock",
#   shr_area_rural = "Share of HH living in rural area",
#   shr_adopted_foster = "Share of HH with adopted foster children",
#   shr_grandchild = "Share of HH with grandchild",
#   shr_father_alive = "Share of HH whose father is alive",
#   shr_mother_alive = "Share of HH whose mother is alive",
#   av_edu_years_25 = "Average years of education 25yrs and above",
#   av_age_head = "Average age of HH head",
#   av_age_mm = "Average age of HH",
#   av_size_hh = "Average Household Size",
#   av_yr_edu = "Average Years of Schooling",
#   av_ll_edu = "Average level of Education",
#   av_water_time = "Average time spent to fetch water"
# ) 




