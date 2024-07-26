# Load packages ----------------------------------------------------------------
library("sf")
library("tidyverse")
library("geodata")
library("terra")
library("exactextractr")
library("parallel")

# Cores for parallel processing
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
    sf::st_filter(africa)  |> 
    sf::st_transform(3857) # Project to web mercator
  
  # Define Buffer
  buffer_dhs <- dhs |> 
    st_buffer(units::as_units(distance* 1000, "m")) |> 
    dplyr::mutate(
      buffer_id = row_number()
    ) |> 
    sf::st_transform(4326) 
  
  dhs <- dhs |> 
    sf::st_transform(4326)
  
  ## Temperature -------------------------------------------------------------
  tmp_raster <- "./data-raw/ceda/cru_ts4.06.1901.2021.tmp.dat.nc" |>
    terra::rast(subds = "tmp")

  tmp_raster <- tmp_raster |>
    subset(time(tmp_raster) >= as.Date("1990-01-01")) |>
    terra::tapp("years", mean)

  tmp_df <- exactextractr::exact_extract(
    tmp_raster,
    buffer_dhs,
    fun = c("mean", "sum"),
    append_cols = "DHSID",
    colname_fun = function(values, fun_name, ...)
      paste("tmp", fun_name, values, sep = "_")
  )

  ### Long reshape ----------------------------


  tmp_mean <- tmp_df  |>
    tidyr::pivot_longer(cols=starts_with("tmp_mean"),
                        names_to = "long_name1",
                        values_to = "tmp_mean")|>
    dplyr::mutate(year=as.numeric(str_sub(long_name1, -4,-1))) |>
    dplyr::select(DHSID,year,tmp_mean)

  tmp_sum <- tmp_df  |>
    tidyr::pivot_longer(cols=starts_with("tmp_sum"),
                        names_to = "long_name2",
                        values_to =  "tmp_sum")|>
    dplyr::mutate(year=as.numeric(str_sub(long_name2, -4,-1))) |>
    dplyr::select(DHSID,year,tmp_sum)

  tmp_df <- tmp_mean |>
    left_join(tmp_sum)


  ### Temporal Join -----------------------------------------------
  tmp_full_df <- dhs |>
    st_drop_geometry() |>
    right_join(tmp_df, by = c("DHSID", "DHSYEAR" = "year")) |>
    arrange(DHSID, DHSYEAR)



  tmp_full_df  <- tmp_full_df  |>
    dplyr::select(DHSID, DHSYEAR, starts_with ("tmp"))




  rm(tmp_raster, tmp_sum,tmp_mean, tmp_df)

  # Export intermediate data set
  tmp_full_df |>
    readr::write_csv(paste0("./data-intermediate/tmp_full_df_", distance, "km.csv.gz"))
  rm(tmp_full_df)




  ## Precipitation -----------------------------------------------------------
  pre_raster <- "./data-raw/ceda/cru_ts4.06.1901.2021.pre.dat.nc" |>
    terra::rast(subds = "pre")

  pre_raster <- pre_raster |>
    subset(time(pre_raster) >= as.Date("1990-01-01")) |>
    terra::tapp("years", mean)

  pre_df <- exactextractr::exact_extract(
    pre_raster,
    buffer_dhs,
    fun = c("mean", "sum"),
    append_cols = "DHSID",
    colname_fun = function(values, fun_name, ...)
      paste("pre", fun_name, values, sep = "_")
  )

  ### Long reshape ------------------------------------------

  pre_mean <- pre_df  |>
    tidyr::pivot_longer(cols=starts_with("pre_mean"),
                        names_to = "long_name1",
                        values_to = "pre_mean")|>
    dplyr::mutate(year=as.numeric(str_sub(long_name1, -4,-1))) |>
    dplyr::select(DHSID,year,pre_mean)

  pre_sum <- pre_df  |>
    tidyr::pivot_longer(cols=starts_with("pre_sum"),
                        names_to = "long_name2",
                        values_to =  "pre_sum")|>
    dplyr::mutate(year=as.numeric(str_sub(long_name2, -4,-1))) |>
    dplyr::select(DHSID,year,pre_sum)

  pre_df <- pre_mean |>
    left_join(pre_sum)

  ### Temporal Join ------------------------------------------------
  pre_full_df <- dhs |>
    st_drop_geometry() |>
    right_join(pre_df, by = c("DHSID", "DHSYEAR" = "year")) |>
    arrange(DHSID, DHSYEAR)


  pre_full_df  <- pre_full_df  |>
    dplyr::select(DHSID, DHSYEAR, starts_with ("pre"))



  rm(pre_raster, pre_sum, pre_mean, pre_df)

  # export intermediate data set
  pre_full_df |>
    readr::write_csv(paste0("./data-intermediate/pre_full_df_", distance, "km.csv.gz"))
  rm(pre_full_df)



  ## Drought -----------------------------------------------------------------
  spei_raster <- "./data-raw/ceda/spei12.nc" |>
    terra::rast(subds = "spei")

  spei_raster <- spei_raster |>
    subset(time(spei_raster) >= as.Date("1990-01-01")) |>
    terra::tapp("years", mean) #|>
  #terra::project("EPSG:3857")

  spei_df <- exactextractr::exact_extract(
    spei_raster,
    buffer_dhs,
    fun = c("mean", "sum"),
    append_cols = "DHSID",
    colname_fun = function(values, fun_name, ...)
      paste("spei", fun_name, values, sep = "_")
  )

  ### Long reshape ---------------------------------------------
  spei_mean <- spei_df  |>
    tidyr::pivot_longer(cols=starts_with("spei_mean"),
                        names_to = "long_name1",
                        values_to = "spei_mean")|>
    dplyr::mutate(year=as.numeric(str_sub(long_name1, -4,-1))) |>
    dplyr::select(DHSID,year,spei_mean)

  spei_sum <- spei_df  |>
    tidyr::pivot_longer(cols=starts_with("spei_sum"),
                        names_to = "long_name2",
                        values_to =  "spei_sum")|>
    dplyr::mutate(year=as.numeric(str_sub(long_name2, -4,-1))) |>
    dplyr::select(DHSID,year,spei_sum)

  spei_df <- spei_mean |>
    left_join(spei_sum)


  ### Temporal Join -------------------------------------------
  spei_full_df <- dhs |>
    st_drop_geometry() |>
    right_join(spei_df, by = c("DHSID", "DHSYEAR" = "year")) |>
    arrange(DHSID, DHSYEAR)


  spei_full_df <- spei_full_df |>
    dplyr::select(DHSID, DHSYEAR, starts_with("spei"))


  rm(spei_raster, spei_sum,spei_mean, spei_df)

  # export intermediate data set
  spei_full_df |>
    readr::write_csv(paste0("./data-intermediate/spei_full_df_", distance, "km.csv.gz"))
  rm(spei_full_df)
  
  
  ## Lights ------------------------------------------------------------------
  t0 <- Sys.time()
  light_rasters <-
    list.files(
      "./data-raw/IAC",
      pattern = ".tif$",
      ignore.case = T,
      all.files = T,
      full.names = T,
      recursive = F
    )

  # store names
  raster_names <- light_rasters |>
    sapply(basename)

  raster_dates <-  paste(substr(raster_names, 4,7), 1, 1, sep = "-") |>
    as.Date()

  # read rasters
  light_rasters <- light_rasters |>
    terra::rast()

  # set names/date
  terra::time(light_rasters) <- raster_dates

  light_rasters <- light_rasters |>
    terra::crop(africa) |>
    terra::tapp("years", mean)


  light_df1 <- exactextractr::exact_extract(
    light_rasters,
    buffer_dhs,
    fun = c("mean", "sum"),
    append_cols = "DHSID",
    colname_fun = function(values, fun_name, ...)
      paste("sol", fun_name,str_sub(values, 3,7), sep = "_")
  )
  t1 <- Sys.time()
  print(t1 - t0)


  ### Long reshape -----------------------------------------------------------------


  light_mean <- light_df1  |>
    tidyr::pivot_longer(cols=starts_with("sol_mean"),
                        names_to = "long_name1",
                        values_to = "sol_mean")|>
    dplyr::mutate(year=as.numeric(str_sub(long_name1, -4,-1))) |>
    dplyr::select(DHSID,year,sol_mean)

  light_sum <- light_df1  |>
    tidyr::pivot_longer(cols=starts_with("sol_sum"),
                        names_to = "long_name2",
                        values_to =  "sol_sum")|>
    dplyr::mutate(year=as.numeric(str_sub(long_name2, -4,-1))) |>
    dplyr::select(DHSID,year,sol_sum)

  light_df <- light_mean |>
    left_join(light_sum)




  ### Temporal Join ---------------------------------------------------
  light_full_df <- dhs |>
    st_drop_geometry() |>
    right_join(light_df, by = c("DHSID", "DHSYEAR" = "year")) |>
    arrange(DHSID, DHSYEAR)


  light_full_df <- light_full_df |>
    dplyr::select(DHSID, DHSYEAR, starts_with("sol"))

  rm(light_rasters, light_sum,light_mean, light_df)

  # export intermediate data set
  light_full_df |>
    readr::write_csv(paste0("./data-intermediate/light_full_df_", distance, "km.csv.gz"))
  rm(light_full_df)

  
  
  ## Conflict Data -----------------------------------------------------------
  conflicts <- "./data-processed/ged_loca.shp" |>
    sf::st_read()

  conflicts <- conflicts |>
    sf::st_filter(africa)

  conf_df <- buffer_dhs |>
    dplyr::select(DHSID, DHSYEAR) |>
    sf::st_join(conflicts) |>
    sf::st_drop_geometry()



  conf_5y_df <- conf_df |>
    dplyr::filter((DHSYEAR - as.numeric(ged_yer)) %in% 0:5) |>
    dplyr::group_by(DHSID) |>
    dplyr::summarise(
      conflict_5y_n = n(),
      conflict_5y_best = sum(ged_bst, na.rm = TRUE),
      conflict_5y_high = sum(ged_hgh, na.rm = TRUE),
      conflict_5y_low = sum(ged_low, na.rm = TRUE)
    ) |>
    ungroup()


  conf_4y_df <- conf_df |>
    dplyr::filter((DHSYEAR - as.numeric(ged_yer)) %in% 0:4) |>
    dplyr::group_by(DHSID) |>
    dplyr::summarise(
      conflict_4y_n = n(),
      conflict_4y_best = sum(ged_bst, na.rm = TRUE),
      conflict_4y_high = sum(ged_hgh, na.rm = TRUE),
      conflict_4y_low = sum(ged_low, na.rm = TRUE)
    ) |>
    ungroup()


  conf_3y_df <- conf_df |>
    dplyr::filter((DHSYEAR - as.numeric(ged_yer)) %in% 0:3) |>
    dplyr::group_by(DHSID) |>
    dplyr::summarise(
      conflict_3y_n = n(),
      conflict_3y_best = sum(ged_bst, na.rm = TRUE),
      conflict_3y_high = sum(ged_hgh, na.rm = TRUE),
      conflict_3y_low = sum(ged_low, na.rm = TRUE)
    ) |>
    ungroup()


  conf_2y_df <- conf_df |>
    dplyr::filter((DHSYEAR - as.numeric(ged_yer)) %in% 0:2) |>
    dplyr::group_by(DHSID) |>
    dplyr::summarise(
      conflict_2y_n = n(),
      conflict_2y_best = sum(ged_bst, na.rm = TRUE),
      conflict_2y_high = sum(ged_hgh, na.rm = TRUE),
      conflict_2y_low = sum(ged_low, na.rm = TRUE)
    ) |>
    ungroup()

  conf_1y_df <- conf_df |>
    dplyr::filter((DHSYEAR - as.numeric(ged_yer)) %in% 0:1) |>
    dplyr::group_by(DHSID) |>
    dplyr::summarise(
      conflict_1y_n = n(),
      conflict_1y_best = sum(ged_bst, na.rm = TRUE),
      conflict_1y_high = sum(ged_hgh, na.rm = TRUE),
      conflict_1y_low = sum(ged_low, na.rm = TRUE)
    ) |>
    ungroup()


  conf_0y_df <- conf_df |>
    dplyr::filter((DHSYEAR - as.numeric(ged_yer)) == 0) |>
    dplyr::group_by(DHSID) |>
    dplyr::summarise(
      conflict_0y_n = n(),
      conflict_0y_best = sum(ged_bst, na.rm = TRUE),
      conflict_0y_high = sum(ged_hgh, na.rm = TRUE),
      conflict_0y_low = sum(ged_low, na.rm = TRUE)
    ) |>
    ungroup()


  # Merge

  conflicts_df <- conf_5y_df |>
    left_join(conf_4y_df) |>
    left_join(conf_3y_df) |>
    left_join(conf_2y_df) |>
    left_join(conf_1y_df) |>
    left_join(conf_0y_df)





  rm(conf_0y_df,conf_1y_df,conf_2y_df,conf_3y_df,conf_4y_df,conf_5y_df)



  # export intermediate data set
  conflicts_df |>
    readr::write_csv(paste0("./data-intermediate/conflicts_df_", distance, "km.csv.gz"))
  rm(conflicts, conflicts_df, conf_df)





  ## Population ------------------------------------------------------------------
  ### 2000-2020  -----------------------------------------------------


  pop_raster1 <-
    list.files(
      "./data-raw/population",
      pattern = "\\.tif$",
      ignore.case = T,
      all.files = T,
      full.names = T,
      recursive = F
    ) |>
    unique() |> # exclude already existing files
    terra::rast(subds = "Population Count")



  pop_df1 <- exactextractr::exact_extract(
    pop_raster1,
    buffer_dhs,
    fun = c("mean", "sum"),
    append_cols = "DHSID",
    colname_fun = function(values, fun_name, ...)
      paste("gpw", fun_name, str_sub(values, 31, 34), sep = "_"))



  ### 1990-1995 -----------------------------------------------------

  t0 <- Sys.time()
  pop_raster2 <-
    list.files(
      "./data-raw/population",
      pattern = "\\.asc$",
      ignore.case = T,
      all.files = T,
      full.names = T,
      recursive = F
    ) |>
    unique() |>
    terra::rast()


  pop_df2 <- exactextractr::exact_extract(
    pop_raster2,
    buffer_dhs,
    fun = c("mean", "sum"),
    append_cols = "DHSID",
    colname_fun = function(values, fun_name, ...)
      paste("gpw", fun_name, paste0(19,str_sub(values, 4, 5)), sep = "_")
  )

  t1 <- Sys.time()
  print(t1 - t0)


  
  ### Reshape -----------------------------------------------------------------

  pop_df <- pop_df1 |>
    left_join(pop_df2, by = "DHSID")

  pop_mean <- pop_df  |>
    tidyr::pivot_longer(cols=starts_with("gpw_mean"),
                        names_to = "long_name1",
                        values_to = "gpw_mean")|>
    dplyr::mutate(year=as.numeric(str_sub(long_name1, -4,-1))) |>
    dplyr::select(DHSID,year,gpw_mean)

  pop_sum <- pop_df  |>
    tidyr::pivot_longer(cols=starts_with("gpw_sum"),
                        names_to = "long_name2",
                        values_to =  "gpw_sum")|>
    dplyr::mutate(year=as.numeric(str_sub(long_name2, -4,-1))) |>
    dplyr::select(DHSID,year,gpw_sum)

  pop_df <- pop_mean |>
    left_join(pop_sum)


  ### Linear interpolation ----------------------------------------
  ids <- pop_df |>
    pull(DHSID) |>
    unique()

  # Find min and max.
  time.min <- pop_df |>
    pull(year) |>
    min()

  time.max <- pop_df |>
    pull(year) |>
    max()

  # Build full panel data set
  pop_full_df <- tidyr::expand_grid(DHSID = ids,
                                    year = seq(time.min, time.max, by = 1)) |>
    left_join(pop_df, by = c("DHSID", "year")) |>
    group_by(DHSID) |>
    mutate(
      gpw_ip_mean = zoo::na.approx(gpw_mean, maxgap = 4, rule = 2),
      gpw_ip_sum = zoo::na.approx(gpw_sum, maxgap = 4, rule = 2),
    )




  ### Temporal Join -------------------------------------
  pop_full_df <- dhs |>
    st_drop_geometry() |>
    right_join(pop_full_df, by = c("DHSID", "DHSYEAR" = "year")) |>
    arrange(DHSID, DHSYEAR)

  pop_full_df <- pop_full_df |>
    dplyr::select(DHSID, DHSYEAR, starts_with("gpw"))



  rm(pop_raster1, pop_raster2, pop_mean,pop_sum)

  # export intermediate data set
  pop_full_df |>
    readr::write_csv(paste0("./data-intermediate/pop_full_df_", distance, "km.csv.gz"))
  rm(pop_full_df)
  rm(pop_df, pop_df1, pop_df2)



  ## GADM Data -----------------------------------------------------------

  gadm <- read_csv("./data-intermediate/gadm_regions_1_2.csv")

  # load intermediate data files
  conflicts_df <- read_csv(paste0("./data-intermediate/conflicts_df_", distance, "km.csv.gz"))
  tmp_full_df <- read_csv(paste0("./data-intermediate/tmp_full_df_", distance, "km.csv.gz"))
  pre_full_df <- read_csv(paste0("./data-intermediate/pre_full_df_", distance, "km.csv.gz"))
  spei_full_df <- read_csv(paste0("./data-intermediate/spei_full_df_", distance, "km.csv.gz"))
  light_full_df <- read_csv(paste0("./data-intermediate/light_full_df_", distance, "km.csv.gz"))
  pop_full_df <- read_csv(paste0("./data-intermediate/pop_full_df_", distance, "km.csv.gz"))




  # Join Data Sets  --------------------------------------------------------------
  full_raster <- dhs |>
    dplyr::left_join(gadm, by = "DHSID") |>
    dplyr::left_join(conflicts_df, by = "DHSID") |>
    dplyr::left_join(tmp_full_df, by = c("DHSID","DHSYEAR")) |>
    dplyr::left_join(pre_full_df, by = c("DHSID","DHSYEAR")) |>
    dplyr::left_join(spei_full_df, by = c("DHSID","DHSYEAR")) |>
    dplyr::left_join(light_full_df, by = c("DHSID","DHSYEAR")) |>
    dplyr::left_join(pop_full_df, by = c("DHSID","DHSYEAR"))


  # Save data -------------------------------------------------------
  full_raster |>
    readr::write_csv(paste0("./data-r4r/data_raster_", distance, "km.csv.gz"))
  #beepr::beep(3)

  for (i in names(full_raster)) {
    print(paste(i, sum(is.na(full_raster[i]))))
  }

  rm(list = ls())
}

# Run analysis different buffer sizes ---------------------------------------
distances <- c(50, 45, 40, 35,30,25,20)
for (distance in distances) {
  message("Running analysis for distance: ", distance)
  tt0 <- Sys.time()
  run_analysis(distance)
  tt1 <- Sys.time()
  message("Finished analysis for distance: ", distance, " in ", tt1 - tt0)
}

rm(list = ls())
