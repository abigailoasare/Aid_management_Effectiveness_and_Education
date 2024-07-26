Sys.setenv(LANGUAGE = "en")
# Load packages ----------------------------------------------------------------
library("haven")
library("dplyr")
library("foreign")
library("ggplot2")
library("tidyverse")
library("data.table")
library("Hmisc")
library("expss")
library("readxl")
library("readr")
library("stringr")
library("zoo")
library(eeptools)# unique ID in a dataframe
library("plm")
##load geo_packages ------------------------------------------------------------
library("sf")
library("sp")
library("raster")
library("R.utils")

# Setup -------------------------------------------------------------------
source("./00_setup.R")



# Load overview data -----------------------------------------------------------
## Load all Aid projects and Save as csv files ---------------------------------

## Projects  --------------------------------------------------------------------
aid_data_projects <- read_csv(file.path(dir[["aid_raw"]], "projects.csv"))



# Aid Locations  ---------------------------------------------------------------
aid_data_locations <- read_csv(file.path(dir[["aid_raw"]], "locations.csv"))


# select Aids in Africa
aid_data_locations <- aid_data_locations %>%
  separate(
    gazetteer_adm_name,
    into = c("planet", "continent", "country", "region", "town")
    ,
    sep = "([.|:])",
    extra = "merge",
    remove = F,
    convert = T
  ) %>%
  filter(continent == "Africa")

## Prepare Aid Data for GIS Processing -----------------------------------------
Aid_locations <- aid_data_locations %>%
  dplyr::select(project_id,
                project_location_id,
                precision_code,
                latitude,
                longitude, 
                country
  ) %>%
  dplyr::mutate(latitude = as.numeric(latitude),
                longitude = as.numeric(longitude)) %>%
  dplyr::rename(
    pro_id = project_id,
    pro_loc_id = project_location_id,
    pre_cod = precision_code,
    lat = latitude,
    long = longitude
  )

# Convert the dataframe to a spatial object. Note that the
#  crs= 4326 parameter assigns a WGS84 coordinate system to the spatial object

Aid_locations <- Aid_locations %>%
  st_as_sf(coords = c("long", "lat"),
           crs = 4326,
           remove = FALSE)


## save as shapefile
sf::st_write(
  Aid_locations,
  dsn = file.path(dir[["data-processed"]], "aid_locations.shp"),
  layer = "aid_locations",
  driver = "ESRI Shapefile",
  append = FALSE
)


# Transactions  ----------------------------------------------------------------
# Transactions  ----------------------------------------------------------------
aid_data_trans <- read_csv(file.path(dir[["aid_raw"]], "transactions.csv"))

aid_data_trans <-as.data.table(aid_data_trans)



# Level  -----------------------------------------------------------------------
aid_data_level <- read_csv(file.path(dir[["aid_raw"]], "level_1a.csv"))


# Save data
Aid_level <-as.data.table(aid_data_level)


# Projects_ancillary -----------------------------------------------------------
aid_data_projects_anci <-
  read_csv(file.path(dir[["aid_raw"]], "projects_ancillary.csv"))

# to replace illegal character
names(aid_data_projects_anci) <-
  str_replace_all(names(aid_data_projects_anci),
                  pattern = "\\ ",
                  replacement = "_")


## Prepare Internal Project Evaluations Data -----------------------------------
aid_data_projects_anci <- aid_data_projects_anci  %>%
  dplyr::rename(
    disc_IEG_IDImpact = "(disc)IEG_IDImpact",
    disc_IEG_BorrPrep = "(disc)IEG_BorrPrep",
    disc_IEG_Sustainability = "(disc)IEG_Sustainability"
  )

Final_AidData_IEG <- aid_data_projects_anci  %>%
  dplyr::select(
    PROJECT_ID,
    IEG_EvalDate,
    IEG_EvalFY,
    TEAM_LEAD,
    IEG_EvalType,
    ERRatAppraisal,
    ERRatCompletion,
    IEG_Outcome,
    IEG_RDO,
    disc_IEG_IDImpact,
    IEG_BankQualityAtEntry,
    IEG_BankQualityOfSupervision,
    IEG_OverallBankPerf,
    disc_IEG_BorrPrep,
    IEG_ImplementingAgencyPerf,
    IEG_GovernmentPerf,
    IEG_OverallBorrPerf,
    IEG_ICRQuality,
    disc_IEG_Sustainability,
    IEG_MEQuality,
    IEG_SourceDocumentURL
  ) %>%
  # Keep Data with Project Performance Ratings
  dplyr::filter(
    !IEG_Outcome == "NA",
    !IEG_Outcome == "Not Applicable",
    !IEG_Outcome == "Not Available",
    !IEG_Outcome == "Not Rated",
    !IEG_BankQualityAtEntry == "NA",
    !IEG_BankQualityAtEntry == "NOT RATED",
    !IEG_BankQualityOfSupervision == "NA",
    !IEG_BankQualityOfSupervision == "NOT RATED",
    !IEG_BankQualityOfSupervision == "NOT AVAILABLE",
    !IEG_BankQualityOfSupervision == "NOT APPLICABLE",
    !IEG_OverallBankPerf =="NA",
    !IEG_ImplementingAgencyPerf == "NA",
    !IEG_ImplementingAgencyPerf == "NOT RATED",
    !IEG_ImplementingAgencyPerf == "NOT APPLICABLE",
    !IEG_GovernmentPerf == "NA",
    !IEG_GovernmentPerf == "NOT AVAILABLE",
    !IEG_GovernmentPerf == "NOT APPLICABLE",
    !IEG_OverallBorrPerf == "NA",
    !IEG_OverallBorrPerf == "NOT AVAILABLE",
    !IEG_OverallBorrPerf == "NOT RATED"
  )


# Drop Duplicate Project Performance Ratings

Final_AidData_IEG <- Final_AidData_IEG %>%
  dplyr::group_by(PROJECT_ID) %>%
  dplyr::mutate(dup = case_when(dplyr::row_number() == 1 ~ 0, TRUE ~ 1)) %>%
  dplyr::filter(!dup == 1) %>%
  dplyr::select(-dup) %>%
  dplyr::ungroup()



# Generate Project Performance Ratings Coding

Final_AidData_IEG <- Final_AidData_IEG %>%
  dplyr::mutate(
    ieg_outcome_code = case_when(
      IEG_Outcome == "Highly Unsatisfactory" ~ 1,
      IEG_Outcome == "Unsatisfactory" ~ 2,
      IEG_Outcome == "Moderately Unsatisfactory" ~ 3,
      IEG_Outcome == "Moderately Satisfactory" ~ 4,
      IEG_Outcome == "Satisfactory" ~ 5,
      IEG_Outcome == "Highly Satisfactory" ~ 6,
      TRUE ~ NA_real_
    ),
    ieg_BQE = case_when(
      IEG_BankQualityAtEntry == "HIGHLY UNSATISFACTORY" ~ 1,
      IEG_BankQualityAtEntry == "UNSATISFACTORY" ~ 2,
      IEG_BankQualityAtEntry == "MODERATELY UNSATISFACTORY" ~ 3,
      IEG_BankQualityAtEntry == "MODERATELY SATISFACTORY" ~ 4,
      IEG_BankQualityAtEntry == "SATISFACTORY" ~ 5,
      IEG_BankQualityAtEntry == "HIGHLY UNSATISFACTORY" ~ 6,
      TRUE ~ NA_real_
    ),
    ieg_BQS = case_when(
      IEG_BankQualityOfSupervision == "HIGHLY UNSATISFACTORY" ~ 1,
      IEG_BankQualityOfSupervision == "UNSATISFACTORY" ~ 2,
      IEG_BankQualityOfSupervision == "MODERATELY UNSATISFACTORY" ~ 3,
      IEG_BankQualityOfSupervision == "MODERATELY SATISFACTORY" ~ 4,
      IEG_BankQualityOfSupervision == "SATISFACTORY" ~ 5,
      IEG_BankQualityOfSupervision == "HIGHLY UNSATISFACTORY" ~ 6,
      TRUE ~ NA_real_
    ),
    ieg_OBaP = case_when(
      IEG_OverallBankPerf == "HIGHLY UNSATISFACTORY" ~ 1,
      IEG_OverallBankPerf == "UNSATISFACTORY" ~ 2,
      IEG_OverallBankPerf == "MODERATELY UNSATISFACTORY" ~ 3,
      IEG_OverallBankPerf == "MODERATELY SATISFACTORY" ~ 4,
      IEG_OverallBankPerf == "SATISFACTORY" ~ 5,
      IEG_OverallBankPerf == "HIGHLY UNSATISFACTORY" ~ 6,
      TRUE ~ NA_real_
    ),
    ieg_IAP = case_when(
      IEG_ImplementingAgencyPerf == "HIGHLY UNSATISFACTORY" ~ 1,
      IEG_ImplementingAgencyPerf == "UNSATISFACTORY" ~ 2,
      IEG_ImplementingAgencyPerf == "MODERATELY UNSATISFACTORY" ~ 3,
      IEG_ImplementingAgencyPerf == "MODERATELY SATISFACTORY" ~ 4,
      IEG_ImplementingAgencyPerf == "SATISFACTORY" ~ 5,
      IEG_ImplementingAgencyPerf == "HIGHLY UNSATISFACTORY" ~                                                                                                                                                                  6,
      TRUE ~ NA_real_
    ),
    ieg_GP = case_when(
      IEG_GovernmentPerf == "HIGHLY UNSATISFACTORY" ~ 1,
      IEG_GovernmentPerf == "UNSATISFACTORY" ~ 2,
      IEG_GovernmentPerf == "MODERATELY UNSATISFACTORY" ~ 3,
      IEG_GovernmentPerf == "MODERATELY SATISFACTORY" ~ 4,
      IEG_GovernmentPerf == "SATISFACTORY" ~ 5,
      IEG_GovernmentPerf == "HIGHLY UNSATISFACTORY" ~ 6,
      TRUE ~ NA_real_
    ),
    ieg_OBoP = case_when(
      IEG_OverallBorrPerf == "HIGHLY UNSATISFACTORY" ~ 1,
      IEG_OverallBorrPerf == "UNSATISFACTORY" ~ 2,
      IEG_OverallBorrPerf == "MODERATELY UNSATISFACTORY" ~ 3,
      IEG_OverallBorrPerf == "MODERATELY SATISFACTORY" ~ 4,
      IEG_OverallBorrPerf == "SATISFACTORY" ~ 5,
      IEG_OverallBorrPerf == "HIGHLY UNSATISFACTORY" ~ 6,
      TRUE ~ NA_real_
    ),
    ieg_ICRQ= case_when(
      IEG_ICRQuality == "UNSATISFACTORY" ~ 1,
      IEG_ICRQuality == "SATISFACTORY" ~ 2,
      IEG_ICRQuality == "EXEMPLARY" ~ 3,
      TRUE ~ NA_real_
    ),
    ieg_sus= case_when(
      disc_IEG_Sustainability == "HIGHLY UNLIKELY" ~ 1,
      disc_IEG_Sustainability == "UNLIKELY" ~ 2,
      disc_IEG_Sustainability == "LIKELY" ~ 3,
      disc_IEG_Sustainability == "HIGHLY LIKELY" ~ 4,
      TRUE ~ NA_real_
    ),
    ieg_MEQ= case_when(
      IEG_MEQuality == "NEGLIGIBLE" ~ 1,
      IEG_MEQuality == "MODEST" ~ 2,
      IEG_MEQuality == "HIGH" ~ 3,
      IEG_MEQuality == "SUBSTANTIAL" ~ 4,
      TRUE ~ NA_real_
    )
  ) %>%
  dplyr::rename(project_id = PROJECT_ID) %>%
  apply_labels(
    ieg_outcome_code = "IEG Outcome",
    ieg_BQE = "IEG Bank Quality At Entry",
    ieg_BQS = "IEG Bank Quality Of Supervision",
    ieg_OBaP = "Overall Bank Performance",
    ieg_IAP = "IEG Implementing Agency Performance",
    ieg_GP = "IEG Government Performance",
    ieg_OBoP = "IEG Overall Borrowing  Performance",
    ieg_MEQ = "IEG Monitoring and Evaluation Quality Ratings"
  )


# save

Final_AidData_IEG <- as.data.table(Final_AidData_IEG)


# Reduce data

Final_AidData_IEG_RD <- Final_AidData_IEG %>%
  dplyr::select(
    project_id,
    IEG_EvalDate,
    IEG_EvalFY,
    TEAM_LEAD,
    IEG_EvalType,
    ieg_outcome_code,
    ieg_BQE,
    ieg_BQS,
    ieg_OBaP,
    ieg_OBoP,
    ieg_IAP,
    ieg_GP,
    ieg_ICRQ,
    ieg_sus,
    ieg_MEQ
  )



# save reduced data

Final_AidData_IEG <- as.data.table(Final_AidData_IEG)



## Aid IEG Updated -------------------------------
aid_ieg_updated <- read_excel(
  file.path(dir[["aid_raw"]],"IEG_ICRR_PPAR_Ratings_2024-01-16.xlsx"))

names(aid_ieg_updated) <-
  str_replace_all(names(aid_ieg_updated),
                  pattern = "\\ ",
                  replacement = "_")

aid_ieg_updated <- aid_ieg_updated %>% 
  dplyr::rename(project_id=Project_ID)%>%
  dplyr::arrange(project_id) %>%
  dplyr::group_by(project_id) %>%
  dplyr::mutate(dup = case_when(dplyr::row_number() == 1 ~ 0, TRUE ~ 1)) %>%
  dplyr::filter(!dup == 1) %>%
  dplyr::select(-dup) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(IEG_EvalFY_ud=str_sub(Evaluation_Date,1,4))


aid_ieg_updated <- aid_ieg_updated %>% 
  dplyr::mutate(
    ieg_outcome_code = case_when(
      IEG_Outcome_Ratings == "Highly Unsatisfactory" ~ 1,
      IEG_Outcome_Ratings == "Unsatisfactory" ~ 2,
      IEG_Outcome_Ratings == "Moderately Unsatisfactory" ~ 3,
      IEG_Outcome_Ratings == "Moderately Satisfactory" ~ 4,
      IEG_Outcome_Ratings == "Satisfactory" ~ 5,
      IEG_Outcome_Ratings == "Highly Satisfactory" ~ 6,
      TRUE ~ NA_real_
    ),
    ieg_BQE = case_when(
      IEG_Quality_at_Entry_Ratings == "Highly Unsatisfactory" ~ 1,
      IEG_Quality_at_Entry_Ratings == "Unsatisfactory" ~ 2,
      IEG_Quality_at_Entry_Ratings == "Moderately Unsatisfactory" ~ 3,
      IEG_Quality_at_Entry_Ratings == "Moderately Satisfactory" ~ 4,
      IEG_Quality_at_Entry_Ratings == "Satisfactory" ~ 5,
      IEG_Quality_at_Entry_Ratings == "Highly Unsatisfactory" ~ 6,
      TRUE ~ NA_real_
    ),
    ieg_BQS = case_when(
      IEG_Quality_of_Supervision_Ratings == "Highly Unsatisfactory" ~ 1,
      IEG_Quality_of_Supervision_Ratings == "Unsatisfactory" ~ 2,
      IEG_Quality_of_Supervision_Ratings == "Moderately Unsatisfactory" ~ 3,
      IEG_Quality_of_Supervision_Ratings == "Moderately Satisfactory" ~ 4,
      IEG_Quality_of_Supervision_Ratings == "Satisfactory" ~ 5,
      IEG_Quality_of_Supervision_Ratings == "Highly Unsatisfactory" ~ 6,
      TRUE ~ NA_real_
    ),
    ieg_OBaP = case_when(
      IEG_Bank_Performance_Ratings == "Highly Unsatisfactory" ~ 1,
      IEG_Bank_Performance_Ratings == "UnsatisfactoryY" ~ 2,
      IEG_Bank_Performance_Ratings== "Moderately Unsatisfactory" ~ 3,
      IEG_Bank_Performance_Ratings == "Moderately Satisfactory" ~ 4,
      IEG_Bank_Performance_Ratings== "Satisfactory" ~ 5,
      IEG_Bank_Performance_Ratings == "Highly Unsatisfactory" ~ 6,
      TRUE ~ NA_real_
    ),
    ieg_MEQ= case_when(
      IEG_Monitoring_and_Evaluation_Quality_Ratings== "Negligible" ~ 1,
      IEG_Monitoring_and_Evaluation_Quality_Ratings == "Modest" ~ 2,
      IEG_Monitoring_and_Evaluation_Quality_Ratings== "High" ~ 3,
      IEG_Monitoring_and_Evaluation_Quality_Ratings == "Substantial" ~ 4,
      TRUE ~ NA_real_
    )
  ) %>%
  apply_labels(
    ieg_outcome_code = "IEG Outcome",
    ieg_BQE = "IEG Bank Quality At Entry",
    ieg_BQS = "IEG Bank Quality Of Supervision",
    ieg_OBaP = "Overall Bank Performance",
    ieg_MEQ = "IEG Monitoring and Evaluation Quality Ratings"
  )


# Reduce data updated

aid_ieg_updated_RD <- aid_ieg_updated %>%
  dplyr::select(
    project_id,
    Evaluation_Date,
    IEG_EvalFY_ud,
    ieg_outcome_code,
    ieg_BQE,
    ieg_BQS,
    ieg_OBaP,
    ieg_MEQ
  )


# Merge Project updated and actual IEG score

Final_AidData_IEG <- Final_AidData_IEG_RD %>%
  left_join(aid_ieg_updated_RD , by = "project_id",
            relationship = "many-to-one") %>%
  arrange(project_id)

Final_AidData_IEG <- Final_AidData_IEG %>%
  mutate(ieg_outcome_code = coalesce(ieg_outcome_code.y,
                                     ieg_outcome_code.x),
         ieg_BQE = coalesce(ieg_BQE.y,
                            ieg_BQE.x),
         ieg_BQS = coalesce(ieg_BQS.y,
                            ieg_BQS.x),
         ieg_OBaP = coalesce(ieg_OBaP.y,
                             ieg_OBaP.x),
         ieg_MEQ = coalesce(ieg_MEQ.y,
                            ieg_MEQ.x)) %>%
  dplyr::select(-ieg_outcome_code.y,-ieg_outcome_code.x,
                -ieg_OBaP.y,-ieg_OBaP.x,-ieg_BQE.x,-ieg_BQE.y,
                -ieg_BQS.x, -ieg_BQS.y,-ieg_MEQ.x,-ieg_MEQ.y,
                -IEG_EvalFY_ud,-Evaluation_Date
  ) %>% 
  arrange(project_id) %>% 
  relocate(ieg_outcome_code,
           ieg_BQE,ieg_BQS,ieg_OBaP,ieg_MEQ,
           .after = IEG_EvalType)


# Prepare AidData Sector Coding Scheme -----------------------------------------
sec_coding <-
  read_csv(file.path(dir[["aid_raw"]], "correspondence_aiddata_coding_scheme_0.csv"))

sec_coding <- sec_coding %>%
  dplyr::filter(!is.na(purpose_code)) %>%
  dplyr::arrange(purpose_code) %>%
  dplyr::mutate(sector_3d  = as.integer(purpose_code / 100)) %>%
  dplyr::select(purpose_code,
                description,
                sector_3d,
                sector_3d_des,
                sector_ma,
                sector_ma_des)

sec_coding <- as.data.table(sec_coding)


# Prepare Aid level_1a Data -----------------------------------------------
## merge IEG ratings to level_1a

Aid_level <- Aid_level %>%
  left_join(Final_AidData_IEG, by = "project_id",
            relationship = "many-to-one") %>%
  arrange(project_id)

test<-Aid_level %>% 
  filter(is.na(ieg_outcome_code))

length(unique(test$project_id))

# Generate Aid data Recipients ISO Code

Aid_level <- Aid_level %>%
  mutate(
    isocode = stringr::str_trim(recipients_iso3, side = "both"),
    isocode = case_when(isocode == "XKX" ~ "KOS", TRUE ~ isocode)
  )

# IEG Rating Outcome of AidData Project Locations ------------------------------
# to replace illegal character
names(Aid_level) <- str_replace_all(names(Aid_level),
                                    pattern =  "\\.x",
                                    replacement = "")
names(Aid_level) <- str_replace_all(names(Aid_level),
                                    pattern =  "\\.y",
                                    replacement = "")

# Generate Indicator Variable According to IEG Rating Outcome
Aid_level <- Aid_level %>%
  dplyr::mutate(
    wb_ieg_code_1 = case_when(ieg_outcome_code == 1 ~ 1, TRUE ~ 0),
    wb_ieg_code_2 = case_when(ieg_outcome_code == 2 ~ 1, TRUE ~
                                0),
    wb_ieg_code_3 = case_when(ieg_outcome_code == 3 ~ 1, TRUE ~
                                0),
    wb_ieg_code_4 = case_when(ieg_outcome_code == 4 ~ 1, TRUE ~
                                0),
    wb_ieg_code_5 = case_when(ieg_outcome_code == 5 ~ 1, TRUE ~
                                0),
    wb_ieg_code_6 = case_when(ieg_outcome_code == 6 ~ 1, TRUE ~
                                0),
    wb_ieg_code_nr = case_when(is.na(ieg_outcome_code) ~ 1, TRUE ~
                                 0) #Non-Rated Projects
  )
# Split Aid data Sector Coding into Multiple Variable Parts
Aid_level <- Aid_level %>%
  separate(
    ad_sector_codes,
    into = c(
      "ad_sector_codes_1",
      "ad_sector_codes_2",
      "ad_sector_codes_3",
      "ad_sector_codes_4",
      "ad_sector_codes_5"
    )
    ,
    sep = "([.|:])",
    extra = "merge",
    remove = F,
    convert = T
  )


# Generate Indicator Variable for Aid_Data Sector Codings ----------------------

sec_list <- c(
  "111",
  "112",
  "113",
  "114",
  "120",
  "121",
  "140",
  "151",
  "160",
  "210",
  "220",
  "230",
  "240",
  "310",
  "311",
  "312",
  "321",
  "322",
  "331",
  "410",
  "430"
)

cols = paste0("wb_ad_sector_", sec_list)


for (i in sec_list) {
  cols = paste0("wb_ad_sector_", i)
  Aid_level[[cols]] <- case_when(
    Aid_level$ad_sector_codes_1 == i |
      Aid_level$ad_sector_codes_2 == i |
      Aid_level$ad_sector_codes_3 == i |
      Aid_level$ad_sector_codes_4 == i |
      Aid_level$ad_sector_codes_5 == i ~ 1,
    TRUE ~ 0
  )
}


Aid_level <- Aid_level %>%
  dplyr::mutate(
    sum_wb_ad_sector = 	wb_ad_sector_111 + wb_ad_sector_112 +
      wb_ad_sector_113 + wb_ad_sector_114 + wb_ad_sector_120 +
      wb_ad_sector_121 + wb_ad_sector_140 + wb_ad_sector_151 +
      wb_ad_sector_160 + wb_ad_sector_210 + wb_ad_sector_220 +
      wb_ad_sector_230 + wb_ad_sector_240 + wb_ad_sector_310 +
      wb_ad_sector_311 + wb_ad_sector_312 + wb_ad_sector_321 +
      wb_ad_sector_322 + wb_ad_sector_331 + wb_ad_sector_410 + wb_ad_sector_430
  )

# Generate Edu Project management score

Aid_level <- Aid_level %>%
  dplyr::rowwise() %>% 
  dplyr::mutate(edu = any(wb_ad_sector_111, wb_ad_sector_112,
                          wb_ad_sector_113, wb_ad_sector_114)) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    ieg_outcome_edu = ifelse(edu >= 1,ieg_outcome_code,0),
    wb_ieg_edu_1 = case_when(edu >= 1 &
                               ieg_outcome_code == 1 ~ 1, TRUE ~ 0),
    wb_ieg_edu_2 = case_when(edu >= 1 &
                               ieg_outcome_code == 2 ~ 1, TRUE ~
                               0),
    wb_ieg_edu_3 = case_when(edu >= 1 &
                               ieg_outcome_code == 3 ~ 1, TRUE ~
                               0),
    wb_ieg_edu_4 = case_when(edu >= 1 &
                               ieg_outcome_code == 4 ~ 1, TRUE ~
                               0),
    wb_ieg_edu_5 = case_when(edu >= 1 &
                               ieg_outcome_code == 5 ~ 1, TRUE ~
                               0),
    wb_ieg_edu_6 = case_when(edu >= 1 &
                               ieg_outcome_code == 6 ~ 1, TRUE ~
                               0),
    ieg_BQE_edu = ifelse(edu >= 1,ieg_BQE, 0),
    ieg_BQS_edu = ifelse(edu >= 1,ieg_BQS,0),
    ieg_OBaP_edu = ifelse(edu >= 1, ieg_OBaP,0),
    ieg_MEQ_edu = ifelse(edu >= 1,ieg_MEQ,0),
    ieg_IAP_edu = ifelse(edu >= 1,ieg_MEQ,0),
    ieg_GP_edu = ifelse(edu >= 1,ieg_IAP,0),
    ieg_ICRQ_edu = ifelse(edu >= 1,ieg_ICRQ,0),
    ieg_sus_edu = ifelse(edu >= 1,ieg_sus,0)
  )

# reduce data
rating_aid  <- Aid_level %>%
  dplyr::select(
    project_id,
    project_location_id,
    IEG_EvalDate,
    IEG_EvalFY,
    TEAM_LEAD,
    IEG_EvalType,
    ieg_outcome_code,
    ieg_outcome_edu,
    starts_with(c("wb_ieg_","ieg_"), ignore.case = TRUE)
  )



write_csv(rating_aid,file.path(dir[["data-processed"]],"rating_aid_map.csv"))

# Expand AidData Project Locations According to Start and End Date  ------------

Aid_level <- Aid_level %>%
  mutate(datestart = format(as.Date(start_actual_isodate), "%Y"),
         dateend = format(as.Date(end_actual_isodate), "%Y"))
#Those project locations without end dates are mainly- 
#additional financing projects (see project_title) 

## Set End Date to the Year 2023 if project status is ``Implementation --------
Aid_level <- Aid_level %>%
  mutate(
    dateend_missing = ifelse(is.na(dateend), 1, 0),
    dateend = case_when(
      dateend_missing == 1 & status == "Implementation" ~ "2023-12-31",
      TRUE ~ dateend
    )
  )


## Set End Date to Start Date if project status is ``Completion----------------

Aid_level <- Aid_level %>%
  mutate(dateend = case_when(
    dateend_missing == 1 &
      status == "Completion" ~ datestart,
    TRUE ~ dateend
  ))
##cross-check
#Aid_1 <- Aid_level %>%
#dplyr::select(status,dateend, datestart,dateend_missing) %>%
#dplyr::filter(dateend_missing == 1)

## Generate Duration Variable for project locations until year 2023 -------------

Aid_level <- Aid_level %>%
  mutate(duration = (
    as.yearmon(dateend, format = "%Y") - as.yearmon(datestart, format = "%Y")
  ) + 1)

## Expand Aid_Data Project Locations According to Start and End Date -----------
## Expand data based on duration column
# Aid_level <-Aid_level%>% 
#   slice(rep(1:n(), times = duration))

Aid_level <- Aid_level %>%
  group_by(project_location_id) %>%
  dplyr::mutate(
    id = row_number(),
    mdate = as.numeric(datestart) + id - 1,
    year  = format(as.yearmon(mdate), "%Y")
  ) %>%
  ungroup() %>%
  dplyr::select(-id) %>%
  arrange(project_location_id)


# Generate Aid data Indicator According to Project Duration

num <- seq(1, 15, by = 1)


for (i in num) {
  cols = paste0("wbAid_dur_", i, "year", sep="")
  Aid_level[[cols]] = case_when(Aid_level$duration >= i ~ 1, TRUE ~ 0)
}

# Create World Bank foreign aid active indicator based on start and end date
Aid_level <- Aid_level %>%
  dplyr::mutate(wb_I_active = case_when(year >= datestart &
                                          year <= dateend ~ 1, TRUE ~ 0))

# Generate Aid Data Time Variable According to Project Duration

Aid_level <- Aid_level %>%
  mutate(wbAid_dur_time = as.numeric(year) - as.numeric(datestart) + 1)

Aid_level <- Aid_level %>%
  dplyr::filter(year < 2023) %>%
  arrange(project_id, project_location_id, year)


Aid_level <- Aid_level %>%
  dplyr::arrange(project_id, project_location_id, year) %>%
  dplyr::relocate(project_id, project_location_id, datestart, dateend,year) %>% 
  dplyr::select(-mdate)




## Clean the Data --------------------------------------------------------------

### Keep Project Locations with Geographic Precision <= ADM2 

Aid_level_1a <- Aid_level %>%
  filter(precision_code < 4)


### Drop Aid Projects with Unspecified Recipients Iso Code 

Aid_level_1a <- Aid_level_1a %>%
  filter(!isocode == "Unspecified")

# Construct Aid Data Variables -------------------------------------------------


Aid_level_1a <- Aid_level_1a %>%
  dplyr::arrange(project_id) %>% 
  dplyr::group_by(project_location_id) %>% 
  dplyr::mutate(pl_id=cur_group_id()) %>% 
  dplyr::ungroup()


# set as panel 
Aid_level_1a <- pdata.frame(Aid_level_1a , index=c("pl_id", "year"))

## Number of project-years  -------------------------------------

Aid_level_1a <- Aid_level_1a %>%
  dplyr::group_by(project_id,year) %>% 
  dplyr::mutate(count_project_id=n()) %>% 
  dplyr::ungroup()



# save Final Aid data ---------------------------------------------------------


write_csv(
  Aid_level_1a,
  file.path(dir[["data-processed"]], "Final_WB_Aid_Projects_bf.csv")
)
#beepr::beep(3)


rm(list = ls())

















