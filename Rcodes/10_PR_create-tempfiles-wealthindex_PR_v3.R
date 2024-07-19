# Load packages -----------------------------------------------------------
library(haven)
library(foreign)
library(tidyverse)
library(dplyr)
library(data.table)
library(Hmisc)#variable labeling
library(expss)# giving variable labels for each variable
library(labelled)#value labeling
library(sjlabelled)
library(naniar) # Replacing missing with NAs
library(stringr)
library("forcats")
library("eeptools")# unique ID in a dataframe

##Set Directories --------------------------------------------------------------
DHS_INPUT_DATA    <- "../output/input/"
DHS_OUTPUT        <- "../output"
DHS_CODE          <- "../code"
DHS_INPUT_IR   <- "../../IR/output/"
DHS_INPUT_MR    <- "../../MR/output/"

# Load overview data -----------------------------------------------------------

overview <- read_dta(file.path(DHS_INPUT_DATA, "overview.dta"))

overview <- overview %>%
  filter(datacategory == "Household survey recode", dcode == "PR")

ccode <- c(overview$ccode)
version <- c(overview$version)

# Separating DTA files ---------------------------------------------------------

dta_files <-
  list.files(
    DHS_INPUT_DATA,
    pattern = ".DTA",
    recursive = T,
    ignore.case = T
  ) %>%
  as_tibble() %>%
  dplyr::filter(value != "overview.dta") %>%
  dplyr::mutate(
    version = stringr::str_sub(value,-8,-7),
    type = stringr::str_sub(value,-10,-9),
    ccode = stringr::str_sub(value,-12,-11)
  )



list_1 <-
  c(
    "AO",
    " ML",
    "BJ",
    "BT",
    "MZ",
    "BF",
    "BU",
    "CM",
    "CV",
    "CF",
    "TD",
    "KM",
    "CG",
    "CD",
    "CI",
    "ST",
    "EG",
    "SL",
    "EK",
    "ER",
    "ET",
    "GA",
    "GM",
    "GH",
    "GN",
    "TG",
    "TN",
    "UG",
    "KE",
    "LS",
    "LB",
    "MD",
    "MW",
    "MA",
    "NI",
    "NG",
    "OS",
    "RW",
    "SN",
    "ZA",
    "SD",
    "SZ",
    "TZ",
    "ZM",
    "ZW"
  )

list_dta <- dta_files %>%
  dplyr::filter(ccode %in% list_1) %>%
  dplyr::select(value) %>%
  as.list() %>%
  unlist()

print(list_dta)

# Selecting variables ----------------------------------------------------------
variables <- c(
  "hhid",
  "hvidx",
  "hv000",
  "hv001",
  "hv002",
  "hv003",
  "hv004",
  "hv005",
  "hv008",
  "hv009",
  "hv016",
  "hv021",
  "hv022",
  "hv023",
  "hv024",
  "hv025",
  "hv111",
  "hv113",
  "hv101",
  "hv102",
  "hv103",
  "hv104",
  "hv105",
  "hv106",
  "hv107",
  "hv213",
  "hv214",
  "hv215",
  "hv026",
  "hv219",
  "hv220",
  "hv243a",
  "hv243d",
  "hv243b",
  "hv201",
  "hv205",
  "hv108",
  "hv109",
  "hv110",
  "hv121",
  "hv122",
  "hv123",
  "hv124",
  "hv125",
  "hv126",
  "hv127",
  "hv128",
  "hv202",
  "hv129",
  "hv247",
  "hv270",
  "hv270a",
  "hv271",
  "hv271a",
  "hv204",
  "hv236",
  "hv244",
  "hv245",
  "hv246",
  "hv246a",
  "hv246b",
  "hv246c",
  "hv246d",
  "hv246e",
  "hv246f",
  "hv243c",
  "hv235",
  "hv237",
  "hv206",
  "hv207",
  "hv208",
  "hv209",
  "hv210",
  "hv211",
  "hv212",
  "hv216",
  "hv221",
  "hv225",
  "hv235",
  "hv246g",
  "hv246h",
  "hv246i",
  "hv246j",
  "hv246k",
  "hv227"
)


var_dstr <- c(
  "hv023",
  "hv022",
  "region"
  #"relation_head",
  #"hv213",
  #"hv214",
  #"hv215",
  #"hv201",
  #"hv202",
  #"hv205",
  #"father_alive",
  #"mother_alive",
  #"age_member",
  #"level_edu",
  #"age_head",
  #"place_reside",
  # "wealth_index")
)


# Variable as string -----------------------------------------------------------
str_vars <<-
  c("hv018", "hv030", "hv031", "sh54", "ha62", "sh23")


# For testing purpose: Limit list size!
#list_dta <- list_dta[1:55]


# Loop across all countries and Versions----------------------------------------

out <- lapply(list_dta, function(x) {
  file <- read_dta(paste(DHS_INPUT_DATA, x, sep = "/")) %>%
    as_tibble() %>%
    dplyr::select(any_of(variables))
  
  # Generating variables that do not exist with NAs
  
  for (f in variables[!(variables %in% names(file))]) {
    #print(f)
    f <- stringr::str_trim(f, side = "both")
    file <- file %>%
      dplyr::mutate(!!f := NA_real_)
  }
  
  # Generating variables
  file <- file %>%
    dplyr::mutate(
      # Floor type
      mat_floor =  case_when(
        hv213 >= 10 &
          hv213 <= 19 ~ 10,
        hv213 >= 20 &
          hv213 <= 29 ~ 20,
        hv213 >= 30 & hv213 <= 39 ~ 30,
        TRUE ~ NA_real_
      ),
      # Wall type
      mat_walls =  case_when(
        hv214 >= 10 &
          hv214 <= 19 ~ 10,
        hv214 >= 20 &
          hv214 <= 29 ~ 20,
        hv214 >= 30 & hv214 <= 39 ~ 30,
        TRUE ~ NA_real_
      ),
      # Roof type
      mat_roof =  case_when(
        hv215 >= 10 & hv215 <= 19 ~ 10,
        hv215 >= 20 & hv215 <= 29 ~ 20,
        hv215 >= 30 & hv215 <= 39 ~ 30,
        TRUE ~ NA_real_
      ),
      # Water Source
      wat_source =   case_when(
        hv201 >= 10 & hv201 <= 19 ~ 10,
        hv201 >= 20 & hv201 <= 29 ~ 20,
        hv201 >= 30 & hv201 <= 39 ~ 30,
        hv201 >= 40 & hv201 <= 49 ~ 40,
        hv201 >= 50 & hv201 <= 59 ~ 50,
        hv201 >= 60 & hv201 <= 69 ~ 60,
        hv201 >= 70 & hv201 <= 79 ~ 70,
        TRUE ~ NA_real_
      ),
      # Toilet type
      wat_toilet =  case_when(
        hv205 >= 10 & hv205 <= 19 ~ 10,
        hv205 >= 20 & hv205 <= 29 ~ 20,
        hv205 >= 30 & hv205 <= 39 ~ 30,
        hv205 >= 40 & hv205 <= 49 ~ 40,
        TRUE ~ NA_real_
      ),
      # Other Animals
      other_animals =  case_when(
        hv246g == 0 |
          hv246h == 0 |
          hv246h == 0  |
          hv246i == 0 | hv246j == 0 | hv246k == 0 ~ 0,
        hv246g >= 1 &
          hv246g <= 95 |  hv246h >= 1 &
          hv246h <= 95 |  hv246i >= 1 & hv246i <= 95 |
          hv246j >= 1 &
          hv246j <= 95 |
          hv246k >= 1 & hv246k <= 95 ~ 1,
        TRUE ~ NA_real_
      ),
      
      # Add Information on Country and Version/Phase
      iso2code = str_sub(x, 1, 2),
      version = str_sub(x, 5, 6),
      
      # Generate Timing indicators
      hv008 = case_when(iso2code == "ET" ~ (hv008 + 92), TRUE ~ hv008),
      hv008 = case_when(iso2code == "NP" ~ (hv008 - 681), TRUE ~ hv008),
      interview_year = as.integer(((hv008 - 1) / 12) + 1900),
      interview_month = hv008 - ((interview_year - 1900) * 12),
      #
      
      # Generate Variables
      edu_years = hv108,
      edu_attainment = hv109,
      edu_inschool = hv110,
      
      # Dummies_for_Yes_No_Variables
      pipe_water = case_when(wat_source == 10 ~ 1, TRUE ~ 0),
      toilet = case_when(wat_toilet == 10 ~ 1, TRUE ~ 0),
      finished_floor = case_when(mat_floor == 30 ~ 1, TRUE ~ 0),
      finished_roof = case_when(mat_roof == 30 ~ 1, TRUE ~ 0),
      finished_wall = case_when(mat_walls == 30 ~ 1, TRUE ~ 0),
      son_daughter = case_when(hv101 == 3 ~ 1, TRUE ~ 0),
      grandchild = case_when(hv101 == 5 ~ 1, TRUE ~ 0),
      adopted_foster = case_when(hv101 == 11 ~ 1, TRUE ~ 0),
      sex_headm = case_when(hv219 == 1 ~ 1, TRUE ~ 0),
      sex_mem = case_when(hv104 == 1 ~ 1, TRUE ~ 0),
      area = case_when(hv025 == 2 ~ 1, TRUE ~ 0),
      birthyear = interview_year - hv105,
      birthyear_head = interview_year - hv220
    ) %>%
    # Renaming Variables
    dplyr::rename(
      sample_weight = hv005,
      date_cmc = hv008,
      size_household = hv009,
      day_interview = hv016,
      region = hv024,
      type_reside = hv025,
      place_reside = hv026,
      room_sleep = hv216,
      sex_head = hv219,
      age_head = hv220,
      loca_water = hv235,
      share_toilet = hv225,
      cattle = hv246a,
      cows = hv246b,
      horses = hv246c,
      goats = hv246d,
      sheep = hv246e,
      chickens = hv246f,
      bank_access = hv247,
      wealth_index = hv270,
      wealth_score = hv271,
      relation_head = hv101,
      reside_usual = hv102,
      slept_last = hv103,
      sex_member = hv104,
      age_member = hv105,
      level_edu = hv106,
      years_edu = hv107,
      mother_alive = hv111,
      father_alive = hv113,
      attendsch_current = hv121,
      levelsch_current = hv122,
      grade_current = hv123,
      eduyears_current = hv124,
      attendsch_previous = hv125,
      levelsch_previous = hv126,
      grade_previous = hv127,
      eduyears_previous = hv128,
      school_status = hv129,
      wealth_index_UrbanRural = hv270a,
      wealth_score_UrbanRural = hv271a,
      dhs_month = interview_month,
      dhs_year = interview_year,
      water_time = hv204,
      person_water = hv236,
      agric_land = hv244,
      hectares_land = hv245,
      livestock = hv246,
      electric = hv206,
      radio = hv207,
      tv = hv208,
      fridge = hv209,
      bike = hv210,
      motorcyle_scooter = hv211,
      car_truck = hv212,
      phone = hv221,
      mobile = hv243a,
      watch = hv243b,
      ani_drawn_cart = hv243c,
      boat_motor = hv243d,
      sleep_net = hv227
    ) %>%
    dplyr::mutate_at(var_dstr, as_label) %>%
    dplyr::mutate_at(var_dstr, as.character) %>%
    dplyr::rename(
      dhs_clust = hv001,
      dhs_version_pr = version,
      dhs_dhscc = iso2code
    ) %>%
    dplyr::mutate(dhs_weights = sample_weight / 1000000) %>%
    
    expss::apply_labels(dhs_weights = "(hv005) sample_weights/1 million") 
  
  return(file)
  
  
})



# Appending all countries and versions -----------------------------------------
dhs_full <- out %>%
  #readr::type_convert() %>%
  dplyr::bind_rows() %>%
  distinct()


dhs_full <- dhs_full %>%
  dplyr::mutate(water_time = ifelse(water_time == 996, 0, water_time))



unique(dhs_full$edu_inschool)
# Replacing all variables that are coded,9,99 and 999  and unknown(8,98,998)
# in the raw data as missing values which are coded as missing.
t0 <- Sys.time()



dhs_full <-
  dhs_full %>% mutate_at(
    c(
      "room_sleep",
      "sex_head",
      "age_head",
      "share_toilet",
      "cows",
      "goats",
      "loca_water",
      "agric_land",
      "hectares_land",
      "livestock",
      "cattle",
      "horses",
      "sheep",
      "chickens",
      "bank_access",
      "reside_usual",
      "slept_last",
      "sex_member",
      "age_member",
      "level_edu",
      "relation_head",
      "years_edu",
      "mother_alive",
      "father_alive",
      "attendsch_current",
      "levelsch_current",
      "grade_current",
      "attendsch_previous",
      "grade_previous",
      "eduyears_previous",
      "school_status",
      "water_time"
    ),
    ~ na_if(., 999)
  )


dhs_full <-
  dhs_full %>% mutate_at(
    c(
      "room_sleep",
      "sex_head",
      "age_head",
      "share_toilet",
      "cows",
      "goats",
      "loca_water",
      "agric_land",
      "hectares_land",
      "livestock",
      "cattle",
      "horses",
      "sheep",
      "chickens",
      "bank_access",
      "reside_usual",
      "slept_last",
      "sex_member",
      "age_member",
      "level_edu",
      "relation_head",
      "years_edu",
      "mother_alive",
      "father_alive",
      "attendsch_current",
      "levelsch_current",
      "grade_current",
      "attendsch_previous",
      "grade_previous",
      "eduyears_previous",
      "school_status",
      "water_time"
    ),
    ~ na_if(., 998)
  )

dhs_full <-
  dhs_full %>% mutate_at(
    c(
      "room_sleep",
      "sex_head",
      "age_head",
      "share_toilet",
      "cows",
      "goats",
      "loca_water",
      "agric_land",
      "livestock",
      "cattle",
      "horses",
      "sheep",
      "chickens",
      "bank_access",
      "reside_usual",
      "slept_last",
      "sex_member",
      "age_member",
      "level_edu",
      "relation_head",
      "years_edu",
      "mother_alive",
      "father_alive",
      "attendsch_current",
      "levelsch_current",
      "grade_current",
      "attendsch_previous",
      "grade_previous",
      "eduyears_previous",
      "school_status",
      "edu_years"
    ),
    ~ na_if(., 99)
  )



dhs_full <-
  dhs_full %>% mutate_at(
    c(
      "room_sleep",
      "sex_head",
      "age_head",
      "share_toilet",
      "cows",
      "goats",
      "loca_water",
      "agric_land",
      "hectares_land",
      "livestock",
      "cattle",
      "horses",
      "sheep",
      "chickens",
      "bank_access",
      "reside_usual",
      "slept_last",
      "sex_member",
      "age_member",
      "level_edu",
      "relation_head",
      "years_edu",
      "mother_alive",
      "father_alive",
      "attendsch_current",
      "levelsch_current",
      "grade_current",
      "attendsch_previous",
      "grade_previous",
      "eduyears_previous",
      "school_status",
      "edu_years"
    ),
    ~ na_if(., 98)
  )

dhs_full <-
  dhs_full %>% mutate_at(c("edu_years", "level_edu"),
                         ~ na_if(., 97))

dhs_full <-
  dhs_full %>% mutate_at(c("edu_years"),
                         ~ na_if(., 96))


dhs_full <-
  dhs_full %>% mutate_at(
    c(
      "sex_head",
      "loca_water",
      "share_toilet",
      "loca_water",
      "agric_land",
      "livestock",
      "electric",
      "radio",
      "tv",
      "fridge",
      "bike",
      "watch",
      "bank_access",
      "reside_usual",
      "slept_last",
      "sex_member",
      "edu_inschool",
      "motorcyle_scooter",
      "ani_drawn_cart",
      "level_edu",
      "mother_alive",
      "father_alive",
      "attendsch_current",
      "car_truck",
      "boat_motor",
      "levelsch_current",
      "attendsch_previous",
      "school_status",
      "phone",
      "mobile",
      "person_water",
      "sleep_net"
    ),
    ~ na_if(., 9)
  )


dhs_full <-
  dhs_full %>% mutate_at(
    c(
      "sex_head",
      "loca_water",
      "share_toilet",
      "loca_water",
      "agric_land",
      "livestock",
      "electric",
      "radio",
      "tv",
      "fridge",
      "bike",
      "watch",
      "bank_access",
      "reside_usual",
      "slept_last",
      "sex_member",
      "motorcyle_scooter",
      "ani_drawn_cart",
      "level_edu",
      "mother_alive",
      "father_alive",
      "attendsch_current",
      "car_truck",
      "boat_motor",
      "levelsch_current",
      "attendsch_previous",
      "school_status",
      "phone",
      "mobile"
    ),
    ~ na_if(., 8)
  )

dhs_full <-
  dhs_full %>% mutate_at(c("level_edu"),
                         ~ na_if(., 4))



t1 <- Sys.time()
print(t1 - t0)




eeptools::isid(
  dhs_full,
  vars = c("dhs_dhscc", "dhs_version_pr", "dhs_year",
           "hhid", "dhs_clust", "hvidx"),
  verbose = TRUE
)






# Labeling variables ------------------------------------------
dhs_full_1 <- dhs_full_1 %>%
  apply_labels(
    dhs_version_pr = "Type of DHS Survey Version",
    sample_weight = "(hv005)sample weight",
    date_cmc  = "(hv008)date of interview (cmc)",
    size_household = "(hv009)number of household members",
    day_interview = "(hv016) day of interview",
    region = "(hv024)region",
    type_reside = "(hv025)type of place of residence",
    place_reside = "(hv026)na - place of residence",
    room_sleep = "(hv216)na - rooms used for sleeping",
    sex_head = "(hv219)sex of head of household",
    age_head = "(hv220)age of head of household",
    loca_water = "(hv235)na - location of source for water",
    share_toilet = "(hv225)na - share toilet with other households",
    cattle = "(hv246a)na - cattle own",
    cows = "(hv246b)na - cows, bulls own",
    horses = "(hv246c)na - horses, donkeys, mules own",
    goats = "(hv246d)na - goats own",
    sheep = "(hv246e)na - sheep own",
    sheep = "(hv246f)na - chickens own",
    bank_access = "(hv247)Any household member that has bank account",
    wealth_index = "(hv270)wealth index combined",
    wealth_score = "(hv271)wealth index factor score combined (5 decimals)",
    relation_head = "(hv101)relationship to head",
    reside_usual = "(hv102)usual resident",
    slept_last = "(hv103)slept last night",
    sex_member = "(hv104)sex of household member",
    age_member  = "(hv105)age of household members",
    level_edu = "(hv106)na - highest educational level",
    years_edu = "(hv107)na - highest year of education at given level",
    mother_alive = "(hv111)na - mother alive",
    father_alive = "(hv113)na - father alive",
    attendsch_current  = "(hv121)na - member attended school during current school year",
    levelsch_current  = "(hv122)na - educational level during current school year",
    grade_current = "(hv123)na - grade of education during current school year",
    eduyears_current = "(hv124)na - education in single years - current school year",
    attendsch_previous = "(hv125)na - member attended school during previous school year",
    levelsch_previous = "(hv126)na - educational level during previous school year",
    grade_previous = "(hv127)na - grade of education during previous school year",
    eduyears_previous = "(hv128)na - education in single years - previous school year",
    school_status = "(hv129)na - school attendance status",
    wealth_index_UrbanRural = "(hv270a)wealth index for urban/rural",
    wealth_score_UrbanRural = "(hv271a)wealth index factor score for urban/rural (5 decimals)",
    dhs_month = "interview month",
    dhs_year = "interview year",
    water_time = "(hv204) time to get water source",
    person_water = "(hv236) person fetching water",
    agric_land  = "(hv244)own land usable for agriculture",
    hectares_land   = "(hv245) hectares for agricultural land",
    livestock  = "(hv246)livestock, herds or farm animals",
    mat_floor  =   "hv213-Main Floor Material",
    mat_walls =  "hv214-Main Wall Material",
    mat_roof = "hv215-Main Roof Material",
    wat_source =   "hv201-Water Source",
    wat_toilet =   "hv205-Toilet facility",
    other_animals =   "Other farm Animals",
    edu_years  = "(hv108) Years of Education",
    edu_attainment = "(hv109) Highest Educational Attainment",
    edu_inschool = "(hv110) Currently in School",
    electric = "(hv206)Household has Electricity",
    radio = "(hv207)Household has radio",
    tv = "(hv208)Household has tv",
    fridge = "(hv209)Household has refrigerator",
    bike = "(hv210)Household has bicycle",
    motorcyle_scooter = "(hv211)Household has moter cycle or scooter",
    car_truck  = "(hv212)Household has a car or truck",
    phone = "(hv221)Household has telephone",
    mobile = "(hv243a)Household has a mobile telephone",
    watch  = "(hv243b)Household has watch",
    ani_drawn_cart = "(hv243c)Household has animal drawn cart",
    boat_motor = "(hv243d)Household has a boat with motor",
    sleep_net = "(hv227)Household has mosquito bed net for sleeping",
    pipe_water = "Household main water source is pipe",
    toilet = "Household toilet facility",
    finished_floor = "Household main floor type",
    finished_roof = "Household main roof type",
    finished_wall = "Household main wall type",
    son_daughter = "Relation to head of household son/daugtherther",
    grandchild = "Relation to head of household as grandchild",
    adopted_foster = "Relation to head of household as adopted/foster child",
    sex_headm = "sex of household head dummy",
    sex_mem = "==1 if household member is male",
    area = "Location DHS household reside rural dummy",
    birthyear = "Birth year of Household member",
    birthyear_head = "Birth year of Household Head"
  )


# Saving dhs_full Data ---------------------------------------------------------

#write_csv(dhs_full_2, file.path(DHS_OUTPUT, "dhs_full.csv"))
expss::write_labelled_csv(dhs_full_1, file.path(DHS_OUTPUT, "dhs_full.csv"))

rm(list = ls())

