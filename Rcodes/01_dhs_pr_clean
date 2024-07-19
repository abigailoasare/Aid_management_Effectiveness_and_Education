# Load packages -----------------------------------------------------------
#Load packages
library(haven)
#for lag function
library(dplyr)
library(plyr)
#Load package "ggplot2"
library(ggplot2)
#Load tidyverse
library(tidyverse)
#load package "DescTools"
library(DescTools)
library(data.table)

# ZIP ---------------------------------------------------------------
##Set Directories ------------------------------------------------------

DHS_INPUT_DATA   <- "../input"
DHS_OUTPUT       <- "../output"
DHS_OUTPUT_DATA  <- "../output/input"
DHS_CODE <- "../code"

# Survey files ------------------------------------------------
# Obtain list of zipped files

zip_files <- list.files(path=DHS_INPUT_DATA, 
                        pattern = ".*PR.*\\DT.zip$", 
                        ignore.case = T, 
                        all.files = T,
                        full.names = T, 
                        recursive=T, 
                        include.dirs = T)

zip_files_1 <-
  list.files(
    path = DHS_INPUT_DATA,
    pattern = ".*PR.*\\DT.zip$",
    ignore.case = TRUE,
    full.names = F
  )


# start a fresh vector to fill
unzippedlist <- vector( mode = "character", length = 0L )

# for every ".zip" file we found...
for( zipfile in zip_files ) {
  # decide on a name for an output folder
  outfolder <- gsub( ".zip","", zipfile,ignore.case=T)
  
  # Create the full path to the output folder
  full_outfolder <- file.path(DHS_OUTPUT_DATA, outfolder)
  
  # Create the output folder
  dir.create(full_outfolder, recursive = TRUE, showWarnings = FALSE)
  
  # unzip into the new output folder
  ldply(.data=zipfile, .fun= unzip, exdir=full_outfolder)
  
  # get a list of files just unzipped
  newunzipped <- list.files( path = outfolder, full.names = T )
  
  # add that new list of files to the complete list
  unzippedlist <- c( unzippedlist, newunzipped )
}

# Create overview table ----------------------------------------------------------
overview <- tibble(ccode = substr(zip_files_1,1,2), 
                   dcode = substr(zip_files_1,3,4), 
                   version= substr(zip_files_1,5,6))
subfolder<-substr(zip_files_1, 1,8)


t0 <- Sys.time()
source("../code/01_label_datasets_v1.R")                    
t1 <- Sys.time()
print(t1 - t0)



write_dta(overview, file.path(DHS_OUTPUT_DATA,"overview.dta"))


