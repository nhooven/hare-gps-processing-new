# PROJECT: GPS data processing
# SCRIPT: 05 - Prepare data for Movebank
# AUTHOR: Nate Hooven
# EMAIL: nathan.d.hooven@gmail.com
# BEGAN: 27 Mar 2026
# COMPLETED: 27 Mar 2026
# MODIFIED: 27 Mar 2026
# R VERSION: 4.4.3

# ______________________________________________________________________________
# 0. Purpose ----
# ______________________________________________________________________________

# here we'll create a reference file with required deployment information,
# and separate .csvs for each deployment

#_______________________________________________________________________________________________
# 1. Load required packages ----
#_______________________________________________________________________________________________

library(tidyverse)
library(DBI)
library(RSQLite)

# ______________________________________________________________________________
# 2. Data locations ----
# ______________________________________________________________________________

# directories
# GPS data
dir.gps <- "D:/hare_project/data_gps/"

# establish connection
db.gps <- dbConnect(SQLite(), "database/gps.db")

# read tables
tbl.deploy <- dbReadTable(db.gps, "deploy")  # deployment
tbl.gps.clean1 <- dbReadTable(db.gps, "gps_clean1")  # read data

# ______________________________________________________________________________
# 3. Reference data ----
# ______________________________________________________________________________
# 3a. Clean ----
# ______________________________________________________________________________

data.ref <- tbl.deploy %>%
  
  # keep relevant columns
  dplyr::select(
    
    sex,
    MRID,
    trackID,
    collarID,
    collar_date,
    retrieve_date,
    mort_date
    
  ) %>%
  
  # correct deploy off date
  # convert to date
  mutate(
    
    collar_date = ymd(collar_date, tz = "America/Los_Angeles"),
    retrieve_date = ymd(retrieve_date, tz = "America/Los_Angeles"),
    mort_date = ymd(mort_date, tz = "America/Los_Angeles")

  ) %>%
  
  # pick the right date for "deploy off"
  mutate(
    
    deploy_off = as.POSIXct(ifelse(is.na(mort_date) == T,
                                   retrieve_date,
                                   mort_date)),
    
    # add "status"
    status = ifelse(is.na(mort_date) == T,
                    "removed",
                    "died")
    
  ) %>%
  
  # rename
  rename(deploy_on = collar_date) %>%
  
  # drop other date columns
  dplyr::select(-c(retrieve_date,
                   mort_date))

# ______________________________________________________________________________
# 3b. Fix overlapping deployments ----

# I input all of these as dates, but Movebank wants a datetime...
# if we redeployed anything on the same day, it will throw an error
# first we'll check if we have that problem at all (at least one in 22Y008)
# then increment one day, because R really wants to truncate the 0 time off of dates...

# ______________________________________________________________________________

data.ref.1 <- data.frame()

for (i in 1:length(unique(data.ref$collarID))) {
  
  # subset
  focal.collar <- data.ref %>% filter(collarID == unique(data.ref$collarID)[i])
  
  # only proceed if > 1 deployments
  if (nrow(focal.collar > 1)) {
    
    # which (if any) on match an off?
    which.on <- which(focal.collar$deploy_on %in% focal.collar$deploy_off, arr.ind = T)
    
    # increment (one day) if necessary
    for (j in 1:length(which.on)) {
      
      focal.collar$deploy_on[which.on[j]] <- focal.collar$deploy_on[which.on[j]] + 60*60*24
      
    }
    
  }
  
  # bind in
  data.ref.1 <- rbind(data.ref.1, focal.collar)
  
}

# remove trackID 1896_2 (no longer needed)
data.ref.1 <- data.ref.1 %>% filter(trackID != "1896_2")

# write to csv
write.csv(data.ref.1, paste0(getwd(), "/data_cleaned/data_mb/reference.csv"))

# ______________________________________________________________________________
# 4. GPS data ----
# ______________________________________________________________________________

tbl.gps.clean1.1 <- data.frame()

for (i in 1:length(unique(tbl.gps.clean1$trackID))) {
  
  # subset
  focal.data <- tbl.gps.clean1 %>% 
    
    filter(trackID == unique(tbl.gps.clean1$trackID)[i]) %>%
    
    # add collarID from reference table
    mutate(collarID = tbl.deploy$collarID[tbl.deploy$trackID == unique(tbl.gps.clean1$trackID)[i]][1])

  # bind in
  tbl.gps.clean1.1 <- rbind(tbl.gps.clean1.1, focal.data)

}

# write to csv
write.csv(tbl.gps.clean1.1, paste0(getwd(), "/data_cleaned/data_mb/gps_data.csv"))

# disconnect
dbDisconnect(db.gps)
