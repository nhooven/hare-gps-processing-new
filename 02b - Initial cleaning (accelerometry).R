# PROJECT: GPS data processing
# SCRIPT: 02b - Initial cleaning (accelerometry)
# AUTHOR: Nate Hooven
# EMAIL: nathan.d.hooven@gmail.com
# BEGAN: 25 Mar 2026
# COMPLETED: 25 Mar 2026
# LAST MODIFIED: 26 Mar 2026
# R VERSION: 4.4.3

# ______________________________________________________________________________
# 1. Load packages ----
# ______________________________________________________________________________

library(tidyverse)
library(DBI)
library(RSQLite)

# ______________________________________________________________________________
# 2. Data locations ----
# ______________________________________________________________________________

# directories
# GPS data
dir.gps <- "D:/hare_project/data_gps/"

# all csvs
dir.gps.csv <- paste0(dir.gps, "alldata/newdata/")

# establish connection
db.gps <- dbConnect(SQLite(), "database/gps.db")

# read deployment table
tbl.deploy <- dbReadTable(db.gps, "deploy")

# and split
tbl.deploy.split <- split(tbl.deploy, tbl.deploy$trackID)

# ______________________________________________________________________________
# 3. Read and clean ----

# these files are huge, and rbind struggles on my PC on the final list

# ______________________________________________________________________________

# function
read_acc_data <- function (x) {
  
  # read csv
  full.data <- read.csv(paste0(dir.gps.csv, x$new_file), sep = "")
  
  # fix "timestamp" issues
  if ("Timestamp" %in% colnames(full.data)) {
    
    colnames(full.data) <- c("TagID",
                             "Date",
                             "Time",
                             "X",
                             "Y",
                             "Z",
                             "Activity",
                             "location.lat",
                             "location.lon",
                             "height.msl",
                             "ground.speed",
                             "satellites",
                             "hdop",
                             "signal.strength",
                             "Battery",
                             "X.V.")
    
  }
  
  # clean
  full.data.1 <- full.data %>%
    
    # drop unused columns (accelerometry, Battery, X.V., metadata)
    # "raw" is actually the battery column in this format
    dplyr::select(c(Date, 
                    Time, 
                    X,
                    Y,
                    Z,
                    Activity)) %>%
    
    # create timestamp and coerce to POSIXct
    mutate(timestamp = dmy_hms(paste0(Date, " ", Time),
                               tz = "UTC")) %>%
    
    # coerce to correct timezone
    mutate(timestamp = with_tz(timestamp,
                               tzone = "America/Los_Angeles")) %>%
    
    # drop date and time
    dplyr::select(-c(Date, Time)) %>%
    
    # join in deployment information
    mutate(
      
      site = x$site,
      sex = x$sex,
      MRID = x$MRID,
      trackID = x$trackID,
      collar_date = x$collar_date,
      retrieve_date = x$retrieve_date,
      mort_date = x$mort_date
      
    )
  
  # return
  return(full.data.1)
  
}

# use function
all.acc.data <- lapply(tbl.deploy.split, read_acc_data)

# bind together 
all.acc.data.df <- do.call(rbind, all.acc.data)



















# disconnect
dbDisconnect(db.gps)