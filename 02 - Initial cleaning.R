# PROJECT: GPS data processing
# SCRIPT: 02 - Initial cleaning
# AUTHOR: Nate Hooven
# EMAIL: nathan.d.hooven@gmail.com
# BEGAN: 26 Mar 2026
# COMPLETED: 26 Mar 2026
# LAST MODIFIED: 27 Mar 2026
# R VERSION: 4.4.3

# ______________________________________________________________________________
# 0. Purpose ----
# ______________________________________________________________________________

# here we read in collar data files, filter relocations, and put into one table
# importantly, we don't remove any erroneous locations

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

# some of these are in an aberrant format, make sure the function can handle it

# ______________________________________________________________________________

# function
read_gps_data <- function (x) {
  
  # read csv
  full.data <- read.csv(paste0(dir.gps.csv, x$new_file), sep = "")
  
  # if 1240's data, fix
  if (x$trackID == "1240_1") {
    
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
                             c(1:7))
    
  }
  
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
  
  # if there's a "pressure" column
  if ("Pressure" %in% colnames(full.data)) {
    
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
                             "1",
                             "2",
                             "3",
                             "4",
                             "5")
    
  }
  
  # clean
  full.data.1 <- full.data %>%
    
    # drop unused columns (accelerometry, Battery, X.V., metadata)
    # "raw" is actually the battery column in this format
    dplyr::select(c(Date, 
                    Time, 
                    location.lat, 
                    location.lon,
                    height.msl,
                    satellites,
                    hdop)) %>%
    
    # coerce to numeric
    mutate_at(.vars = c("location.lon",
                        "location.lat",
                        "height.msl",
                        "hdop"),
              .funs = as.numeric) %>%
    
    # keep only real lat/long
    filter(location.lat > 48 & location.lat < 49 &
             location.lon > -120 & location.lon < -119) %>%
    
    # create timestamp and coerce to POSIXct
    mutate(timestamp = dmy_hms(paste0(Date, " ", Time),
                               tz = "UTC")) %>%
    
    # coerce to correct timezone
    mutate(timestamp = with_tz(timestamp,
                               tzone = "America/Los_Angeles")) %>%
    
    # drop date and time
    dplyr::select(-c(Date, Time)) %>%
    
    # rename - we need these to play nice with SQL
    rename(lat = location.lat,
           lon = location.lon,
           elev = height.msl) %>%
    
    # join in deployment information
    mutate(
      
      site = x$site,
      sex = x$sex,
      MRID = x$MRID,
      trackID = x$trackID
      
    )
  
  # ensure that there actually are data
  if (nrow(full.data.1) > 0) {
    
    # return
    return(full.data.1)
    
  } else {
    
    print("Warning: no data")
    
  }
  
}

# use function
all.gps.data <- lapply(tbl.deploy.split, read_gps_data)

# bind together
all.gps.data.df <- do.call(rbind, all.gps.data)

# change timestamp to character for SQL
all.gps.data.df$timestamp <- as.character(all.gps.data.df$timestamp)

# change trackID for 1896 - this was really the same track
all.gps.data.df$trackID[all.gps.data.df$trackID == "1896_2"] <- "1896_1"

# ______________________________________________________________________________
# 4. Add to database ----
# ______________________________________________________________________________

# drop table for overwriting
dbExecute(
  
  db.gps,
  
  "DROP TABLE gps_read;"
  
)

# create table
dbExecute(
  
  db.gps, 
  
  "CREATE TABLE gps_read (
  
  lat float,
  lon float,
  elev int,
  satellites int(1),
  hdop float,
  timestamp char(19),
  site char(2),
  sex char(1),
  MRID varchar(11),
  trackID varchar(6),
  FOREIGN KEY (trackID) REFERENCES deploy(trackID)
  
  );"
)

# write
dbWriteTable(db.gps, "gps_read", all.gps.data.df, append = T)

# test to make sure it works
dbGetQuery(db.gps, "SELECT * FROM gps_read LIMIT 10;")

# disconnect
dbDisconnect(db.gps)
