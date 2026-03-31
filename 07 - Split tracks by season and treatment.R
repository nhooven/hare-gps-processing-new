# PROJECT: GPS data processing
# SCRIPT: 07 - Split tracks by season and treatment
# AUTHOR: Nate Hooven
# EMAIL: nathan.d.hooven@gmail.com
# BEGAN: 31 Mar 2026
# COMPLETED: 
# MODIFIED: 31 Mar 2026
# R VERSION: 4.4.3

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

# read table
tbl.gps.clean3 <- dbReadTable(db.gps, "gps_clean3")  # read data

# ______________________________________________________________________________
# 3. Add split identifiers ----
# ______________________________________________________________________________

tbl.gps.clean4 <- tbl.gps.clean3 %>%
  
  mutate(
    
    # season only
    track_season = paste0(trackID, "_", season),
    
    # season and year (separating post1 and post2)
    track_season_year = paste0(trackID, "_", season, "_", year),
    
    # season and post-treatment (probably what we're use for analysis)
    track_season_post = paste0(trackID, "_", season, "_",
                               ifelse(year %in% c("POST1", "POST2"),
                                      "POST",
                                      "PRE"))
    
  )

# tabulate
length(unique(tbl.gps.clean4$trackID))             # 127 tracks
length(unique(tbl.gps.clean4$track_season))        # 186
length(unique(tbl.gps.clean4$track_season_year))   # 243
length(unique(tbl.gps.clean4$track_season_post))   # 217

# ______________________________________________________________________________
# 4. Add to database ----
# ______________________________________________________________________________

str(tbl.gps.clean4)

# create table
dbExecute(
  
  db.gps, 
  
  "CREATE TABLE gps_clean4 (
  
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
  days_cap float,
  year varchar(5),
  season varchar(3),
  trt varchar(4),
  track_season varchar(255),
  track_season_year varchar(255),
  track_season_post varchar(255),
  FOREIGN KEY (trackID) REFERENCES deploy(trackID)
  
  );"
)

# write
dbWriteTable(db.gps, "gps_clean4", tbl.gps.clean4, append = T)

# test to make sure it works
dbGetQuery(db.gps, "SELECT * FROM gps_clean4 LIMIT 10;")

# disconnect
dbDisconnect(db.gps)
