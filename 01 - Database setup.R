# PROJECT: GPS data processing
# SCRIPT: 01 - Database setup
# AUTHOR: Nate Hooven
# EMAIL: nathan.d.hooven@gmail.com
# BEGAN: 25 Mar 2026
# COMPLETED: 25 Mar 2026
# LAST MODIFIED: 27 Mar 2026
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

# read data
# deployment information (base table)
table.deploy <- read.csv(paste0(getwd(), "/data_raw/deploy_inc.csv"))

# ______________________________________________________________________________
# 3. Set up database ----
# ______________________________________________________________________________

# establish connection
db.gps <- dbConnect(SQLite(), "database/gps.db")

# deployment table
dbExecute(
  
  db.gps, 
  
  "CREATE TABLE deploy (
  
  site char(2),
  animalID varchar(9),
  sex char(1),
  ET1 int(4),
  MRID varchar(11),
  trackID varchar(6),
  collarID char(6),
  collar_date char(10),
  retrieve_date char(10),
  mort_date char(10),
  original_file varchar(14),
  new_file varchar(10),
  notes varchar(255),
  PRIMARY KEY (trackID)
  
  );"
          )

# ______________________________________________________________________________
# 4. Prepare table and write ----
# ______________________________________________________________________________

table.deploy.1 <- table.deploy %>%
  
  # change dates to SQL-friendly YYYY-MM-DD
  # probably not necessary - we'll keep that format but store as a text
  mutate(
    
    collar.date = as.character(mdy(collar.date)),
    retrieve.date = as.character(mdy(retrieve.date)),
    mort.date = as.character(mdy(mort.date))

  ) %>%
  
  # rename to replace periods
  rename(
    
    collar_date = collar.date,
    retrieve_date = retrieve.date,
    mort_date = mort.date,
    original_file = original.file,
    new_file = new.file
    
  )

# write
dbWriteTable(db.gps, "deploy", table.deploy.1, append = T)

# test to make sure it works
dbGetQuery(db.gps, "SELECT * FROM deploy LIMIT 10;")

dbDisconnect(db.gps)

# next, we'll read in GPS data iteratively, clean, and write to the database
