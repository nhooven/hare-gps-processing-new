# PROJECT: GPS data processing
# SCRIPT: 06 - Add season and treatment covariates
# AUTHOR: Nate Hooven
# EMAIL: nathan.d.hooven@gmail.com
# BEGAN: 27 Mar 2026
# COMPLETED: 31 Mar 2026
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

# read tables
tbl.deploy <- dbReadTable(db.gps, "deploy")  # deployment
tbl.gps.clean1 <- dbReadTable(db.gps, "gps_clean1")  # read data

# ______________________________________________________________________________
# 3. Days since capture ----
# ______________________________________________________________________________

# split
tbl.gps.clean1.split <- split(tbl.gps.clean1, tbl.gps.clean1$trackID)

# function
add_days_cap <- function (x) {
  
  # focal deploy
  focal.deploy <- tbl.deploy %>% 
    
    filter(trackID == x$trackID[1]) %>%
    
    mutate(collar_date = ymd(collar_date, tz = "America/Los_Angeles"))
  
  # add column
  x.1 <- x %>% 
    
    # convert to posix
    mutate(timestamp = ymd_hms(timestamp, tz = "America/Los_Angeles")) %>%
    
    mutate(days_cap = as.numeric(timestamp - focal.deploy$collar_date))
  
  return(x.1)
  
}

# use function
tbl.gps.clean2 <- lapply(tbl.gps.clean1.split, add_days_cap)

# bind together
tbl.gps.clean2.df <- do.call(rbind, tbl.gps.clean2)

# ______________________________________________________________________________
# 4. Years, seasons, and treatments ----
# ______________________________________________________________________________
# 4a. Define ----

# YEAR: PRE, POST1, POST2
# SEASON: snow-off, snow-on
# TRT: CTRL, RET, PIL

# controls will all switch on Oct 5 for consistency

# ______________________________________________________________________________

# year
lookup.year.expand <- expand.grid(site = c("1A", "1B", "1C", "2A", "2B", "2C", "3A", "3B", "3C", "4A", "4B", "4C"),
                                  year = c("PRE", "POST1", "POST2"))

lookup.year.expand

lookup.year <- cbind(
  
  lookup.year.expand,
                     
  data.frame(
    
    start = c(
      
      # PRE (all the same)
      rep(ymd("2022-10-01", tz = "America/Los_Angeles"), 12),   
      
      # POST1
      ymd("2023-10-12", tz = "America/Los_Angeles"),
      ymd("2023-10-11", tz = "America/Los_Angeles"),
      ymd("2023-10-05", tz = "America/Los_Angeles"),
      ymd("2023-10-07", tz = "America/Los_Angeles"),
      ymd("2023-10-06", tz = "America/Los_Angeles"),
      ymd("2023-10-05", tz = "America/Los_Angeles"),
      ymd("2023-10-04", tz = "America/Los_Angeles"),
      ymd("2023-10-05", tz = "America/Los_Angeles"),
      ymd("2023-10-05", tz = "America/Los_Angeles"),
      ymd("2023-10-10", tz = "America/Los_Angeles"),
      ymd("2023-10-09", tz = "America/Los_Angeles"),
      ymd("2023-10-05", tz = "America/Los_Angeles"),
      
      # POST2
      ymd("2024-10-12", tz = "America/Los_Angeles"),
      ymd("2024-10-11", tz = "America/Los_Angeles"),
      ymd("2024-10-05", tz = "America/Los_Angeles"),
      ymd("2024-10-07", tz = "America/Los_Angeles"),
      ymd("2024-10-06", tz = "America/Los_Angeles"),
      ymd("2024-10-05", tz = "America/Los_Angeles"),
      ymd("2024-10-04", tz = "America/Los_Angeles"),
      ymd("2024-10-05", tz = "America/Los_Angeles"),
      ymd("2024-10-05", tz = "America/Los_Angeles"),
      ymd("2024-10-10", tz = "America/Los_Angeles"),
      ymd("2024-10-09", tz = "America/Los_Angeles"),
      ymd("2024-10-05", tz = "America/Los_Angeles")
      
    ),
    
    end = c(
      
      # PRE
      ymd("2023-10-11", tz = "America/Los_Angeles"),
      ymd("2023-10-10", tz = "America/Los_Angeles"),
      ymd("2023-10-04", tz = "America/Los_Angeles"),
      ymd("2023-10-06", tz = "America/Los_Angeles"),
      ymd("2023-10-05", tz = "America/Los_Angeles"),
      ymd("2023-10-04", tz = "America/Los_Angeles"),
      ymd("2023-10-03", tz = "America/Los_Angeles"),
      ymd("2023-10-05", tz = "America/Los_Angeles"),
      ymd("2023-10-04", tz = "America/Los_Angeles"),
      ymd("2023-10-09", tz = "America/Los_Angeles"),
      ymd("2023-10-08", tz = "America/Los_Angeles"),
      ymd("2023-10-04", tz = "America/Los_Angeles"),
      
      # POST1
      ymd("2024-10-11", tz = "America/Los_Angeles"),
      ymd("2024-10-10", tz = "America/Los_Angeles"),
      ymd("2024-10-04", tz = "America/Los_Angeles"),
      ymd("2024-10-06", tz = "America/Los_Angeles"),
      ymd("2024-10-05", tz = "America/Los_Angeles"),
      ymd("2024-10-04", tz = "America/Los_Angeles"),
      ymd("2024-10-03", tz = "America/Los_Angeles"),
      ymd("2024-10-05", tz = "America/Los_Angeles"),
      ymd("2024-10-04", tz = "America/Los_Angeles"),
      ymd("2024-10-09", tz = "America/Los_Angeles"),
      ymd("2024-10-08", tz = "America/Los_Angeles"),
      ymd("2024-10-04", tz = "America/Los_Angeles"),
      
      # POST2 (all the same)
      rep(ymd("2025-11-01", tz = "America/Los_Angeles"), 12)
      
    )
    
  )
  
) %>%
  
  # pivot
  pivot_wider(names_from = year,
              values_from = c(start, end))

# season
# we'll just check if a relocation lives inside (snow on) or outside (snow off) any of these
lookup.season <- data.frame(
  
  lz = c("SFL", "XMC"),
  start1 = c(ymd("2022-11-06", tz = "America/Los_Angeles"),
             ymd("2022-11-06", tz = "America/Los_Angeles")),
  end1 = c(ymd("2023-04-30", tz = "America/Los_Angeles"),
           ymd("2023-04-30", tz = "America/Los_Angeles")),
  start2 = c(ymd("2023-10-25", tz = "America/Los_Angeles"),
             ymd("2023-10-25", tz = "America/Los_Angeles")),
  end2 = c(ymd("2024-04-24", tz = "America/Los_Angeles"),
           ymd("2024-04-12", tz = "America/Los_Angeles")),
  start3 = c(ymd("2024-10-31", tz = "America/Los_Angeles"),
             ymd("2024-11-02", tz = "America/Los_Angeles")),
  end3 = c(ymd("2025-04-30", tz = "America/Los_Angeles"),
           ymd("2025-04-17", tz = "America/Los_Angeles"))
  
)

# ______________________________________________________________________________
# 4b. Add in ----
# ______________________________________________________________________________

tbl.gps.clean3 <- tbl.gps.clean2.df %>%
  
  # year
  # join
  left_join(lookup.year) %>%
  
  mutate(
    
    year = case_when(
      
      timestamp <= end_PRE ~ "PRE",
      timestamp >= start_POST1 & timestamp <= end_POST1 ~ "POST1",
      timestamp >= start_POST2 & timestamp <= end_POST2 ~ "POST2"
      
    )
    
  ) %>%
  
  dplyr::select(-c(start_PRE, start_POST1, start_POST2,
                   end_PRE, end_POST1, end_POST2)) %>%
  
  # season
  # add lz
  mutate(lz = ifelse(substr(site, 1, 1) %in% c(1, 2, 3), "SFL", "XMC")) %>%
  
  # join
  left_join(lookup.season) %>%
  
  mutate(
    
    season = case_when(
      
      # snow on
      (timestamp >= start1 & timestamp <= end1) |
      (timestamp >= start2 & timestamp <= end2) |
      (timestamp >= start3 & timestamp <= end3) ~ "on",
      .default = "off"
      
    )
    
  ) %>%
  
  dplyr::select(-c(lz, start1, end1, start2, end2, start3, end3)) %>%
  
  # treatment
  mutate(
    
    trt = case_when(
      
      site %in% c("1C", "2C", "3C", "4C") ~ "CTRL",
      site %in% c("1A", "2B", "3B", "4A") ~ "RET",
      site %in% c("1B", "2A", "3A", "4B") ~ "PIL"
      
    )
    
  ) %>%
  
  # timestamp as character
  mutate(timestamp = as.character(timestamp))
  
# ______________________________________________________________________________
# 5. Add to database ----
# ______________________________________________________________________________

str(tbl.gps.clean3)

# create table
dbExecute(
  
  db.gps, 
  
  "CREATE TABLE gps_clean3 (
  
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
  FOREIGN KEY (trackID) REFERENCES deploy(trackID)
  
  );"
)

# write
dbWriteTable(db.gps, "gps_clean3", tbl.gps.clean3, append = T)

# test to make sure it works
dbGetQuery(db.gps, "SELECT * FROM gps_clean3 LIMIT 10;")

# disconnect
dbDisconnect(db.gps)
