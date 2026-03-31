# PROJECT: GPS data processing
# SCRIPT: 04 - Remove erroneous relocations
# AUTHOR: Nate Hooven
# EMAIL: nathan.d.hooven@gmail.com
# BEGAN: 26 Mar 2026
# COMPLETED: 26 Mar 2026
# MODIFIED: 27 Mar 2026
# R VERSION: 4.4.3

# ______________________________________________________________________________
# 0. Purpose ----
# ______________________________________________________________________________

# before applying the error model, we'll use more traditional screening:
  # anything on or before collar day, on or after and retrieve/mort day
  # impossible elevations
  # high (> 10) HDOP

# we'll then use the error model to look at speeds, with error

#_______________________________________________________________________________
# 1. Load required packages ----
#_______________________________________________________________________________

library(tidyverse)
library(DBI)
library(RSQLite)
library(ctmm)                 # CTSP movement modeling

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
tbl.gps.read <- dbReadTable(db.gps, "gps_read")  # read data

# and split
tbl.deploy.split <- split(tbl.deploy, tbl.deploy$trackID)

# error model
load("data_cleaned/error_model.RData")

# ______________________________________________________________________________
# 3. "Traditional" screening ----
# ______________________________________________________________________________

error_screen_1 <- function (x) {
  
  # collar and retrieve dates 
  first.date <- ymd(x$collar_date, tz = "America/Los_Angeles")
  
  last.dates <- c(ymd(x$retrieve_date, 
                      tz = "America/Los_Angeles"), 
                  ymd(x$mort_date, tz = 
                        "America/Los_Angeles"))
  
  last.date <- last.dates[which.min(last.dates)]  # check which one comes first
  
  # subset data
  focal.data <- tbl.gps.read %>% 
    
    filter(trackID == x$trackID) %>%
    
    # correct timestamp
    mutate(timestamp = ymd_hms(timestamp, tz = "America/Los_Angeles")) %>%
    
    # SCREEN 1 - drop anything before/on collar date, on/after retrieve date
    # importantly, when doing POSIX math, treat the dates as 00:00,
    # so we need to add a full day to the first date
    filter(
      
      timestamp > first.date + 60*60*24 &
      timestamp < last.date
      
    ) %>%
    
    # SCREEN 2 - impossible elevations
    # drop anything < 1000 and > 2000
    filter(elev > 1000 & elev < 2000) %>%
    
    # SCREEN 3 - HDOP > 10
    filter(hdop < 10)
  
  # return
  return(focal.data)
  
}

# use function
gps.screen.1 <- lapply(tbl.deploy.split, error_screen_1)

# bind together
gps.screen.1.df <- do.call(rbind, gps.screen.1)

# check how we did
plot(gps.screen.1.df$lon, gps.screen.1.df$lat)  # still a few obvious outliers

# ______________________________________________________________________________
# 4. Error model speed screening ----
# ______________________________________________________________________________
# 4a. Examine speed thresholds ----
# ______________________________________________________________________________

speed_threshold <- function (x) {
  
  # subset
  focal.data <- gps.screen.1.df %>% filter(trackID == x$trackID)
  
  # only proceed if > relocation
  if (nrow(focal.data) > 1) {
    
    # convert to Movebank format
    focal.movebank <- data.frame("timestamp" = focal.data$timestamp,
                                 "location.lat" = focal.data$lat,
                                 "location.long" = focal.data$lon,
                                 "height above mean sea level" = focal.data$elev,
                                 "GPS satellite count" = focal.data$satellites,
                                 "GPS HDOP" = focal.data$hdop)
    
    # convert to telemetry object
    focal.telem <- as.telemetry(object = focal.movebank,
                                timeformat = "auto",
                                timezone = "America/Los_Angeles",
                                keep = TRUE)
    
    # add a "class" variable for error model
    focal.telem$class <- as.factor(ifelse(focal.telem$GPS.satellite.count > 3,
                                          "3D",
                                          "2D"))
    
    # add in error model
    uere(focal.telem) <- best.uere.HDOP.class
    
    # create outlier object
    focal.outlie <- outlie(focal.telem, plot = F)
    
    # speeds
    focal.speed <- data.frame(trackID = x$trackID,
                              speed = focal.outlie$speed)
    
    return(focal.speed)
    
  }
  
}

# use function
speed.threshold <- lapply(tbl.deploy.split, speed_threshold)

speed.threshold.df <- do.call(rbind, speed.threshold)

# quantiles
quantile(speed.threshold.df$speed)

# how many above 0.1 m/s?
sum(speed.threshold.df$speed > 0.1)

# histogram
ggplot(data = speed.threshold.df,
       aes(x = speed)) +
  
  theme_classic() +
  
  geom_histogram(bins = 10000) +
  
  coord_cartesian(xlim = c(0, 0.5))
  
# let's use 0.1 m/s (8.64 km/day) as the cutoff

# ______________________________________________________________________________
# 4b. Screen based on speed ----
# ______________________________________________________________________________

error_screen_2 <- function (x, threshold = 0.1) {
  
  # subset
  focal.data <- gps.screen.1.df %>% filter(trackID == x$trackID)
  
  # only proceed if > relocation
  if (nrow(focal.data) > 1) {
  
  # convert to Movebank format
  focal.movebank <- data.frame("timestamp" = focal.data$timestamp,
                               "location.lat" = focal.data$lat,
                               "location.long" = focal.data$lon,
                               "height above mean sea level" = focal.data$elev,
                               "GPS satellite count" = focal.data$satellites,
                               "GPS HDOP" = focal.data$hdop)
  
  # convert to telemetry object
  suppressMessages(
    
    suppressWarnings(
  
  focal.telem <- as.telemetry(object = focal.movebank,
                              timeformat = "auto",
                              timezone = "America/Los_Angeles",
                              keep = TRUE)
  
  )
  
  )
  
  # add a "class" variable for error model
  focal.telem$class <- as.factor(ifelse(focal.telem$GPS.satellite.count > 3,
                                        "3D",
                                        "2D"))
  
  # add in error model
  uere(focal.telem) <- best.uere.HDOP.class
  
  # create outlier object
  focal.outlie <- outlie(focal.telem, plot = F)
  
  # which are < threshold?
  which.toKeep <- which(focal.outlie$speed < threshold)
  
  focal.data.1 <- focal.data[which.toKeep, ]
  
  # return
  return(focal.data.1)
  
  }
  
}

# use function
gps.screen.2 <- lapply(tbl.deploy.split, error_screen_2)

gps.screen.2.df <- do.call(rbind, gps.screen.2)

# check how we did
plot(gps.screen.2.df$lon, gps.screen.2.df$lat)  # okay, looking good!

# ______________________________________________________________________________
# 5. Final cleaning and add to database ----
# ______________________________________________________________________________

gps.screen.3.df <- gps.screen.2.df %>%
  
  # timestamp as character
  mutate(timestamp = as.character(timestamp))

str(gps.screen.3.df)

# create table
dbExecute(
  
  db.gps, 
  
  "CREATE TABLE gps_clean1 (
  
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
dbWriteTable(db.gps, "gps_clean1", gps.screen.3.df, append = T)

# test to make sure it works
dbGetQuery(db.gps, "SELECT * FROM gps_clean1 LIMIT 10;")

# disconnect
dbDisconnect(db.gps)
