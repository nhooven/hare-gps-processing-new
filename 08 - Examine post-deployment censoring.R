# PROJECT: GPS data processing
# SCRIPT: 08 - Examine post-deployment censoring
# AUTHOR: Nate Hooven
# EMAIL: nathan.d.hooven@gmail.com
# BEGAN: 31 Mar 2026
# COMPLETED: 
# MODIFIED: 31 Mar 2026
# R VERSION: 4.4.3

#_______________________________________________________________________________
# 1. Load required packages ----
#_______________________________________________________________________________

library(tidyverse)
library(DBI)
library(RSQLite)
library(amt)

# ______________________________________________________________________________
# 2. Data locations ----
# ______________________________________________________________________________

# directories
# GPS data
dir.gps <- "D:/hare_project/data_gps/"

# establish connection
db.gps <- dbConnect(SQLite(), "database/gps.db")

# read table
tbl.gps <- dbReadTable(db.gps, "gps_clean4")  # read data

# ______________________________________________________________________________
# 3. Calculate speeds by day ----

# we'll just do this by trackID

# ______________________________________________________________________________

all.speeds <- data.frame()

for (i in 1:length(unique(tbl.gps$trackID))) {
  
  focal.track <- tbl.gps %>%
    
    filter(trackID == unique(tbl.gps$trackID)[i]) %>%
    
    # correct timestamp
    mutate(timestamp = ymd_hms(timestamp, tz = "America/Los_Angeles")) %>% 
    
    make_track(
      
      .x = lon,
      .y = lat,
      .t = timestamp,
      crs = "epsg:4326",
      all_cols = T
      
    ) %>%
    
    # convert to UTM
    transform_coords(crs_to = "epsg:32611")
  
  # calculate speeds
  focal.track$speed <- speed(focal.track)
  
  # summarize
  focal.speed <- focal.track %>%
    
    dplyr::select(sex,
                  trackID,
                  days_cap,
                  speed) %>%
    
    # truncate days_cap
    mutate(days_cap = trunc(days_cap)) %>%
    
    group_by(days_cap) %>%
    
    summarize(speed.mean = mean(speed)) %>%
    
    ungroup() %>%
    
    # add identifiers
    mutate(sex = focal.track$sex[1],
           trackID = focal.track$trackID[1])
  
  # bind in
  all.speeds <- rbind(all.speeds, focal.speed)
  
}

# ______________________________________________________________________________
# 4. Visualize ----
# ______________________________________________________________________________

ggplot(data = all.speeds) +
  
  theme_classic() +
  
  facet_wrap(~ sex) +
  
  geom_point(aes(x = days_cap,
                 y = speed.mean),
             alpha = 0.05) +
  
  geom_boxplot(aes(x = as.factor(days_cap),
                   y = speed.mean),
               outliers = F,
               fill = NA) +
  
  scale_x_discrete(breaks = seq(1, 30, 2)) +
  
  coord_cartesian(xlim = c(0, 30),
                  ylim = c(0, 0.04))

# let's drop anything < 5 days
