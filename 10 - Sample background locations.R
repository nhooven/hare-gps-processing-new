# PROJECT: GPS data processing
# SCRIPT: 10 - Sample background locations
# AUTHOR: Nate Hooven
# EMAIL: nathan.d.hooven@gmail.com
# BEGAN: 20 Apr 2026
# COMPLETED: 20 Apr 2026
# MODIFIED: 20 Apr 2026
# R VERSION: 4.4.3

#_______________________________________________________________________________
# 1. Load required packages ----
#_______________________________________________________________________________

library(tidyverse)
library(DBI)
library(RSQLite)
library(ctmm)
library(sf)
library(terra)

# ______________________________________________________________________________
# 2. Read in data ----
# ______________________________________________________________________________

# establish connection
db.gps <- dbConnect(SQLite(), "database/gps.db")

# read table
tbl.gps <- dbReadTable(db.gps, "gps_clean4")  # read data

# AKDE models
all.akde <- readRDS("data_cleaned/all_akde.rds")

# model selection summaries
model.select <- readRDS("data_cleaned/all_model_select.rds")

# ______________________________________________________________________________
# 3. Clean relocations and prepare for G(s) sampling ----

# From Eisaguirre et al. (2025)
# https://doi.org/10.1111/ddi.70028

# ______________________________________________________________________________

# days < to censor
censor.day = 5

# split by track_season_post
tbl.gps.1 <- split(tbl.gps, tbl.gps$track_season_post)

# ensure that the sequence of tracks is the same
model.select.1 <- model.select %>%
  
  filter(mod == 1)

# ______________________________________________________________________________

# function
gps_clean <- function (x) {
  
  x.1 <- x %>% 
    
    # keep anything above the censor day
    filter(days_cap > censor.day) %>%
    
    # keep relevant columns for sampling
    dplyr::select(
      
      c(
        
        lat,
        lon,
        timestamp,
        site,
        sex,
        MRID,
        year,
        season,
        trt,
        track_season_post
        
      )
      
    ) %>%
    
    # add case variable
    mutate(case = 1)
  
  # we could only fit CTMMs with > 3 relocs
  if (nrow(x.1) > 3) {
    
    return(x.1)
    
  } else {
    
    return(NA)
    
  }
    
}

tbl.gps.2 <- lapply(tbl.gps.1, gps_clean)

# ______________________________________________________________________________
# 4. Sample from AKDEs ----
# ______________________________________________________________________________
# 4a. Function ----

# this accepts a cleaned relocation dataset (x) and an AKDE object (y)

# ______________________________________________________________________________

sample_akde <- function (x, y) {
  
  # transform relocations to UTM
  x.1 <- x %>% 
    
    st_as_sf(coords = c("lon", "lat"),
             crs = "epsg:4326") %>%
    
    st_transform(crs = "epsg:32611")
  
  # convert AKDE to raster
  y.rast <- y |> 
    
    raster(DF = "PDF") |>
    
    rast() |>
    
    project("epsg:32611")
  
  # extract 99% contour
  contour <- as.sf(y, level.UD = 0.99)[2, ] |>
    
    # convert to UTM
    st_transform(crs = "epsg:32611")
  
  # sample 100 background per 1 used
  background <- st_sample(
    
    contour,
    size = nrow(x.1) * 100,
    type = "regular",
    exact = T
    
  )
  
  # add to sf
  x.2 <- x.1 %>%
    
    bind_rows(
      
      st_as_sf(background) %>%
        
        rename(geometry = x) %>%
        
        mutate(
          
          timestamp = NA,
          site = x.1$site[1],
          sex = x.1$sex[1],
          MRID = x.1$MRID[1],
          year = x.1$year[1],
          season = x.1$season[1],
          trt = x.1$trt[1],
          track_season_post = x.1$track_season_post[1],
          case = 0
          
        )
      
    )
  
  # extract G(s) values
  x.2$akde <- extract(y.rast, x.2)$layer
  
  # change background G(s) to 0
  x.2$akde <- ifelse(x.2$case == 1, x.2$akde, 0)
  
  # return
  return(x.2)
  
}
  
# ______________________________________________________________________________
# 4b. Loop ----

tbl.gps.3 <- do.call(rbind, tbl.gps.2)

# ______________________________________________________________________________

all.samples <- data.frame()

for (i in unique(model.select.1$i)) {
  
  # subset correct track
  focal.track <- model.select.1$track_season_post[model.select.1$i == i]
  
  focal.relocs <- tbl.gps.3 %>%
    
    filter(track_season_post == focal.track)
  
  focal.akde <- all.akde[[i]]
  
  # use function
  focal.samples <- sample_akde(focal.relocs, focal.akde)
  
  # bind in 
  all.samples <- rbind(all.samples, focal.samples)
  
}

# ______________________________________________________________________________
# 5. Save to file ----
# ______________________________________________________________________________

all.samples.1 <- as.data.frame(all.samples) %>%
  
  mutate(
    
    x = st_coordinates(all.samples)[ , 1],
    y = st_coordinates(all.samples)[ , 2]
    
  ) %>%
  
  # drop geometry
  select(-geometry)

saveRDS(all.samples.1, "data_cleaned/use_background.rds")

# in the future, I'll add to the SQL db