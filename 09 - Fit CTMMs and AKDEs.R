# PROJECT: GPS data processing
# SCRIPT: 09 - Fit CTMMs and AKDEs
# AUTHOR: Nate Hooven
# EMAIL: nathan.d.hooven@gmail.com
# BEGAN: 31 Mar 2026
# COMPLETED: 31 Mar 2026
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

# error model
load("data_cleaned/error_model.RData")

# ______________________________________________________________________________
# 3. Select CTMMs ----

# to begin with, we'll consider each track_season_post for modeling

# days < to censor
censor.day = 5

# sampling schedule
samp.sched <- c(2, 12) %#% "hour"

# ______________________________________________________________________________
# 3a. Helper functions ----
# ______________________________________________________________________________

# formatting
format_ctmm <- function (x) {
  
  # convert to Movebank format
  x.mb <- data.frame("timestamp" = x$timestamp,
                     "location.lat" = x$lat,
                     "location.long" = x$lon,
                     "height above mean sea level" = x$elev,
                     "GPS satellite count" = x$satellites,
                     "GPS HDOP" = x$hdop)
  
  # convert to telemetry object
  suppressMessages(
    
    suppressWarnings(
    
    x.telem <- as.telemetry(object = x.mb,
                          timeformat = "auto",
                          timezone = "America/Los_Angeles",
                          keep = T)
    
   )
  
  )
  
  # add "class" variable for error model
  x.telem$class <- as.factor(ifelse(x.telem$GPS.satellite.count > 3,
                                    "3D",
                                    "2D"))
  
  # add in error model
  uere(x.telem) <- best.uere.HDOP.class
  
  # return
  return(x.telem)
  
}

# ______________________________________________________________________________
# 3b. Loop through ----
# ______________________________________________________________________________

# lists
all.model.select <- list()
all.top.model <- list()
all.top.model.summ <- list()

# start time
start.time <- Sys.time()

for (i in 1:length(unique(tbl.gps$track_season_post))) {
  
  # subset data
  focal.data <- tbl.gps %>% filter(track_season_post == unique(tbl.gps$track_season_post)[i]) %>%
    
    # drop days post collaring
    filter(days_cap > censor.day)
  
  # as.telemetry requires > 3 relocations
  if (nrow(focal.data) > 3) {
  
  # format 
  focal.data.1 <- format_ctmm(focal.data)
  
  # variogram
  focal.variog <- variogram(focal.data.1, dt = samp.sched, error = T)
  
  # guesstimated model parameters
  focal.param <- variogram.fit(focal.variog, 
                               name = "guess.param", 
                               interactive = F)
  
  # model selection
  suppressWarnings(focal.mods <- ctmm.select(focal.data.1, CTMM = focal.param, verbose = T))
  
  # model selection table
  all.model.select[[i]] <- cbind(
    
    as.data.frame(summary(focal.mods)), 
    data.frame("track_season_post" = focal.data$track_season_post[1]),
    mod = 1:nrow(summary(focal.mods)),
    i
    
    )
  
  # top model
  all.top.model[[i]] <- focal.mods[[1]]
  all.top.model.summ[[i]] <- summary(focal.mods[[1]])
  
  } else {
    
    all.model.select[[i]] <- NA
    all.top.model[[i]] <- NA
    all.top.model.summ[[i]] <- NA
    
  }
  
  # progress
  elapsed.time <- round(as.numeric(difftime(Sys.time(), 
                                            start.time, 
                                            units = "mins")), 
                        digits = 1)
  
  print(
    
    paste0("Completed track ", i, " / ", length(unique(tbl.gps$track_season_post)), 
           " - ", elapsed.time, " mins")
    
  )
  
}

# save selection summaries (critical for the next step!)
all.model.select.df <- data.frame()

for (i in 1:length(all.model.select)) {
  
  focal.select <- all.model.select[[i]]
  
  if (is.null(ncol(focal.select)) == F) {
    
    names(focal.select) <- c("dAICc", "dRMSPE", "DOF", "track_season_post", "mod", "i")
    
    # bind in
    all.model.select.df <- rbind(all.model.select.df, focal.select)
    
  }
  
}

saveRDS(all.model.select.df, "data_cleaned/all_model_select.rds")

# ______________________________________________________________________________
# 3c. Model selection ----
# ______________________________________________________________________________

# which are NA?
which.na.model.select <- which(is.na(all.model.select))

# ensure names are the same
change_names <- function (x) {
  
  if (length(names(x)) == 6) {
  
  names(x) <- c("dAICc", "dRMSPE", "DOF[area]", "track_season_post", "mod", "i")
  
  x$model <- rownames(x)
  
  return(x)
  
  } else {
    
    return(NA)
    
  }
  
}

all.model.select.1 <- lapply(all.model.select, change_names)

all.model.select.1.df <- do.call(rbind, all.model.select.1[-which.na.model.select])

# keep only the top model and count
all.model.select.1.df %>%
  
  filter(dAICc == 0) %>%
  
  group_by(model) %>%
  
  summarize(count = n())

# all are range-resident!

# ______________________________________________________________________________
# 4. Save top models ----
# ______________________________________________________________________________

saveRDS(all.top.model, "data_cleaned/top_models.rds")

# ______________________________________________________________________________
# 5. Fit AKDEs ----

# we will only output the AKDEs here
# later we can export rasters for visualization and use as a covariate (time marginalization)

# we can apply the Vander Wal and Rodgers method here to extract contours

# ______________________________________________________________________________
# 5a. Core area function ----

# this accepts an AKDE object

# ______________________________________________________________________________

hr_core <- function (x) {
  
  # define a reasonable range of isopleths
  isopleths <- seq(0.05, 0.95, by = 0.10)
  
  # full home range area
  full.hr.area <- as.sf(x, level.UD = 0.95)
  
  # loop through and bind into df
  all.df <- data.frame()
  
  for (i in isopleths) {
    
    # extract polygon
    focal.poly <- as.sf(x, level.UD = i)
    
    # generate df of areas
    focal.df <- data.frame(
      
      IV = i,
      
      level = c("low", "est", "high"),
      
      # area (in m2)
      A = c(as.numeric(st_area(focal.poly[1, ])),  # low
            as.numeric(st_area(focal.poly[2, ])),  # est
            as.numeric(st_area(focal.poly[3, ]))), 
      
      # percent area (of 95% area)
      PA = c(as.numeric(st_area(focal.poly[1, ]) / st_area(full.hr.area[1, ])),  # low
             as.numeric(st_area(focal.poly[2, ]) / st_area(full.hr.area[2, ])),  # est
             as.numeric(st_area(focal.poly[3, ]) / st_area(full.hr.area[3, ])))  # high
      
    ) 
    
    # bind into master df
    all.df <- rbind(all.df, focal.df)
    
  }
  
  # fit exponential regression
  # VWR define the regression equation as:
  
  # ln(PA) ~ ln(b0) + (b1 * IV)
  
  # when we fit this, the intercept is on the log scale and must be exponentiated
  
  est.df <- all.df %>% filter(level == "est")
  
  core.reg <- lm(log(PA) ~ IV, data = est.df)
  
  # calculate outer boundary of core area
  # VW-R take the first derivative of this function to yield:
  # core IV = -ln(b0 * b1) / b1
  core.IV = as.numeric(-log(exp(core.reg$coefficients[1]) * 
                              core.reg$coefficients[2]) / 
                         core.reg$coefficients[2])
  

  return(core.IV)
  
}

# ______________________________________________________________________________
# 5b. Loop ----
# ______________________________________________________________________________

# lists
all.akde <- list()
all.contours <- data.frame()

for (i in 1:length(unique(tbl.gps$track_season_post))) {
  
  # subset data
  focal.data <- tbl.gps %>% filter(track_season_post == unique(tbl.gps$track_season_post)[i]) %>%
    
    # drop days post collaring
    filter(days_cap > censor.day)
  
  # as.telemetry requires > 3 relocations
  if (nrow(focal.data) > 3) {
    
    # format 
    focal.data.1 <- format_ctmm(focal.data)
    
    # fit AKDE
    focal.akde <- akde(data = focal.data.1,
                       CTMM = all.top.model[[i]],
                       debias = T,
                       weights = T,
                       res = 10)
    
    # assign
    all.akde[[i]] <- focal.akde
    
    # convert to terra raster and project
    focal.rast <- project(
      
      rast(
        
        raster(focal.akde, DF = "PDF")
        
        ),
      
      "epsg:32611"
      
    )
    
    # 95% contour
    focal.95 <- st_transform(as.sf(focal.akde,
                                   level.UD = 0.95)[2, ],
                             "epsg:32611")
    
    # core area
    focal.core.IV <- hr_core(focal.akde)
    
    focal.core <- st_transform(as.sf(focal.akde,
                                     level.UD = focal.core.IV)[2, ],
                               "epsg:32611")
    
    # bind contours together
    # track info
    focal.track.info <- focal.data %>%
      
      dplyr::select(site,
                    sex,
                    MRID,
                    trackID,
                    year,
                    season,
                    trt) %>%
      
      slice(1)
    
    focal.contours <- rbind(focal.95, focal.core) %>%
      
      # drop name
      dplyr::select(-name) %>%
      
      # bind
      bind_cols(focal.track.info)
    
    # bind in
    all.contours <- rbind(all.contours, focal.contours)
    
  } else {
    
    all.akde[[i]] <- NA
    
  }
  
}

# ______________________________________________________________________________
# 5c. Write to file ----
# ______________________________________________________________________________

st_write(all.contours, "data_cleaned/spatial/all_contours.shp", append = F)

saveRDS(all.akde, "data_cleaned/all_akde.rds")

# ______________________________________________________________________________
# 5d. Visualize by site ----
# ______________________________________________________________________________

plot(st_geometry(all.contours %>% filter(site == "1A")))
plot(st_geometry(all.contours %>% filter(site == "1B")))
plot(st_geometry(all.contours %>% filter(site == "1C")))
plot(st_geometry(all.contours %>% filter(site == "2A")))
plot(st_geometry(all.contours %>% filter(site == "2B")))
plot(st_geometry(all.contours %>% filter(site == "2C")))
plot(st_geometry(all.contours %>% filter(site == "3A")))
plot(st_geometry(all.contours %>% filter(site == "3B")))
plot(st_geometry(all.contours %>% filter(site == "3C")))
plot(st_geometry(all.contours %>% filter(site == "4A")))
plot(st_geometry(all.contours %>% filter(site == "4B")))
plot(st_geometry(all.contours %>% filter(site == "4C")))
