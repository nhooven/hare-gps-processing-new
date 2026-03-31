# PROJECT: GPS data processing
# SCRIPT: 03 - Error model
# AUTHOR: Nate Hooven
# EMAIL: nathan.d.hooven@gmail.com
# BEGAN: 17 Jun 2023
# COMPLETED: 17 Jun 2023
# MODIFIED: 26 Mar 2026
# R VERSION: 4.4.3

# ______________________________________________________________________________
# 0. Purpose ----
# ______________________________________________________________________________

# this is the same error modeling approach as before
# we'll use stationary collar tests to fit an error model we can apply to all collar data

#_______________________________________________________________________________________________
# 1. Load required packages ----
#_______________________________________________________________________________________________

library(tidyverse)       # manipulate and clean data
library(lubridate)       # easily work with dates
library(sf)              # spatial data manipulation
library(amt)             # work with movement data
library(ctmm)            # error model

#_______________________________________________________________________________________________
# 2. Read in data ----

# due to the GPS csv format, must specify "" separator and fill all columns

#_______________________________________________________________________________________________

# directory
gps.dir <- "D:/hare_project/data_GPS/testdata_2022/"

test.data <- list()

test.data[[1]] <- read.csv(paste0(gps.dir, "22Y001_S2.csv"), sep = "", fill = TRUE)
test.data[[2]] <- read.csv(paste0(gps.dir, "22Y002_S1.csv"), sep = "", fill = TRUE)
test.data[[3]] <- read.csv(paste0(gps.dir, "22Y003_S1.csv"), sep = "", fill = TRUE)
test.data[[4]] <- read.csv(paste0(gps.dir, "22Y004_S1.csv"), sep = "", fill = TRUE)
test.data[[5]] <- read.csv(paste0(gps.dir, "22Y005_S1.csv"), sep = "", fill = TRUE)
test.data[[6]] <- read.csv(paste0(gps.dir, "22Y006_S1.csv"), sep = "", fill = TRUE)
test.data[[7]] <- read.csv(paste0(gps.dir, "22Y007_S2.csv"), sep = "", fill = TRUE)
test.data[[8]] <- read.csv(paste0(gps.dir, "22Y008_S1.csv"), sep = "", fill = TRUE)
test.data[[9]] <- read.csv(paste0(gps.dir, "22Y009_S1.csv"), sep = "", fill = TRUE)
test.data[[10]] <- read.csv(paste0(gps.dir, "22Y010_S1.csv"), sep = "", fill = TRUE)
test.data[[11]] <- read.csv(paste0(gps.dir, "22Y011_S1.csv"), sep = "", fill = TRUE)
test.data[[12]] <- read.csv(paste0(gps.dir, "22Y012_S1.csv"), sep = "", fill = TRUE)
test.data[[13]] <- read.csv(paste0(gps.dir, "22Y013_S1.csv"), sep = "", fill = TRUE)
test.data[[14]] <- read.csv(paste0(gps.dir, "22Y014_S1.csv"), sep = "", fill = TRUE)
test.data[[15]] <- read.csv(paste0(gps.dir, "22Y015_S2.csv"), sep = "", fill = TRUE)
test.data[[16]] <- read.csv(paste0(gps.dir, "22Y016_S1.csv"), sep = "", fill = TRUE)
test.data[[17]] <- read.csv(paste0(gps.dir, "22Y017_S1.csv"), sep = "", fill = TRUE)
test.data[[18]] <- read.csv(paste0(gps.dir, "22Y018_S1.csv"), sep = "", fill = TRUE)
test.data[[19]] <- read.csv(paste0(gps.dir, "22Y019_S1.csv"), sep = "", fill = TRUE)
test.data[[20]] <- read.csv(paste0(gps.dir, "22Y020_S2.csv"), sep = "", fill = TRUE)

#_______________________________________________________________________________________________
# 2. Bind together, keep only relocation rows, and columns we need ----
#_______________________________________________________________________________________________

# bind together
all.data <- do.call(rbind, test.data)

# keep only relocation rows
all.data <- all.data %>% 
  
  drop_na(hdop) %>%
  
  # select only columns we need
  dplyr::select(TagID, 
                Date, 
                Time, 
                location.lat, 
                location.lon, 
                height.msl,
                ground.speed, 
                satellites, 
                hdop, 
                signal.strength)

#_______________________________________________________________________________________________
# 3. Create telemetry objects for error modeling ----
#_______________________________________________________________________________________________

# create timestamp
all.data.1 <- all.data %>% 
  
  mutate(timestamp = dmy_hms(paste(Date, Time),
                             tz = "UTC"),
         ground.speed = as.numeric(ground.speed),
         satellites = as.integer(satellites)) %>%
  
  # select only columns we need
  dplyr::select(TagID, 
                timestamp, 
                location.lat, 
                location.lon, 
                height.msl,
                ground.speed, 
                satellites, 
                hdop, 
                signal.strength) %>%
  
  # remove anything with a > 0 ground speed and < 3 satellites
  filter(ground.speed == 0 &
           satellites > 2)

# make telemetry objects - loop through all and pack into list
all.telemetry <- list()

tag.list <- unique(all.data.1$TagID)

for (i in 1:length(tag.list)) {
  
  indiv.tag <- tag.list[i]
  
  # subset only one tag
  tag.data <- all.data.1 %>% filter(TagID == indiv.tag)
  
  # make a track
  tag.track <- make_track(tag.data, 
                          .x = location.lon, 
                          .y = location.lat, 
                          .t = timestamp,
                          crs = 4326,          # WGS84 lat/long
                          all_cols = TRUE)
  
  # rename columns for Movebank naming conventions
  names(tag.data) <- c("tag.local.identifier",
                       "timestamp",
                       "location.lat",
                       "location.long",
                       "height above mean sea level",
                       "ground speed",
                       "GPS satellite count",
                       "GPS HDOP",
                       "GPS maximum signal strength")
  
  # create a telemetry object
  tag.telem <- as.telemetry(object = tag.data,
                            timeformat = "auto",
                            timezone = "UTC",
                            keep = TRUE)
  
  # subset for model fitting
  tag.telem.1 <- tag.telem[ , c(1:5, 14, 16:18)]
  
  # change n satellites to "class" column
  tag.telem.1$class <- as.factor(ifelse(tag.telem.1$GPS.satellite.count > 3,
                                        "3D",
                                        "2D"))
  
  # build into list
  all.telemetry[[i]] <- tag.telem.1
  
}

#_______________________________________________________________________________________________
# 4. Plot to examine error ----

# create sf object
all.data.sf <- all.data.1 %>%
  
  st_as_sf(coords = c("location.lon",
                      "location.lat"),
           crs = "EPSG:4326") %>%
  
  st_transform(crs = "EPSG:32611")

#_______________________________________________________________________________________________
# 4a. By n satellites ----
#_______________________________________________________________________________________________

ggplot() +
  
  theme_bw() +
  
  # by satellites
  facet_wrap(~ satellites) +
  
  geom_sf(data = all.data.sf,
          aes(size = hdop,
              color = satellites),
          alpha = 0.35) +
  
  geom_point(data = data.frame(x = 304899.43,
                               y = 5403550.25),
             aes(x = x,
                 y = y),
             color = "red",
             shape = 21,
             size = 4) +
  
  coord_sf(datum = st_crs(32611),
           xlim = c(304860,
                    304940),
           ylim = c(5403505,
                    5403600)) +
  
  scale_color_viridis_c() +
  
  theme(axis.text = element_blank())

#_______________________________________________________________________________________________
# 4b. First 10 devices ----
#_______________________________________________________________________________________________

all.data.sf.1 <- all.data.sf %>%
  
  filter(TagID %in% unique(all.data.sf$TagID)[1:10])

ggplot() +
  
  theme_bw() +
  
  # by satellites
  facet_wrap(~ TagID) +
  
  geom_sf(data = all.data.sf.1,
          aes(size = hdop,
              color = satellites),
          alpha = 0.35) +
  
  geom_point(data = data.frame(x = 304899.43,
                               y = 5403550.25),
             aes(x = x,
                 y = y),
             color = "red",
             shape = 21,
             size = 4) +
  
  coord_sf(datum = st_crs(32611),
           xlim = c(304860,
                    304940),
           ylim = c(5403505,
                    5403600)) +
  
  scale_color_viridis_c() +
  
  theme(axis.text = element_blank())

#_______________________________________________________________________________________________
# 4c. Second 10 devices ----
#_______________________________________________________________________________________________

all.data.sf.2 <- all.data.sf %>%
  
  filter(TagID %in% unique(all.data.sf$TagID)[11:20])

ggplot() +
  
  theme_bw() +
  
  # by satellites
  facet_wrap(~ TagID) +
  
  geom_sf(data = all.data.sf.2,
          aes(size = hdop,
              color = satellites),
          alpha = 0.35) +
  
  geom_point(data = data.frame(x = 304899.43,
                               y = 5403550.25),
             aes(x = x,
                 y = y),
             color = "red",
             shape = 21,
             size = 4) +
  
  coord_sf(datum = st_crs(32611),
           xlim = c(304860,
                    304940),
           ylim = c(5403505,
                    5403600)) +
  
  scale_color_viridis_c() +
  
  theme(axis.text = element_blank())

#_______________________________________________________________________________________________
# 5. Fit error model ----
#_______________________________________________________________________________________________

# here, we'll fit four models (homoskedastic, HDOP, fix type ["class"], and HDOP + fix type)
# all other variable types have not been implemented in ctmm yet

# format datasets
t.none <- lapply(all.telemetry, function(t){ t$HDOP <- NULL; 
t$class <- NULL; t })

t.HDOP <- lapply(all.telemetry, function(t){ t$class <- NULL; t })

t.class <- lapply(all.telemetry, function(t){ t$HDOP <- NULL; t })

t.HDOP.class <- all.telemetry

# fit pooled models
uere.none <- uere.fit(t.none)
uere.HDOP <- uere.fit(t.HDOP)
uere.class <- uere.fit(t.class)
uere.HDOP.class <- uere.fit(t.HDOP.class)

summary(list("homoskedastic" = uere.none,
             "HDOP" = uere.HDOP,
             "fix type" = uere.class,
             "HDOP + fix type" = uere.HDOP.class))

# fit individual models
ueres.none <- lapply(t.none, uere.fit)
ueres.HDOP <- lapply(t.HDOP, uere.fit)
ueres.class <- lapply(t.class, uere.fit)
ueres.HDOP.class <- lapply(t.HDOP.class, uere.fit)

summary(list("joint - homoskedastic" = uere.none,
             "joint - HDOP" = uere.HDOP,
             "joint - fix type" = uere.class,
             "joint - HDOP + fix type" = uere.HDOP.class,
             "indiv - homoskedastic" = ueres.none,
             "indiv - HDOP" = ueres.HDOP,
             "indiv - fix type" = ueres.class,
             "indiv - HDOP + fix type" = ueres.HDOP.class))

# the best model here is the individual one that combines HDOP + fix type
# this means there is a ton of variability across tags
summary(ueres.HDOP)

# many of these have Z2 statistics within the 4-ish threshold reported by Fleming et al.
# I'll remove the poor-fitting collars (11 and 8) and re-fit
best.uere.none <- uere.fit(t.none[c(1:7, 9:10, 12:20)])
best.uere.HDOP <- uere.fit(t.HDOP[c(1:7, 9:10, 12:20)])
best.uere.class <- uere.fit(t.class[c(1:7, 9:10, 12:20)])
best.uere.HDOP.class <- uere.fit(t.HDOP.class[c(1:7, 9:10, 12:20)])

summary(list("homoskedastic" = best.uere.none,
             "HDOP" = best.uere.HDOP,
             "fix type" = best.uere.class,
             "HDOP + fix type" = best.uere.HDOP.class))

# by removing these two problematic collars, we dropped out Z^2 to 3.04
# and really reduced the estimated RMS error

summary(best.uere.HDOP.class)

# save to file
save(best.uere.HDOP.class, file = "data_cleaned/error_model.RData")

#_______________________________________________________________________________________________
# 6. Look at how well we did ----
#_______________________________________________________________________________________________

# assign RMS UERE to entire dataset
uere(all.telemetry[c(1:7, 9:10, 12:20)]) <- best.uere.HDOP.class

# calculate residuals of calibration data
RES <- lapply(all.telemetry[c(1:7, 9:10, 12:20)], residuals)

# scatter plot of residuals with 50%, 95%, and 99.9% coverage areas
plot(RES, col.DF=NA, level.UD=c(0.50,0.95,0.999))

# check calibration data for autocorrelation using fast=FALSE because samples are small
ACFS <- lapply(RES, function(R){correlogram(R,fast=FALSE,dt=10 %#% 'min',trace=FALSE)})

# pooling ACFs
ACF <- mean(ACFS)

plot(ACF)
