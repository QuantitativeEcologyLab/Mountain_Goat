library(sf)
library(dplyr)
library(ctmm)
library(lubridate)



#............................................................
# Import Helicopter flight data ----
#............................................................


#Import a SINGLE helicopter data file to inspect data

#Inspect layers in the gpx file
st_layers("data/helicopter/2019_Helicopter_flight_data/2019-10-23T16-24-53Z.gpx")

#Import a single helicopter file using the "track_points" layer
heli <- st_read("data/helicopter/2019_Helicopter_flight_data/2019-10-23T16-24-53Z.gpx",
                layer = 'track_points', quiet = TRUE)





#............................................................
## Import 2019 helicopter data ----
#............................................................


#Import ALL 2019 helicopter files in the folder

#find and list files with names ending in ".gpx" in the folder
heli_2019_folder <- list.files("data/helicopter/2019_Helicopter_flight_data",
                               pattern = "\\.gpx$", full.names = TRUE)

#create an empty list to store the data in
heli_2019_list <- list()

#Import every file
for (file in heli_2019_folder) {
  gpx_data <- st_read(file, layer = 'track_points', quiet = TRUE)
  heli_2019_list[[file]] <- gpx_data
}

#Combine 2019 imported data into single dataframe
heli_2019 <- do.call(rbind, heli_2019_list)

#............................................................
## Import 2020 helicopter data ----
#............................................................

#Import a SINGLE 2020 helicopter data file to inspect data

#Inspect layers in the gpx file
st_layers("data/helicopter/2020_21_HNZ_flight_data/2020-10-01T15-41-09Z.gpx")

#Import a single helicopter file using the "track_points" layer
heli2 <- st_read("data/helicopter/2020_21_HNZ_flight_data/2020-10-01T15-41-09Z.gpx",
                 layer = 'track_points', quiet = TRUE)

#.....

#Import ALL 2020 helicopter files in the folder

#find and list files with names ending in ".gpx" in the folder
heli_2020_folder <- list.files("data/helicopter/2020_21_HNZ_flight_data",
                               pattern = "\\.gpx$", full.names = TRUE)

#create an empty list to store the data in
heli_2020_list <- list()

#Import every file
for (file in heli_2020_folder) {
  gpx_data <- st_read(file, layer = 'track_points', quiet = TRUE)
  heli_2020_list[[file]] <- gpx_data
}

#Combine 2020 imported data into single dataframe
heli_2020 <- do.call(rbind, heli_2020_list)

#............................................................
## Import 2021 helicopter data ----
#............................................................

#Import a SINGLE 2021 helicopter data file to inspect data

#Inspect layers in the gpx file
st_layers("data/helicopter/2021_HNZ_flight_data/2021-02-09T18-33-48Z.gpx")

#Import a single helicopter file using the "track_points" layer
heli3 <- st_read("data/helicopter/2021_HNZ_flight_data/2021-02-09T18-33-48Z.gpx",
                 layer = 'track_points', quiet = TRUE)

#.....

#Import ALL 2021 helicopter files in the folder

#find and list files with names ending in ".gpx" in the folder
heli_2021_folder <- list.files("data/helicopter/2021_HNZ_flight_data",
                               pattern = "\\.gpx$", full.names = TRUE)

#create an empty list to store the data in
heli_2021_list <- list()

#Import every file
for (file in heli_2021_folder) {
  gpx_data <- st_read(file, layer = 'track_points', quiet = TRUE)
  heli_2021_list[[file]] <- gpx_data
}

#Combine 2021 imported data into single dataframe
heli_2021 <- do.call(rbind, heli_2021_list)




# saveRDS(heli_2019, file = "data/helicopter/heli_2019.rds")
# saveRDS(heli_2020, file = "data/helicopter/heli_2020.rds")
# saveRDS(heli_2021, file = "data/helicopter/heli_2021.rds")
# heli_2019 <- readRDS("data/helicopter/heli_2019.rds")
# heli_2020 <- readRDS("data/helicopter/heli_2020.rds")
# heli_2021 <- readRDS("data/helicopter/heli_2021.rds")


#________________________________________________________________________

# Helicopter data carpentry ----


## 2019 Helicopter Data Prep ----


heli_2019 <- readRDS("data/helicopter/heli_2019.rds")
#Label individual flights
heli_2019$IDs <- 0
for(i in 1:nrow(heli_2019)){
  ith <- heli_2019$track_seg_point_id[i]
  if(ith == 0){ID <- length(unique(heli_2019$IDs))+1}
  heli_2019$IDs[i] <- ID
}
heli_2019$IDs <- heli_2019$IDs-1

heli_2019_dat <- data.frame(timestamp = heli_2019$time,
                            ID = heli_2019$IDs,
                            lat = st_coordinates(heli_2019)[,2],
                            long = st_coordinates(heli_2019)[,1],
                            HDOP = heli_2019$hdop)


#.........................................................
## 2020 Helicopter Data Prep ----

#import 2020
heli_2020 <- readRDS("data/helicopter/heli_2020.rds")

#Label individual flights
heli_2020$IDs <- 0
for(i in 1:nrow(heli_2020)){
  ith <- heli_2020$track_seg_point_id[i]
  if(ith == 0){ID <- length(unique(heli_2020$IDs))+1}
  heli_2020$IDs[i] <- ID
}
#Some label corrections
heli_2020$IDs <- heli_2020$IDs-1
heli_2020$IDs<- heli_2020$IDs + max(heli_2019$IDs)

heli_2020_dat <- data.frame(timestamp = heli_2020$time,
                            ID = heli_2020$IDs,
                            lat = st_coordinates(heli_2020)[,2],
                            long = st_coordinates(heli_2020)[,1],
                            HDOP = heli_2020$hdop)


#.........................................................
## 2021 Helicopter Data Prep ----

heli_2021 <- readRDS("data/helicopter/heli_2021.rds")
#Label individual flights
heli_2021$IDs <- 0
for(i in 1:nrow(heli_2021)){
  ith <- heli_2021$track_seg_point_id[i]
  if(ith == 0){ID <- length(unique(heli_2021$IDs))+1}
  heli_2021$IDs[i] <- ID
}
#Some label corrections
heli_2021$IDs <- heli_2021$IDs-1
heli_2021$IDs<- heli_2021$IDs + max(heli_2020$IDs)


heli_2021_dat <- data.frame(timestamp = heli_2021$time,
                            ID = heli_2021$IDs,
                            lat = st_coordinates(heli_2021)[,2],
                            long = st_coordinates(heli_2021)[,1],
                            HDOP = heli_2021$hdop)



#Join everything together
heli_gps <- rbind(heli_2019_dat, heli_2020_dat)
heli_gps <- rbind(heli_gps, heli_2021_dat)

#save(heli_gps, file = "data/helicopter/heli_gps.rda")
load("data/helicopter/heli_gps.rda")


#___________________________________________________________________

# Heli Data  ----

load("data/helicopter/heli_gps.rda")
heli_data <- heli_gps

#format timestamp
heli_data$timestamp <- as.POSIXct(heli_data$timestamp, format = "%Y-%m-%d %H:%M:%S")
#add a various time columns
heli_data$date <- format(heli_data$timestamp, "%Y-%m-%d")
heli_data$year <- format(heli_data$timestamp, "%Y")
heli_data$month <- month(heli_data$timestamp, label = FALSE) #label = false for numerical month
heli_data$day <- format(heli_data$timestamp, "%d")
heli_data$month_day <- format(heli_data$timestamp, "%m-%d")
heli_data$doy <- yday(heli_data$timestamp) #day of the year
heli_data$date <- as.Date(heli_data$date)




#....................................................................
# Define the seasons for all years ----

# MM-DD
# April 1 to June 8
spring_start <- "04-01"
spring_end <- "06-08"

# June 9 to Sept 30
summer_start <- "06-09"
summer_end <- "09-30"

# # Jan 1 to March 31
# winter_start <- "01-01"
# winter_end <-"03-31"

heli_data$season <- NA
heli_data$season[heli_data$month_day >= spring_start & heli_data$month_day <= spring_end] <- "spring"
heli_data$season[heli_data$month_day >= summer_start & heli_data$month_day <= summer_end] <- "summer"
# heli_data$season[heli_data$month_day >= winter_start & heli_data$month_day <= winter_end] <- "winter"


# save(heli_data, file = "data/helicopter/heli_data.rda")
#load("data/helicopter/heli_data.rda")




