
library(lubridate)
# devtools::install_github("ctmm-initiative/ctmm", force = TRUE)
library(ctmm)
library(ggplot2)
library(gridExtra)


#____________________________________________________________
# Data import and pre-processing ----

#import the data
goat_gps <- read.csv("data/goat/tracking_data/goat-data-3_2024_01_11.csv")
calib <- read.csv("data/goat/tracking_data/goat-calibration-data_2024_01_11.csv")

#create and format various time columns
goat_gps$timestamp <- as.POSIXct(goat_gps$timestamp, format = "%Y-%m-%d %H:%M:%S")
goat_gps$date <- format(goat_gps$timestamp, "%Y-%m-%d")
goat_gps$year <- year(goat_gps$timestamp)
goat_gps$month <- month(goat_gps$timestamp, label = FALSE) #label = false for numerical month
goat_gps$day <- day(goat_gps$timestamp)
goat_gps$month_day <- format(goat_gps$timestamp, "%m-%d")
goat_gps$doy <- yday(goat_gps$timestamp) #day of the year

# goat_capture <- read.csv("data/goat/Cathedral goat capture database copy.csv")


#....................................................................
# Subset data to years of interest: 2019, 2020, 2021 ----
#....................................................................

#subset goat data that only occurred in 2019, 2020 and 2021
goat_data <- goat_gps[goat_gps$year %in% c("2019", "2020", "2021"),]


#....................................................................
# Remove pre-collaring fixes ----
#....................................................................

# 49001 recorded fixes

plot(x = goat_data$location.long, y = goat_data$location.lat)

# Identify fixes that occurred prior to collaring (collaring started June 24, 2019)
#59 fixes
test <- goat_data[goat_data$date <= "2019-06-23",]
# Subset dataset to only contain collared fixes and remove fixes that occurred pre-collaring
goat_data <- goat_data[!(goat_data$date <= "2019-06-23"),]

# Mortality of goat 3 aug 24, 2019, collar-id was reused

test <- goat_data[goat_data$date <= "2019-08-25" & goat_data$animal == "30636",]

# Identify fixes that occurred pre-collaring for collaring of a new goat using old collar from previously killed goat (re-collared new goat July 21, 2020)
# 12 fixes
test2 <- goat_data[goat_data$location.lat >= 49.4,]
# Subset dataset to only contain collared fixes and remove fixes that occurred pre-collaring
goat_data <- goat_data[!(goat_data$location.lat >= 49.4),]

# Identify any post-mortality fixes for goat 30636 (mortality March 16, 2021)
# 0 fixes
test3 <- goat_data[goat_data$date >= "2021-03-17" & goat_data$animal == "30636",]



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

goat_data$season <- NA
goat_data$season[goat_data$month_day >= spring_start & goat_data$month_day <= spring_end] <- "spring"
goat_data$season[goat_data$month_day >= summer_start & goat_data$month_day <= summer_end] <- "summer"
# goat_data$season[goat_data$month_day >= winter_start & goat_data$month_day <= winter_end] <- "winter"

goat_data$individual.local.identifier <- paste(goat_data$season, goat_data$year, goat_data$individual.local.identifier, sep = "_")


#......................................................


# # Add a column for the anthropause period
# goat_data$anthro <- as.character(goat_data$year)
# goat_data[which(goat_data$anthro == "2019"),"anthro"] <- "normal"
# goat_data[which(goat_data$anthro == "2020"),"anthro"] <- "anthropause"
# goat_data[which(goat_data$anthro == "2021"),"anthro"] <- "normal"
# goat_data$anthro <- as.factor(goat_data$anthro)
# str(goat_data)




#....................................................................

#Convert to telemetry
goats <- as.telemetry(goat_data, mark.rm = TRUE)

#Calibrate measurement error
CALIBRATION <- as.telemetry(calib, mark.rm = TRUE)
UERE <- uere.fit(CALIBRATION)
uere(goats) <- UERE


save(goat_data, file = "data/goat/goat_data.rda")
save(goats,file="data/goat/goats_telemetry.rda")
write.csv(goat_data, "data/goat/goat_data_2019-2021.csv", row.names = FALSE)

load("data/goat/goat_data.rda")
# load("data/goat/goats_telemetry.rda")


#....................................................................

# Check the number of individuals per year
unique(goat_data$animal[goat_data$year == "2019"]) #10
unique(goat_data$animal[goat_data$year == "2020"]) #10
unique(goat_data$animal[goat_data$year == "2021"]) #10
unique(goat_gps$animal[goat_gps$year == "2022"]) #8
unique(goat_gps$animal[goat_gps$year == "2023"]) #7


goat_30636 <- goat_data[goat_data$animal == "30636" & goat_data$year == "2021",]
# March 16, 2021 last day of tracking


#....................................................................
# Visualize goat activity ----
#....................................................................

# Inspect goat data, sampling frequency

#subset based on year to plot
goat_2019 <- goat_data[goat_data$year == "2019",]
goat_2020 <- goat_data[goat_data$year == "2020",]
goat_2021 <- goat_data[goat_data$year == "2021",]


plot_annual_goat_2019 <-
  ggplot(data = goat_2019,
         aes(y = animal, x = doy)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_x_continuous(limits = c(-2, 370), expand = c(0, 1),
                     breaks = seq(0, 365, by = 30),
                     labels = c(month.abb, month.abb[1])) +  # Use month abbreviations
  ggtitle("Goat Activity") + 
  xlab("Month") +
  ylab("2019") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14, family = "sans", face = "bold", hjust = 0.5),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        legend.position="none",
        legend.key = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))



plot_annual_goat_2020 <-
  ggplot(data = goat_2020,
         aes(y = animal, x = doy)) +
  geom_bar(stat = "identity", position = "stack") + 
  scale_x_continuous(limits = c(-2, 370), expand = c(0, 1),
                     breaks = seq(0, 365, by = 30),
                     labels = c(month.abb, month.abb[1])) +  # Use month abbreviations
  xlab("Month") +
  ylab("2020") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text( size = 14, family = "sans", face = "bold"),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        legend.position="none",
        legend.key = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))


plot_annual_goat_2021 <-
  ggplot(data = goat_2021,
         aes(y = animal, x = doy)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_x_continuous(limits = c(-2, 370), expand = c(0, 1),
                     breaks = seq(0, 365, by = 30),
                     labels = c(month.abb, month.abb[1])) +  # Use month abbreviations
  xlab("Month") +
  ylab("2021") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14, family = "sans", face = "bold", hjust = 0.5),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        legend.position="none",
        legend.key = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))


plot_goat_activity <- grid.arrange(plot_annual_goat_2019,
                                   plot_annual_goat_2020,
                                   plot_annual_goat_2021,
                                   ncol = 1)

ggsave(plot_goat_activity, filename = "figures/annual_goat_activity_2019-2021.png", device = NULL,
       path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)


#............................................................
# Visualize goat data ----
#............................................................

# park boundary as a sf object
library(sf)

# Import Provincial park polygons
bc_parks <- st_read("data/habitat/bc_provincial_parks/TA_PARK_ECORES_PA_SVW/TA_PEP_SVW_polygon.shp")
# Convert CRS to lat/long projection
bc_parks <- st_transform(bc_parks, crs = st_crs(4326))
st_crs(bc_parks)
#subset to cathedral park
cathedral <- bc_parks[bc_parks$PROT_NAME == "CATHEDRAL PARK", ] #row 95


# cathedral park boundary
ggplot() +
  geom_sf(data = cathedral, fill = NA, color = "black", size = 1)


# goat data
plot_goat_data <-
ggplot() +
  geom_point(data = goat_data, aes(x = location.long, y = location.lat)) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14, family = "sans", face = "bold", hjust = 0.5),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        legend.position="none",
        legend.key = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggsave(plot_goat_data, filename = "figures/goat_data_2019-2021.png", device = NULL,
       path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)


plot_goat_park <- 
ggplot() +
  geom_sf(data = cathedral, fill = NA, color = "black", size = 1) +
  geom_point(data = goat_data, aes(x = location.long, y = location.lat)) +  # Add goat data points
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14, family = "sans", face = "bold", hjust = 0.5),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        legend.position="none",
        legend.key = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))


ggsave(plot_goat_park, filename = "figures/goat_park_2019-2021.png", device = NULL,
       path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)
