library(sf)
library(ggplot2)
library(ctmm)
library(lubridate)
library(gridExtra)
library(cowplot)

load("data/helicopter/heli_data.rda")


#.................................................................
# Figure of helicopter tracks ----
#...............................................................

#Convert to individual telemetry objects and & identify how many recorded flights
# Note: if there is an 'individual.local.identifier' column, you will run into problems, (related to the ctmm package and its syntaxs)
tracks_2019 <- as.telemetry(heli_data[heli_data$year == "2019",]) #42 flights
projection(tracks_2019) <- median(tracks_2019)
tracks_2020 <- as.telemetry(heli_data[heli_data$year == "2020",]) #93 flights
projection(tracks_2019) <- median(tracks_2019)
tracks_2021 <- as.telemetry(heli_data[heli_data$year == "2021",]) #59 flights
projection(tracks_2021) <- median(tracks_2019)


# # Plot helicopter tracks ----
# 
# png(file="figures/helicopter/heli_tracks.png",
#     width=3*1.5, height=6*1.5, units="in", res=600)
# par(mfrow = c(3,1))
# 
# plot(tracks_2019, col = viridis::viridis(length(tracks_2019)))
# title("a)", adj = 0)
# mtext("2019", side = 3, line = 1, adj = 0.5)
# 
# plot(tracks_2020, col = viridis::viridis(length(tracks_2020)))
# title("b)", adj = 0)
# mtext("2020", side = 3, line = 1, adj = 0.5)
# 
# plot(tracks_2021, col = viridis::viridis(length(tracks_2021)))
# title("c)", adj = 0)
# mtext("2021", side = 3, line = 1, adj = 0.5)
# 
# dev.off()


#..................................................................
# Cathedral park ----
#..................................................................


# Import Provincial park polygons
bc_parks <- st_read("data/habitat/bc_provincial_parks/TA_PARK_ECORES_PA_SVW/TA_PEP_SVW_polygon.shp")
# Convert CRS to lat/long projection
bc_parks <- st_transform(bc_parks, crs = st_crs(4326))
st_crs(bc_parks)
#subset to cathedral park
cathedral <- bc_parks[bc_parks$PROT_NAME == "CATHEDRAL PARK", ] #row 95


#..................................................................
# annual helicopter tracks ----
#..................................................................


h2019 <- heli_data[heli_data$year == "2019",]
h2020 <- heli_data[heli_data$year == "2020",]
h2021 <- heli_data[heli_data$year == "2021",]

# facet_wrap(~ year, strip.position = "left",
#   nrow = 3) + # create a panel for each year

plot_tracks2019 <-
  ggplot(data = h2019) + 
  geom_point(aes(x = long, y = lat, color = ID), size = 0.01) +
  scale_color_viridis_c() +
  ggtitle("a)") +
  labs(tag = "2019") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(vjust = -8,size = 8, family = "sans"),
        plot.title.position = "plot",
        plot.tag.position = c(-0.08,0.53),
        plot.tag = element_text(angle = 90, size = 10, family = "sans", face = "bold"),
        axis.title.y = element_text(size=8, family = "sans"),
        axis.title.x = element_text(size=8, family = "sans"),
        axis.text.y = element_text(size=6, family = "sans"),
        axis.text.x  = element_text(size=6, family = "sans"),
        legend.position="none",
        legend.key = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0,0.1,0,1), "cm"))

# ggsave(last_plot(), file = "figures/helicopter/test.png", width = 6.86, height = 6, units = "in", dpi = 600, bg = "transparent",)

plot_tracks2020 <-
  ggplot(data = h2020) + 
  geom_point(aes(x = long, y = lat, color = ID), size = 0.01) +
  scale_color_viridis_c() +
  ggtitle("c)") +
  labs(tag = "2020") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(vjust = -8, size = 8, family = "sans"),
        plot.title.position = "plot",
        plot.tag.position = c(-0.08,0.53),
        plot.tag = element_text(angle = 90, size = 10, family = "sans", face = "bold"),
        axis.title.y = element_text(size=8, family = "sans"),
        axis.title.x = element_text(size=8, family = "sans"),
        axis.text.y = element_text(size=6, family = "sans"),
        axis.text.x  = element_text(size=6, family = "sans"),
        legend.position="none",
        legend.key = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0,0.1,0,1), "cm"))


plot_tracks2021 <-
  ggplot(data = h2021) + 
  geom_point(aes(x = long, y = lat, color = ID), size = 0.01) +
  scale_color_viridis_c() +
  ggtitle("e)") +
  labs(tag = "2021") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(vjust = -8, size = 8, family = "sans"),
        plot.title.position = "plot",
        plot.tag.position = c(-0.08,0.53),
        plot.tag = element_text(angle = 90, size = 10, family = "sans", face = "bold"),
        axis.title.y = element_text(size=8, family = "sans"),
        axis.title.x = element_text(size=8, family = "sans"),
        axis.text.y = element_text(size=6, family = "sans"),
        axis.text.x  = element_text(size=6, family = "sans"),
        legend.position="none",
        legend.key = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0,0.1,0,1), "cm"))

#.................................................................
# Multi-panel ----
#.................................................................

plot_tracks <- grid.arrange(plot_tracks2019, plot_tracks2020, plot_tracks2021,
                            ncol=1)

# ggsave(plot_tracks, width = 3.23, height = 6, units = "in", dpi = 600, bg = "transparent",
#        file="figures/helicopter/annual_flight_tracks.png")


#.............................................

# # Aggregate by date and flight ID to count the number of flights and when they occurred
# flights <- aggregate(date ~ ID + date, data = heli_data, FUN = unique)
# flights$year <- year(flights$date)
# 
# 
# #subset based on year to plot
# flights_2019 <- flights[flights$year == "2019",]
# flights_2020 <- flights[flights$year == "2020",]
# flights_2021 <- flights[flights$year == "2021",]
# 
# f2019 <- as.data.frame(table(flights_2019$date))
# names(f2019)[1] <- "date"
# f2019$date <- as.Date(f2019$date)
# f2019$doy <- yday(f2019$date)
# 
# f2020 <- as.data.frame(table(flights_2020$date))
# names(f2020)[1] <- "date"
# f2020$date <- as.Date(f2020$date)
# f2020$doy <- yday(f2020$date)
# 
# f2021 <- as.data.frame(table(flights_2021$date))
# names(f2021)[1] <- "date"
# f2021$date <- as.Date(f2021$date)
# f2021$doy <- yday(f2021$date)


#.................................................................
# Figure flight frequency per year ----
#...............................................................



plot_flight_2019 <-
  ggplot(data = f2019,
         aes(y = Freq, x = doy)) +
  geom_bar(stat = "identity", position = "stack", fill = "black") +
  scale_x_continuous(limits = c(-2, 340), expand = c(0, 1), # so the plot extends a bit after december
                     breaks = seq(0, 365, by = 30),
                     labels = c(month.abb, "")) +  # Use month abbreviations
  xlab("") +
  ylab("Flights per day") +
  ggtitle("b)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(vjust = -8, size = 8, family = "sans"),
        plot.title.position = "plot",
        axis.title.y = element_text(size=8, family = "sans"),
        axis.title.x = element_text(size=8, family = "sans", face = "bold"),
        axis.text.y = element_text(size=6, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        legend.position="none",
        legend.key = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        panel.background = element_rect(scales::alpha("#005f73", 0.3)),
        plot.background = element_rect(size = 1, fill = "transparent", color = NA),
        plot.margin = unit(c(0,0.1,0,0.3), "cm"))




plot_flight_2020 <-
  ggplot(data = f2020,
         aes(y = Freq, x = doy)) +
  geom_bar(stat = "identity", position = "stack", fill = "black") +
  scale_x_continuous(limits = c(-2, 340), expand = c(0, 1), # so the plot extends a bit after december
                     breaks = seq(0, 365, by = 30),
                     labels = c(month.abb, "")) +  # Use month abbreviations
  xlab("") +
  ylab("Flights per day") +
  ggtitle("d)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(vjust = -8, size = 8, family = "sans"),
        plot.title.position = "plot",
        axis.title.y = element_text(size=8, family = "sans"),
        axis.title.x = element_text(size=8, family = "sans", face = "bold"),
        axis.text.y = element_text(size=6, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        legend.position="none",
        legend.key = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        panel.background = element_rect(scales::alpha("#ae2012", 0.3)),
        plot.background = element_rect(size = 1, fill = "transparent", color = NA),
        plot.margin = unit(c(0,0.1,0,0.3), "cm"))


plot_flight_2021 <-
  ggplot(data = f2021,
         aes(y = Freq, x = doy)) +
  geom_bar(stat = "identity", position = "stack", fill = "black") +
  scale_x_continuous(limits = c(-2, 340), expand = c(0, 1), # so the plot extends a bit after december
                     breaks = seq(0, 365, by = 30),
                     labels = c(month.abb, "")) +  # Use month abbreviations
  ylab("Flights per day") +
  xlab("Month") +
  ggtitle("f)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(vjust = -8, size = 8, family = "sans"),
        plot.title.position = "plot",
        axis.title.y = element_text(size=8, family = "sans"),
        axis.title.x = element_text(size=8, family = "sans", face = "bold"),
        axis.text.y = element_text(size=6, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        legend.position="none",
        legend.key = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        panel.background = element_rect(scales::alpha("#0a9396", 0.3)),
        plot.background = element_rect(size = 1, fill = "transparent", color = NA),
        plot.margin = unit(c(0,0.1,0,0.3), "cm"))

#.................................................................
# Multi-panel ----
#.................................................................

annual_heli_flights_per_day <- grid.arrange(plot_flight_2019,
                                            plot_flight_2020,
                                            plot_flight_2021,
                                            ncol = 1)


# annual_heli_flights_per_day
# ggsave(annual_heli_flights_per_day, width = 6.86, height = 6, units = "in", dpi = 600, bg = "transparent",
#        file="figures/helicopter/annual_heli_flights_per_day.png")


#.................................................................
# Multi-panel combine all ----
#.................................................................

heli_flights <- plot_grid(plot_tracks, annual_heli_flights_per_day,
                          ncol=2,
                          rel_widths = c(1,2))
heli_flights

ggsave(heli_flights, width = 6.86, height = 6, units = "in", dpi = 600, bg = "transparent",
       file="figures/helicopter/heli_flights.png")






#.................................................................
# Figure based on ID flight data points frequency per year ----
#...............................................................


#subset based on year to plot
flight_2019 <- heli_data[heli_data$year == "2019",]
flight_2020 <- heli_data[heli_data$year == "2020",]
flight_2021 <- heli_data[heli_data$year == "2021",]


annual_heli_2019 <-
  ggplot(data = flight_2019,
         aes(y = ID, x = doy)) +
  geom_bar(stat = "identity", position = "stack", fill = "black") +
  scale_x_continuous(limits = c(-2, 340), expand = c(0, 1), # so the plot extends a bit after december
                     breaks = seq(0, 365, by = 30),
                     labels = c(month.abb, "")) +  # Use month abbreviations
  scale_y_continuous(labels = function(x) sprintf("%.1e", x)) + # adjust scientific notation to contain a decimal point
  ggtitle("Helicopter Activity") + 
  xlab("") +
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
        panel.background = element_rect(scales::alpha("#005f73", 0.3)),
        plot.background = element_rect(size = 1, fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))




annual_heli_2020 <-
  ggplot(data = flight_2020,
         aes(y = ID, x = doy)) +
  geom_bar(stat = "identity", position = "stack", fill = "black") +
  scale_x_continuous(limits = c(-2, 340), expand = c(0, 1), # so the plot extends a bit after december
                     breaks = seq(0, 365, by = 30),
                     labels = c(month.abb, "")) +  # Use month abbreviations
  scale_y_continuous(labels = scales::scientific) + #put in scientific notation
  xlab("") +
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
        panel.background = element_rect(scales::alpha("#ae2012", 0.3)),
        plot.background = element_rect(size = 1, fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))


annual_heli_2021 <-
  ggplot(data = flight_2021,
         aes(y = ID, x = doy)) +
  geom_bar(stat = "identity", position = "stack", fill = "black") +
  scale_x_continuous(limits = c(-2, 340), expand = c(0, 1), # so the plot extends a bit after december
                     breaks = seq(0, 365, by = 30),
                     labels = c(month.abb, "")) +  # Use month abbreviations
  scale_y_continuous(labels = scales::scientific) +
  xlab("Month") +
  ylab("2021") +
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
        panel.background = element_rect(scales::alpha("#0a9396", 0.3)),
        plot.background = element_rect(size = 1, fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))




annual_flight_plot <- grid.arrange(annual_heli_2019,
                                   annual_heli_2020,
                                   annual_heli_2021,
                                   ncol = 1)


annual_flight_plot
ggsave(annual_flight_plot, width = 6.86, height = 6, units = "in", dpi = 600, bg = "transparent",
       file="figures/helicopter/annual_heli_activity.png")







#...............................................................
# Figure of Helicopter activity based on Flight ID ----
#...............................................................

plot_2019 <-
  ggplot(data = flight_2019,
         aes(y = ID, x = doy)) +
  geom_point() + 
  scale_x_continuous(limits = c(-2, 370), expand = c(0, 1),
                     breaks = seq(0, 365, by = 30),
                     labels = c(month.abb, month.abb[1])) +
  xlab("Month") +
  ylab("Flight ID") +
  ggtitle("2019") +
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



plot_2020 <-
  ggplot(data = flight_2020,
         aes(y = ID, x = doy)) +
  geom_point() + 
  scale_x_continuous(limits = c(-2, 370), expand = c(0, 1),
                     breaks = seq(0, 365, by = 30),
                     labels = c(month.abb, month.abb[1])) +
  xlab("Month") +
  ylab("Flight ID") +
  ggtitle("2020") +
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



plot_2021 <-
  ggplot(data = flight_2021,
         aes(y = ID, x = doy)) +
  geom_point() + 
  scale_x_continuous(limits = c(-2, 370), expand = c(0, 1),
                     breaks = seq(0, 365, by = 30),
                     labels = c(month.abb, month.abb[1])) + 
  xlab("Month") +
  ylab("Flight ID") +
  ggtitle("2021") +
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



flight_plot <- grid.arrange(plot_2019,
                            plot_2020,
                            plot_2021,
                            ncol = 1)
flight_plot
ggsave(flight_plot, filename = "figures/helicopter/flight_id_activity.png", device = NULL,
       path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)

