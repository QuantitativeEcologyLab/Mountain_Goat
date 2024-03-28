
library(ctmm)
library(raster)
library(lme4)
library(dplyr)
library(ggplot2)
library(gridExtra)

# Load data ----
load("data/goat/goats_telemetry2.rda") #goat data (telemetry)
load("data/goat_fits2.rda")
load("data/goat_akdes2.rda")
load("data/home_range/dat_hr2.rda")

# Currently in BC Albers crs
elev = raster('data/habitat/rasters/elev_25m.tif')
#slope = raster('data/habitat/rasters/slope_25m.tif')
#escape = raster('data/habitat/rasters/escape_25m.tif')
dis_escape = raster('data/habitat/rasters/dist_escape_25m.tif')


#scale/center goat-habitat covariate raster data via terra pkg
#scl.elev = scale(elev)
#scl.slope = scale(slope)
#scl.escape = scale(escape)
#scl.dis_escape = scale(dis_escape)


# raster covariates must be in a named list
R <- list(elevation = elev, 
          dis_escape = dis_escape)





#....................................................................
# RSF Model ----
#....................................................................

# #test for a single goat
# i <- 1
# DATA <- goats[[i]]
# AKDE <- AKDES[[i]]
# rsf <- rsf.fit(DATA,AKDE,R=R)

START <- Sys.time()

# Fit RSF models (needs to be raster object and not spatraster)

rsf <- list()
for(i in 1:length(goats2)){
  #Extract individual
  DATA <- goats2[[i]]
  AKDE <- AKDES2[[i]]
  
  # Fit rsf
  rsf[[i]] <- rsf.fit(DATA,AKDE, R=R)
}
names(rsf) <- names(goats2)


#rsf_results = do.call(rbind, RSF)
save(rsf, file = "data/rsf/rsf.rda")


END <- Sys.time()

#_______________________________________________________________________
#09.ctmm.RSF.r


load("data/rsf/rsf.rda")

# Extract rsf coefficients ----
rsf_df <- list()

#check field names
summary(rsf[[1]])

for(i in 1:length(rsf)){
  # test loop for one iteration
  # i = 1
  # summary(rsf[[i]])
  
  #create and transpose (flip the rows/columns) a dataframe
  elev <- data.frame(t(summary(rsf[[i]])$CI["elevation (1/elevation)",]))
  elev_cov = rsf[[i]]$COV['elevation','elevation']
  dis_escape <- data.frame(t(summary(rsf[[i]])$CI["dis_escape (1/dis_escape)",])) 
  dis_escape_cov = rsf[[i]]$COV['dis_escape','dis_escape']

  c(elev, elev_cov, dis_escape, dis_escape_cov)
  
  res <- cbind(elev, elev_cov, dis_escape, dis_escape_cov)
  # rename the columns
  names(res) <- c("elev_low","elev_est","elev_high", "elev_cov",
                  "dis_escape_low","dis_escape_est","dis_escape_high", "dis_escape_cov")
  
  res$individual.local.identifier <- rsf[[i]]@info$identity
  
  rsf_df[[i]] <- res
}

rsf_df <- do.call(rbind,rsf_df)

# searches for string of text and extract information from 'individual.local.identifier' column and puts a string of text into a new column based on those conditions
rsf_df$year <- NA
rsf_df[grepl("2019", rsf_df$individual.local.identifier),"year"] <- "2019"
rsf_df[grepl("2020", rsf_df$individual.local.identifier),"year"] <- "2020"
rsf_df[grepl("2021", rsf_df$individual.local.identifier),"year"] <- "2021"
rsf_df$season <- NA
rsf_df[grepl("spring", rsf_df$individual.local.identifier),"season"] <- "spring"
rsf_df[grepl("summer", rsf_df$individual.local.identifier),"season"] <- "summer"
#extract text after the second _ underscore (i.e. collar_id)
rsf_df$collar_id <- sub("^(?:[^_]*_){2}(.*)", "\\1", rsf_df$individual.local.identifier)
#reorganize columns
rsf_df <- relocate(rsf_df, c(individual.local.identifier, collar_id, season, year), .before = elev_low)

# write.csv(rsf_df, "data/rsf/rsf_coefficients.csv", row.names = FALSE)
rsf_df <- read.csv("data/rsf/rsf_coefficients.csv")

# Values near 0 = no preference
# Values below 0 = selected for, to be closer to escape terrain
# values above 0 = selected against


#..................................................

# Calculate the mean of rsf coefficients 


# #spring
# mean(rsf[28:35]) #2020
# mean(rsf[36:44]) #2021
# #summer
# mean(rsf[45:53]) #2019
# mean(rsf[54:63]) #2020
# mean(rsf[64:72]) #2021

#............................................................
# RSF per season for each year

rsf_spring_2020 <- subset(rsf_df, year == "2020" & season == "spring")
rsf_spring_2021 <- subset(rsf_df, year == "2021" & season == "spring")
rsf_summer_2019 <- subset(rsf_df, year == "2019" & season == "summer")
rsf_summer_2020 <- subset(rsf_df, year == "2020" & season == "summer")
rsf_summer_2021 <- subset(rsf_df, year == "2021" & season == "summer")

#___________________________________________________________
# Calculate Mean RSF ----

# Create dataframe to store rsf mean values
mean_rsf_df <-  data.frame(season_year = c("rsf_spring_2020",
                                           "rsf_spring_2021",
                                           "rsf_summer_2019",
                                           "rsf_summer_2020",
                                           "rsf_summer_2021"))
# calculate mean values for each season and year
means1 <- colMeans(rsf_spring_2020[, 5:12], na.rm = TRUE)
means2 <- colMeans(rsf_spring_2021[, 5:12], na.rm = TRUE)
means3 <- colMeans(rsf_summer_2019[, 5:12], na.rm = TRUE)
means4 <- colMeans(rsf_summer_2020[, 5:12], na.rm = TRUE)
means5 <- colMeans(rsf_summer_2021[, 5:12], na.rm = TRUE)

means <- rbind(means1,
               means2,
               means3,
               means4,
               means5)

mean_rsf_df <- cbind(mean_rsf_df, means)

#extract text after the first _ underscore and before the second _ underscore (i.e. season)
mean_rsf_df$season <- sub("^[^_]*_(.*?)_.*", "\\1", mean_rsf_df$season_year)
#extract text after the second _ underscore (i.e. year)
mean_rsf_df$year <- sub("^(?:[^_]*_){2}(.*)", "\\1", mean_rsf_df$season_year)
mean_rsf_df <- relocate(mean_rsf_df, c(season, year), .after = season_year)

# write.csv(mean_rsf_df, "data/rsf/mean_rsf_coefficients.csv", row.names = FALSE)
mean_rsf_df <- read.csv("data/rsf/mean_rsf_coefficients.csv")


#..................................................
# Plot RSF spring elevation ----
#..................................................

# Plot elevation for spring across all years

rsf_df$year <- as.factor(rsf_df$year)
rsf_df$collar_id <- as.factor(rsf_df$collar_id)

pd = position_dodge(width = 0.5)

plot_rsf_elevation_spring <-
#drop NA values (i.e. winter season)
ggplot(data = na.omit(rsf_df[rsf_df$season == "spring",])) + 
  geom_hline(yintercept = 0, col = "grey70", linetype = "dashed") +
  geom_pointrange(aes(x = collar_id, y = elev_est, ymin = elev_low, ymax = elev_high, 
                      color = year), 
                  size = 0.5, position = pd) +
  ylab("Elevation") +
  ggtitle("a)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = -0.05, size = 10, family = "sans"),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_blank(), 
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        legend.position = c(0.5, 1.05), #horizontal, vertical
        legend.justification = "center",
        legend.direction = "horizontal",
        legend.title = element_blank(), 
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA)) +
  scale_colour_manual(values = c("#005f73", "#ae2012", "#0a9396"), 
                      breaks = c('2019', '2020', '2021'))
  

# ggsave(plot_rsf_elevation_spring,
#        file="figures/individual_plots/rsf_elevation_spring.png",
#        width = 6.86, height = 3, units = "in", dpi = 600, bg = "transparent")




#..........................................................
# Plot RSF spring distance to escape ----
#..........................................................


# Plot distance to escape for spring across all years

plot_rsf_dis_escape_spring <-
ggplot(data = na.omit(rsf_df[rsf_df$season == "spring",])) + 
  geom_hline(yintercept = 0, col = "grey70", linetype = "dashed") +
  geom_pointrange(aes(x = collar_id, y = dis_escape_est, ymin = dis_escape_low, ymax = dis_escape_high, color = year), 
                  size = 0.5, position = pd) +
  ylab("Distance to Escape") +
  ggtitle("b)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = -0.05, size = 10, family = "sans"),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_blank(), 
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        legend.position="none",
        legend.title = element_blank(), 
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA)) +
  scale_colour_manual(values = c("#005f73", "#ae2012", "#0a9396"), 
                      breaks = c('2019', '2020', '2021'))


# ggsave(plot_rsf_elevation_spring,
#        file="figures/individual_plots/rsf_dis_escape_spring.png",
#        width = 6.86, height = 3, units = "in", dpi = 600, bg = "transparent")



#............................................
# Multi-panel RSF spring ----
#............................................

plot_rsf_spring <- grid.arrange(plot_rsf_elevation_spring, plot_rsf_dis_escape_spring,
                                ncol = 1)

ggsave(plot_rsf_spring,
       file="figures/rsf/rsf_spring.png",
       width = 6.86, height = 6, units = "in", dpi = 600, bg = "transparent")




#..................................................
# Plot RSF summer elevation (all years) ----
#..................................................


# Plot elevation for summer across all years

rsf_elevation_summer <-
  ggplot(data = na.omit(rsf_df[rsf_df$season == "summer",])) +
  geom_hline(yintercept = 0, col = "grey70", linetype = "dashed") +
  geom_pointrange(aes(x = collar_id, y = elev_est, ymin = elev_low, ymax = elev_high, color = year), 
                  size = 0.5, position = pd) +
  ylab("Elevation") +
  ggtitle("a)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = -0.05, size = 10, family = "sans"),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_blank(), 
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        legend.position = c(0.5, 1.05), #horizontal, vertical
        legend.justification = "center",
        legend.direction = "horizontal",
        legend.title = element_blank(), 
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA)) +
  scale_colour_manual(values = c("#005f73", "#ae2012", "#0a9396"), 
                      breaks = c('2019', '2020', '2021'))

# ggsave(rsf_elevation_summer,
#        file="figures/individual_figures/rsf_elevation_summer.png",
#        width = 6.86, height = 3, units = "in", dpi = 600, bg = "transparent")


#..................................................
# Plot RSF summer distance to escape terrain ----
#..................................................


# Plot distance to escape terrain for summer across all years

rsf_dis_escape_summer <-
ggplot(data = na.omit(rsf_df[rsf_df$season == "summer",])) +
  geom_hline(yintercept = 0, col = "grey70", linetype = "dashed") +
  geom_pointrange(aes(x = collar_id, y = dis_escape_est, ymin = dis_escape_low, ymax = dis_escape_high, color = year), 
                  size = 0.5, position = pd) +
  ylab("Distance to Escape") +
  ggtitle("b)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = -0.05, size = 10, family = "sans"),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_blank(), 
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        legend.position="none",
        legend.title = element_blank(), 
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA)) +
  scale_colour_manual(values = c("#005f73", "#ae2012", "#0a9396"), 
                      breaks = c('2019', '2020', '2021'))

# ggsave(rsf_dis_escape_summer,
#        file="figures/individual_figures/rsf_dis_escape_summer.png",
#        width = 6.86, height = 3, units = "in", dpi = 600, bg = "transparent")



#............................................
# Multi-panel RSF summer ----
#............................................

plot_rsf_summer <- grid.arrange(rsf_elevation_summer, rsf_dis_escape_summer,
                                ncol = 1)


ggsave(plot_rsf_summer,
       file="figures/rsf/rsf_summer.png",
       width = 6.86, height = 6, units = "in", dpi = 600, bg = "transparent")



#..................................................
# Plot mean RSF elevation ----
#..................................................

# Plot the average RSF elevation for both seasons across all years

mean_rsf_df$year <- as.factor(mean_rsf_df$year)

mean_rsf_elev <- 
ggplot(data = mean_rsf_df) +
  geom_hline(yintercept = 0, col = "grey70", linetype = "dashed") +
  geom_pointrange(aes(x = season, y = elev_est, ymin = elev_low, ymax = elev_high, color = year), 
                  size = 0.5, position = pd) +
  ylab("Mean Elevation") +
  ggtitle("a)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = -0.05, size = 10, family = "sans"),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_blank(), 
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        legend.position = c(0.5, 1.05), #horizontal, vertical
        legend.justification = "center",
        legend.direction = "horizontal",
        legend.title = element_blank(), 
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA)) +
  scale_colour_manual(values = c("#005f73", "#ae2012", "#0a9396"), 
                      breaks = c('2019', '2020', '2021'))

# ggsave(mean_rsf_elev,
#        file="figures/individual_figures/mean_rsf_elev.png",
#        width = 6.86, height = 3, units = "in", dpi = 600, bg = "transparent")



#..................................................
# Plot mean RSF distance to escape terrain ----
#..................................................

# Plot the average RSF distance to escape terrain for both seasons across all years

mean_rsf_dis_escape <-
ggplot(data = mean_rsf_df) +
  geom_hline(yintercept = 0, col = "grey70", linetype = "dashed") +
  geom_pointrange(aes(x = season, y = dis_escape_est, ymin = dis_escape_low, ymax = dis_escape_high, color = year), 
                  size = 0.5, position = pd) +
  ylab("Mean Distance to Escape") +
  ggtitle("b)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = -0.05, size = 10, family = "sans"),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_blank(), 
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        legend.position="none",
        legend.title = element_blank(), 
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA)) +
  scale_colour_manual(values = c("#005f73", "#ae2012", "#0a9396"), 
                      breaks = c('2019', '2020', '2021'))


# ggsave(mean_rsf_dis_escape,
#        file="figures/individual_figures/mean_rsf_dis_escape.png",
#        width = 6.86, height = 3, units = "in", dpi = 600, bg = "transparent")




#............................................
# Multi-panel Mean RSF ----

plot_mean_rsf <- grid.arrange(mean_rsf_elev, mean_rsf_dis_escape,
                              ncol=1)

ggsave(plot_mean_rsf,
       file="figures/rsf/mean_rsf.png",
       width = 3.23, height = 6, units = "in", dpi = 600, bg = "transparent")

ggsave(plot_mean_rsf,
       file="figures/rsf/mean_rsf_wide.png",
       width = 6.86, height = 6, units = "in", dpi = 600, bg = "transparent")
