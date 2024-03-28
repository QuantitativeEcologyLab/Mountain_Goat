
library(ctmm)
library(raster)
library(sf)
library(terra)


# Load data ----
load("data/goat/goats_telemetry.rda") #goat data (telemetry)
load("data/goat_fits.rda")
load("data/goat_akdes.rda")

load("data/home_range/dat_hr.rda")

#.......................................................................
# Inspect Home-range area estimates ----
#.......................................................................

# Check home range results for irregularities such as dispersing or nomadic movement behaviour

# Inspect home-range estimates via plot
# ctmm::meta(AKDES)
#large home ranges with large CI is observed, get empirical values


# Create a dataframe store the results
dat.hr <- as.data.frame(names(FITS))
names(dat.hr)[1] <- "individual.local.identifier"
#extract text before the first _ underscore (i.e. season)
dat.hr$season <- sub("^(.*?)_.*", "\\1", dat.hr$individual.local.identifier)
#extract text after the first _ underscore and before the second _ underscore (i.e. year)
dat.hr$year <- sub("^[^_]*_(.*?)_.*", "\\1", dat.hr$individual.local.identifier)
#extract text after the second _ underscore (i.e. collar_id)
dat.hr$collar_id <- sub("^(?:[^_]*_){2}(.*)", "\\1", dat.hr$individual.local.identifier)


#.......................................................................


#Get HR values (units = square km) ----
HR_size <- data.frame()
for (i in 1:length(AKDES)) {
  summary <- summary(AKDES[[i]])$CI
  HR_size <- rbind(HR_size, summary)
}
names(HR_size)[1] <- "HR_low"
names(HR_size)[2] <- "HR_est"
names(HR_size)[3] <- "HR_high"
rownames(HR_size) <- NULL

dat.hr <- cbind(dat.hr, HR_size)



#________________________________________________________________
#Fit_Mods.R

#Get diffusion estimates ----
diffusion_results <- data.frame(
  diffusion = numeric(length(FITS)),
  diffusion_min = numeric(length(FITS)),
  diffusion_max = numeric(length(FITS))
)


# #Get diffusion values (units = square meters/second)
# units = FALSE removes the units that FITS has assigned and then by default it is in m^2/second

for (i in seq_along(FITS)) {
  # Check if "diffusion (square meters/second)" is present in the row names
  if ("diffusion (square meters/second)" %in% row.names(summary(FITS[[i]], units = FALSE)$CI)) {
    # Update the corresponding row in the data frame
    diffusion_results[i, "diffusion"] <- summary(FITS[[i]], units = FALSE)$CI["diffusion (square meters/second)", 2]
    diffusion_results[i, "diffusion_min"] <- summary(FITS[[i]], units = FALSE)$CI["diffusion (square meters/second)", 1]
    diffusion_results[i, "diffusion_max"] <- summary(FITS[[i]], units = FALSE)$CI["diffusion (square meters/second)", 3]
  }
}


# Convert m^2/second into km^2/day
diffusion_results$diffusion_km2_day <- diffusion_results$diffusion * 0.0864
diffusion_results$diffusion_min_km2_day <- diffusion_results$diffusion_min * 0.0864
diffusion_results$diffusion_max_km2_day <- diffusion_results$diffusion_max * 0.0864


dat.hr <- cbind(dat.hr, diffusion_results)


#................................



#Get degrees of freedom (for movement?)
DOF_results <- data.frame()
for (i in 1:length(FITS)) {
  summary <- summary(FITS[[i]])$DOF
  DOF_results <- rbind(DOF_results, summary)
}
names(DOF_results)[1] <- "DOF_mean"
names(DOF_results)[2] <- "DOF_area"
names(DOF_results)[3] <- "DOF_diffusion"
names(DOF_results)[4] <- "DOF_speed"

dat.hr <- cbind(dat.hr, DOF_results)


# save(dat.hr, file = "data/home_range/dat_hr.rda")
# load("data/home_range/dat_hr.rda")

#..................................................................
# Variograms ----
#..................................................................

# Individuals with home-range larger than 500km, inspect movement behaviour via variogram to see if it 
# If variogram never hits a stable asymptote, so it was likely dispersing or moving nomadically during that period

# [34] spring_2020_30636
# [47] summer_2019_30551
level <- c(0.5,0.95) # 50% and 95% CIs
xlim <- c(0,12 %#% "hour") # as a 0-12 hour window


# As per Dr. Noonan, need not drop NA_2021_30551. 

SVF_spring_2020_30636 <- variogram(goats[[34]]) # estimate semi-variance function

png(file="figures/individual_plots/SVF_spring_2020_30636.png",
    width=6.86*1.5, height=3*1.5, units="in", res=600)
par(mfrow = c(1,2))
plot(SVF_spring_2020_30636,xlim=xlim,level=level)
title("zoomed in")
plot(SVF_spring_2020_30636,fraction=0.65,level=level)
title("zoomed out")
dev.off()




SVF_summer_2019_30551 <- variogram(goats[[47]]) # estimate semi-variance function

png(file="figures/individual_plots/SVF_summer_2019_30551.png",
    width=6.86*1.5, height=3*1.5, units="in", res=600)
par(mfrow = c(1,2))
plot(SVF_summer_2019_30551,xlim=xlim,level=level)
title("zoomed in")
plot(SVF_summer_2019_30551,fraction=0.65,level=level)
title("zoomed out")
dev.off()











#..................................................................


#Remove individuals with large HR (34,47)
goats2 <- goats[-c(34,47)]
FITS2 <- FITS[-c(34,47)]
AKDES2 <- AKDES[-c(34,47)]
dat.hr2 <- dat.hr[-c(34,47),]

# Renumber the rownames properly 
rownames(dat.hr2) <- 1:nrow(dat.hr2)

#..................................................................


test <- goat_data[goat_data$individual.local.identifier == "spring_2020_30636",] #486
test2 <- goat_data[goat_data$individual.local.identifier == "summer_2019_30551",] #360

#Remove irregular location points from goat data
goat_data2 <- goat_data
goat_data2 <- goat_data2[goat_data2$individual.local.identifier != "spring_2020_30636",]
goat_data2 <- goat_data2[goat_data2$individual.local.identifier != "summer_2019_30551",]

#total data points per year
table(goat_data2$year)
#total data points per season
table(goat_data2$season)

save(goat_data2, file = "data/goat/goat_data2.rda")
save(goats2,file="data/goat/goats_telemetry2.rda")
save(FITS2,file="data/goat_fits2.rda")
save(AKDES2,file="data/goat_akdes2.rda")
save(dat.hr2,file="data/home_range/dat_hr2.rda")





