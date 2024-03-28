
library(ctmm)
library(terra)
library(tictoc)
# 
# load("data/goat/goats_telemetry.rda")
# load("data/goat/goat_data.rda")



 
#____________________________________________________________
# Fit movement models

START <- Sys.time()

FITS <- list()
for(i in 1:length(goats)){
  
  #Extract individual
  DATA <- goats[[i]]
  
  # create guesstimate non-interactively
  GUESS <- ctmm.guess(DATA,CTMM=ctmm(error=FALSE),interactive=FALSE) # Error is off for now to speed up the process
  
  # fit models
  FITS[[i]] <- ctmm.select(DATA, GUESS, trace = 3, cores=-1)
  
}
#rename for convenience
names(FITS) <- names(goats)

END <- Sys.time()

save(FITS,file="data/goat_fits.rda")
#load("data/goat/goat_fits.rda")


#____________________________________________________________
# Estimate home-range areas
START <- Sys.time()
tic()
AKDES <- akde(goats,FITS,weights=TRUE)
toc()
END <- Sys.time()
#__________________________________________________________________________
# Fit_Mods.R

# in ctmm you can create 2 things:
# PMF: a raster of the probabilities of where the animal could be in the HR
# AKDE shapefile: a boundary of some given quantile

#And save

#save rda:
save(AKDES,file="data/goat_akdes.rda")
# load("data/goat_akdes.rda")

#save UD as raster:
dir.create("data/home_range/UD", recursive = TRUE)
#Note: includes distribution function = probability mass function
#QUESTION: for the PMF you don't specify the level.UD because it's a PMF rather than a contour
for (i in 1:length(AKDES)) {
  UD_file <- file.path("data/home_range/UD", paste0(names(AKDES)[i], ".tif"))
  writeRaster(AKDES[[i]], filename = UD_file, format = 'GTiff', DF = "PMF",
              overwrite = TRUE)
  }




#save 95% range estimate as shapefile:
# DF = "PMF" is not possible in a shp file, shp is only for points or boundaries
#Note: this sometimes works and sometimes doesnt because it says writeShapefile() doesnt exist, writeVector() does work as of recent update, now it doesn't, currently using writeShapefile() as of 2024-03-12
dir.create("data/home_range/shp", recursive = TRUE)

for (name in names(AKDES)) {
  shp.path <- file.path("data/home_range/shp", paste0(name, ".shp"))
  writeVector(AKDES[[name]], shp.path,
              level.UD=0.95, level=0.95, overwrite = TRUE)
}

for (name in names(AKDES)) {
  shp.path <- file.path("data/home_range/shp", paste0(name, ".shp"))
  writeShapefile(AKDES[[name]], shp.path,
                 level.UD=0.95, level=0.95, overwrite = TRUE)
}