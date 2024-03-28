

library(terra)
library(corrplot)
library(sf)

# 1. Extract the 95% home-range estimate shapefiles
# 2. Assign habitat to each of the home-range polygons

#______________________________________________
# 06.extract.raster.95.hr.estimate.r

#Import habitat rasters 
#Note: downloaded by Ryan

#define the crs you want the rasters in
bc_albers <- "EPSG:3005"

# Currently in BC Albers crs
elev = rast('data/habitat/rasters/elev_25m.tif')
slope = rast('data/habitat/rasters/slope_25m.tif')
escape = rast('data/habitat/rasters/escape_25m.tif')
dis_escape = rast('data/habitat/rasters/dist_escape_25m.tif')

#scale/center raster data via terra pkg
scl.elev = scale(elev)
scl.slope = scale(slope)
scl.escape = scale(escape)
scl.dis_escape = scale(dis_escape)

# Combine rasters
rstack1 = c(elev, slope, escape, dis_escape, 
            scl.elev, scl.slope, scl.escape, scl.dis_escape)
rstack2 = c(elev, slope, dis_escape, 
            scl.elev, scl.slope, scl.dis_escape)

#................................................

dat.shp = st_read(dsn = './data/home_range/merged_95_HR/merged_95_HR.shp')

#all raster values as mean values
dat.extract = extract(rstack2, dat.shp, fun = 'mean', bind = TRUE, na.rm = TRUE)

dat.df = as.data.frame(dat.extract)

#rename columns
names(dat.df)[1] <- "individual.local.identifier"
names(dat.df)[2] <- "mean_elev"
names(dat.df)[3] <- "mean_slope"
names(dat.df)[4] <- "mean_dis_escape"
names(dat.df)[5] <- "mean_scl.elev"
names(dat.df)[6] <- "mean_scl.slope"
names(dat.df)[7] <- "mean_scl.dis_escape"


load("data/home_range/dat_hr2.rda")

dat.c <- cbind(dat.hr2, dat.df, by = "individual.local.identifier", all = TRUE)


#corrplot to determine variables to include in full model
dat.cor = dat.c[,c("HR_est", 
                     "mean_elev", "mean_slope", "mean_dis_escape",
                     "mean_scl.elev", "mean_scl.slope", "mean_scl.dis_escape")]
names(dat.cor) = c("Home Range", 
                   "Elevation", "Slope", "Dist. to Escape Terrain",
                   "Scaled Elevation", "Scaled Slope", "Scaled Dist. to Escape Terrain")
plot_corrplot <- corrplot(cor(dat.cor), method = 'number')

#save plot
png(file="figures/corrplot.png",
    width=6.86, height=6, units="in", res=600)
plot_corrplot
dev.off()




