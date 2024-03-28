
# Merge 95 HR estimate shapefiles


#____________________________________________________________________
# 06a.load_merge_shapefiles_per_period.r
# Merge all 95% HR estimate ----

# Import 95% home range estimate shapefiles

# Set folder path containing the exported subfolders
folder_path <- "data/home_range/shp"

# Load .shp files from subfolders
shp.dir <- list.files(path = folder_path, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)


#Read each shapefile into a list
shp.files <- lapply(shp.dir, st_read)
names(shp.files) <- names(AKDES)
#Remove individuals with large HR, drop 34,47
shp.files <- shp.files[-c(34,47)]

# Combine all the shapefiles into a single sf object
#dat_shp = dplyr::bind_rows(shp.files)
dat_shp <- do.call(rbind, shp.files)

# Subset 95% est shapefiles only based on the text "95% est"
library(stringr)
dat_shp <- dat_shp[str_detect(dat_shp$name, "est"),]
# dat_shp <- dat_shp[grepl("95% est", dat_shp$name), ]
rownames(dat_shp) <- NULL

dat_shp = st_transform(dat_shp, crs = st_crs(bc_albers))
st_crs(dat_shp)


#save shapefile
st_write(dat_shp, dsn = 'data/home_range/merged_95_HR', 
         driver = 'ESRI Shapefile', append=FALSE)

dat.shp = st_read(dsn = './data/home_range/merged_95_HR')