

# movement and hr analysis
library(ctmm)

# import data
load("data/movement_model/fits_20250301.rda")
load("data/home_range/akdes_20250301.rda")


#...........................................................
# #check for significance, home range ----
#...........................................................


# subset data
no_fire_year <- AKDES[!grepl("2023", names(AKDES))]
fire_year <- AKDES[grepl("2023", names(AKDES))]

#calculate mean home range sizes for no fire years
meta(no_fire_year)
#calculate mean home range sizes for fire period
meta(fire_year)

#test to see significance of year on home range using the meta() function
hr_year_compare <- list(no_fire = no_fire_year,
                        fire = fire_year)
COL_year <- c("grey", "#A50026")
meta(hr_year_compare, col = COL_year, sort = TRUE)



#...........................................................
# #check for significance, diffusion
#...........................................................

no_fire_year2 <- FITS[!grepl("2023", names(FITS))]
fire_year2 <- FITS[grepl("2023", names(FITS))]

#calculate mean home range sizes for no fire years
meta(no_fire_year2, variable = "diffusion")
#calculate mean home range sizes for fire period
meta(fire_year2, variable = "diffusion")

#test to see significance of year on diffusion using the meta() function
diff_year_compare <- list(no_fire = no_fire_year2,
                          fire = fire_year2)
COL_year <- c("grey", "#A50026")
meta(diff_year_compare, col = COL_year, sort = TRUE, "diffusion")



