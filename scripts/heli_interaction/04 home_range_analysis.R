


library(ctmm)
library(ggplot2)
library(gridExtra)

# Load data ----
load("data/goat/goat_data2.rda")
load("data/goat/goats_telemetry2.rda")
load("data/goat_fits2.rda")
load("data/goat_akdes2.rda")
load("data/home_range/dat_hr2.rda")

#names(dat.hr2)[1] <- "ID"

#Check locations of where each season and year is in the dataframe
dat.hr2[dat.hr2$season == "spring" & dat.hr2$year == "2019",] #[]
dat.hr2[dat.hr2$season == "spring" & dat.hr2$year == "2020",] #[28:35]
dat.hr2[dat.hr2$season == "spring" & dat.hr2$year == "2021",] #[36:44]

dat.hr2[dat.hr2$season == "summer" & dat.hr2$year == "2019",] #[45:53]
dat.hr2[dat.hr2$season == "summer" & dat.hr2$year == "2020",] #[54:63]
dat.hr2[dat.hr2$season == "summer" & dat.hr2$year == "2021",] #[64:72]

# dat.hr2[dat.hr2$season == "winter" & dat.hr2$year == "2019",] #[]
# dat.hr2[dat.hr2$season == "winter" & dat.hr2$year == "2020",] #[]
# dat.hr2[dat.hr2$season == "winter" & dat.hr2$year == "2021",] #[]

# dat.hr2[dat.hr2$year == "2019",] #[c()]
# dat.hr2[dat.hr2$year == "2020",] #[c()]
# dat.hr2[dat.hr2$year == "2021",] #[c()]

#........................................................
# Home-range ----


# Mean HR size
# ctmm::meta(AKDES2)




#____________________________________________________________
# Compare between years

#Mean HR size based on year only
# #2019
# ctmm::meta(AKDES2[c()])
# #2020
# ctmm::meta(AKDES2[c()])
# #2021
# ctmm::meta(AKDES2[c()])





# Mean HR size based on year and season
#spring (units = km^2)
#meta(AKDES2[]) #2019
ctmm::meta(AKDES2[28:35]) #2020
ctmm::meta(AKDES2[36:44]) #2021

#summer (units = km^2)
ctmm::meta(AKDES2[45:53]) #2019
ctmm::meta(AKDES2[54:63]) #2020
ctmm::meta(AKDES2[64:72]) #2021

# #winter
# ctmm::meta(AKDES2[]) #2019
# ctmm::meta(AKDES2[]) #2020
# ctmm::meta(AKDES2[]) #2021



#.............................................
# Diffusion rate ----
#.............................................

# 
# #Mean HR size based on year for any season
# #2019 (km^2/day)
# ctmm::meta(FITS2[c(1:9,44:52)],variable = "diffusion")
# #2020 (km^2/day)
# ctmm::meta(FITS2[c(10:19,27:34,53:62,72:80)],variable = "diffusion")
# #2021 (hectares/day)
# ctmm::meta(FITS2[c(20:26,35:43,63:71,81:90)],variable = "diffusion")



#Mean diffusion rate based on year and time block

#convert hectares (hm^2/day) to km^2/day (km^2/day = hectare * 0.01)

#spring 
#meta(FITS2[]) #2019
ctmm::meta(FITS2[28:35],variable = "diffusion") #2020 (hm^2/day)

ctmm::meta(FITS2[36:44],variable = "diffusion") #2021 (hm^2/day)
#convert values into km^2/day
32.4929442 * 0.01 #low
51.3818456 * 0.01 #est
77.386390 * 0.01  #high

#summer (km^2)
ctmm::meta(FITS2[45:53],variable = "diffusion") #2019
ctmm::meta(FITS2[54:63],variable = "diffusion") #2020
ctmm::meta(FITS2[64:72],variable = "diffusion") #2021

# #winter
# #ctmm::meta(FITS2[]) #2019
# ctmm::meta(FITS2[72:80],variable = "diffusion") #2020, in hectares units
# ctmm::meta(FITS2[81:90],variable = "diffusion") #2021, in hectares units


#....................................................
# Meta results dataframe ----



# create a dataframe to store the results
meta_2020_spring <- as.data.frame(ctmm::meta(AKDES2[28:35])) #2020
meta_2020_spring <- meta_2020_spring[1,]
meta_2020_spring$year <- "2020"
meta_2021_spring <- as.data.frame(ctmm::meta(AKDES2[36:44])) #2021
meta_2021_spring <- meta_2021_spring[1,]
meta_2021_spring$year <- "2021"
meta_spring <- rbind(meta_2020_spring,meta_2021_spring)
meta_spring$season <- "spring"


#summer (units = km^2)
meta_2019_summer <- as.data.frame(ctmm::meta(AKDES2[45:53])) #2020
meta_2019_summer <- meta_2019_summer[1,]
meta_2019_summer$year <- "2019"
meta_2020_summer <- as.data.frame(ctmm::meta(AKDES2[54:63])) #2020
meta_2020_summer <- meta_2020_summer[1,]
meta_2020_summer$year <- "2020"
meta_2021_summer <- as.data.frame(ctmm::meta(AKDES2[64:72])) #2021
meta_2021_summer <- meta_2021_summer[1,]
meta_2021_summer$year <- "2021"

meta_summer <- rbind(meta_2019_summer, meta_2020_summer,meta_2021_summer)
meta_summer$season <- "summer"

hr_results <- rbind(meta_spring, meta_summer)
hr_results$results <- "95_hr_area_sq_km"


#~~~~~~~~~~~~~~~~~~~~~diffusion (movement rates)

# create a dataframe to store the results
#spring (units = hm^2/day)
meta_diff_2020_spring <- as.data.frame(ctmm::meta(FITS2[28:35],variable = "diffusion")) #2020
meta_diff_2020_spring <- meta_diff_2020_spring[1,]
meta_diff_2020_spring$year <- "2020"
meta_diff_2021_spring <- as.data.frame(ctmm::meta(FITS2[36:44],variable = "diffusion")) #2021
meta_diff_2021_spring <- meta_diff_2021_spring[1,]
meta_diff_2021_spring$year <- "2021"
meta_diff_spring <- rbind(meta_diff_2020_spring,meta_diff_2021_spring)
meta_diff_spring$season <- "spring"
#convert hm^2/day into km^2/day
meta_diff_spring[,1:3] <- meta_diff_spring[,1:3] * 0.01

#summer (units = km^2/day)
meta_diff_2019_summer <- as.data.frame(ctmm::meta(FITS2[45:53],variable = "diffusion")) #2020
meta_diff_2019_summer <- meta_diff_2019_summer[1,]
meta_diff_2019_summer$year <- "2019"
meta_diff_2020_summer <- as.data.frame(ctmm::meta(FITS2[54:63],variable = "diffusion")) #2020
meta_diff_2020_summer <- meta_diff_2020_summer[1,]
meta_diff_2020_summer$year <- "2020"
meta_diff_2021_summer <- as.data.frame(ctmm::meta(FITS2[64:72],variable = "diffusion")) #2021
meta_diff_2021_summer <- meta_diff_2021_summer[1,]
meta_diff_2021_summer$year <- "2021"

meta_diff_summer <- rbind(meta_diff_2019_summer, meta_diff_2020_summer,meta_diff_2021_summer)
meta_diff_summer$season <- "summer"

diff_results <- rbind(meta_diff_spring, meta_diff_summer)
diff_results$results <- "diffusion_sq_km2_day"

results <- rbind(hr_results, diff_results)
#drop rownames
rownames(results) <- NULL

library(dplyr)
results <- relocate(results, c(results, season, year), .before = "low")

write.csv(results, file = "data/home_range/home_range_analysis_results.csv", row.names = FALSE)



#...................................................................
# Plot spring hr ----
#...................................................................


# a) -> boxplot of HR sizes in the 3 years
#dat.hr2$period <- factor(dat.hr2$period, levels = c('before', 'during', 'after'))

dat.hr2$year <- factor(dat.hr2$year, levels = c('2019', '2020', '2021'))

plot_hr_spring <-
  ggplot(data = dat.hr2[dat.hr2$season == "spring",],
         mapping = aes(x = year, y = HR_est, fill = year)) +
  geom_boxplot(alpha = 0.5, size = 0.3, outlier.size = 0.3) +
  geom_jitter(alpha = 0.9, size = 1, aes(col = year), width = 0.1) +
  labs(y = bquote(bold("Home range area" ~ (km^2))),
       x = "Year") +
  ggtitle("a)") +
  # ggtitle(label = "Spring",
  #         subtitle = "a)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = -0.05, size = 10, family = "sans"),
        axis.title.y = element_text(size = 10, family = "sans", face = "bold"),
        axis.title.x = element_text(size = 10, family = "sans", face = "bold"),
        axis.text.y = element_text(size = 8, family = "sans"),
        axis.text.x  = element_text(size = 8, family = "sans"),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA)) +
  scale_fill_manual(values = c("#005f73", "#ae2012", "#0a9396"), 
                    breaks = c('2019', '2020', '2021')) +
  scale_colour_manual(values = c("#005f73", "#ae2012", "#0a9396"), 
                      breaks = c('2019', '2020', '2021'))


ggsave(plot_hr_spring, width = 6.86, height = 6, units = "in", dpi = 600, bg = "transparent",
        file="figures/individual_plots/spring_hr.png")

#...................................................................
# Plot spring diffusion ----
#...................................................................


plot_diff_spring <-
  ggplot(data = dat.hr2[dat.hr2$season == "spring",],
         mapping = aes(x = year, y = diffusion_km2_day, fill = year)) +
  geom_boxplot(alpha = 0.5, size = 0.3, outlier.size = 0.3) +
  geom_jitter(alpha = 0.9, size = 1, aes(col = year), width = 0.1) +
  labs(y = bquote(bold("Diffusion rate " ~ (km^2/day))),
       x = "Year") +
  ggtitle("b)") +
  # ggtitle(label = "",
  #         subtitle = "b)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = -0.05, size = 10, family = "sans"),
        axis.title.y = element_text(size = 10, family = "sans", face = "bold"),
        axis.title.x = element_text(size = 10, family = "sans", face = "bold"),
        axis.text.y = element_text(size = 8, family = "sans"),
        axis.text.x  = element_text(size = 8, family = "sans"),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA)) +
  scale_fill_manual(values = c("#005f73", "#ae2012", "#0a9396"), 
                    breaks = c('2019', '2020', '2021')) +
  scale_colour_manual(values = c("#005f73", "#ae2012", "#0a9396"), 
                      breaks = c('2019', '2020', '2021'))

ggsave(plot_diff_spring, width = 6.86, height = 6, units = "in", dpi = 600, bg = "transparent",
        file="figures/individual_plots/spring_diffusion.png")


#..........................................................
# Multi-panel ----
#..........................................................

# Vertical
plot_spring <- grid.arrange(plot_hr_spring, plot_diff_spring,
                               ncol = 1)
ggsave(plot_spring, width = 3.23, height = 6, units = "in", dpi = 600, bg = "transparent",
       file="figures/home_range/spring_hr_diff.png")


# Horizontal
plot_spring_h <- grid.arrange(plot_hr_spring, plot_diff_spring,
                               ncol = 2)
ggsave(plot_spring_h, width = 6.86, height = 3.23, units = "in", dpi = 600, bg = "transparent",
       file="figures/home_range/spring_hr_diff_h.png")





#...................................................................
# Plot summer hr ----
#...................................................................


plot_hr_summer <-
  ggplot(data = dat.hr2[dat.hr2$season == "summer",],
         mapping = aes(x = year, y = HR_est, fill = year)) +
  geom_boxplot(alpha = 0.5, size = 0.3, outlier.size = 0.3) +
  geom_jitter(alpha = 0.9, size = 1, aes(col = year), width = 0.1) +
  labs(y = bquote(bold("Home range area" ~ (km^2))),
       x = "Year") +
  # ggtitle(label = "Summer",
  #         subtitle = "a)") +
  ggtitle("a)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = -0.05, size = 10, family = "sans"),
        axis.title.y = element_text(size = 10, family = "sans", face = "bold"),
        axis.title.x = element_text(size = 10, family = "sans", face = "bold"),
        axis.text.y = element_text(size = 8, family = "sans"),
        axis.text.x  = element_text(size = 8, family = "sans"),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA)) +
  scale_fill_manual(values = c("#005f73", "#ae2012", "#0a9396"), 
                    breaks = c('2019', '2020', '2021')) +
  scale_colour_manual(values = c("#005f73", "#ae2012", "#0a9396"), 
                      breaks = c('2019', '2020', '2021'))


ggsave(plot_hr_summer, width = 6.86, height = 6, units = "in", dpi = 600, bg = "transparent",
       file="figures/individual_plots/summer_hr.png")



#...................................................................
# Plot summer diffusion ----
#...................................................................

plot_diff_summer <-
  ggplot(data = dat.hr2[dat.hr2$season == "summer",],
         mapping = aes(x = year, y = diffusion_km2_day, fill = year)) +
  geom_boxplot(alpha = 0.5, size = 0.3, outlier.size = 0.3) +
  geom_jitter(alpha = 0.9, size = 1, aes(col = year), width = 0.1) +
  labs(y = bquote(bold("Diffusion rate " ~ (km^2/day))),
       x = "Year") +
  # ggtitle(label = "",
  #         subtitle = "b)") +
  ggtitle("b)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = -0.05, size = 10, family = "sans"),
        axis.title.y = element_text(size = 10, family = "sans", face = "bold"),
        axis.title.x = element_text(size = 10, family = "sans", face = "bold"),
        axis.text.y = element_text(size = 8, family = "sans"),
        axis.text.x  = element_text(size = 8, family = "sans"),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA)) +
  scale_fill_manual(values = c("#005f73", "#ae2012", "#0a9396"), 
                    breaks = c('2019', '2020', '2021')) +
  scale_colour_manual(values = c("#005f73", "#ae2012", "#0a9396"), 
                      breaks = c('2019', '2020', '2021'))


ggsave(plot_diff_summer, width = 6.86, height = 6, units = "in", dpi = 600, bg = "transparent",
       file="figures/individual_plots/summer_diffusion.png")



#...................................................................
# Multi-panel ----
#...................................................................

# Vertical
hr_diff_summer <- grid.arrange(plot_hr_summer, plot_diff_summer,
                               ncol = 1)
ggsave(hr_diff_summer, width = 3.23, height = 6, units = "in", dpi = 600, bg = "transparent",
       file="figures/home_range/summer_hr_diff.png")


# Horizontal
hr_diff_summer_h <- grid.arrange(plot_hr_summer, plot_diff_summer,
                               ncol = 2)
ggsave(hr_diff_summer_h, width = 6.86, height = 3.23, units = "in", dpi = 600, bg = "transparent",
       file="figures/home_range/summer_hr_diff_h.png")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PLOTS NOT IN USE ----
# #...................................................................
## # Plot winter season ----
# 
# plot_hr_winter <-
#   ggplot(data = dat.hr2[dat.hr2$season == "winter",],
#          mapping = aes(x = year, y = HR_est, fill = year)) +
#   geom_boxplot(alpha = 0.5, size = 0.3, outlier.size = 0.3) +
#   geom_jitter(alpha = 0.9, size = 1, aes(col = year), width = 0.1) +
#   labs(y = bquote(bold("Home range area" ~ (km^2))),
#        x = "year") +
#   ggtitle(label = "Winter",
#           subtitle = "a)") +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.title = element_text(hjust = 0.5, size = 14, family = "sans", face = "bold"),
#         axis.title.y = element_text(size = 10, family = "sans", face = "bold"),
#         axis.title.x = element_text(size = 10, family = "sans", face = "bold"),
#         axis.text.y = element_text(size = 8, family = "sans"),
#         axis.text.x  = element_text(size = 8, family = "sans"),
#         legend.position = "none",
#         panel.background = element_rect(fill = "transparent"),
#         plot.background = element_rect(fill = "transparent", color = NA)) +
#   scale_fill_manual(values = c("#005f73", "#ae2012", "#0a9396"), 
#                     breaks = c('2019', '2020', '2021')) +
#   scale_colour_manual(values = c("#005f73", "#ae2012", "#0a9396"), 
#                       breaks = c('2019', '2020', '2021'))
# 
# 
# plot_diff_winter <-
#   ggplot(data = dat.hr2[dat.hr2$season == "winter",],
#          mapping = aes(x = year, y = diffusion, fill = year)) +
#   geom_boxplot(alpha = 0.5, size = 0.3, outlier.size = 0.3) +
#   geom_jitter(alpha = 0.9, size = 1, aes(col = year), width = 0.1) +
#   labs(y = bquote(bold("Diffusion rate" ~ (m^2/day))),
#        x = "year") +
#   ggtitle(label = "",
#           subtitle = "b)") +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         #plot.title = element_text(size = 14, family = "sans", face = "bold"),
#         axis.title.y = element_text(size = 10, family = "sans", face = "bold"),
#         axis.title.x = element_text(size = 10, family = "sans", face = "bold"),
#         axis.text.y = element_text(size = 8, family = "sans"),
#         axis.text.x  = element_text(size = 8, family = "sans"),
#         legend.position = "none",
#         panel.background = element_rect(fill = "transparent"),
#         plot.background = element_rect(fill = "transparent", color = NA)) +
#   scale_fill_manual(values = c("#005f73", "#ae2012", "#0a9396"), 
#                     breaks = c('2019', '2020', '2021')) +
#   scale_colour_manual(values = c("#005f73", "#ae2012", "#0a9396"), 
#                       breaks = c('2019', '2020', '2021'))
# 
# 
# 
# hr_diff_winter <- grid.arrange(plot_hr_winter, plot_diff_winter,
#                                ncol = 1)
# ggsave(hr_diff_winter, width = 3.23, height = 6, units = "in", dpi = 600, bg = "transparent",
#        file="figures/hr_diff_winter.png")










# #.............................................
## # Plot meta() ----
# #.............................................
# 
# 
# #spring
# png(file="figures/meta_figure_spring.png",
#     width=6.86*1.5, height=3*1.5, units="in", res=600)
# par(mfrow = c(1,2))
# #Home-range sizes
# AKDES_sorted_spring <- list("2020" = AKDES2[27:34],
#                             "2021" = AKDES2[35:43])
# 
# 
# ctmm::meta(AKDES_sorted_spring, col = c("#de2d26", "#ddaa33", "#001219"))
# title("Spring", adj = 0)
# mtext("a)", adj = 0)
# 
# 
# #Diffusion rates
# FITS_sorted_spring <- list("2020" = FITS2[27:34],
#                            "2021" = FITS2[35:43])
# 
# ctmm::meta(FITS_sorted_spring,variable = "diffusion", col = c("#de2d26", "#ddaa33", "#001219"))
# mtext("b)", adj = 0)
# dev.off()
# 
# 
# 
# 
# # summer season
# png(file="figures/meta_figure_summer.png",
#     width=6.86*1.5, height=3*1.5, units="in", res=600)
# par(mfrow = c(1,2))
# #Home-range sizes
# AKDES_sorted_summer <- list("2019" = AKDES2[44:52],
#                             "2020" = AKDES2[53:62],
#                             "2021" = AKDES2[63:71])
# 
# 
# ctmm::meta(AKDES_sorted_summer, col = c("#228833", "#de2d26", "#ddaa33", "#001219"))
# title("Summer", adj = 0)
# mtext("a)", adj = 0)
# 
# #Diffusion rates
# FITS_sorted_summer <- list("2019" = FITS2[44:52],
#                            "2020" = FITS2[53:62],
#                            "2021" = FITS2[63:71])
# 
# ctmm::meta(FITS_sorted_summer,variable = "diffusion", col = c("#228833","#de2d26","#ddaa33", "#001219"))
# mtext("b)", adj = 0)
# dev.off()
# 
# 
# # winter season
# 
# png(file="figures/meta_figure_winter.png",
#     width=6.86*1.5, height=3*1.5, units="in", res=600)
# par(mfrow = c(1,2))
# #Home-range sizes
# AKDES_sorted_winter <- list("2020" = AKDES2[72:80],
#                             "2021" = AKDES2[81:90])
# 
# 
# ctmm::meta(AKDES_sorted_winter, col = c("#de2d26","#ddaa33", "#001219"))
# title("Winter", adj = 0)
# mtext("a)", adj = 0)
# 
# #Diffusion rates
# FITS_sorted_winter <- list("2020" = FITS2[72:80],
#                            "2021" = FITS2[81:90])
# 
# ctmm::meta(FITS_sorted_winter,variable = "diffusion", col = c("#de2d26","#ddaa33", "#001219"))
# mtext("b)", adj = 0)
# dev.off()






#..................................................
## Plot all home range size ----
#..................................................


# Plot all home ranges regardless of season and year

mean_HR_est <- round(mean(dat.hr2$HR_est), 2)

plot_hr <-
  ggplot() +
  geom_vline(data = dat.hr2, aes(xintercept = mean_HR_est),
             linetype = "dotdash") +
  geom_linerange(data = dat.hr2,
                 aes(xmin = HR_low, xmax = HR_high, y = individual.local.identifier, color = year),
                 linewidth = 3, alpha = 0.5) +
  labs(x = bquote(bold("Home range area" ~ (km^2))),
       y = "") +
  geom_point(data = dat.hr2,
             aes(x = HR_est, y = individual.local.identifier, fill = "white"), color = "white",
             size = 2) +
  geom_point(data = dat.hr2,
             aes(x = HR_est, y = individual.local.identifier, color = year),
             size = 1.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14, family = "sans", face = "bold"),
        axis.title.y = element_text(size = 10, family = "sans", face = "bold"),
        axis.title.x = element_text(size = 10, family = "sans", face = "bold"),
        axis.text.y = element_text(size = 8, family = "sans"),
        axis.text.x  = element_text(size = 8, family = "sans"),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA)) +
  scale_fill_manual(values = c("#005f73", "#ae2012", "#0a9396"),
                    breaks = c('2019', '2020', '2021')) +
  scale_colour_manual(values = c("#005f73", "#ae2012", "#0a9396"),
                      breaks = c('2019', '2020', '2021'))


ggsave(plot_hr, width = 6.86, height = 6, units = "in", dpi = 600, bg = "transparent",
       file="figures/plot_hr.png")



#..................................................
## Plot all diffusion ----
#..................................................


# Plot all diffusion rates regardless of season and year

mean_diffusion <- round(mean(dat.hr2$diffusion), 2)

plot_diff <-
  ggplot() +
  geom_vline(data = dat.hr2, aes(xintercept = mean_diffusion),
             linetype = "dotdash") +
  geom_linerange(data = dat.hr2,
                 aes(xmin = diffusion_min, xmax = diffusion_max, y = individual.local.identifier, color = year),
                 linewidth = 3, alpha = 0.5) +
  labs(x = bquote(bold("Diffusion rates" ~ (km^2))),
       y = "") +
  geom_point(data = dat.hr2,
             aes(x = diffusion, y = individual.local.identifier, fill = "white"), color = "white",
             size = 2) +
  geom_point(data = dat.hr2,
             aes(x = diffusion, y = individual.local.identifier, color = year),
             size = 1.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14, family = "sans", face = "bold"),
        axis.title.y = element_text(size = 10, family = "sans", face = "bold"),
        axis.title.x = element_text(size = 10, family = "sans", face = "bold"),
        axis.text.y = element_text(size = 8, family = "sans"),
        axis.text.x  = element_text(size = 8, family = "sans"),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA)) +
  scale_fill_manual(values = c("#005f73", "#ae2012", "#0a9396"),
                    breaks = c('2019', '2020', '2021')) +
  scale_colour_manual(values = c("#005f73", "#ae2012", "#0a9396"),
                      breaks = c('2019', '2020', '2021'))


ggsave(plot_hr, width = 6.86, height = 6, units = "in", dpi = 600, bg = "transparent",
       file="figures/plot_hr.png")
