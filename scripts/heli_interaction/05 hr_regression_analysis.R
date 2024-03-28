
library(ctmm)
library(lme4)


# Load data ----
load("data/goat/goats_telemetry2.rda") #goat data (telemetry)
load("data/goat_fits2.rda")
load("data/goat_akdes2.rda")
load("data/home_range/dat_hr2.rda")


#________________________________________________________________
#07.HR_Regression.r


# Spring models ----

# Build full model based on 95% home range area estimate, weighted regression
#dof mean is the effective sample size for estimating the center of the animal's home range.

# Comparing spring (anthropause) season only, subset data out
dat.spring <- dat.hr2[dat.hr2$season == "spring",]

# use collar_id instead of "individual.local.identifier" because individual.local.identifier contains spring, year and collar id and when using this, it accounts for all of that junk instead of just collar id hence using ONLY collar_id
#moreover when using individual.local.identifier in the model, its being set up as a 3 way interaction between season,year,goat and thats a no no
m_hr_spring <- glmer(HR_est ~ year + 
                       (1|collar_id),
                     family = Gamma('log'),
                     weights = DOF_area,
                     data = dat.spring,
                     na.action = "na.fail")



summary(m_hr_spring)
#year2021 isnt much different from year 2020, but year 2020 is much different than 0, and 0 is the baseline ie. having no home range size, no diffusion rate, etc.

#..............................................
#test for significance using likelihood ratio test (Note: this can be achieved by just running summary on the model and the "Pr(>|z|)" produces the same value)
m_hr_spring2 <- glmer(HR_est ~ 1 + (1|collar_id),
                      family = Gamma('log'),
                      weights = DOF_area,
                      data = dat.spring,
                      na.action = "na.fail")
test_results <- anova(m_hr_spring, m_hr_spring2)
round(test_results$`Pr(>Chisq)`[2], 2) #p = 0.51 (0.508 from summary)
#..............................................





#export and save summary output to a textfile
sink("data/home_range/m_hr_spring_summary.txt")
print(summary(m_hr_spring))
cat("\n") #enter blank line
# calculate the CI values
print("CI Values (lower, upper)")
# est + upper & lower z-score * std err
print(-0.1105 + c(-1.96,1.96) * 0.1670)
sink() #terminate output exporting connection/process (multiple functions can be exported)

# #AICc based model selection
# library(MuMIn)
# hr_sel <- dredge(m_hr); hr_sel
# #several warnings
# 
# #write.csv(hr_sel, "./data/home_range/selmod_HR.csv", row.names = FALSE)
# 
# sink("data/home_range/selmod_HR.txt")
# print(hr_sel)
# sink()


#~~~~~~~~~~~~~~~


#build full model based on diffusion rates
m_diffusion_spring <- glmer(diffusion_km2_day ~ year + 
                       (1|collar_id),
                     family = Gamma('log'),
                     weights = DOF_diffusion,
                     data = dat.spring,
                     na.action = "na.fail")

summary(m_diffusion_spring)

#export and save summary output to a textfile
sink("./data/home_range/m_diffusion_spring_summary.txt")
print(summary(m_diffusion_spring))
cat("\n") #enter blank line
# calculate the CI values
print("CI Values (lower, upper)")
# est + upper & lower z-score * std err
print(0.42487   + c(-1.96,1.96) * 0.01564  )
sink() #terminate output exporting connection/process (multiple functions can be exported)

# #AICc based model selection
# diff_sel <- dredge(m_diffusion); diff_sel
# 
# sink("./data/home_range/aic_selected_model_diffusion.txt")
# print(diff_sel)
# sink()


#...........................................................
# Summer models ----
#...........................................................



dat.summer <- dat.hr2[dat.hr2$season == "summer",]

m_hr_summer <- glmer(HR_est ~ year + 
                       (1|collar_id),
                     family = Gamma('log'),
                     weights = DOF_area,
                     data = dat.summer,
                     na.action = "na.fail")






summary(m_hr_summer)
#year2021 isnt much different from year 2020, but year 2020 is much different than 0, and 0 is the baseline ie. having no home range size, no diffusion rate, etc.


#export and save summary output to a textfile
sink("data/home_range/m_hr_summer_summary.txt")
print(summary(m_hr_summer))
cat("\n") #enter blank line
# calculate the CI values
print("year2020 CI Values (lower, upper)")
# est + upper & lower z-score * std err
print(0.42487   + c(-1.96,1.96) * 0.04826  )
cat("\n") #enter blank line
print("year2021 CI Values (lower, upper)")
print(0.39581 + c(-1.96,1.96) * 0.06011)
sink() 

#~~~~~~~~


#build full model based on diffusion rates
m_diffusion_summer <- glmer(diffusion_km2_day ~ year + 
                              (1|collar_id),
                            family = Gamma('log'),
                            weights = DOF_diffusion,
                            data = dat.summer,
                            na.action = "na.fail")

summary(m_diffusion_summer)


#export and save summary output to a textfile
sink("data/home_range/m_diffusion_summer_summary.txt")
print(summary(m_diffusion_summer))
cat("\n") #enter blank line
# calculate the CI values
print("year2020 CI Values (lower, upper)")
# est + upper & lower z-score * std err
print(0.261020      + c(-1.96,1.96) * 0.006140    )
cat("\n") #enter blank line
print("year2021 CI Values (lower, upper)")
print(0.153666    + c(-1.96,1.96) * 0.006246  )
sink()

