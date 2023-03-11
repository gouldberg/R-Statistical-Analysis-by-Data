# setwd("//media//kswada//MyFiles//R//lalonde_nswcps")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\algebra_nation")

packages <- c("dplyr", "tidyverse", "MatchIt", "Matching", "survey")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data: Algebra Nation
# ------------------------------------------------------------------------------


data <- read.csv("algebra_nation.csv")


str(data)


car::some(data)


names(data)




# ----------
# Standardize continuous covariates

data$numOfStud2014 <- scale(data$numOfStud2014)

data$meanScale2012 <- scale(data$meanScale2012)

data$lev1Perc2012 <- scale(data$lev1Perc2012)

data$lev5Perc2012 <- scale(data$lev5Perc2012)

data$perc.free.lunch <- scale(data$perc.free.lunch)

data$perc.reduced.lunch <- scale(data$perc.reduced.lunch)



# ----------
# convert binary variables to factors

data$SeniorHigh <- factor(data$SeniorHigh)

data$middleHigh <- factor(data$middleHigh)




# ----------
# breakdown locale into size and type variables

data$locationSize <- with(data, ifelse(Locale.=="City: Large" |
                                         Locale.=="Suburb: Large", "Large", ifelse( Locale.=="City: Midsize"|
                                                                                      Locale.=="Suburb: Midsize", "Midsize", "Small")))  

data$locationSize <- factor(data$locationSize)

data$locationRural <- with(data, ifelse(Locale.=="Rural: Distant" |
                                          Locale.=="Rural: Fringe" |
                                          Locale.=="Rural: Remote", "Rural", "Urban"))  

data$locationRural <- factor(data$locationRural)





# ------------------------------------------------------------------------------
# Compute inverse probability weights:  conditioned by GPS
# ------------------------------------------------------------------------------


# numerator of weighting equation: marginal density of the treatment variable

data$numerator <- with(data, dnorm(logLoginsPerExaminee, 
                                   mean = mean(logLoginsPerExaminee), 
                                   sd = sd(logLoginsPerExaminee))) 



# compute IPW = marginal density / GPS  (conditioned by GPS)

data$IPW <- with(data, numerator / GPS)




# ----------
car::densityPlot(data$IPW)



summary(data$IPW)



# -->
# maximum weight is 7.068, so extreme weights are not a concern.




# ------------------------------------------------------------------------------
# Compute inverse probability weights by ipw:  conditioned by covariate
# ------------------------------------------------------------------------------

library(ipw)


IPW2 <- ipwpoint(exposure = logLoginsPerExaminee,
                 family = "gaussian", numerator = ~ 1,
                 denominator = ~ Charter + Magnet. + Title.I.School. +
                   locationRural + locationSize + Students. + 
                   SeniorHigh+numOfStud2014 + meanScale2012 +
                   lev1Perc2012 + lev5Perc2012 +
                   perc.free.lunch + perc.reduced.lunch, 
                 data = data)


data$IPW2 <- IPW2$ipw.weights



# ----------
# compare the two IPW

with(data, cor(cbind(IPW, IPW2)))




# ----------
car::densityPlot(data$IPW2)


summary(data$IPW)

summary(data$IPW2)



