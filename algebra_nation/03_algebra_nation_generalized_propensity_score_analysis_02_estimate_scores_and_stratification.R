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
# Define model and regression
# ------------------------------------------------------------------------------
  

# define model:  treat ~ covariate

formula1 <- formula("logLoginsPerExaminee ~ Charter + Magnet. +
  Title.I.School. + locationRural + locationSize+Students. +
  SeniorHigh + numOfStud2014 + meanScale2012 + lev1Perc2012 +
  lev5Perc2012 + perc.free.lunch+perc.reduced.lunch")




# ----------
# fit regression model for treatment doses

mod <- lm(formula = formula1, data = data)



summary(mod)



car::vif(mod)



car::residualPlots(mod)




# ------------------------------------------------------------------------------
# Estimate generalized propensity score
# ------------------------------------------------------------------------------


# conditional density of fitted(mod)

data$GPS <- dnorm(data$logLoginsPerExaminee, 
                  mean = fitted(mod), sd = sd(data$logLoginsPerExaminee))



car::densityPlot(data$GPS)




# ------------------------------------------------------------------------------
# GPS distribution by strata
# ------------------------------------------------------------------------------

data$strataGPS <- with(data, cut(GPS, include.lowest = T, labels = 1:5,
                                breaks = quantile(GPS, probs = seq(0, 1, 0.2))))


lattice::histogram(~ GPS | strataGPS, data = data)



with(data, by(GPS, strataGPS, summary))


