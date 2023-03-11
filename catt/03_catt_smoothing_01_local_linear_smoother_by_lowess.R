setwd("//media//kswada//MyFiles//R//catt")

packages <- c("dplyr", "fda", "refund")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  CATT
# ------------------------------------------------------------------------------

# VAS Longitudinal Measurement
Data <- read.csv("va.csv")


# VAS Baseline Measurements
Base <- read.csv("bv.csv")


# Other Subject Information
general_info <- read.csv("gi.csv")


str(Data)

str(Base)

str(general_info)



# ----------
car::some(Data)

car::some(Base)

car::some(general_info)



# ------------------------------------------------------------------------------
# Preprocess data
# ------------------------------------------------------------------------------

# Each row corresponds to a particular subject, so we start by pulling off the subject ids for each dataset.
IdData <- Data[,1]
IdBase <- Base[,1]
IdGeneral <- general_info[,1]



# Unique ids
Code_levels <- levels(IdBase)


# Number of subjects
n <- length(Code_levels)


# The "time" variable which is given bby the week of the study
Week <- Data[,"week"]


# Unique weeks
Week_levels <- levels(factor(Week))


# Select Visual Acuity values
VA <- Data[,"studyeye_va"]
VAB <- Base[,"studyeye_va"]
Age <- general_info[,"age"]


Y_1 <- by(c(VAB, VA), c(IdBase, IdData), c)
T_1 <- by(c(rep(0, times = n), Week), c(IdBase, IdData), c)
counts <- sapply(Y_1, length, simplify = TRUE)



# Drop those with only a baseline measurement
( Y_1 <- Y_1[counts != 1] )
( T_1 <- T_1[counts != 1] )
counts <- counts[counts != 1]
n <- length(counts)



# ------------------------------------------------------------------------------
# Fit a local linear smoother using loess
# ------------------------------------------------------------------------------

set.seed(2016)

n_fit <- floor(n / 2)


# number of lambdas
r = 20
( lambda_all <- 0.2 * 10^seq(0, 2, length = r) )



# ----------
# randomly divide sample into a testing atraining set
Fit_sample <- sample(1:n, n_fit)

Fit <- cbind(unlist(T_1[Fit_sample]), unlist(Y_1[Fit_sample]))
Fit <- na.omit(Fit)

Test <- cbind(unlist(T_1[-Fit_sample]), unlist(Y_1[-Fit_sample]))
Test <- na.omit(Fit)



# ----------
# Mean Prediction Error
MPE <- numeric(0)


# check fir for each lambda
for(lambda in lambda_all){
  
  loess_tmp <- loess(Fit[,2] ~ Fit[,1], span = lambda, degree = 1)
  
  pred <- predict(loess_tmp, newdata = Test[,1])
  
  MPE <- c(MPE, mean((pred - Test[,2]) ^ 2))
}


par(mfrow = c(1,1))
plot(MPE ~ lambda_all, type = "b")



# ----------
# select best lambda and refit with all data
( lambda <- lambda_all[which.min(MPE)] )

All <- rbind(Fit, Test)

LLF <- loess(All[,2] ~ All[,1], span = lambda, degree = 1)


summary(LLF)



# ----------
par(mfrow=c(1,1))
plot(All[,1], All[,2], pch = "*", col = gray(0.8))
lines(fitted(LLF))


