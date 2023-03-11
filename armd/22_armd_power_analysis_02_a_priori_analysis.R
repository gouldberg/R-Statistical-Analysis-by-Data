
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\armd")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# data:  ARMD (Age-Related Macular Degeneration Trial)
# ------------------------------------------------------------------------------


armd.wide <- read.csv("armd.wide.txt", header = T, sep = "\t")


armd0 <- read.csv("armd0.txt", header = T, sep = "\t")



str(armd.wide)


str(armd0)



# ----------
data("armd", package = "nlmeU")


str(armd)




# ------------------------------------------------------------------------------
# A Priori Power Calculations for a Hypothetical Study
#  - Constructing an exemplary dataset for an a priori power analysis of the treatment effect in a hypothetical study
# ------------------------------------------------------------------------------


# create an exemplary dataset


npg <- 20

subject <- 1:(2 * npg)


treat.f <- gl(2, npg, labels = c("Placebo", "Active"))

treat.f



# subject-level data
dts <- data.frame(subject, treat.f)


dtL <- list(time = c(4, 12, 24, 52), subject = subject)


dtLong <- expand.grid(dtL)


mrgDt <- merge(dtLong, dts, sort = FALSE)


exmpDt <- within(mrgDt, 
                 {
                   # Under H0
                   m0 <- 65 - 0.1 * time
                   
                   # Under HA
                   mA <- 85 - 0.1 * time
                   
                   mA <- ifelse(treat.f %in% "Active", mA, m0)
                 })


head(exmpDt)



# ----------

selDt <- with(exmpDt,
              {
                lvls <- levels(treat.f)
                
                i <- match(lvls, treat.f)
                
                subj <- subject[i]
                
                subset(exmpDt, subject %in% subj)
              })



xyplot(mA ~ time,
       groups = treat.f,
       data = selDt,
       type = "l",
       auto.key = list(lines = TRUE, points = FALSE),
       grid = TRUE
       )




# ------------------------------------------------------------------------------
# A Priori Power Calculations for a Hypothetical Study
#  - Constructing an object of class lme representing the alternative model
# ------------------------------------------------------------------------------


# object of class pdMat and varPower to define variance-covariance structures


D0 <- diag(c(100, 0.09))


sgma <- 5


( D <- D0 / (sgma * sgma) )


( pd1 <- pdDiag(D, form = ~ time, data = armd) )


( vF <- varPower(form = ~time, fixed = 0.15) )




# ----------
# fitting the model to the exemplary data using the lme() function with 0 iterations


cntrl <- lmeControl(maxIter = 0, msMaxIter = 0, niterEM = 0, returnObject = TRUE, opt = "optim")


fmA <- lme(mA ~ time + treat.f,
           random = list(subject = pd1),
           weights = vF,
           data = exmpDt,
           control = cntrl)


fixef(fmA)


sigma(fmA)





# ------------------------------------------------------------------------------
# A Priori Power Calculations for a Hypothetical Study
#  - The use of the Pwr() function to perform the a priori power calculations
#    for the treatment effect in a hypothetical study
# ------------------------------------------------------------------------------


# Power calculations using the Pwr() function

Pwr(fmA, sigma = sgma, L = c("treat.fActive" = 1))




# ----------
# Use of the altB argument to create data for plotting the power curve


dif <- seq(1, 15, by = 0.1)

dim(dif) <- c(length(dif), 1)

colnames(dif) <- "treat.fActive"


dtF <- Pwr(fmA, sigma = sgma, L = c("treat.fActive" = 1), altB = dif)


dtF[,1:4]



# ----------
# plotting the power curve
lattice::xyplot(Power ~ treat.fActive,
                data = dtF, type = "l",
                autoo.key = list(lines = TRUE, points = FALSE),
                grid = TRUE)





# ------------------------------------------------------------------------------
# 2nd approach: simulation
# Simulation of the F-test statistics based on the model-fit object
# ------------------------------------------------------------------------------


simA <- simulateY(fmA, sigma = sgma, nsim = 1000)



dt <- exmpDt



simfmA <- apply(simA, 2, 
                function(y){
                  dt$mA <- y
                  auxFit <- update(fmA, data = dt)
                  anova(auxFit)
                })



# First ANOVA
simfmA[[1]]




# ------------------------------------------------------------------------------
# Empirical power of the F-test for the treatement effect based on the simulated values of the F-test statistics
# ------------------------------------------------------------------------------


FstatE <- sapply(simfmA, function(x) x["treat.f", "F-value"])



summary(FstatE)




Fcrit <- qf(1 - 0.05, 1, 38, ncp = 0)


( nsim <- length(FstatE) )


( powerE <- sum(FstatE > Fcrit) / nsim )



