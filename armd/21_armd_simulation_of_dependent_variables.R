
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
# Simulation of the Dependent variable
# ------------------------------------------------------------------------------

library(nlmeU)


simY <- simulateY(fm16.5ml, nsim = 1000)


auxDt <- subset(armd, select = c(subject, visual, visual0, time, treat.f))


# IT TAKES TIME !!!:  3 min.
simYsumm <- apply(simY, MARGIN = 2, 
                  FUN = function(y){
                    
                    # dependent variable updated
                    auxDt$visual <- y
                    
                    # update model with new Y
                    auxFit <- update(fm16.5ml, data = auxDt)
                    summ <- summary(auxFit)
                    beta <- fixef(summ)
                    list(beta = beta)
                  })



# beta for 1st simulation
simYsumm[[1]]




# ----------
# summary statistics of the empirical distribution of beta for model


# matrix with beta
( betaE <- sapply(simYsumm, FUN = function(x) x$beta) )


# empirical beta
rowMeans(betaE)


# empirical var(beta)
cov(t(betaE))




