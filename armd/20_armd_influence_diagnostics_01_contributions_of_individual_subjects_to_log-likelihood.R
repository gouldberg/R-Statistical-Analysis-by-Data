
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
# Extracting selected results for mdodel M16.5
# ------------------------------------------------------------------------------

library(lme4)


fm16.5ml <- update(fm16.5, method = "ML")



# recall model formula
formula(fm16.5ml)



# recall data name
fm16.5ml$call$data



# ----------
# log-ML value
# Note that the number of degrees of freedom reported by lokLik() is equal to 8.
# It corresponds to the total number of the parameters in the model,
# i.e., four fixed-effects coefficients (beta),
# four variance-covariance parameters describing the diagonal matri D,
# one parameter (delta) related to the power variance function describing the diagonal matrix R
# and the scale parameter sigam

logLik(fm16.5ml)




# ----------
# fixed effects estimates and their variance-covariance matrix
beta0 <- fixef(fm16.5ml)

names(beta0)

names(beta0) <- abbreviate(names(beta0), minlength = 7)

beta0

vcovb <- vcov(fm16.5ml)

vcovb



# ------------------------------------------------------------------------------
# contributions of individual subjecs to the log-likelihood for the model:  illustration purpose
# ------------------------------------------------------------------------------


library(nlmeU)


# data for subject "1"
df1 <- subset(armd, subject %in% "1")



# ----------
# calculate the contribution of one subject in the data to the overall log-likelihood
# logLik, for subject "1"

logLik1(fm16.5ml, df1)




# ----------
lLik.i <- by(armd, armd$subject,
             FUN = function(dfi) logLik1(fm16.5ml, dfi))



lLik.i <- as.vector(lLik.i)


# logLik(i) for the first 5 subjects
lLik.i[1:5]


sum(lLik.i)




# ----------
# plot of individual contributions to the log-likelihood


nx <- by(armd, armd$subject, nrow)

lLik.n <- lLik.i / as.vector(nx)


outL <- lLik.n < -6

lLik.n[outL]


subject.c <- levels(armd$subject)

subject.x <- as.numeric(subject.c)

plot(lLik.n ~ subject.x, type = "h")

points(subject.x[outL], lLik.n[outL], type = "p", pch = 16)

text(subject.x[outL], lLik.n[outL], subject.c[outL])




# ------------------------------------------------------------------------------
# model fitted to a sequence of "leave-one-subject-out" (LOO) datasets
# ------------------------------------------------------------------------------

# creating the object lmeUall containing models

lmeU <- function(cx){
  dfU <- subset(armd, subject != cx)
  
  update(fm16.5ml, data = dfU)
}



# list with Leave-One-Subject-Out fits
lmeUall <- lapply(subject.c, lmeU)


names(lmeUall) <- subject.c




# ----------
# exploring the contents of the lmeUall objects

names(lmeUall)[1:6]

dataU6 <- lmeUall[["6"]]$data


dim(dataU6)


unique(dataU6$subject)[1:6]




# ------------------------------------------------------------------------------
# Likelihood displacements for model
#   - twice the difference between the log-likelihood computed at a maximum and displaced values of estimated parameters
# ------------------------------------------------------------------------------

# caltulating of the likelihood displacements

lLik <- function(cx){
  
  lmeU <- lmeUall[[cx]]
  
  lLikU <- logLik(lmeU, REML = FALSE)
  
  df.s <- subset(armd, subject == cx)
  
  lLik.s <- logLik1(lmeU, df.s)
  
  return(lLikU + lLik.s)
}



# for all subject
lLikUall <- sapply(subject.c, lLik)


dif.2Lik <- 2 * (logLik(fm16.5ml) - lLikUall)


summary(dif.2Lik)




# ----------
# plot of the likelihood displacements with an indicatoin of outlying values

names(dif.2Lik) <- subject.c

outL <- dif.2Lik > 0.5


dif.2Lik[outL]


subject.f <- factor(subject.c, levels = subject.c)


myPanel <- function(x, y, ...){
  x1 <- as.numeric(x)
  
  panel.xyplot(x1, y, ...)
  
  ltext(x1[outL], y[outL], subject.c[outL])
}



dtp <- lattice::dotplot(dif.2Lik ~ subject.f, panel = myPanel, type = "h")

lxlims <- length(dtp$x.limits)

update(dtp, xlim = rep("", lxlims), grid = "h")




# -->
# the seven subjects with the likelihood-displacement values larger than 0.5 are clearly identified.


