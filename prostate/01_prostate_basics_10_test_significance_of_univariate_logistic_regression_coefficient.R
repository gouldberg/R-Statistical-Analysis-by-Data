setwd("//media//kswada//MyFiles//R//prostate")

packages <- c("dplyr", "Hmisc", "rms", "lattice", "ggplot2")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  prostate
# ------------------------------------------------------------------------------

getHdata(prostate)

str(prostate)

car::some(prostate)


# ----------
# convert an old data format to R format
prostate$sdate <- as.Date(prostate$sdate)
data <- prostate
str(data)


data$cvd <- data$status %in% c("dead - heart or vascular", "dead - cerebrovascular")



# ------------------------------------------------------------------------------
# conversion
# ------------------------------------------------------------------------------

# combining an infrequent category with the next category, and dichotomizing ekg

levels(data$ekg)[levels(data$ekg) %in% c("old MI", "recent MI")] <- "MI"

data$ekg.norm <- 1 * (data$ekg %in% c("normal", "benign"))

levels(data$ekg) <- abbreviate(levels(data$ekg))

data$pfn <- as.numeric(data$pf)

levels(data$pf) <- levels(data$pf)[c(1,2,3,3)]

data$rxn <- as.numeric(data$rx)



# ------------------------------------------------------------------------------
# Test for the significance of coefficients
# Univariale logistic regression
# ------------------------------------------------------------------------------
source("//media//kswada//MyFiles//R//utilities//UnivarLogiReg.R")


# ----------
# set variable names
x_var <- c("rx","dtime","age","wt","pf","hx","sbp","dbp","ekg","hg","sz","sg","ap","bm")

y_var <- "cvd"



# ----------
crit <- c(0.25, 0.05, 0.001)

uni_lr <- UnivarLogiReg(y_var = y_var, x_var = x_var, data = data, crit = crit)

uni_lr
# p.value of wt > 0.25
# It seems that ap needs transformation by log


