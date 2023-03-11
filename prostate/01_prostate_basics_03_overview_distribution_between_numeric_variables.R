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
# Overview distribution of each variable (numeric, integer) and correlation among variables
# ------------------------------------------------------------------------------
library(gpairs)
library(vcd)

( var <- setdiff(unique(var_num, var_int), var_bin) )

# marical and conditional plots
gpairs(data[,var],
       diag.pars = list(fontsize = 16, hist.color ="lightgray",
                        mosaic.pars = list(gp = shading_Friendly, gp_args = list(interpolate=1:4)),
                        outer.rot = c(45,45)))

car::scatterplotMatrix(data[,var])



# ----------
# check correlation
psych::pairs.panels(data[, var])

