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
# Transform using nonparametric smoothers
#   - The ACE (alternating conditional expectation) nonparametric additive regression method of Breiman and Friedman
#     transforms both the left-hand-side variable and all the right-hand-side variables so as to optimize R^2.
#   - Hmisc::transace() calls acepack package.
#   - ACE does not handle missing values, so here transace is run after single imputation bby transcan.
# ------------------------------------------------------------------------------

x <- with(imp, cbind(sz, sg, ap, sbp, dbp, age, wt, hg, ekg, pf, bm, hx))


# Several predictors are restricted to be monotonically transformed, so we should tell to transace() the vectors of variable names
# specifying what to assume about each column of x for transace. Binary variables are not transformed.
mono <- c("sz", "sg", "ap", "sbp", "dbp", "age", "pf")
cat <- c("ekg")
bin <- c("bm", "hx")



# ----------
acet <- transace(x, monotonic = mono, categorical = cat, binary = bin)


head(acet)



# -->
# Except for ekg, age, and for arbitrary sign reversals, the transformations using transace were similar to those by transcan()
# The transcan transformation for ekg makes more sense.

