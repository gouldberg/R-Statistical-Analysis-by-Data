setwd("//media//kswada//MyFiles//R//prostate")

packages <- c("dplyr", "Hmisc", "rms", "lattice", "ggplot2")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  prostate
#   - 506 patient prostate cancer dataset from Byar and Green. These data were from a randomized trial comparing four treatments
#     for stage 3 and 4 prostate cancer, with almost equal numbers of patients on placebo and each of three does of estrogen.
#   - Four patients had missing values on all of the following variables: wt, pf, hx, sbp, dbp, ekg, hg, bm
#     Two of these patients were also missing sz. These patients are excluded from consideration.
#   - The ultimate goal of an analysis of the dataset might be to discover patterns in survival or to do an analysis of covariance to assess
#     the effect of treatment while adjusting for patient heterogeneity.
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
# basics
# ------------------------------------------------------------------------------

Hmisc::describe(data)

Hmisc::describe(data$status)

# -->
# Check for description of "status"



# ----------
# repsonse variable
addmargins(table(data$status))


# -->
# There are 354 deaths among the 502 patients.
# If predicting survival time were of major interest, we could develop a reliable model if no more than about 354/15 = 24 parameters
# were EXAMINED against Y in unpenalized modeling.



# ----------
psych::describe(data)

car::scatterplotMatrix(data)



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
# Categorize variable names by class or type
# ------------------------------------------------------------------------------

var_fac <- sapply(1:ncol(data), function(x) is.factor(data[,x]))
( var_fac <- colnames(data)[var_fac] )

var_char <- sapply(1:ncol(data), function(x) is.character(data[,x]))
( var_char <- colnames(data)[var_char] )

var_num <- sapply(1:ncol(data), function(x) is.numeric(data[,x]))
( var_num <- colnames(data)[var_num] )

var_int <- sapply(1:ncol(data), function(x) is.integer(data[,x]))
( var_int <- colnames(data)[var_int] )

var_bin <- c("stage", "bm", "hx", "ekg.norm")

var_cat <- c("sg", "dbp", "sbp", "rx", "pf", "status", "ekg")


