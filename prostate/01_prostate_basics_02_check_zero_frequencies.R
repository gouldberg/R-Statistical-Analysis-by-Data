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
# Check zero frequencies
#   - zero frequency cells have potential impact to logistic regression's predictive performance
# ------------------------------------------------------------------------------

# check sum(is.na)
sum(is.na(data))
check_sumisna <- function(data){ sum(is.na(data)) }
apply(data, FUN=check_sumisna, MARGIN=2)



# ----------
# Check zero frequency cells  (potential impact to logistic regression's predictive performance)
dat.tab <- xtabs(~ rx + pf + cvd, data = data)
ftable(dat.tab)

dat.tab <- xtabs(~ stage + bm + hx + cvd, data = data)
ftable(dat.tab)

ftable(dat.tab, row.vars = c(1,2))


