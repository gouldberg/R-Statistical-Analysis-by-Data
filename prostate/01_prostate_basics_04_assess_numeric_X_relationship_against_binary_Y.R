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
# Assess numeric X relationship against binary Y
# ------------------------------------------------------------------------------
( var <- setdiff(unique(var_num, var_int), var_bin) )

psych::pairs.panels(data[, c(var, "cvd")], bg=c("blue", "yellow")[data$cvd])

car::scatterplotMatrix(data[, c(var, "cvd")])



# ----------
# Margianl plots to check relationship X against Y
op <- par(mfrow=c(2,2), mar = c(4,4,1,2.5) + .1 , cex.lab = 1.4)
col <- c("lightblue", "blue")
par(mfrow=c(2,2))
plot(cvd ~ age, data = data, col = col)
plot(cvd ~ wt, data = data, col = col)
plot(cvd ~ sbp, data = data, col = col)
plot(cvd ~ dbp, data = data, col = col)
plot(cvd ~ hg, data = data, col = col)
plot(cvd ~ sz, data = data, col = col)
plot(cvd ~ sg, data = data, col = col)
plot(cvd ~ ap, data = data, col = col)


