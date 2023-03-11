setwd("//media//kswada//MyFiles//R//tension")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  tension
# ------------------------------------------------------------------------------

data("tension", package = "MPsychoR")


str(tension)



# ----------
# convert to fdata object
library(fda.usc)

# tension time series
tension1 <- as.matrix(tension[, 1:800])

cond <- tension$condition


ftension <- fdata(tension1, argvals = seq(1, 80, length.out = 800),
                  names = list(main = "Music tension", xlab = "Time (sec)", ylab = "Tension"))


ftension




# ------------------------------------------------------------------------------
# Explore to which degree a location function changes across multiple samples by bootstrap
# ------------------------------------------------------------------------------

fsplit$AuditoryVisual$names$main <- NULL

fsplit$Auditory$names$main <- NULL

fsplit$Visual$names$main <- NULL


set.seed(123)



# ----------
graphics.off()
par(mfrow = c(1,2))

control = list("col" = c("darkgrey", "blue", "cyan"), "lty" = c(2,1,1), "lwd" = c(1,3,1))


AVboot <- fdata.bootstrap(fsplit$AuditoryVisual, draw = TRUE, draw.control = control)

title("Bootstrap Mean (Auditory-Visual)")



AVboot2 <- fdata.bootstrap(fsplit$Auditory, draw = TRUE, draw.control = control)

title("Bootstrap Mean (Auditory)")



AVboot3 <- fdata.bootstrap(fsplit$Visual, draw = TRUE, draw.control = control)

title("Bootstrap Mean (Visual)")








