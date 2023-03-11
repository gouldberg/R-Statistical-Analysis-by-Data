setwd("//media//kswada//MyFiles//R//db")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  db
# ------------------------------------------------------------------------------
data("db", package = "gamlss.data")


str(db)

car::some(db)



# ------------------------------------------------------------------------------
# Check the outliers
# ------------------------------------------------------------------------------

# There are 7 extreme outlies with residuals greater than 3.5
sum(abs(resid(m0_c) >= 3.5))



# ----------
graphics.off()

wp(m0_c, xvar = db$age, ylim.worm = 1.5, n.inter = 9)



# ----------
Q.stats(m0_c, xvar = db$Tage, n.inter = 20)



# ----------
# cut at Tage
centiles.split(m0_c, xvar = db$Tage, xcut.points = c(1.0, 1.2, 2.4, 2.5))

