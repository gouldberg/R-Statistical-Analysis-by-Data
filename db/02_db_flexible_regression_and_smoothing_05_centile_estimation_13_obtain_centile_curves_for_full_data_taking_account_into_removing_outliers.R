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
# Calibration
#   - One solution to the distorted (*) centile percentages is to use calibration
#     (*) a difference between the nominal model percentages and the sample percentages below the centile curve
# ------------------------------------------------------------------------------

centiles(m0_c, xvar = db$age, legend = F)

calibration(m0_c, xvar = db$age)



# ------------------------------------------------------------------------------
# Remove outliers and refit
# ------------------------------------------------------------------------------

which(resid(m0_c) > 3.5)

which(resid(m0_c) < -3.5)


dbsub <- subset(db, (resid(m0_c) > -3.5) & (resid(m0_c) < 3.5))


m03_sub <- gamlss(head ~ pb(age ^ m0_c$power), sigma.fo = ~ pb(age ^ m0_c$power), nu.fo = ~ pb(age ^ m0_c$power), tau.fo = ~ pb(age ^ m0_c$power), 
                  family = BCTo, data = dbsub)


# ----------
graphics.off()

wp(m03_sub, xvar = dbsub$age, ylim.worm = 1.5, n.inter = 9)



# ----------
Q.stats(m03_sub, xvar = dbsub$Tage, n.inter = 20)



# ----------
# cut at Tage
centiles.split(m03_sub, xvar = dbsub$Tage, xcut.points = c(1.0, 1.2, 2.4, 2.5))



# ----------
# Compare centile curves
centiles(m0_c, xvar = db$age, legend = F)

centiles(m03_sub, xvar = dbsub$age, legend = F)

calibration(m03_sub, xvar = dbsub$age)



