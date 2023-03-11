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
# Obtain centile curves for the full data set db at centiles given by cent
#  - If the outliers are belived to be genuine observations, then centile curves should be obtained for the full data set db.
#    The centile curve percentages for m03_sub need to be adjusted for the cases removed from each tail.
# ------------------------------------------------------------------------------

cent <- c(0.4, 2, 10, 25, 50, 75, 90, 98, 99.6)



# ----------
# lower percentage removed
a <- (2 / 7040) * 100

# upper percentage removed
b <- (7 / 7040) * 100


( newcent <- (cent - a) / (1 - (a + b) / 100) )


centiles(m03_sub, xvar = dbsub$age, cent = cent, legend = F)

centiles(m03_sub, xvar = dbsub$age, cent = newcent, legend = F)

centiles(m03_sub, xvar = dbsub$age, cent = newcent, legend = F)

