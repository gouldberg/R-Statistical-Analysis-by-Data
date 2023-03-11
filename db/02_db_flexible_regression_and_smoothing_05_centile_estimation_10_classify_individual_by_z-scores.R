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
# Case3:  z-score given y and x
#   - We want to classify whether the individual is at risk
# ------------------------------------------------------------------------------

yval <- db$head

xval <- db$age


# ----------
graphics.off()
mat3 <- centiles.pred(m0_c, xname = "age", xval = xval, yval = yval, type = "z-scores", plot = T)

head(mat3)



# alternatively
# z.scores() is applicable only to LMS model
z.scores(m0_c, x = xval, y = yval)



# ----------
hist(mat3)
