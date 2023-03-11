setwd("//media//kswada//MyFiles//R//dbbmi")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  dbbmi
# ------------------------------------------------------------------------------

data("dbbmi", package = "gamlss.data")


str(dbbmi)

car::some(dbbmi)



# ------------------------------------------------------------------------------
# Case3:  z-score given y and x
#   - We want to classify whether the individual is at risk
# ------------------------------------------------------------------------------

xval <- dbbmi$age

yval <- dbbmi$bmi


# ----------
mat3 <- centiles.pred(m0, xname = "age", xval = xval, yval = yval, type = "z-scores")

head(mat3)



# alternatively
# z.scores() is applicable only to LMS model
z.scores(m0, x = xval, y = yval)



# ----------
hist(mat3)
