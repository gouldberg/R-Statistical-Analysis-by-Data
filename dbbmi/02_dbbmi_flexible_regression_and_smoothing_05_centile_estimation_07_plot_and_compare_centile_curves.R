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
# Compare fitted centile curves
# ------------------------------------------------------------------------------


m2 <- lms(bmi, age, data = dbbmi1, trans.x = TRUE, families = c("SHASH"), n.cyc = 100)


centiles.com(m0, m2, xvar = dbbmi1$age, legend = TRUE, color = TRUE)


