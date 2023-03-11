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
# Compare fitted centile curves
# ------------------------------------------------------------------------------


m2 <- lms(head, age, data = db, trans.x = TRUE, families = c("SHASH"), n.cyc = 100)


centiles.com(m0_c, m2, xvar = db$age, legend = TRUE, color = TRUE)

centiles.com(m0_c, m01, xvar = db$age, legend = TRUE, color = TRUE)


