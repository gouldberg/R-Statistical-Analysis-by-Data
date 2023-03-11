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
# plot fitted centile curves for y against x for different centile percentages
# ------------------------------------------------------------------------------

centiles(m03, db$age, legend = FALSE)


# -->
# Note that the sample percentage of observations below each of the fited centile curves from the fitted model
# are printed (at the end of each line,
# so comparisons with nominal model centiles (printed after below on each line)



# ------------------------------------------------------------------------------
# Get infromation on how well a distribution fits at a particular age from centiles()
# ------------------------------------------------------------------------------

sub1 <- subset(db, age < 3)


h1 <- gamlssML(head, data = sub1, family = BCTo)


centiles(h1, sub1$age, cent = c(1, 2.5, 10, 25, 50, 75, 90, 97.5, 99), legend = FALSE)


# -->
# The table shows that the fit did not capture the lower tail of the BMI distributiion well, but captured the upper tail far better



# ----------
index <- 1:(nrow(sub1))

centiles(h1, index, legend = F)
