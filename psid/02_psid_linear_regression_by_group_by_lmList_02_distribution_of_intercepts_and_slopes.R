setwd("//media//kswada//MyFiles//R//psid")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  psid
# ------------------------------------------------------------------------------

data("psid", package = "faraway")


str(psid)

dim(psid)


car::some(psid)



# ------------------------------------------------------------------------------
# the distribution of intercepts and slopes
# ------------------------------------------------------------------------------

par(mfrow = c(1, 1))

plot(intercepts, slopes, xlab = "Intercept", ylab = "Slope")



# ----------
formula <- ~ slopes + intercepts

scatterplotMatrix(formula, data = df,
                  smooth = FALSE,
                  id = list(n = 3), ellipse = TRUE, col = gray(0.3), pch = 20)



# ----------
formula <- ~ slopes + intercepts | sex

scatterplotMatrix(formula, data = df,
                  smooth = FALSE,
                  id = list(n = 3), ellipse = TRUE, col = c("black", gray(0.7)), pch = c(1, 20))


# -->
# women has higher income growth rate than men (slopes are larger)
# men has higher income than women (intercepts are larger)



# ----------
formula <- ~ slopes + intercepts | edulevel

scatterplotMatrix(formula, data = df,
                  smooth = FALSE,
                  id = list(n = 3), ellipse = TRUE, col = c("black", gray(0.7), "blue"), pch = c(1, 2, 20))


# -->
# women has higher income growth rate than men (slopes are larger)
# men has higher income than women (intercepts are larger)



# ------------------------------------------------------------------------------
# the distribution of slopes by sex
# ------------------------------------------------------------------------------

psex <- psid$sex[match(1:85, psid$person)]

boxplot(split(slopes, psex))




# ------------------------------------------------------------------------------
# the distribution of intercepts and slopes by education level
# ------------------------------------------------------------------------------

boxplot(split(intercepts, df$edulevel))

boxplot(split(slopes, df$edulevel))



