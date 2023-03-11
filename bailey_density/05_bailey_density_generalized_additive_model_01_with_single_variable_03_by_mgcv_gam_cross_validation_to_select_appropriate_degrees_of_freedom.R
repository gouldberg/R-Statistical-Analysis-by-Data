# setwd("//media//kswada//MyFiles//R//bailey_density")
setwd("//media//kswada//MyFiles//R//Generalized_additive_model//bailey_density")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Bailey Density
# ------------------------------------------------------------------------------

DF1 <- read.table(file = "BaileyDensity.txt", header = TRUE)


str(DF1)


dim(DF1)


car::some(DF1)



# ----------
# missing values for the spatial coordinates to be removed
colSums(is.na(DF1))


DF  <- na.exclude(DF1)



# ------------------------------------------------------------------------------
# GAM with cross validation for value of degree of freedom
#   - mgcv package comes with useful tool of cross-validation that automatically chooses the amount of smoothing for a smoother 
# ------------------------------------------------------------------------------

# to apply cross-validation, drop the fx and k
M5 <- mgcv::gam(Dens ~ s(MeanDepth), data = DF)


summary(M5)


# -->
# resulted in 5.62 degrees of freedom for the smoother.



# ----------
# resulting smoother.
par(mar = c(5,5,3,3))

plot(M5, cex.lab = 1.5, shade = TRUE)




# ----------
# fitted values  --> shifted upward by intercept

plot(M5, shift = coef(M5)[1], shade = TRUE)



# -->
# Note that still the confidence interval is lower than zero...


