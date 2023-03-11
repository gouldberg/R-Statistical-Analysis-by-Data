setwd("//media//kswada//MyFiles//R//intelligence")

packages <- c("dplyr", "smacof")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  intelligence
# ------------------------------------------------------------------------------

data(intelligence, package = "smacof")


intelligence

car::some(intelligence)



# ------------------------------------------------------------------------------
# Compare variants of MDS models
# ------------------------------------------------------------------------------

idiss <- sim2diss(intelligence[,paste0("T", 1:8)])



# ----------
fit.interval <- mds(idiss, type = "interval")

fit.ordinal <- mds(idiss, type = "ordinal")

# type = "ratio" is the default
fit.ratio <- mds(idiss)

fit.spline <- mds(idiss, type = "mspline")



# ----------
# Shepard Diagram
op <- par(mfrow = c(1,4))

plot(fit.interval, plot.type = "Shepard", main = paste("Stress", round(fit.interval$stress, 2)))

plot(fit.ordinal,  plot.type = "Shepard", main = paste("Stress", round(fit.ordinal$stress, 2)))

plot(fit.ratio,    plot.type = "Shepard", main = paste("Stress", round(fit.ratio$stress, 2)))

plot(fit.spline,   plot.type = "Shepard", main = paste("Stress", round(fit.spline$stress, 2)))

par(op)



# ----------
# Configurations
op <- par(mfrow = c(2,2))

plot(fit.interval, main = paste("Stress", round(fit.interval$stress, 2)))  # --> 3rd

plot(fit.ordinal,  main = paste("Stress", round(fit.ordinal$stress, 2)))  # --> best

plot(fit.ratio,    main = paste("Stress", round(fit.ratio$stress, 2)))

plot(fit.spline,   main = paste("Stress", round(fit.spline$stress, 2)))  # --> second

par(op)



# -->
# Ordinal MDS and interval MDS arrive at similar solutions
