setwd("//media//kswada//MyFiles//R//wish")

packages <- c("dplyr", "smacof")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  wish
# ------------------------------------------------------------------------------

data(wish, package = "smacof")


wish


car::some(wish)



# ------------------------------------------------------------------------------
# Compare variants of MDS models
# ------------------------------------------------------------------------------

wish2 <- (9 - wish) / 8


# method = 1: "corr"
diss <- sim2diss(wish2)

# method = 7 is converting "cooccurence" similarities into dissimilarities
diss_7 <- sim2diss(wish, method = 7)



# ----------
fit.interval <- mds(wish2, type = "interval")

fit.interval2 <- mds(diss, type = "interval")

fit.ordinal <- mds(diss_7, type = "ordinal")

# type = "ratio" is the default
fit.ratio <- mds(diss)

fit.spline <- mds(diss, type = "mspline")



# ----------
# Shepard Diagram
op <- par(mfrow = c(2,3))

plot(fit.interval, plot.type = "Shepard", main = paste("Stress", round(fit.interval$stress, 2)))

plot(fit.interval2, plot.type = "Shepard", main = paste("Stress", round(fit.interval2$stress, 2)))

plot(fit.ordinal,  plot.type = "Shepard", main = paste("Stress", round(fit.ordinal$stress, 2)))

plot(fit.ratio,    plot.type = "Shepard", main = paste("Stress", round(fit.ratio$stress, 2)))

plot(fit.spline,   plot.type = "Shepard", main = paste("Stress", round(fit.spline$stress, 2)))

par(op)



# ----------
# Configurations
op <- par(mfrow = c(2,3))

plot(fit.interval, main = paste("Stress", round(fit.interval$stress, 2)))  # --> second

plot(fit.interval2, main = paste("Stress", round(fit.interval2$stress, 2)))

plot(fit.ordinal,  main = paste("Stress", round(fit.ordinal$stress, 2)))  # --> best

plot(fit.ratio,    main = paste("Stress", round(fit.ratio$stress, 2)))

plot(fit.spline,   main = paste("Stress", round(fit.spline$stress, 2)))

par(op)



# -->
# Ordinal MDS and interval MDS arrive at similar solutions
