setwd("//media//kswada//MyFiles//R//crabs")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  crabs
# ------------------------------------------------------------------------------

data("crabs", package = "MASS")


str(crabs)


car::some(crabs)



# ------------------------------------------------------------------------------
# Confidence interval and prediction interval
# ------------------------------------------------------------------------------

x <- crabs$FL[1:50]

new <- data.frame(x = seq(max(crabs$FL[1:50]), min(crabs$FL[1:50]), length =50))

pred.int <- predict(lm(crabs$RW[1:50] ~ x), new, interval = "confidence")

pred.fit <- predict(lm(crabs$RW[1:50] ~ x), new, interval = "prediction")

plot(crabs$FL[1:50], crabs$RW[1:50], pch = 21, cex = 1.8, bg = "grey50", asp = 1)

matplot(new$x, cbind(pred.int), lty = c(1,2,2), type = "l", col = 1, add = T)

matplot(new$x, cbind(pred.fit), lwd = c(1,2,2), lty = c(1,2,2), col = c("black", rep("grey50", 2)), type = "l", add = T)




