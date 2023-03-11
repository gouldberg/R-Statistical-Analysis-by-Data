setwd("//media//kswada//MyFiles//R//eqexp")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  eqexp
#   - Various bivariate earthquakes (EQ) and explosions (EX) recorded at 40 pts/s including an even NZ (Novaya Zemlya)
# ------------------------------------------------------------------------------

data(eqexp, package = "astsa")


head(eqexp)

str(eqexp)


attach(eqexp)



# ----------
P <- 1:1024

S <- P + 1024


x <- cbind(EQ5[P], EQ6[P], EX5[P], EX6[P], NZ[P], EQ5[S], EQ6[S], EX5[S], EX6[S], NZ[S])

x.name <- c("EQ5-P", "EQ6-P", "EX5-P", "EX6-P", "NZ-P", "EQ5-S", "EQ6-S", "EX5-S", "EX6-S", "NZ-S")

colnames(x) <- x.name



# ------------------------------------------------------------------------------
# Classify new observation
# ------------------------------------------------------------------------------

new.data <- cbind(NZ.P, NZ.S)

d <- sum(d.slopes * new.data) + d.inter

( post.eq <- exp(d) / (1 + exp(d)) )




# print(disc function, posteriors) and plot results
graphics.off()

par(mfrow=c(1,1))

cat(d.slopes[1], "mag.P + ", d.slopes[2], "mag.S +", d.inter, "\n")
cat("P(EQ|data) = ", post.eq, "  P(EX|data) = ", 1 - post.eq, "\n")

plot(eq.P, eq.S, xlim = c(0, 1.5), ylim = c(0.75, 1.25), xlab = "log mag(P)", ylab = "log mag(S)", pch = 8, cex = 1.1, lwd = 2, main = "Classification Based on Magnitude Features")
points(ex.P, ex.S, pch = 6, cex = 1.1, lwd = 2)
points(new.data, pch = 3, cex = 1.1, lwd = 2)
abline(a = -d.inter/d.slopes[2], b = -d.slopes[1]/d.slopes[2])
text(eq.P - 0.07, eq.S + 0.005, label = names(eqexp[1:8]), cex = 0.8)
text(ex.P + 0.07, ex.S + 0.003, label = names(eqexp[9:16]), cex = 0.8)
text(NZ.P + 0.05, NZ.S + 0.003, label = names(eqexp[17]), cex = 0.8)
legend("topright", c("EQ", "EX", "NZ"), pch = c(8,6,3), pt.lwd = 2, cex = 1.1)



# -->
# The tendncy of the earthquakes to have higher values for log10 S, relative to log10 P
# The use of the logarithm of the ratio log10 P - log10 S in some references is a tacit indicator that
# a linear function of the two parameteres will be a useful discriminant

# The unknown event NZ is classified as an explosion, with posterior probability 0.960
