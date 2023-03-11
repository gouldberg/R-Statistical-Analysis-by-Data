
packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# SSA matrix decomposition and grouping diagnostics
# SSA decomposision
# ------------------------------------------------------------------------------

ts <- read.csv("Mayport.csv")


sea <- ts$observed



# ----------
# SSA Decomposition

library(Rssa)


# input SSA window length L = (12-month cycle) (30 repetitions)
L <- 432

s <- ssa(x, L, kind = "toeplitz-ss")





# ------------------------------------------------------------------------------
# SSA matrix decomposition and grouping diagnostics
# Run grouping diagnostics to group eigentriplets
# ------------------------------------------------------------------------------


# 1st visual diagnostics:  Eigenspectrum

# plot 1st 20 largest
eigenvalues <- plot(s, numvalues = 20, col = "black", lwd = 2)



# ----------
# 2nd visual diagnostic:  Eigenvector plots

# plot 1st 10 for 300 periods
plot(s, type = "vectors", idx = 1:10, xlim = c(1,300), cl = "black", lwd = 2)




# ----------
# 3rd visual diagnostic:  Pairwise scatterplots of eigenvectors

# plot 1st 9 pairs
plot(s, type = "paired", idx = 1:9, plot.contrib = FALSE, col = "black", lwd = 2)




# ----------
# weighted correlation matrix

# 1st 20 eigentriplets
plot(w <- wcor(s, groups = c(1:20)))


# table for 1st 10 eigentriplets
w.corr.res <- wcor(s, groups = c(1:10))






# ------------------------------------------------------------------------------
# Scatter plot cosine and sine functions oscillating at idential frequency
# ------------------------------------------------------------------------------


# parameters defining fundamental frequencies

# number of cycles each T
a <- 1

# cycle length
T <- c(12, 6, 4)

# time
t <- 1:20



# ----------
# fundamental frequency for 12-period cycle
w1 <- 2 * pi / T[1]

sin1 <- sin(a * w1 * t)

cos1 <- cos(a * w1 * t)



# 6-period cycle
w2 <- 2 * pi / T[2]

sin2 <- sin(a * w2 * t)

cos2 <- cos(a * w2 * t)



# 4-period cycle
w3 <- 2 * pi / T[3]

sin3 <- sin(a * w3 * t)

cos3 <- cos(a * w3 * t)



# ----------
par(mfrow = c(2,2))

plot(cos1, sin1, main = "Scatterplot", type = "l", lwd = 2, cex.axis = 1.1, font.axis = 2)

plot(cos2, sin2, main = "Scatterplot", type = "l", lwd = 2, cex.axis = 1.1, font.axis = 2)

plot(cos3, sin3, main = "Scatterplot", type = "l", lwd = 2, cex.axis = 1.1, font.axis = 2)



