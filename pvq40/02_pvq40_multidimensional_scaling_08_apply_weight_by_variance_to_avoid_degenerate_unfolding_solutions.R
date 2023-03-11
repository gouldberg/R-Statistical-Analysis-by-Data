setwd("//media//kswada//MyFiles//R//pvq40")

packages <- c("dplyr", "smacof")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  PVQ40
# ------------------------------------------------------------------------------

data(PVQ40, package = "smacof")
# data(PVQ40agg, package = "smacof")


str(PVQ40)

attributes(PVQ40)


car::some(PVQ40)
# car::some(PVQ40agg)



# ------------------------------------------------------------------------------
# MDS on scores averaged by category
# ------------------------------------------------------------------------------

var_n <- c("SE", "CO", "TR", "BE", "UN", "SD", "ST", "HE", "AC", "PO")

var_n_select <- c("se1, se2", "co1, co2", "tr1, tr2", "be1, be2", "un1, un2, un3", "sd1, sd2", "st1, st2", "he1, he2", "ac1, ac2", "po1, po2")


# Take row means for each category of measure
for(i in 1:length(var_n)){
  eval(parse(text = paste0(var_n[i], " <- rowMeans(subset(PVQ40, select = c(", var_n_select[i], ")), na.rm = TRUE)")))
}

raw <- cbind(SE, CO, TR, BE, UN, SE, ST, HE, AC, PO)



# ----------
R <- cor(raw)

diss <- sim2diss(R)

res <- mds(diss, init = "torgerson", type = "ordinal")



# ----------
graphics.off()
par(mfrow=c(1,1))
plot(res)
polygon(res$conf, lty = 2)

erg    <- fitCircle(res$conf[,1], res$conf[,2])
draw.circle(erg$cx, erg$cy, radius = erg$radius, border = "blue", lty = 2)



# ------------------------------------------------------------------------------
# Unfolding:  use original preferences (degenerate solutions)
# ------------------------------------------------------------------------------

# Preferences into dissimilarities
( pref <- max(raw) - raw )


result <- unfolding(pref)


plot(result)


result$stress


# -->
# Some persons generate highly different ratings, others rate all values the same.
# The latter data are easy to represent in unfolding in a quasi-degenerate solution with the person points densely clustered in the center
# of a circle of points representing the personal values.

# So, if you have a large proportion of persons with almost constant ratings, unfolding become almost trivial.



# ------------------------------------------------------------------------------
# Unfolding:  use original preferences with transformations
# ------------------------------------------------------------------------------

# Compute ratio unfolding for a start configuration
result2 <- unfolding(delta= pref, type="ratio",
                  ndim=2, itmax=10000, omega=0, verbose=TRUE)


# Initial configuration of the individuals
resunf <- unfolding(delta = pref, type="mspline", 
                    conditionality="row", ndim=2, itmax=10000, 
                    omega=0.1, spline.intKnots=1, verbose=TRUE,
                    init=list(result2$conf.row, result2$conf.col))



# ----------
par(mfrow=c(1,2))
plot(resunf, plot.type="Shepard")
plot(resunf, plot.type="confplot")



# ------------------------------------------------------------------------------
# Unfolding solutions with weighting by variance
#  - weights are variances of pref per row  
# ------------------------------------------------------------------------------

( var.per.row <- apply(pref, 1, var) )


nobs <- dim(raw)[1]
W <- matrix(var.per.row, nrow = nobs, ncol = ncol(pref))


# If var = 0, use smallest non-zero variance
W[W == 0] <- min(W[W != min(W)])


W


# ----------
out <- unfolding(pref, weightmat = W)



# ----------
par(mfrow = c(1,3))
plot(result, main = "quasi-degerante solution")
plot(resunf, main = "use transformations")
plot(out, main = "solution with weight by variance")



# ----------
result$stress
out$stress


# -->
# not so different ....