setwd("//media//kswada//MyFiles//R//crimes")

packages <- c("dplyr", "smacof", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  crimes
# ------------------------------------------------------------------------------
data("crimes", package = "smacof")

str(crimes)

car::some(crimes)



# ------------------------------------------------------------------------------
# One-dimensional Multidimensional Scaling
# ------------------------------------------------------------------------------

# run MDS in 1 dim
# Note! uniscale() runs for VERY long if the number of variables is not VERY small!
# if so, use: result <- mds(diss, ndim=1, type="ratio") with the (small) risk of not getting the best-possible solution
result2 <- uniscale(diss)


result2

result2$conf



# ----------
# plot unidimensional configuration horizontally
plot(result2)


# plot vertically
graphics.off()
plot(rep(0, length(result2$conf)), -result2$conf,  axes = FALSE, ann = FALSE, pch = 20, type = "o", xlim = c(-0.2, 0.8))
text(rep(0.10, length(result2$conf)), -result2$conf, names(result2$conf))
text(rep(-0.05, length(result2$conf)), -result2$conf, pos=2, sprintf("%.2f",round(-1*result2$conf, 2) ))



# -->
# 2-dimensional MDS solution exhibits clearly more scatter.
# One should, therefore, not interpret this configuration too closely, because it is partly misleading.

# Robbberty and Burglary are correlated with 0.62 and Robberty and Auto Theft also 0.62.
# Yet, in the 1d MDS solution, Burglary is about half as far from RObberty as Auto Theft is from Robberty.

# But 1d solution makes sense in that it orders the various crimes in terms of increasing violence and brutality.




