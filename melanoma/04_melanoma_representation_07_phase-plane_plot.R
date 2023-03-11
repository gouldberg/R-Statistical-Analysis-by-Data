# setwd("//media//kswada//MyFiles//R//melanoma")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\melanoma")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  melanoma
# ------------------------------------------------------------------------------

melanoma <- t(matrix(scan("melanoma.txt", 0), 3, 37)) %>% as.data.frame() %>% dplyr::select(V2, V3)

# data("melanoma", package = "fda")


colnames(melanoma) <- c("year", "incidence")



str(melanoma)




# ------------------------------------------------------------------------------
# segment by years
# ------------------------------------------------------------------------------

graphics.off()

par(mfrow=c(2,2), mar = c(2,2,2,2))

yr <- c(1936, 1944, 1953, 1961, 1970)


plot(year, mela, type="p", cex=1, xlab="", ylab = "", main="Smoothing by Linear Differential Operator")
lines(melafd_l)
abline(v = yr, lty = 2, col = "gray")

plot(melafd_l, Lfd=1, ylab="", main="Velocity", cex=1.2)
abline(v = yr, lty = 2, col = "gray")

plot(melafd_l, Lfd=2, ylab="", main="Acceleration", cex=1.2)
abline(v = yr, lty = 2, col = "gray")




# ------------------------------------------------------------------------------
# Compare Phase-Plane plot
# ------------------------------------------------------------------------------

par(mfrow=c(2,2))

phaseplanePlot(1936:1945, melafd_l, labels = list(evalarg = seq(1936, 1945, by = 1)), main = "1936 - 1945")

phaseplanePlot(1944:1954, melafd_l, labels = list(evalarg = seq(1944, 1954, by = 1)), main = "1944 - 1954")

phaseplanePlot(1952:1962, melafd_l, labels = list(evalarg = seq(1952, 1962, by = 1)), main = "1952 - 1962")

phaseplanePlot(1960:1971, melafd_l, labels = list(evalarg = seq(1960, 1971, by = 1)), main = "1960 - 1971")


phaseplanePlot(1944:1962, melafd_l, labels = list(evalarg = seq(1944, 1962, by = 1)), main = "1944 - 1962")


# -->
# Note that year 1944 - 1954, the subcycle is not recognized


