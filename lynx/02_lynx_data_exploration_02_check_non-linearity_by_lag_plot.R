setwd("//media//kswada//MyFiles//R//lynx")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  lynx
# ------------------------------------------------------------------------------

data(lynx, package = "datasets")


str(lynx)


lynx



# ------------------------------------------------------------------------------
# data exploration:  Check the non-linearity by lag1.plot
# ------------------------------------------------------------------------------


lynxl <- log10(lynx)

dlynxl <- diff(log10(lynx))


graphics.off()


# scatterplot with lowess fit
astsa::lag1.plot(lynxl, max.lag = 1, corr = FALSE)

# astsa::lag1.plot(dlynxl, max.lag = 1, corr = FALSE)


astsa::lag1.plot(lynxl, max.lag = 2, corr = TRUE)




# -->
# suggesting threshold = 2.5




# ----------
par(mfrow=c(1,1))

plot(lynxl, type = "l")

abline(h = 2.5, col = "gray", lty = 2)





# ------------------------------------------------------------------------------
# data exploration:  by autopairs
# ------------------------------------------------------------------------------

library(tsDyn)

par(mfrow = c(1,1), mar = c(2,2,0,0))

autopairs(lynxl, lag = 1, type = "regression")
par(new=T)
autopairs(lynxl, lag = 1, type = "levels")



# ----------
autopairs(lynxl, lag = 1, type = "image")


autopairs(lynxl, lag = 1, type = "persp")



# ----------
autopairs(lynxl, lag = 3, type = "regression")
par(new=T)
autopairs(lynxl, lag = 3, type = "levels")



