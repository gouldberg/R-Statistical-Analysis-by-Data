setwd("//media//kswada//MyFiles//R//db")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  db
# ------------------------------------------------------------------------------
data("db", package = "gamlss.data")


str(db)

car::some(db)



# ------------------------------------------------------------------------------
# Case1:  Centile for y given x and centile percentage
# ------------------------------------------------------------------------------

nage <- seq(0, 20, 0.1)


# model m0_c by LMS method
mat1 <- centiles.pred(m0_c, xname = "age", xvalues = nage, plot=TRUE, legend = FALSE, xlab = "age", ylab = "head")


head(mat1)




# ------------------------------------------------------------------------------
# Case2:  Centile for y given x and centile z-score
# ------------------------------------------------------------------------------

dev <- c(-4, -3, -2, -1, 0, 1, 2, 3, 4)

round(100 * dNO(dev), 3)


mat2 <- centiles.pred(m0_c, xname = "age", xvalues = nage, type = "standard-centiles", dev = dev, plot = T)

head(mat2)


