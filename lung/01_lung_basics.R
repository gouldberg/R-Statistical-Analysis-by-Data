setwd("//media//kswada//MyFiles//R//lung")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  lung
#  - The data are provided by the Global Lung Function Initiative, and are accessed at
#    www.ers-education.org/guidelines/global-lung-function-initiative/statistics.aspx
#  - The response variable is the forced expired volumne (fev) and the explanatory variables are height and age
# ------------------------------------------------------------------------------

lung <- read.csv("data.csv", header = T)

str(lung)

car::some(lung)


# ----------
# only males data

dm <- subset(lung, sex == 1)

dim(dm)



# ----------
# only males data

dfe <- subset(lung, sex == 2)

dim(dfe)



# ----------
# apply a log transformation to height and age

dm <- transform(dm, la = log(age), lh = log(height))

dfe <- transform(dfe, la = log(age), lh = log(height))



# ------------------------------------------------------------------------------
# basics:  male data
# ------------------------------------------------------------------------------

lattice::xyplot(fev ~ height, data = dm, type = c("g", "p", "smooth"), col.line = "black", pch = 20, col = gray(0.8))

lattice::xyplot(fev ~ lh, data = dm, type = c("g", "p", "smooth"), col.line = "black", pch = 20, col = gray(0.8))


lattice::xyplot(fev ~ age, data = dm, type = c("g", "p", "smooth"), col.line = "black", pch = 20, col = gray(0.8))

lattice::xyplot(fev ~ la, data = dm, type = c("g", "p", "smooth"), col.line = "black", pch = 20, col = gray(0.8))



# ----------
# scatter plot in 3D
lattice::cloud(fev ~ height * age, data = dm)


rgl::plot3d(dm$height, dm$age, dm$fev)


car::scatter3d(dm$height, dm$age, dm$fev, xlab = "height", ylab = "age", zlab = "fev", ticktype = "detailed")




# ------------------------------------------------------------------------------
# basics:  female data
# ------------------------------------------------------------------------------

lattice::xyplot(fev ~ height, data = dfe, type = c("g", "p", "smooth"), col.line = "black", pch = 20, col = gray(0.8))

lattice::xyplot(fev ~ lh, data = dfe, type = c("g", "p", "smooth"), col.line = "black", pch = 20, col = gray(0.8))


lattice::xyplot(fev ~ age, data = dfe, type = c("g", "p", "smooth"), col.line = "black", pch = 20, col = gray(0.8))

lattice::xyplot(fev ~ la, data = dfe, type = c("g", "p", "smooth"), col.line = "black", pch = 20, col = gray(0.8))



# ----------
# scatter plot in 3D
lattice::cloud(fev ~ height * age, data = dfe)


rgl::plot3d(dfe$height, dfe$age, dfe$fev)


car::scatter3d(dfe$height, dfe$age, dfe$fev, xlab = "height", ylab = "age", zlab = "fev", ticktype = "detailed")

