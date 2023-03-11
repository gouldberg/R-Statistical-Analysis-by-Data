# setwd("//media//kswada//MyFiles//R//ketchup")
setwd("//media//kswada//MyFiles//R//Binary_ratio_multinominal_response_and_related_model//ketchup")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Ketchup
# ------------------------------------------------------------------------------

data("Ketchup", package = "Ecdat")


dim(Ketchup)


str(Ketchup)


head(Ketchup)



# ----------
data <- Ketchup




# ------------------------------------------------------------------------------
# 3-D plot:  price of Heinz and Hunts and choice probability for Heinz
# ------------------------------------------------------------------------------

library(rgl)
# library(akima)

coef1 <- result$coefficients[1]

coef2 <- result$coefficients[2]



# ----------
x1 <- seq(0.1, 1.8, length=18)

x2 <- x1

d <- expand.grid(x1=x1, x2=x2)

d$p1 <- exp(coef2*d$x1)/(exp(coef2*d$x1)+exp(coef1+coef2*d$x2))

d$p2 <- exp(coef1+coef2*d$x2)/(exp(coef2*d$x1)+exp(coef1+coef2*d$x2))



# ----------
plot3d(d$x1, d$x2, d$p2)

# surface3d(x=d$x1, y=d$x2, z=d$p2, alpha=0.4, front="lines", back="lines")



# ----------
plot3d(d$x1, d$x2, d$p1)

# surface3d(d$x1, d$x2, d$p1, alpha=0.4, front="lines", back="lines")


