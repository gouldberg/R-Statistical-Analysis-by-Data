setwd("//media//kswada//MyFiles//R//rent")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  rent
# ------------------------------------------------------------------------------
data("rent", package = "gamlss.data")


str(rent)

car::some(rent)



# ------------------------------------------------------------------------------
# prediction
# ------------------------------------------------------------------------------

newrent <- data.frame(expand.grid(Fl = seq(30, 120, 5), A = seq(1890, 1990, 5), H = c(1), loc = c(2)))


# ----------
pred1 <- predict(rps, newdata = newrent, type = "response")


pred2 <- predict(rcs, newdata = newrent, type = "response")



# ------------------------------------------------------------------------------
# Contour plot
# ------------------------------------------------------------------------------
Fln <- seq(30, 120, 5)
An <- seq(1890, 1990, 5)


op <- par(mfrow=c(1,2))W
contour(Fln, An, matrix(pred1, nrow = length(Fln)), nlevels = 30,
        ylab = "year of construction", xlab = "floor space", main = "pb()")

contour(Fln, An, matrix(pred2, nrow = length(Fln)), nlevels = 30,
        ylab = "year of construction", xlab = "floor space", main = "cs()")

par(op)



# ------------------------------------------------------------------------------
# Surfaces plot
# ------------------------------------------------------------------------------

library(lattice)

p1 <- wireframe(pred1 ~ Fl * A, newrent, aspect = c(1, 0.5), drape = TRUE, colorkey = list(space = "right", height = 0.6))

p2 <- wireframe(pred2 ~ Fl * A, newrent, aspect = c(1, 0.5), drape = TRUE, colorkey = list(space = "right", height = 0.6))



# ----------
print(p1, split = c(1, 1, 2, 1), more = TRUE)

print(p2, split = c(2, 1, 2, 1))
