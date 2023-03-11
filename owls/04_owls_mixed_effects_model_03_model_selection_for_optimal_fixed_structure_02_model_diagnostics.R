# setwd("//media//kswada//MyFiles//R//owls")
setwd("//media//kswada//MyFiles//R//Mixed_effects_and_multilevel_model//owls")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Owls
# ------------------------------------------------------------------------------

Owls <- read.table(file = "Owls.txt", header = TRUE)


str(Owls)

dim(Owls)

car::some(Owls)




# ------------------------------------------------------------------------------
# refit with REML and validate the model
# ------------------------------------------------------------------------------

# homogeneity seems a fair assumption

E2 <- resid(M5, type="normalized")

F2 <- fitted(M5)

op <- par(mfrow=c(2,2),mar=c(4,4,3,2))

MyYlab = "Residuals"

plot(x = F2, y = E2, xlab = "Fitted values", ylab = MyYlab)

boxplot(E2 ~ SexParent, data = Owls, main = "Sex of parent", ylab = MyYlab)

boxplot(E2 ~ FoodTreatment, data = Owls, main = "Food treatment", ylab = MyYlab)

plot(x = Owls$ArrivalTime, y = E, main = "Arrival time", ylab = MyYlab, xlab = "Time (hours)")

par(op)




# ----------
# Smoother should not any pattern, but unfortunately it raises some suspicion about independence assumtion.
library(lattice)

xyplot(E2 ~ ArrivalTime | SexParent * FoodTreatment, data=Owls,
       ylab="Residuals",xlab="Arrival time (hours)",
       panel = function(x,y){
         panel.grid(h=-1, v= 2)
         panel.points(x, y, col=1)
         panel.loess(x, y, span=0.5, col=1, lwd=2)})


# -->
# The answer is to fit an additive mixed model
