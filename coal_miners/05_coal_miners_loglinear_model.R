setwd("//media//kswada//MyFiles//R//coal_miners")

packages <- c("dplyr", "vcd", "MASS", "datasets")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Coal Miners
# ------------------------------------------------------------------------------

data("CoalMiners", package = "vcd")

data <- CoalMiners



# ----------
# Breathlessness * Wheeze * Age
dim(data)

dimnames(data)


data


# ----------
# Age 20-24 to be omitted from analysis here
dat2 <- data[,,2:9]


structable(. ~ Age, data = dat2)




# ------------------------------------------------------------------------------
# Fitting loglinear models:  mutual independence model and associated response model
# ------------------------------------------------------------------------------

CM <- as.data.frame(data)


colnames(CM)[1:2] <- c("B", "W")

head(CM)





# ----------
# mutual independence model:  [B][W][Age]
cm.glm0 <- glm(Freq ~ B + W + Age, data = CM, family = poisson)



# associated response model:  [BW][Age], which asserts that the association between B and W is independent of Age
cm.glm1 <- glm(Freq ~ B * W + Age, data = CM, family = poisson)



summary(cm.glm0)


summary(cm.glm1)




# ----------

LRstats(cm.glm0, cm.glm1)



# -->
# baseline model (assocaiated response model) fits very badly


