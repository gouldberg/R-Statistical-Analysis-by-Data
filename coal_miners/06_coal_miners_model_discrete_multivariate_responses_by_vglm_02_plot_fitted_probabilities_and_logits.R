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
# fitted probabilities and logits
# ------------------------------------------------------------------------------

age <- coalminers$age




# ----------
# fitted probabilities for the 4 response combinations and log odds

P <- fitted(cm.vglm1)

colnames(P) <- c("bw", "bW", "Bw", "BW")

P
lP <- qlogis(P)




# ----------
# corresponding observed probabilities and log odds

Y <- depvar(cm.vglm1)

lY <- qlogis(Y)




# ----------

col <- c("red", "blue", "red", "blue")

pch <- c(1, 2, 16, 17)


par(mfrow = c(1,2))

matplot(age, P, type = "l",
        col = col, lwd = 2, cex = 1.2, cex.lab = 1.2,
        xlab = "Age", ylab = "Probability", xlim = c(20, 65))

matpoints(age, Y, pch = pch, cex = 1.2, col = col)
text(64, P[9,] + c(0, .01, -.01, 0), labels = colnames(P), col = col, cex = 1.2)
text(20, P[1,] + c(0, .01, -.01, 0), labels = colnames(P), col = col, cex = 1.2)


matplot(age, lP, type = "l",
        col = col, lwd = 2, cex = 1.2, cex.lab = 1.2,
        xlab = "Age", ylab = "Logit", xlim = c(20, 65))

matpoints(age, lY, pch = pch, cex = 1.2, col = col)
text(64, lP[9,] + c(0, .01, -.01, 0), labels = colnames(lP), col = col, cex = 1.2)
text(20, lP[1,] + c(0, .01, -.01, 0), labels = colnames(lP), col = col, cex = 1.2)


