setwd("//media//kswada//MyFiles//R//wish")

packages <- c("dplyr", "smacof")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  wish
# ------------------------------------------------------------------------------

data(wish, package = "smacof")


wish


car::some(wish)



# ------------------------------------------------------------------------------
# Exhibits the coordinates of the MDS solution on 2 external scales
#   - The fit of the external scales in this MDS configuration is given by the correlation of these scales with the projections of the points onto
#     straight lines through the arrows that represent them.
#   - The length of the two arrows represents, approximately, the relative fit of the external scales.
# ------------------------------------------------------------------------------

diss <- sim2diss(wish, method = 7)

ex <- mds(diss, type = "ordinal")

ex$stress


plot(ex, label.conf=list(pos=5))




# ----------
# economic development of country for 12 countries
ecdev <- c(3,1,3,3,8,3,7,9,4,7,10,6)

# number of inhabitants for 12 countries
inhabs <- c(87,17,8,30,51,500,3,100,750,235,201,20)


labs <- attr(wish, "Labels")


fitbi <- biplotmds(res, cbind(ecdev, inhabs))

plot(fitbi, main = "", xlab = "", ylab = "", cex = 1.3, label.conf = list(cex = 1.2, pos = ifelse(labs != "RUSSIA", 3, 1)), 
     vecscale = 0.5, vec.conf = list(cex = 1.2, col = "red", cex = 1.2, length = 0.1))





fit1 <- lm(ecdev ~ ex$conf)

fit2 <- lm(inhabs ~ ex$conf)

cor(fit1$fitted.values, ecdev)

cor(fit2$fitted.values, inhabs)


abline(a=0, b=fit1$coefficients[3]/fit1$coefficients[2], lty=2, col="red" )

abline(a=0, b=fit2$coefficients[3]/fit2$coefficients[2], lty=2, col="blue" )




# ------------------------------------------------------------------------------
# Checking for different local minima
# ------------------------------------------------------------------------------

set.seed(1)

solutions <- icExplore(diss, type = "ordinal", nrep = 75, returnfit=TRUE)

solutions

plot(solutions)



# ------------------------------------------------------------------------------
# Compare two different solutions
# ------------------------------------------------------------------------------

start1 <- solutions$mdsfit[[3]]$conf

start2 <- solutions$mdsfit[[2]]$conf



# MDS solution with starting start1 config.
ex1 <- mds(diss, type = "ordinal", init=start1)

# MDS solution with starting start2 config.
ex2 <- mds(diss, type = "ordinal", init=start2)


ex1$stress

ex2$stress


# -->
# almost same stress



# ----------
fit1 <- lm(ecdev ~ ex1$conf); fit2 <- lm(inhabs ~ ex1$conf)

cor(fit1$fitted.values, ecdev); cor(fit2$fitted.values, inhabs)



# ----------
out <- Procrustes(ex1$conf, ex2$conf)

fit12 <- lm(ecdev ~ out$Yhat); fit22 <- lm(inhabs ~ out$Yhat)

cor(fit12$fitted.values, ecdev); cor(fit22$fitted.values, inhabs)



# ----------
# plot 2 same-Stress MDS solutions for country similarity data, with fitted external scales
par(mfrow=c(1,2))

plot(ex1, main="", label.conf=list(label=TRUE, pos=5, cex=1), xlab="", ylab="",
     xlim=c(-1,1), ylim=c(-1,1), axes=FALSE, frame.plot=TRUE, asp=1, 
     type = "p", pch = 16, cex = 1 )

abline(a=0, b=fit1$coefficients[3]/fit1$coefficients[2], lty=2, col="red" )
abline(a=0, b=fit2$coefficients[3]/fit2$coefficients[2], lty=2, col="blue" )

plot(out$Yhat, main="", pch=16, cex=1 , xlim=c(-1,1), ylim=c(-1,1), asp=1, 
     axes=FALSE, frame.plot=TRUE, xlab="", ylab="")

thigmophobe.labels(out$Yhat[,1], out$Yhat[,2], labels=attr(wish, "Labels"), cex=1)

abline(a=0, b=fit12$coefficients[3]/fit12$coefficients[2], lty=2, col="red" )
abline(a=0, b=fit22$coefficients[3]/fit22$coefficients[2], lty=2, col="blue" )


# -->
# The two solutions are rather similar (after Procrustean fitting) but differ in two important details:

# the positions of Japan and Islael are swapped in comparison with where they are in the right configuration
# India is positioned more in the center of the configuration.

# This means that the very large countries are closer together on the line "inhabitants".
# So this internal scale correlates with the external scale "number of inhabitants with r = 0.46.

