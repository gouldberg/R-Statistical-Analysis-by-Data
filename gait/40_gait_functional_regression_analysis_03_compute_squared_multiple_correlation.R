setwd("//media//kswada//MyFiles//R//gait")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gait
# ------------------------------------------------------------------------------

data("gait", package = "fda")

str(gait)

dim(gait)




# ------------------------------------------------------------------------------
# Compute SSE0 and SSE1
# ------------------------------------------------------------------------------

gaitfine = seq(0, 20, len = 101)

ncurve        = dim(gait)[2]


# ----------
kneefinemat   = eval.fd(gaitfine, kneefd)

kneemeanvec   = eval.fd(gaitfine, mean(kneefd))

kneehatfinemat= eval.fd(gaitfine, kneehatfd)


resmat        = kneefinemat - kneehatfinemat

resmat0 = kneefinemat - kneemeanvec %*% matrix(1, 1, ncurve)



# ----------
SSE0 = apply((resmat0)^2, 1, sum)

SSE1 = apply(resmat^2, 1, sum)



# ------------------------------------------------------------------------------
# OR simple this way
# ------------------------------------------------------------------------------

kneemat     <- eval.fd(gaitfine, kneefd)

#kneehatmat  <- eval.fd(gaitfine, kneehatfd)
kneehatmat  <- predict(kneehatfd, gaitfine)

kneemeanvec <- as.vector(eval.fd(gaitfine, mean(kneefd)))



# -----------
SSE0 <- apply((kneemat - outer(kneemeanvec, rep(1,ncurve)))^2, 1, sum)

SSE1 <- apply((kneemat - kneehatmat)^2, 1, sum)



# ------------------------------------------------------------------------------
# Compute squared multiple correlation
# ------------------------------------------------------------------------------

Rsqr <- (SSE0 - SSE1) / SSE0




# ------------------------------------------------------------------------------
# plot squared multiple correlation
# ------------------------------------------------------------------------------

op <- par(mfrow=c(1,1), ask=FALSE)

plot(gaitfine, Rsqr, type="l", ylim=c(0,0.4))



