setwd("//media//kswada//MyFiles//R//pbc")

packages <- c("dplyr", "gamair", "mgcv")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  pbc
# ------------------------------------------------------------------------------

data(pbc, package = "survival")
data(pbcseq, package = "survival")


str(pbc)
str(pbcseq)


car::some(pbc)
car::some(pbcseq)



# ------------------------------------------------------------------------------
# Survival function and one standard error bands for subject, and time varying covariates through time
# ------------------------------------------------------------------------------

# event times
te <- sort(unique(pb$futime))


# data for subject k
k <- 25
di <- pbcseq[pbcseq$id == k,]


pd <- data.frame(lapply(X = di, FUN = app, t = di$day, to = te))

pd$tf <- factor(te)

X <- predict(mod_bam, newdata = pd, type = "lpmatrix")

eta <- drop(X %*% coef(mod_bam))

H <- cumsum(exp(eta))

J <- apply(exp(eta) * X, 2, cumsum)

se <- diag(J %*% vcov(mod_bam) %*% t(J)) ^ 0.5


# ----------
# Survival function and one standard error bands of the subject
graphics.off()
par(mfrow = c(2,3))

plot(stepfun(te, c(1, exp(-H))), do.points = FALSE, ylim = c(0.7, 1), ylab = "S(t)", xlab = "t(days)", main = "", lwd = 2)
lines(stepfun(te, c(1, exp(-H + se))), do.points = FALSE)
lines(stepfun(te, c(1, exp(-H - se))), do.points = FALSE)
rug(pbcseq$day[pbcseq$id == k])



# ----------
# Time varying covariate through time
plot(di$day, di$protime)
lines(te, pd$protime)

plot(di$day, di$platelet)
lines(te, pd$platelet)

plot(di$day, di$bili)
lines(te, pd$bili)

plot(di$day, di$albumin)
lines(te, pd$albumin)

plot(di$day, di$ast)
lines(te, pd$ast)


# -->
# Notice how the reduction in the survival function relative to the constant baseline covariate model is probably driven by deterioration in serum albumin
# and platelet counts in particular, and some increase in clotting time towards the end of the data.
