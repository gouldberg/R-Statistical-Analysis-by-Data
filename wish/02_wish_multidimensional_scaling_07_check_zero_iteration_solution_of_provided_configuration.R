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
# Check starting configurations and its stress values
# ------------------------------------------------------------------------------

# Classical MDS aka Torgerson Scaling
tstart <- torgerson(wish)



# ----------
# Compute the stress for 0 iterations based on a starting configuration provided
st0 <- stress0(wish, init = tstart)



# ----------
par(mfrow=c(1,1))
plot(tstart, main = "torgerson start", ylim = c(-2.5, 2.5), xlim = c(-2.5, 2.5))
text(tstart[,1], tstart[,2]+0.2, labels = attributes(wish)$Labels, cex = 0.7)

st0


# -->
# Starting configuration based on Torgerson scaling has Stress 0.345