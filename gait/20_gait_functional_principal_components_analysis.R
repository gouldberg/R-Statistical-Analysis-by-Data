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
# Functional Principal Component Analysis
# ------------------------------------------------------------------------------

# Smooth with lambda as determined before
gaitfdPar  <- fdPar(gaitbasis, harmaccelLfd20, lambda=1e-2)



# ----------
nharm = 4

gaitpca.fd <- pca.fd(gaitfd, nharm=nharm, gaitfdPar)



# ----------
# varimax rotation
gaitpca2.fd <- varmx.pca.fd(gaitpca.fd)



