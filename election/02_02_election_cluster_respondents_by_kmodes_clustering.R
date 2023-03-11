# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("//media//kswada//MyFiles//R//election")

packages <- c("dplyr", "Hmisc")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  election
# ------------------------------------------------------------------------------

library(poLCA)

data(election)

str(election)



# ----------
dat <- election




# ---------------------------------------------------------------------------
# k-modes clustering
#   - k-prototypes clustering can not be applied to data wiht all categorical variables
# ---------------------------------------------------------------------------
library(klaR)


nrow(dat[complete.cases(dat[,c(1:12)]),])

dat_comp <- dat[complete.cases(dat[,c(1:12)]),]


k <- 5
kmod <- kmodes(data = dat_comp[,c(1:12)], modes = k)



# ----------
kmod



# ---------------------------------------------------------------------------
# check the clusters variation
# ---------------------------------------------------------------------------
# segment group statistics
seg.summ <- function(data, groups){
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))
}


table(kmod$cluster)

dat_comp$cluster <- kmod$cluster


library(lattice)


var_n <- c("MORALG", "CARESG", "KNOWG", "LEADG", "DISHONG", "INTELG", "MORALB", "CARESB", "KNOWB", "LEADB", "DISHONB", "INTELB")

graphics.off()

i <- 3
eval(parse(text = paste0("p <- histogram( ~ ", var_n[i], " | cluster, data = dat_comp, layout = c(5,1))")))
plot(p)



# ---------------------------------------------------------------------------
# Visualize the clusters
#   - clusplot() will perform dimensional reduction with principal components or multidimensional scaling as the data warrant,
#     and then plot the observations with cluster membership identified.
# ---------------------------------------------------------------------------

library(cluster)


# lines = 0: omit distance lines between groups
# labels = 3: plot data label
clusplot(dat_comp[,c(1:12)], kmod$cluster, color = TRUE, shade = TRUE, labels = 4, lines = 0, main = "kmodes plot (k = 5)")


# -->
# NOT GOOD ....




