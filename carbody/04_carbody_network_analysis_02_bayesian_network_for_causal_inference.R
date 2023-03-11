
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\carbody")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  car body coating process data
# ------------------------------------------------------------------------------

dat <- read.csv("carbody_dat.txt", header = TRUE, skip = 2, sep = "\t")


str(dat)


car::some(dat)




# ----------
dat_s <- data.frame(scale(dat))


head(dat_s)




# ------------------------------------------------------------------------------
# Bayesian Network
# ------------------------------------------------------------------------------


dat_s2 <- dat_s %>% dplyr::select(-centmaku, -makuwid)



library(bnlearn)


set.seed(123)


# we perform 10 random restarts with 100 perturbations in order to avoid nasty local minima.
# In Bayesian Network, we use input data, not correaltion matrix
# If the variables are numeric, a Gaussian network is applied. 

# hc:  simple algorithm called "hill-climbing", this algorithm starts with a representation
# without any edge and, subsequently, adds, deletes, and reverses one edge at a time until a target score
# (which judges the goodness of fit)


# we use the BIC as target score
fitBN <- hc(dat_s2, restart = 10, perturb = 100)



# ----------
# compute a measure for the strength of the connections

estrength <- arc.strength(fitBN, dat_s2, "bic-g")


head(estrength[order(estrength[,3]),], 5)


estrength



# ----------
# this requires "Rgraphviz" ...

library(Rgraphviz)

strength.plot(fitBN, estrength, main = "Bayesian Network", shape = "ellipse")




# ------------------------------------------------------------------------------
# Model averaging
#    - in order to stabilize network, perform model averaging based on bootstrap samples of th data
# ------------------------------------------------------------------------------


set.seed(123)


bootnet <- boot.strength(dat_s2, R = 500, algorithm = "hc")


bootnet



estrength %>% filter(to == "tochaku")

bootnet %>% filter(to == "tochaku")



# -->
# 97.6% of the fitted networks include the connection from "fukituke" to "tochaku"
# The probability that the corresponding edge goes in that direction is 0.68



# ------------
bootnet %>% filter(to == "tochaku", strength > 0.5, direction > 0.5)


bootnet %>% filter(to == "hakidashi" | from == "hakidashi", strength > 0.5, direction > 0.5)




# ----------
# we set a threshold for th estrength of 0.85 which means that only edges should only be retained

avgnet <- averaged.network(bootnet, threshold = 0.85)


avgnet




# ----------
# compute the edge strenghts using the BIC once more

estrength_av <- arc.strength(avgnet, dat_s2, "bic-g")

estrength_av %>% filter(to == "tochaku")


