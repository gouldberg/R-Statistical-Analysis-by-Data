setwd("//media//kswada//MyFiles//R//els_career_academy")

packages <- c("dplyr", "mice", "mitools")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# Estimate propensity scores using generalized boosted modeling (GBM)
#   - McCaffrey et al. (2004, 2013) proposed and demonstrated combining GBM with logistic regression trees for propensity score estimation,
#     and their method is fully implemented in the R package twang.
# ------------------------------------------------------------------------------

library(twang)


set.seed(2015)


# change the treatment indicator from factor to numeric, which is required by GBM
imputation1$treat <- as.numeric(imputation1$treat == 1) 



# McCaffrey et al. recommended using a measure of covariate balance, such as the standardized mean difference between treated and untreated groups,
# to stop the GBM algorithm the first time that a minimum covariate balance is achieved, but there is no guarantee that
# better covariate balance would not be achieved if the algorithm runs additional iterations.

# stop.method = c("es.max") defines the stopping criterion, which is set to the maximum effect size.
# Thus, after each iteration, the algorithm computes the effect size for each variables as the standardized mean difference between
# treatment and untreated groups and stores the maximum effect size across variables.
# Once the maximum number of iterations is reached, the algorithm provides the estimates obtained at the point where the maximum effect size is smallest.

##### IT TAKES TIME !!! --- 10 min.
myGBM <- ps(psFormula, data = imputation1, n.trees = 10000, interaction.depth = 4,
            shrinkage = 0.01, stop.method = c("es.max"), estimand = "ATT", 
            verbose = TRUE, sampw = imputation1$bystuwt)



# obtain information about balance achieved and iteration numbers
# Estimates are obtained at iteration 1393 and the smallest value of the maximum effect size is 0.093

summary(myGBM)



# ----------
# obtain tiff image

tiff("gbm_for_treat.tif", res=600, compression = "lzw", height=6, width=15, units="in")

plot(myGBM,type="b", color=F, lwd=2)

dev.off()



# obtain a pdf
# pdf("gbm_for_treat.pdf",, height=6, width=15)
# plot(myGBM,type="b", color=F)
# dev.off()



# ----------
# extract estimated propensity scores from object

pScoresGBM <- myGBM$ps

names(pScoresGBM) = "pScoresGBM"

imputation1$pScoresGBM <- unlist(pScoresGBM)




# ----------
# save data file
ELS.data.imputed <- imputation1


# save results
save(list=c("ELS.data.imputed", "allImputations", "myctree", "mycforest", "myGBM", "pScores", "pScoresRf", "pScoresGBM"), file="imputed_and_estimated_propensity_score.Rdata", compress=T)


