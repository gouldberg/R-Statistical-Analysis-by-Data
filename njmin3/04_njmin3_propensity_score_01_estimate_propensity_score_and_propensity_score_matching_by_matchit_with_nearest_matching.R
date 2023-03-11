setwd("//media//kswada//MyFiles//R//njmin3")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  njmin3
# ------------------------------------------------------------------------------

data("njmin3", package = "POE5Rdata")

dim(njmin3)

str(njmin3)


car::some(njmin3)



# ------------------------------------------------------------------------------
# Data preparation
# ------------------------------------------------------------------------------

# exclude the missing value record
njmin3_2 <- njmin3 %>% filter(!is.na(fte)) %>% dplyr::select(-demp)


# Dummy coded --> one variable
njmin3_2 <- njmin3_2 %>% mutate(rest_type = ifelse(bk == 1, "bk", ifelse(kfc == 1, "kfc", ifelse(roys == 1, "roys", ifelse(wendys == 1, "wendys", "none")))))

# Note that pa1, pa2 is related to "nj", so we can not use for covariate


# ----------
# also convert to factors
njmin3_2 <- rapply(njmin3_2, f = as.factor, class = "integer", how = "replace")

str(njmin3_2)



# ------------------------------------------------------------------------------
# Propensity Score Matching
# ------------------------------------------------------------------------------

library(MatchIt)


# Matching by propensity score
# including variables of selection-bias sources
# method = "near":  nearest neighbor matching
# default distance measure is "logit"
# replace = TRUE:  each control unit can be matched only once

# Note that pa1, pa2 are related to "nj", so we can not use them as covariates

# Dummy coded or NOT  AND  replace = TRUE or FALSE
m_mod1 <- matchit(formula = nj ~ bk + kfc + roys + wendys + co_owned, data = njmin3_2, method = "nearest", replace = FALSE)
m_mod2 <- matchit(formula = nj ~ bk + kfc + roys + wendys + co_owned, data = njmin3_2, method = "nearest", replace = TRUE)
m_mod3 <- matchit(formula = nj ~ rest_type + co_owned, data = njmin3_2, method = "nearest", replace = FALSE)
m_mod4 <- matchit(formula = nj ~ rest_type + co_owned, data = njmin3_2, method = "nearest", replace = TRUE)

# same variables included as Difference-in-Differences approach
m_mod5 <- matchit(formula = nj ~ kfc + roys + wendys + co_owned + southj + centralj + pa1, data = njmin3_2, method = "nearest", replace = FALSE)
m_mod6 <- matchit(formula = nj ~ kfc + roys + wendys + co_owned + southj + centralj + pa1, data = njmin3_2, method = "nearest", replace = TRUE)



# ----------
summary(m_mod)




# ----------
# basic summary table of matched data
m_mod1$nn
m_mod2$nn
m_mod3$nn
m_mod4$nn



# estimated distance measure for each unit
m_mod$distance



# weghts assigned to each unit in the matching process.
# Unmatched units have weights equal to zero.
# Matched treated units have weight 1.
# Each matched control unit has weight proportaional to the number of treatment units to which it was matched,
# and the sum of the control weights is equal to the number of uniquely matched control units
m_mod$weights



# the units ineligible for matching due to common support restrictions
m_mod$discarded[m_mod$discarded == TRUE]



# ----------
# If the empirical distributions are the same in the treated and control groups, the points in the Q-Q plots would all lie
# on the 45 degree line
# Deviations from the 45 degree line indicate differences in the empirical distribution
plot(m_mod)



# The jitter plot shows the overall distribution of propensity scores in treated and control groups
plot(m_mod, type = "jitter")



# The histogram of distributions of propensity score
plot(m_mod, type = "hist", col = gray(0.7))



# ------------------------------------------------------------------------------
# Assess covariate balance
# ------------------------------------------------------------------------------

library(cobalt)


love.plot(m_mod1, stats = "mean.diffs", threshold = 0.1, stars = "std")

love.plot(m_mod2, stats = "mean.diffs", threshold = 0.1, stars = "std")

love.plot(m_mod3, stats = "mean.diffs", threshold = 0.1, stars = "std")

love.plot(m_mod4, stats = "mean.diffs", threshold = 0.1, stars = "std")

love.plot(m_mod5, stats = "mean.diffs", threshold = 0.1, stars = "std")

love.plot(m_mod6, stats = "mean.diffs", threshold = 0.1, stars = "std")


# -->
# mean difference / its standard deviations



# ------------------------------------------------------------------------------
# Create data after matching
# ------------------------------------------------------------------------------

matched_data1 <- match.data(m_mod1)
matched_data2 <- match.data(m_mod2)
matched_data3 <- match.data(m_mod3)
matched_data4 <- match.data(m_mod4)
matched_data5 <- match.data(m_mod5)
matched_data6 <- match.data(m_mod6)


head(matched_data)



# ----------
nrow(matched_data)


# note that matched_data has only matched control and treated



# ------------------------------------------------------------------------------
# plot distance
# ------------------------------------------------------------------------------

graphics.off()

lattice::histogram(~ distance | nj, data = matched_data1, 
                   xlab = "propensity score", ylab = "percentage", 
                   layout = c(1,2), xlim = c(0, 1), ylim = c(0, 60))

lattice::histogram(~ distance | nj, data = matched_data2, 
                   xlab = "propensity score", ylab = "percentage", 
                   layout = c(1,2), xlim = c(0, 1), ylim = c(0, 60))

lattice::histogram(~ distance | nj, data = matched_data3, 
                   xlab = "propensity score", ylab = "percentage", 
                   layout = c(1,2), xlim = c(0, 1), ylim = c(0, 60))

lattice::histogram(~ distance | nj, data = matched_data4, 
                   xlab = "propensity score", ylab = "percentage", 
                   layout = c(1,2), xlim = c(0, 1), ylim = c(0, 60))

lattice::histogram(~ distance | nj, data = matched_data5, 
                   xlab = "propensity score", ylab = "percentage", 
                   layout = c(1,2), xlim = c(0, 1), ylim = c(0, 60))

lattice::histogram(~ distance | nj, data = matched_data6, 
                   xlab = "propensity score", ylab = "percentage", 
                   layout = c(1,2), xlim = c(0, 1), ylim = c(0, 60))


# ------------------------------------------------------------------------------
# Check the assumption of ignorability of treatment assignment
# AUC and ROC: distance's power to discriminate New Jersey or not ?
# ------------------------------------------------------------------------------

library(ROCR)


# AUC
performance(prediction(matched_data1$distance, matched_data1$nj), measure = "auc")@y.values[[1]]
performance(prediction(matched_data2$distance, matched_data2$nj), measure = "auc")@y.values[[1]]
performance(prediction(matched_data3$distance, matched_data3$nj), measure = "auc")@y.values[[1]]
performance(prediction(matched_data4$distance, matched_data4$nj), measure = "auc")@y.values[[1]]
performance(prediction(matched_data5$distance, matched_data5$nj), measure = "auc")@y.values[[1]]
performance(prediction(matched_data6$distance, matched_data6$nj), measure = "auc")@y.values[[1]]


graphics.off()
par(mfrow = c(1,1))
perf <- performance(prediction(matched_data1$distance, matched_data1$nj), "tpr", "fpr")
plot(perf, col = "blue")
abline(a = 0, b = 1)


graphics.off()
par(mfrow = c(1,1))
perf <- performance(prediction(matched_data2$distance, matched_data2$nj), "tpr", "fpr")
plot(perf, col = "blue")
abline(a = 0, b = 1)


graphics.off()
par(mfrow = c(1,1))
perf <- performance(prediction(matched_data6$distance, matched_data6$nj), "tpr", "fpr")
plot(perf, col = "blue")
abline(a = 0, b = 1)


# -->
# method = "nearest" and replace = FALSE:  propensity scores is predictive ...
# method = "nearest" and replace = TRUE:  propensity scores is NOT predictive


