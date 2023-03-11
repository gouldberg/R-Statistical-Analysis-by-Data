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
# Inverse Probability Weighting
# ------------------------------------------------------------------------------


library(WeightIt)


# estimate weight
# method = "ps":  default, propensity score weighting
weighting <- weightit(formula = nj ~ bk + kfc + roys + wendys + co_owned, data = njmin3_2, method = "ps", estimatnd = "ATE")



summary(weighting)



# ----------
sum(weighting$weights)
nrow(njmin3_2)



# ------------------------------------------------------------------------------
# Check the weights distribution
# ------------------------------------------------------------------------------

matched_data10 <- match.data(weighting)


lattice::histogram(~ weights | nj, data = matched_data10, 
                   xlab = "weights", ylab = "percentage", 
                   layout = c(1,2), col = gray(0.6), border = FALSE)



# ------------------------------------------------------------------------------
# Assess covariate balance
# ------------------------------------------------------------------------------

library(cobalt)


love.plot(weighting, stats = "mean.diffs", threshold = 0.1, stars = "std")



# -->
# BALANCED !!


# ------------------------------------------------------------------------------
# Estimate Average Treatment Effect on Treated using estimated weights
# ------------------------------------------------------------------------------
# use the estimated weights
IPW_result <- lm(data = njmin3_2, formula = fte ~ nj * d, weights = weighting$weights) %>% tidy()


IPW_result



# -->
# coefficient of nj1 * d1  = 2.62
# almost close to the value estimated from matched pair by propensity score

