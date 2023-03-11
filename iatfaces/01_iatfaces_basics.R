setwd("//media//kswada//MyFiles//R//iatfaces")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  iatfaces
#   - Data collected within the context of the implicit association test (IAT).
#     The IAT measures differential association of two target concepts with an attribute.
#   - During the experiment participants saw images of people with long and wide faces, as well as positively and negatively valenced wards.
#     In the first critical block ("congruent block"), participants were asked to press a response key if they saw a long-faced person/positive word
#     and a different response key if they saw a wide-faced person/negative word,
#     In the second critical block ("incongruent block"), the pairing was reversed.
#     IAT theory states that participants are expected to be able to respond fast in the congruent condition and slow in the incongruent condition.
#     The response variable used here is the response time latency in ms.
#   - The dataset includes responses of four participants. Each participant was exposed to 80 trials: 40 congruent block trials, 
#     followed by 40 incongruent block trials.
#     For such settings, where each individual produces his/her own trajectory, an HMM is typically fitted for each person individually.
# ------------------------------------------------------------------------------

data("iatfaces", package = "MPsychoR")


str(iatfaces)


p1dat <- subset(iatfaces, id == 1)
p2dat <- subset(iatfaces, id == 2)
p3dat <- subset(iatfaces, id == 3)
p4dat <- subset(iatfaces, id == 4)


dim(p1dat)
dim(p2dat)



# ------------------------------------------------------------------------------
# basic analysis
# ------------------------------------------------------------------------------
# Here we take the log of the latency response in order to achieve a "healthier" looking distribution with respect to normality,
# since we are going to fit a Gaussian HMM.

par(mfrow = c(2, 2), mar = c(2, 2, 2, 2))
plot(1:80, log(p1dat$latency), type = "l", ylab = "Latency (log)", xlab = "Trial", main = "IAT Latency (Person 1)")
abline(v = 40, col = "gray", lty = 2)

plot(1:80, log(p2dat$latency), type = "l", ylab = "Latency (log)", xlab = "Trial", main = "IAT Latency (Person 2)")
abline(v = 40, col = "gray", lty = 2)
plot(1:80, log(p3dat$latency), type = "l", ylab = "Latency (log)", xlab = "Trial", main = "IAT Latency (Person 3)")
abline(v = 40, col = "gray", lty = 2)
plot(1:80, log(p4dat$latency), type = "l", ylab = "Latency (log)", xlab = "Trial", main = "IAT Latency (Person 4)")
abline(v = 40, col = "gray", lty = 2)



# -->
# For person 1 and 3: two-state model might be fitted.
# For person 4: 4-state model might be fitted
# For person 2: difficult ..





