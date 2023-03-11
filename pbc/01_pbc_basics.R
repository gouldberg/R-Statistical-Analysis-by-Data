setwd("//media//kswada//MyFiles//R//pbc")

packages <- c("dplyr", "gamair")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  pbc
#   - Primary biliary cirrhosis is a disease caused by autoimmune damage to the bile ducts of the liver.
#     It can eventually lead to cirrhosis of the liver
#   - Data contains data fromes pbc and pbcseq from a trial conducted at the Mayo clinic between 1974 and 1984 testing
#     the drug D-penicillamine against a placebo.
#   - pbc:  a number of baseline measurements are available for each patient,
#   - pbcseq:  data frame time varying measurements are available.
#   - 6% of the patients exited the trial by receiving a transplant -- here these will be treated as censored.
#
#   - Variables (only main)
#        - stage:  classified as 1 to 4, where 1 corresponds to subtle bile damage and other early signs and 4 is cirrhosis.
#        - albumin:  important blood protein produced in the liber
#        - alk.phos:  alkaline phosphotase is an enzyme particularly concentrated in the liver and bile ducts
#        - ast:  aspartate aminotransferase is an enzyme commonly used as a biomaker for liver health
#        - bili:  bilirubin is a product of the breakdown of aged red boold cells processed in the liver and excreted in bile and urine
#        - platelet:  count of platelets, which are the component of blood responsible for colotting,
#                     but can be destroyed in a damaged liver
#        - protime:  standardized blood clotting time measure related to the previous covariate.
# ------------------------------------------------------------------------------

data(pbc, package = "survival")
data(pbcseq, package = "survival")


str(pbc)
str(pbcseq)


car::some(pbc)
car::some(pbcseq)



# ------------------------------------------------------------------------------
# basic analysis
# ------------------------------------------------------------------------------

plot(time ~ id, data = pbc, type = "h")



# ----------
psych::describe(pbc)



