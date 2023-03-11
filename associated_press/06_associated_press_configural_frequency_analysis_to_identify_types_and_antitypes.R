# setwd("//media//kswada//MyFiles//R//associated_press//")
setwd("//media//kswada//MyFiles//R//Bayesian_inference//associated_press//")

packages <- c("dplyr", "rstan")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Associated Press (AP)
#   - the partial sample from AP documents (from "topicmodels")
# ------------------------------------------------------------------------------

source("associated_press.R")

# number ot topics
K

# number of words
V

# number of documents
D

# data size
N


wordID
length(unique(wordID))


freq


docID


# ----------
data <- data.frame(docID = docID, wordID = wordID, freq = freq)
dat_mat <- data %>% tidyr::spread(., key = wordID, value = freq) %>% dplyr::select(-docID) %>% as.matrix()
dat_mat[is.na(dat_mat)] <- 0
rownames(dat_mat) <- paste0("doc_", 1:100)
colnames(dat_mat) <- paste0("wd", 1:4954)



# ------------------------------------------------------------------------------
# Configural Frequency Analysis for 2-dimensional table
#   - General method for contingency tables and aims to find so-called types and antitypes.
#     Cells that have significantly more observations than expected are identified as types, conversely, cells that have significantly less observations
#     than expected are declared as antitypes.
# ------------------------------------------------------------------------------

# The list of Docs and Terms (Rows and Columns)
( configs <- expand.grid(dimnames(dat_mat)) )


( counts <- as.vector(dat_mat) )



# ----------
# analysis of configuration frequenceis (CFA, NOT Confirmatory Factor Analysis !!!)
library(cfa)

fit.cfa <- cfa(configs, counts, binom.test = TRUE, sorton = "n")

names(fit.cfa)


head(fit.cfa$table)


fit.cfa$summary.stats

fit.cfa$levels



# ----------
# print top 10 types out of a total 95 types, as identified by the binomial test.
types <- fit.cfa$table[fit.cfa$table$sig.bin == TRUE, 1:3]

head(types, 10)



# -->
# The strongest type is doc_90 on wd209
