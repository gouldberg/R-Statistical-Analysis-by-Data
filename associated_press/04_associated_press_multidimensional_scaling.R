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
# Multidimensional Scaling:  by words:  IT TAKES TIME !!! --- NOT RUN !!!
# ------------------------------------------------------------------------------

library(smacof)


mat_dist <- psych::cor2dist(dat_mat)


# IT TAKES TIME !!!
# fit_mds <- mds(mat_dist, type = "interval")


# fit_mds

# summary(fit_mds)




# ----------
# plot(fit_mds)





# ------------------------------------------------------------------------------
# Multidimensional Scaling:  by document
# ------------------------------------------------------------------------------

mat_dist2 <- psych::cor2dist(t(dat_mat))


fit_mds2 <- mds(mat_dist2, type = "interval")


fit_mds2

summary(fit_mds2)




# ----------
plot(fit_mds2)


