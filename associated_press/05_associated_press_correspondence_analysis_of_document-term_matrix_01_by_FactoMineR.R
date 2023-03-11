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
# Simple Correspondence Analysis by FactoMineR
# ------------------------------------------------------------------------------

res.ca <- CA(dat_mat)


# main results: inertia associated with each component, coordinates, contributions, 
# and representation qualities of the rows and columns
summary(res.ca)


# comparison with ca()



# ------------------------------------------------------------------------------
# Plot rows and columns separately
# ------------------------------------------------------------------------------

par(mfrow = c(1,2))

plot(res.ca, invisible = "col")

plot(res.ca, invisible = "row")



# ------------------------------------------------------------------------------
# Representing exact barycenters
# ------------------------------------------------------------------------------

coord.col <- sweep(res.ca$col$coord, 2, sqrt(res.ca$eig[,1]), FUN = "*")

coord.row <- sweep(res.ca$row$coord, 2, sqrt(res.ca$eig[,1]), FUN = "*")



# ----------
graphics.off()
par(mfrow = c(1,2))

plot(res.ca, invisible = "col")
points(coord.col, pch = 17, col = "red")
text(coord.col, rownames(coord.col), col = "red")

plot(res.ca, invisible = "row")
points(coord.row, pch = 20, col = "blue")
text(coord.row, rownames(coord.row), col = "blue")




# ------------------------------------------------------------------------------
# Coordinates, relative contributions (in %), and Representation, Quality for Each Category and Dimension
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))
plot(res.ca)


summary(res.ca)


# ----------
# contribution:
# the contribution of point i to the inertia of the dimension of rank s
# the atypical case of a dimension caused by only one or two points can thus be detected immediately



# ----------
# quality of points on a dimension:
# indicates how the deviation of category i from the average profile is expressed on the dimension of rank s.



# ----------
# Distance (squared) to Average Profile and Inertia (in Initial Spaces)
res.ca$row$inertia / res.ca$call$marge.row
res.ca$row$inertia

res.ca$col$inertia / res.ca$call$marge.col
res.ca$col$inertia


# -->
# Distances does not take the category's sample size into account.
