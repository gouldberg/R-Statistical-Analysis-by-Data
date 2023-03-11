setwd("//media//kswada//MyFiles//R//pvq40")

packages <- c("dplyr", "smacof")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  PVQ40
# ------------------------------------------------------------------------------

data(PVQ40, package = "smacof")


str(PVQ40)


attributes(PVQ40)


car::some(PVQ40)



# ------------------------------------------------------------------------------
# Ordinal multidimensional scaling
# ------------------------------------------------------------------------------

# inter-correlation of the scores
( r <- cor(PVQ40, use = "pairwise.complete.obs") )



# Convert correlations to dissimilarity matrix
diss <- sim2diss(r, method = "corr") 



# ----------
# Ordinal MDS
res <- mds(delta = diss, type = "ordinal")

codes <- substring(colnames(PVQ40), 1, 2)


plot(res)


# -->
# You can add by hand straight lines to cut ("partition") the space like a acake into wedge-like regions.
# The partitioning lines form a particular pattern called circumplex, a circle of regions enmanating from a common origin.
# Each region contains only items of one particular type, except for a few minor errors where points (e.g. col4 or tr1)
# fall into the respective neighboring region.
# Such a structure is certainly unlikely to result by change, and this is even more true since it replicates numerous similar studies.


