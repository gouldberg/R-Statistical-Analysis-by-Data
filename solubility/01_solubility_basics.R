setwd("//media//kswada//MyFiles//R//solubility")

packages <- c("dplyr", "AppliedPredictiveModeling", "caret", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# QSAR modeling (Quantitative Structure-Activity Relationship)
#   - Chemicals, including drugs, can be represented by chemical formulas. From this configuration, quantitative measurements can be derived,
#     such as the molecular weight, electrical charge, or surfce area. These quantities are referred to as chemical descriptors, and
#     there are myriad types of descriptors that can be derived from chemical equation.
#   - Some characteristics of molecules cannot be analytically determined from the chemical structure. The relationship between the 
#     chemical structure and its activity is usually determined empirically using experiments.
#   - One way to do this is to create a biological assay for the target of interest.  A set of compunds can then be placed into assay and their activity,
#     or inhibition, is measured. This activity information generates data which can be used as the training set for predictive modeling so that
#     compounds, which may not yet exist, can be screened for activity.
#     --> this process is referred to as QSAR (Quantitative Structure-Activity Relationship) modeling. Leach and Gillet (2003) provide a high-level
#     introcution to QSAR modeling and molecular descriptors.
#
# data:  solubility
#   - While activity is important, other characteristics need to be assessed to determine if a compound is "drug-like". Physical qualities, such as
#     solubility or lipophilicity (i.e., "greasiness"), are evaluated as well as other properties, such as toxicity.
#     Compound's olubility is very important if it is to be given orally or by injection.
#   - Tetko et al. (2001) and Huuskonen (2000) investigated a set of compounds with corresponding experimental solubility values using complex sets of
#     descriptors. They used linear regression and neural network models to estimate the relationship between chemical structure and solubility.
#
#   - For our analyses, we will use 1,267 compounds and a set of more understandable descriptors that fall into one of 3 groups.
#       - 208 binary "fingerprints" that indicate the presence or absence of a particular chemical substructure
#       - 16 count descriptors, such as the number of bonds or the number of bromine atoms.
#       - 4 continuous descriptors, such as molecular weight or surface area.
#
# ------------------------------------------------------------------------------
data("solubility", package = "AppliedPredictiveModeling")

ls(pattern = "^solT")


dim(solTrainX)

names(solTrainX)

head(solTestY)


str(solTrainX)



