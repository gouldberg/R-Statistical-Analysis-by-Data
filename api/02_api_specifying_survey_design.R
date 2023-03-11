setwd("//media//kswada//MyFiles//R//api")

packages <- c("dplyr", "survey")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  subsample from the California Academic Performance Index
# ------------------------------------------------------------------------------

data(api)

str(apiclus1)



# ------------------------------------------------------------------------------
# Specifying survey design
#   - Survey designs are specified using the svydesign function.
#     The main arguments to the function are id to specify sampling units (PSUs and optionally later stages),
#     strata to specify strata, weights to specify sampling weights, and fpc to specify finite population size corrections.
#     These arguments should be given as formulas, referring to columns in a data frame given as the data argument.
#   - The resulting survey design object contains all the data and meta-data needed for analysis,
#     and will be supplied as an argument to analysis functions.
# ------------------------------------------------------------------------------

# The apistrat data frame has stratified independent sample

str(apistrat)


# stratified on stype, with sampling weights pw. 
table(apistrat$stype)
apistrat$pw

# The fpc variable contains the population size for the stratum. 
apistrat$fpc



# ----------
# svydesign():
# ids: formula or data frame specifying cluster ids from largest level to smallest level, ~0 or ~1 is a formula for no clusters
# strata: formula or data frame specifying strata, use NULL for no strata
# fpc: finite population correction
# weights:  formula or vector specifying sampling weights as an alternative to prob

# By default, svydesign assumes that all PSUs, even those in different strata, have a unique value of the id variable.
# This allows some data errors to be detected. If your PSUs reuse the same identifiers across strata then set nest=TRUE.


# ----------
# The finite population correction (fpc) is used to reduce the variance when a substantial fraction of the total population of interest has been sampled.
# It may not be appropriate if the target of inference is the process generating the data rather than the statistics of a particular finite population.

# The finite population correction can be specified either as the total population size in each stratum or as the fraction of the total population that has been sampled.
# In either case the relevant population size is the sampling units.
# That is, sampling 100 units from a population stratum of size 500 can be specified as 500 or as 100/500=0.2.
# The exception is for PPS sampling without replacement, where the sampling probability (which will be different for each PSU) must be used.

# If population sizes are specified but not sampling probabilities or weights, 
# the sampling probabilities will be computed from the population sizes assuming simple random sampling within strata.

dstrat <- svydesign(id = ~ 1, strata = ~ stype, weights = ~ pw, data = apistrat, fpc = ~ fpc)

dstrat



# As the schools are sampled independently, each record in the data frame is a separate PSU.
# This is indicated by id=~1. Since the sampling weights could have been determined from the population size an equivalent declaration would be

dstrat <- svydesign(id = ~ 1, strata = ~ stype,  data = apistrat, fpc = ~ fpc)

dstrat



# ----------
# The apiclus1 data frame is a cluster sample: all schools in a random sample of school districts.

apiclus1$dnum
apiclus1$pw
apiclus1$fpc

dclus1 <- svydesign(id = ~ dnum, weights = ~ pw, data = apiclus1, fpc = ~ fpc)

dclus1


# There is no strata argument as the sampling was not stratified. 
# The variable dnum identifies school districts (PSUs) and is specified as the id argument.
# Again, the weights argument is optional, as the sampling weights can be computed from the population size.
# To specify sampling with replacement, simply omit the fpc argument:
  
dclus1 <- svydesign(id = ~ dnum, weights = ~ pw, data = apiclus1)

dclus1



# ----------
# A design may have strata and clusters.
# In that case svydesign assumes that the clusters are numbered uniquely across the entire sample, rather than just within a stratum.
# This enables some sorts of data errors to be detected. 
# If your clusters are only numbered uniquely within a stratum use the option nest=TRUE to specify this and disable the checking.

# The apiclus2 data set contains a two-stage cluster sample. First, school districts were sampled. If there were fewer than five schools in the district, all were taken, otherwise a random sample of five.

names(apiclus2)

apiclus2$dnum
apiclus2$snum

dclus2 <- svydesign(id = ~ dnum + snum, fpc = ~ fpc1 + fpc2, data = apiclus2)

dclus2



# The multistage nature of the sampling is clear in the id and fpc arguments.
# At the first stage the sampling units are identified by dnum and the population size by fpc1.
# At the second stage, units within each school district are identified by snum and the number of units within the district by fpc2.
# When a finite population correction is not given, and sampling is with replacement, only the first stage of the design is needed.
# The following two declarations are equivalent for treating the two-stage cluster design as if the first stage were with replacement.

dclus2wr <- svydesign(id = ~ dnum + snum, weights = ~ pw, data = apiclus2)

dclus2wr2 <- svydesign(id = ~ dnum, weights = ~ pw, data = apiclus2)

dclus2wr

dclus2wr2
