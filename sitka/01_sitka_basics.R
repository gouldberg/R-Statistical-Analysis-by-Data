setwd("//media//kswada//MyFiles//R//sitka")

packages <- c("dplyr", "lattice", "SemiPar")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# This tutorial is based on
# Chapter 7. GAMs in Practive: mgcv from "Generalized Additive Models An Introduction with R 2nd Edition" (by Simon N. Wood)


# ------------------------------------------------------------------------------
# data:  sitka
#   - 79 growth trajectories of Sitka spruce trees grown in either elevated ozone or control conditions.
#   - Variables
#        - id.num:  identity of the tree: 1 ... 79
#        - order:  time order ranking within each tree
#        - days:  since 1st January, 1988
#        - log.size:  log of tree 'size'
#        - ozone:  1 enhanced ozone treatment  0 control
# ------------------------------------------------------------------------------
data(sitka, package = "SemiPar")


str(sitka)


car::some(sitka)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

