setwd("//media//kswada//MyFiles//R//intelligence")

packages <- c("dplyr", "smacof")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  intelligence
#   - A frequent application of MDS is testing structural hypotheses. A typical case is intelligence diagnostics.
#     Here, persons are asked to solve several test items. The items can be classified on the basis of their content into different categories
#     of two design factors, called facets in this context.
#     Some test items require the testee to solve computational problmes with numbers and numerical operations.
#     Other items ask for geometrical solutions where figures have to be rotated in 3-dimensional space or pictures have to be completed.
#     Other test items require applying learned rules, while still others have to be solved by finding such rules.
#   - The data in our smalle example are the inter-correlations of 8 intelligence test items.  The items are coded in terms of the facets
#     "Format = {N(umerical), G(eometrical)}" and "Requirement = {A(pply), I(nfer)}
# ------------------------------------------------------------------------------

data(intelligence, package = "smacof")


intelligence

car::some(intelligence)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------


