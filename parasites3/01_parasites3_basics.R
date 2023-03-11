setwd("//media//kswada//MyFiles//R//parasites3")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  parasites3
#   - Sardella and Timi (2004) examined the parasite populations and communities of the hake species Merluccius hubbsi captured in four zones
#     off the Argentine coast and used these as bioogical tags for discriminating among populations.
#   - The hake fishery in Argentina is in danger of collapse due to overexploitation that has produced a severe decline in fish catches in recent years.
#     Knowledge of stock composition of the Argentine hake fishery is essential to facilitating the recovery of M.hubbsi.
#   - Fish were caught by trawl in four areas: the Argentine-Uruguayan Common Fishing Zone (June - December 1993),
#     the San Jorge Gulf (july 1998), the Patagonian Shelf (July - September 1999), and the San Matias Gulf (July 1999).
#     A total of 344 specimens of M.hubbsi were examined for parasites.
#     To minimize the influence of host size or age on the parasite burden, only adult fish of comparable length were included in the analysis.
#     Twenty-size parasite species were found.
#   - The underlying biological quesiton is whether the prevalence of E.oatesi can be modelled as a function of size (length or weight), sex, and area.
#     We are also interested in whether the length effect differs with sex and area.
#     Some of the other covariates (year, month) may play a role as well, although we do not expect this.
#   - Variables:
#        - Month, Year:  Month, year of sampling
#        - Area:  Area (zone) of sampling
#        - ID:  identifier for specimen
#        - Length:  Total length (cm)
#        - Weight:  Weight
#        - Sex:  Sex
#        - Elytrophalloides_oatesi:  Absence or presence of Elytrophalloides oatesi
# ------------------------------------------------------------------------------

Par <- read.table(file = "Parasites3.txt", header = TRUE)


str(Par)

dim(Par)



# ----------
Par$Worms <- Par$Elytrophalloides_oatesi



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

table(Par$Worms)



# -->
# We have roughly similar number of zeros (absence) and ones (presence):
# hence a GLM with a binomial distribution and logistic link function can be considered for the analysis

