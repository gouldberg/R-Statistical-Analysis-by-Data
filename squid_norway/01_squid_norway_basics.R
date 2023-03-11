setwd("//media//kswada//MyFiles//R//squid_norway")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Squid Norway
#   - Isotopes are variants of a chemical element that differ in atomic mass due to possessing different numbers of neutrons.
#     Extra neutrons can make an atom unstable (radioactive), but often the resulting isotpe is stable.
#     Although they have identical chemical properties, due to their slightly greater mass heavier isotopes are preferentially retained in the body,
#     and the proportion of heavy isotopes in body tissues tends to gradually increase over time.
#     In practice, it is the stable isotope composition of the diet that most strongly influences that of body tissues.
#   - Animals higher up a food chain tend to eat larger prey and thus have higher proportions of heavy isotopes in their tissues.
#     Thus, in ecology, following the principle that 'you are what you eat', the ratio of two nitrogen isotopes (N15 to N14) in body tissues
#     can be used as an indicator of trophic level.
#     The change in isotope ratio as an organism moves up the food chain is greater for nitrogen than for carbon, and the ratio of C13 to C12 is
#     often used as an indicator of the isotopic composition of animals at the base of the food chain.
#     Since this differs between marine and freshwater systems and between inshore and offshore waters, etc, carbon isotope ratios are used to
#     identify the ecosystem in which an animal feeds.
#   - Mendes and collaborators measured XXX in the oceanic squid Gonatus fabricii, considered to be the most abundant squid species in the
#     North Atlantic's and sub-arctic regions and a main food source for a variety of higher predators.
#     The underlying ecological question is whether latitude, capture depth, and body size can explain intra-population isotopic variation in G. fabricii
#     and whether those parameters have implications for interpretation of predator isotope values.
# ------------------------------------------------------------------------------
Squid <- read.table(file = "SquidNorway.txt", header = TRUE)


str(Squid)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
