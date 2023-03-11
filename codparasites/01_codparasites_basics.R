setwd("//media//kswada//MyFiles//R//codparasites")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Cod Parasites
#  - The cod fishery is extremely important to the economy of Norway, so anything that affects the health of the cod population
#    and its ecosystem can have severe consequences.
#    The red king crab Paralithodes camtschaticus was deliverately introduced by Russian scientists to the Barents Sea in the 1960s and 1970s
#    from its native area in teh North Pacific. The carapace of these crabs is used by the leech Johanssania arctica to deposit its eggs.
#    This leech in turn is a vector for the blood parasite Trypanosoma murmanensis that can infect marine fish, including cod.
#  - Hammingsen et al. (2005) examined cod for trypanosome infections during annual cruises along the coast of Finnmark in North Norway over three successive years
#    and in four different areas. They show that trypanosome infections are strongest in the area Varangerfjord where the density of red king crabs is highest.
#    Thus, there is evidence that the introduction of the foreign red king crabs had an indirect detrimental effect on the helath of the native cod population.
#    This situation stands out because it is not an introduced parasite that is dangerous for a native host, but rather an introduced host that promotes
#    transmission of two endemic parasites. They call the connections among these factors "an unholy trinity".
#  - This data from Hemmingsen et al. (2005) gives the results for 1254 cod caucht by one ship in annual autumn cruises from 1999 - 2001.
#  - Variables:
#      - intensity:  records the counted number of Trypanosoma parasites found in blood samples from these fish
#      - prevalence:  to distinguish between infected vs. non-infected fish
# ------------------------------------------------------------------------------

data("CodParasites", package = "countreg")


dim(CodParasites)

str(CodParasites)


car::some(CodParasites)



# ----------
CodParasites$prevalance <- ifelse(CodParasites$intensity == 0, "no", "yes")



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
