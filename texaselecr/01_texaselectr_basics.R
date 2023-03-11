setwd("//media//kswada//MyFiles//R//texaselectr")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Texas Electr
#  - This data were used by Kumbhakar (1996) and Horrace and Schmidt (1996) and concern the production cost of electric firms in Texas.
# ------------------------------------------------------------------------------

data("TexasElectr", package = "pder")


str(TexasElectr)


dim(TexasElectr)


car::some(TexasElectr)

  

# ------------------------------------------------------------------------------
# basics:  
# ------------------------------------------------------------------------------


