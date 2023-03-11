setwd("//media//kswada//MyFiles//R//chredlin")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  chredlin
#   - Insurance redlining refers to the practice of refusing to issue insurance to certain types of people or within some geographic area.
#     The name comes from the act of drawing a red line around an area on a map.
#     In the late 1970s, the US Commission on Civil Rights examined charges by several Chicago community organizations that insurance companies were redlining their neighborhood.
#     Because comprehensive information about individuals being refused and renewed in Chicago by zip code for the months of December 1977 through May 1978 was recorded.
#     The FAIR plan was offered by the city of Chicago as a default policy to homeowners who had been rejected by the voluntary market.
#   - The data come from Andrews and Herzberg (1985).
#     Information on other variables that might affect insurance writing such as fire and theft rates was also collected at the zip code level.
#   - The variables are:
#        - race:  racial composition in percentage of minority
#        - fire:  fires per 100 housing units
#        - theft:  thefts per 1000 populations
#        - age:  percentage of housing units built before 1939
#        - involact:  new FAIR plan policies and renewals per 100 housing units
#        - income:  median family income in thousands of dollars
#        - side:  north or sourth side of Chicago
# ------------------------------------------------------------------------------

data("chredlin", package = "faraway")

str(chredlin)


car::some(chredlin)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

