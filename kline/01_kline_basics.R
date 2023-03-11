setwd("//media//kswada//MyFiles//R//kline")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Kline
#   - The island societies of Oceania provide a natural experiment in technological evolution.
#     Different historical island populations possessed tool kits of different size.
#     These kits include fish hooks, axes, boats, hand plows, and many other types of tools.
#   - A number of theories predict that 
#     larger populations will both develop and sustain more complex tool kits. So the natural variation in population size induced by
#     natural variation in island size in Oceania provides a natural experitment to test these ideas.
#     It is also suggested that contact rates among populations effectively increae population sie, as it is relevant to technological evolution.
#     So variation in contact rates among Oceanic societies is also relevant.
# ------------------------------------------------------------------------------
data("Kline", package = "rethinking")

d <- Kline

dim(d)

str(d)


# -->
# keep in mind that the number of rows is not clearly the same as the sample size in a count model.


