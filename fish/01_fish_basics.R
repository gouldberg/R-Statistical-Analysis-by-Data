setwd("//media//kswada//MyFiles//R//fish")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  fish
#   - This data are records of visits to a national park.
#     The question of interest is how many fisha n average visitor takes per hour, when fishing.
#     The problem is that not everyone tried to fish, so the numbers are zero-inflated.
#   - variables
#       - fish_caucht (outcome):  number of fish caught during visit
#       - livebait:  whether or not group used livebait to fish
#       - comper:  whether or not group had a camper
#       - persons: number of adults in group
#       - child: number of children in group
#       - hours: number of hours group spent in park
# ------------------------------------------------------------------------------
data("Fish", package = "rethinking")

d <- Fish

dim(d)

str(d)



# ------------------------------------------------------------------------------
# data:  basics
# ------------------------------------------------------------------------------

# distribution of number of fishes per hour
d %>% mutate(fish_per_h = fish_caught / hours) %>% dplyr::select(fish_per_h) %>% .[[1]] %>% hist(., breaks = seq(0, 70, 1), ylim = c(0, 20))


# ----------
# distribution of number of fishes per hour * persons
d %>% mutate(fish_per_hp = fish_caught / (hours * persons)) %>% dplyr::select(fish_per_hp) %>% .[[1]] %>% hist(., breaks = seq(0, 30, 1), ylim = c(0, 20))


