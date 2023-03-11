setwd("//media//kswada//MyFiles//R//jansen_strayberry")

packages <- c("dplyr", "datasets", "vcd", "vcdExtra", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  jansen.strawberry
#  - frequency data frame of counts of damage from fungus from a field experiment reported by Jansen (1990).
# ------------------------------------------------------------------------------
data("jansen.strawberry", package = "agridat")

data <- jansen.strawberry

car::some(data)



# ----------
# create a 3 * 4 * 3 table of crossings of 3 male parents with 4 (different) female parents, recording the number of plants in 4 blocks of 9 or 10 plants
# each showing red core disease in 3 ordered categories, C1, C2, or C3

dat <- transform(data, category = ordered(category, levels = c("C1", "C2", "C3")))
levels(dat$male) <- paste0("M", 1:3)
levels(dat$female) <- paste0("F", 1:4)

tab <- xtabs(count ~ male + female + category, data = dat)

names(dimnames(tab)) <- c("Male parent", "Female parent", "Disease category")


ftable(tab)



# ------------------------------------------------------------------------------
# mosaic display for 3-way association, highlighting "Disease category"
#  - 3-way mosaic plot with the tiles colored in increasing shades of some color according to disease category
# ------------------------------------------------------------------------------
mosaic(tab, highlighting = "Disease category")


# -->  C3 is largest in F1 regardless of male parent



mosaic(margin.table(tab, c(2,1,3)), highlighting = "Disease category")




