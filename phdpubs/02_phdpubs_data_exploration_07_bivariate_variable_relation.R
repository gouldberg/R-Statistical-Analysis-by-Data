setwd("//media//kswada//MyFiles//R//phdpubs")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  PhdPubs data
# ------------------------------------------------------------------------------

data("PhdPubs", package = "vcdExtra")


dim(PhdPubs)

str(PhdPubs)


car::some(PhdPubs)



# ------------------------------------------------------------------------------
# data exploration:  bivariate variable relation
# ------------------------------------------------------------------------------

var <- colnames(PhdPubs)


# marginal and conditional plots
gpairs(PhdPubs[,var],
       diag.pars = list(fontsize = 16),
       mosaic.pars = list(gp = shading_Friendly, gp_args = list(interpolate=1:4)),
       outer.rot = c(45,45))



# ------------------------------------------------------------------------------
# data exploration:  Including zero-counts for articles
# ------------------------------------------------------------------------------

PhdPubs2 <- PhdPubs %>% mutate(prevalence = ifelse(articles == 0, 0, 1))


var <- colnames(PhdPubs2)


# marginal and conditional plots
gpairs(PhdPubs2[,var],
       diag.pars = list(fontsize = 16),
       mosaic.pars = list(gp = shading_Friendly, gp_args = list(interpolate=1:4)),
       outer.rot = c(45,45))

