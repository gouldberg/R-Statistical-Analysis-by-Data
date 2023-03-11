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
# data exploration:  discretized Y (zero counts) vs. continuous and categorical X
# ------------------------------------------------------------------------------

data <- PhdPubs

plot(factor(articles == 0) ~ factor(married), data = data, ylab = "No articles", ylevels=2:1)

plot(factor(articles == 0) ~ factor(female), data = data, ylab = "No articles", ylevels=2:1)

plot(factor(articles == 0) ~ factor(phdprestige), data = data, ylab = "No articles", ylevels=2:1)

plot(factor(articles == 0) ~ factor(kid5), data = data, ylab = "No articles", ylevels=2:1)

plot(factor(articles == 0) ~ mentor, data = data, breaks = quantile(mentor, probs = seq(0,1,0.2)), ylevels=2:1, ylab = "No articles")



# ----------
# similar plot by conditional density plot
# --> no need to distretize continuous variables

cdplot(factor(articles == 0) ~ mentor, data = data, ylevels=2:1, ylab = "No articles")



# ------------------------------------------------------------------------------
# data exploration:  discretized Y (zero counts) vs. categorical X by doubledecker
# ------------------------------------------------------------------------------

phd.tab <- xtabs(~ married + female + factor(articles == 0), data = PhdPubs)


names(dimnames(phd.tab))[3] <- "NoArticles"

doubledecker(NoArticles ~ female + married, data = phd.tab, margins = c(1, 5, 3, 1))



# ----------
# mosaic plot in the doubledecker format
doubledecker(NoArticles ~ female + married, data = phd.tab,
             gp = shading_hcl, expected = ~ female:married + NoArticles,
             margins = c(1, 5, 3, 1))



# ----------
phd.tab <- xtabs(~ kid5 + cutfac(mentor, q = 5) + factor(articles == 0), data = PhdPubs)

names(dimnames(phd.tab))[3] <- "NoArticles"
names(dimnames(phd.tab))[2] <- "mentor_c"

doubledecker(NoArticles ~ kid5 + mentor_c, data = phd.tab, margins = c(1, 5, 3, 1))

doubledecker(NoArticles ~ kid5 + mentor_c, data = phd.tab,
             gp = shading_hcl, expected = ~ kid5:mentor_c + NoArticles,
             margins = c(1, 5, 3, 1))


# -->
# Shading reflects departure from a model in which prevalence is independent of two variables jointly.
# This plot is helpful to assess whether we need to include interaction to model zero counts

