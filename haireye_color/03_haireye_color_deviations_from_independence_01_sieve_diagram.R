setwd("//media//kswada//MyFiles//R//haireye_color")

packages <- c("dplyr", "datasets", "vcd", "vcdExtra", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  HairEyeColor
# ------------------------------------------------------------------------------
data("HairEyeColor", package = "datasets")

data <- HairEyeColor

data



# ------------------------------------------------------------------------------
# Sieve diagrams
#  - Riedwyl and Schupbach (1983, 1984) proposed a sieve diagram (later called a parquet diagram).
#  - In this display, the area of each rectangle is always proportional to expected frequency but observed frequency is shown by the number of squares in each rectangles.
#  - The difference between observed and expected frequency appears as variations in the density of shading
# ------------------------------------------------------------------------------
# Calculate expected frequencies
haireye <- margin.table(data, 1:2)
expected <- vcd::independence_table(haireye)
round(expected, 1)



# ----------
# Sieve diagrams and expected frequencies 
vcd::sieve(haireye, sievetype = "expected", shade = TRUE, main="Expected frequencies", labeling = labeling_values, value_type = "expected",
      gp_text = gpar(fontface = 2), gp = shading_sieve(interpolate = 0, line_col="darkgrey",eps=Inf,lty="dashed"))


# Sieve diagrams and observed frequencies
vcd::sieve(haireye, shade = TRUE, main="Observed frequencies", labeling = labeling_values, value_type = "observed",
      gp_text = gpar(fontface = 2))


# --> High frequency of blue-eyed blonds and people with brown eyes and dark hair.
# People with hazel eyes are also more likely to have red or brown hair, and those with green eyes more likely to have red or blond hair,
# than would be observed under independence.


