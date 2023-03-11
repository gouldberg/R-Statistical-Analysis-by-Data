setwd("//media//kswada//MyFiles//R//neuralactivity")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  NeuralActivity
# ------------------------------------------------------------------------------

data("NeuralActivity", package = "MPsychoR")

str(NeuralActivity)


NeuralActivity



# ------------------------------------------------------------------------------
# Individual Differences Scaling (three-way MDS)
# plot configuration with external classification
# ------------------------------------------------------------------------------

# As an additional plotting flavor, we incorporate external information (social dimension scores) in the configuration plot
# and color the points according to their scores on this scale.

data(NeuralScales, package ="MPsychoR")


NeuralScales$Social


cols <- cut(NeuralScales$Social, 5, labels = FALSE)



library(colorspace)
colpal <- rev(sequential_hcl(5))

pal <- palette(colpal)


# The configuration plot (group stimulus space)
plot(fitNeuro, col = cols, label.conf = list(col = cols), main = "Neural Activity Space")



# ----------

library(SDMTools)
SDMTools::legend.gradient(cbind(x = c(0.85,0.95,0.95,0.85), y = c(-0.9,-0.9,-0.5,-0.5)), 
                limits = c("low", "high"), cols = colpal, title = "Social", cex = 0.8)

palette(pal)


# -->
# Wee see that states where interaction with other people is required (e.g., affection, friendliness, playfulness, dominance, lust)
# score highly on the social dimension.

