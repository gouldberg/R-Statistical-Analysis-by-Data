setwd("//media//kswada//MyFiles//R//helm")

packages <- c("dplyr", "smacof")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  helm
# ------------------------------------------------------------------------------

data(helm, package = "smacof")


str(helm)


attributes(helm)


car::some(helm)



# ------------------------------------------------------------------------------
# Fit dimensional weighting model (INDSCAL model)
# ------------------------------------------------------------------------------

res.helm <- indscal(helm, type = "interval")

res.helm



# Stress-1 value
res.helm$stress


# group space
res.helm$gspace



# ------------------------------------------------------------------------------
# Plot group space
# ------------------------------------------------------------------------------
# group space, the expected color circle, slightly squeezed in the vertical direction as the fitted circle (dashed line) shows.
graphics.off()
par(mfrow=c(1,1))

plot(res.helm)
polygon(res.helm$gspace)

erg <- smacof::fitCircle(res.helm$gspace[,1], res.helm$gspace[,2])
draw.circle(erg$cx, erg$cy, radius = erg$radius, border = "black", lty = 2)



# ------------------------------------------------------------------------------
# Space of 2 extreme persons
# ------------------------------------------------------------------------------

# Weight matrix for 2 persons
# CD4:  most deuteranopic person,  N6a:  color-normal person
res.helm$cweights$CD4

res.helm$cweights$N6a


# -->
# person CD4 stretches the group space along Dimension 1 relative to Dimension 2 by a factor of roughly 3 (= 1.25 / 0.44)
# person N6a stretches the group space along Dimension 2 relative to Dimension 1 by a factor of roughly 1.45 (= 1.22 / 0.84)
# Perso N6a weights Dimension 2 three times as much as CD4


# ----------
# Stress per subject (%)
sort(res.helm$sps)



# ----------
# plot 2 persons
op <- par(mfrow = c(1,2))

plot(res.helm$conf$CD4, pch = 20, cex = 0.8, main = "CD4", xlim = c(-1,1), ylim = c(-1,1), asp = 1)
polygon(res.helm$conf$CD4)
text(res.helm$conf$CD4, labels = rownames(res.helm$conf$CD4), cex = 0.8, pos = 3)

plot(res.helm$conf$N6a, pch = 20, cex = 0.8, main = "N6a", xlim = c(-1,1), ylim = c(-1,1), asp = 1)
polygon(res.helm$conf$N6a)
text(res.helm$conf$N6a, labels = rownames(res.helm$conf$N6a), cex = 0.8, pos = 3)

par(op)


# -->
# CD4:  The color circle is streatched along Dimension 1 or, which has the same effect, it is comporessed along Dimension 2.
# This reflects that this person cannot discriminate well between green and the purplish-red colors

# N6a:  stretches the group space somewhat along Dimension 2.



# ------------------------------------------------------------------------------
# Visualize subject space
# ------------------------------------------------------------------------------

# extract weight for each dimension for each subject
sub_space <- t(sapply(1:length(res.helm$cweights), function(x) diag(res.helm$cweights[[x]])))

row.names(sub_space) <- names(helm)

sub_space



# ----------
# Visualize individual weights (subject space of an INDSCAL solution)
graphics.off()
par(mfrow = c(1,1))

plot(sub_space, pch = 20, cex = 0.8, xlim = c(0.2, 1.5), ylim = c(0.2, 1.5), asp = 1)
text(sub_space, labels = rownames(sub_space), cex = 0.8, pos = 3)
abline(0, 1)

text(sub_space, labels = rownames(sub_space), cex = 0.8, pos = 3)
draw.circle(x = 0, y = 0.15, radius = 1.4, border = "black", lty = 2)



# -->
# One must be careful, though, in interpreting these weights.
# They are only meaningful relative to the group space, and the group space is, unfortunately, not unique.
# If the group space is stretched or compressed along its dimensions, different weights are found for each person, while the overall fit of the MDS
# solution remains the same.


