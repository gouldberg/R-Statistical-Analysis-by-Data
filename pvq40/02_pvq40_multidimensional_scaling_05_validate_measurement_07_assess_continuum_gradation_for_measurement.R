setwd("//media//kswada//MyFiles//R//pvq40")

packages <- c("dplyr", "smacof")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  PVQ40
# ------------------------------------------------------------------------------

data(PVQ40, package = "smacof")


str(PVQ40)


attributes(PVQ40)


car::some(PVQ40)



# ------------------------------------------------------------------------------
# Search for a configuration with all 40 points and check clusters or transition
# ------------------------------------------------------------------------------


data.r <- na.omit(PVQ40)


# center ratings
data1 <- data.r - rowMeans(data.r)


# turn preference ratings into dissimilarities
diss <- max(data1) - data1



# ----------
# Search for a configuration with all 40 points and with 146 additional points for the 146 individuals
# such that the distances between the person points and the value points directly match the dissimilarities (except for an overall scaling factor)
#  --> This model is ratio unfolding (default)
unf <- unfolding(diss)

graphics.off()
plot(unf, what="columns", col.columns=1, asp=1, xlim=c(-1.3,1.3), ylim=c(-1.3,1.3),
     label.conf.columns=list(col="black"), main="", pch=16)



# ----------
# fit circle
circle <- fitCircle(unf$conf.col[,1], unf$conf.col[,2])

draw.circle(circle[[1]], circle[[2]], radius=circle[[3]])



# -->
# The items in some categories (e.g., TR) scatter quite a bit in space, while others (e.g., PO) form dense clusters.
# Also, there is considerable overlap of the various types of value items (e.g., BE and UN).
# This indicates that the circle of 10 basic values may be understood more as a continuum of personal values with gradual transitions
# rather than as a necklace of discrete points



