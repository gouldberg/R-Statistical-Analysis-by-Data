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
# Internal type of unfolding
#   - External unfolding is rarely used, because one typically wants to allow the data to speak for themselves.
#     Hence, unfolding is almost always done without using any fixed configurations
# ------------------------------------------------------------------------------

# center ratings
c <- raw - rowMeans(raw)


# turn preference ratings into dissimilarities
diss <- max(c) - c



# ----------
# Search for a configuration with 10 points for the 10 basic values and with 146 additional points for the 146 individuals
# such that the distances between the person points and the value points directly match the dissimilarities (except for an overall scaling factor)
#  --> This model is ratio unfolding (default)
result <- unfolding(diss) 


result

summary(result)



# ----------
plot(result, cex=1, xlab="", ylab="", main="", pch=16, asp=1,
     col.rows="#1010CC66", col.columns="black",
     label.conf.rows=list(col="blue", cex=.6),
     label.conf.columns=list(col="black", cex=1.1))

erg <- fitCircle(result$conf.col[,1], result$conf.col[,2])

draw.circle(erg$cx, erg$cy, radius = erg$radius, border = "black", lty = 2)



# -->
# Almost every person is well represented in this configuration.
# That is not trivial, because, for example, a person who rates achievement as not so important but power as very important would not fit well into this solution,
# because this persons profile would require a point that is far from AC but close to PO.
# Such a point obvisouly does not exist, because AC and PO are close together and, thus, must receive similar ratings.



# ------------------------------------------------------------------------------
# Shepard diagram of unfolding distances versus reversed importance ratings (dissimilarities)
# ------------------------------------------------------------------------------

plot(result, plot.type = "Shepard", pch = 1, cex = 1)


