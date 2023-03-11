# setwd("//media//kswada//MyFiles//R//owls")
setwd("//media//kswada//MyFiles//R//Mixed_effects_and_multilevel_model//owls")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Owls
# ------------------------------------------------------------------------------

Owls <- read.table(file = "Owls.txt", header = TRUE)


str(Owls)

dim(Owls)

car::some(Owls)



# ----------
M.lm <- lm(NegPerChick ~ SexParent * FoodTreatment + SexParent * ArrivalTime, data = Owls)


mod_obj <- M.lm




# ------------------------------------------------------------------------------
# Try data transformation (log10)
# ------------------------------------------------------------------------------

# However, as there is no clear pattern in any of these graphs, we cannot easily model the heterogeneity.
# As applied a log10(Y + 1) transformation on the sibling negotiation data

Owls$LogNeg <- log10(Owls$NegPerChick + 1)

M2.lm <- lm(LogNeg ~ SexParent * FoodTreatment + SexParent * ArrivalTime,data=Owls)

plot(M2.lm, 1)


E <- rstandard(M2.lm)

op <- par(mar=c(3,3,2,2))
boxplot(E ~ Nest, data = Owls, axes=FALSE, ylim=c(-3,3))
abline(0,0)
axis(2)
text(1:27,-2.5, levels(Owls$Nest), cex=0.75, srt=65)
par(op)



# -->
# For some nests, all residuals are above or below the zero line, indicating that the term 'nest' has to be included in the model.



