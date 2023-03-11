setwd("//media//kswada//MyFiles//R//tv")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  TV viewing data
# ------------------------------------------------------------------------------
data("TV", package = "vcdExtra")

TV

dim(TV)

str(TV)


TV2 <- margin.table(TV, c(1,3))

TV2



# ------------------------------------------------------------------------------
# Stacking table:  3-way table into 2-way stacked table
# ------------------------------------------------------------------------------

TV.df <- as.data.frame.table(TV)


levels(TV.df$Time) <- rep(c("8", "9", "10"), c(4, 4, 3))


TV3 <- xtabs(Freq ~ Day + Time + Network, TV.df)


TV3



# ------------------------------------------------------------------------------
# Multiple correspondence analysi by mjca() for 3-way table directly
# plot of Burt matrix (?):  category points are joined separately by lines for the factor variables
# ------------------------------------------------------------------------------

res <- plot(tv.mca, labels = 0, pch = ".", cex.lab = 1.2)


coords <- data.frame(res$cols, tv.mca$factors)

nlev <- tv.mca$levels.n

fact <- unique(as.character(coords$factor))


cols <- c("blue", "red", "gray")
points(coords[,1:2], pch=rep(16:18, nlev), col=rep(cols, nlev), cex=1.2)
text(coords[,1:2], label=coords$level, col=rep(cols, nlev), pos=3, cex=1.2, xpd=TRUE)


lwd <- c(2, 2, 2, 4)
for(i in seq_along(fact)) lines(Dim2 ~ Dim1, data = coords, subset = factor==fact[i], lwd = lwd[i], col = cols[i])
legend("bottomright", legend = c("Day", "Time", "Network"), title = "Factor", title.col = "black", col = cols, text.col = cols, pch = 16:18, bg = "gray95", cex = 1.2)



# -->
# If you compare the MCA result with mosaic matrix, you will see that they are both showing the bivariate pairwise associations among these variables,
# but in different ways.
# The mosaic plots show the details of marginal and joint frequencies together with residuals from independence for each 2 * 2 marginal subtable.
# The MCA plot using the Burt matrix summarized each category poin in terms of a 2D representation of contributions to total inertia (association).


pairs(TV3, gp = shading_Friendly2, space = 0.25, gp_args = list(interpolate = 1:4), diag_panel_args = list(offset_varnames = -0.5))



