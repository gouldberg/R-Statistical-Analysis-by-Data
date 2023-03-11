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
# data exploration:  table and barplot
# ------------------------------------------------------------------------------

( tab <- table(PhdPubs$articles) )


# ----------
k = names(tab)

par(mfrow=c(1,1))
b <- barplot(tab, names.arg = k, xlab = "Number of Articles", ylab = "Frequency")
lines(x = b, y = tab, col = "red")



# -->
# but this is misleading ..., zero frequencies data are not shown.



# ------------------------------------------------------------------------------
# data exploration:  table and barplot with zero frequencies are also shown
# ------------------------------------------------------------------------------

art.fac <- factor(PhdPubs$articles, levels = 0:19)

art.tab <- table(art.fac)

art.tab



# ----------
ci <- mean(PhdPubs$articles) + c(-1, 1) * sd(PhdPubs$articles)


barplot(art.tab, xlab = "Number of articles", ylab = "Frequency", col = "lightblue")

abline(v = mean(PhdPubs$articles), col = "red", lwd = 3)

lines(x = ci, y = c(-4, 4), col = "red", lwd = 3, xpd = TRUE)



# ------------------------------------------------------------------------------
# data exploration:  barplot in log scale
# ------------------------------------------------------------------------------

# in log scale
barplot(art.tab + 1, ylab = "log(Frequency + 1)", xlab = "Number of articles", col = "lightblue", log = "y")


