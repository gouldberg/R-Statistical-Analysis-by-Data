# setwd("//media//kswada//MyFiles//R//nondurables")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\nondurables")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  nondurables
#   - the nondurable goods manufacturing index for the United States.
# ------------------------------------------------------------------------------

# data("nondurables", package = "fda")

nondurables <- read.csv("nondurables.txt", header = TRUE) %>% pull()

nondurables <- ts(nondurables, start = 1919, end = 2000, frequency = 12)


str(nondurables)


# this is time-series data




# ------------------------------------------------------------------------------
# More closer look at a comparatively stable period by smoothing and interpolation
# ------------------------------------------------------------------------------

# set up a basis for smoothing log nondurable goods index

goodsbasis  = create.bspline.basis(rangeval=c(1919,2000), nbasis=979, norder=8)


goodsbasis



# ----------
# Convert integer to linear differential operator
# m = 1:  penalize the square of the slope or valocity
# m = 2:  penalize the squared acceleration
# m = 3:  penalize the squared rate of change of acceleration
# m = 4:  penalize the squared curvature of acceleration

LfdobjNonDur= int2Lfd(m = 4)




# ----------
# smooth the data using function smooth.basisPar, which does not require setting up a functional parameter object
# zoo::coredata():  generic functions for extracting the core data contained in a (more complex) object and replacing it.

logNondurSm = smooth.basisPar(argvals=zoo::index(nondurables),
                              y=log10(zoo::coredata(nondurables)), fdobj=goodsbasis,
                              Lfdobj=LfdobjNonDur, lambda=1e-11)



# ----------
# The log nondurable goods index for 1964 to 1970,  1990 to 1996

sel64.70 = ((1964<=zoo::index(nondurables)) & (zoo::index(nondurables)<=1970) )
sel90.96 = ((1990<=zoo::index(nondurables)) & (zoo::index(nondurables)<=1996) )

t64.70 = seq(1964, 1970, len=850)
t90.96 = seq(1990, 1996, len=850)


par(mfrow = c(2,1))

plot(zoo::index(nondurables)[sel64.70],
     log10(nondurables[sel64.70]), xlab='Year',
     ylab='Log10 Nondurable Goods Index', las=1, ylim = c(1.60, 1.80))

abline(v=1965:1970, lty='dashed')

lines(t64.70, predict(logNondurSm, t64.70))


plot(zoo::index(nondurables)[sel90.96],
     log10(nondurables[sel90.96]), xlab='Year',
     ylab='Log10 Nondurable Goods Index', las=1, ylim = c(1.95, 2.15))

abline(v=1990:1996, lty='dashed')

lines(t90.96, predict(logNondurSm, t90.96))




# -->
# The nondurable goods manufacturing index varies fairly smoothly and regularly within each year.
# We now see that the variation within this year is more complex.
# This curve oscillates three times during a year, with the size of the oscillation being smallest in spring,
# larger in the summer, and largest in the autumn.

# In fact, each year shows smooth variation with a similar amount of detail, and we now consider how we can explore these within-year patterns.


