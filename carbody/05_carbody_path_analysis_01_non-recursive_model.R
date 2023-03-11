
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\carbody")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  car body coating process data
# ------------------------------------------------------------------------------

dat <- read.csv("carbody_dat.txt", header = TRUE, skip = 2, sep = "\t")


str(dat)


car::some(dat)




# ----------
dat_s <- data.frame(scale(dat))


head(dat_s)




# ------------------------------------------------------------------------------
# Path analysis:  non-recursive model
# ------------------------------------------------------------------------------


dat_s2 <- dat_s %>% dplyr::select(-centmaku, -makuwid)



library(lavaan)


mod_form <- '
fukituke ~ temp
kishaku ~ moisture
airpres ~ fukituke
patternwid ~ fukituke + airpres
gunspeed ~ moisture
toryotemp ~ temp
nendo ~ temp + kishaku + toryotemp
hakidashi ~ gunspeed + temp + moisture + nendo
tochaku ~ fukituke + patternwid + hakidashi + toryotemp
'


fit1 <- sem(mod_form, data = dat_s2)



# ----------
summary(fit1, standardized = T, fit.measures = T)




# ----------
library(semPlot)


#semPaths(fit1, whatLabels = "stand", layout = "tree", style = "lisrel",
#         ncharNodes = 0, sizeMan = 8, edge.label.cex = 1.2)


par(mfrow = c(1,1))

semPaths(fit1, whatLabels = "stand")




# -->
# note that path coefficient

# fukituke --> tochaku = -0.64

# fukituke --> patternwid = -0.362
# patternwid --> tochaku = -0.470

# fukituke --> airpress = 0.191
# pirpress --> patternwid = 0.324
# patternwid --> tochaku = -0.470


# tukituke --> tochaku  total effect is -0.492
-0.64 + (- 0.362 * -0.470) + (0.191 * 0.324 * -0.370)





# ----------
summary(object = fit1, fit.measures = TRUE)




# -->
# test statistic (X^2) is not rejected  (null: the model path is true)
# CFI (comparative fit index) = 0.979 (>= 0.95)
# TLI (Tucker-Lewis Index) = 0.968
# RMSEA = 0.052 < 0.05,  good fit
# SRMSR = 0.094, close to zero (goodi fit)



# ----------
parameterEstimates(fit1, ci = TRUE)

standardizedSolution(fit1)





