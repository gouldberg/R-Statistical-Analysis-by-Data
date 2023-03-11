setwd("//media//kswada//MyFiles//R//ew_eng")

packages <- c("dplyr", "smacof")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  EW_eng
# ------------------------------------------------------------------------------

data(EW_eng, package = "smacof")


str(EW_eng)


EW_eng$east

EW_eng$west



# ------------------------------------------------------------------------------
# MDS for each group (East and West)
#   - We can use as initial solution obtained from other group's solution
# ------------------------------------------------------------------------------

labels.short <- c("interesting","independent","responsibility","meaningful", 
                  "advancement","recognition","help others","useful","social","secure job", 
                  "income", "spare time", "healthy")

attr(EW_eng$west, "Labels") <- attr(EW_eng$east, "Labels") <- labels.short



# ----------
res.west <- mds(sim2diss(EW_eng$west, method="corr"), type="ordinal")



res.east <- mds(sim2diss(EW_eng$east, method="corr"), type="ordinal", 
                init=res.west$conf)



# ----------
# Use initial solution of res.west
res.east2 <- mds(sim2diss(EW_eng$east, method="corr"), type="ordinal", 
                init=res.west$conf)



# ----------
res.west$stress

res.east$stress

res.east2$stress



# ----------
par(mfrow = c(2,3))
plot(res.west, main="west")
plot(res.east, main="east")
plot(res.east2, main="east2")

plot(res.west, plot.type="Shepard", main=paste0("west (Stress1 = ", round(res.west$stress, 2), ")"))
plot(res.east, plot.type="Shepard", main=paste0("east (Stress1 = ", round(res.east$stress, 2), ")"))
plot(res.east2, plot.type="Shepard", main=paste0("east2 (Stress1 = ", round(res.east2$stress, 2), ")"))
