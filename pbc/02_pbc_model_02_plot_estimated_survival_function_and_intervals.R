setwd("//media//kswada//MyFiles//R//pbc")

packages <- c("dplyr", "gamair", "mgcv")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  pbc
# ------------------------------------------------------------------------------

data(pbc, package = "survival")
data(pbcseq, package = "survival")


str(pbc)
str(pbcseq)


car::some(pbc)
car::some(pbcseq)



# ------------------------------------------------------------------------------
# plot estimated survival functions and one S.E. bands
# ------------------------------------------------------------------------------

np <- 300

patients <- c(10, 66, 25)


par(mfrow=c(1,3))

for(i in patients){
  newd <- data.frame(matrix(0, np, 0))
  
  for(n in names(pbc))  newd[[n]] <- rep(pbc[[n]][i], np)
  
  newd$time <- seq(0, 4500, length = np)
  
  # ----------
  # predict
  fv <- predict(b2, newdata = newd, type = "response", se = TRUE)
  
  
  # plot the survival function with one S.E. intervals
  plot(newd$time, fv$fit, type = "l", ylim = c(0., 1), xlab = "time", ylab = "survival", lwd = 2, main = paste0("patient :", i))
  lines(newd$time, fv$fit + fv$se.fit, col = "grey")
  lines(newd$time, fv$fit - fv$se.fit, col = "grey")
  
  
  # and intervals based on cumulative hazard S.E.
  se <- fv$se.fit / fv$fit
  lines(newd$time, exp(log(fv$fit) + se))
  lines(newd$time, exp(log(fv$fit) + se))
}


# -->
# The figures in each panel are the patient's actual survival or censoring (barred) time.
# The thin black lines show the bands produced via transformation from the cumulative hazard scale,
# whereas the grey lines are approximations direcly on the survival scale.


