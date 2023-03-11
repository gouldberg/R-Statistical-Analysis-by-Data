setwd("//media//kswada//MyFiles//R//brains")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  brains
# ------------------------------------------------------------------------------

data(brains, package = "gamlss.mx")

str(brains)


car::some(brains)



# ----------
# Since the distribution of both brain size and body weight are highly skewed, a log transformation was applied to both
# variables to give transformed variables

brains <- transform(brains, lbrain = log(brain), lbody= log(body))




# ------------------------------------------------------------------------------
# Plot fitted values and weighted average
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))
with(brains, plot(lbody, lbrain, 
                  pch = c(21, 22, 23)[max.col(br.3$post.prob[[1]])],
                  bg = c("red", "green3", "blue")[max.col(br.3$post.prob[[1]])]))

for(k in 0:3){
  with(brains, lines(fitted(br.3, K = k)[order(lbody)] ~ lbody[order(lbody)],
                     lty = k+1, lwd = 2, col = c("black", "red", "green3", "blue")[k+1]))
}

legend("topleft", legend = c("Weighted Average", "Component 1", "Component 2", "Component 3"), pch = c(20, 21, 22, 23),
       pt.bg = c("black", "red", "green3", "blue"), lty = 1:4, lwd = 2, col = c("black", "red", "green3", "blue"))



# ----------
# The weighted average for the (conditional) parameters mu for the K components for each observation
fitted(br.3, K = 0)

