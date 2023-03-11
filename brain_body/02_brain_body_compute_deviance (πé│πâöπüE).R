setwd("//media//kswada//MyFiles//R//brain_body")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  brain and body
#   - Average brain volumes and body masses for seven hominin species
# ------------------------------------------------------------------------------
sppnames <- c("afarensis", "africanus", "habilis", "boisei", "rudolfensis", "ergaster", "sapiens")

brainvolcc <- c(438, 452, 612, 521, 752, 871, 1350)

masskg <- c(37.0, 35.5, 34.5, 41.5, 55.5, 61.0, 53.5)

d <- data.frame(species = sppnames, brain = brainvolcc, mass = masskg)

str(d)



# ------------------------------------------------------------------------------
# Simulate the distribution of deviance by number of parameters
# ------------------------------------------------------------------------------
# You can parallelize the simulations by replacing the replicate() line with mcreplicate()
N <- 20
kseq <- 1:5
n.cores = 20


dev <- sapply(kseq, function(k){
  print(k);
  # r <- replicate(1e4, sim.train.test(N = N, k = k));
  r <- mcreplicate(1e4, sim.train.test(N = N, k = k, mc.cores = n.cores));
  c(mean(r[1,]), mean(r[2,]), sd(r[1,]), sd(r[2,]))
})
  

# ----------
plot(kseq, dev[1,], ylim = c(min(dev[1:2,]) - 5, max(dev[1:2]) + 10), xlim = c(1, 5.1), xlab = "number of parameters", ylab = "deviance", pch = 16, col = rangi2)
mtext(concat("N = ", N))
points(kseq + 0.1, dev[2,])


for(i in kseq){
  pts_in <- dev[1, i] + c(-1, +1) * dev[3, i]
  pts_out <- dev[2, i] + c(-1, +1) * dev[4, i]
  lines(c(i, i), pts_in, col = rangi2)
  lines(c(i, i) + 0.1, pts_out)
}


