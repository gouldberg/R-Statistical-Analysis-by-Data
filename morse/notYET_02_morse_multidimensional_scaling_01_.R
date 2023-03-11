setwd("//media//kswada//MyFiles//R//morse")

packages <- c("dplyr", "smacof")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  morse
# ------------------------------------------------------------------------------

# morse2 = 1 - confusion probabilities
data(morse, package = "smacof")
data(morse2, package = "smacof")


str(morse)
str(morse2)


morse
morse2



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------





res <- mds(delta = morse, type = "ordinal")

res2 <- mds(delta = morse2, type = "ordinal")


graphics.off()
par(mfrow=c(1,2))

plot(res, main = "morse")
plot(res2, main = "morse2")



fit.drift <- driftVectors(morse2, type = "ordinal")

fit.drift

plot(fit.drift)






P <- matrix( c(0,  4,  6, 13, 
               5,  0, 37, 21, 
               4, 38,  0, 16, 
               8, 31, 18,  0), nrow=4, ncol=4, byrow=TRUE) 
S <- (P + t(P))/2  # Symmetric part
A <- (P - t(P))/2  # Skewsymmetric part
n <- dim(P)[1]

diss <- sim2diss(S, method=40) 

res1 <- mds(diss, type="interval")

plot(res1, cex=2.5, main="", ylim=c(-.6,1.1), xlim=c(-.4, 1.1))

X <- res1$conf

for (i in 1:n) { 
  sx <- 0
  sy <- 0
  x  <- X[i,1]
  y  <- X[i,2]
  for (j in 1:n) { 
    c <- A[i,j]/mean(P)
    a <- X[j,1]
    b <- X[j,2]
    slope <- (b - y)/(a - x)
    if (c != 0) { 
      angle <- atan(slope); 
      length1 <- abs(c)
      x1 <- x + length1 * cos(angle)
      y1 <- y + length1 * sin(angle) 
      arrows(x, y, x1, y1, length=0.10, col="blue", lwd=2, lty=1) 
    }  ## arrows
    if (i != j) { 
      sx <- sx + x1 - x 
      sy <- sy + y1 - y 
    } 
  } 
  arrows(x, y, x+sx, y+sy, length=0.20, col="red", lwd=4, lty=1.5) 
}  ## drift vector
