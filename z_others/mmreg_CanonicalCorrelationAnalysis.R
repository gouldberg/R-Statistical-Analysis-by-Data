# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
rm(list=ls())
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "psych", "CCA")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# data:  mmreg
#  - data on 3 psychological variables, 4 academic variables (standardized test scores) and gender for 600 college freshman
#  - female is a zero-one indicator variable with the one indicating a female student
#
# Researcher is interested in how the set of psychological variables relates to the academic variables and gender.
# Also, interested in how many dimensions (canonical variables) are necessary to understand the association between the 2 sets of variables.
# Multivariate multiple regression is a reasonable option if you have no interest in dimensionality
# ------------------------------------------------------------------------------

mm <- read.csv("./RefData/Others/mmreg.csv")

colnames(mm) <- c("Control", "Concept", "Motivation", "Read", "Write", "Math", "Science", "Sex")

summary(mm)


# ------------------------------------------------------------------------------
# Correlation within and between psych and acad
# ------------------------------------------------------------------------------
psych <- mm[,1:3]
acad <- mm[,4:8]

pairs.panels(psych)
pairs.panels(acad)
pairs.panels(cbind(psych, acad))


# Correlation matrices within and between psych and acad
( cormat <- matcor(psych, acad) )


# ------------------------------------------------------------------------------
# Canonical correlation and loadings
# ------------------------------------------------------------------------------

( cc1 <- cc(psych, acad) )


# canonical correlations
cc1$cor


# raw canonical coefficients
cc1[3:4]


# Compute canonical loadings
( cc2 <- comput(psych, acad, cc1) )


# canonical loadings:  correlations between variables and the canonical variates
cc2[3:6]


# ------------------------------------------------------------------------------
# tests of canonical dimensions
# ------------------------------------------------------------------------------
ev <- (1 - cc1$cor^2)

n <- dim(psych)[1]
p <- length(psych)
q <- length(acad)
k <- min(p, q)
m <- n - 3/2 - (p + q)/2

w <- rev(cumprod(rev(ev)))

d1 <- d2 <- f <- vector("numeric", k)

for (i in 1:k) {
  s <- sqrt((p^2 * q^2 - 4)/(p^2 + q^2 - 5))
  si <- 1/s
  d1[i] <- p * q
  d2[i] <- m * s - p * q/2 + 1
  r <- (1 - w[i]^si)/w[i]^si
  f[i] <- r * d2[i]/d1[i]
  p <- p - 1
  q <- q - 1
}

# The 1st test:  whether all 3 dimensions are significant ?  --> significant
# The 2nd test:  whether dimensions 2 and 3 combined are significant ?  --> significant
# The 3rd test:  whether dimensions 3, by itself, is significant ?  --> NOT significant
pv <- pf(f, d1, d2, lower.tail = FALSE)
(dmat <- cbind(WilksL = w, F = f, df1 = d1, df2 = d2, p = pv))



# ------------------------------------------------------------------------------
# standardized canonical coeffs
#  - When the variables in the model have very different standard deviations, the standardized coeffs allow for easier comparisons among variables
# ------------------------------------------------------------------------------
# standardized psych canonical coefficients diagonal matrix of psych sd's
s1 <- diag(sqrt(diag(cov(psych))))

cc1[3]
s1 %*% cc1$xcoef


# standardized acad canonical coefficients diagonal matrix of acad sd's
s2 <- diag(sqrt(diag(cov(acad))))

cc1[4]
s2 %*% cc1$ycoef



# ------------------------------------------------------------------------------
# Calculation by matrix operation
# ------------------------------------------------------------------------------

( cormat <- cor(cbind(psych, acad)) )


# Correlation matrix of response variables
( r11 <- cormat[1:3, 1:3] )

# Correlation matrix of explanatory variables
( r22 <- cormat[-(1:3), -(1:3)] )

( r12 <- cormat[1:3, -(1:3)] )
( r21 <- cormat[-(1:3), 1:3] )


( E1 <- solve(r11) %*% r12 %*% solve(r22) %*% r21 )

( E2 <- solve(r22) %*% r21 %*% solve(r11) %*% r12 )


( e1 <- eigen(E1) )
( e2 <- eigen(E2) )



cc1$cor[1]
sqrt(e1$values[1])

cc1$cor[2]
sqrt(e1$values[2])

cc1$cor[3]
sqrt(e1$values[3])

e1$vectors
s1 %*% cc1$xcoef

e2$vectors
s2 %*% cc1$ycoef
