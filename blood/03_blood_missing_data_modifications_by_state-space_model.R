setwd("//media//kswada//MyFiles//R//blood")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  blood
# ------------------------------------------------------------------------------

data(blood, package = "astsa")

str(blood)

blood



# ------------------------------------------------------------------------------
# Missing data modifications by state-space model
#   - According to Jones, "Platelet count at about 100 days post transplant has previously been shown to be a good indicator of subsequent long term survival."
#     For this particular situation, we model the three variables in terms of the state equation.
# ------------------------------------------------------------------------------

y <- cbind(WBC, PLT, HCT)


num <- nrow(y)



# ----------
# make array of obs matrices

A <- array(0, dim = c(3, 3, num))

for(k in 1:num){ if(y[k,1] > 0) A[,,k] = diag(1,3) }

y

A



# ----------
# Initial values

mu0 <- matrix(0, 3, 1)

Sigma0 <- diag(c(0.1, 0.1, 1), 3)

Phi <- diag(1, 3)

cQ <- diag(c(0.1, 0.1, 1), 3)

cR <- diag(c(0.1, 0.1, 1), 3)



# ----------
# EM procedure

( em <- EM1(num, y, A, mu0, Sigma0, Phi, cQ, cR, 100, 0.001) )



# ----------
# estimators
em$Phi

em$Q

em$R




# ----------
ks <- Ksmooth1(num, y, A, em$mu0, em$Sigma0, em$Phi, 0, 0, chol(em$Q), chol(em$R), 0)

y1s <- ks$xs[1,,]

y2s <- ks$xs[2,,]

y3s <- ks$xs[3,,]



# ----------
p1 <- 2 * sqrt(ks$Ps[1,1,])

p2 <- 2 * sqrt(ks$Ps[2,2,])

p3 <- 2 * sqrt(ks$Ps[3,3,])



# ------------------------------------------------------------------------------
# Plot smoothed values and standard error bands
# ------------------------------------------------------------------------------

graphics.off()

par(mfrow = c(3, 1))

plot(WBC, type = "p", pch = 19, ylim = c(1,5), xlab = "day")
lines(y1s)
lines(y1s + p1, lty = 2, col = 4)
lines(y1s - p1, lty = 2, col = 4)


plot(PLT, type = "p", pch = 19, ylim = c(3,6), xlab = "day")
lines(y2s)
lines(y2s + p2, lty = 2, col = 4)
lines(y2s - p2, lty = 2, col = 4)


plot(HCT, type = "p", pch = 19, ylim = c(20,40), xlab = "day")
lines(y3s)
lines(y3s + p3, lty = 2, col = 4)
lines(y3s - p3, lty = 2, col = 4)





# ------------------------------------------------------------------------------
# Almost same codes from "Nonlinear Time Series"
# ------------------------------------------------------------------------------

data(WBC)

num = length(WBC)
A = array(ifelse(WBC>0, 1,0), dim=c(1,1,num))  # WBC = 0 when missing, otherwise, WBC > 0 

# Run EM
mu0 = 0; Sigma0 = .1; Phi = 1; cQ = .1; cR = .1            # initial parameter values
(em = EM1(num,WBC,A,mu0,Sigma0,Phi,0,0,cQ,cR,0,100,.001))

# Run and graph smoother
ks = Ksmooth1(num, WBC, A, em$mu0, em$Sigma0, em$Phi, 0, 0, sqrt(em$Q), sqrt(em$R), 0)
Xs = ks$xs  
Ps = 3*sqrt(ks$Ps)
plot(WBC, type="p", pch=20, ylim=c(1,5), xlab="day")
lines(Xs)
mss = 1 - as.vector(A)    # mark missing data
lines(mss, type="h", lwd=2)
xx=c(time(WBC), rev(time(WBC)))  # here and below puts conf intervals in gray
yy=c(Xs-Ps, rev(Xs+Ps))
polygon(xx, yy, border=NA, col=gray(.5, alpha = .3))

