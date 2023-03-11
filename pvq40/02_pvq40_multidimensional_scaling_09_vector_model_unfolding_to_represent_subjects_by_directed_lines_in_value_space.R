setwd("//media//kswada//MyFiles//R//pvq40")

packages <- c("dplyr", "smacof")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  PVQ40
# ------------------------------------------------------------------------------

data(PVQ40, package = "smacof")
# data(PVQ40agg, package = "smacof")


str(PVQ40)

attributes(PVQ40)


car::some(PVQ40)
# car::some(PVQ40agg)



# ------------------------------------------------------------------------------
# MDS on scores averaged by category
# ------------------------------------------------------------------------------

var_n <- c("SE", "CO", "TR", "BE", "UN", "SD", "ST", "HE", "AC", "PO")

var_n_select <- c("se1, se2", "co1, co2", "tr1, tr2", "be1, be2", "un1, un2, un3", "sd1, sd2", "st1, st2", "he1, he2", "ac1, ac2", "po1, po2")


# Take row means for each category of measure
for(i in 1:length(var_n)){
  eval(parse(text = paste0(var_n[i], " <- rowMeans(subset(PVQ40, select = c(", var_n_select[i], ")), na.rm = TRUE)")))
}

raw <- cbind(SE, CO, TR, BE, UN, SE, ST, HE, AC, PO)



# ----------
R <- cor(raw)

diss <- sim2diss(R)

res <- mds(diss, init = "torgerson", type = "ordinal")



# ----------
graphics.off()
par(mfrow=c(1,1))
plot(res)
polygon(res$conf, lty = 2)

erg    <- fitCircle(res$conf[,1], res$conf[,2])
draw.circle(erg$cx, erg$cy, radius = erg$radius, border = "blue", lty = 2)



# ------------------------------------------------------------------------------
# Unfolding:  vector model
#   - Represent the persons by directed lines running through the origin, not by ideal points.
#     Each such line is oriented in space such that the projections of the points representing the choice objects onto this line
#     correspond optimally to the observed preference scores of a person.
#   - Expressed differently, each person's preference scores are explained by a weighted sum of the dimensions of the objects.
#   - For example, person p may weight Dimension 1 by 60% and Dimension 2 by 40% and give both a positive sign (more = better).
#     This defines the orientation of p's preference vector:  It runs into the 2:00 o'clock direction.
#
#   - But the vector model cannot represent cases where a person's preference strengths keep going up until a certain point is reached and then
#     drop monotonically as one continues moving further on the person's vector.
#     If, however, all ideal points move far outside the configuration of the choice objects, the vector model approximates the ideal-point model
#     as a special case.
# ------------------------------------------------------------------------------

vmu <- function (P, ndim = 2, center = TRUE, scale = FALSE){

  # P is an N (persons) by m (objects) matrix of similarities (preferences)
  m <- dim(P)[2]
  S <- svd(t(scale(t(P), center = center, scale = scale)))
  X <- m^( 1/2) * S$v                 ## X = objects
  Y <- m^(-1/2) * S$u %*% diag(S$d)   ## Y = persons
  row.names(X) <- colnames(P)
  row.names(Y) <- rownames(P)
  return(list(X = X[, 1:ndim], Y = Y[, 1:ndim], VAF = sum(S$d[1:ndim]^2)/sum(S$d^2), d = S$d)) 
} 


res <- vmu(raw)


res


# -->
# overall, the solution accounts for 65% of the variance.



# ----------
par(mfrow=c(1,1))
plot(1.2 * res$X[, 1], 1.2 * res$X[, 2], type = "n", asp = 1, xlab = "", ylab = "")
abline(h = 0, v = 0, col = "gray60", lty = 2)
arrows(rep(0, nrow(res$Y)), rep(0, nrow(res$Y)), res$Y[, 1], res$Y[, 2], col = "red", length = 0.1)
text(res$X[, 1], res$X[, 2], rownames(res$X), cex = 1.5)


# person 133
abline(0, res$Y[133,2]/res$Y[133,1], col = "gray60")
text(res$Y[133,1], res$Y[133,2], labels="133")



# -->
# Arrow shows direction and strength of person's strivings in value space.
# Arrow representing person #133 is marked by a solid line running from the upper left-hand side of the plot to the lower right-hand side.
# It explains this person's data quite well, except for some discrepancies on HE and AC.

# The unfolding solution shows tha most persons in this sample lean toward self-enhancement, not toward self-transcendence.
# The object points from a roughly circular configuraion, similar to the ideal-point solution.



# ----------
# person 133
( v133 <- round(raw[133,], 2) )

# centered value for person 133
( v133c <- round(raw[133,] - mean(raw[133,]), 2) )

# VMU reconstructed values for person 133
( v133r <- round(t(res$X %*% res$Y[133,]), 2) )

# Projected values for person 133
( v133p <- round(t(res$X %*% res$Y[133,] / (sum(res$Y[133,]^2)^.5)), 2) )



# The centered ratings of person #133 correlate with the his / her reconstructed scores with r = 0.95
cor(t(v133r), v133)



