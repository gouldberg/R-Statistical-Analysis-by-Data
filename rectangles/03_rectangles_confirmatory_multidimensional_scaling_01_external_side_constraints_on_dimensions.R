setwd("//media//kswada//MyFiles//R//rectangles")

packages <- c("dplyr", "smacof")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  rectangles
# ------------------------------------------------------------------------------

data(rectangles, package = "smacof")


str(rectangles)


car::some(rectangles)



# ------------------------------------------------------------------------------
# Confirmatory MDS
# ------------------------------------------------------------------------------

# MDS with theory-based initial configuration:  ordinal
fit.expl <- mds(rectangles, type = "ordinal", init = rect_constr) 


# ----------
# Confirmatory MDS enforcing the design grid:  ordinal
fit.cfdiag <- smacofConstraint(rectangles, constraint = "diagonal",
                               type = "ordinal", ties = "secondary",  
                               init = fit.expl$conf, external = rect_constr, 
                               constraint.type = "ordinal")  

# ----------
# Confirmatory MDS:  coordinates are a linear combination of the design grid:  ordinal
fit.cflin  <- smacofConstraint(rectangles, constraint = "linear", 
                               type = "ordinal", ties = "secondary", 
                               init = fit.expl$conf, external = rect_constr, 
                               constraint.type = "ordinal")  


# ----------
# Confirmatory MDS enforcing the design grid:  interval
fiti.cfdiag <- smacofConstraint(rectangles, constraint = "diagonal",
                               type = "ordinal", ties = "secondary",  
                               init = fit.expl$conf, external = rect_constr, 
                               constraint.type = "interval")  

# ----------
# Confirmatory MDS:  coordinates are a linear combination of the design grid:  interval
fiti.cflin  <- smacofConstraint(rectangles, constraint = "linear", 
                               type = "ordinal", ties = "secondary", 
                               init = fit.expl$conf, external = rect_constr, 
                               constraint.type = "interval")  

# ----------
fit.expl$stress

fit.cfdiag$stress

fit.cflin$stress

fiti.cfdiag$stress

fiti.cflin$stress


# ----------
graphics.off()
op <- par(mfrow = c(2,3))

conf <- fit.expl$conf

plot(fit.expl, main = "Expl. MDS", cex = 2)  
lines(conf[1:4,]);          lines(conf[5:8,]); lines(conf[9:12,]); 
lines(conf[13:16,]);        lines(conf[c(1,5,9,13),]); 
lines(conf[c(2,6,10,14),]); lines(conf[c(3,7,11,15),]); 
lines(conf[c(4,8,12,16),])

conf <- fit.cfdiag$conf

plot(fit.cfdiag, main = "Conf. MDS (C = diag) ordinal", cex = 2)
lines(conf[1:4,]);          lines(conf[5:8,]); lines(conf[9:12,]); 
lines(conf[13:16,]);        lines(conf[c(1,5,9,13),]); 
lines(conf[c(2,6,10,14),]); lines(conf[c(3,7,11,15),]); 
lines(conf[c(4,8,12,16),])

conf <- fiti.cfdiag$conf

plot(fiti.cfdiag, main = "Conf. MDS (C = diag) interval", cex = 2)
lines(conf[1:4,]);          lines(conf[5:8,]); lines(conf[9:12,]); 
lines(conf[13:16,]);        lines(conf[c(1,5,9,13),]); 
lines(conf[c(2,6,10,14),]); lines(conf[c(3,7,11,15),]); 
lines(conf[c(4,8,12,16),])

conf <- fit.cflin$conf

plot(fit.cflin, main = "Conf. MDS (C is not diag) ordinal", cex = 2)
lines(conf[1:4,]);          lines(conf[5:8,]); lines(conf[9:12,]); 
lines(conf[13:16,]);        lines(conf[c(1,5,9,13),]); 
lines(conf[c(2,6,10,14),]); lines(conf[c(3,7,11,15),]); 
lines(conf[c(4,8,12,16),])

conf <- fiti.cflin$conf

plot(fiti.cflin, main = "Conf. MDS (C is not diag) interval", cex = 2)
lines(conf[1:4,]);          lines(conf[5:8,]); lines(conf[9:12,]); 
lines(conf[13:16,]);        lines(conf[c(1,5,9,13),]); 
lines(conf[c(2,6,10,14),]); lines(conf[c(3,7,11,15),]); 
lines(conf[c(4,8,12,16),])

par(op)



# -->
# The exploratory MDS solution is already nearly theory-compatible except for some small dents of the grid. Its Stress is 0.089.
# The first confirmatory solution is theory-wise perfect, with a Stress of 0.115.
# Hence, the dents of the grid in the exploratory MDS solution do not explain the data "much" better.
# Rather it seems taht they essentially represent some of the data noise.

# If we drop the diagonality constraint, we get the sheared grid. Its Stress is 0.103, slightly better than without the rotation.
# It suggests that not the original dimensions were rescaled but a slightly rotated (but theoretically obscure) dimension system.
# This causes the shearing of the grid. In practive, such shearings can become extreme in this model which make the solution difficult to interpret.

# If we set constraint.type = "interval", the transformations on the design grid are limited to stretchings of the external scales, i.e.,
# to simple dimensional weightings (plus possible shearings)
# Under this condition, the successively smaller compressions of the grid along its dimensions cannot occur anymore in the MDS solution.
# This would be undesirable here, because the Weber-Fechner law of perception is predicting such logarithmic shrinkage effects.



