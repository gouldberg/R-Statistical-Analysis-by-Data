setwd("//media//kswada//MyFiles//R//parasites3")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  parasites3
# ------------------------------------------------------------------------------

Par <- read.table(file = "Parasites3.txt", header = TRUE)


str(Par)

dim(Par)



# ----------
Par$Worms <- Par$Elytrophalloides_oatesi



# ------------------------------------------------------------------------------
# Model 5,6,7:  Include "Area" and no smoothers (GLM model)
# ------------------------------------------------------------------------------

# No interaction smoothing
M5 <- mgcv::gam(Worms ~ s(Length) + Sex + Area, 
          data = Par,
          family = binomial)


# gam model with no smoother
M6 <- mgcv::gam(Worms ~ Length * Sex + Area, 
          data = Par,
          family = binomial)


# glm model
M7 <- glm(Worms ~ Length * Sex + Area, 
          data = Par,
          family = binomial)



# -->
# Optimal model is not GAM model but GLM model !!!
AIC(M1, M2, M3, M4, M5, M6, M7)



# ----------
# Since the optimal model is a GLM with interaction, we will look at the numerical output of M7
# and notice that 2-way interaction is weakly significant.
drop1(M7, test= "Chi")



# ----------
MD <- expand.grid(Length = seq(29.5, 57, length = 100),
                  Sex    = levels(Par$Sex),
                  Area   = levels(Par$Area))

P7 <- predict(M7, newdata = MD, se = TRUE, type = "link")

MD$fit <- P7$fit

MD$se.fit <- P7$se.fit


xyplot(Worms ~ Length | Area * Sex,
       data = Par,
       xlab = list(label = "Length", cex = 1.5),
       ylab = list(ylab.name, cex = 1.5),
       panel = function(x,y,subscripts,...){
         panel.points(x, y, cex = 1, pch = 16, col = 1)
         Sexi   <- as.character(Par[subscripts,"Sex"][1])
         Areai  <- as.character(Par[subscripts,"Area"][1])
         MDi    <- MD[MD$Sex==Sexi & MD$Area == Areai,]
         Fit    <- exp(MDi$fit) / (1+exp(MDi$fit))
         SE.UP  <- exp(MDi$fit + 2 * MDi$se.fit) / (1+exp(MDi$fit + 2 * MDi$se.fit))
         SE.Low <- exp(MDi$fit - 2 * MDi$se.fit) / (1+exp(MDi$fit - 2 * MDi$se.fit))
         panel.polygon(c(MDi$Length, rev(MDi$Length)),
                       c(SE.Low, rev(SE.UP)),
                       col = grey(0.5),border=NULL,
                       density = 50   )
         panel.lines(MDi$Length, Fit, lwd = 3, col = 1)
       })



# plot(M7)