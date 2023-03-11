setwd("//media//kswada//MyFiles//R//spiders")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Spiders
# ------------------------------------------------------------------------------

Spiders <- read.table(file = "Spiders.txt", header = TRUE, dec = ".")


str(Spiders)



# ----------
# Some plots are dropped from the analysis
Spiders$fPlot <- factor(Spiders$Plot)

Spiders <- Spiders %>% filter(! fPlot %in% c("4", "9", "11", "14", "23"))

Spiders$fPlot <- as.factor(as.numeric(Spiders$fPlot))



# ------------------------------------------------------------------------------
# Standardizing covariate (to compare with MCMC result)
# ------------------------------------------------------------------------------

# Add na.rm = TRUE if need to deal with NAs
MyNorm <- function(x){ (x - mean(x)) / sd(x) }

Spiders$HerbLayerc <- MyNorm(Spiders$HerbLayer)
Spiders$GroundVegc <- MyNorm(Spiders$GroundVeg)
Spiders$Litterc    <- MyNorm(Spiders$Litter)



# ------------------------------------------------------------------------------
# Model fixed part:  refit final model by REML
# ------------------------------------------------------------------------------

M5 <- lmer(Hlog10 ~ HerbLayerc + GroundVegc + Litterc + (1 | fPlot), data = Spiders, REML = TRUE)


summary(M5)



# ------------------------------------------------------------------------------
# Calculate fitted values and standard errors
# ------------------------------------------------------------------------------
range(Spiders$HerbLayerc)

MyData <- data.frame(HerbLayerc = seq(-1.3, 2, length = 10), GroundVegc = 0, Litterc = 0)

X <- model.matrix(~HerbLayerc + GroundVegc + Litterc, data = MyData)



# ----------
# Extract estimated regression parameters
Betas     <- fixef(M5)


# Calculate predicted values
FitManual <- X %*% Betas


# Calculate thte standard erros of the predicted values
( SE        <- sqrt(diag(X %*% vcov(M5) %*% t(X))) )


# ----------
( Upper <- FitManual + 1.96* SE )
( Lower <- FitManual - 1.96* SE )


