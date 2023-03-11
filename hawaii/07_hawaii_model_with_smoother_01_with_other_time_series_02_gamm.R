# setwd("//media//kswada//MyFiles//R//hawaii")
setwd("//media//kswada//MyFiles//R//Generalized_linear_model_and_heterogeneity//hawaii")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Hawaii
# ------------------------------------------------------------------------------

Hawaii <- read.table(file = "Hawaii.txt", header = TRUE)


str(Hawaii)


dim(Hawaii)


car::some(Hawaii)


# ----------
Hawaii$Birds <- sqrt(Hawaii$Moorhen.Kauai)




# ------------------------------------------------------------------------------
# data exploration:  time series plot
# ------------------------------------------------------------------------------

# Stilt.Maui and Coot.Maui

Birds <- c(Hawaii$Stilt.Maui, Hawaii$Coot.Maui)


Time <- rep(Hawaii$Year, 2)


Rain <- rep(Hawaii$Rainfall, 2)


ID <- factor(rep(c("Stilt.Maui", "Coot.Maui"), each = length(Hawaii$Year)))



# ----------
xyplot(Birds ~ Time | ID, col = 1, type = "o")



# ----------
Birds_sm <- Hawaii$Stilt.Maui

Birds_cm <- Hawaii$Coot.Maui

Birds_so <- Hawaii$Stilt.Oahu

Birds_co <- Hawaii$Coot.Oahu



# ------------------------------------------------------------------------------
# Input = Stilt.Maui  Ouput = Coot.Maui  by gamm
# ------------------------------------------------------------------------------

library(mgcv)


rain <- Hawaii$Rainfall

year <- Hawaii$Year



( u <- gamm(Birds_cm ~ s(as.numeric(year)) + s(Birds_sm) + s(rain), correlation = corAR1(form = ~ year)) )

( u <- gamm(Birds_cm ~ ti(as.numeric(year), rain) + s(Birds_sm), correlation = corAR1(form = ~ year)) )

( u <- gamm(Birds_cm ~ s(as.numeric(year)) + s(Birds_sm, rain), correlation = corAR1(form = ~ year)) )

# ( u <- gamm(Birds_cm ~ ti(as.numeric(year), rain) + s(Birds_sm), correlation = corGaus(form = ~ year, nugget = TRUE)) )

# ( u <- gamm(Birds_cm ~ ti(as.numeric(year), rain) + s(Birds_sm)) )


# ----------
summary(u$gam)


anova(u$gam)



# ----------
summary(u$lme)



# -->
# phi1 = 0.45 or 0.70



# ------------------------------------------------------------------------------
# plot smoothers
# ------------------------------------------------------------------------------

graphics.off()

par(mfrow = c(2,2))

plot(u$gam, scale = FALSE, shade = TRUE)




# ------------------------------------------------------------------------------
# Assess residual pattern:  Normalised residuals
# ------------------------------------------------------------------------------

E22 <- residuals(u$lme, type = "normalized")


EAll2 <- vector(length = length(Birds_cm))

EAll2[] <- NA

I12 <- !is.na(Birds_cm)

EAll2[I12] <- E22


graphics.off()
par(mfrow = c(1,1))

plot(EAll2 ~ year, col = 1, ylab = "Residuals", type = "o", main = "residuals of Coot.Maui predicted by Stilt.Maui", ylim = c(-3, 3))
abline(h = 0, lty = 2, col = "gray")



# ----------
# for comparison 

BM2 <- gamm(Birds ~ Rain + ID +
              s(as.numeric(Time), by = ID), weights = varIdent(form = ~1 | ID), correlation = corAR1(form = ~ Time | ID))


E2 <- residuals(BM2$lme, type = "normalized")
EAll <- vector(length = length(Birds))
EAll[] <- NA
I1 <- !is.na(Birds)
EAll[I1] <- E2

idx <- which(ID == "Coot.Maui")

lines(EAll[idx] ~ Time[idx], col = "blue", ylab = "Residuals", type = "o", main = "residual of Coot.Maui by All model", ylim = c(-3, 3))

