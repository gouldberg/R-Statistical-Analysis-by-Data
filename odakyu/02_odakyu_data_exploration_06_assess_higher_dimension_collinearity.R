
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\odakyu")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  odakyu
# ------------------------------------------------------------------------------

dat <- read.csv("odakyu_dat.txt", header = TRUE, sep = "\t")


str(dat)


car::some(dat)



# ----------
dat$floor <- as.factor(dat$floor)

dat$ac <- as.factor(dat$ac)

dat$closet <- as.factor(dat$closet)

dat$flooring <- as.factor(dat$flooring)

dat$balcony <- as.factor(dat$balcony)



# ----------
# total minutes
dat$min_tot <- dat$bus_min + dat$walk_min + dat$train_min


# price including train fares for 20 days per month
dat$price_tot <- dat$price + dat$train_fare * 20


# price per area
dat$price_ave <- dat$price / dat$area
dat$price_tot_ave <- dat$price_tot / dat$area


# bus
dat <- dat %>% mutate(bus_flg = ifelse(bus_min > 0, 1, 0))
dat$bus_flg <- as.factor(dat$bus_flg)




# ------------------------------------------------------------------------------
# data exploration:  Assess Higher Dimension Collinearity by based on VIF (Variance Inflation Factor)
# ------------------------------------------------------------------------------


# FUNCTIONS

corvif <- function(dataz) {
  dataz <- as.data.frame(dataz)
  
  #vif part
  form    <- formula(paste("fooy ~ ",paste(strsplit(names(dataz)," "),collapse=" + ")))
  dataz   <- data.frame(fooy=1 + rnorm(nrow(dataz)) ,dataz)
  lm_mod  <- lm(form,dataz)
  
  cat("\n\nVariance inflation factors\n\n")
  print(myvif(lm_mod))
}


myvif <- function(mod) {
  v <- vcov(mod)
  assign <- attributes(model.matrix(mod))$assign
  if (names(coefficients(mod)[1]) == "(Intercept)") {
    v <- v[-1, -1]
    assign <- assign[-1]
  } else warning("No intercept: vifs may not be sensible.")
  terms <- labels(terms(mod))
  n.terms <- length(terms)
  if (n.terms < 2) stop("The model contains fewer than 2 terms")
  if (length(assign) > dim(v)[1] ) {
    diag(tmp_cor)<-0
    if (any(tmp_cor==1.0)){
      return("Sample size is too small, 100% collinearity is present")
    } else {
      return("Sample size is too small")
    }
  }
  R <- cov2cor(v)
  detR <- det(R)
  result <- matrix(0, n.terms, 3)
  rownames(result) <- terms
  colnames(result) <- c("GVIF", "Df", "GVIF^(1/2Df)")
  for (term in 1:n.terms) {
    subs <- which(assign == term)
    result[term, 1] <- det(as.matrix(R[subs, subs])) * det(as.matrix(R[-subs, -subs])) / detR
    result[term, 2] <- length(subs)
  }
  if (all(result[, 2] == 1)) {
    result <- data.frame(GVIF=result[, 1])
  } else {
    result[, 3] <- result[, 1]^(1/(2 * result[, 2]))
  }
  invisible(result)
}




# ----------
# including min_tot, but excluding other minutes data
obj_var <- c("shikirei", "area", "years",
             "num_exchange", "num_stations",
             "train_fare", "km", "min_tot",
             "floor", "ac", "closet", "flooring", "balcony", "bus_flg")



# corvif:  Each covariate is used in turn as a response variable with all other variables as covariates
# the R^2 is extracted and the VIF is calculated

corvif(dat[,obj_var])




# -->
# the variable with GVIF > 3.0:  train_fare and km



# ----------
# excluding km

obj_var <- c("shikirei", "area", "years",
             "num_exchange", "num_stations",
             "train_fare", "min_tot",
             "floor", "ac", "closet", "flooring", "balcony", "bus_flg")



corvif(dat[,obj_var])




# ----------
# excluding train_fare
obj_var <- c("shikirei", "area", "years",
             "num_exchange", "num_stations",
             "min_tot",
             "floor", "ac", "closet", "flooring", "balcony", "bus_flg")



corvif(dat[,obj_var])





# -------------------------------------------------------------------------------
# not inluding min_tot
obj_var <- c("bus_min", "walk_min", "shikirei", "area", "years",
             "train_min", "num_exchange", "num_stations",
             "train_fare", "km",
             "floor", "ac", "closet", "flooring", "balcony", "bus_flg")


corvif(dat[,obj_var])




# excluding km
obj_var <- c("bus_min", "walk_min", "shikirei", "area", "years",
             "train_min", "num_exchange", "num_stations",
             "train_fare",
             "floor", "ac", "closet", "flooring", "balcony", "bus_flg")


corvif(dat[,obj_var])



# excluding train_fare
obj_var <- c("bus_min", "walk_min", "shikirei", "area", "years",
             "train_min", "num_exchange", "num_stations",
             "floor", "ac", "closet", "flooring", "balcony", "bus_flg")


corvif(dat[,obj_var])



# excluding bus_flg
obj_var <- c("bus_min", "walk_min", "shikirei", "area", "years",
             "train_min", "num_exchange", "num_stations",
             "floor", "ac", "closet", "flooring", "balcony")


corvif(dat[,obj_var])

