setwd("//media//kswada//MyFiles//R//magazine_prices")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Magazine Prices
# ------------------------------------------------------------------------------

data("MagazinePrices", package = "pder")


str(MagazinePrices)


dim(MagazinePrices)


car::some(MagazinePrices)



# ----------
# we here remove duplicated rows (but "length" is changed from 6 to 7 within year) for Science Digest year 45
# also included == 1:  year 53 to 79 for palane balancing
MagazinePrices2 <- MagazinePrices %>% filter(! (year == 45 & magazine == "Science Digest" & length == 7)) %>% filter(included == 1)


# ----------
mp <- pdata.frame(MagazinePrices2, index = c("magazine", "year"), drop.index = FALSE)


head(mp)



# ------------------------------------------------------------------------------
# Plot Between / OLS / Random / Within models
# ------------------------------------------------------------------------------

baw <- FALSE

library(ggplot2)

plotplm <- function(x, N = 10, seed = 1, lgth = 0.1){
  mydata <- model.frame(x)
  onames <- names(mydata)
  names(mydata) <- c("y", "x")
  LGTH <- (max(mydata$x) - min(mydata$x)) ^ 2 +
    (max(mydata$y) - min(mydata$y)) ^ 2
  lgth <- lgth * sqrt(LGTH) / 2
  seed <- set.seed(seed)
  theids <- sample(unique(index(mydata)[[1]]), N)
  small <- subset(mydata, index(mydata)[[1]] %in% theids)
  small <- cbind(small, id = index(small)[[1]])
  ymean <- with(small, tapply(y, id, mean)[as.character(theids)])
  xmean <- with(small, tapply(x, id, mean)[as.character(theids)])
  within <- update(x, model = "within")
  alpha <- mean(mydata[[1]]) - coef(within) * mean(mydata[[2]])
  beta <- as.numeric(coef(within))
  random <- update(within, model = "random")
  between <- update(within, model = "between")
  ols <- update(within, model = "pooling")
  FE <- fixef(within)[as.character(theids)]
  DATA <- data.frame(id = names(FE), FE = as.numeric(FE), slope = beta,
                     xmean = xmean, ymean = ymean,
                     xmin = xmean - lgth / sqrt(1 + beta ^ 2),
                     xmax = xmean + lgth / sqrt(1 + beta ^ 2),
                     ymin = ymean - lgth * beta / sqrt(1 + beta ^ 2),
                     ymax = ymean + lgth * beta / sqrt(1 + beta ^ 2))
  MODELS <- data.frame(models = c("ols", "random", "within", "between"),
                       intercept = c(coef(ols)[1], coef(random)[1], alpha, coef(between)[1]),
                       slope = c(coef(ols)[2], coef(random)[2], coef(within), coef(between)[2]))
  if (! baw){
    ggplot(data = small, aes(x = x, y = y, color = id)) + geom_point(size = 0.4) +
      geom_segment(aes(x = xmin, xend = xmax, y = ymin, yend = ymax, color = id), data = DATA) +
      geom_abline(aes(intercept = intercept, slope = slope, lty = models), data = MODELS) +
      geom_point(aes(x = xmean, y = ymean, color = id), size = 1, shape = 13, data = DATA) +
      xlab(onames[2]) + ylab(onames[1]) +
      theme(legend.text = element_text(size = 6),
            legend.title= element_text(size = 8),
            axis.title = element_text(size = 8))
  } else {
    ggplot(data = small, aes(x = x, y = y)) + geom_point(size = 0.4, aes(shape = id)) +
      geom_segment(aes(x = xmin, xend = xmax, y = ymin, yend = ymax), data = DATA) +
      geom_abline(aes(intercept = intercept, slope = slope, lty = models), data = MODELS) +
      geom_point(aes(x = xmean, y = ymean, shape = id), size = 1,  data = DATA) +
      scale_shape_manual(values=1:N) +
      xlab(onames[2]) + ylab(onames[1]) +
      theme(legend.text = element_text(size = 6),
            legend.title= element_text(size = 8),
            axis.title = element_text(size = 8))
  }
}




# ----------
plotplm(plm(change ~ length, data = mp), N = 38, seed = 4, lgth = .05)



# -->
# Within estimator:  the longer the length, the larger the change ratio
# Between estimator:  the longer the length, the smaller the change ratio




plotplm(plm(change ~ cumsales, data = mp), N = 10, seed = 4, lgth = .05)


plotplm(plm(price ~ length, data = mp), N = 38, seed = 4, lgth = .05)


