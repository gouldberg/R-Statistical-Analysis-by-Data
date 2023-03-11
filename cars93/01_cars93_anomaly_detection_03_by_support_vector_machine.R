rm(list = ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\05_anomaly_detection\\01_異常検知\\cars93")



# ------------------------------------------------------------------------------
# data:  Cars93
# ------------------------------------------------------------------------------


library(MASS)


str(Cars93)


car::some(Cars93)





# ------------------------------------------------------------------------------
# Select variables and transform data
# ------------------------------------------------------------------------------


# select 15 variables

cc <- c("Min.Price", "Price", "Max.Price", "MPG.city", "MPG.highway", "EngineSize", "Horsepower", "RPM", "Rev.per.mile",
        "Fuel.tank.capacity", "Length", "Wheelbase", "Width", "Turn.circle", "Weight")


Xc <- Cars93[,cc]


rownames(Xc) <- Cars93[,"Make"]


head(Xc)



# ----------
# column is Make and row is variable

summary(Xc)




# ------------------------------------------------------------------------------
# Anomaly Detection:  basd on nonlinear model  (Support Vector Machine)
#   - Nonlinear transformation the data by RBF Kernel
# ------------------------------------------------------------------------------

X <- scale(Xc)



# ----------
library(kernlab)


sigma <- 0.01

rbf <- rbfdot(sigma = sigma)


# approximately parameters for proportions for anomaly detected data
nu <- 0.05

ocsvm <- ksvm(X, type = "one-svc", kernel = rbf, nu = nu)


# support vectors (alpha vector)
ocsvm@alpha




# ----------
# anomaly detected data  (corresponding to support vector)

ocsvm@alphaindex


fitted(ocsvm)


X[ocsvm@alphaindex,]




# ----------
colorcode <- rep(0, nrow(X))

colorcode[ocsvm@alphaindex] <- 1


graphics.off()

par(mfrow = c(2,2))

# for(i in 2:ncol(X)){
for(i in 2:5){
    
  plot(X[,i] ~ X[,i-1], pch = 21, bg = colorcode, main = paste0(colnames(X)[i], " : ", colnames(X)[i-1]))
  
  text(X[colorcode == 1, c(i-1, i)] + 0.1, labels = rownames(X)[colorcode == 1])
}




