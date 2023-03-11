# setwd("//media//kswada//MyFiles//R//melanoma")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\melanoma")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  melanoma
# ------------------------------------------------------------------------------

melanoma <- t(matrix(scan("melanoma.txt", 0), 3, 37)) %>% as.data.frame() %>% dplyr::select(V2, V3)

# data("melanoma", package = "fda")


colnames(melanoma) <- c("year", "incidence")



str(melanoma)




# ------------------------------------------------------------------------------
# Smoothing after removing linear trend:  Choose best # of fourier basis and roughness parameter
# ------------------------------------------------------------------------------

res <- lsfit(x = year, y = mela)$residual


# Convert integer to linear differential operator
# m = 1:  penalize the square of the slope or valocity
# m = 2:  penalize the squared acceleration
# m = 3:  penalize the squared rate of change of acceleration
# m = 4:  penalize the squared curvature of acceleration
Lfdobj = int2Lfd(m = 4)



t36.72 = seq(1936, 1972, by = 1)
yearRng = c(1936, 1972)



# ----------
# # of basis
( basis_vec <- seq(5, 15, 2) )


# ----------
# lambda
# df: degrees of freedom   gcv:  GCV values
# step through values of log(lambda)
loglam        = seq(-4, 2, 0.25)
nlam          = length(loglam)


# ----------
dfsave        = matrix(NA, nrow = length(basis_vec), ncol = nlam)
rownames(dfsave) = basis_vec
colnames(dfsave) = loglam
gcvsave       = dfsave


# compute GCV value by # of basis and lambda
for(i in 1:length(basis_vec)){
  
  yearbasis = create.fourier.basis(yearRng, basis_vec[i])
  
  for (ilam in 1:nlam) {
    cat(paste('# of basis = ', basis_vec[i], '-- log10 lambda =', loglam[ilam],'\n'))
    lambda        = 10 ^ loglam[ilam]
    Sm = smooth.basisPar(argvals = t36.72,
                         y = res, 
                         fdobj = yearbasis,
                         Lfdobj = Lfdobj, lambda = lambda)
    dfsave[i, ilam]  = Sm$df
    gcvsave[i, ilam] = sum(Sm$gcv)
  }
}



# ----------
gcvsave


matplot(t(gcvsave), type = "l")

# # of basis = 9 and lambda = 10 ^ 0.25 is smallest gcv
# only slightly different from the analysis before removing linear trend
gcvsave[which.min(gcvsave)]




# ------------------------------------------------------------------------------
# Refit by best parameter
# ------------------------------------------------------------------------------

nbasis = 9

lambda = 10 ^ 0.25

yearbasis = create.fourier.basis(yearRng, nbasis)


# ----------
Sm = smooth.basisPar(argvals = t36.72,
                     y = res, 
                     fdobj = yearbasis,
                     Lfdobj = Lfdobj, lambda = lambda)



# ----------
t36.72_l = seq(1936, 1972, length = 600)

plot(t36.72, lsfit(x = year, y = mela)$residual, xlab='Year', ylab = '# of melanoma cases per 100,000', las=1)

lines(t36.72_l, predict(Sm, t36.72_l))

