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
# Random starting configuration
# ------------------------------------------------------------------------------

random.multistart <- function(diss, type = "ordinal", nrep = 100){
  
  s1 <- 1
  
  for(i in 1:nrep){
    
    out <- mds(diss, type = type, init = "random")
    
    if(out$stress < s1){
      object <- out
      s1 <- out$stress
    }
  }
  
  return(object)
}


set.seed(123)



# random starting and select minimum stress configurations
result <- random.multistart(diss = rectangles, type = "ordinal", nrep = 500)

result



# ----------
# only once, but not init = "torgenson" (as default) but random
res <- mds(rectangles, type = "ordinal", init = "random")



# ----------
result$stress

res$stress


# -->
# but almost same



# ----------
graphics.off()

par(mfrow = c(1,2))

plot(result)
plot(res)


# -->
# random starting configuration seems to be worse, but lower stress



# ----------
graphics.off()

par(mfrow = c(1,2))

plot(result, plot.type = "Shepard")
plot(res, plot.type = "Shepard")

