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
# Check starting configurations and its stress values
# ------------------------------------------------------------------------------

# Classical MDS aka Torgerson Scaling
tstart <- torgerson(rectangles)



# ----------
# Compute the stress for 0 iterations based on a starting configuration provided
st0 <- stress0(rectangles, init = tstart)



# ----------
par(mfrow=c(1,1))
plot(tstart, main = "torgerson start", cex = 1.5)
lines(tstart[1:4,]);          lines(tstart[5:8,]); lines(tstart[9:12,]); 
lines(tstart[13:16,]);        lines(tstart[c(1,5,9,13),]); 
lines(tstart[c(2,6,10,14),]); lines(tstart[c(3,7,11,15),]); 
lines(tstart[c(4,8,12,16),])

st0


# -->
# Starting configuration based on Torgerson scaling has Stress 0.147



# ------------------------------------------------------------------------------
# Check original configuration used for experiment
# ------------------------------------------------------------------------------

rect_constr


st1 <- stress0(rectangles, init = rect_constr)


par(mfrow=c(1,1))
plot(rect_constr, main = "rect_constr", cex = 1.5)
lines(rect_constr[1:4,]);          lines(rect_constr[5:8,]); lines(rect_constr[9:12,]); 
lines(rect_constr[13:16,]);        lines(rect_constr[c(1,5,9,13),]); 
lines(rect_constr[c(2,6,10,14),]); lines(rect_constr[c(3,7,11,15),]); 
lines(rect_constr[c(4,8,12,16),])

st1
