rm(list = ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\05_anomaly_detection\\simdata")



# ------------------------------------------------------------------------------
# data:  generation
# ------------------------------------------------------------------------------


set.seed(1)


tt <- 0.1


x1 <- seq(0, 10, by = tt)


x2 <- seq(10.1, 20, by = tt)


x3 <- seq(20.2, 30, by = tt)


y1 <- sin(pi * x1) + rnorm(length(x1), sd = 0.07)


y2 <- sin(2 * pi * x2) + rnorm(length(x2), sd = 0.07)


y3 <- sin(pi * x3) + rnorm(length(x3), sd = 0.07)


xi <- c(y1, y2, y3)




# ----------
par(mfrow = c(1,1))

plot(xi, type = "l")




# ------------------------------------------------------------------------------
# HOT-SAX algorithm for time series discord discovery for V2
# ------------------------------------------------------------------------------


dat <- xi


library(jmotif)




# ----------
# window size is important !!!  --> now some large windows

w_size <- 30


# paa_size:  the PAA size (Piecewise Aggregate Approximation)
# a_size:  the alphabet size
# discords_num:  number of discords to report

discords <- find_discords_hotsax(dat, w_size = w_size, paa_size = 4, a_size = 4, n_threshold = 0.01, discords_num = 5)


discords




# ----------

( idx <- discords$position[which.max(discords$nn_distance)] )

idx <- 185



# ----------

graphics.off()

par(mfrow = c(1,1))

plot(dat, type = "l", col = "cornflowerblue")

lines(x = c(idx:(idx + w_size)), y = dat[idx:(idx + w_size)], col = "red", lwd = 2)





# ------------------------------------------------------------------------------
# Grammatical inference with RePair
#   - RePair is a dictionary-based compression method proposed in 1999 by Larsson and Moffat.
#     In contrast with Sequitur, Repair is an off-line algorithm that requires the whole input sequence
#     to be accessible before building a grammar.
#     Similar to Sequitur, RePair also can be utilized as a grammar-based compressor able to discover a compact grammar
#     that generates the text. It is a remarkably simple algorithm which is known for its very fast decompression.
#   - In short, RePair performs a recursive pairing step
#      -- finding the most frequent pair of symbols in the input sequence and replacing it with a new symbol
#      -- until every pair appears only once.
# ------------------------------------------------------------------------------


dat <- xi





# ----------
# discretize the data 

# discretization parameters
w <- 30

p <- 4

a <- 4


dat_sax <- sax_via_window(dat, w, p, a, "none", 0.01)


# time point = 1,2
dat_sax[[1]]
dat_sax[[2]]




# ----------
# get the string representation of time series
dat_str <- paste(dat_sax, collapse = " ")

dat_str



# infer the grammar
dat_grammar <- str_to_repair_grammar(dat_str)   


# 1st rule of the grammar (second list element)
str(dat_grammar[[2]])

dat_grammar[[2]]




# ----------
# initialize the density curve
density_curve <- rep(0, length(dat))


rule$rule_interval_starts



# account for all the rule intervals

for(i in 2:length(dat_grammar)){
  
  rule <- dat_grammar[[i]]
  
  for(j in 1:length(rule$rule_interval_starts)){
    
    xs <- rule$rule_interval_starts[j]
    
    xe <- rule$rule_interval_ends[j] + w
    
    density_curve[xs:xe] <- density_curve[xs:xe] + 1
  }
}




# ----------
# plot the density curve

graphics.off()

par(mfrow = c(2,1))

plot(dat, type = "l")
abline(v = c(100, 200), col = "blue", lty = 2)

plot(density_curve, type = "l")
abline(v = c(100, 200), col = "blue", lty = 2)


