#
#  Chapter 3.3, Labby Dice example
#
dat <- c(53222, 52118, 52465, 52338, 52244, 53285)  #  these are the observed values - see table in slides
# what we did before was compute E = 52612, then
# \sum (O-E)^2/E  - this is the equation for the chi square
# (chisq-test = 24.73)  -  this is the value we got in lecture, see slide

#  this is the actual chi squared info we need

goodness_res <- vector(length = 10000)
expected <- sum(dat) / 6  # because its 6 equal catagories - 6 sides of the dice

#  this is a chi squaure, for every element in the empty 10000 vector, compute the observed
#  if the true proportin was 1/6 then - expected object, squared divided by expected. 
for(j in 1:10000) {
  goodness_res[j] <- sum( (rmultinom(n = 1, size = sum(dat), prob = rep(1/6, 6)) - expected)^2 / expected )
}  # a multinomial is just a collection of n values with a probability that equals 1. 
   # binomial is just two values that equal 1 prob. 

p_val <- length(which(goodness_res >= 24.73)) / 10000
p_val  #  significant 0.0002

# options(scipen = 9999)




#  A different way to do it besides using multinomial: 

#  sample(1:6, 317000, replace = TRUE)
#
#  Benford's Law: P(d) = log_10 (1 + 1/d)
#
# benford <- log10(1 + 1/(1:9))
# rmultinom(n = 1, size = 10, prob = benford)

#



#  A different example - this requires a different layout
#  Chapter 3.4, 
#
chisq_test <- function(observed) {
  stopifnot(class(observed) == "matrix") 
  nrow <- dim(observed)[1]
  ncol <- dim(observed)[2]
  if(nrow < 2 | ncol < 2) { stop("This is not a matrix of at least 2x2 size.")}
 
  row_sums <- apply(observed, MAR = 1, sum)
  col_sums <- apply(observed, MAR = 2, sum)
  grand_total <- sum(row_sums)
  
  augment <- matrix(NA, nrow = nrow + 1, ncol = ncol + 1)
  augment[1:nrow, 1:ncol] <- observed
  augment[, ncol+1] <- c(row_sums, grand_total)
  augment[nrow+1, ] <- c(col_sums, grand_total)

  expected <- observed
  for(i in 1:ncol) {
    for(j in 1:nrow) {
      expected[j, i] <- augment[j, ncol + 1] * augment[nrow + 1, i] / augment[nrow + 1, ncol + 1]
    }
  }
  chisq_test <- sum((observed - expected)^2 / expected)
  df <- (nrow - 1) * (ncol - 1)
  
  return_vec <- c(chisq_test, df)
  return(return_vec)
}

dat2 <- c(63, 31, 25, 88, 55, 33, 96, 55, 32)
dat2_mat <- matrix(data = dat2, nrow = 3, ncol = 3, byrow = TRUE)

var1_totals <- apply(dat2_mat, MARGIN = 1, FUN = sum)
var2_totals <- apply(dat2_mat, MARGIN = 2, FUN = sum)

sim_vector <- c( rep(4, var1_totals[1]),
                 rep(5, var1_totals[2]),
                 rep(6, var1_totals[3]))

chisq_res <- vector(length = 10000)
for(i in 1:10000) { 
  randomize <- sample(sim_vector, length(sim_vector), replace = FALSE)
  grades <- randomize[1:var2_totals[1]]
  popular <- randomize[(var2_totals[1]+1):(var2_totals[1]+var2_totals[2])]
  sports <- randomize[(var2_totals[1]+var2_totals[2]+1):(sum(var2_totals))]
  
  result <- matrix(data = 0, nrow = 3, ncol = 3)
  for(j in 1:3) {
    result[j, 1] <- length(which(grades == j+3))
    result[j, 2] <- length(which(popular == j+3))
    result[j, 3] <- length(which(sports == j+3))
  }
  chisq_res[i] <- chisq_test(result)[1]
} 

# original result: 1.3121
length(which(chisq_res >= 1.3121)) / 10000
