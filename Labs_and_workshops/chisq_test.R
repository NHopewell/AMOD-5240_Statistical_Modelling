#
#  This computes chisq test statistic and df
#  for a 2x2 or larger contingency table (test of 
#  independence)chisq_test <- function(observed) {
  stopifnot(class(observed) == "matrix") 
  nrow <- dim(observed)[1]
  ncol <- dim(observed)[2]
  if(nrow < 2 | ncol < 2) { stop("This is not a matrix of at least 2x2 size.")}
 
  row_sums <- apply(observed, MAR = 1, sum)
  col_sums <- apply(observed, MAR = 2, sum)
  grand_total <- sum(row_sums)
  
  augmented_matrix <- matrix(NA, nrow = nrow + 1, ncol = ncol + 1)
  augmented_matrix[1:nrow, 1:ncol] <- observed
  augmented_matrix[, ncol+1] <- c(row_sums, grand_total)
  augmented_matrix[nrow+1, ] <- c(col_sums, grand_total)

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
}.
#
chisq_test <- function(observed) {
  stopifnot(class(observed) == "matrix") 
  nrow <- dim(observed)[1]
  ncol <- dim(observed)[2]
  if(nrow < 2 | ncol < 2) { stop("This is not a matrix of at least 2x2 size.")}
 
  row_sums <- apply(observed, MAR = 1, sum)
  col_sums <- apply(observed, MAR = 2, sum)
  grand_total <- sum(row_sums)
  
  augmented_matrix <- matrix(NA, nrow = nrow + 1, ncol = ncol + 1)
  augmented_matrix[1:nrow, 1:ncol] <- observed
  augmented_matrix[, ncol+1] <- c(row_sums, grand_total)
  augmented_matrix[nrow+1, ] <- c(col_sums, grand_total)

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

