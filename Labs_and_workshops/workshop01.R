#
#  this is a comment
#
x <- sample(1:20, 1, replace=FALSE)
y <- sample(c("a", "b", "c", "d", "e", "f", "g"), 1)
coin_flips <- sample(c("H", "T"), 20, replace = TRUE)
coin_flips2 <- sample(c(0, 1), 20, replace = TRUE)   # let the 1s be Heads
# how many heads?
sum(coin_flips2)
# how many tails?
20 - sum(coin_flips2)
length(coin_flips2) - sum(coin_flips2)

#
#  48 managers, 48 case files, 24 females and 24 males.
#  *IF* there was no bias ... we expect female+male to be independent of the decision
#  * 35 promotions, 13 fail-to-promotes
#  * sample from this set of decisions 24x ==> females
#  * the leftovers ==> males
#

# lets let 1 be a promotion, 0 be a not-promotion
cases <- c( rep(1, 35), rep(0, 13) )
trial1 <- sample(cases, 24, replace=FALSE)  
# Percentage promoted ... total # of 1s / total # of samples 
females <- sum(trial1) / length(trial1)
males <- ( sum(cases) - sum(trial1) ) / length(trial1)
result <- males - females 

results <- rep(0, 1000)

for(j in 1:1000) {
  trial <- sample(cases, 24, replace=FALSE)   
  females <- sum(trial) / length(trial)
  males <- ( sum(cases) - sum(trial) ) / length(trial) 
  results[j] <- males - females
}
# create a histogram (default settings)
hist(results)

### Logical comparison
#
#  Any logical operator returns a TRUE/FALSE logical value
#  in R. The base set are: >, <, ==, <=, >=
#
sum( results >= 0.291 ) 

#
#  Your practice : DVD
#

#
#  97 purchases, 53 no-purchase (150 total)
#  75 treatment, 75 control
#  0.200 = treatment - control
#

# Define 1 = buy, 0 = no-buy
cases <- c( rep(1, 97), rep(0, 53) )

# trial = pull 75 cases from the sack ==> control
#       => the remaining are the treatment group
results <- rep(0, 10000)
for(j in 1:10000) { 
  control <- sum( sample(cases, 75, replace=FALSE) ) 
  treatment <- sum(cases) - control
  results[j] <- (treatment - control) / 75
}


hist(results)


















