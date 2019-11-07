####
# Statistical Modelling lab 1 

setwd("C:/Users/nicho/Desktop/My Files/Statistical Modelling R Labs/Statistical Modelling data files")

  
### >>>> Sampling 

mySample <- sample(1:20, 1, replace = FALSE)
letterSample <- sample(c("A", "B","C"), 1)
  # coin flips
coinFlip <- sample(C("H", "T"), 20, replace = TRUE)
coinFlip2 <- sample(c(0,1), 20, replace = TRUE)
sum(coinFlip2)
20 - sum(coinFlip2)
length(coinFlip2) - sum(coinFlip2)


# simulaiton sample using example from lecture about promotions and gender

#    40 managers, 40 case files, 24 female and 24 males
#    If no bias, female would be independant of decison


# let the 1 be a promotion anf let 0 be a non promotion

cases <- c(rep(1,35), rep(0,13))
sampleCases <- sample(cases, 24, replace = FALSE)

# percentage promotion = total number of 1s / total number of samples
females <- sum(sampleCases) / length(sampleCases)

males <- sum(cases) - sum(sampleCases) / length(sampleCases)

resultTrial1 <- males - females
# this is the result of the first hypothetical trial, but we need to compare many trials ot our observed result

results <- rep(0, 1000)

# use a for loop
for(j in 1:100), {
  
  
}

  


#  Logical comparison

#  Logical operators return a true / false value

#  We are interested in results being greater than or equal to 0.291
results >= 0.291

# Since results is a vector of 1000 values, it will return 1000 values of true and false
# to avoid this we just use the sum function - t/f are just 0s and 1s behind the scenes
# this will show how many are as extreme or more extreme

# this value of 0.20 is what we found in the population
sum(results >= 0.20)




# _________

#  New exmaple of simulation 

#  Using the opportunity cost example about DVDs
#   Page 35 of the slides from week 2

#  97, 53, 150 are the important numbers

# 97 purchases, 53 no purchases, 150 total
# 75 treatment and 75 control
# 0.20 = treatment - control

# 1 = buy 0 = no buy
cases <- c(rep(1,97), rep(0,53))

# a trial = pull 75 cases from the sack and assign that to be a control group
# the remaining are the treatment group

# I want to sample from cases 75 times without replacement

control <- sum(sample(cases, 75, replace = FALSE))
treament <- sum(cases - control)
result <- (treament - control) / 75


#  insert his code 


# to get p value 

sum(results >= 0.20)
# should equal 82
82/1000 * 2
# this will get the 2 pailed p value
82/1000
# this will get a 1 tailed p value


