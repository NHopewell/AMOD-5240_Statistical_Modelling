 library(ggplot2)

# let 1 be reported 'yes' symptoms did improve. Let 0 be 'no' symtptoms did not improve
casesSinus <- c(rep(1, 131), rep(0,35))

# Simulate 10,000 iterations
numtSinus <- 10000
resultSinus <- rep(0, numtSinus)

newResults = data.frame(resultSinus)
View(newResults)



for(j in 1:numtSinus) {
  trialSinus = sample(casesSinus, 85, replace = FALSE)
  antiTreat = sum(trialSinus) / length(trialSinus)
  conTreat = ( sum(casesSinus) - sum(trialSinus))/(length(casesSinus) - length(trialSinus))
  resultSinus[j] = conTreat - antiTreat
}

histResults <- ggplot(newResults, aes(resultSinus)) +
  theme(legend.position = "none") +
  geom_histogram(aes(y=..density..), colour="black", fill="grey", bins = 21) +
  labs(x= "Simulated Sinusitis Study Under Conditions of Null Hypothesis", y = "Proportion of Simulated Scenarios")+
  geom_vline(xintercept = mOutcome-(2*sdOutcome), colour="blue")+
  geom_vline(xintercept = mOutcome+(2*sdOutcome), colour="blue")+
  geom_vline(xintercept = -0.026, colour = "red", linetype = "dotted", size = 1.5)+
  stat_function(fun = dnorm,
                args = list(mean = mean(newResults$resultSinus, na.rm = TRUE), sd =                        sd(newResults$resultSinus, na.rm = TRUE)),
                colour = "black", size = 1)
histResults 

?ggplot



mOutcome = mean(newResults$resultSinus)
sdOutcome = sd(newResults$resultSinus)


# add lines for 2 SDs above and below
