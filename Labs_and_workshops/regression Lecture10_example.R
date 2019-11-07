#
# Some Linear Model example code


# Let's start with Lock5 data set:
library(Lock5withR)
data("RestaurantTips")
head(RestaurantTips)

# Always start by plotting the variables. In this case, we want to examine
# the Tip as a function of the bill total (Bill)

plot(Tip ~ Bill, data = RestaurantTips)

# let's do a linear fit
fit <- lm(Tip ~ Bill, data = RestaurantTips)
str(fit)
# fit doesn't have the Standard Errors of the fit - need to summary() the fit
summ <- summary(fit)
summ$coefficients

# Do the QQ plot
qqnorm(fit$residuals)

# there are six points which don't seem to fit : really big errors
plot(Tip ~ Bill, data = RestaurantTips)
x <- sort(RestaurantTips$Bill)
y <- summ$coefficients["(Intercept)", "Estimate"] +
     summ$coefficients["Bill", "Estimate"] * x
# (this is the plot of the fit overlaid on the scatterplot)

# Chris' question: identification of high residuals
amended <- data.frame(RestaurantTips, Resid = fit$residuals)
amended[amended$Resid > 5.0, ]
amended[amended$Resid > 2.0, ]
# looks like there's only two bills > $60, and 5 > $50



