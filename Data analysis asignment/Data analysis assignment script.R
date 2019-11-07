############################################
### AMOD 5240: Data Analaysis Assignment ###
###          Nicholas Hopewell           ###
###               0496633                ###
############################################

###      Prediction of House Prices     ###


#  Set working directory 
setwd("C:\\Users\\nicho\\OneDrive\\Desktop\\Assignments\\Statistical Modelling assignments\\Data analysis asignment")

#  Read csv file and save in object 'priceData'
priceData<-read.csv("house_prices.csv", header = TRUE)

#  View the data set (does not appear in markdown file obviously)
#View(priceData) 

head(priceData)
#  Examine the structure of the data set
str(priceData)
#  Get summary statistics (min, max, median, mean, 1st and 3rd quartiles) for each variable
summary(priceData)

describe(priceData) 


library("dplyr")
priceSub <- select(priceData, price:yr_built, sqft_living15, sqft_lot15)
head(priceSub)

(cors <- round(cor(priceSub), 3))


library("Hmisc")
(cors2 <- rcorr(as.matrix(priceSub)))


library("corrplot")
corrplot(cors, method = "ellipse", type ='upper', tl.col = "darkblue")


priceMod<-lm(price ~ sqft_living, data = priceData)
summary(priceMod)
