

census <- read.table("https://www.openintro.org/stat/data/cc.txt", header = TRUE, stringsAsFactors = FALSE)

options(scipen=0)

head(census, 10)

str(census)

View(census)

# this is encoded totally wrong for states that range from 1 - 9


FIPS_correct <- sprintf("%05d",census$FIPS)
census_fixed <- cbind(FIPS_correct, census)

View(census_fixed)

# save state as first two digits of correct FIPS column
census_fixed <- transform(census_fixed, State = substr(FIPS_correct, 1, 2))


# of the fixed dataframe, group by state and summarize pop2000 and pop2010 as a sum saving results in a new data frame
# this code from one day to the next without changing one single thing stopped working. 
census_pops <- census_fixed %>% 
  group_by(State) %>% 
  dplyr::summarise(sum2000 = sum(pop2000),
                   sum2010 = sum(pop2010))


# save sum of population of US states in 2000
USpop2000 <- sum(census_pops$sum2000)
# population distribution across states in 2000
print(census_pops$sum2000 / USpop2000)
# verify values sum to 1
sum(print(census_pops$sum2000 / USpop2000))



# save sum of population of US states in 2010
USpop2010 <- sum(census_pops$sum2010)
# population distribution across states in 2010
print(census_pops$sum2010 / USpop2010)
# verify values sum to 1
sum(print(census_pops$sum2010 / USpop2010))

library(plyr)
library(forcats)
census_pops_named = revalue(census_pops$State, c("01" = "Alabama", "02" = "Alaska", "04" = "Arizona", "05" = "Arkansas", "06" = "California", "08" = "Colorado",
                                                 "09" = "Connecticut", "10" = "Delaware", "11" = "District of Columbia", "12" = "Florida", "13" = "Georgia",
                                                 "15" = "Hawaii", "16" = "Idaho", "17" = "Illinois", "18" = "Indiana", "19" = "Iowa", "20" = "Kansas", "21" = "Kentucky",
                                                 "22" = "Louisiana", "23" = "Maine", "24" = "Maryland", "25" = "Massachusetts", "26" = "Michigan", "27" = "Minnesota", 
                                                 "28" = "Mississippi", "29" = "Missouri", "30" = "Montana", "31" = "Nebraska", "32" = "Nevada", "33" = "New Hampshire",
                                                 "34" = "New Jersey", "35" = "New Mexico", "36" = "New York", "37" = "North Carolina", "38" = "North Dakota", 
                                                 "39" = "Ohio", "40" = "Oklahoma", "41" = "Oregon", "42" = "Pennsylvania", "44"= "Rhode Island",
                                                 "45" = "South Carolina", "46" = "South Dakota", "47" = "Tennessee", "48" = "Texas", "49" = "Utah", "50" = "Vermont",
                                                 "51" = "Virginia", "53" = "Washington", "54" = "West Virginia", "55" = "Wisconsin","56" = "Wyoming"))


census_final <- cbind(census_pops_named , census_pops)



US_retire <- mean(census_fixed$age_over_65)

US_retire
  

sd(census_fixed$age_over_65)










<br />  
  
  ```{r}
sum(census$pop2010)


census_fixed %>%
  group_by(State) %>% 
  dplyr::summarise(retired = sum(pop2010))
```



Population of the US in 2010 = 308,405,509
Population of Florida in 2010 = 18,801,310

Because the null hypothsis is that the proportions are equal, I will used a pooled proportion estimate: 
  
  $SE$ = sqrt((0.181x(1 - 0.181)/18801310) + (0.1583x(1 - 0.1583)/308405509)) = 0.00009119504








library(olsrr) # for subset selection

census_Reg <- census[,-2]
ownership_model <- lm(home_ownership ~ ., data = census_Reg )
ols_step_forward(ownership_model, details = TRUE)






logi_data <- census_fixed%>%
              group_by(State) %>% 
              dplyr::summarise(state_poverty = mean(poverty)) %>%
              mutate(affluent = ifelse(state_poverty <= 10, 1,0))



