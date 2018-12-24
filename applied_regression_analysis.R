
library(tidyverse)
library(stargazer)

## Loading and preparing data
turnout <- read_csv("turnout.csv") %>% select(-X1)
str(turnout, max.level=1)

turnout <- turnout %>% mutate(race=as.factor(race))

## Summarizing data

# Summarizing the data the R way
summary(turnout)

# Stargazer provides a more "Stata" way to summarize the dataframe 
turnout %>% as.data.frame %>% stargazer(type="text")

# Reporting frequecies
table(car_accidents$sex)

# Averaging by year
turnout %>% group_by(educate) %>%
  summarize(vote = mean(vote), count=n())




