
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
table(turnout$vote)

# Averaging by year
turnout %>% group_by(educate) %>%
  summarize(vote = mean(vote), count=n())

## Visualizing data

# Histograms
ggplot(turnout, aes(educate)) + geom_histogram()
ggplot(turnout, aes(income)) + geom_histogram()

# Scatterplots
ggplot(turnout, aes(educate, income)) + geom_point() + geom_smooth(method="lm")

# Densities
ggplot(turnout, aes(income)) + geom_density() 
ggplot(turnout) + 
  geom_density(aes(x=educate, colour=as.factor(vote)))

## Binscatter

# Averaging the 
turnout_binned <- turnout %>% mutate(bin=cut2(educate, g=9)) %>% 
  group_by(bin) %>% dplyr::summarize(mean_vote=mean(vote), n=n(), sd=sd(vote), na.rm = TRUE)

ggplot(turnout_binned, aes(mean_vote, bin, group=1)) + geom_point(aes(size=n), shape=1) + 
  geom_smooth(method="lm", se=FALSE) 

## Linear regression models
lmodel1 <- lm(data = turnout, formula = vote ~ educate + income)

# Examiming the output the base R way
summary(lmodel1)

# Examining the output in a tidy way
library(broom)
stargazer(lmodel1, type = "text")

# To access coefficient one can save it as a dataframe
coefs <- data.frame(tidy(lmodel1))

# Other functional forms
lmodel2 <- lm(data = turnout, vote ~ educate + income + I(race))
stargazer(lmodel2, type = "text")

## Hypothesis testing
library(car)
hypothesis1 <- linearHypothesis(lmodel1, c("income", "educate"))
hypothesis2 <- linearHypothesis(lmodel1, c("income = educate"))

## Robust standard errors
# Correcting for heteroskedasticity
library(lmtest)

# Output as a dataframe
lmodel1_corrected_df <- lmodel1 %>% coeftest(vcoc=hccm) %>% tidy %>% data.frame

# Output as a table
lmodel1_corrected_table <- lmodel1 %>% coeftest(vcov=hccm)
stargazer(lmodel1,se=list(lmodel1_corrected_table[,2]),type="text")

# Correcting for clustering
library(clubSandwich)

#Output as a dataframe
lmodel1_corrected_df <- lmodel1 %>% coef_test("CR1", cluster=turnout$race)  %>% data.frame

# Output as a table
lmodel1_corrected_table <- coef_test(lmodel1, "CR1", cluster=turnout$race) %>% 
stargazer(lmodel1, se=list(lmodel1_corrected_table$SE),type="text")


## Panel data
library(plm)

#Dahlberg and Johansson Municipal Expenditure Data, 265 Swedish Municipalities, 9 years
#Variables in the file are
#ID = Identification, 1,..., 265
#YEAR = year, 1979,...,1987
#EXPEND = Expenditures
#REVENUE = Receipts, taxes and Fees
#GRANTS = Government grants and shared tax revenues
#See Greene (2003, pp. 551 and elsewhere) for analysis of these data. The article on which the analysis is based is Dahlberg, M. and E. Johannson, E., "An Examination of the Dynamic Behavior of Local Governments using GMM Bootstrapping Methods," Journal of Applied Econometrics, 15, 2000, pp. 401-416.  (These data were downloaded from the JAE data archive.)

muni_data <- read_csv("dahlberg.csv")
names(muni_data) <- tolower(names(muni_data))

# Describing the data
pdim(muni_data)

# Panel data models
lm_pooled <- plm(expend ~ revenue + grants, data = muni_data, index = c("id", "year"), model = "pooling")
lm_random <- plm(expend ~ revenue + grants, data = muni_data, index = c("id", "year"), model = "random")
lm_fixed <- plm(expend ~ revenue + grants, data = muni_data, index = c("id", "year"), model = "within")
lm_first_diff <- plm(expend ~ revenue + grants, data = muni_data, index = c("id", "year"), model = "within")

# Finally: clustering at the unit level with FE models
lm_fixed <- plm(expend ~ revenue + grants, data = muni_data, index = c("id", "year"), model = "within")

coeftest(lm_fixed, vcov=vcovHC(lm_fixed, type="HC1", cluster="group")) %>% tidy

# More about clustering here: 
# https://stats.stackexchange.com/questions/10017/standard-error-clustering-in-r-either-manually-or-in-plm/60262




