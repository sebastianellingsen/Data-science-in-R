
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
lm_first_diff <- plm(expend ~ revenue + grants, data = muni_data, index = c("id", "year"), model = "within", effect = "twoways")

# Finally: clustering at the unit level with FE models
lm_fixed <- plm(expend ~ revenue + grants, data = muni_data, index = c("id", "year"), model = "within")

coeftest(lm_fixed, vcov=vcovHC(lm_fixed, type="HC1", cluster="group")) %>% tidy

# More about clustering here: 
# https://stats.stackexchange.com/questions/10017/standard-error-clustering-in-r-either-manually-or-in-plm/60262


## Use felm for applied regressions 



## Example for showing unbiasedness
# generate a fictious population
pop <- rnorm(10000, 10, 1)

# sample from the population and estimate the mean
est1 <- replicate(expr = mean(sample(x = pop, size = 5)), n = 25000)

est2 <- replicate(expr = mean(sample(x = pop, size = 25)), n = 25000)

fo <- replicate(expr = sample(x = pop, size = 5)[1], n = 25000)


# plot density estimate Y_1
plot(density(fo), 
     col = 'green', 
     lwd = 2,
     ylim = c(0, 2),
     xlab = 'estimates',
     main = 'Sampling Distributions of Unbiased Estimators')

# add density estimate for the distribution of the sample mean with n=5 to the plot
lines(density(est1), 
      col = 'steelblue', 
      lwd = 2, 
      bty = 'l')

# add density estimate for the distribution of the sample mean with n=25 to the plot
lines(density(est2), 
      col = 'red2', 
      lwd = 2)

# add a vertical line at the true parameter
abline(v = 10, lty = 2)

# add N(10,1) density to the plot
curve(dnorm(x, mean = 10), 
      lwd = 2,
      lty = 2,
      add = T)

# add a legend
legend("topleft",
       legend = c("N(10,1)",
                  expression(Y[1]),
                  expression(bar(Y) ~ n == 5),
                  expression(bar(Y) ~ n == 25)
       ), 
       lty = c(2, 1, 1, 1), 
       col = c('black','green', 'steelblue', 'red2'),
       lwd = 2)






# plot the standard normal density on the interval [-4,4]
curve(dnorm(x),
      xlim = c(-4, 4),
      main = 'Calculating a p-Value',
      yaxs = 'i',
      xlab = 'z',
      ylab = '',
      lwd = 2,
      axes = 'F')

# add x-axis
axis(1, 
     at = c(-1.5, 0, 1.5), 
     padj = 0.75,
     labels = c(expression(-frac(bar(Y)^"act"~-~bar(mu)[Y,0], sigma[bar(Y)])),
                0,
                expression(frac(bar(Y)^"act"~-~bar(mu)[Y,0], sigma[bar(Y)]))))

# shade p-value/2 region in left tail
polygon(x = c(-6, seq(-6, -1.5, 0.01), -1.5),
        y = c(0, dnorm(seq(-6, -1.5, 0.01)),0), 
        col = 'steelblue')

# shade p-value/2 region in right tail
polygon(x = c(1.5, seq(1.5, 6, 0.01), 6),
        y = c(0, dnorm(seq(1.5, 6, 0.01)), 0), 
        col = 'steelblue')





# vector of sample sizes
n <- c(10000, 5000, 2000, 1000, 500)

# sample observations, estimate using 'sd()' and plot the estimated distributions
sq_y <- replicate(n = 10000, expr = sd(rnorm(n[1], 10, 10)))
plot(density(sq_y),
     main = expression('Sampling Distributions of' ~ s[Y]),
     xlab = expression(s[y]),
     lwd = 2)

for (i in 2:length(n)) {
  sq_y <- replicate(n = 10000, expr = sd(rnorm(n[i], 10, 10)))
  lines(density(sq_y), 
        col = i, 
        lwd = 2)
}

# add a legend
legend("topleft",
       legend = c(expression(n == 10000),
                  expression(n == 5000),
                  expression(n == 2000),
                  expression(n == 1000),
                  expression(n == 500)), 
       col = 1:5,
       lwd = 2)



## Hypothesis testing
# set seed
set.seed(1)

# generate some sample data
sampledata <- rnorm(100, 10, 10)

# check the type of the outcome produced by t.test
t.test(sampledata)



# set random seed
set.seed(1)

# draw data from two different populations with equal mean
sample_pop1 <- rnorm(100, 10, 10)
sample_pop2 <- rnorm(100, 10, 20)

# perform a two sample t-test
t.test(sample_pop1, sample_pop2)




## Calculating correlations, add this here 
library(MASS)

# set random seed
set.seed(1)

# positive correlation (0.81)
example1 <- mvrnorm(100,
                    mu = c(0, 0), 
                    Sigma = matrix(c(2, 2, 2, 3), ncol = 2),
                    empirical = TRUE)

# negative correlation (-0.81)
example2 <- mvrnorm(100,
                    mu = c(0, 0), 
                    Sigma = matrix(c(2, -2, -2, 3), ncol = 2),
                    empirical = TRUE)

# no correlation 
example3 <- mvrnorm(100,
                    mu = c(0, 0), 
                    Sigma = matrix(c(1, 0, 0, 1), ncol = 2),
                    empirical = TRUE)

# no correlation (quadratic relationship)
X <- seq(-3, 3, 0.01)
Y <- - X^2 + rnorm(length(X))

example4 <- cbind(X, Y)

# divide plot area as 2-by-2 array
par(mfrow = c(2, 2))

# plot datasets
plot(example1, col = 'steelblue', pch = 20, xlab = 'X', ylab = 'Y', 
     main = "Correlation = 0.81")

plot(example2, col = 'steelblue', pch = 20, xlab = 'X', ylab = 'Y', 
     main = "Correlation = -0.81")

plot(example3, col = 'steelblue', pch = 20, xlab = 'X', ylab = 'Y', 
     main = "Correlation = 0")

plot(example4, col = 'steelblue', pch = 20, xlab = 'X', ylab = 'Y', 
     main = "Correlation = 0")




# 
# 
# The following packages are needed for reproducing the code presented in this chapter:
#   
#   AER - accompanies the Book Applied Econometrics with R Kleiber & Zeileis (2008) and provides useful functions and data sets.
# 
# MASS - a collection of functions for applied statistics.
# 
# Make sure these are installed before you go ahead and try to replicate the examples. The safest way to do so is by checking whether the following code chunk executes without any errors.

library(AER)
library(MASS)
data(CASchools)



## Manually calculating the ols coefficients

attach(CASchools) # allows to use the variables contained in CASchools directly

# compute beta_1_hat
beta_1 <- sum((CASchools$STR - mean(CASchools$STR)) * (CASchools$score - mean(CASchools$score))) / sum((CASchools$STR - mean(CASchools$STR))^2)

# compute beta_0_hat
beta_0 <- mean(score) - beta_1 * mean(STR)

# print the results to the console
beta_1


## Compare to the linear model 
# estimate the model and assign the result to linear_model
linear_model <- lm(score ~ STR, data = CASchools)

# print the standard output of the estimated lm object to the console 
linear_model




## Measures of goodness of fit 
# compute R^2 manually
SSR <- sum(mod_summary$residuals^2)
TSS <- sum((score - mean(score))^2)
R2 <- 1 - SSR/TSS

# print the value to the console
R2






## Hypothesis testing 

library(AER)
library(scales)


























