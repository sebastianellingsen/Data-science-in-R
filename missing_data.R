# This note presents how to deal with missing data

# Missing at random
cars <- mtcars %>% mutate(mpg = ifelse(runif(32)>0.7, NA, mpg),
                          hp = ifelse(runif(32)>0.9, NA, hp),
                          wt = ifelse(runif(32)>0.85, NA, wt))

## Detecting missing data

# Let's summarize the data to look for missing values using stargazer:
cars %>% as.data.frame %>% stargazer(type="text")

# Base R has several features to detect missing data
