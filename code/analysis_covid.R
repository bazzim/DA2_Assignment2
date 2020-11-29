# Data Analysis 2 and Coding with R
# 
# Date 2020-11-29

library(WDI)
library(tidyverse)

# Get data from clean folder

df <- read.csv('data/clean/covid_clean.csv')


# Analysis of the data ----------------------------------------------------

## Main Aim

# The main aim of this analysis is find out if we can predict the number of deaths from COVID-19 by knowing the number of 
# registered COVID-19 cases. There are two variables: the explanatory variable is the total number of registered COVID-19
# cases for a certain country, and the dependent variable is the total number of registered deaths from COVID-19 in the same country.
# We have a nearly population data in our sample as we have the data for all world countries except only 6 countries 
# (Diamond Princess, MS Zaandam, Western Sahara, Taiwan, Ertirea, and Holy See (Vatican). 
# Both datasets make sense for our analysis; it is a good to model the research question.


###
# Date to analyze: 17 September 2020
# Explanatory variable: Number of registered cases
# Dependent Variable: Number of registered deaths.

# 1.Number of registered cases

library(ggpubr) # publication ready graphs

summary(df$confirmed)

ggplot(data = df, aes(x = confirmed))+ geom_histogram()

# quantile-quantile plot to check distribution
ggqqplot(df$confirmed)

# log transform

df<- df %>% mutate(log_confirmed = log(confirmed))

head(df)

summary(log(df$confirmed))

summary(df$log_confirmed)

ggplot(data = df, aes(x = log_confirmed))+ geom_histogram()

ggqqplot(df$log_confirmed)

# The summary statistics for the number of registered cases are indicating right tailed distribution.
# This is shown in the histogram and the quantile-quantile plot of this variable.
# To make this variable more normally distributed, we ln transformed this variable to another variable called log_confirmed.
# The log transformed variable is more normally distributed as shown by the histogram and qqplot.


# 2.Number of registered deaths

summary(df$death)

ggplot(data = df, aes(x = death))+ geom_histogram()

# quantile-quantile plot to check the distrubtion
ggqqplot(df$death)

# log transform

df<- df %>% mutate(log_death = log(death)) %>%
  
  mutate(log_death = ifelse(log_death== -Inf,0,log_death))

head(df)

summary(df$log_death)

ggplot(data = df, aes(x = log_death))+ geom_histogram()

ggqqplot(df$log_death)

# The summary statistics for the number of registered deaths are indicating right tailed distribution.
# This is shown in the histogram and the quantile-quantile plot of this variable.
# To make this variable more normally distributed, we ln transformed this variable to another variable called log_death. 
# Because our death column contain zero variables, we replaced the -Inf of the ln transformed variable by 0.
# The log transformed variable is more normally distributed as shown by the histogram and qqplot.
# We will not drop any observations from our data to not lose vital informations.

# Although the plot are skewed and have outliers. These outliers were not removed as it may bias our estimates.
# Variables were not scaled as they have the same unit of measurement, the total number of deaths or cases per country.


df2<- df %>% select(confirmed, death, log_confirmed, log_death)

dat<-sapply(df2,summary) %>% data.frame() %>% 
  
  rownames_to_column(var = "summary")

dat2<-sapply(df2,sd) 

round(dat2,2) # 2 d.p

dat %>% add_row(summary = "standard_deviation", confirmed = 674664.34,
                death = 19545.43, log_confirmed = 2.54,
                log_death = 2.77) 

# Both the explanatory and dependent variables are skewed and not normally distributed. 
# However, log transformed variables are more normally distributed than the original variables.


## Plotting different scatter-plots with lowess

# scatter plot: original variables
ggplot(data = df, aes(x=confirmed, y = death))+ geom_point()+
  geom_smooth(method = "loess")

# scatter plot: confirmed vs.log_death
ggplot(data = df, aes(x=confirmed, y = log_death))+ geom_point()+
  geom_smooth(method = "loess")

# scatter plot: log_confirmed vs. death
ggplot(data = df, aes(x= log_confirmed, y = death))+ geom_point()+
  geom_smooth(method = "loess")

# scatter plot: log_confirmed vs. log_death
ggplot(data = df, aes(x= log_confirmed, y = log_death))+ geom_point()+
  geom_smooth(method = "loess")

# When plotting the original variables, no clear correlation between the variables was observed
# The log transformation can be used to disclose a relation between the variables.
# The best fitting line is for plotting the log_death vs. the log_confirmed, where the most of data
# points do not deviate heavily from the fitted lowess line. Also, the data points are equally distributed
# along the lowess line so this nearly linear relation can be modeled with our regression model.



# Regression Models -------------------------------------------------------

## i. Simple linear regression
mod_l <- lm(log_death ~ log_confirmed, data = df)

summary(mod_l)

ggplot(df, aes(log_confirmed, log_death)) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ x)


## ii. Quadratic (linear) regression

# fit second order polynomial regression
mod_q <- lm(log_death ~ poly(log_confirmed,2, raw = TRUE),data = df)

summary(mod_q)

ggplot(df, aes(log_confirmed, log_death)) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x,2, raw = TRUE))


## iii. Piecewise linear spline regression

library(lspline)

# 2 knots dividing range of log_conformed into 3 equal intervals
mod_s <- lm(log_death ~ elspline(log_confirmed, 3), data = df)

summary(mod_s)

ggplot(df, aes(log_confirmed, log_death)) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ elspline(x, 3))


## iv. Weighted linear regression, using population as weights
mod_w <- lm(log_death ~ log_confirmed,data = df, weights = population)

summary(mod_w)

ggplot(df, aes(log_confirmed, log_death)) +
  geom_point() +
  stat_smooth(method = lm, aes(weight = population),formula = y ~ x)


# Model Comparisons -------------------------------------------------------

library(modelr)

modelr::rmse(mod_l,df) #root mean squared error for simple linear regression model

modelr::rsquare(mod_l,df) #R^2 for simple linear regression model

modelr::rmse(mod_q,df) #root mean squared error for quadratic (linear) regression model

modelr::rsquare(mod_q,df) #R^2 for quadratic (linear) regression model

modelr::rmse(mod_s,df) #root mean squared error for piecewise linear spline regression model

modelr::rsquare(mod_s,df) #R^2 for piecewise linear spline regression model

modelr::rmse(mod_w,df) #root mean squared error for weighted linear regression model

modelr::rsquare(mod_w,df) #R^2 for weighted linear regression model

dat<- data.frame("model" = c("linear","quadratic","spline","weighted"),
                 
                 "R-squared" = c(rsquare(mod_l,df),rsquare(mod_q,df),
                                 
                                 rsquare(mod_s,df),rsquare(mod_w,df)),
                 
                 "root_mean_squared_error" = c(rmse(mod_l,df),rmse(mod_q,df),
                                               
                                               rmse(mod_s,df), rmse(mod_w,df)))
round(dat,3)
e <- anova(mod_l,mod_w, mod_s, mod_q)
View(e)
# The best model is quadratic model with smallest root mean squared error and highest R-squared.
# This is also evident when using the ANOVA test to statistically compare the models, where the quadratic model
# has the lowest residual sum of squares (RSS). This is also evident from the graphical smoothing of this data.

# Formula for the coefficients:
#   $logdeath = -2.7 + 0.68(logconfirmed)+ 0.02(logconfirmed)^2$
  

# The interpretation of $\beta_1$ is for every 1% increase in the confirmed cases, the deaths will increase by 100*(1.01^0.68 -1)= 0.67%.
# For every 10% increase in the confirmed cases, the deaths will increase by 100*(1.1^0.68 -1)= 6.7%.


# Hypothesis Testing ------------------------------------------------------

# on $\beta_1$ (which interacts with x) 
# Test the following hypothesis: H0 : $\beta_1$ = 0; HA : $\beta_1 \neq 0$.
# Choose a significance level and make your conclusion.
# Our significance level is 0.05. We can conclude that our sample indicates that $\beta_1$ is significantly larger than 0.

library(broom)

v <- tidy(mod_q)
g <- glance(mod_q)

# The p-value of the total model is almost 0. This indicates that our model is significantly better than the null model.
# The r-squared is 0.90 or our models describes about 90% of the variability in the log deaths of COVID-19.



# Analysis of the residuals -----------------------------------------------

# Interpretation for countries with the largest negative errors. 
# Interpretation for countries with the largest positive errors. 

# plotting residuals vs fitted values
plot(mod_q, which = 1)

# largest countries with negative errors
dat<- data.frame(country = df$country, resid = mod_q$residuals,
                 fitted = mod_q$fitted.values, actual = df$log_death)

head(dat)

dat %>% arrange(resid) %>% head(5)


# largest countries with positive errors
dat %>% arrange(resid) %>% tail(5)

