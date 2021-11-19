############################################
### Analysis of the advertising data set ###
############################################

## 200 observations
## Sales: Number of units sold (in thousand)
## TV, Radio, Newpaper: different media budgets (in thousand EUR)
## NewspaperCat: Categorical variable indicating whether 
##                the Newspaper budget was between (0, 25000], (25000, 50000], or more than 50000

## Read in the data
advertising <- read.csv(url("http://statmath.wu.ac.at/~vana/datasets/advertising.csv"))

filtered <- subset(advertising, select = -c(X.1, X, NewspaperCat))
head(filtered, n = 10)
plot(filtered)
cor(filtered)

## remove first two columns X.1 and X as they are redundant.
advertising <- advertising[, -(1:2)]

## Descriptives
summary(advertising)
str(advertising)

# Plot of sales with the different media budgets
plot(advertising$TV, advertising$Sales, ylab = "Sales", xlab = "TV")

plot(Sales ~ Radio, data = advertising)
plot(Sales ~ Newspaper, data = advertising)
plot(Sales ~ NewspaperCat, data = advertising) # boxplot


## Fit a linear regression of Sales ~ TV
fitTV <- lm(Sales ~ TV, data = advertising) 
fitTV

plot(Sales ~ TV, data = advertising)
abline(fitTV, col = "red", lwd = 2)

## Fit a linear regression of Sales ~ Radio
fitRadio <- lm(Sales ~ Radio, data = advertising)
plot(Sales ~ Radio, data = advertising)
abline(fitRadio, col = "red", lwd = 2)
fitRadio

## Fit a linear regression of Sales ~ Newspaper
fitNews <- lm(Sales ~ Newspaper, data = advertising)
plot(Sales ~ Newspaper, data = advertising)
abline(fitNews, col = "red", lwd = 2)

# Summary of the linear model
summary(fitTV)
## %<----------------------------------
## Interpretation of the coefficients
## 1 unit increase in x causes a beta1 unit increase in y. 
## 1 unit increase (i.e., 1000$ increase) in TV ad budget leads to a 0.047537 * 1000= 47.537 increase in sales.  
## %<----------------------------------

## Diagnostic plots
plot(fitTV, 1) ## Fitted values vs residual plot - a good fit would be indicated by a random plot. 
plot(fitTV, 2) ## Normal qqplot - checking the normality of residuals (important for validity of standard errors)
plot(fitTV, 4) ## Identification of influential observations


# Get the R^2
summary(fitTV)$r.squared
summary(fitRadio)$r.squared
summary(fitNews)$r.squared

## When looking at the scatterplot, a non linearity is apparent, especially in the bottom left corner.
## incorporate nonlinearity for TV budget by using the log variables 
## 
fitTV2 <- lm(log(Sales) ~ log(TV), data = advertising)
summary(fitTV2)
## %<----------------------------------
## Interpretation of the coefficients
## Given the log scale of both variables, the coefficient beta1 can be intrepreted in terms of percentage change:
## 1% unit increase in x causes a beta1% increase in y. 
## 1% unit increase in TV ad budget leads to a 0.35% increase in sales.  
## %<----------------------------------

## plot on the log scale
plot(log(Sales) ~ log(TV),  data = advertising)
abline(fitTV2,  col = "red", lwd = 2)

## plot on original scale
plot(Sales ~ TV,  data = advertising) ## scatterplot
abline(fitTV, col = "red", lwd = 2) ## linear regression on original scale
## plot regression line on the log scale to original scale
## 1. extract fitted values on log scale
yhat_log <- fitTV2$fitted.values ## these are logs
yhat <- exp(yhat_log) ## transform to original scale
lines(sort(advertising$TV), yhat[order(advertising$TV)], 
       col = "blue", lwd = 2)


## check R squared for the different models
summary(fitTV)$r.squared
summary(fitTV2)$r.squared

################################
## Multiple linear regression ##
################################
fitAll <- lm(Sales ~ TV + Radio + Newspaper, 
             data = advertising) 

summary(fitAll)

## plot scatterplots for all pairs of variables
pairs(advertising[c("TV", "Radio", "Newspaper", "Sales")])
cor(advertising[,1:4])

## Model selection
## forward selection based on AIC
step(fitAll, direction = "forward")

## backward selection based on AIC
step(fitAll, direction = "backward")

## forward-backward selection based on AIC
fit.select <- step(fitAll, direction = "both")
fit.select

## Categorical variables as independent variables
table(advertising$NewspaperCat)
if (!require(dplyr)) install.packages("dplyr"); library("dplyr");
## compute average sales in each category of Newspaper
advertising %>% group_by(NewspaperCat) %>% summarize(mean_size = mean(Sales, na.rm = TRUE))

fitNewsCat <- lm(Sales ~ NewspaperCat, data = advertising)
summary(fitNewsCat)
## %<----------------------------------
## Interpretation of the coefficients
## Category (0, 25) is the baseline. 
## 1) The intercept tell us that the average sales in category (0,25) are at 12.9588 * 1000 = 12958.8 units
## 2) The coefficient NewspaperCat(25,50] gives the additional amount of sales in category(25,50) on top of the baseline:
## I.e., being in category (25,50) adds more 0.9412*1000 = 941.2 units on top of the baseline of 12958.8 units. 
## In category (25, 50) the average sales are hence 12958.8 +  941.2 = 13900 units
## 3) The coefficient NewspaperCat(50,150] gives the additional amount of sales in category(50,150) on top of the baseline:
## I.e., being in category (50,150) adds more  3.9886*1000 = 3988.6 units on top of the baseline of 12958.8 units. 
## In category (50, 150) the average sales are hence 12958.8 +  3988.6 = 16947.4 units
## %<----------------------------------


## Compare models
## Residual standard error
c(summary(fitTV)$sigma, 
  summary(fit.select)$sigma, 
  summary(fitAll)$sigma)
## adjusted R squared
c(summary(fitTV)$adj.r.squared, 
  summary(fit.select)$adj.r.squared, 
  summary(fitAll)$adj.r.squared)

## make an out of sample exercise
## keep last 20 observations for testing
test_dat  <- advertising[181:200, ]
train_dat <- advertising[1:180, ]

fit1     <- lm(Sales ~ TV, data = train_dat)
fit2     <- lm(Sales ~ TV + Radio, data = train_dat)
fitAll <- lm(Sales ~ TV + Radio + Newspaper, 
             data = train_dat)

## compute the prediction errors
p1 <- predict(fit1, test_dat) # predictions
summary(p1 - test_dat$Sales) 
p2 <- predict(fit2, test_dat) # predictions

p3 <- predict(fitAll, test_dat) # predictions
## mean squared error
c(mean((p1 - test_dat$Sales)^2), 
  mean((p2 - test_dat$Sales)^2), 
  mean((p3 - test_dat$Sales)^2))

