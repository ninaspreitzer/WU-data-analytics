if (!require(ROCR)) install.packages("ROCR"); library(ROCR)
if (!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
###################
### Click model ###
###################

clicks <- read.csv(url("http://statmath.wu.ac.at/~vana/datasets/click.csv"), sep = ",", header = TRUE)

## Descriptive statistics
summary(clicks) 
## balanced data set, 50% clicks
## 48.1% male
## Average age 36, clearly not a website targeting youngsters
## Quite big variation in income
## all values of Ad.Topic.Line are distinct, so this variable can be exculded from the analysis
## Few values are available for each city so it is hard to find a pattern, can be excluded
unique(clicks$Country) # 237 countries
head(sort(table(clicks$Country), decreasing = TRUE), 10)  ## Exclude Countries

## Graphics
ggplot(clicks, aes(x = Daily.Time.Spent.on.Site, y = Age,
                   group = factor(Clicked.on.Ad), 
                   col = factor(Clicked.on.Ad))) + 
  geom_point()

## Area income
ggplot(clicks, aes(x = Area.Income,
                   col = factor(Clicked.on.Ad))) + 
  geom_density()

## Gender
spineplot(factor(Clicked.on.Ad) ~ factor(Male), data = clicks)

## Time
str(clicks$Timestamp)
## it is a factor, lets transform to date
clicks$Timestamp <- as.POSIXct(clicks$Timestamp)
## Time of the day and of the month and of the year could play a role.
## We need to process the date:
## extract year
table(format(clicks$Timestamp, "%Y"))## not useful

## extract month
table(format(clicks$Timestamp, "%m"))
clicks$months <- factor(format(clicks$Timestamp, "%m"))

## extract day of week
clicks$dow <- factor(weekdays(clicks$Timestamp))

## Time of day
table(as.numeric(format(clicks$Timestamp, "%H")))
hours <- as.numeric(format(clicks$Timestamp, "%H"))
clicks$hours[hours %in% c(23, 0:7)] <- "night"
clicks$hours[hours %in% c(8:12)] <- "morning"
clicks$hours[hours %in% c(13:18)] <- "afternoon"
clicks$hours[hours %in% c(19:22)] <- "evening"
clicks$hours <- factor(clicks$hours)

plot(factor(Clicked.on.Ad) ~ hours, data = clicks) ## no clear effect
plot(factor(Clicked.on.Ad) ~ months, data = clicks) ## no clear effect
plot(factor(Clicked.on.Ad) ~ dow,   data = clicks) ## no clear effect

## Estimating logistic regression
## using all variables
fit_logit <- glm(Clicked.on.Ad ~ Area.Income + Age + Daily.Time.Spent.on.Site, 
           data = clicks, family = binomial())
summary(fit_logit)
## %<----------------------------------
## Interpretation of the coefficients
## 1 unit increase in xi leads to a betai unit increase in log-odds ratios.
## 1) alpha =  1.504e+01 gives the baseline log odds when the x variables are zero. --> what does 0 age mean? alpha not meaningful 
## 2) 1 unit increase in income causes a log odds ratio of beta1 = -1.173e-04.
##    which is the same as: 1 unit increase in income translates to a odds ratio of exp(beta1) =  0.9998827.
##    This means that the odds of clicking decrease by  (0.9998827-1)*100=-0.01173 for every additional income dollar.
## 3) 1 unit increase in Age causes a log odds ratio of beta2 =  1.630e-01.
##    which is the same as: 1 unit increase in Age translates to a odds ratio of exp(beta2) = 1.177037.
##    This means that the odds of clicking increase by  17.7037% for every additional age year.
## 4) 1 unit increase in Daily.Time.Spent.on.Site causes a log odds ratio of beta3 =  -2.048e-01.
##    which is the same as: 1 unit increase in Daily.Time.Spent.on.Site translates to a odds ratio of exp(beta3) =0.8148103.
##    This means that the odds of clicking decrease by  18.5% for every additional minute spent on the website.
## %<----------------------------------

fit_logit2 <- glm(Clicked.on.Ad ~ Area.Income + Age + Daily.Time.Spent.on.Site + Male + 
                    dow + months + hours, 
                 data = clicks, family = binomial())

summary(fit_logit2)  
AIC(fit_logit, fit_logit2)

fit_logit_step <- step(fit_logit2, direction = "both")
  
## compare predictions_on a test set:
## first build a training and a test sample
## Reproducibility???
id_train <- sample(1:1000, 900)
train <- clicks[id_train, ]
test  <- clicks[-id_train, ]
## fit models only on train data
fit_logit_train <- glm(Clicked.on.Ad ~ Area.Income + Age + Daily.Time.Spent.on.Site, 
                 data = train, family = binomial())

fit_logit_step_train <- glm(Clicked.on.Ad ~ Area.Income + Age + Daily.Time.Spent.on.Site + Male, 
                       data = train, family = binomial())

## in-sample, so-called fitted values
p1 <- predict(fit_logit_train,
              newdata = test,
              type = "response") ## probability
pred_1 <- prediction(p1, labels = test$Clicked.on.Ad)

p2 <- predict(fit_logit_step_train,
              newdata = test,
              type = "response") ## probability
pred_2 <- prediction(p2, labels = test$Clicked.on.Ad)

## ROC curve
perf_1 <- performance(pred_1, measure = "tpr", x.measure = "fpr") 
plot(perf_1)
perf_2 <- performance(pred_2, measure = "tpr", x.measure = "fpr") 
plot(perf_2, add = TRUE, col = "red")


auc1 <- performance(pred_1, measure = "auc")@y.values[[1]]

auc2 <- performance(pred_2, measure = "auc")@y.values[[1]]

c(auc1, auc2)

## For calibration
## Compute the mean squared error
mean((p1 - test$Clicked.on.Ad)^2)
mean((p2 - test$Clicked.on.Ad)^2)

## Classification
## https://en.wikipedia.org/wiki/Precision_and_recall
summary(p1)
binary_pred_logit <- (p1 > 0.5) + 0
tab <- table(pred=binary_pred_logit, true=test$Clicked.on.Ad)
tab

## accuracy
sum(diag(tab))/sum(tab) ## accuracy
pred_1 <- prediction(p1, labels = test$Click)
perf_1 <- performance(pred_1, measure = "acc")
plot(perf_1)
## Problems??

## true positive rate, recall: correctly predicted positive/total true positive
tab[2,2]/(tab[1,2] + tab[2,2]) 
plot(performance(pred_1, measure = "tpr"))


## Positive predictive value, precision: true positive/total predicted positive
tab[2,2]/(tab[2,1] + tab[2,2]) 
plot(performance(pred_1, measure = "prec"), add=TRUE, col = "red")

## F1 score - harmonic mean of precision and recall
plot(performance(pred_1, measure = "f"), add=TRUE, col = "blue")
