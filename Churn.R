
# IBM Churn Extra Assignment Submission

# Importing packages
library('caret')
library('ellipse')
library('psych')
library('tidyr')
library('ggplot2')

# Importing data
load(url("http://sensational-data.com/d/ibmchurn.rda"))

# Split data into train and test into 80%, 20%
set.seed(12345) # For reproducibility

id <- sample(1:2, nrow(ibmchurn), replace = TRUE, 
             prob = c(0.8, 0.2))

# Splitting in training 
trainingSet <- subset(ibmchurn[id == 1, ], select = -c(CustomerID))
testSet  <- subset(ibmchurn[id == 2, ], select = -c(CustomerID))

# Getting rid of rows containing missing values (should be only 11)
trainingSet <- trainingSet %>% drop_na()

# Training model
control <- trainControl(method = 'cv', number = 10, verboseIter = TRUE)
metric <- 'Accuracy'

# LDA: Linear Discriminant Analysis
fit.lda <- train(Churn~.,
                  data = trainingSet,
                  method = 'lda',
                  metric = metric,
                  trControl = control,
                  na.action = na.pass)

# DT: Decision Tree
fit.dt <- train(Churn~.,
                  data = trainingSet,
                  method = 'rpart',
                  metric = metric,
                  trControl = control,
                  na.action = na.pass)

# KNN: K-nearest neighbour
fit.knn <- train(Churn~.,
                 data = trainingSet,
                 method = 'knn',
                 metric = metric,
                 trControl = control,
                 na.action = na.pass)

# SVM: Support Vector Machine
fit.svm <- train(Churn~.,
                 data = trainingSet,
                 method = 'svmRadial',
                 metric = metric,
                 trControl = control,
                 na.action = na.pass)

# Random Forest
fit.rf <- train(Churn~.,
                data = trainingSet,
                method = 'rf',
                metric = metric,
                trControl = control,
                na.action = na.pass)

# Naive Bayes
fit.nbayes <- train(Churn~.,
                data = trainingSet,
                method = 'nb',
                metric = metric,
                trControl = control,
                na.action = na.pass)

# Neural Net
fit.nn <- train(Churn~.,
                data = trainingSet,
                method = 'nnet',
                metric = metric,
                trControl = control,
                na.action = na.pass)

# Anlayzing results
results <- resamples(list(
  lda = fit.lda,
  dt = fit.dt,
  knn = fit.knn,
  svm = fit.svm,
  rf = fit.rf,
  nn = fit.nn,
  nbayes = fit.nbayes
))

# Plotting Accuracy and Kappa accross different models
dotplot(results)

# Computing Precision & Recall for each model
computePrecisionRecall <- function(mod) {
  pred <- predict(mod, testSet)
  prec <- precision(pred, testSet$Churn)
  rec <- recall(pred, testSet$Churn)
  
  print(paste('  Model: ', mod[1]))
  print(paste('  Accuracy: ', round(max(mod$results$Accuracy), digits = 3)))
  print(paste('  Precision: ', round(prec, digits = 3)))
  print(paste('  Recall: ', round(rec, digits = 3)))
  print('- - - - - - - - -')
}

# Printing stats for each model
for (model in list(fit.lda, fit.dt, fit.knn, fit.svm, fit.rf, fit.nbayes, fit.nn)) {
  computePrecisionRecall(model)
}

# See Console for Accuracy, Precision & Recall for each individual model

# - Some Thoughts -
# As can be seen in the plot above, models are very close in accuracy, except for Naive Bayes.
# Given that the dataset is skewed towards "No Churn", the general accuracy is only slightly better
# than always guessing "No Churn". The neural network seems to be the best option with this particular seed,
# but since initializing the weigths and parameters randomly is always a hit or miss, the ranking of the
# models might change with a different seed. As far as we read on the internet, the kappa metric is a measure
# of a result's independence from it's underlying observations, but we are not sure we understood thid correctly
# and would appreciate some more input on this one.

