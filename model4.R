# packages 
library(rpart)  #CART
library(rpart.plot)
library(caTools)
library(pROC)
library(imbalance)

data = bnc.loan
data = data.frame(
  education = data$education,
  age = data$age,
  marital = data$marital,
  job = data$job,
  deposit = data$deposit,
  balance = data$balance,
  lead = data$lead
)
summary(data)

# split data into train and test
set.seed(929)
newset = sample.split(Y=data$lead, SplitRatio = 0.7)
data.train = data[newset,]
data.test = data[!newset,]

##################################
#   Build Decision Tree Model    #
##################################
DTmodel1 = rpart(
  lead ~ ., 
  method = "class", 
  data = data.train,
  parms = list (split ="information"),
  control = rpart.control(maxdepth = 10)
)

# plot model
dtplot(DTmodel1)

##################################
#   Make predictions w/ Model    #
##################################

# prediction
data.test.pred = predict(DTmodel1, data.test, type = "class")

# probability
data.test.prob = predict(DTmodel1, data.test, type = "prob")

actual = data.test$lead
results = data.frame(actual = actual, prediction = data.test.pred, probability = prob.dominant(data.test.prob))
View(results)

##################################
#   Measure Model Performance    #
##################################
# accuracy
measure.acc(actual = actual, predicted = data.test.pred)

# sensitivity
measure.sens(actual = actual, predicted = data.test.pred)
measure.spec(actual = actual, predicted = data.test.pred)

# ROC
measure.roc(actual, data.test.prob)


