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
#data$lead = as.factor(data$lead)

# split data into train and test
set.seed(929)
newset = sample.split(Y=data$lead, SplitRatio = 0.7)
data.train = data[newset,]
data.test = data[!newset,]

# oversample
imbalanceRatio(data.train, "lead")
data.train = rsample(data.train)
#View(data.train)

#SMOTE
# data.train$education = as.numeric(as.factor(data.train$education))
# data.test$education = as.numeric(as.factor(data.test$education))

data.train$job = as.numeric(as.factor(data.train$job))
data.test$job = as.numeric(as.factor(data.test$job))

data.train$marital = as.numeric(as.factor(data.train$marital))
data.test$marital = as.numeric(as.factor(data.test$marital))

#View(data.train)
#summary(data.train)

# data.train = smotesample(data.train, 0.8, "lead")
imbalanceRatio(data.train, "lead")
#data.train = oversample(data.train, ratio = 0.9, method = "SMOTE", classAttr = "lead")

# ADSN
# data.train = adsnsample(data.train, 0.8, "lead")

# func to plot dt models
dtplot = function (model) {
  rpart.plot(model, type = 1, extra = 101, fallen.leaves = T, cex = 0.6) #extra 2 4 8 101
}

# build model
DTmodel1 = rpart(
  lead ~ ., 
  method = "class", 
  data = data.train,
  parms = list (split ="information")
)

dtplot(DTmodel1)

# make prediction
data.test.pred =  predict(DTmodel1, data.test, type = "class")
data.test.prob =  predict(DTmodel1, data.test, type = "prob")
actual = data.test$lead

results = data.frame(actual = actual, prediction = data.test.pred, probability = data.test.prob)
View(results)

# measure model performance
# accuracy
measure.acc(actual = actual, predicted = data.test.pred)

# sensitivity
measure.sens(actual = actual, predicted = data.test.pred)
measure.spec(actual = actual, predicted = data.test.pred)

# ROC
View(data.test.prob[,2])
View(data.test.prob)

measure.roc(actual, data.test.prob)


