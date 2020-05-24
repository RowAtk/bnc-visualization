# packages 
library(rpart)  #CART
library(rpart.plot)
library(caTools)
library(pROC)

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
View(data)

lead.neg = data[data$lead == 0,] 
lead.pos = data[data$lead == 1,] 

nrow(lead.neg) # 225
nrow(lead.pos) # 350

newdata = rbind(lead.neg[0:225,],lead.pos[0:200,])

#newds = sample.split(Y=data$lead, SplitRatio = 1)

# split into train and test
newDataset <-sample.split(Y=newdata$lead, SplitRatio = 0.7)
trainSet <- data[newDataset,]
testSet <- data[!newDataset,]

count = as.numeric(nrow(trainSet))
View(trainSet)
nrow(trainSet[])

dtplot = function (model) {
  rpart.plot(model, type = 1, extra = 101, fallen.leaves = T, cex = 0.6) #extra 2 4 8 101
}


# build and plot models
# find lead as func of education, age, marital status, profession, deposit, balance

# information gain
#####################
#msplit1 = as.numeric(round(0.01 * count, digits = 0))
DTmodel1 = rpart(lead ~ ., method = "class", data = trainSet,
                parms = list (split ="information gain"), 
                control = rpart.control(maxdepth = 6)) 
#
dtplot(DTmodel1)


#####################
msplit2 = as.numeric(round(0.02 * count, digits = 0))
DTmodel2 = rpart(lead ~ ., method = "class", data = trainSet,
                parms = list (split ="information gain"), 
                control = rpart.control(minsplit = msplit2, maxdepth = 4))  

dtplot(DTmodel2)

# gini
#####################
msplit1 = as.numeric(round(0.01 * count, digits = 0))
DTmodel3 = rpart(lead ~ ., method = "class", data = trainSet,
                parms = list (split ="gini"), 
                control = rpart.control(minsplit = msplit1, maxdepth = 4)) 

dtplot(DTmodel3)

#####################
msplit2 = as.numeric(round(0.02 * count, digits = 0))
DTmodel4 = rpart(lead ~ ., method = "class", data = trainSet,
                parms = list (split ="gini"), 
                control = rpart.control(minsplit = msplit2, maxdepth = 4)) 

dtplot(DTmodel4)


# make predictions
test.pred = testSet$lead

# DTMODEL 1
predTest1 <- predict(DTmodel1, testSet, type = "class")
probTest1 <- predict(DTmodel1, testSet, type = "prob")
results1 = table(actual = test.pred, prediction = predTest1)
View(results1)

# DTMODEL 2
predTest2 <- predict(DTmodel2, testSet, type = "class")
probTest2 <- predict(DTmodel2, testSet, type = "prob")
results2 = table(actual = test.pred, prediction = predTest2)
View(results2)

# DTMODEL 3
predTest3 <- predict(DTmodel3, testSet, type = "class")
probTest3 <- predict(DTmodel3, testSet, type = "prob")
results3 = table(actual = test.pred, prediction = predTest3)
View(results3)

nrow(trainSet[trainSet$lead == 0,])
nrow

nrow(data[data$lead == 0,])
nrow(data)

# calculate accuracy
# DTMODEL 1
accuracy1 = sum(diag(results1))/sum(results1)
accuracy1

ROC <- roc(test.pred, probTest1[,2])
plot(ROC, col="blue")
AUC <- auc(ROC)
AUC

# DTMODEL 2
accuracy2 = sum(diag(results2))/sum(results2)
accuracy2

ROC <- roc(test.pred, probTest2[,2])
plot(ROC, col="blue")
AUC <- auc(ROC)
AUC

# DTMODEL 3
accuracy3 = sum(diag(results3))/sum(results3)
accuracy3

ROC <- roc(test.pred, probTest3[,2])
plot(ROC, col="blue")
AUC <- auc(ROC)
AUC

