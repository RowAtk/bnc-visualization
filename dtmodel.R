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
nrow(newdata[newdata$lead == 0,])
nrow(newdata[newdata$lead == 1,])
#newds = sample.split(Y=data$lead, SplitRatio = 1)

# split into train and test
newDataset <-sample.split(Y=newdata$lead, SplitRatio = 0.6)
trainSet <- data[newDataset,]
testSet <- data[!newDataset,]

nrow(trainSet[trainSet$lead == 0,])
nrow(trainSet[trainSet$lead == 1,])

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

# make predictions
test.pred = testSet$lead

# DTMODEL 1
predTest1 <- predict(DTmodel1, testSet, type = "class")
probTest1 <- predict(DTmodel1, testSet, type = "prob")
results1 = table(actual = test.pred, prediction = predTest1)
View(results1)

nrow(trainSet[trainSet$lead == 0,])
nrow(trainSet[trainSet$lead == 1,])

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

