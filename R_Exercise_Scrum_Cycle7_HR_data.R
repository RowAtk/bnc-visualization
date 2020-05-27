# Classification Tree with rpart
#packages required
#install.packages("rpart")
#install.packages("caTools")
#install.packages("pROC")

library(rpart)  #CART
library(rpart.plot)
library(caTools)
library(pROC)

###STEP 1 - LOADING data2
data2 = read.csv(file = 'HR.csv')
str(data2)
summary(data2)
data2$left <- as.factor(data2$left)
str(data2)

###STEP 2 - SPLIT data2 INTO TRAIN and TEST - Stratified Sampling
set.seed(123)
newdata2set <-sample.split(Y=data2$left, SplitRatio = 0.7)
traindata2 <- data2[newdata2set,]
testdata2 <- data2[!newdata2set,]

###STEP 3 - BUILD THE MODEL - Fit a DT using Training data2
DTmodel<- rpart(left ~ ., method = "class", data = traindata2,
                parms = list (split ="information gain"), 
                control = rpart.control(minsplit = 100, maxdepth = 4))  

rpart.plot(DTmodel, type = 3, extra = 101, fallen.leaves = F, cex = 0.6) #extra 2 4 8 101
DTmodel

###STEP 4 - USE THE MODEL TO MAKE PREDICTIONS ON TEST data2
predTest <- predict(DTmodel, testdata2, type = "class")
probTest <- predict(DTmodel, testdata2, type = "prob")
actualTest <- testdata2$left

#Print 10 records
actualTest[1:10]
predTest[1:10]

#Add variables to Test data2
testdata2$Actual <- actualTest
testdata2$Predictions <- predTest
testdata2$Probability1 <- probTest[,2] #2nd column of the variable probTest 
testdata2$Probability0 <- probTest[,1] #1st column of the variable probTest 
testdata2$Probability <- ifelse(testdata2$Probability0 > testdata2$Probability1, testdata2$Probability0,testdata2$Probability1)

###STEP 5 - CALCULATE THE ACCURACY
t1 <- table(Predicted_Value = predTest, Actual_Value = actualTest)
t1
accuracy1 <- sum(diag(t1))/sum(t1)
accuracy1

### ROC and Area Under the Curve 
ROC <- roc(actualTest, probTest[,2])
plot(ROC, col="blue")
AUC <- auc(ROC)
AUC
