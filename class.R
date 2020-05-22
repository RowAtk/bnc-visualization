library(rpart)
library(rpart.plot)
library(caTools)
#library(pROC)

# Step 1 - loading data
data = read.csv(file = 'HR.csv')
View(data)
summary(data)

nrow(data[data$left == 1,])/nrow(data)

# Step 2 - spliting data into train and test (stratified sampling)
set.seed(123)
newds = sample.split(Y = data$left, SplitRatio = 0.7)
data.train = data[newds,]
data.test = data[!newds,]

# Setp 3 - build model - Fit a DT using Training data
DTModel = rpart(
  left ~., 
  method = "class", 
  data = data.train, 
  parms = list(split = "information gain"),
  control = rpart.control(minsplit = 100, maxdepth = 4)
)

rpart.plot(DTModel, type = 4, extra = 101, fallen.leaves = T, cex = 0.7)

# Step 4 - use model to make predictions on test data
pred.test = predict(DTModel, data.test, type = "class")
prob.test = predict(DTModel, data.test, type = "class")
