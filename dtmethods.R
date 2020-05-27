##################################
#       OVERSAMPLING METHODS     #
##################################
# oversample minority class in training data
# SMOTE ADSYN RANDOM

# RANDOM - Double lead 0s (customized function)
rsample = function (train) {
  nrow(train)
  minority = train[train$lead == 0,]
  nrow(minority)
  train = rbind(train, minority)
  
  table(train$lead)
  imbalanceRatio(train, "lead")
  return(train)
}


# SMOTE
smotesample = function (train, ratio, classAttr) {
  imbalanceRatio(train, classAttr = classAttr)
  print(ratio)
  data.train = oversample(train, ratio = ratio, method = "SMOTE", classAttr = classAttr)
}

# ADASYN
adasynsample = function (train, ratio, classAttr) {
  imbalanceRatio(train, classAttr = classAttr)
  data.train = oversample(train, ratio = ratio, method = "ADASYN", classAttr = classAttr)
}

##################################
#        MEASURING MODELS        #
##################################

# ACCURCY
measure.acc = function (actual, predicted) {
  t1 = table(Actual_Value = actual, Predicted_Value = predicted)
  print(t1)
  acc = sum(diag(t1))/sum(t1)
  return(acc)
}

# Sensitivity
measure.sens = function (actual, predicted) {
  t1 = table(Actual_Value = actual, Predicted_Value = predicted)
  a = t1[1,1]
  b = t1[1,2]
  sens = a / (a+b)
  return(sens)
}

# Specifity
measure.spec = function (actual, predicted) {
  t1 = table(Actual_Value = actual, Predicted_Value = predicted)
  View(t1)
  d = t1[2,2]
  c = t1[2,1]
  spec = d / (d+c)
  return(spec)
}

# ROC
measure.roc = function (actual, prob) {
  ROC = roc(actual, data.test.prob[,2])
  plot(ROC, col="purple")
  AUC <- auc(ROC)
  AUC
}

##################################
#        Factor to Numeric       #
##################################

tonumeric = function(data, target) {
  for (col in colnames(data)) {
    if(col != target & class(data[[col]]) == "factor") {
      data[,col] = as.numeric(data[,col])
    }
  }
  return(data)
}

##################################
#        Plot DT Model           #
##################################
dtplot = function (model) {
  rpart.plot(model, type = 1, extra = 101, fallen.leaves = T, cex = 0.6) #extra 2 4 8 101
}


