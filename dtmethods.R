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
  data.train = oversample(train, ratio = ratio, method = "SMOTE", classAttr = classAttr)
}

# ADSN

adsnsample = function (train, ratio, classAttr) {
  imbalanceRatio(train, classAttr = classAttr)
  data.train = oversample(train, ratio = ratio, method = "ADSN", classAttr = classAttr)
}

##################################
#        MEASURING MODELS        #
##################################

# ACCURCY
measure.acc = function (actual, predicted) {
  t1 = table(Actual_Value = actual, Predicted_Value = predicted)
  print(t1)
  View(t1)
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
  a = t1[2,2]
  b = t1[2,1]
  spec = a / (a+b)
  return(spec)
}

# ROC
measure.roc = function (actual, prob) {
  ROC = roc(actual, data.test.prob[,2])
  plot(ROC, col="purple")
  AUC <- auc(ROC)
  AUC
}


