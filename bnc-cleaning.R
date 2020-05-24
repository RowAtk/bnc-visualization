rm(list = ls())

# packages
# -----------------------------------------
# install.packages('dataQualityR')
library('dataQualityR')
# -----------------------------------------


bnc.loan = read.csv(
  file = 'bnc-loan-ds.csv',
  stringsAsFactors = TRUE,
  header = TRUE,
  sep = ',',
  na.strings = c("", "NA")
)

dataQuality = function () {
  num.file <- "dq_num.csv"
  cat.file <- "dq_cat.csv"
  
  checkDataQuality(
    data = bnc.loan, 
    out.file.num= num.file, 
    out.file.cat= cat.file
  )
  
  dq.num = read.csv(file = num.file, sep = ',')
  View(dq.num)
  
  dq.cat = read.csv(file = cat.file, sep = ',')
  View(dq.cat)
}

View(bnc.loan)
summary(bnc.loan)

bnc.loan$NPS = NULL

summary(bnc.loan)

# KPIs
# - balances
# - active deposit activity in the last 3 years
# - don't currently have loans. 

# For the persons who already have loans:
# - age, profession, marital status & education.

removeNaRows = function(data, col) {
  return (data[!is.na(col),])
}
# missing values
# RefNum 
bnc.loan = bnc.loan[!is.na(bnc.loan$RefNum),]

# Age range (cat)
# N/A: 0, outliers: F

# age (num)
# N/A: 0, outliers: F

# Job (cat)
# N/A: 7, outlier: F
bnc.loan = removeNaRows(bnc.loan, bnc.loan$job)
#bnc.loan = bnc.loan[!is.na(bnc.loan$job),]

# marital (cat)
# N/A: 0, outliers: F

# education (cat)
# N/A: 98, outliers: F
new_level = "unspecified"
levels(bnc.loan$education) = c(levels(bnc.loan$education), new_level)
bnc.loan$education[is.na(bnc.loan$education)] = new_level

# balance 
# N/A: 0, outliers: T
boxplot(bnc.loan$balance)

# deposit
# N/A: 17.35%, outliers: T
boxplot(bnc.loan$deposit)
bnc.loan = removeNaRows(bnc.loan, bnc.loan$deposit)

# housing, loan, month, date, lead, product, qualified, won (cat)
# N/A: 0, outliers: F

# duration
# N/A: 0, outliers:
boxplot(bnc.loan$duration)

# contacted
# N/A: 20%, outliers: F
# missing values replaced with 0
bnc.loan$contacted[is.na(bnc.loan$contacted)] = 0 

# loan value
# N/A: 17%, outliers: F
boxplot(bnc.loan$loanvalue)

data = bnc.loan[is.na(bnc.loan$loanvalue),]
loanview = data.frame(
  product = data$product,
  housing = data$housing,
  loan = data$loan,
  loanvalue = data$loanvalue
)
View(loanview)
lost_cause = nrow(loanview[loanview$housing=='no' & loanview$loan=='no' & loanview$product=='no product',])
percent = lost_cause/nrow(loanview)

View(loanview[loanview$housing=='no' & loanview$loan=='no' & loanview$product=='no product',])

# give customers with no loan a loan value of 0, where loanvalue = N/A
bnc.loan$loanvalue[is.na(bnc.loan$loanvalue) & bnc.loan$housing=='no' & bnc.loan$loan=='no'] = 0

# find mean for records w/ housing alone, loan alone and both
hmean = round(mean(bnc.loan$loanvalue[bnc.loan$housing == 'yes' & bnc.loan$loan == 'no'], na.rm = TRUE), digits=2)
lmean = round(mean(bnc.loan$loanvalue[bnc.loan$loan == 'yes' & bnc.loan$housing == 'no'], na.rm = TRUE), digits=2)
hlmean = round(mean(bnc.loan$loanvalue[bnc.loan$loan == 'yes' & bnc.loan$housing == 'yes'], na.rm = TRUE), digits=2)

# replace na values for loanvalue with respective mean
bnc.loan$loanvalue[is.na(bnc.loan$loanvalue) & bnc.loan$housing == 'yes' & bnc.loan$loan == 'no'] = hmean
bnc.loan$loanvalue[is.na(bnc.loan$loanvalue) & bnc.loan$loan == 'yes' & bnc.loan$housing == 'no'] = lmean
bnc.loan$loanvalue[is.na(bnc.loan$loanvalue) & bnc.loan$housing == 'yes' & bnc.loan$loan == 'yes'] = hlmean


# convert numeric variables to factors
# loan, housing, won, lead, qualified, contacted
bnc.loan$loan = as.factor(bnc.loan$loan)
bnc.loan$housing = as.factor(bnc.loan$housing)
bnc.loan$won = as.factor(bnc.loan$won)
bnc.loan$lead = as.factor(bnc.loan$lead)
bnc.loan$qualified = as.factor(bnc.loan$qualified)
bnc.loan$contacted = as.factor(bnc.loan$contacted)

# month -convert to quarters
dates = as.Date(bnc.loan$date, format = "%m/%d/%Y")
mquarters =quarters(dates)
bnc.loan$quarter = as.factor(mquarters)

nrow(bnc.loan[!complete.cases(bnc.loan),])
summary(bnc.loan)

dataQuality()
