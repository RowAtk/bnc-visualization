rm(list = ls())

# packages
# -----------------------------------------
# install.packages('dataQualityR')
library('dataQualityR')
# -----------------------------------------


bnc.loan.data = read.csv(
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
    data = bnc.loan.data, 
    out.file.num= num.file, 
    out.file.cat= cat.file
  )
  
  dq.num = read.csv(file = num.file, sep = ',')
  View(dq.num)
  
  dq.cat = read.csv(file = cat.file, sep = ',')
  View(dq.cat)
}

View(bnc.loan.data)
summary(bnc.loan.data)

bnc.loan.data$NPS = NULL

summary(bnc.loan.data)

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
bnc.loan.data = bnc.loan.data[!is.na(bnc.loan.data$RefNum),]

# Age range (cat)
# N/A: 0, outliers: F

# age (num)
# N/A: 0, outliers: F

# Job (cat)
# N/A: 7, outlier: F
bnc.loan.data = removeNaRows(bnc.loan.data, bnc.loan.data$job)
#bnc.loan.data = bnc.loan.data[!is.na(bnc.loan.data$job),]

# marital (cat)
# N/A: 0, outliers: F

# education (cat)
# N/A: 98, outliers: F
new_level = "unspecified"
levels(bnc.loan.data$education) = c(levels(bnc.loan.data$education), new_level)
bnc.loan.data$education[is.na(bnc.loan.data$education)] = new_level

# balance 
# N/A: 0, outliers: T
boxplot(bnc.loan.data$balance)

# deposit
# N/A: 17.35%, outliers: T
boxplot(bnc.loan.data$deposit)
bnc.loan.data = removeNaRows(bnc.loan.data, bnc.loan.data$deposit)

# housing, loan, month, date, lead, product, qualified, won (cat)
# N/A: 0, outliers: F

# duration
# N/A: 0, outliers:
boxplot(bnc.loan.data$duration)

# contacted
# N/A: 20%, outliers: F
# missing values replaced with 0
bnc.loan.data$contacted[is.na(bnc.loan.data$contacted)] = 0 

# loan value
# N/A: 17%, outliers: F
boxplot(bnc.loan.data$loanvalue)

data = bnc.loan.data[is.na(bnc.loan.data$loanvalue),]
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
bnc.loan.data$loanvalue[is.na(bnc.loan.data$loanvalue) & bnc.loan.data$housing=='no' & bnc.loan.data$loan=='no'] = 0

# find mean for records w/ housing alone, loan alone and both
hmean = round(mean(bnc.loan.data$loanvalue[bnc.loan.data$housing == 'yes' & bnc.loan.data$loan == 'no'], na.rm = TRUE), digits=2)
lmean = round(mean(bnc.loan.data$loanvalue[bnc.loan.data$loan == 'yes' & bnc.loan.data$housing == 'no'], na.rm = TRUE), digits=2)
hlmean = round(mean(bnc.loan.data$loanvalue[bnc.loan.data$loan == 'yes' & bnc.loan.data$housing == 'yes'], na.rm = TRUE), digits=2)

# replace na values for loanvalue with respective mean
bnc.loan.data$loanvalue[is.na(bnc.loan.data$loanvalue) & bnc.loan.data$housing == 'yes' & bnc.loan.data$loan == 'no'] = hmean
bnc.loan.data$loanvalue[is.na(bnc.loan.data$loanvalue) & bnc.loan.data$loan == 'yes' & bnc.loan.data$housing == 'no'] = lmean
bnc.loan.data$loanvalue[is.na(bnc.loan.data$loanvalue) & bnc.loan.data$housing == 'yes' & bnc.loan.data$loan == 'yes'] = hlmean

nrow(bnc.loan.data[!complete.cases(bnc.loan.data),])
summary(bnc.loan.data)

dataQuality()

