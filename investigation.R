# lost causes
lc = bnc.loan[bnc.loan$housing == 'no' & bnc.loan$loan == 'no' & bnc.loanproduct == 'no product',]
View(lc)


# business loan people
bloan = bnc.loan[bnc.loan$product == 'business',]
View(bloan)
summary(bloan)

nrow(bnc.loan[bnc.loan$lead == 0, ]) / nrow(bnc.loan[bnc.loan$lead == 1, ])
