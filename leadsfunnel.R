# leads Data
leads.data = data.frame(leads = c(0,0,0,0), leadstage=c("leads", "qualified", "contacted", "won") )

# add row by row

leads.data[1,1] = nrow(bnc.loan[bnc.loan$lead == 1,])
leads.data[2,1] = nrow(bnc.loan[bnc.loan$qualified == 1,])
leads.data[3,1] = nrow(bnc.loan[bnc.loan$contacted == 1,])
leads.data[4,1] = nrow(bnc.loan[bnc.loan$won == 1,])
View(leads.data)
