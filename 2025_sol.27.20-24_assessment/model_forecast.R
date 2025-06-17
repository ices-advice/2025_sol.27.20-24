library(stockassessment)
load("data/data.RData", verbose = TRUE)
load("model/fit.rData")

FC<-list()
set.seed(123456)
FC[[length(FC)+1]] <- forecast(fit, fval=c(NA,.175,.076, .076), fscale=c(NA,NA,NA,NA), catchval=c(208,NA,NA,NA), label="MSY approach, Fmsy lower*SSB2026/MSY Btrigger", rec.years=2022:2024, savesim=TRUE)

FC[[length(FC)+1]] <- forecast(fit, fval=c(NA,.175,.099, .099), fscale=c(NA,NA,NA,NA), catchval=c(208,NA,NA,NA), label="MSY approach, Fmsy upper*SSB2026/MSY Btrigger", rec.years=2022:2024, , savesim=TRUE)

FC[[length(FC)+1]] <- forecast(fit, fval=c(NA,.175,.099, .099), fscale=c(NA,NA,NA,NA), catchval=c(208,NA,NA,NA), label="Fmsy *SSB2026/MSY Btrigger", rec.years=2022:2024, savesim=TRUE)

FC[[length(FC)+1]] <- forecast(fit, fval=c(NA,.175,.182, .182), fscale=c(NA,NA,NA,NA), catchval=c(208,NA,NA,NA), label="Fmsy", rec.years=2022:2024, savesim=TRUE)

FC[[length(FC)+1]] <- forecast(fit, fval=c(NA,.175, NA, .182), fscale=c(NA,NA,NA,NA), catchval=c(208,NA,251,NA), label="TAC2024+20%", rec.years=2022:2024, savesim=TRUE)

FC[[length(FC)+1]] <- forecast(fit, fval=c(NA,.175 ,NA, NA), fscale=c(NA,NA,NA,NA), catchval=c(208,NA,0,0), label="zero catch", rec.years=2022:2024, savesim=TRUE)

FC[[length(FC)+1]] <- forecast(fit, fval=c(NA,.175,0.175, NA), fscale=c(NA,NA,NA,1), catchval=c(208,NA,NA,NA),label="other options, F=F2024", rec.years=2022:2024,       savesim=TRUE)

FC[[length(FC)+1]] <- forecast(fit, fval=c(NA,.175,NA,.175), fscale=c(NA,NA,NA,NA), catchval=c(208,NA,NA,NA),nextssb=c(NA,NA,1353,NA),label="SSB(2027)= SSB(2026)", rec.years=2022:2024, savesim=TRUE)

FC[[length(FC)+1]] <- forecast(fit, fval=c(NA,.175,.182, .182), fscale=c(NA,NA,NA,NA), catchval=c(208,NA,NA,NA), label="MAP range Fupper", rec.years=2022:2024, savesim=TRUE)

FC[[length(FC)+1]] <- forecast(fit, fval=c(NA,.175,.14, .14), fscale=c(NA,NA,NA,NA), catchval=c(208,NA,NA,NA), label="MAP range Flower", rec.years=2022:2024, savesim=TRUE)


Blim=2094
above<-function(x){xx<-mean(x[[4]]$ssb>Blim);yr<-x[[4]]$year; round(xx*100,1)}
Pabove<-lapply(FC, above)
names(Pabove)<-lapply(FC, function(x){attr(x,"label")})
Pabove<-as.matrix(unlist(Pabove))
colnames(Pabove)<-paste0("P(SSB",FC[[1]][[4]]$year, " > ",Blim,") in %")

save(FC, Pabove, file="model/forecast.RData")


