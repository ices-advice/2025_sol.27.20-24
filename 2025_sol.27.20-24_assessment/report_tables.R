
mkdir("report")

source("common.R")

load("model/fit.rData")
load("model/forecast.RData")
load("model/retro_fit.rData")

basefit<-NULL
if(file.exists("model/fit.RData")){
  local({load("model/fit.RData"); basefit<<-fit})
}else{
  basefit <- fit
}
fits <- c(base=basefit,current=fit)

years <- unique(fit$data$aux[, "year"])


tsb<-tsbtable(fit)
colnames(tsb)<-c("TSB","Low", "High")
tab.summary <- cbind(summary(fit), tsb)
write.taf(tab.summary, "report/Table 1. Estimated recruitment, spawning stock biomass (SSB).csv")

ftab <- faytable(fit)
write.taf(ftab, "report/Table 2. Estimated fishing mortality at age.csv")

ntab <- ntable(fit)
write.taf(ntab, "report/Table 3. Estimated stock numbers at age.csv")

ptab <- partable(fit)
write.taf(ptab, "report/Table 4. Table of model parameters.csv")

mtab <- modeltable(c(Current=fit, base=basefit))
write.taf(mtab, "report/Table 5. Model fitting.csv")

sdState<-function(fit, y=max(fit$data$years)-1:0){
  idx <- names(fit$sdrep$value) == "logR"
  sdLogR<-fit$sdrep$sd[idx][fit$data$years%in%y]
  idx <- names(fit$sdrep$value) == "logssb"
  sdLogSSB<-fit$sdrep$sd[idx][fit$data$years%in%y]
  idx <- names(fit$sdrep$value) == "logfbar"
  sdLogF<-fit$sdrep$sd[idx][fit$data$years%in%y]
  ret<-cbind(sdLogR, sdLogSSB, sdLogF)
  rownames(ret)<-y
  colnames(ret)<-c("sd(log(R))", "sd(log(SSB))", "sd(log(Fbar))")
  return(ret)
}

sdtab <- sdState(fit)
write.taf(mtab, "report/Table 6. Table of selected sd.csv")

nftab<-function(forecast){
  mat<-t(sapply(forecast, function(yr)exp(apply(yr$sim,2,median))))
  minA<-attr(forecast, "fit")$conf$minAge
  maxA<-attr(forecast, "fit")$conf$maxAge
  matN<-mat[,(minA:maxA)-minA+1]
  matF<-mat[,-c((minA:maxA)-minA+1)]  
  yr<-sapply(forecast,function(f)f$year)
  colnames(matN)<-paste0("N",minA:maxA)
  idx <- attr(forecast,"fit")$conf$keyLogFsta[1, ] + 2
  matF <- cbind(NA, matF)[, idx]
  matF[, idx == 1] <- 0
  colnames(matF)<-paste0("F",minA:maxA)
  res<-cbind(matN,matF)
  rownames(res)<-yr
  return(res)
}

## retro
tailtable <- function(fits, what=ssbtable, years=NULL){
  if(!is.null(attr(fits, "fit"))) {
    fits[[length(fits) + 1]] <- attr(fits, "fit")
    fits <- fits[c(length(fits), 1:(length(fits) - 1))]
  }
  if(is.null(years)){
    years <- max(fits[[1]]$data$years)-c(0:4)
  } 
  res <- lapply(fits,function(x){xx<-what(x); xx[rownames(xx)%in%years,1,drop=FALSE]})
  res <- outer(1:length(res),years, FUN=Vectorize(function(x,y){xx<-res[[x]]; ifelse(any(y%in%rownames(xx)),xx[rownames(xx)==y],NA)}))
  rownames(res) <- paste0("M", 1:nrow(res))
  colnames(res) <- years
  res
}

ttab1<-tailtable(retro_fit,what=ssbtable)
write.taf(ttab1, "report/Table 7a. SSB from retro analysis.csv")

ttab2<-tailtable(retro_fit,what=fbartable)
write.taf(ttab2, "report/Table 7b. Fbar from retro analysis.csv")


## forcast 
if(exists("FC")){  
  ii<-0
  lapply(FC, function(f){
    ii<<-ii+1;
    tf<-attr(f,"tab");
    dec<-c(3,3,3,rep(0,ncol(tf)-3))
    lbl <- gsub(", |/|\\*", "_", attr(f,"label"))
    write.taf(tf, paste0('report/Forecast table ',ii, "_", lbl, '.csv'))
  })
  
  
  
  ii=ii+1   
  nft<-nftab(FC[[length(FC)]])
  dec<-ifelse(colMeans(nft)>10,0,3)
  write.taf(nft, paste0('report/Forecast table ',ii, "_N and F for option.csv"))
  write.taf(Pabove, paste0('report/Forecast table ',ii, "Probability above Blim.csv"))

  
  colMedian<-function(X){
    apply(X, 2,median)
  }
  
  fore<-FC[[length(FC)]]
  years<-sapply(fore,function(x)x$year)
  Nstr<-paste0("N",colnames(ntable(fit)))
  Fstr<-paste0("F",sapply(sapply(0:max(fit$conf$keyLogFsta), function(k)((fit$conf$minAge):(fit$conf$maxAge))[which(fit$conf$keyLogFsta[1,]==k)]),paste,collapse=","))
  states<-t(sapply(fore,function(x)colMedian(exp(x$sim))))
  rownames(states)<-years
  colnames(states)<-c(Nstr,Fstr)
  write.taf(states, paste0("report/Table Xd. Forecasts.csv"))
     
}  


