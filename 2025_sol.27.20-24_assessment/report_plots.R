
library(icesTAF)
library(stockassessment)

mkdir("report")

source("common.R")

load("model/fit.rData")
load("model/retro_fit.rData")
load("model/leaveout.rData")
load("model/residuals.rData")
load("model/forecast.RData")

basefit<-NULL
if(file.exists("model/fit.RData")){
  local({load("model/fit.RData"); basefit<<-fit})
}else{
  basefit <- fit
}
fits <- c(base=basefit,current=fit)

exfitname <- scan("boot/data/sam_config/viewextra.cfg", what="", comment.char="#", quiet=TRUE)
for(nam in exfitname){
  local({
    fit<-urlLoadFit(paste0("https://www.stockassessment.org/datadisk/stockassessment/userdirs/user3/",nam,"/run/model.RData"))
    if(!is.null(fit)){
      i <- length(fits)
      fits[[i+1]] <<- fit
      names(fits)[i+1] <<- nam
    }else{
      warning(paste0("View extra stock ", nam, " not found or of incompatible format (skipped)"))
    }
  })
}

plotcounter<-1
tit.list<-list()

## input data plots

## specieal


srlagplot<-function (fit, textcol = "red", add = FALSE, ...) {
  X <- summary(fit)
  n <- nrow(X)
  lag <- fit$conf$minAge+1
  idxR <- (lag + 1):n
  idxS <- 1:(n - lag)
  R <- rectable(fit, lag=TRUE)[idxR, 1]
  S <- X[idxS, 4]
  Rnam <- paste0("R(age ",lag,")")
  Snam <- colnames(X)[4]
  y <- rownames(X)
  if (add) {
    lines(S, R)
  }
  else {
    plot(S, R, xlab = Snam, ylab = Rnam, type = "l", xlim = range(0, S), ylim = range(0, R), ...)
  }
  text(S, R, labels = y[idxR], cex = 0.7, col = textcol)
}

sdplot<-function(fit){
  cf <- fit$conf$keyVarObs
  fn <- attr(fit$data, "fleetNames")
  ages <- fit$conf$minAge:fit$conf$maxAge
  pt <- partable(fit)
  sd <- unname(exp(pt[grep("logSdLogObs",rownames(pt)),1]))
  v<-cf
  v[] <- c(NA,sd)[cf+2]
  res<-data.frame(fleet=fn[as.vector(row(v))],name=paste0(fn[as.vector(row(v))]," age ",ages[as.vector(col(v))]), sd=as.vector(v))
  res<-res[complete.cases(res),]
  o<-order(res$sd)
  res<-res[o,]
  par(mar=c(10,6,2,1))
  barplot(res$sd, names.arg=res$name,las=2, col=colors()[as.integer(as.factor(res$fleet))*10], ylab="SD"); box()
}

## model output plots ##
taf.png("summary", width = 1600, height = 2000)
plot(fit)
dev.off()

taf.png("Spawning stock biomass")
ssbplot(fits, addCI = TRUE)
dev.off()

taf.png("Average fishing mortality")
fbarplot(fits, addCI=TRUE)
dev.off()

taf.png("Recruitment")
recplot(fits, addCI=TRUE, las=0)
dev.off()

taf.png("Catch")
catchplot(fits, addCI=TRUE)
dev.off()

taf.png("Spawner-resruits", width = 1600, height = 2000)
srplot(fit)
dev.off()

taf.png("spacner-recruite2", width = 1600, height = 2000)
sdplot(fit)
dev.off()

taf.png("Partial F", width = 1600, height = 2000)
xF<-faytable(fit)
matplot(rownames(xF),xF,lty=1:ncol(xF),col=1:ncol(xF), type='l', xlab='Year', lwd=5)
dev.off()

taf.png("Yield per Recruit", width = 1600, height = 2000)
plot(ypr(fit))
dev.off()

#----
taf.png("Selection pattern")

sel <- t(faytable(fit)/fbartable(fit)[,1])
sel[is.na(sel)]<-0
op <- par(mfrow=c(3,3), mai=c(0.4,0.5,0.2,0.2))
age.sel<-as.integer(rownames(sel))
for(i in round(seq(1,dim(sel)[2],length=9))){
  plot(age.sel, sel[,i], type="l", xlab="", ylab="", lwd=1.5, ylim=c(0,max(sel)))
  if (i+1<dim(sel)[2])try(lines(age.sel, sel[,i+1], col="red", lwd=1.5))
  if (i+2<dim(sel)[2]) try(lines(age.sel, sel[,i+2], col="blue", lwd=1.5))
  if (i+3<dim(sel)[2]) try(lines(age.sel, sel[,i+3], col="green", lwd=1.5))
  if (i+4<dim(sel)[2]) try(lines(age.sel, sel[,i+4], col="blue3", lwd=1.5))
  legend("bottomright",paste(c(colnames(sel)[i],colnames(sel)[i+1],colnames(sel)[i+2],colnames(sel)[i+3])), lty=rep(1,4), col=c("black","red","blue","green"),bty="n")
}
mtext("Age", 1, outer=T, line=1)
mtext("F/Fbar", 2, outer=T, line=1)
par(op)
dev.off()


taf.png("Selection pattern2")
mn.sel <- apply(sel,1,mean)
sd.sel <- apply(sel,1,quantile,probs=c(0.025,0.975))
plot(age.sel, mn.sel, ylim=c(0,max(sd.sel)), type="l", xlab='Age', ylab="F/Fbar", lwd=2)
for(i in 1:length(age.sel)){
  lines(rep(age.sel[i],2), sd.sel[,i])
}
dev.off()

####
if(!all(fit$conf$obsCorStruct=="ID")){ 
  taf.png("Estimated correlations", width = 1600, height = 2000)
  corplot(fit)			  
  dev.off()
}

for(f in 1:fit$data$noFleets){
  taf.png(paste0("Fit to data_", f), width = 1600, height = 2000)
  fitplot(fit, fleets=f)
  dev.off()
}


taf.png("Fit to catch weight-at-age", width = 1600, height = 2000)
CW<-exp(fit$pl$logCW)[,,1]
matplot(fit$dat$catchMeanWeight[,,1], xaxt = "n")
axis(1,at=1:length(fit$data$years), labels=fit$data$years)
matplot(CW, type="l", add=T)
dev.off()

#----

taf.png("One-observation-ahead residuals", width = 1600, height = 2000)
plot(resids)
par(mfrow=c(1,1))
dev.off()

taf.png("Process residuals", width = 1600, height = 2000)
plot(resids_process)
par(mfrow=c(1,1))
dev.off()

taf.png("Leaveout (SSB)", width = 1600, height = 2000)
ssbplot(leaveout_fits)
dev.off()

taf.png("Leaveout (Average F)", width = 1600, height = 2000)
fbarplot(leaveout_fits)
dev.off()

taf.png("Leaveout (Recruitment)", width = 1600, height = 2000)
recplot(leaveout_fits)
dev.off()

taf.png("Leaveout (Catch)", width = 1600, height = 2000)
catchplot(leaveout_fits)
dev.off()

##
taf.png("retro_fit", width = 1600, height = 2000)
plot(retro_fit)
par(mfrow=c(1,1))
dev.off()

rho <- mohn(retro_fit)

taf.png("Retrospective (SSB)", width = 1600, height = 2000)
ssbplot(retro_fit, las=0, drop=0)
legend("topright", legend=paste0("Rho ",round(rho[2]*100),"%"))
dev.off()

taf.png("Retrospective (Average F)", width = 1600, height = 2000)
fbarplot(retro_fit, las=0, drop=0)
legend("topright", legend=paste0("Rho ",round(rho[3]*100),"%"))
dev.off()

taf.png("Retrospective (Recruitment)", width = 1600, height = 2000)
recplot(retro_fit, las=0, drop=0)
legend("topright", legend=paste0("Rho ",round(rho[1]*100),"%"))
dev.off()

taf.png("Retrospective (Catch)", width = 1600, height = 2000)
catchplot(retro_fit)
dev.off()

if(exists("FC")){  
  lapply(FC, function(f){
    lbl <- gsub(", |/|\\*", "_", attr(f,"label"))
    lbl <- gsub("\\%", "pct", lbl)
    lbl <- gsub("\\+", "_plus_", lbl)
    taf.png(paste(lbl), width = 1600, height = 2000)
    plot(f)
    dev.off()
    })
}  
