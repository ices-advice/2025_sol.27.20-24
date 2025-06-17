## Preprocess data, write TAF data tables

## Before:
## After:

## load libraries
library(icesTAF)
library(stockassessment)
library(dplyr)

# ensure directory
mkdir("data")

#  Read underlying data from bootstrap/data

read.ices.taf <- function(file) {
  read.ices(
    taf.data.path("sam_data", file)
  )
}

cn<-read.ices.taf("cn.dat")
cw<-read.ices.taf("cw.dat")
dw<-read.ices.taf("dw.dat")
lw<-read.ices.taf("lw.dat")
mo<-read.ices.taf("mo.dat")
nm<-read.ices.taf("nm.dat")
pf<-read.ices.taf("pf.dat")
pm<-read.ices.taf("pm.dat")
sw<-read.ices.taf("sw.dat")
lf<-read.ices.taf("lf.dat")
surveys<-read.ices.taf("survey.dat")

soleSurveyCV <- read.table(taf.data.path("sam_data", "Sole2024-CV.csv"))
soleSurveyCV <- soleSurveyCV  / mean(as.matrix(soleSurveyCV),na.rm=TRUE)
soleSurveyCV <- c(soleSurveyCV[,]$X1, soleSurveyCV[,]$X2, soleSurveyCV[,]$X3, soleSurveyCV[,]$X4,
                  soleSurveyCV[,]$X5, soleSurveyCV[,]$X6, soleSurveyCV[,]$X7, soleSurveyCV[,]$X8,
                  soleSurveyCV[,]$X9)

attributes(surveys[[1]])$weight = (1/soleSurveyCV^2)

dat<-setup.sam.data(surveys=surveys,
                    residual.fleet=cn, 
                    prop.mature=mo, 
                    stock.mean.weight=sw, 
                    catch.mean.weight=cw, 
                    dis.mean.weight=dw, 
                    land.mean.weight=lw,
                    prop.f=pf, 
                    prop.m=pm, 
                    natural.mortality=nm, 
                    land.frac=lf)

save(dat, file = "data/data.RData")
