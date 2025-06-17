## Extract results of interest, write TAF output tables

## Before:
## After: csv tables of assessment output

library(icesTAF)
library(stockassessment)

mkdir("output")

# load fit
load("model/fit.rData", verbose = TRUE)
load("model/retro_fit.rData", verbose = TRUE)

# Model Parameters
partab <- partable(fit)

# Fs
fatage <- faytable(fit)
fatage <- as.data.frame(fatage)

# Ns
natage <- as.data.frame(ntable(fit))

# Catch
catab <- as.data.frame(catchtable(fit))
colnames(catab) <- c("Catch", "Low", "High")

# TSB
tsb <- as.data.frame(tsbtable(fit))
colnames(tsb) <- c("TSB", "Low", "High")

# Summary Table
tab.summary <- cbind(as.data.frame(summary(fit)), tsb)
tab.summary <- tab.summary[-nrow(tab.summary), ]
# should probably make Low and High column names unique R_Low etc.

mohns_rho <- mohn(retro_fit)
mohns_rho <- as.data.frame(t(mohns_rho))

## Write tables to output directory
write.taf(
  c("partab", "tab.summary", "natage", "fatage", "mohns_rho"),
  dir = "output"
)


