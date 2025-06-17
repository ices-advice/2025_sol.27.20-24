## Run analysis, write model results

## Before:
## After:

# load libraries
library(icesTAF)
library(stockassessment)

# ensure model dir exists
mkdir("model")

# load data required
load("data/data.RData", verbose = TRUE)

# setup configuration
conf <-
  loadConf(
    dat,
    taf.data.path("sam_config/model.cfg"),
    patch = TRUE
  )

# define parameters
par <- defpar(dat, conf)

# fit model
fit <- sam.fit(dat, conf, par)

# retrospective fits
retro_fit <- retro(fit, year = rev(tail(fit$data$years,5)))

# leave one out fits
leaveout_fits <- leaveout(fit, fleet = as.list(3:fit$data$noFleets))

# residuals
resids <- residuals(fit)
resids_process <- procres(fit)

# save model fits
save(fit, file = "model/fit.RData")
save(retro_fit, file = "model/retro_fit.RData")
save(leaveout_fits, file = "model/leaveout.RData")
save(resids, resids_process, file = "model/residuals.RData")
