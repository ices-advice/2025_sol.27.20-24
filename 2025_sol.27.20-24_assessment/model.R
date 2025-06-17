## Run analysis, write model results

## Before:
## After:

library(icesTAF)

mkdir("model")

#download standard help scripts
sam_assessment <- "Sole20_24_2025_new_indicesCV_conf"

sam_dir <-
  paste0(
    "https://stockassessment.org/datadisk/stockassessment/userdirs/user3/",
    sam_assessment,
    "/src/"
  )

files <- "common.R"

for (file in files) {
  download(paste0(sam_dir, file))
}

##

source.taf("data.R")
source.taf("model_fit.R")
source.taf("model_forecast.R")
source.taf("output.R")
source.taf("report_plots.R")
source.taf("report_tables.R")


