
mkdir("boot/data/sam_config")

sam_assessment <- "Sole20_24_2025_new_indicesCV_conf"

sam_dir <-
  paste0(
    "https://stockassessment.org/datadisk/stockassessment/userdirs/user3/",
    sam_assessment,
    "/conf/"
  )

files <- c("model.cfg", "viewextra.cfg")

wd <- getwd()
setwd(paste0(getwd(), "/boot/data/sam_config"))

for (file in files) {
  download(paste0(sam_dir, file))
}

setwd(wd)



