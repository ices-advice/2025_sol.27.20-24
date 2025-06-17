
setwd("C:/Users/jostou/OneDrive - Danmarks Tekniske Universitet/TAF_assesments/2025_sol.27.20-24_assessment")

library(icesTAF)
library(glue)
library(rvest)
library(data.table)

sam_assessment <- "Sole20_24_2025_new_indicesCV_conf"

mkdir("boot/data/sam_data")

sam_dir <-
  paste0(
    "https://stockassessment.org/datadisk/stockassessment/userdirs/user3/",
    sam_assessment,
    "/data/"
  )

files <-
  paste0(
    c("cn", "cw", "dw", "lf", "lw", "mo", "nm", "pf", "pm", "survey", "sw"),
    ".dat"
  )


wd <- getwd()
setwd(paste0(getwd(), "/boot/data/sam_data"))

for (file in files) {
  download(paste0(sam_dir, file)) #stupid downloader cannot have change in directory
}



get_files <- function(url) {
  tab <- url |>
    read_html() |>
    html_element("table") |>
    html_table(header = TRUE) |>
    tail(-2)
  out <- tab$Name
  file.path(url, out[nzchar(out)])
}

additional <- get_files(sam_dir)
additional <- additional[additional %like% "Sole2024-CV"]

sapply(additional, TAF::download, dir = ".")


setwd(wd)
