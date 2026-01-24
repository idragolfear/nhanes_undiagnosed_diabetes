# analysis/00_download_raw.R
# Download NHANES 2017-2018 XPT files into data/raw (gitignored)
library(here)
library(fs)
library(haven)
cycle <- "2017-2018"
dest_dir <- here("data", "raw")
dir_create(dest_dir)
modules <- c(
  "DEMO_J", # demographics  
  "BMX_J",  # body measures
  "GHB_J",  # HbA1c
  "GLU_J",  # fasting glucose
  "DIQ_J"
)
base_url <- "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles"
urls <- paste0(base_url, "/", modules, ".xpt")
print(urls)
dest_files <- file.path(dest_dir, paste0(modules, ".xpt"))
map <- data.frame(
  module = modules,
  url    = urls,
  dest   = dest_files
)
for (i in seq_along(urls)) {
  url <-  urls[i]
  dest <- dest_files[i] 
  if (file_exists(dest)) {
    next
  }
  download.file(url = url, destfile = dest, mode = "wb", method = "curl")
}