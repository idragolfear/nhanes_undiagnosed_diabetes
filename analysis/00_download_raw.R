# Цей файл завантажує сирі дані з NHANES, просто готові файли щоб вони у нас були
# analysis/00_download_raw.R
# Download NHANES 2017-2018 XPT files into data/raw (gitignored)

library(here)
library(fs)
library(haven)

raw_data_path <- here::here("data", "raw")

fs::dir_create(raw_data_path)

modules <- c(
  "DEMO_J", # demographics  
  "BMX_J",  # body measures
  "GHB_J",  # HbA1c
  "GLU_J",  # fasting glucose
  "DIQ_J"   # Анкета, де людей питали: "Чи казав вам лікар, що у вас діабет?".
)

base_url <- "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles"
urls <- paste0(base_url, "/", modules, ".xpt")

# створюємо змінну, з шляхами куди покламти файли. file.path склеює в яку папку
# який файл з назвами покласти. Воно працює для всіх компів. Тут ми як шлях вка
# зуємо куди поставити поімт завантажені файли.
raw_file_path <- file.path(raw_data_path, paste0(modules, ".xpt"))

# створюємо таблицю data.frame - якраз ця функція. Щоб зрунчо працювало. Тут
# ми кажемо який модуль, яке до нього посилання, і куди поімт покласти
map <- data.frame(
  module = modules,
  url    = urls,
  dest   = raw_file_path
)

# цикл, загалом має такий формат бо ми в кінці створюємо функцію завантаження
# в циклі можуть бути різні дії. Це базові функції R все
for (i in seq_along(urls)) {
  url <-  urls[i]
  dest <- raw_file_path[i] 
  if (file_exists(dest)) {
    next
  }
  download.file(url = url, destfile = dest, mode = "wb", method = "curl")
}