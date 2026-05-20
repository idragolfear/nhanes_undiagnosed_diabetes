# analysis/01_build_dataset.R
# Build analytical dataset from NHANES XPT files
# Step 0: project paths + raw data presence checks

library(here)
library(fs)
library(haven)
library(dplyr)

# ствоємо лист. Він має декілька переваг, чому ми просто змінні не запишемо
# найголовніше, що ми можемо застосувати функцію до path і вона буде до всіх її
# значень застосована. Тому замість 4 рядків (або більше, ящко ми ще щось додами)
# ми можемо до paths застосувати і все
paths <- list(
  raw     = here::here("data", "raw"),
  clean   = here::here("data", "clean"),
  outputs = here::here("outputs"),
  report  = here::here("report")
)

#Навіщо? перевірка, ми дивисося чи інсує сама папка так, ящко ні треба створити
if (!dir_exists(paths$raw)) {
  stop("Folder data/raw does not exist. Create it locally (it is gitignored).")
}

# ?Що робить? dir_ls - вказує шляхи до файлів. Ми отримаємо список файлів з їх шляхами
# ?Навіщо? Тоді нам не треба наприклад писати функцію для кожного файлу окремо, а відразу застосувати її до всього списку.
# Приклад: замість того щоб писати haven::read_xpt DEMO_J, GLU_J, BMX_J й так далі,
#          буде lapply(xpt_files, haven::read_xpt) - і всі файли прочитаються.
xpt_files <- fs::dir_ls(
  paths$raw,
  regexp = "(?i)\\.xpt$",
  type   = "file"
)

# Навіщо? перевірка чи там є файли, щоб ми код в пусту не робили
if (length(xpt_files) == 0) {
  stop("No .XPT files found in data/raw")
}

# ?Що робить? path_file - перетворює "data/raw/DEMO_J.xpt" в "DEMO_J.xpt"
# ?Що робить? path_ext_remove - перетворює "DEMO.xpt" в "DEMO_J"
# ?Навіщо? ми можемо додати нові змінні, і тоді доведеться в ручну додавати нові
#          змінні. А так все йде автоматизовано. І якщо ми додамо нові змінні, вони
#          автоматично підтягнуться
module_names <- fs::path_ext_remove(fs::path_file(xpt_files))

# ?Що робить? setNames - присвоює вектору, листу й т.д. імена. Зазвичай береться
#             такий же по довжині вектор імен і присвоюється.
# ?Що робить? lapply - застосовує формулу до списку або вектора.І повертає список, а не просто вектор, тому тут можуть бути різні дані
# ?Навіщо? lapply - замість того, щоб писати функцію для кожного елемента окремо
#          ми говоримо візьми 10 значень (які є в векторі або списку), і застосуй до них таку формулу
raw <- setNames(
  lapply(xpt_files, haven::read_xpt),
  module_names
)

# ?Що робить? sapply - застосовує формулу до списку або вектора.І повертає вектор
# ?Що робить? function(df) "SEQN" %in% names(df) - це анонімна функція, вона мені треба раз, тому я не створюю прям змінну з функцією
# ?Навіщо? SEQN це номер пацієнта. Саме по цьому ми будемо стикувати різні таблиці. Тому наявність SEQN є критично важливою, без нього ми не зможемо таблиці поєднати
has_seqn <- sapply(raw, function(df) "SEQN" %in% names(df))
stopifnot(all(has_seqn))

# Пояснення: для кращого розуміння, краще йти з ядра функції. 
#            df$SEQN - тут ми показали, що там треба стовпець SEQN з нашого df
#            duplicated(df$SEQN) - тут говоримо, що нас цікавлять дублікати
#            any(duplicated(df$SEQN)) - достатньо й одного. Any - якщо хочаб 1 дублікат є, нам вже скажуть. Є ще all  - то протилежність, там потрібні що всі були
dup_seqn <- sapply(raw, function(df) any(duplicated(df$SEQN)))
stopifnot(!any(dup_seqn))

stopifnot("DEMO_J" %in% names(raw))

# Пояснення: DEMO є головним, бо у ньому найбільша кількість учасників. Це перша анкета, яка заповнюється, тому вона повинна бути точно у всіх. По ній ми і рівняємо з іншими.
# ?Що робить? left_join - з'єднує таблиці, бере за основу ліву таблицю (тут демо) за певним ключем(SEQN).навіть якщо даних в правій таблиці немає, вона все одно їх з'єднає, але залишить NA. 
demo <- raw$DEMO_J
n_demo <- nrow(demo)
demo_bmx <- demo |> 
  dplyr::left_join(raw$BMX_J, by = "SEQN")
# ?Навіщо? перевірка тут, бо інколи якщоє дублікати, то може збільшитися кількість рядків, після цього наш аналіз зіпсується
stopifnot(nrow(demo_bmx) == n_demo)

core <- demo_bmx |> 
  dplyr::select(
    SEQN, #унікальний ID учасника
    RIDAGEYR, # вік у роках
    RIAGENDR, # стіть
    SDMVSTRA, # Masked Variance Pseudo-Stratum, для коректного розрахунку дисперсії (standard errors, CI, p-values) у складній вибірці. Не можна дати реальні страти, бо тоді людей можна визначаити, тому роблять масковані
    SDMVPSU,  # Masked Variance Pseudo-Primary Sampling Unit, тобто кластер, географічний регіон, округ, групу домогосподарства. Також замаскований, бо можна було б географічний віднайти
    WTMEC2YR, # Full Sample 2-Year MEC Exam Weight - вага
    BMXBMI,   # Body Mass Index (kg/m**2)
    RIDRETH1, # Race/Hispanic origin
    DMDEDUC2, # Education level - Adults 20+
    INDFMPIR  # Ratio of family income to poverty  
  )

n_core <- nrow(core)

core <- core |> 
  dplyr::left_join(
    raw$GHB_J |> dplyr::select(SEQN, LBXGH), # тут глікований гемоглобін беру
    by = "SEQN"
  )
stopifnot(nrow(core) == n_core)

core <- core |> 
  dplyr::left_join(
    raw$GLU_J |> dplyr::select(SEQN, LBXGLU, WTSAF2YR), # глюкоза натще мг/дл, WTSAF2YR - це статистична вага, саме для цього
    by = "SEQN"
  )

core <- core |>
  dplyr::left_join(raw$DIQ_J |> dplyr::select(SEQN, DIQ010), # DIQ010 - Doctor told you have diabetes
            by = "SEQN")
stopifnot(nrow(core) == n_core)

###
### Дивися проблему 1 та 2 
###
nhanes_adult <- core |> 
  dplyr::filter(RIDAGEYR >= 18) |> # фільтруємо щоб були лише дорослі
  dplyr::mutate(
    diabetes_a1c   = ifelse(!is.na(LBXGH) & LBXGH >= 6.5, 1, 0), # 1 = діабет наявний. Також зверни увагу на оператор & 0 умови повинні виконуватися одночасно
    diagnosed_dm   = ifelse(DIQ010 == 1, 1, ifelse(DIQ010 %in% c(2,3), 0, NA)), # тут найкраще щоб зрозуміти значення, дивися на кодування на сайті, що значать 2,3. Як це читається ifelse (умова, значення якщо TRUE, значення якщо FALSE), і тут ми якраз можемо вкласти ще одне іфелс в значення якщо FALSE тобто щоб перевірити ще одну умову. Для цього є краще рішення, dplyr::case_when. То й загалом виходить як 5 штук 
    undiagnosed_dm = ifelse(diabetes_a1c == 1 & diagnosed_dm == 0, 1,
                            ifelse(diabetes_a1c == 1 & diagnosed_dm == 1, 0, 0)) # тут якраз знову через вкладене іф елс. З цього я отримаю у того у кого значення глікованого 1, і його не казали (а у мене тас насправді ще одне включене)
  )

# ось тут якраз я вказав змінну де вище на початку робив. Тобто шлях яикм зручно користуватися. Тобот з листа створено
fs::dir_create(paths$clean) 

# saveRDS(object = що зберігаємо, file = куди і під якою назвою). там є ще інші умови, але зазвичай ми залишаємо їх дефолтно
saveRDS(nhanes_adult, 
        file = file.path(paths$clean, "nhanes_adult_core.rds")) # спочатку шлях, потім назву файла. Розширення для мене та інших, щоб читалося легше


