library(here)
library(survey)
library(dplyr)

nhanes <- readRDS(here("data", "clean", "nhanes_adult_core.rds"))

n_total <- nrow(nhanes)

# 3. Survey design - тут складаємо дизайн по якому будемо оцінювати. Для всіх наутпних значень (віку, статі й т.д.) ваги, кластери стратифікація визначені. Тому я цим і закладаю анступний дизайн, щоб вони там враховувалися
des_mec <- survey::svydesign(
  id      = ~SDMVPSU, # кластер тут вказую ~ - це знаки, де ми змінні вказуємо
  strata  = ~SDMVSTRA, # страту
  weights = ~WTMEC2YR, # ваги
  nest    = TRUE, # Каже R що номери PSU не унікальні глобально — тобто в різних стратах може бути PSU з однаковим номером. nest = TRUE говорить розглядати їх як вкладені всередині страт, не плутати між собою.
  data    = nhanes
)

# 4. Описова статистика
mean_age <- survey::svymean(~RIDAGEYR, design = des_mec, na.rm = TRUE) # na.rm = TRUE - ігнорувати пропущені значення при розрахунку середнього
mean_bmi <- survey::svymean(~BMXBMI,   design = des_mec, na.rm = TRUE)
mean_a1c <- survey::svymean(~LBXGH,    design = des_mec, na.rm = TRUE)
sex_pct  <- survey::svymean(~as.factor(RIAGENDR), design = des_mec, na.rm = TRUE)
# як виявилося, то доцільніше рахувати медіани, а не середні. Бо маю дуже скошені дані, тому для імт, та глікованого гемоглобіну, буде медіана з квартилями
med_bmi <- survey::svyquantile(~BMXBMI, design = des_mec,
                               quantiles = c(0.25, 0.5, 0.75),
                               na.rm = TRUE)
med_a1c <- survey::svyquantile(~LBXGH, design = des_mec,
                               quantiles = c(0.25, 0.5, 0.75),
                               na.rm = TRUE)

# 5. 95% CI
ci_age <- confint(mean_age)
ci_bmi <- confint(mean_bmi)
ci_a1c <- confint(mean_a1c)

# 6. Зберігаємо
saveRDS(
  list(
    n        = n_total,
    mean_age = mean_age, ci_age = ci_age,
    mean_bmi = mean_bmi, ci_bmi = ci_bmi,
    mean_a1c = mean_a1c, ci_a1c = ci_a1c,
    med_a1c = med_a1c,
    med_bmi = med_bmi,
    sex_pct  = sex_pct
  ),
  file = here("data", "clean", "summary_stats.rds")
)
