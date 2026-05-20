library(here)
library(dplyr)
library(flextable)

# 1. Завантажуємо дані
s <- readRDS(here("data", "clean", "summary_stats.rds"))

# 2. Збираємо датафрейм
table1 <- data.frame(
  Variable = c(
    "Загальна кількість, n",
    "Вік, років",
    "ІМТ, кг/м²",
    "HbA1c, %",
    "Чоловіків, %",
    "Жінок, %"
  ),
  Value = c(
    as.character(s$n),
    sprintf("%.1f (%.1f–%.1f)", coef(s$mean_age), s$ci_age[1], s$ci_age[2]),
    sprintf("%.1f (%.1f–%.1f)", coef(s$mean_bmi), s$ci_bmi[1], s$ci_bmi[2]),
    sprintf("%.1f (%.1f–%.1f)", coef(s$mean_a1c), s$ci_a1c[1], s$ci_a1c[2]),
    sprintf("%.1f%%", coef(s$sex_pct)[1] * 100),
    sprintf("%.1f%%", coef(s$sex_pct)[2] * 100)
  )
)

# 3. Робимо і зберігаємо як PNG
flextable(table1) |>
  set_header_labels(Variable = "Variable", Value = "Mean (95% CI)") |>
  bold(part = "header") |>
  autofit() |>
  save_as_image(path = here("output", "table1.png"))

