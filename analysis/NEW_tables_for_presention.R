library(here)
library(dplyr)
library(flextable)

# 1. Завантажуємо дані
s <- readRDS(here("data", "clean", "summary_stats.rds"))

# 2. Збираємо датафрейм
table1 <- data.frame(
  Показник = c(
    "Загальна кількість, n",
    "Вік, років",
    "ІМТ, кг/м²",
    "HbA1c, %",
    "Чоловіки, %",
    "Жінки, %"
  ),
  Значення = c(
    # N
    as.character(s$n),
    
    # Вік — mean (95% CI)
    sprintf("%.1f (%.1f–%.1f)",
            coef(s$mean_age),
            s$ci_age[1],
            s$ci_age[2]),
    
    # ІМТ — median [Q1; Q3]
    sprintf("%.1f [%.1f; %.1f]",
            s$med_bmi$BMXBMI["0.5",  "quantile"],
            s$med_bmi$BMXBMI["0.25", "quantile"],
            s$med_bmi$BMXBMI["0.75", "quantile"]),
    
    # HbA1c — median [Q1; Q3]
    sprintf("%.1f [%.1f; %.1f]",
            s$med_a1c$LBXGH["0.5",  "quantile"],
            s$med_a1c$LBXGH["0.25", "quantile"],
            s$med_a1c$LBXGH["0.75", "quantile"]),
    
    # Стать
    sprintf("%.1f%%", coef(s$sex_pct)[1] * 100),
    sprintf("%.1f%%", coef(s$sex_pct)[2] * 100)
  )
)

# 3. Будуємо і зберігаємо таблицю
flextable(table1) |>
  set_header_labels(
    Показник  = "Показник",
    Значення  = "Значення"
  ) |>
  add_footer_lines(
    "Вік подано як середнє (95% ДІ); ІМТ та HbA1c — медіана [Q1; Q3]; стать — зважена частка (%)"
  ) |>
  bold(part = "header") |>
  italic(part = "footer") |>
  fontsize(size = 12, part = "all") |>
  autofit() |>
  save_as_image(path = here("output", "newtable1.png"))