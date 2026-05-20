library(ggplot2)

# ---- Нові дані для побудови кривої ----
newdata <- data.frame(
  BMXBMI = seq(20, 45, by = 0.1),
  RIDAGEYR = mean(nhanes$RIDAGEYR, na.rm = TRUE),
  RIAGENDR = 1
)

# ---- Прогнозовані ймовірності ----
newdata$prob <- as.numeric(
  predict(
    fit_udm_basic,
    newdata = newdata,
    type = "response"
  )
)

# ---- Графік ----
ggplot(newdata, aes(x = BMXBMI, y = prob)) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "Прогнозована ймовірність недіагностованого\nцукрового діабету залежно від ІМТ",
    x = "Індекс маси тіла (кг/м²)",
    y = "Ймовірність"
  ) +
  theme_minimal(base_size = 10)+
theme(
  plot.title = element_text(hjust = 0.5)
)
