# analysis/03_models.R
# Survey-weighted regression models (NHANES 2017–2018)
# Primary outcome: HbA1c (LBXGH) with MEC weights (WTMEC2YR)
# Secondary outcome: fasting glucose (LBXGLU) with fasting weights (WTSAF2YR)

library(here)
library(dplyr)
library(survey)
library(splines)
# ---- Load data ----
nhanes <- readRDS(here("data", "clean", "nhanes_adult_core.rds"))

# ---- Survey design objects ----
des_hba1c <- svydesign(
  id      = ~SDMVPSU,
  strata  = ~SDMVSTRA,
  weights = ~WTMEC2YR,
  nest    = TRUE,
  data    = nhanes
)

des_glu <- svydesign(
  id      = ~SDMVPSU,
  strata  = ~SDMVSTRA,
  weights = ~WTSAF2YR,
  nest    = TRUE,
  data    = nhanes |> filter(!is.na(LBXGLU))
)

# ---- Model 1 (Primary): HbA1c ----
fit_hba1c_linear <- svyglm(
  LBXGH ~ BMXBMI + RIDAGEYR + factor(RIAGENDR),
  design = des_hba1c
)

# ---- Model 2 (Secondary): fasting glucose ----
fit_glu_linear <- svyglm(
  LBXGLU ~ BMXBMI + RIDAGEYR + factor(RIAGENDR),
  design = des_glu
)

# ---- Extract results (estimates + 95% CI) ----
hba1c_ci <- confint(fit_hba1c_linear)
glu_ci   <- confint(fit_glu_linear)

hba1c_res <- cbind(
  estimate = coef(fit_hba1c_linear),
  hba1c_ci
)

glu_res <- cbind(
  estimate = coef(fit_glu_linear),
  glu_ci
)

fit_hba1c_spline <- svyglm(
  LBXGH ~ ns(BMXBMI, df = 4) + RIDAGEYR + factor(RIAGENDR),
  design = des_hba1c
)

# ---- Interaction model: BMI × sex ----
fit_hba1c_int <- svyglm(
  LBXGH ~ BMXBMI * factor(RIAGENDR) + RIDAGEYR,
  design = des_hba1c
)