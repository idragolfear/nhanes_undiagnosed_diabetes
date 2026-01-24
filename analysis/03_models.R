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

nhanes <- nhanes |>
  mutate(
    race_eth = factor(RIDRETH1),
    educ     = factor(DMDEDUC2)
  )

nhanes <- nhanes |>
  mutate(
    race_cat = case_when(
      RIDRETH1 %in% c(1, 2) ~ "Hispanic",
      RIDRETH1 == 3         ~ "NH_White",
      RIDRETH1 == 4         ~ "NH_Black",
      RIDRETH1 == 5         ~ "Other",
      TRUE                  ~ NA_character_
    ),
    race_cat = factor(race_cat)
  )

nhanes <- nhanes |>
  mutate(
    educ_cat = case_when(
      DMDEDUC2 %in% c(1, 2) ~ "<HS",
      DMDEDUC2 == 3         ~ "HS",
      DMDEDUC2 %in% c(4, 5) ~ ">HS",
      TRUE                  ~ NA_character_
    ),
    educ_cat = factor(educ_cat)
  )


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

des_mec <- svydesign(
  id      = ~SDMVPSU,
  strata  = ~SDMVSTRA,
  weights = ~WTMEC2YR,
  nest    = TRUE,
  data    = nhanes
)

fit_udm_basic <- svyglm(
  undiagnosed_dm ~ RIDAGEYR + BMXBMI + factor(RIAGENDR),
  design = des_mec,
  family = quasibinomial()
)

or_basic <- cbind(
  OR  = exp(coef(fit_udm_basic)),
  exp(confint(fit_udm_basic))
)

round(or_basic, 3)

fit_udm_socio <- svyglm(
  undiagnosed_dm ~ RIDAGEYR + BMXBMI + factor(RIAGENDR) +
    race_eth + educ + INDFMPIR,
  design = des_mec,
  family = quasibinomial()
)

or_socio <- cbind(
  OR = exp(coef(fit_udm_socio)),
  exp(confint(fit_udm_socio))
)

fit_udm_socio2 <- svyglm(
  undiagnosed_dm ~ RIDAGEYR + BMXBMI + factor(RIAGENDR) +
    race_cat + educ_cat + INDFMPIR,
  design = des_mec,
  family = quasibinomial()
)

or_socio2 <- cbind(
  OR = exp(coef(fit_udm_socio2)),
  exp(confint(fit_udm_socio2))
)




