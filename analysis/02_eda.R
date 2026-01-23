library(here)
library(survey)
library(dplyr)

nhanes <- readRDS(here("data", "clean", "nhanes_adult_core.rds"))

n_total <- nrow(nhanes)
n_hba1c <- sum(!is.na(nhanes$LBXGH))
n_glu   <- sum(!is.na(nhanes$LBXGLU))

des_hba1c <- svydesign(
  id     = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WTMEC2YR,
  nest   = TRUE,
  data   = nhanes
)

des_glu <- svydesign(
  id     = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WTSAF2YR,
  nest   = TRUE,
  data   = nhanes |> filter(!is.na(LBXGLU))
)

svymean(~LBXGH, design = des_hba1c, na.rm = TRUE)

svymean(~LBXGLU, design = des_glu, na.rm = TRUE)

svymean(~BMXBMI, design = des_hba1c, na.rm = TRUE)

c(
  total_adults = n_total,
  hba1c_available = n_hba1c,
  glucose_available = n_glu
)

# End of EDA:
# - analytic sample size established
# - survey design objects created
# - survey-weighted descriptive statistics computed