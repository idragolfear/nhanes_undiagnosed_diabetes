# analysis/01_build_dataset.R
# Build analytical dataset from NHANES XPT files
# Step 0: project paths + raw data presence checks
library(here)
library(fs)
library(haven)
library(dplyr)
paths <- list(
  raw     = here("data", "raw"),
  clean   = here("data", "clean"),
  outputs = here("outputs"),
  report  = here("report")
)
if (!dir_exists(paths$raw)) {
  stop("Folder data/raw does not exist. Create it locally (it is gitignored).")
}
xpt_files <- dir_ls(
  paths$raw,
  regexp = "(?i)\\.xpt$",
  type   = "file"
)
if (length(xpt_files) == 0) {
  stop("No .XPT files found in data/raw")
}
module_names <- path_ext_remove(path_file(xpt_files))
raw <- setNames(
  lapply(xpt_files, read_xpt),
  module_names
)
has_seqn <- sapply(raw, function(df) "SEQN" %in% names(df))
stopifnot(all(has_seqn))
dup_seqn <- sapply(raw, function(df) any(duplicated(df$SEQN)))
stopifnot(!any(dup_seqn))
stopifnot("DEMO_J" %in% names(raw))
demo <- raw$DEMO_J
n_demo <- nrow(demo)
demo_bmx <- demo |> 
  left_join(raw$BMX_J, by = "SEQN")
stopifnot(nrow(demo_bmx) == n_demo)
core <- demo_bmx |> 
  select(
    SEQN,
    RIDAGEYR,
    RIAGENDR,
    SDMVSTRA,
    SDMVPSU,
    WTMEC2YR,
    BMXBMI,
    # socio-demographics (from DEMO)
    RIDRETH1,   # race/ethnicity
    DMDEDUC2,   # education (>=20y)
    INDFMPIR    # poverty-income ratio
  )
n_core <- nrow(core)
core <- core |> 
  left_join(
    raw$GHB_J |> select(SEQN, LBXGH),
    by = "SEQN"
  )
stopifnot(nrow(core) == n_core)
core <- core |> 
  left_join(
    raw$GLU_J |> select(SEQN, LBXGLU, WTSAF2YR),
    by = "SEQN"
  )
core <- core |>
  left_join(raw$DIQ_J |> select(SEQN, DIQ010), by = "SEQN")
stopifnot(nrow(core) == n_core)
nhanes_adult <- core |> 
  filter(RIDAGEYR >= 18) |> 
  mutate(
    diabetes_a1c = ifelse(!is.na(LBXGH) & LBXGH >= 6.5, 1, 0),
    diagnosed_dm = ifelse(DIQ010 == 1, 1, ifelse(DIQ010 %in% c(2,3), 0, NA)),
    undiagnosed_dm = ifelse(diabetes_a1c == 1 & diagnosed_dm == 0, 1,
                            ifelse(diabetes_a1c == 1 & diagnosed_dm == 1, 0, 0))
  )
dir_create(paths$clean)

saveRDS(nhanes_adult, file = file.path(paths$clean, "nhanes_adult_core.rds"))