# scripts/02_clean_and_derive_metabric.R

library(tidyverse)
library(here)
library(janitor)
library(stringr)

# 1. Load the 'raw-clean' clinical data ----
clinical <- readRDS(here("data_clean", "metabric_clinical_clean.rds"))

# Quick check
glimpse(clinical)

# 2. Standardise key text variables (all lower-case, trim spaces) ----
clinical_std <- clinical %>%
  mutate(
    er_status   = str_squish(str_to_lower(er_status)),
    pr_status   = str_squish(str_to_lower(pr_status)),
    her2_status = str_squish(str_to_lower(her2_status)),
    overall_survival_status = str_squish(str_to_lower(`overall_survival_status`)),
    relapse_free_status     = str_squish(str_to_lower(`relapse_free_status`)),
    er_status_measured_by_ihc = str_squish(str_to_lower(`er_status_measured_by_ihc`))
  )

# Fix common typos like "positve" -> "positive"
clinical_std <- clinical_std %>%
  mutate(
    er_status = recode(er_status,
                       "positve" = "positive"),
    pr_status = recode(pr_status,
                       "positve" = "positive"),
    er_status_measured_by_ihc = recode(er_status_measured_by_ihc,
                                       "positve" = "positive")
  )

# 3. Convert numeric-like variables to numeric ----
clinical_std <- clinical_std %>%
  mutate(
    age_at_diagnosis              = as.numeric(age_at_diagnosis),
    tumor_size                    = as.numeric(`tumor_size`),
    tumor_stage                   = as.numeric(`tumor_stage`),
    neoplasm_histologic_grade     = as.numeric(`neoplasm_histologic_grade`),
    lymph_nodes_examined_positive = as.numeric(`lymph_nodes_examined_positive`),
    overall_survival_months       = as.numeric(`overall_survival_months`),
    relapse_free_status_months    = as.numeric(`relapse_free_status_months`)
  )

# 4. Create derived variables (RWE-style) ----

clinical_derived <- clinical_std %>%
  mutate(
    # Age groups
    age_group = cut(
      age_at_diagnosis,
      breaks = c(0, 49, 64, 74, Inf),
      labels = c("<50", "50-64", "65-74", "75+")
    ),
    
    # Hormone receptor positive (ER+ or PR+)
    hr_positive = if_else(
      er_status == "positive" | pr_status == "positive",
      1L, 0L,
      missing = 0L
    ),
    
    # HER2 positive
    her2_positive = if_else(
      her2_status == "positive",
      1L, 0L,
      missing = 0L
    ),
    
    # Triple-negative: ER-, PR-, HER2-
    tnbc = if_else(
      er_status == "negative" &
        pr_status == "negative" &
        her2_status == "negative",
      1L, 0L,
      missing = 0L
    ),
    
    # Node positive
    node_positive = if_else(
      lymph_nodes_examined_positive > 0,
      1L, 0L,
      missing = 0L
    ),
    
    # Survival event (1 = deceased, 0 = living)
    event = if_else(
      overall_survival_status == "deceased",
      1L, 0L,
      missing = 0L
    )
  )

# 5. Apply simple cohort inclusion criteria ----
# (You can refine these later.)

clinical_cohort <- clinical_derived %>%
  filter(
    !is.na(age_at_diagnosis),
    age_at_diagnosis >= 20,
    age_at_diagnosis <= 90,
    !is.na(tumor_size),
    !is.na(overall_survival_months),
    !is.na(event)
  )

# 6. Save final analytic cohort ----
if (!dir.exists(here("data_clean"))) {
  dir.create(here("data_clean"))
}

saveRDS(clinical_cohort, here("data_clean", "metabric_clean_final.rds"))

# Quick sanity check
clinical_cohort %>%
  count(event)
