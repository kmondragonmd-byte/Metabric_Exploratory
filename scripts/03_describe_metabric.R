# scripts/03_describe_metabric.R

# 1. Load packages and data ----
library(tidyverse)
library(here)
library(gtsummary)

cohort <- readRDS(here("data_clean", "metabric_clean_final.rds"))

# Quick look at the data structure
glimpse(cohort)


# 2. Basic cohort-level summaries ----

# Number of patients
n_patients <- nrow(cohort)
n_patients

# Event counts (overall survival)
cohort |>
  count(event)  # 1 = deceased, 0 = living

# Check hormone receptor, HER2, TNBC
cohort |>
  summarise(
    pct_hr_positive   = mean(hr_positive, na.rm = TRUE) * 100,
    pct_her2_positive = mean(her2_positive, na.rm = TRUE) * 100,
    pct_tnbc          = mean(tnbc, na.rm = TRUE) * 100
  )


# 3. Table 1: baseline characteristics by event status ----
# 0 = alive, 1 = deceased

table1 <- cohort |>
  select(
    age_at_diagnosis, age_group,
    tumor_size, tumor_stage, neoplasm_histologic_grade,
    lymph_nodes_examined_positive,
    er_status, pr_status, her2_status,
    hr_positive, her2_positive, tnbc,
    chemotherapy, hormone_therapy, radio_therapy,
    event
  ) |>
  tbl_summary(
    by = event,
    label = list(
      age_at_diagnosis ~ "Age at diagnosis (years)",
      age_group ~ "Age group",
      tumor_size ~ "Tumor size (mm)",
      tumor_stage ~ "Tumor stage",
      neoplasm_histologic_grade ~ "Histologic grade",
      lymph_nodes_examined_positive ~ "Positive nodes",
      er_status ~ "ER status",
      pr_status ~ "PR status",
      her2_status ~ "HER2 status",
      hr_positive ~ "HR positive",
      her2_positive ~ "HER2 positive",
      tnbc ~ "Triple negative",
      chemotherapy ~ "Chemotherapy",
      hormone_therapy ~ "Hormone therapy",
      radio_therapy ~ "Radiotherapy"
    ),
    missing = "ifany"
  ) |>
  add_p()

table1


# 4. Simple plots ----
# Make sure figs/ exists
if (!dir.exists(here("figs"))) {
  dir.create(here("figs"))
}

# 4.1 Age distribution by event
p_age <- cohort |>
  ggplot(aes(x = age_at_diagnosis, fill = factor(event))) +
  geom_histogram(alpha = 0.5, bins = 30, position = "identity") +
  labs(
    title = "Age at diagnosis by survival status",
    x = "Age at diagnosis (years)",
    y = "Number of patients",
    fill = "Event (1 = deceased)"
  ) +
  theme_minimal()

p_age

ggsave(
  filename = here("figs", "age_by_event.png"),
  plot = p_age,
  width = 6, height = 4, dpi = 300
)


# 4.2 Tumor size distribution by event
p_size <- cohort |>
  ggplot(aes(x = tumor_size, fill = factor(event))) +
  geom_histogram(alpha = 0.5, bins = 30, position = "identity") +
  labs(
    title = "Tumor size by survival status",
    x = "Tumor size (mm)",
    y = "Number of patients",
    fill = "Event (1 = deceased)"
  ) +
  theme_minimal()

p_size

ggsave(
  filename = here("figs", "tumor_size_by_event.png"),
  plot = p_size,
  width = 6, height = 4, dpi = 300
)


# 5. Save the Table 1 as an HTML file (optional) ----
table1_html <- table1 |>
  as_gt()

gt::gtsave(table1_html, here("figs", "table1_baseline_by_event.html"))
