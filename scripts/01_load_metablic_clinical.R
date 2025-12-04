# scripts/01_load_metabric_clinical.R

# Load packages
library(tidyverse)
library(here)
library(janitor)

# Read the METABRIC clinical CSV from data_raw
clinical_raw <- readr::read_csv(here::here("data_raw", "metabric_clinical.csv"))

# Peek at the structure
glimpse(clinical_raw)

# Clean column names to snake_case
clinical_clean <- clinical_raw %>%
  janitor::clean_names()

# Save a clean RDS copy for later steps
saveRDS(clinical_clean, here::here("data_clean", "metabric_clinical_clean.rds"))
