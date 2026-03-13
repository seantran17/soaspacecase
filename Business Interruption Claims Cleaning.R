# ============================================================
#  SOA 2026 Case Study — BI Claims Cleaning
#  Version: IMPUTE (no row dropping, no missing indicators)
#
#  Rules:
#   1) Validity checks; invalid -> NA
#   2) Cross-fill freq <-> sev via policy_id (fills NA + invalid)
#   3) Remaining NA/outliers:
#        - Non-(count/amount): random draw within valid ranges
#        - claim_count: system mean freq * exposure, rounded, capped 0..4
#        - claim_amount: exp(mean(log(amount))) per system (lognormal median proxy)
# ============================================================

# ── 0. Dependencies ──────────────────────────────────────────
if (!requireNamespace("readxl",  quietly = TRUE)) install.packages("readxl")
if (!requireNamespace("dplyr",   quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")
if (!requireNamespace("tidyr",   quietly = TRUE)) install.packages("tidyr")

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)

set.seed(20260305)  # reproducible random fills

# ── 1. Load raw sheets ───────────────────────────────────────
FILE_PATH <- "srcsc2026claimsbusinessinterruption.xlsx"  # adjust if needed
freq_raw <- read_excel(FILE_PATH, sheet = "freq")
sev_raw  <- read_excel(FILE_PATH, sheet = "sev")

# ── 2. Validity rules ────────────────────────────────────────
VALID_SOLAR  <- c("Helionis Cluster", "Epsilon", "Zeta")
VALID_SCORES <- 1:5

clean_solar_system <- function(x) {
  str_extract(x, "^(Helionis Cluster|Epsilon|Zeta)")
}

# Helper: sample from observed distribution (fallback for IDs / solar_system)
sample_from_dist <- function(x, n = 1) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(rep(NA, n))
  sample(x, size = n, replace = TRUE)
}

# ── 3. Clean 'freq' sheet (invalid -> NA) ────────────────────
freq <- freq_raw %>%
  mutate(
    solar_system = clean_solar_system(solar_system),
    solar_system = if_else(solar_system %in% VALID_SOLAR, solar_system, NA_character_),
    
    production_load = if_else(!is.na(production_load) & (production_load < 0 | production_load > 1),
                              NA_real_, production_load),
    
    energy_backup_score = if_else(!is.na(energy_backup_score) & round(energy_backup_score) %in% VALID_SCORES,
                                  round(energy_backup_score), NA_real_),
    
    supply_chain_index = if_else(!is.na(supply_chain_index) & (supply_chain_index < 0 | supply_chain_index > 1),
                                 NA_real_, supply_chain_index),
    
    avg_crew_exp = if_else(!is.na(avg_crew_exp) & (avg_crew_exp < 1 | avg_crew_exp > 30),
                           NA_real_, avg_crew_exp),
    
    maintenance_freq = if_else(!is.na(maintenance_freq) & (maintenance_freq < 0 | maintenance_freq > 6),
                               NA_real_, maintenance_freq),
    
    safety_compliance = if_else(!is.na(safety_compliance) & round(safety_compliance) %in% VALID_SCORES,
                                round(safety_compliance), NA_real_),
    
    exposure = if_else(!is.na(exposure) & (exposure <= 0 | exposure > 1),
                       NA_real_, exposure),
    
    # claim_count: coerce to numeric first, then validate integer 0..4
    claim_count = suppressWarnings(as.numeric(claim_count)),
    claim_count = if_else(!is.na(claim_count) & (claim_count < 0 | claim_count > 4 | claim_count != floor(claim_count)),
                          NA_real_, claim_count)
  ) %>%
  mutate(
    energy_backup_score = suppressWarnings(as.integer(energy_backup_score)),
    safety_compliance   = suppressWarnings(as.integer(safety_compliance)),
    maintenance_freq    = suppressWarnings(as.integer(maintenance_freq)),
    claim_count         = suppressWarnings(as.integer(claim_count))
  )

# ── 4. Clean 'sev' sheet (invalid -> NA) ─────────────────────
sev <- sev_raw %>%
  mutate(
    solar_system = clean_solar_system(solar_system),
    solar_system = if_else(solar_system %in% VALID_SOLAR, solar_system, NA_character_),
    
    production_load = if_else(!is.na(production_load) & (production_load < 0 | production_load > 1),
                              NA_real_, production_load),
    
    energy_backup_score = if_else(!is.na(energy_backup_score) & round(energy_backup_score) %in% VALID_SCORES,
                                  round(energy_backup_score), NA_real_),
    
    safety_compliance = if_else(!is.na(safety_compliance) & round(safety_compliance) %in% VALID_SCORES,
                                round(safety_compliance), NA_real_),
    
    exposure = if_else(!is.na(exposure) & (exposure <= 0 | exposure > 1),
                       NA_real_, exposure),
    
    claim_amount = if_else(!is.na(claim_amount) & claim_amount <= 0,
                           NA_real_, claim_amount)
  ) %>%
  mutate(
    energy_backup_score = suppressWarnings(as.integer(energy_backup_score)),
    safety_compliance   = suppressWarnings(as.integer(safety_compliance))
  )

# ── 5. Cross-reference freq ↔ sev via policy_id (fill NA + invalid) ──
shared_cols <- c("solar_system", "station_id",
                 "production_load", "energy_backup_score",
                 "safety_compliance", "exposure")

lookup_cols <- c("policy_id", shared_cols)

# freq -> sev
freq_lookup <- freq[, lookup_cols] %>%
  group_by(policy_id) %>%
  summarise(across(everything(), ~ first(na.omit(.))), .groups = "drop")
names(freq_lookup)[-1] <- paste0(names(freq_lookup)[-1], "_from_freq")

sev <- sev %>%
  left_join(freq_lookup, by = "policy_id")

for (col in shared_cols) {
  col_fill <- paste0(col, "_from_freq")
  if (col_fill %in% names(sev)) {
    sev[[col]] <- ifelse(is.na(sev[[col]]), sev[[col_fill]], sev[[col]])
    sev[[col_fill]] <- NULL
  }
}

# sev -> freq
sev_lookup <- sev[, lookup_cols[lookup_cols %in% names(sev)]] %>%
  group_by(policy_id) %>%
  summarise(across(everything(), ~ first(na.omit(.))), .groups = "drop")
names(sev_lookup)[-1] <- paste0(names(sev_lookup)[-1], "_from_sev")

freq <- freq %>%
  left_join(sev_lookup, by = "policy_id")

for (col in shared_cols) {
  col_fill <- paste0(col, "_from_sev")
  if (col_fill %in% names(freq)) {
    freq[[col]] <- ifelse(is.na(freq[[col]]), freq[[col_fill]], freq[[col]])
    freq[[col_fill]] <- NULL
  }
}

# ── 6. Fill remaining missing IDs first (solar_system, station_id) ──
# 6a) solar_system: if still NA, sample by observed distribution in each table
solar_dist <- freq$solar_system[!is.na(freq$solar_system)]
freq$solar_system[is.na(freq$solar_system)] <- sample_from_dist(solar_dist, sum(is.na(freq$solar_system)))

solar_dist_sev <- sev$solar_system[!is.na(sev$solar_system)]
sev$solar_system[is.na(sev$solar_system)] <- sample_from_dist(solar_dist_sev, sum(is.na(sev$solar_system)))

# 6b) station_id: if still NA, sample from observed station_id within same solar_system
fill_station_by_system <- function(df) {
  df %>%
    group_by(solar_system) %>%
    mutate(
      station_id = if_else(
        is.na(station_id),
        sample_from_dist(station_id, n = dplyr::n()),
        station_id
      )
    ) %>%
    ungroup()
}
freq <- fill_station_by_system(freq)
sev  <- fill_station_by_system(sev)

# ── 7. Impute remaining predictors (NOT count/amount) by random valid ranges ──
impute_uniform <- function(x, lo, hi) {
  idx <- is.na(x)
  if (any(idx)) x[idx] <- runif(sum(idx), lo, hi)
  x
}
impute_discrete <- function(x, values) {
  idx <- is.na(x)
  if (any(idx)) x[idx] <- sample(values, size = sum(idx), replace = TRUE)
  x
}

# freq predictors
freq <- freq %>%
  mutate(
    production_load     = impute_uniform(production_load, 0, 1),
    supply_chain_index  = impute_uniform(supply_chain_index, 0, 1),
    avg_crew_exp        = impute_uniform(avg_crew_exp, 1, 30),
    maintenance_freq    = as.integer(round(impute_discrete(maintenance_freq, 0:6))),
    energy_backup_score = as.integer(round(impute_discrete(energy_backup_score, 1:5))),
    safety_compliance   = as.integer(round(impute_discrete(safety_compliance, 1:5))),
    # exposure: sample from observed exposure distribution (more realistic than uniform)
    exposure = {
      idx <- is.na(exposure)
      if (any(idx)) exposure[idx] <- sample_from_dist(exposure, sum(idx))
      exposure
    }
  )

# sev predictors
sev <- sev %>%
  mutate(
    production_load     = impute_uniform(production_load, 0, 1),
    energy_backup_score = as.integer(round(impute_discrete(energy_backup_score, 1:5))),
    safety_compliance   = as.integer(round(impute_discrete(safety_compliance, 1:5))),
    exposure = {
      idx <- is.na(exposure)
      if (any(idx)) exposure[idx] <- sample_from_dist(exposure, sum(idx))
      exposure
    }
  )

# ── 8. Handle claim_count NA/abnormal using system mean freq * exposure ──
# Compute system mean frequency from available (non-missing) rows
freq_rate_by_ss <- freq %>%
  filter(!is.na(claim_count), !is.na(exposure), exposure > 0) %>%
  group_by(solar_system) %>%
  summarise(lambda = sum(claim_count) / sum(exposure), .groups = "drop")

freq <- freq %>%
  dplyr::left_join(freq_rate_by_ss, by = "solar_system") %>%
  dplyr::mutate(
    claim_count = dplyr::if_else(
      is.na(claim_count),
      as.integer(round(pmin(4, pmax(0, lambda * exposure)))),
      claim_count
    )
  ) %>%
  dplyr::select(-lambda)

# ── 9. Handle claim_amount NA/outliers using exp(mean(log(amount))) per system ──
# Define "obviously impossible" high outlier rule on log scale (conservative):
# mark as outlier if log(amount) > mean + 6*sd within system (only when sd exists)
sev <- sev %>%
  dplyr::group_by(solar_system) %>%
  dplyr::mutate(
    ln_amt = log(claim_amount),
    ln_mean = mean(ln_amt, na.rm = TRUE),
    ln_sd   = sd(ln_amt, na.rm = TRUE),
    is_outlier_high = dplyr::if_else(
      !is.na(ln_amt) & !is.na(ln_sd) & ln_sd > 0,
      ln_amt > (ln_mean + 6 * ln_sd),
      FALSE
    ),
    claim_amount = dplyr::if_else(
      is.na(claim_amount) | claim_amount <= 0 | is_outlier_high,
      exp(ln_mean),
      claim_amount
    )
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(-ln_amt, -ln_mean, -ln_sd, -is_outlier_high)

# ── 10. Final outputs ────────────────────────────────────────
freq_clean <- freq
sev_clean  <- sev

cat("=== freq_clean (imputed): rows =", nrow(freq_clean), " | cols =", ncol(freq_clean), "\n")
cat("Remaining NAs in freq_clean?\n")
print(colSums(is.na(freq_clean)))

cat("\n=== sev_clean (imputed): rows =", nrow(sev_clean), " | cols =", ncol(sev_clean), "\n")
cat("Remaining NAs in sev_clean?\n")
print(colSums(is.na(sev_clean)))

# Optional: write out
# write.csv(freq_clean, "bi_freq_clean_imputed.csv", row.names = FALSE)
# write.csv(sev_clean,  "bi_sev_clean_imputed.csv",  row.names = FALSE)

# ============================================================
#  EXTRA: Exposure summary by solar_system (freq_clean & sev_clean)
#  Append this after freq_clean / sev_clean are created
# ============================================================

cat("\n=== Exposure summary by solar_system (FREQ) ===\n")
exposure_by_ss_freq <- freq_clean %>%
  dplyr::group_by(solar_system) %>%
  dplyr::summarise(
    n_rows        = dplyr::n(),
    exposure_sum  = sum(exposure, na.rm = TRUE),
    exposure_mean = mean(exposure, na.rm = TRUE),
    exposure_sd   = sd(exposure, na.rm = TRUE),
    exposure_min  = min(exposure, na.rm = TRUE),
    exposure_p25  = as.numeric(stats::quantile(exposure, 0.25, na.rm = TRUE)),
    exposure_med  = as.numeric(stats::quantile(exposure, 0.50, na.rm = TRUE)),
    exposure_p75  = as.numeric(stats::quantile(exposure, 0.75, na.rm = TRUE)),
    exposure_max  = max(exposure, na.rm = TRUE),
    .groups = "drop"
  )

print(exposure_by_ss_freq, row.names = FALSE)