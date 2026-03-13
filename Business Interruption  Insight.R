# ============================================================
#  CONTINUATION — appended after your BI cleaning + modelling
#  SAFE VERSION: explicitly uses dplyr:: / tidyr:: everywhere
#  so it won't break if MASS::select (or others) masks dplyr.
#
#  Requires: freq_clean and sev_clean in environment
#
#  Outputs:
#   1) Mean & variance of annual frequency (Overall + 3 systems)
#   2) System-level totals:
#        a) Total claim frequency per system = sum(count)/sum(exposure)
#        b) Policy count (rows) per system
#   3) Histograms for numeric predictors in freq (excluding annual_freq)
#   4) Claim amount distribution (sev) — raw + log10
#   5) Correlation heatmap (numeric predictors + annual_freq)
# ============================================================

# ── 0. Dependencies ──────────────────────────────────────────
pkgs <- c("dplyr", "ggplot2", "tidyr", "scales", "tibble")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)

library(ggplot2)
library(scales)

stopifnot(exists("freq_clean"), exists("sev_clean"))

# ── 1. Compute annualised claim frequency ────────────────────
freq_clean <- dplyr::mutate(freq_clean, annual_freq = claim_count / exposure)

# ============================================================
#  A) Mean & Variance bar chart (Overall + 3 systems)
# ============================================================

freq_stats <- function(df, label) {
  data.frame(
    group     = label,
    mean_freq = mean(df$annual_freq, na.rm = TRUE),
    var_freq  = var(df$annual_freq,  na.rm = TRUE),
    n         = nrow(df)
  )
}

stats_df <- dplyr::bind_rows(
  freq_stats(freq_clean, "Overall"),
  freq_stats(dplyr::filter(freq_clean, solar_system == "Helionis Cluster"), "Helionis\nCluster"),
  freq_stats(dplyr::filter(freq_clean, solar_system == "Epsilon"),          "Epsilon"),
  freq_stats(dplyr::filter(freq_clean, solar_system == "Zeta"),             "Zeta")
)

stats_df$group <- factor(stats_df$group, levels = stats_df$group)

stats_long <- stats_df %>%
  dplyr::select(group, mean_freq, var_freq) %>%
  tidyr::pivot_longer(cols = c(mean_freq, var_freq),
                      names_to = "statistic",
                      values_to = "value") %>%
  dplyr::mutate(statistic = dplyr::recode(statistic,
                                          mean_freq = "Mean",
                                          var_freq  = "Variance"))

p_grouped <- ggplot2::ggplot(stats_long, ggplot2::aes(x = group, y = value, fill = statistic)) +
  ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.6), width = 0.55) +
  ggplot2::geom_text(ggplot2::aes(label = ifelse(value < 0.001,
                                                 formatC(value, format = "e", digits = 2),
                                                 round(value, 3))),
                     position = ggplot2::position_dodge(width = 0.6),
                     vjust = -0.4, size = 3) +
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.18))) +
  ggplot2::labs(
    title    = "Annual Claim Frequency — Mean & Variance (Overall + Systems)",
    subtitle = "annual_freq = claim_count / exposure",
    x        = NULL,
    y        = "Annual Claim Frequency",
    fill     = NULL
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title      = ggplot2::element_text(face = "bold"),
    plot.subtitle   = ggplot2::element_text(color = "grey40", size = 9),
    legend.position = "top",
    panel.grid.major.x = ggplot2::element_blank(),
    axis.text.x     = ggplot2::element_text(size = 10)
  )

print(p_grouped)
ggplot2::ggsave("bi_grouped_freq_mean_var.png", p_grouped, width = 10, height = 5, dpi = 150)
cat("Saved: bi_grouped_freq_mean_var.png\n")

cat("\n── Grouped Frequency Statistics ────────────────────────\n")
print(dplyr::mutate(stats_df,
                    mean_freq = round(mean_freq, 6),
                    var_freq  = round(var_freq, 6)),
      row.names = FALSE)

# ============================================================
#  B) System-level totals
#     1) Total claim frequency per system = sum(count) / sum(exposure)
#     2) Policy count per system = number of rows
# ============================================================

sys_totals <- freq_clean %>%
  dplyr::group_by(solar_system) %>%
  dplyr::summarise(
    n_policies        = dplyr::n(),
    total_claims      = sum(claim_count),
    total_exposure    = sum(exposure),
    total_freq        = total_claims / total_exposure,
    .groups = "drop"
  )

# 1) Total frequency bar chart
p_total_freq <- ggplot2::ggplot(sys_totals, ggplot2::aes(x = solar_system, y = total_freq)) +
  ggplot2::geom_col(width = 0.65) +
  ggplot2::geom_text(ggplot2::aes(label = round(total_freq, 4)), vjust = -0.35, size = 3.2) +
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
  ggplot2::labs(
    title    = "Total Claim Frequency by Solar System",
    subtitle = "Total frequency = sum(claim_count) / sum(exposure)",
    x        = NULL,
    y        = "Total Claim Frequency"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    plot.subtitle = ggplot2::element_text(color = "grey40", size = 9),
    panel.grid.major.x = ggplot2::element_blank()
  )

print(p_total_freq)
ggplot2::ggsave("bi_total_freq_by_system.png", p_total_freq, width = 9, height = 5, dpi = 150)
cat("Saved: bi_total_freq_by_system.png\n")

# 2) Policy count (rows) bar chart
p_policy_count <- ggplot2::ggplot(sys_totals, ggplot2::aes(x = solar_system, y = n_policies)) +
  ggplot2::geom_col(width = 0.65) +
  ggplot2::geom_text(ggplot2::aes(label = scales::comma(n_policies)), vjust = -0.35, size = 3.2) +
  ggplot2::scale_y_continuous(labels = scales::comma, expand = ggplot2::expansion(mult = c(0, 0.15))) +
  ggplot2::labs(
    title    = "Policy Count (Rows) by Solar System",
    subtitle = "Number of rows in freq_clean for each system",
    x        = NULL,
    y        = "Policy Count"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    plot.subtitle = ggplot2::element_text(color = "grey40", size = 9),
    panel.grid.major.x = ggplot2::element_blank()
  )

print(p_policy_count)
ggplot2::ggsave("bi_policy_count_by_system.png", p_policy_count, width = 9, height = 5, dpi = 150)
cat("Saved: bi_policy_count_by_system.png\n")

cat("\n── System Totals ────────────────────────────────────────\n")
print(sys_totals, row.names = FALSE)

# ============================================================
#  C) Histograms of numeric predictors in FREQ (excluding annual_freq)
# ============================================================

# Identify numeric variables robustly without using select(where()) to avoid masking issues
is_num <- vapply(freq_clean, is.numeric, logical(1))
freq_num_vars <- names(freq_clean)[is_num]

# Exclude annual_freq from histogram set
freq_num_vars <- setdiff(freq_num_vars, "annual_freq")

# (Optional) If you also want to exclude claim_count, uncomment:
# freq_num_vars <- setdiff(freq_num_vars, "claim_count")

freq_long <- freq_clean %>%
  dplyr::select(dplyr::all_of(freq_num_vars)) %>%
  tidyr::pivot_longer(cols = dplyr::everything(),
                      names_to = "variable",
                      values_to = "value")

p_freq_hist <- ggplot2::ggplot(freq_long, ggplot2::aes(x = value)) +
  ggplot2::geom_histogram(bins = 40, color = "white", linewidth = 0.3) +
  ggplot2::facet_wrap(~ variable, scales = "free", ncol = 3) +
  ggplot2::labs(
    title    = "FREQ — Distribution of Numeric Variables (excluding annual_freq)",
    subtitle = "Faceted histograms; scales are free per variable",
    x        = NULL,
    y        = "Count"
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    plot.title    = ggplot2::element_text(face = "bold"),
    plot.subtitle = ggplot2::element_text(color = "grey40", size = 9),
    strip.text    = ggplot2::element_text(face = "bold", size = 9)
  )

print(p_freq_hist)
ggplot2::ggsave("bi_freq_numeric_histograms.png", p_freq_hist, width = 12, height = 8, dpi = 150)
cat("Saved: bi_freq_numeric_histograms.png\n")

# ============================================================
#  D) SEV — Claim amount distribution
# ============================================================

# 1) Raw scale histogram
p_sev_amt <- ggplot2::ggplot(sev_clean, ggplot2::aes(x = claim_amount)) +
  ggplot2::geom_histogram(bins = 60, color = "white", linewidth = 0.3) +
  ggplot2::scale_x_continuous(labels = scales::comma) +
  ggplot2::labs(
    title    = "SEV — Claim Amount Distribution (Raw Scale)",
    subtitle = "Histogram of claim_amount",
    x        = "Claim Amount",
    y        = "Count"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    plot.subtitle = ggplot2::element_text(color = "grey40", size = 9)
  )

print(p_sev_amt)
ggplot2::ggsave("bi_sev_claim_amount_hist_raw.png", p_sev_amt, width = 10, height = 6, dpi = 150)
cat("Saved: bi_sev_claim_amount_hist_raw.png\n")

# 2) Log10 scale histogram (usually more readable for heavy tails)
p_sev_amt_log <- ggplot2::ggplot(sev_clean, ggplot2::aes(x = claim_amount)) +
  ggplot2::geom_histogram(bins = 60, color = "white", linewidth = 0.3) +
  ggplot2::scale_x_log10(labels = scales::comma) +
  ggplot2::labs(
    title    = "SEV — Claim Amount Distribution (Log10 Scale)",
    subtitle = "Same data, x-axis on log scale",
    x        = "Claim Amount (log10 scale)",
    y        = "Count"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    plot.subtitle = ggplot2::element_text(color = "grey40", size = 9)
  )

print(p_sev_amt_log)
ggplot2::ggsave("bi_sev_claim_amount_hist_log10.png", p_sev_amt_log, width = 10, height = 6, dpi = 150)
cat("Saved: bi_sev_claim_amount_hist_log10.png\n")

# ============================================================
#  E) Correlation heatmap (numeric predictors + annual_freq)
# ============================================================

corr_vars <- c(
  "production_load", "energy_backup_score", "supply_chain_index",
  "avg_crew_exp", "maintenance_freq", "safety_compliance", "exposure",
  "annual_freq"
)

corr_df <- freq_clean %>%
  dplyr::select(dplyr::all_of(corr_vars))

corr_mat <- stats::cor(corr_df, use = "pairwise.complete.obs", method = "pearson")

corr_long <- as.data.frame(corr_mat) %>%
  tibble::rownames_to_column("Var1") %>%
  tidyr::pivot_longer(cols = -Var1, names_to = "Var2", values_to = "correlation") %>%
  dplyr::mutate(label = ifelse(abs(correlation) >= 0.02, sprintf("%.2f", correlation), ""))

p_heatmap <- ggplot2::ggplot(corr_long, ggplot2::aes(x = Var2, y = Var1, fill = correlation)) +
  ggplot2::geom_tile(color = "white", linewidth = 0.4) +
  ggplot2::geom_text(ggplot2::aes(label = label), size = 2.8) +
  ggplot2::scale_fill_gradient2(midpoint = 0, limits = c(-1, 1), name = "Pearson r") +
  ggplot2::labs(
    title    = "Correlation Heatmap — Predictors & Annual Claim Frequency",
    subtitle = "Pearson correlation; values shown where |r| >= 0.02",
    x        = NULL,
    y        = NULL
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    plot.title      = ggplot2::element_text(face = "bold"),
    plot.subtitle   = ggplot2::element_text(color = "grey40", size = 9),
    axis.text.x     = ggplot2::element_text(angle = 35, hjust = 1, size = 9),
    axis.text.y     = ggplot2::element_text(size = 9),
    panel.grid      = ggplot2::element_blank()
  )

print(p_heatmap)
ggplot2::ggsave("bi_correlation_heatmap_no_missing.png", p_heatmap, width = 10, height = 8, dpi = 150)
cat("Saved: bi_correlation_heatmap_no_missing.png\n")

cat("\n── Done ─────────────────────────────────────────────────\n")

# ============================================================
#  F) annual_freq distribution by solar_system (facet)
#     Histogram + density curve (linear + log10 x-axis)
# ============================================================

# Ensure annual_freq exists
stopifnot("annual_freq not found" = "annual_freq" %in% names(freq_clean))

# Keep only the three systems (in case there are any stray values)
af_df <- freq_clean %>%
  dplyr::filter(!is.na(solar_system)) %>%
  dplyr::mutate(
    solar_system = factor(solar_system, levels = c("Helionis Cluster", "Epsilon", "Zeta"))
  )

# ---- F1. Linear scale (facet) ----
p_af_lin_fac <- ggplot2::ggplot(af_df, ggplot2::aes(x = annual_freq)) +
  ggplot2::geom_histogram(
    ggplot2::aes(y = ..density..),
    bins = 80, color = "white", linewidth = 0.3, alpha = 0.70
  ) +
  ggplot2::geom_density(linewidth = 0.9, na.rm = TRUE) +
  ggplot2::facet_wrap(~ solar_system, scales = "free_y", ncol = 3) +
  ggplot2::labs(
    title    = "annual_freq Distribution by Solar System (Linear Scale)",
    subtitle = "annual_freq = claim_count / exposure | Histogram (density) + Density curve",
    x        = "annual_freq",
    y        = "Density"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title    = ggplot2::element_text(face = "bold"),
    plot.subtitle = ggplot2::element_text(color = "grey40", size = 9),
    strip.text    = ggplot2::element_text(face = "bold", size = 10)
  )

print(p_af_lin_fac)
ggplot2::ggsave("bi_annual_freq_dist_linear_by_system.png", p_af_lin_fac,
                width = 13, height = 5.5, dpi = 150)
cat("Saved: bi_annual_freq_dist_linear_by_system.png\n")

# ---- F2. Log10 x-axis (facet) ----
# Add tiny epsilon to avoid log(0)
eps <- 1e-6

p_af_log_fac <- ggplot2::ggplot(af_df, ggplot2::aes(x = annual_freq + eps)) +
  ggplot2::geom_histogram(
    ggplot2::aes(y = ..density..),
    bins = 80, color = "white", linewidth = 0.3, alpha = 0.70
  ) +
  ggplot2::geom_density(linewidth = 0.9, na.rm = TRUE) +
  ggplot2::scale_x_log10() +
  ggplot2::facet_wrap(~ solar_system, scales = "free_y", ncol = 3) +
  ggplot2::labs(
    title    = "annual_freq Distribution by Solar System (Log10 x-axis)",
    subtitle = "annual_freq = claim_count / exposure | log10(x + 1e-6) | Histogram (density) + Density curve",
    x        = "annual_freq (log10 scale)",
    y        = "Density"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title    = ggplot2::element_text(face = "bold"),
    plot.subtitle = ggplot2::element_text(color = "grey40", size = 9),
    strip.text    = ggplot2::element_text(face = "bold", size = 10)
  )

print(p_af_log_fac)
ggplot2::ggsave("bi_annual_freq_dist_log10_by_system.png", p_af_log_fac,
                width = 13, height = 5.5, dpi = 150)
cat("Saved: bi_annual_freq_dist_log10_by_system.png\n")