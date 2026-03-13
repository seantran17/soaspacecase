# ============================================================
#  BI Frequency Modelling (3 systems): ZINB only
#  Output: Observed vs Expected distribution of claim_count (bar overlay)
#  - For each solar_system, plot bars for Observed count vs Expected count
#  - Focus on counts 0..4 (as per data dictionary)
# ============================================================

stopifnot("freq_clean not found — run cleaning first" = exists("freq_clean"))

# ── 0. Dependencies ─────────────────────────────────────────
pkgs <- c("dplyr", "ggplot2", "pscl", "tidyr", "purrr", "tibble", "scales")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)

library(ggplot2)
library(scales)

# ── 1. Prepare base dataset ─────────────────────────────────
PREDICTORS <- c("production_load", "energy_backup_score",
                "supply_chain_index", "avg_crew_exp",
                "maintenance_freq", "safety_compliance")

base_df <- freq_clean %>%
  dplyr::select(
    dplyr::any_of(c("solar_system")),
    dplyr::all_of(PREDICTORS),
    exposure, claim_count
  ) %>%
  dplyr::filter(
    !is.na(solar_system),
    !is.na(exposure), exposure > 0,
    !is.na(claim_count)
  )

SYSTEMS <- c("Helionis Cluster", "Epsilon", "Zeta")
groups <- lapply(SYSTEMS, function(ss) dplyr::filter(base_df, solar_system == ss))
names(groups) <- SYSTEMS

# ── 2. ZINB formula (rate via offset) ───────────────────────
count_rhs <- paste(PREDICTORS, collapse = " + ")
zinb_formula <- stats::as.formula(
  paste0("claim_count ~ ", count_rhs, " + offset(log(exposure)) | ", count_rhs)
)

# ── 3. Fit ZINB per system and build Observed vs Expected ----
build_obs_vs_exp <- function(df, grp) {
  
  zinb <- tryCatch(
    pscl::zeroinfl(zinb_formula, data = df, dist = "negbin", link = "logit", EM = FALSE),
    error = function(e) { cat("ZINB failed for", grp, ":", conditionMessage(e), "\n"); NULL }
  )
  if (is.null(zinb)) return(NULL)
  
  # Observed counts
  obs_tbl <- df %>%
    dplyr::count(claim_count, name = "Observed") %>%
    dplyr::rename(k = claim_count)
  
  # Expected counts under ZINB for k = 0..4
  mu_count <- stats::predict(zinb, type = "count")  # NB mean for count component
  pi_zero  <- stats::predict(zinb, type = "zero")   # structural zero prob
  size     <- zinb$theta
  
  ks <- 0:4
  exp_tbl <- tibble::tibble(k = ks) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      Expected = sum(
        if (k == 0) {
          pi_zero + (1 - pi_zero) * stats::dnbinom(0, mu = mu_count, size = size)
        } else {
          (1 - pi_zero) * stats::dnbinom(k, mu = mu_count, size = size)
        }
      )
    ) %>%
    dplyr::ungroup()
  
  out <- dplyr::full_join(obs_tbl, exp_tbl, by = "k") %>%
    dplyr::mutate(
      Observed = tidyr::replace_na(Observed, 0),
      group = grp
    ) %>%
    tidyr::pivot_longer(cols = c("Observed", "Expected"),
                        names_to = "type", values_to = "count") %>%
    dplyr::mutate(
      type = factor(type, levels = c("Observed", "Expected")),
      k = factor(k, levels = ks)
    )
  
  out
}

obs_exp_all <- dplyr::bind_rows(
  purrr::map2(groups, names(groups), build_obs_vs_exp)
)

stopifnot("No results produced — check model fits" = nrow(obs_exp_all) > 0)

# ── 4. Plot: Observed vs Expected distributions (facet by system) ──
p_obs_exp <- ggplot2::ggplot(obs_exp_all,
                             ggplot2::aes(x = k, y = count, fill = type)) +
  ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.75),
                    width = 0.70, alpha = 0.90) +
  ggplot2::facet_wrap(~ group, scales = "free_y", ncol = 3) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::labs(
    title    = "Claim Count Distribution: Observed vs Expected (ZINB)",
    subtitle = "Bars compare observed counts to ZINB expected counts for k = 0..4",
    x        = "Claim Count (k)",
    y        = "Number of Policies",
    fill     = NULL
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title      = ggplot2::element_text(face = "bold"),
    plot.subtitle   = ggplot2::element_text(color = "grey40", size = 9),
    legend.position = "top",
    panel.grid.major.x = ggplot2::element_blank(),
    strip.text      = ggplot2::element_text(face = "bold", size = 10)
  )

print(p_obs_exp)
ggplot2::ggsave("bi_zinb_observed_vs_expected_counts_3systems.png",
                p_obs_exp, width = 14, height = 6, dpi = 150)
cat("Saved: bi_zinb_observed_vs_expected_counts_3systems.png\n")