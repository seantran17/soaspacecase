# ============================================================
#  BI Aggregate Loss Simulation (3 systems)
#  - Exposure: shuffle within system each iteration (sum exposure unchanged)
#  - Frequency: ZINB per system (policy-level, uses shuffled exposure)
#  - Severity: Lognormal per system (fit meanlog/sdlog from sev_clean)
#  - Output: mean aggregate loss + VaR99 (10,000 sims)
#  - Plus: lognormal fit diagnostics plots/tests
# ============================================================

# ---- 0) Packages ----
pkgs <- c("dplyr", "ggplot2", "pscl", "tibble", "scales")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
library(ggplot2)
library(scales)

set.seed(20260305)

stopifnot(exists("freq_clean"), exists("sev_clean"))

SYSTEMS <- c("Helionis Cluster", "Epsilon", "Zeta")

# ---- 1) Fit severity: lognormal parameters per system ----
# (Assumes claim_amount > 0; your cleaning should ensure that)
sev_fit <- sev_clean %>%
  dplyr::filter(!is.na(solar_system), solar_system %in% SYSTEMS,
                !is.na(claim_amount), claim_amount > 0) %>%
  dplyr::mutate(ln_amt = log(claim_amount)) %>%
  dplyr::group_by(solar_system) %>%
  dplyr::summarise(
    n_sev   = dplyr::n(),
    meanlog = mean(ln_amt),
    sdlog   = sd(ln_amt),
    .groups = "drop"
  )

print(sev_fit, row.names = FALSE)

# ---- 2) Quick diagnostics: does log(amount) look Normal? ----
# Notes:
# - KS test is very sensitive with large n; use as a flag, not a verdict.
# - QQ plot + histogram overlay are more informative.

for (ss in SYSTEMS) {
  df_ss <- sev_clean %>%
    dplyr::filter(solar_system == ss, !is.na(claim_amount), claim_amount > 0) %>%
    dplyr::mutate(ln_amt = log(claim_amount))
  
  meanlog <- sev_fit$meanlog[sev_fit$solar_system == ss]
  sdlog   <- sev_fit$sdlog[sev_fit$solar_system == ss]
  
  # 2a) QQ plot of log amounts vs Normal(meanlog, sdlog)
  png(paste0("sev_lognormal_QQ_", gsub("[^A-Za-z0-9]", "_", ss), ".png"),
      width = 800, height = 650, res = 140)
  qq <- qqnorm(df_ss$ln_amt, main = paste0("QQ Plot: log(claim_amount) ~ Normal (", ss, ")"),
               xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
  qqline(df_ss$ln_amt, col = "red", lwd = 2)
  dev.off()
  
  # 2b) Histogram of log amounts + fitted Normal density
  p_hist_ln <- ggplot2::ggplot(df_ss, ggplot2::aes(x = ln_amt)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ..density..),
                            bins = 60, color = "white", linewidth = 0.3, alpha = 0.80) +
    ggplot2::stat_function(fun = dnorm,
                           args = list(mean = meanlog, sd = sdlog),
                           linewidth = 1.0) +
    ggplot2::labs(
      title = paste0("log(claim_amount) Histogram + Fitted Normal (", ss, ")"),
      subtitle = "If lognormal is reasonable, log(amount) should look ~ Normal",
      x = "log(claim_amount)", y = "Density"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
  ggplot2::ggsave(
    filename = paste0("sev_lognormal_log_hist_", gsub("[^A-Za-z0-9]", "_", ss), ".png"),
    plot = p_hist_ln, width = 10, height = 6, dpi = 150
  )
  
  # 2c) KS test on log amounts vs fitted normal
  # (very sensitive at large n; interpret cautiously)
  ks <- suppressWarnings(stats::ks.test(df_ss$ln_amt, "pnorm", mean = meanlog, sd = sdlog))
  cat("\n[KS on log(amount) ~ Normal] ", ss, "\n")
  print(ks)
}

cat("\nSaved diagnostic plots:\n",
    " - sev_lognormal_QQ_<system>.png\n",
    " - sev_lognormal_log_hist_<system>.png\n", sep = "")

# ---- 3) Fit frequency: ZINB model per system (policy-level) ----
PREDICTORS <- c("production_load", "energy_backup_score",
                "supply_chain_index", "avg_crew_exp",
                "maintenance_freq", "safety_compliance")

# Prepare modelling df (must contain predictors + exposure + claim_count)
freq_base <- freq_clean %>%
  dplyr::filter(!is.na(solar_system), solar_system %in% SYSTEMS,
                !is.na(exposure), exposure > 0,
                !is.na(claim_count)) %>%
  dplyr::select(solar_system, dplyr::all_of(PREDICTORS), exposure, claim_count)

count_rhs <- paste(PREDICTORS, collapse = " + ")
zinb_formula <- stats::as.formula(
  paste0("claim_count ~ ", count_rhs, " + offset(log(exposure)) | ", count_rhs)
)

zinb_models <- list()
for (ss in SYSTEMS) {
  df_ss <- freq_base %>% dplyr::filter(solar_system == ss)
  cat("\nFitting ZINB for", ss, "n =", nrow(df_ss), "\n")
  zinb_models[[ss]] <- pscl::zeroinfl(zinb_formula, data = df_ss, dist = "negbin", link = "logit", EM = FALSE)
}

# ---- 4) Simulation function per system ----
simulate_one_system <- function(ss, n_sims = 10000, cap_count = 4) {
  df_ss <- freq_base %>% dplyr::filter(solar_system == ss)
  model <- zinb_models[[ss]]
  
  # severity params
  meanlog <- sev_fit$meanlog[sev_fit$solar_system == ss]
  sdlog   <- sev_fit$sdlog[sev_fit$solar_system == ss]
  
  n_pol <- nrow(df_ss)
  stopifnot(n_pol > 0)
  
  agg_losses <- numeric(n_sims)
  
  for (b in seq_len(n_sims)) {
    # 4a) shuffle exposures (sum unchanged)
    df_iter <- df_ss
    df_iter$exposure <- sample(df_ss$exposure, size = n_pol, replace = FALSE)
    
    # 4b) policy-level ZINB parameters under shuffled exposure
    pi0 <- stats::predict(model, newdata = df_iter, type = "zero")   # Pr(structural zero)
    mu  <- stats::predict(model, newdata = df_iter, type = "count")  # NB mean for count part (includes offset)
    theta <- model$theta
    
    # 4c) draw counts: structural zero else NB
    u <- runif(n_pol)
    counts <- integer(n_pol)
    idx_nb <- (u >= pi0)
    
    if (any(idx_nb)) {
      counts[idx_nb] <- stats::rnbinom(sum(idx_nb), size = theta, mu = mu[idx_nb])
    }
    
    # cap to 0..4 if you want to stay consistent with data dictionary
    if (!is.null(cap_count)) counts <- pmin(cap_count, pmax(0L, counts))
    
    tot_count <- sum(counts)
    
    # 4d) draw severities for total claims (lognormal) and sum
    if (tot_count > 0) {
      agg_losses[b] <- sum(stats::rlnorm(tot_count, meanlog = meanlog, sdlog = sdlog))
    } else {
      agg_losses[b] <- 0
    }
  }
  
  list(
    system = ss,
    n_policies = n_pol,
    mean_agg = mean(agg_losses),
    VaR99 = as.numeric(stats::quantile(agg_losses, 0.99, names = FALSE)),
    TVaR99 = mean(agg_losses[agg_losses >= stats::quantile(agg_losses, 0.99)]),
    losses = agg_losses
  )
}

# ---- 5) Run 10k sims per system and summarise ----
N_SIMS <- 10000

sim_results <- lapply(SYSTEMS, function(ss) simulate_one_system(ss, n_sims = N_SIMS, cap_count = 4))

summary_tbl <- dplyr::bind_rows(lapply(sim_results, function(res) {
  data.frame(
    solar_system = res$system,
    n_policies   = res$n_policies,
    mean_agg_cost = res$mean_agg,
    VaR99         = res$VaR99,
    TVaR99        = res$TVaR99
  )
}))

cat("\n=== Simulation Summary (per system) ===\n")
print(summary_tbl, row.names = FALSE)

# Optional: save results
# write.csv(summary_tbl, "bi_agg_loss_sim_summary_by_system.csv", row.names = FALSE)

# ============================================================
#  EXTRA: Expected single-claim severity (E[claim_amount]) by system
#  Append this after sev_fit is created
# ============================================================

# For Lognormal(meanlog, sdlog):  E[X] = exp(meanlog + 0.5 * sdlog^2)
sev_fit <- sev_fit %>%
  dplyr::mutate(
    expected_claim_amount_lognormal = exp(meanlog + 0.5 * sdlog^2)
  )

cat("\n=== Expected single-claim amount by solar_system (lognormal mean) ===\n")
print(sev_fit %>% dplyr::select(solar_system, n_sev, meanlog, sdlog, expected_claim_amount_lognormal),
      row.names = FALSE)

# Also show empirical mean from data for comparison (no distribution assumption)
emp_mean <- sev_clean %>%
  dplyr::filter(!is.na(solar_system), solar_system %in% SYSTEMS,
                !is.na(claim_amount), claim_amount > 0) %>%
  dplyr::group_by(solar_system) %>%
  dplyr::summarise(
    empirical_mean_claim_amount = mean(claim_amount),
    .groups = "drop"
  )

cat("\n=== Empirical mean single-claim amount by solar_system (data mean) ===\n")
print(emp_mean, row.names = FALSE)

# Merge comparison table (optional)
sev_mean_compare <- sev_fit %>%
  dplyr::select(solar_system, n_sev, expected_claim_amount_lognormal) %>%
  dplyr::left_join(emp_mean, by = "solar_system")

cat("\n=== Lognormal mean vs empirical mean (by solar_system) ===\n")
print(sev_mean_compare, row.names = FALSE)

# ============================================================
#  (3 systems on ONE chart, 10,000 sims each)
# ============================================================

# Build a long data frame of all simulated losses
loss_df <- dplyr::bind_rows(lapply(sim_results, function(res) {
  data.frame(
    solar_system = res$system,
    agg_loss = res$losses
  )
}))

# ---- A) Density on raw scale (may be very spiky near 0) ----
p_density_raw <- ggplot2::ggplot(loss_df, ggplot2::aes(x = agg_loss, color = solar_system)) +
  ggplot2::geom_density(linewidth = 1.2, adjust = 1.2) +
  ggplot2::scale_x_continuous(labels = scales::comma) +
  ggplot2::labs(
    title = "Simulated Short-Term Aggregate Claim Amount — Density (Raw Scale)",
    subtitle = "10,000 simulations per solar system; density curves over aggregate annual claim amount",
    x = "Aggregate claim amount",
    y = "Density",
    color = "Solar system"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    legend.position = "top"
  )

print(p_density_raw)
ggplot2::ggsave("bi_agg_loss_density_raw.png", p_density_raw,
                width = 11, height = 6, dpi = 160)
cat("Saved: bi_agg_loss_density_raw.png\n")

# ---- B) Density on log1p scale  ----
loss_df2 <- loss_df
loss_df2$log_agg_loss <- log1p(loss_df2$agg_loss)

p_density_log1p <- ggplot2::ggplot(loss_df2, ggplot2::aes(x = log_agg_loss, color = solar_system)) +
  ggplot2::geom_density(linewidth = 1.2, adjust = 1.2) +
  ggplot2::labs(
    title = "Simulated Short-Term Aggregate Claim Amount — Density ",
    subtitle = "log1p(x) = log(1+x) preserves zeros and improves tail visibility",
    x = "log(1 + aggregate claim amount)",
    y = "Density",
    color = "Solar system"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    legend.position = "top"
  )

print(p_density_log1p)
ggplot2::ggsave("bi_agg_loss_density_log1p.png", p_density_log1p,
                width = 11, height = 6, dpi = 160)
cat("Saved: bi_agg_loss_density_log1p.png\n")

# ============================================================
#  Per-claim severity cap using Lognormal VaR (by system)
#  Cap = VaR_p of Lognormal(meanlog, sdlog)
# ============================================================

# Choose cap percentile (common choices: 0.99 or 0.995)
CAP_P <- 0.99   # change to 0.995 if you want a stricter cap

sev_cap_tbl <- sev_fit
sev_cap_tbl$cap_percentile <- CAP_P
sev_cap_tbl$claim_amount_cap <- stats::qlnorm(
  CAP_P,
  meanlog = sev_cap_tbl$meanlog,
  sdlog   = sev_cap_tbl$sdlog
)

cat("\n=== Per-claim severity cap by solar_system (Lognormal VaR) ===\n")
print(
  sev_cap_tbl[, c("solar_system", "n_sev", "meanlog", "sdlog", "cap_percentile", "claim_amount_cap")],
  row.names = FALSE
)

# ============================================================
#   Premium under capped severity + program limit
#  - No exposure shuffling: use exposure as in original data
#  - Per-claim cap: claim_amount = min(claim_amount, cap_by_system)
#  - Program limit cap on annual aggregate: min(agg_loss, PROGRAM_LIMIT)
#  - 10,000 Monte Carlo per system
#  - Outputs: mean, VaR99, RC; premium (ER=10%, CoC=rf+6%); premium per exposure
#  - Plots: density curves for 3 systems + total portfolio
# ============================================================

if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
library(dplyr)

N_SIMS <- 10000
PROGRAM_LIMIT <- 50922293127  # Đ 50,922,293,127 (apply as cap, as requested)

ER_NEW <- 0.10
RF_1Y  <- 0.0474
COC_RATE <- RF_1Y + 0.06  # 6% + 1Y risk-free

# total exposures from your table (Figure 2)
EXPO_TBL <- data.frame(
  solar_system = c("Helionis Cluster", "Epsilon", "Zeta"),
  total_exposure = c(9970, 19899, 20022)
)

# ---- Precompute policy-level ZINB parameters (pi0, mu) with original exposure ----
# This avoids calling predict() inside each simulation loop.
zinb_params <- list()

for (ss in SYSTEMS) {
  df_ss <- freq_base[freq_base$solar_system == ss, , drop = FALSE]
  model <- zinb_models[[ss]]
  
  pi0 <- stats::predict(model, newdata = df_ss, type = "zero")
  mu  <- stats::predict(model, newdata = df_ss, type = "count")
  theta <- model$theta
  
  zinb_params[[ss]] <- list(
    n_pol = nrow(df_ss),
    pi0 = pi0,
    mu  = mu,
    theta = theta
  )
}

# ---- Helper: simulate capped aggregate loss for ONE system ----
simulate_system_capped <- function(ss, n_sims = 10000, cap_count = 4L) {
  
  pars <- zinb_params[[ss]]
  n_pol <- pars$n_pol
  pi0   <- pars$pi0
  mu    <- pars$mu
  theta <- pars$theta
  
  # lognormal params
  meanlog <- sev_fit$meanlog[sev_fit$solar_system == ss]
  sdlog   <- sev_fit$sdlog[sev_fit$solar_system == ss]
  
  # per-claim cap (already computed in sev_cap_tbl)
  cap_val <- sev_cap_tbl$claim_amount_cap[sev_cap_tbl$solar_system == ss]
  if (length(cap_val) != 1 || is.na(cap_val)) stop("Missing cap for system: ", ss)
  
  agg_losses <- numeric(n_sims)
  
  for (b in seq_len(n_sims)) {
    # policy-level counts from ZINB (no exposure shuffle)
    u <- runif(n_pol)
    counts <- integer(n_pol)
    idx_nb <- (u >= pi0)
    
    if (any(idx_nb)) {
      counts[idx_nb] <- stats::rnbinom(sum(idx_nb), size = theta, mu = mu[idx_nb])
    }
    
    # cap counts to 0..4
    if (!is.null(cap_count)) counts <- pmin(as.integer(cap_count), pmax(0L, counts))
    
    tot_count <- sum(counts)
    
    if (tot_count > 0) {
      sev_draws <- stats::rlnorm(tot_count, meanlog = meanlog, sdlog = sdlog)
      sev_draws <- pmin(sev_draws, cap_val)  # per-claim cap
      agg_losses[b] <- sum(sev_draws)
    } else {
      agg_losses[b] <- 0
    }
    
    # apply program limit cap (as requested, cap each system annual aggregate)
    if (agg_losses[b] > PROGRAM_LIMIT) agg_losses[b] <- PROGRAM_LIMIT
  }
  
  agg_losses
}

# ---- Run simulations for 3 systems ----
losses_by_system <- list()
for (ss in SYSTEMS) {
  cat("\nSimulating capped aggregate losses for:", ss, "\n")
  losses_by_system[[ss]] <- simulate_system_capped(ss, n_sims = N_SIMS, cap_count = 4L)
}

# ---- Summary metrics + premium (per system) ----
premium_tbl <- data.frame(
  solar_system = SYSTEMS,
  mean_net_loss = NA_real_,
  VaR99_net_loss = NA_real_,
  RC_net = NA_real_,
  premium_total = NA_real_,
  premium_per_exposure = NA_real_,
  stringsAsFactors = FALSE
)

for (i in seq_along(SYSTEMS)) {
  ss <- SYSTEMS[i]
  x <- losses_by_system[[ss]]
  
  m <- mean(x)
  v99 <- as.numeric(stats::quantile(x, 0.99, names = FALSE))
  rc <- v99 - m
  
  prem <- (m + COC_RATE * rc) / (1 - ER_NEW)
  
  tot_exp <- EXPO_TBL$total_exposure[EXPO_TBL$solar_system == ss]
  prem_per_exp <- prem / tot_exp
  
  premium_tbl$mean_net_loss[i] <- m
  premium_tbl$VaR99_net_loss[i] <- v99
  premium_tbl$RC_net[i] <- rc
  premium_tbl$premium_total[i] <- prem
  premium_tbl$premium_per_exposure[i] <- prem_per_exp
}

cat("\n=== Premium & Risk Metrics (Net-of-cap, Net-of-program-limit) ===\n")
print(premium_tbl, row.names = FALSE)

# ---- Build long DF for plots ----
loss_long <- dplyr::bind_rows(lapply(SYSTEMS, function(ss) {
  data.frame(
    solar_system = ss,
    agg_loss = losses_by_system[[ss]]
  )
}))

# ---- Plot 1: 3-system density curves (raw) ----
p_density_3sys <- ggplot2::ggplot(loss_long, ggplot2::aes(x = agg_loss, color = solar_system)) +
  ggplot2::geom_density(linewidth = 1.2, adjust = 1.3) +
  ggplot2::scale_x_continuous(labels = scales::comma) +
  ggplot2::labs(
    title = "Short-Term Aggregate Loss Density by System",
    subtitle = paste0("N = ", N_SIMS, " sims per system"),
    x = "Annual aggregate loss (capped)",
    y = "Density",
    color = "Solar system"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    legend.position = "top"
  )

print(p_density_3sys)
ggplot2::ggsave("bi_agg_loss_density_3systems_capped.png", p_density_3sys,
                width = 11, height = 6, dpi = 160)
cat("Saved: bi_agg_loss_density_3systems_capped.png\n")

# ---- Plot 2: 3-system density curves (log1p; more readable tails) ----
loss_long2 <- loss_long
loss_long2$log_agg_loss <- log1p(loss_long2$agg_loss)

p_density_3sys_log <- ggplot2::ggplot(loss_long2, ggplot2::aes(x = log_agg_loss, color = solar_system)) +
  ggplot2::geom_density(linewidth = 1.2, adjust = 1.3) +
  ggplot2::labs(
    title = "Short-Term Aggregate Loss — Density by Solar System (log1p scale)",
    subtitle = "log1p(x)=log(1+x) preserves zeros and improves tail visibility",
    x = "log(1 + annual aggregate loss)",
    y = "Density",
    color = "Solar system"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    legend.position = "top"
  )

print(p_density_3sys_log)
ggplot2::ggsave("bi_agg_loss_density_3systems_capped_log1p.png", p_density_3sys_log,
                width = 11, height = 6, dpi = 160)
cat("Saved: bi_agg_loss_density_3systems_capped_log1p.png\n")

# ---- (Optional but useful) Portfolio total = sum of 3 systems per simulation ----
# Align sims by index: assume same N_SIMS for each system
loss_total <- losses_by_system[[SYSTEMS[1]]] +
  losses_by_system[[SYSTEMS[2]]] +
  losses_by_system[[SYSTEMS[3]]]

# If you also want a total portfolio cap (not asked, so commented):
# loss_total <- pmin(loss_total, PROGRAM_LIMIT)

p_total <- ggplot2::ggplot(data.frame(loss_total = loss_total), ggplot2::aes(x = loss_total)) +
  ggplot2::geom_density(linewidth = 1.2, adjust = 1.3) +
  ggplot2::scale_x_continuous(labels = scales::comma) +
  ggplot2::labs(
    title = "Total Portfolio Aggregate Loss (Sum of 3 Systems) — Density",
    subtitle = paste0("N = ", N_SIMS, " sims; each system uses per-claim cap and program cap"),
    x = "Total annual aggregate loss",
    y = "Density"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))

print(p_total)
ggplot2::ggsave("bi_agg_loss_density_total_portfolio.png", p_total,
                width = 11, height = 6, dpi = 160)
cat("Saved: bi_agg_loss_density_total_portfolio.png\n")

# ============================================================
#  Stress / Profit diagnostics — PROGRAM LEVEL (not by system)
#  Fixes caps:
#   - Single-claim limit: from sev_cap_tbl (per system)
#   - System section limit: from Figure 1 (per system)
#   - Entire program limit: from Figure 1 (program total)
#  Stress:
#   - Frequency multiplier = 1.5
#   - Severity multiplier  = 1.5
# ============================================================

# --- Inputs (from figures) ---
PROGRAM_LIMIT <- 50922293127  # Entire program cap

SYSTEM_LIMITS <- data.frame(
  solar_system = c("Helionis Cluster", "Epsilon", "Zeta"),
  system_limit = c(10964880078, 20301632164, 19655780885)
)

ER_NEW <- 0.10
FREQ_MULT <- 1.5
SEV_MULT  <- 1.5

# ---- helpers ----
tvar99 <- function(x) {
  q <- as.numeric(stats::quantile(x, 0.99, names = FALSE))
  mean(x[x >= q])
}

# ============================================================
#  BASELINE PROGRAM LOSS DISTRIBUTION (from existing simulations)
#  losses_by_system[[ss]] exists: each is length N_SIMS
#  Apply: system limit cap -> sum -> program limit cap
# ============================================================

stopifnot(all(c("Helionis Cluster","Epsilon","Zeta") %in% names(losses_by_system)))
N_SIMS <- length(losses_by_system[[SYSTEMS[1]]])

cap_system_vector <- function(ss, x) {
  lim <- SYSTEM_LIMITS$system_limit[SYSTEM_LIMITS$solar_system == ss]
  pmin(x, lim)
}

L_prog_baseline <- cap_system_vector("Helionis Cluster", losses_by_system[["Helionis Cluster"]]) +
  cap_system_vector("Epsilon",          losses_by_system[["Epsilon"]]) +
  cap_system_vector("Zeta",             losses_by_system[["Zeta"]])

L_prog_baseline <- pmin(L_prog_baseline, PROGRAM_LIMIT)

# Program premium = sum of system premiums (premium_tbl already computed earlier)
Premium_prog <- sum(premium_tbl$premium_total[premium_tbl$solar_system %in% SYSTEMS])
Expense_prog <- ER_NEW * Premium_prog

# ============================================================
#  Task 1: P(Pi < 0) where Pi = Premium - Expense - L(b)
# ============================================================
Pi_baseline <- Premium_prog - Expense_prog - L_prog_baseline
prob_profit_lt_0 <- mean(Pi_baseline < 0)

# ============================================================
#  Task 2: Expected tail shortfall = Expense + TVaR99(L) - Premium
# ============================================================
TVaR99_L_prog <- tvar99(L_prog_baseline)
expected_tail_shortfall <- Expense_prog + TVaR99_L_prog - Premium_prog

cat("\n=== PROGRAM LEVEL: Task 1 & 2 (Baseline) ===\n")
cat(sprintf("Program Premium        : %s\n", scales::comma(Premium_prog)))
cat(sprintf("Program Expense (10%%)  : %s\n", scales::comma(Expense_prog)))
cat(sprintf("P(Profit < 0)          : %.4f\n", prob_profit_lt_0))
cat(sprintf("TVaR99(L_program)      : %s\n", scales::comma(TVaR99_L_prog)))
cat(sprintf("Expected Tail Shortfall: %s\n", scales::comma(expected_tail_shortfall)))

# ============================================================
#  Task 3: ONE stress-year simulation (program level)
#    - Frequency x1.5 (mu multiplied)
#    - Severity  x1.5 (amount multiplied then per-claim capped)
#    - Apply system caps then program cap
#    - Premium unchanged
#    - Output: Expense + L_stress - Premium
# ============================================================

simulate_one_year_system_stress <- function(ss, cap_count = 4L) {
  pars <- zinb_params[[ss]]
  n_pol <- pars$n_pol
  pi0   <- pars$pi0
  mu    <- pars$mu
  theta <- pars$theta
  
  meanlog <- sev_fit$meanlog[sev_fit$solar_system == ss]
  sdlog   <- sev_fit$sdlog[sev_fit$solar_system == ss]
  
  # single-claim cap for this system (from sev_cap_tbl)
  cap_val <- sev_cap_tbl$claim_amount_cap[sev_cap_tbl$solar_system == ss]
  
  # ---- Frequency stress: multiply mu by 1.5 ----
  mu_stress <- FREQ_MULT * mu
  
  # draw counts from ZINB
  u <- runif(n_pol)
  counts <- integer(n_pol)
  idx_nb <- (u >= pi0)
  if (any(idx_nb)) {
    counts[idx_nb] <- stats::rnbinom(sum(idx_nb), size = theta, mu = mu_stress[idx_nb])
  }
  if (!is.null(cap_count)) counts <- pmin(as.integer(cap_count), pmax(0L, counts))
  
  tot_count <- sum(counts)
  
  # ---- Severity stress: draw lognormal, multiply by 1.5, cap each claim ----
  if (tot_count > 0) {
    sev_draws <- stats::rlnorm(tot_count, meanlog = meanlog, sdlog = sdlog)
    sev_draws <- SEV_MULT * sev_draws
    sev_draws <- pmin(sev_draws, cap_val)
    L_ss <- sum(sev_draws)
  } else {
    L_ss <- 0
  }
  
 
  L_ss
}

# One stress-year loss per system
L_H <- simulate_one_year_system_stress("Helionis Cluster", cap_count = 4L)
L_E <- simulate_one_year_system_stress("Epsilon", cap_count = 4L)
L_Z <- simulate_one_year_system_stress("Zeta", cap_count = 4L)

L_prog_stress_raw <- L_H + L_E + L_Z

# Excess over program cap
excess_over_program <- max(0, L_prog_stress_raw - PROGRAM_LIMIT)

# Apply program cap for capped view
L_prog_stress_capped <- min(L_prog_stress_raw, PROGRAM_LIMIT)

# Profit/cash metric using capped loss (optional)
stress_value_capped <- Expense_prog + L_prog_stress_capped - Premium_prog

cat("\n=== PROGRAM LEVEL: Task 3 (One stress-year; NO system section limits) ===\n")
cat(sprintf("Stress-year program loss (raw, before program cap): %s\n", scales::comma(L_prog_stress_raw)))
cat(sprintf("Program cap: %s\n", scales::comma(PROGRAM_LIMIT)))
cat(sprintf("Excess over program cap: %s\n", scales::comma(excess_over_program)))
cat(sprintf("Stress-year program loss (capped): %s\n", scales::comma(L_prog_stress_capped)))
cat(sprintf("Expense + L(capped) - Premium: %s\n", scales::comma(stress_value_capped)))
