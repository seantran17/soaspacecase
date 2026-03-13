# ============================================================
#  Helionis Cluster — CSM% (10y) + IRR (10y)
#  WITH Investment Float Factor alpha = 0.4
#
#  Changes vs previous:
#   - Investment income uses float factor:
#       InvIncome_t = alpha * (Premium_t - Expense_t) * inv_rate
#
#  Other key assumptions:
#   - Profit-basis annual CF (years 1..10):
#       CF_t = Premium_t - Expense_t - ExpectedLoss_t + InvIncome_t
#   - IRR:
#       t=0:  -RC
#       t=1..10: CF_t
#       t=10: +RC (capital returned)
#   - Discontinuance (Scenario 10):
#       inforce = 1 - disc*0.5  (avg mid-year lapse)
#       Premium, Loss, InvestBase scale by inforce (Expense = ER*Premium auto)
# ============================================================

pkgs <- c("dplyr", "ggplot2", "scales")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
library(dplyr)
library(ggplot2)
library(scales)

set.seed(20260305)

# ---- Inputs (Helionis) ----
EL0 <- 9690080279      # year-1 expected loss (portfolio total)
RC  <- 1516371716      # required capital proxy
T   <- 10              # years

# ---- Investment float factor ----
ALPHA_FLOAT <- 0.40

# ---- Toggle (keep as you had) ----
# TRUE  = Equity IRR (Premium includes CoC charge)
# FALSE = Operating IRR (Premium excludes CoC charge)
INCLUDE_COC_IN_PREMIUM <- TRUE

# ---- Scenario table ----
scn <- tibble(
  scenario = c("Baseline","2","3","4","5","6","7","8","9","10"),
  ER = c(0.30, 0.45, 0.60, 0.30, 0.30, 0.30, 0.30, 0.30, 0.30, 0.30),
  rf1y = c(0.0179,0.0179,0.0179,0.0179,0.0179,0.0545,0.0015,0.0179,0.0179,0.0179),
  coc_spread = c(0.06,0.06,0.06,0.08,0.10,0.06,0.06,0.06,0.06,0.06),
  inv_spread = c(0.005,0.005,0.005,0.005,0.005,0.005,0.005,0.005,-0.01,0.005),
  infl = c(0.02465,0.02465,0.02465,0.02465,0.02465,0.02465,0.02465,0.0708,0.02465,0.02465),
  disc = c(0,0,0,0,0,0,0,0,0,0.10)
)

# ---- Helpers ----
premium_level <- function(EL, ER, coc_rate, RC, include_coc = TRUE) {
  rhs <- if (include_coc) (EL + coc_rate * RC) else EL
  rhs / (1 - ER)
}

pv_remaining <- function(x, t, rf) {
  rem <- t:length(x)
  sum(x[rem] / (1 + rf)^((rem - (t-1))))
}

solve_irr_robust <- function(cashflows, lower = -0.9, upper_start = 0.5, upper_max = 1e6) {
  npv <- function(r) sum(cashflows / (1 + r)^(0:(length(cashflows)-1)))
  lo <- lower; hi <- upper_start
  v_lo <- npv(lo); v_hi <- npv(hi)
  
  if (is.finite(v_lo) && is.finite(v_hi) && v_lo * v_hi < 0) {
    return(uniroot(function(r) npv(r), lower = lo, upper = hi)$root)
  }
  while (hi < upper_max && is.finite(v_hi) && v_lo * v_hi > 0) {
    hi <- hi * 2
    v_hi <- npv(hi)
    if (is.finite(v_lo) && is.finite(v_hi) && v_lo * v_hi < 0) {
      return(uniroot(function(r) npv(r), lower = lo, upper = hi)$root)
    }
  }
  NA_real_
}

# ---- Core per scenario ----
build_path <- function(one_scn) {
  
  ER   <- one_scn$ER
  rf   <- one_scn$rf1y
  infl <- one_scn$infl
  disc <- one_scn$disc
  
  coc_rate <- rf + one_scn$coc_spread
  inv_rate <- rf + one_scn$inv_spread
  
  # realistic lapse: average mid-year
  inforce <- 1 - disc * 0.5
  
  Prem1 <- premium_level(EL0, ER, coc_rate, RC, include_coc = INCLUDE_COC_IN_PREMIUM)
  
  years <- 1:T
  
  Premium  <- Prem1 * (1 + infl)^(years - 1) * inforce
  Expense  <- ER * Premium
  Loss_inc <- EL0  * (1 + infl)^(years - 1) * inforce
  
  # Investment base: net premium (= Premium - Expense)
  # Float factor alpha applied here
  InvestBase <- (Premium - Expense)
  InvIncome  <- ALPHA_FLOAT * InvestBase * inv_rate
  
  # Profit-basis CF (your definition)
  CF <- Premium - Expense - Loss_inc + InvIncome
  
  # IRR: -RC at t=0, CF1..CF10, +RC at year 10
  CF_irr <- c(-RC, CF)
  CF_irr[length(CF_irr)] <- CF_irr[length(CF_irr)] + RC
  irr <- solve_irr_robust(CF_irr)
  
  # CSM proxy: PV of remaining CF + fixed CU release
  CSM0 <- sum(CF / (1 + rf)^(1:T))
  CSM0 <- ifelse(CSM0 <= 0, 1e-6, CSM0)
  
  CSM <- numeric(T + 1)
  CSM[1] <- CSM0
  for (t in 1:T) {
    PV_rem <- pv_remaining(CF, t, rf)
    PV_rem <- max(PV_rem, 0)
    remaining_years <- (T - t + 1)
    release <- PV_rem / remaining_years
    CSM[t+1] <- PV_rem - release
  }
  CSM_pct <- CSM / CSM0 * 100
  
  list(scenario = one_scn$scenario, year = 0:T, CSM_pct = CSM_pct, irr = irr)
}

# ---- Run ----
paths <- lapply(split(scn, scn$scenario), function(df) build_path(df[1, ]))

csm_df <- bind_rows(lapply(paths, function(p) {
  data.frame(scenario = p$scenario, year = p$year, CSM_pct = p$CSM_pct)
}))

irr_df <- bind_rows(lapply(paths, function(p) {
  data.frame(scenario = p$scenario, IRR_10yr = p$irr, IRR_pct = p$irr * 100)
})) %>%
  mutate(scenario = factor(scenario, levels = c("Baseline","2","3","4","5","6","7","8","9","10"))) %>%
  arrange(scenario)

# ---- Plot 1 ----
group1 <- c("Baseline","2","3","4","5")
p1 <- ggplot(csm_df %>% filter(scenario %in% group1),
             aes(x = year, y = CSM_pct, color = scenario, linetype = scenario)) +
  geom_line(linewidth = 1.25) +
  scale_y_continuous(limits = c(0, 105), labels = function(x) paste0(round(x), "%")) +
  scale_x_continuous(breaks = 0:T) +
  labs(
    title = "CSM Remaining (%) Over Time — Baseline vs Scenarios 2–5",
    x = "Time (years)", y = "CSM Remaining (%)",
    color = "Scenario", linetype = "Scenario"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = c(0.82, 0.78),
    legend.background = element_rect(fill = alpha("white", 0.7), color = NA),
    legend.title = element_text(size = 9),
    legend.text  = element_text(size = 8)
  )
ggsave("csm_pct_baseline_vs_2to5.png", p1, width = 10.5, height = 6, dpi = 160)

# ---- Plot 2 ----
group2 <- c("Baseline","6","7","8","9","10")
p2 <- ggplot(csm_df %>% filter(scenario %in% group2),
             aes(x = year, y = CSM_pct, color = scenario, linetype = scenario)) +
  geom_line(linewidth = 1.25) +
  scale_y_continuous(limits = c(0, 105), labels = function(x) paste0(round(x), "%")) +
  scale_x_continuous(breaks = 0:T) +
  labs(
    title = "CSM Remaining (%) Over Time — Baseline vs Scenarios 6–10",
    x = "Time (years)", y = "CSM Remaining (%)",
    color = "Scenario", linetype = "Scenario"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = c(0.82, 0.78),
    legend.background = element_rect(fill = alpha("white", 0.7), color = NA),
    legend.title = element_text(size = 9),
    legend.text  = element_text(size = 8)
  )
ggsave("csm_pct_baseline_vs_6to10.png", p2, width = 10.5, height = 6, dpi = 160)

# ---- Plot 3: IRR ----
p3 <- ggplot(irr_df, aes(x = scenario, y = IRR_pct)) +
  geom_col(width = 0.72, na.rm = TRUE) +
  geom_text(aes(label = ifelse(is.na(IRR_pct), "NA", sprintf("%.1f%%", IRR_pct))),
            vjust = ifelse(is.na(irr_df$IRR_pct) | irr_df$IRR_pct >= 0, -0.35, 1.2),
            size = 3) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0.12, 0.20))) +
  labs(
    title = paste0("10-Year IRR Across Scenarios (alpha = ", ALPHA_FLOAT,
                   ifelse(INCLUDE_COC_IN_PREMIUM,
                          "; Equity IRR; Premium includes CoC)",
                          "; Operating IRR; Premium excludes CoC)")),
    x = "Scenario",
    y = "IRR (annual, %)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(size = 9)
  )
ggsave("irr_10yr_all_scenarios.png", p3, width = 11, height = 6, dpi = 160)

cat("\nSaved charts:\n",
    " - csm_pct_baseline_vs_2to5.png\n",
    " - csm_pct_baseline_vs_6to10.png\n",
    " - irr_10yr_all_scenarios.png\n", sep = "")

print(p1); print(p2); print(p3)

cat("\nIRR table:\n")
print(irr_df %>% mutate(IRR_pct = round(IRR_pct, 3)), row.names = FALSE)