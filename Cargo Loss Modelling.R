# ACTL5100 Cargo Loss Modelling
library(readxl)
clms_cargo_freq <- read_xlsx("C:/Users/stsse/OneDrive/Documents/UNSW/ACTL5100/cargo_freq_cln.xlsx")
clms_cargo_sev <- read_xlsx("C:/Users/stsse/OneDrive/Documents/UNSW/ACTL5100/cargo_sev_cln.xlsx")

# Cargo Frequency -----------------------------------------------------------
# Make discrete variables factors
clms_cargo_freq$route_risk <- factor(clms_cargo_freq$route_risk)
clms_cargo_sev$claim_seq <- factor(clms_cargo_sev$claim_seq)
clms_cargo_sev$route_risk <- factor(clms_cargo_sev$route_risk)
clms_cargo_freq$cargo_type <- factor(clms_cargo_freq$cargo_type)
clms_cargo_sev$cargo_type <- factor(clms_cargo_sev$cargo_type)

# Create frequency model that accounts for exposure
# Look at correlations
cor(clms_cargo_freq[sapply(clms_cargo_freq, is.numeric)])

# Most correlated variables are pilot experience, solar radiation,
# and debris density
library(MASS)
set.seed(5110)
n <- nrow(clms_cargo_freq)
train_idx <- 1:floor(0.6*n)
valid_idx <- (floor(0.6*n)+1):floor(0.8*n)
test_idx  <- (floor(0.8*n)+1):n
train_data <- clms_cargo_freq[train_idx, ]
valid_data <- clms_cargo_freq[valid_idx, ]
test_data  <- clms_cargo_freq[test_idx, ]
train_data$cargo_type <- factor(train_data$cargo_type)
valid_data$cargo_type <- factor(valid_data$cargo_type)
test_data$cargo_type <- factor(test_data$cargo_type)

library(combinat)

vars <- c("route_risk",
          "pilot_experience",
          "solar_radiation",
          "debris_density")

all_subsets <- unlist(
  lapply(1:length(vars), function(x) combn(vars, x, simplify = FALSE)),
  recursive = FALSE
)

models <- list()
val_ll <- numeric(length(all_subsets))

for(i in seq_along(all_subsets)){
  
  form <- as.formula(
    paste("claim_count ~",
          paste(all_subsets[[i]], collapse = "+"),
          "+ offset(log(exposure))")
  )
  
  models[[i]] <- glm.nb(form,
                        data = train_data)
  
  mu <- predict(models[[i]], newdata = valid_data, type = "response")
  theta <- models[[i]]$theta
  
  val_ll[i] <- sum(
    dnbinom(valid_data$claim_count,
            size = theta,
            mu = mu,
            log = TRUE)
  )
}

best_idx <- which.max(val_ll)

best_model <- models[[best_idx]]

all_subsets[[best_idx]]

valid_pred <- predict(best_model, newdata = valid_data, type = "response")
test_pred <- predict(best_model, newdata = test_data, type = "response")

valid_ll <- sum(
  dnbinom(valid_data$claim_count,
          size = best_model$theta,
          mu = valid_pred,
          log = TRUE)
)

test_ll <- sum(
  dnbinom(test_data$claim_count,
          size = best_model$theta,
          mu = test_pred,
          log = TRUE)
)

valid_ll
test_ll

summary(best_model)

# Chosen Frequency Model ----------------------------
freq_model <- glm.nb(claim_count ~
                       route_risk +
                       pilot_experience +
                       debris_density +
                       offset(log(exposure)),
                     data = clms_cargo_freq)

summary(freq_model)

# Cargo Severity --------------------------------------------------------------
library(ggplot2)
threshold <- exp(14.8)
ggplot(clms_cargo_sev, aes(x = claim_amount, fill = claim_amount >= threshold)) +
  geom_histogram(bins = 100, color = "black", alpha = 0.7) +
  scale_fill_manual(
    values = c("skyblue", "orange"),
    labels = c("Working", "Tail")
  ) +
  labs(
    x = "Claim Amount (log scale)",
    y = "Frequency",
    title = "Working vs Tail Claims",
    fill = paste0("Claim Threshold = $", format(threshold, big.mark = ","))
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.85, 0.85),  # top right inside plot
    legend.background = element_rect(fill = "white", color = "black")
  )

ggplot(clms_cargo_sev, aes(x = claim_amount, fill = claim_amount >= threshold)) +
  geom_histogram(bins = 100, color = "black", alpha = 0.7) +
  scale_x_log10() +
  scale_fill_manual(
    values = c("skyblue", "orange"),
    labels = c("Working", "Tail")
  ) +
  labs(
    x = "Claim Amount (log scale)",
    y = "Frequency",
    title = "Working vs Tail Claims",
    fill = paste0("Claim Threshold = $", format(threshold, big.mark = ","))
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.85, 0.85),  # top right inside plot
    legend.background = element_rect(fill = "white", color = "black")
  )

# From the log transformed severity, we observe two peaks
# The right peak is too large to be considered a tail

# Explore possible influential factors
# Histogram shows two distinct groups of claims
library(dplyr)
num_vars <- c("cargo_value", "weight", "distance", "transit_duration",
              "pilot_experience", "vessel_age", "solar_radiation",
              "debris_density", "exposure")

create_numeric_band <- function(df, var_name, n_quantiles = n, new_var_name = NULL) {
  if (is.null(new_var_name)) {
    new_var_name <- paste0(var_name, "_band_", n_quantiles)
  }
  
  probs <- seq(0, 1, length.out = n_quantiles + 1)
  breaks <- unique(quantile(df[[var_name]], probs = probs, na.rm = TRUE))
  
  df <- df %>%
    mutate(!!new_var_name := cut(.data[[var_name]], breaks = breaks, include.lowest = TRUE))
  
  return(df)
}

for (n in 2:5) {
  for (v in num_vars) {
    clms_cargo_sev <- create_numeric_band(
      clms_cargo_sev, 
      var_name = v, 
      n_quantiles = n
    )
    
    # summarize mean claim per band
    band_col <- paste0(v, "_band_", n)
    summary <- clms_cargo_sev %>%
      group_by(.data[[band_col]]) %>%
      summarise(
        mean_claim = mean(claim_amount, na.rm = TRUE),
        var_claim  = var(claim_amount, na.rm = TRUE),
        n_rows     = n(),
        .groups = "drop"
      )
    
    print(paste("Variable:", v, "- Bands:", n))
    print(summary)
  }
}

# Appears that mainly cargo value, weight are the numerical variables
# which influence claim size the most
# Weight, cargo type, value, route risk, solar, debris all seem to have
# influence on severity

# Consider fitting Gamma GLM with Log link
set.seed(5110)
n <- nrow(clms_cargo_sev)
train_idx <- 1:floor(0.6*n)
valid_idx <- (floor(0.6*n)+1):floor(0.8*n)
test_idx  <- (floor(0.8*n)+1):n
train_data <- clms_cargo_sev[train_idx, ]
valid_data <- clms_cargo_sev[valid_idx, ]
test_data  <- clms_cargo_sev[test_idx, ]
train_data$cargo_type <- factor(train_data$cargo_type)
valid_data$cargo_type <- factor(valid_data$cargo_type)
test_data$cargo_type <- factor(test_data$cargo_type)

library(combinat)

vars <- c("cargo_type",
          "route_risk",
          "cargo_value",
          "weight",
          "solar_radiation",
          "debris_density")

all_subsets <- unlist(
  lapply(1:length(vars), function(x) combn(vars, x, simplify = FALSE)),
  recursive = FALSE
)

models <- list()
val_deviance <- numeric(length(all_subsets))

for(i in seq_along(all_subsets)){
  
  form <- as.formula(
    paste("claim_amount ~", paste(all_subsets[[i]], collapse = "+"))
  )
  
  models[[i]] <- glm(form,
                     family = Gamma(link = "log"),
                     data = train_data)
  
  pred <- predict(models[[i]], newdata = valid_data, type = "response")
  
  val_deviance[i] <- mean((valid_data$claim_amount - pred)^2)
}

best_idx <- which.min(val_deviance)

best_model <- models[[best_idx]]

all_subsets[[best_idx]]

valid_pred <- predict(best_model, newdata = valid_data, type = "response")
test_pred <- predict(best_model, newdata = test_data, type = "response")

valid_mse <- mean((valid_data$claim_amount - valid_pred)^2)
test_mse <- mean((test_data$claim_amount - test_pred)^2)

test_mse

summary(best_model)

# Chosen Severity Model ----------------------------
sev_glm <- glm(claim_amount ~ 
                 cargo_type +
                 route_risk +
                 cargo_value +
                 weight,
               family = Gamma(link = "log"),
               data = clms_cargo_sev)

plot(sev_glm)
summary(sev_glm)

pseudo_Rsquared <- 1 - deviance(sev_glm) / sev_glm$null.deviance

# Visual Comparison to Observed Claim Amounts --------------------
pred <- predict(sev_glm, type="response")

plot(clms_cargo_sev$claim_amount, pred,
     xlab="Observed severity",
     ylab="Predicted severity")

abline(0,1,col="red")

plot(density(clms_cargo_sev$claim_amount), lwd=2)
lines(density(pred), col="red", lwd=2)

legend("topright",
       legend=c("Observed","Predicted"),
       col=c("black","red"),
       lwd=2)

obs_log <- log(clms_cargo_sev$claim_amount)
pred_log <- log(pred)

plot(density(obs_log), lwd=2,
     main="Observed vs Predicted Log Claim Severity",
     xlab="Log Claim Amount")

lines(density(pred_log), col="red", lwd=2)

legend("topright",
       legend=c("Observed","Predicted"),
       col=c("black","red"),
       lwd=2)

# Baseline Premium --------------------------------------------
base <- exp(sev_glm$coefficients["(Intercept)"]) *
  exp(freq_model$coefficients["(Intercept)"])

# Route risk 3 is the extremely common, bring this into the baseline
base <- base * exp(sev_glm$coefficients["route_risk3"])

# Add 20% premium loading
prem_loading <- 1.2
base <- base * prem_loading
base
mean(clms_cargo_sev$claim_amount)

# Base premium = $196421.3

# Relativities
cargo_type_high <- exp(mean(c(sev_glm$coefficients["cargo_typegold"],
                          sev_glm$coefficients["cargo_typeplatinum"])))
cargo_type_high_round <- 44
route_risk_high <- exp(mean(c(sev_glm$coefficients["route_risk4"],
                              sev_glm$coefficients["route_risk5"])))
route_risk_high_round <- 2
median_weight <- exp(median(clms_cargo_sev$weight) *
                      sev_glm$coefficients["weight"])
median_weight_round <- 1.75
mean_cargo_value <- exp(mean(clms_cargo_sev$cargo_value) *
                            sev_glm$coefficients["cargo_value"])
mean_cargo_value_round <- 1.25
safety_discount <- 0.9

# Salaries and Administrative Costs --------------------------
total_salary <- 260573000
total_admin <- 10000000000

# Interest Rates -----------------------------------------------------
rates <- read_xlsx("C:/Users/stsse/OneDrive/Documents/UNSW/ACTL5100/SOA_2026_Case_Study_Materials/SOA_2026_Case_Study_Materials/srcsc-2026-interest-and-inflation.xlsx")
rates <- rates[-c(1, 2),]

colnames(rates)[1] <- "year"
colnames(rates)[2] <- "inflation"
colnames(rates)[3] <- "overnight_spot"
colnames(rates)[4] <- "annual_spot"
colnames(rates)[5] <- "10year_spot"

rates$year <- as.numeric(rates$year)
rates$inflation <- as.numeric(rates$inflation)
rates$overnight_spot <- as.numeric(rates$overnight_spot)
rates$annual_spot <- as.numeric(rates$annual_spot)
rates$`10year_spot` <- as.numeric(rates$`10year_spot`)

mu_inf <- mean(rates$inflation)
var_inf <- var(rates$inflation)

mu_1_spot <- mean(rates$annual_spot)
var_1_spot <- mean(rates$annual_spot)

# Aggregate Modelling ---------------------------------------------
# Monte Carlo
MC <- 5000
years <- 10

total_claims_1yr <- numeric(MC)
total_premium_1yr <- numeric(MC)
net_revenue_1yr <- numeric(MC)

total_claims_10yr <- numeric(MC)
total_premium_10yr <- numeric(MC)
net_revenue_10yr <- numeric(MC)

# Dataframe Setup and Transformation
new_policy_sev <- clms_cargo_freq

# Change exposure to 1 (because we are assuming all policyholders
# in historical data will remain for the entire duration of 10 years)
new_policy_sev$exposure <- 1

# Precompute base premium multiplier for each policy
# Static Multiplier
new_policy_sev$static_mult <- 1

new_policy_sev$static_mult[new_policy_sev$cargo_type %in% c("gold","platinum")] <- 
  new_policy_sev$static_mult[new_policy_sev$cargo_type %in% c("gold","platinum")] *
  cargo_type_high_round

new_policy_sev$static_mult[new_policy_sev$route_risk %in% c(4,5)] <- 
  new_policy_sev$static_mult[new_policy_sev$route_risk %in% c(4,5)] * 
  route_risk_high_round

new_policy_sev$static_mult[new_policy_sev$weight > 75000] <- 
  new_policy_sev$static_mult[new_policy_sev$weight > 75000] *
  median_weight_round

new_policy_sev$static_mult[new_policy_sev$cargo_value > 90000000] <- 
  new_policy_sev$static_mult[new_policy_sev$cargo_value > 90000000] *
  mean_cargo_value_round

# Dynamic Multiplier
new_policy_sev$dyn_mult <- 1

# Add yearly columns
new_policy_sev[, paste0("n_claims_", 1:years)] <- 0
new_policy_sev[, paste0("claim_amount_", 1:years)] <- 0
new_policy_sev[, paste0("premium", 1:years)] <- 0

claims_yearly <- numeric(years)
premium_yearly <- numeric(years)
pv_claims <- numeric(years)
pv_premium <- numeric(years)
pv_salary <- numeric(years)
pv_admin  <- numeric(years)

# Rename claim count
new_policy_sev$n_claims_0 <- new_policy_sev$claim_count

# Precompute parameters
lambda_hat <- predict(freq_model, type = "response")
theta_hat <- freq_model$theta

phi <- summary(sev_glm)$dispersion
mu_all <- predict(sev_glm, newdata = new_policy_sev, type = "response")

# Simulation
set.seed(5100)

for(mc in 1:MC) {

# Generate annual inflation & spot rates for this MC
inflations <- rnorm(years, mean = mu_inf, sd = sqrt(var_inf))
spots <- rnorm(years, mean = mu_1_spot, sd = sqrt(var_1_spot))
  
for(t in 1:years) {
# Claim Frequencies
n_claims_t <- rnbinom(124752, size = theta_hat, mu = lambda_hat)
new_policy_sev[[paste0("n_claims_", t)]] <- n_claims_t

# Claim Severities
claim_amount_t <- numeric(nrow(new_policy_sev))

claim_indices <- which(n_claims_t > 0)

all_claims <- rgamma(sum(n_claims_t[claim_indices]), shape = 1/phi, scale = mu_all[rep(claim_indices, n_claims_t[claim_indices])] * phi)

claim_sums <- rowsum(all_claims, rep(claim_indices, n_claims_t[claim_indices]))

claim_amount_t[claim_indices] <- as.numeric(claim_sums)

new_policy_sev[[paste0("claim_amount_", t)]] <- claim_amount_t

# Premiums
premium_t <- base * new_policy_sev$static_mult  # only static multipliers

# Apply dynamic safety discount based on previous year
prev_claims <- if(t == 1) new_policy_sev$n_claims_0 else new_policy_sev[[paste0("n_claims_", t-1)]]
premium_t[prev_claims == 0] <- premium_t[prev_claims == 0] * safety_discount
new_policy_sev[[paste0("premium", t)]] <- premium_t

# Aggregate sums
claims_yearly[t] <- sum(claim_amount_t)

premium_yearly[t] <- sum(premium_t)

# Apply inflation
cum_inflation <- prod(1 + inflations[1:t])
claims_yearly[t] <- claims_yearly[t] * cum_inflation
premium_yearly[t] <- premium_yearly[t] * cum_inflation

# Apply discount
cum_discount <- prod(1 + spots[1:t])
pv_claims[t] <- claims_yearly[t] / cum_discount
pv_premium[t] <- premium_yearly[t] / cum_discount

pv_salary[t] <- total_salary * cum_inflation / cum_discount
pv_admin[t]  <- total_admin * cum_inflation / cum_discount

}

# Save 1-year aggregates (first year only)
total_claims_1yr[mc] <- pv_claims[1]
total_premium_1yr[mc] <- pv_premium[1]
net_revenue_1yr[mc] <- pv_premium[1] - pv_claims[1] - pv_salary[1] - pv_admin[1]

# Save 10-year aggregates (sum of 10 years)
total_claims_10yr[mc] <- sum(pv_claims)
total_premium_10yr[mc] <- sum(pv_premium)
net_revenue_10yr[mc] <- sum(pv_premium) - sum(pv_claims) - sum(pv_salary) - sum(pv_admin)
}

results <- data.frame(
  total_claims_1yr,
  total_premium_1yr,
  net_revenue_1yr,
  total_claims_10yr,
  total_premium_10yr,
  net_revenue_10yr
)

# Expected Aggregate Values, Variance, VaR and TVaR
alpha_95 <- 0.95
alpha_99 <- 0.99

# Short-Term (1Y)
# Claims
exp_claims_s <- mean(results$total_claims_1yr)
var_claims_s <- var(results$total_claims_1yr)

VaR_95_claims_s <- quantile(results$total_claims_1yr, probs = alpha_95)
TVaR_95_claims_s <- mean(results$total_claims_1yr[results$total_claims_1yr > VaR_95_claims_s])

VaR_99_claims_s <- quantile(results$total_claims_1yr, probs = alpha_99)
TVaR_99_claims_s <- mean(results$total_claims_1yr[results$total_claims_1yr > VaR_99_claims_s])

# Premium
exp_prem_s <- mean(results$total_premium_1yr)
var_prem_s <- var(results$total_premium_1yr)

VaR_95_prem_s <- quantile(results$total_premium_1yr, probs = 1 - alpha_95)
TVaR_95_prem_s <- mean(results$total_premium_1yr[results$total_premium_1yr < VaR_95_prem_s])

VaR_99_prem_s <- quantile(results$total_premium_1yr, probs = 1 - alpha_99)
TVaR_99_prem_s <- mean(results$total_premium_1yr[results$total_premium_1yr < VaR_99_prem_s])

# Net Revenue
exp_rev_s <- mean(results$net_revenue_1yr)
var_rev_s <- var(results$net_revenue_1yr)

VaR_95_rev_s <- quantile(results$net_revenue_1yr, probs = 1 - alpha_95)
TVaR_95_rev_s <- mean(results$net_revenue_1yr[results$net_revenue_1yr < VaR_95_rev_s])

VaR_99_rev_s <- quantile(results$net_revenue_1yr, probs = 1 - alpha_99)
TVaR_99_rev_s <- mean(results$net_revenue_1yr[results$net_revenue_1yr < VaR_99_rev_s])

# Long-Term (10Y)
# Claims
exp_claims_l <- mean(results$total_claims_10yr)
var_claims_l <- var(results$total_claims_10yr)

VaR_95_claims_l <- quantile(results$total_claims_10yr, probs = alpha_95)
TVaR_95_claims_l <- mean(results$total_claims_10yr[results$total_claims_10yr > VaR_95_claims_l])

VaR_99_claims_l <- quantile(results$total_claims_10yr, probs = alpha_99)
TVaR_99_claims_l <- mean(results$total_claims_10yr[results$total_claims_10yr > VaR_99_claims_l])

# Premium
exp_prem_l <- mean(results$total_premium_10yr)
var_prem_l <- var(results$total_premium_10yr)

VaR_95_prem_l <- quantile(results$total_premium_10yr, probs = 1 - alpha_95)
TVaR_95_prem_l <- mean(results$total_premium_10yr[results$total_premium_10yr < VaR_95_prem_l])

VaR_99_prem_l <- quantile(results$total_premium_10yr, probs = 1 - alpha_99)
TVaR_99_prem_l <- mean(results$total_premium_10yr[results$total_premium_10yr < VaR_99_prem_l])

# Net Revenue
exp_rev_l <- mean(results$net_revenue_10yr)
var_rev_l <- var(results$net_revenue_10yr)

VaR_95_rev_l <- quantile(results$net_revenue_10yr, probs = 1 - alpha_95)
TVaR_95_rev_l <- mean(results$net_revenue_10yr[results$net_revenue_10yr < VaR_95_rev_l])

VaR_99_rev_l <- quantile(results$net_revenue_10yr, probs = 1 - alpha_99)
TVaR_99_rev_l <- mean(results$net_revenue_10yr[results$net_revenue_10yr < VaR_99_rev_l])

# Table Format
short_term_table <- data.frame(
  Claims = c(exp_claims_s, var_claims_s,
             VaR_95_claims_s, TVaR_95_claims_s,
             VaR_99_claims_s, TVaR_99_claims_s),
  
  Premium = c(exp_prem_s, var_prem_s,
              VaR_95_prem_s, TVaR_95_prem_s,
              VaR_99_prem_s, TVaR_99_prem_s),
  
  Net_Revenue = c(exp_rev_s, var_rev_s,
                  VaR_95_rev_s, TVaR_95_rev_s,
                  VaR_99_rev_s, TVaR_99_rev_s)
)

rownames(short_term_table) <- c(
  "Expected Value",
  "Variance",
  "VaR 95%",
  "TVaR 95%",
  "VaR 99%",
  "TVaR 99%"
)

long_term_table <- data.frame(
  Claims = c(exp_claims_l, var_claims_l,
             VaR_95_claims_l, TVaR_95_claims_l,
             VaR_99_claims_l, TVaR_99_claims_l),
  
  Premium = c(exp_prem_l, var_prem_l,
              VaR_95_prem_l, TVaR_95_prem_l,
              VaR_99_prem_l, TVaR_99_prem_l),
  
  Net_Revenue = c(exp_rev_l, var_rev_l,
                  VaR_95_rev_l, TVaR_95_rev_l,
                  VaR_99_rev_l, TVaR_99_rev_l)
)

rownames(long_term_table) <- c(
  "Expected Value",
  "Variance",
  "VaR 95%",
  "TVaR 95%",
  "VaR 99%",
  "TVaR 99%"
)

short_term_table
long_term_table