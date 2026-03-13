# ACTL5100 Cargo Loss Data Cleaner
library(readxl)
clms_cargo_sheets <- excel_sheets("C:/Users/stsse/OneDrive/Documents/UNSW/ACTL5100/SOA_2026_Case_Study_Materials/SOA_2026_Case_Study_Materials/srcsc-2026-claims-cargo.xlsx")
clms_cargo_all <- lapply(clms_cargo_sheets, function(x){
  read_excel("C:/Users/stsse/OneDrive/Documents/UNSW/ACTL5100/SOA_2026_Case_Study_Materials/SOA_2026_Case_Study_Materials/srcsc-2026-claims-cargo.xlsx",
             sheet = x)
})
clms_cargo_freq <- clms_cargo_all[[1]]
clms_cargo_sev <- clms_cargo_all[[2]]

library(dplyr)
library(stringr)
# Cleaning idenifier names
clms_cargo_freq <- clms_cargo_freq %>%
  mutate(
    policy_id = substr(trimws(as.character(policy_id)), 1, 9),
    shipment_id = substr(trimws(as.character(shipment_id)), 1, 8),
    cargo_type = str_remove(cargo_type, "_.*"),
    container_type = str_remove(container_type, "_.*")
  )

clms_cargo_sev <- clms_cargo_sev %>%
  mutate(
    claim_id = substr(trimws(as.character(claim_id)), 1, 13),
    policy_id = substr(trimws(as.character(policy_id)), 1, 9),
    shipment_id = substr(trimws(as.character(shipment_id)), 1, 8),
    cargo_type = str_remove(cargo_type, "_.*"),
    container_type = str_remove(container_type, "_.*")
  )

# Checks
unique(nchar(clms_cargo_freq$policy_id))
unique(nchar(clms_cargo_freq$shipment_id))
list(unique(clms_cargo_freq$cargo_type))
list(unique(clms_cargo_freq$container_type))
unique(nchar(clms_cargo_sev$claim_id))
unique(nchar(clms_cargo_sev$policy_id))
unique(nchar(clms_cargo_sev$shipment_id))
list(unique(clms_cargo_sev$cargo_type))
list(unique(clms_cargo_sev$container_type))

# All qualitative data names cleaned

# Fill in missing identifiers via matching across freq and sev
# Matching data
# Matching ids from freq to sev
id_pairs_freq <- clms_cargo_freq %>%
  distinct(policy_id, shipment_id)

policy_to_shipment_freq <- id_pairs_freq %>%
  group_by(policy_id) %>%
  filter(n() == 1) %>%
  ungroup()

clms_cargo_sev <- clms_cargo_sev %>%
  left_join(policy_to_shipment_freq,
            by = "policy_id",
            suffix = c("", "_from_pol")) %>%
  mutate(
    shipment_id = coalesce(shipment_id, shipment_id_from_pol)
  ) %>%
  select(-shipment_id_from_pol)

shipment_to_policy_freq <- id_pairs_freq %>%
  group_by(shipment_id) %>%
  filter(n() == 1) %>%
  ungroup()

clms_cargo_sev <- clms_cargo_sev %>%
  left_join(shipment_to_policy_freq,
            by = "shipment_id",
            suffix = c("", "_from_ship")) %>%
  mutate(
    policy_id = coalesce(policy_id, policy_id_from_ship)
  ) %>%
  select(-policy_id_from_ship)

# Matching ids from sev to freq
id_pairs_sev <- clms_cargo_sev %>%
  distinct(policy_id, shipment_id)

policy_to_shipment_sev <- id_pairs_sev %>%
  group_by(policy_id) %>%
  filter(n() == 1) %>%
  ungroup()

clms_cargo_freq <- clms_cargo_freq %>%
  left_join(policy_to_shipment_sev,
            by = "policy_id",
            suffix = c("", "_from_pol")) %>%
  mutate(
    shipment_id = coalesce(shipment_id, shipment_id_from_pol)
  ) %>%
  select(-shipment_id_from_pol)

shipment_to_policy_sev <- id_pairs_sev %>%
  group_by(shipment_id) %>%
  filter(n() == 1) %>%
  ungroup()

clms_cargo_freq <- clms_cargo_freq %>%
  left_join(shipment_to_policy_sev,
            by = "shipment_id",
            suffix = c("", "_from_ship")) %>%
  mutate(
    policy_id = coalesce(policy_id, policy_id_from_ship)
  ) %>%
  select(-policy_id_from_ship)

# Manual matching for sev
clms_cargo_sev$shipment_id[17233] <- "S-796804"
clms_cargo_sev$shipment_id[26326] <- "S-896387"
clms_cargo_sev$shipment_id[25608] <- "S-088755"
clms_cargo_sev$shipment_id[6063] <- "S-149455"

clms_cargo_sev$policy_id[6777] <- "CL-549777"
clms_cargo_sev$policy_id[17216] <- "CL-269506"
clms_cargo_sev$policy_id[5242] <- "CL-925439"
clms_cargo_sev$policy_id[11828] <- "CL-383625"
clms_cargo_sev$policy_id[24514] <- "CL-437917"
clms_cargo_sev$policy_id[11887] <- "CL-329223"
clms_cargo_sev$policy_id[18473] <- "CL-387603"
clms_cargo_sev$policy_id[24642] <- "CL-096861"
clms_cargo_sev$policy_id[28784] <- "CL-656944"

# Note that there are some shipments with multiple policies (Many insurers share cargo)
# and some policies with multiple shipments (Split up their shipments)
# Both are reasonable assumptions and won't be cleaned

# How to clean freq ids if there are no clms i.e. no obs in sev?
# We can assume these are all different, give placeholder text and keep them
# They influence average claim frequency

# Giving Placeholder IDs
clms_cargo_freq <- clms_cargo_freq %>%
  mutate(
    policy_id = ifelse(
      is.na(policy_id),
      paste0("MISSING_POL_", row_number()),
      policy_id
    ),
    shipment_id = ifelse(
      is.na(shipment_id),
      paste0("MISSING_SHIP_", row_number()),
      shipment_id
    )
  )

clms_cargo_sev <- clms_cargo_sev %>%
  mutate(
    claim_id = ifelse(
      is.na(claim_id),
      paste0("MISSING_CLM_", row_number()),
      claim_id
    )
  )

# Matching IDs with cargo and container
cargo_map_freq <- clms_cargo_freq %>%
  distinct(policy_id, shipment_id, cargo_type, container_type) %>%
  group_by(policy_id, shipment_id) %>%
  filter(n() == 1) %>%
  ungroup()

clms_cargo_sev <- clms_cargo_sev %>%
  left_join(
    cargo_map_freq,
    by = c("policy_id", "shipment_id"),
    suffix = c("", "_from_freq")
  ) %>%
  mutate(
    cargo_type = coalesce(cargo_type, cargo_type_from_freq),
    container_type = coalesce(container_type, container_type_from_freq)
  ) %>%
  select(-cargo_type_from_freq, -container_type_from_freq)

cargo_map_sev <- clms_cargo_sev %>%
  distinct(policy_id, shipment_id, cargo_type, container_type) %>%
  group_by(policy_id, shipment_id) %>%
  filter(n() == 1) %>%
  ungroup()

clms_cargo_freq <- clms_cargo_freq %>%
  left_join(
    cargo_map_sev,
    by = c("policy_id", "shipment_id"),
    suffix = c("", "_from_sev")
  ) %>%
  mutate(
    cargo_type = coalesce(cargo_type, cargo_type_from_sev),
    container_type = coalesce(container_type, container_type_from_sev)
  ) %>%
  select(-cargo_type_from_sev, -container_type_from_sev)

# Cleaning claim counts
sum(is.na(clms_cargo_sev$claim_amount))
44/30650
# Only 0.14% of data with missing claim amount, remove them
clms_cargo_sev_na_omit <- clms_cargo_sev %>%
  filter(!is.na(claim_amount))

clms_cargo_freq <- clms_cargo_freq %>%
  left_join(
    clms_cargo_sev_na_omit %>%
      count(policy_id, shipment_id, cargo_type, container_type, name = "obs_clm"),
    by = c("policy_id", "shipment_id", "cargo_type", "container_type")
  ) %>%
  mutate(
    claim_count = ifelse(is.na(obs_clm), 0, obs_clm)
  ) %>%
  select(-obs_clm)

# Cleaning numerical variables
# Matching Values in freq and sev
clms_cargo_com <- clms_cargo_freq %>%
  left_join(
    clms_cargo_sev,
    by = c("policy_id", "shipment_id"),
    suffix = c("_freq", "_sev")
  )

reconcile_numeric <- function(freq, sev, min_val, max_val) {
  
  freq_valid <- !is.na(freq) & freq >= min_val & freq <= max_val
  sev_valid  <- !is.na(sev)  & sev  >= min_val & sev  <= max_val
  
  case_when(
    # both valid and equal
    freq_valid & sev_valid & freq == sev ~ freq,
    
    # only freq valid
    freq_valid & !sev_valid ~ freq,
    
    # only sev valid
    !freq_valid & sev_valid ~ sev,
    
    # both valid but different
    freq_valid & sev_valid & freq != sev ~ (freq + sev) / 2,
    
    # neither valid
    TRUE ~ NA_real_
  )
}

reconcile_route <- function(freq, sev) {
  
  freq_valid <- freq %in% 1:5
  sev_valid  <- sev  %in% 1:5
  
  case_when(
    freq_valid & sev_valid & freq == sev ~ freq,
    freq_valid & !sev_valid ~ freq,
    !freq_valid & sev_valid ~ sev,
    freq_valid & sev_valid & freq != sev ~ round((freq + sev)/2),
    TRUE ~ NA_real_
  )
}

clms_cargo_com <- clms_cargo_com %>%
  mutate(
    cargo_value = reconcile_numeric(cargo_value_freq, cargo_value_sev, 50000, 680000000),
    weight = reconcile_numeric(weight_freq, weight_sev, 1500, 250000),
    route_risk = reconcile_route(route_risk_freq, route_risk_sev),
    distance = reconcile_numeric(distance_freq, distance_sev, 1, 100),
    transit_duration = reconcile_numeric(transit_duration_freq, transit_duration_sev, 1, 60),
    pilot_experience = reconcile_numeric(pilot_experience_freq, pilot_experience_sev, 1, 30),
    vessel_age = reconcile_numeric(vessel_age_freq, vessel_age_sev, 1, 50),
    solar_radiation = reconcile_numeric(solar_radiation_freq, solar_radiation_sev, 0, 1),
    debris_density = reconcile_numeric(debris_density_freq, debris_density_sev, 0, 1),
    exposure = reconcile_numeric(exposure_freq, exposure_sev, 0, 1)
  )

updated_vals <- clms_cargo_com %>%
  select(
    policy_id, shipment_id,cargo_value, weight, route_risk, distance, transit_duration, 
    pilot_experience, vessel_age, solar_radiation, debris_density, exposure
  )

updated_vals_unique <- updated_vals %>%
  group_by(policy_id, shipment_id) %>%
  summarise(
    cargo_value = mean(cargo_value, na.rm = TRUE),
    weight = mean(weight, na.rm = TRUE),
    route_risk = round(mean(route_risk, na.rm = TRUE)),
    distance = mean(distance, na.rm = TRUE),
    transit_duration = mean(transit_duration, na.rm = TRUE),
    pilot_experience = mean(pilot_experience, na.rm = TRUE),
    vessel_age = mean(vessel_age, na.rm = TRUE),
    solar_radiation = mean(solar_radiation, na.rm = TRUE),
    debris_density = mean(debris_density, na.rm = TRUE),
    exposure = mean(exposure, na.rm = TRUE),
    .groups = "drop"
  )

clms_cargo_freq <- clms_cargo_freq %>%
  select(-cargo_value, -weight, -route_risk, -distance,
         -transit_duration, -pilot_experience, -vessel_age,
         -solar_radiation, -debris_density, -exposure) %>%
  left_join(updated_vals_unique, by = c("policy_id", "shipment_id"))

clms_cargo_sev <- clms_cargo_sev %>%
  select(-cargo_value, -weight, -route_risk, -distance,
         -transit_duration, -pilot_experience, -vessel_age,
         -solar_radiation, -debris_density, -exposure) %>%
  left_join(updated_vals_unique, by = c("policy_id", "shipment_id"))

# Renumbering claim_seq
clms_cargo_sev <- clms_cargo_sev %>%
  arrange(policy_id, shipment_id) %>%
  group_by(policy_id, shipment_id) %>%
  mutate(
    claim_seq = row_number()
  ) %>%
  ungroup()

# Final checks before estimations
clms_cargo_freq$shipment_id[45395] <- "S-582053"
clms_cargo_freq$claim_count[45395] <- 1
clms_cargo_sev$cargo_value[5637] <- 5200000
clms_cargo_sev$weight[5637] <- 100000
clms_cargo_sev$route_risk[5637] <- 2
clms_cargo_sev$distance[5637] <- 38.25
clms_cargo_sev$transit_duration[5637] <- 21.377
clms_cargo_sev$pilot_experience[5637] <- 19.148
clms_cargo_sev$vessel_age[5637] <- 22.641
clms_cargo_sev$solar_radiation[5637] <- 0.228
clms_cargo_sev$debris_density[5637] <- 0.121
clms_cargo_sev$exposure[5637] <- 0.238

clms_cargo_freq$weight[31340] <- 10000 # too mismatched 1970419 vs 0.00001, 10000 from similar shipments
clms_cargo_sev$weight[22615] <- 10000

clms_cargo_freq$route_risk[10137] <- 3 # -3
clms_cargo_sev$route_risk[24036] <- 3

clms_cargo_freq$distance[56148] <- 100 # 105.83
clms_cargo_sev$distance[5328] <- 100
clms_cargo_freq$distance[80059] <- 100 # 102.41
clms_cargo_sev$distance[9964] <- 100
clms_cargo_sev$distance[9965] <- 100
clms_cargo_freq$distance[83789] <- 100 # 101.7
clms_cargo_sev$distance[18789] <- 100
clms_cargo_sev$distance[18790] <- 100
clms_cargo_sev$distance[18791] <- 100

clms_cargo_freq$transit_duration[50201] <- 14.651 # only freq with -14.651
clms_cargo_sev$transit_duration[28966] <- 14.651

# Clean remaining cargo types by matching value and weight
cargo_lookup <- bind_rows(
  clms_cargo_freq %>% select(cargo_value, weight, cargo_type),
  clms_cargo_sev  %>% select(cargo_value, weight, cargo_type)
) %>%
  filter(!is.na(cargo_type)) %>%
  group_by(cargo_value, weight) %>%
  summarise(
    cargo_type = first(cargo_type),
    .groups = "drop"
  )

clms_cargo_freq <- clms_cargo_freq %>%
  left_join(cargo_lookup,
            by = c("cargo_value", "weight"),
            relationship = "many-to-one",
            suffix = c("", "_lookup")) %>%
  mutate(
    cargo_type = coalesce(cargo_type, cargo_type_lookup)
  ) %>%
  select(-cargo_type_lookup)

# Clean cargo value by matching type and weight
cargo_val_lookup <- bind_rows(
  clms_cargo_freq %>% select(cargo_value, weight, cargo_type),
  clms_cargo_sev  %>% select(cargo_value, weight, cargo_type)
) %>%
  filter(!is.na(cargo_value)) %>%
  group_by(cargo_type, weight) %>%
  summarise(
    cargo_value = first(cargo_value),
    .groups = "drop"
  )

clms_cargo_freq <- clms_cargo_freq %>%
  left_join(cargo_val_lookup,
            by = c("cargo_type", "weight"),
            relationship = "many-to-one",
            suffix = c("", "_lookup")) %>%
  mutate(
    cargo_value = coalesce(cargo_value, cargo_value_lookup)
  ) %>%
  select(-cargo_value_lookup)

# Clean weight by matching cargo type and value
weight_lookup <- bind_rows(
  clms_cargo_freq %>% select(cargo_value, weight, cargo_type),
  clms_cargo_sev  %>% select(cargo_value, weight, cargo_type)
) %>%
  filter(!is.na(weight)) %>%
  group_by(cargo_type, cargo_value) %>%
  summarise(
    weight = first(weight),
    .groups = "drop"
  )

clms_cargo_freq <- clms_cargo_freq %>%
  left_join(weight_lookup,
            by = c("cargo_type", "cargo_value"),
            relationship = "many-to-one",
            suffix = c("", "_lookup")) %>%
  mutate(
    weight = coalesce(weight, weight_lookup)
  ) %>%
  select(-weight_lookup)

# Still row 54530 to estimate weight due to non-similar properties
clms_cargo_freq$weight[54530] <- mean(clms_cargo_freq$weight[clms_cargo_freq$cargo_type == "rare earths"], na.rm = TRUE)

# Look at correlations between all other numerical variables 
# to observe any potentially influential behaviour on distributions
numeric_data <- clms_cargo_freq %>%
  select(where(is.numeric))

cor_matrix <- cor(numeric_data, use = "complete.obs")

cor_matrix

# Possible influences for:
# Route Risk - Debris and Solar
# Distance - Transit, Debris and Solar

# Filling in route risk
route_risk_lookup <- clms_cargo_freq %>%
  filter(!is.na(route_risk)) %>%
  group_by(cargo_type, cargo_value, weight) %>%
  summarise(
    n_rr = n(),
    mean_rr = mean(route_risk),
    median_rr = median(route_risk),
    sd_rr = sd(route_risk),
    .groups = "drop"
  )

# Appears reasonable to use median 
# (even without considerations for Solar and Debris)
clms_cargo_freq <- clms_cargo_freq %>%
  left_join(route_risk_lookup,
            by = c("cargo_type", "cargo_value", "weight"),
            relationship = "many-to-one") %>%
  mutate(
    route_risk = ifelse(
      is.na(route_risk),
      median_rr,
      route_risk
    )
  ) %>%
  select(-n_rr, -mean_rr, -median_rr, -sd_rr)

# Rows 13086 and 17268 based on only cargo_type
clms_cargo_freq$route_risk[13086] <- median(clms_cargo_freq$route_risk[clms_cargo_freq$cargo_type == "lithium"], na.rm = TRUE)
clms_cargo_freq$route_risk[17268] <- median(clms_cargo_freq$route_risk[clms_cargo_freq$cargo_type == "supplies"], na.rm = TRUE)

# Filling in distance
distance_lookup <- clms_cargo_freq %>%
  filter(!is.na(distance)) %>%
  group_by(cargo_type, cargo_value, weight) %>%
  summarise(
    n_dis = n(),
    mean_dis = mean(distance),
    median_dis = median(distance),
    sd_dis = sd(distance),
    .groups = "drop"
  )

# Due to generally large sample sizes, mean is used
clms_cargo_freq <- clms_cargo_freq %>%
  left_join(distance_lookup,
            by = c("cargo_type", "cargo_value", "weight"),
            relationship = "many-to-one") %>%
  mutate(
    distance = ifelse(
      is.na(distance),
      mean_dis,
      distance
    )
  ) %>%
  select(-n_dis, -mean_dis, -median_dis, -sd_dis)

# Clean transit duration
transit_lookup <- clms_cargo_freq %>%
  filter(!is.na(transit_duration)) %>%
  group_by(cargo_type, cargo_value, weight) %>%
  summarise(
    n_transit = n(),
    mean_transit = mean(transit_duration),
    median_transit = median(transit_duration),
    sd_transit = sd(transit_duration),
    .groups = "drop"
  )

clms_cargo_freq <- clms_cargo_freq %>%
  mutate(
    distance_band = ceiling(distance / 10) * 10
  )

transit_dis_lookup <- clms_cargo_freq %>%
  filter(!is.na(transit_duration)) %>%
  group_by(cargo_type, cargo_value, weight, distance_band) %>%
  summarise(
    n_transit = n(),
    mean_transit = mean(transit_duration),
    median_transit = median(transit_duration),
    sd_transit = sd(transit_duration),
    .groups = "drop"
  )

var(transit_lookup$sd_transit[transit_lookup$n_transit>31], na.rm = TRUE)
var(transit_dis_lookup$sd_transit[transit_dis_lookup$n_transit>31], na.rm = TRUE)

# Will use distributions determined without distance due to lower variance
clms_cargo_freq <- clms_cargo_freq %>%
  left_join(transit_lookup,
            by = c("cargo_type", "cargo_value", "weight"),
            relationship = "many-to-one") %>%
  mutate(
    transit_duration = ifelse(
      is.na(transit_duration),
      mean_transit,
      transit_duration
    )
  ) %>%
  select(-n_transit, -mean_transit, -median_transit, -sd_transit)

# Clean pilot
pilot_lookup <- clms_cargo_freq %>%
  filter(!is.na(pilot_experience)) %>%
  group_by(cargo_type, cargo_value, weight) %>%
  summarise(
    n_pil = n(),
    mean_pil = mean(pilot_experience),
    median_pil = median(pilot_experience),
    sd_pil = sd(pilot_experience),
    .groups = "drop"
  )

# Use mean
clms_cargo_freq <- clms_cargo_freq %>%
  left_join(pilot_lookup,
            by = c("cargo_type", "cargo_value", "weight"),
            relationship = "many-to-one") %>%
  mutate(
    pilot_experience = ifelse(
      is.na(pilot_experience),
      mean_pil,
      pilot_experience
    )
  ) %>%
  select(-n_pil, -mean_pil, -median_pil, -sd_pil)

# Row 303 missing, will give average based on cargo value
clms_cargo_freq$pilot_experience[303] <- 14.90811

# Clean Vessel Age
vessel_lookup <- clms_cargo_freq %>%
  filter(!is.na(vessel_age)) %>%
  group_by(cargo_type, cargo_value, weight) %>%
  summarise(
    n_vessel = n(),
    mean_vessel = mean(vessel_age),
    median_vessel = median(vessel_age),
    sd_vessel = sd(vessel_age),
    .groups = "drop"
  )

clms_cargo_freq <- clms_cargo_freq %>%
  left_join(vessel_lookup,
            by = c("cargo_type", "cargo_value", "weight"),
            relationship = "many-to-one") %>%
  mutate(
    vessel_age = ifelse(
      is.na(vessel_age),
      mean_vessel,
      vessel_age
    )
  ) %>%
  select(-n_vessel, -mean_vessel, -median_vessel, -sd_vessel)

# Clean Solar Radiation
solar_lookup <- clms_cargo_freq %>%
  filter(!is.na(solar_radiation)) %>%
  group_by(cargo_type, cargo_value, weight) %>%
  summarise(
    n_sol = n(),
    mean_sol = mean(solar_radiation),
    median_sol = median(solar_radiation),
    sd_sol = sd(solar_radiation),
    .groups = "drop"
  )

solar_rr_lookup <- clms_cargo_freq %>%
  filter(!is.na(solar_radiation)) %>%
  group_by(cargo_type, cargo_value, weight, route_risk) %>%
  summarise(
    n_sol = n(),
    mean_sol = mean(solar_radiation),
    median_sol = median(solar_radiation),
    sd_sol = sd(solar_radiation),
    .groups = "drop"
  )

solar_dis_lookup <- clms_cargo_freq %>%
  filter(!is.na(solar_radiation)) %>%
  group_by(cargo_type, cargo_value, weight, distance_band) %>%
  summarise(
    n_sol = n(),
    mean_sol = mean(solar_radiation),
    median_sol = median(solar_radiation),
    sd_sol = sd(solar_radiation),
    .groups = "drop"
  )

solar_rr_dis_lookup <- clms_cargo_freq %>%
  filter(!is.na(solar_radiation)) %>%
  group_by(cargo_type, cargo_value, weight, route_risk, distance_band) %>%
  summarise(
    n_sol = n(),
    mean_sol = mean(solar_radiation),
    median_sol = median(solar_radiation),
    sd_sol = sd(solar_radiation),
    .groups = "drop"
  )

var(solar_lookup$sd_sol[solar_lookup$n_sol>31], na.rm = TRUE)
var(solar_rr_lookup$sd_sol[solar_rr_lookup$n_sol>31], na.rm = TRUE)
var(solar_dis_lookup$sd_sol[solar_dis_lookup$n_sol>31], na.rm = TRUE)
var(solar_rr_dis_lookup$sd_sol[solar_rr_dis_lookup$n_sol>31], na.rm = TRUE)

# Lowest variance is only cargo type, value and weight
clms_cargo_freq <- clms_cargo_freq %>%
  left_join(solar_lookup,
            by = c("cargo_type", "cargo_value", "weight"),
            relationship = "many-to-one") %>%
  mutate(
    solar_radiation = ifelse(
      is.na(solar_radiation),
      mean_sol,
      solar_radiation
    )
  ) %>%
  select(-n_sol, -mean_sol, -median_sol, -sd_sol)

# 52471 missing, give mean of same cargo type and weight and value = 600000
clms_cargo_freq$solar_radiation[52471] <- 0.2442433

# Clean Debris Density
debris_lookup <- clms_cargo_freq %>%
  filter(!is.na(debris_density)) %>%
  group_by(cargo_type, cargo_value, weight) %>%
  summarise(
    n_deb = n(),
    mean_deb = mean(debris_density),
    median_deb = median(debris_density),
    sd_deb = sd(debris_density),
    .groups = "drop"
  )

debris_rr_lookup <- clms_cargo_freq %>%
  filter(!is.na(debris_density)) %>%
  group_by(cargo_type, cargo_value, weight, route_risk) %>%
  summarise(
    n_deb = n(),
    mean_deb = mean(debris_density),
    median_deb = median(debris_density),
    sd_deb = sd(debris_density),
    .groups = "drop"
  )

debris_dis_lookup <- clms_cargo_freq %>%
  filter(!is.na(debris_density)) %>%
  group_by(cargo_type, cargo_value, weight, distance_band) %>%
  summarise(
    n_deb = n(),
    mean_deb = mean(debris_density),
    median_deb = median(debris_density),
    sd_deb = sd(debris_density),
    .groups = "drop"
  )

debris_rr_dis_lookup <- clms_cargo_freq %>%
  filter(!is.na(debris_density)) %>%
  group_by(cargo_type, cargo_value, weight, route_risk, distance_band) %>%
  summarise(
    n_deb = n(),
    mean_deb = mean(debris_density),
    median_deb = median(debris_density),
    sd_deb = sd(debris_density),
    .groups = "drop"
  )

var(debris_lookup$sd_deb[debris_lookup$n_deb > 31], na.rm = TRUE)
var(debris_rr_lookup$sd_deb[debris_rr_lookup$n_deb > 31], na.rm = TRUE)
var(debris_dis_lookup$sd_deb[debris_dis_lookup$n_deb > 31], na.rm = TRUE)
var(debris_rr_dis_lookup$sd_deb[debris_rr_dis_lookup$n_deb > 31], na.rm = TRUE)

clms_cargo_freq <- clms_cargo_freq %>%
  left_join(debris_lookup,
            by = c("cargo_type", "cargo_value", "weight"),
            relationship = "many-to-one") %>%
  mutate(
    debris_density = ifelse(
      is.na(debris_density),
      mean_deb,
      debris_density
    )
  ) %>%
  select(-n_deb, -mean_deb, -median_deb, -sd_deb)

# 51010 missing, give same mean as same weight, cargo type and value = 7380000
clms_cargo_freq$debris_density[51010] <- 0.242277

# Clean exposure
exp_lookup <- clms_cargo_freq %>%
  filter(!is.na(exposure)) %>%
  group_by(cargo_type, cargo_value, weight) %>%
  summarise(
    n_exp = n(),
    mean_exp = mean(exposure),
    median_exp = median(exposure),
    sd_exp = sd(exposure),
    .groups = "drop"
  )

clms_cargo_freq <- clms_cargo_freq %>%
  left_join(exp_lookup,
            by = c("cargo_type", "cargo_value", "weight"),
            relationship = "many-to-one") %>%
  mutate(
    exposure = ifelse(
      is.na(exposure),
      mean_exp,
      exposure
    )
  ) %>%
  select(-n_exp, -mean_exp, -median_exp, -sd_exp)

# 230 unidentified container types
# Approx 1.8% of frequency data
# Exploratory Data Analysis for Container Type
library(ggplot2)
plot_by_container <- function(data, response_var = "container_type") {
  
  # Ensure response is factor
  data[[response_var]] <- as.factor(data[[response_var]])
  
  vars <- setdiff(names(data), response_var)
  
  for (v in vars) {
    
    # Skip ID columns if needed
    if (v %in% c("policy_id", "shipment_id")) next
    
    if (is.numeric(data[[v]])) {
      
      p <- ggplot(data, aes_string(x = v, fill = response_var)) +
        geom_density(alpha = 0.4) +
        labs(title = paste("Density of", v, "by", response_var)) +
        theme_minimal()
      
      print(p)
      
    } else if (is.factor(data[[v]]) || is.character(data[[v]])) {
      
      p <- ggplot(data, aes_string(x = v, fill = response_var)) +
        geom_bar(position = "dodge") +
        labs(title = paste("Bar Plot of", v, "by", response_var)) +
        theme_minimal()
      
      print(p)
    }
  }
}

plot_by_container(clms_cargo_freq)

# All plots show that container types behave very similarly
# regardless of variable, logistic unlikely to be useful
# NA Container Types to be removed
clms_cargo_freq <- clms_cargo_freq %>%
  filter(!is.na(container_type))

# Did not remove any non-zero claims

# Now remove NA claim amounts from original dataset
# and assume negative claim amounts are meant to be positive
clms_cargo_sev <- clms_cargo_sev %>%
  mutate(
    claim_amount = abs(claim_amount)
  ) %>%
  filter(!is.na(claim_amount))

# Number of claim amounts outside of given range
length(clms_cargo_sev$claim_amount[clms_cargo_sev$claim_amount < 31000])
length(clms_cargo_sev$claim_amount[clms_cargo_sev$claim_amount > 678000000])

# 2239/30606 approx 7% of data outside ranges, will not remove or transform
# Assume data dictionary range is incorrect

library(openxlsx)
write.xlsx(clms_cargo_freq, "cargo_freq_cln.xlsx")
write.xlsx(clms_cargo_sev, "cargo_sev_cln.xlsx")