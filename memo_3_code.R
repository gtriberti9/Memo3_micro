library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(gridExtra)
library(patchwork)

########### MAIN VARIABLES ##########

# Household: household_id
# Food price: fp3_price
# Phone Survey cross section Household weight round 1, 2 ,3 .. 6: phw13, phw14, phw15...
# Food product: fp_01 (Maize grain/flour)
# Unit: fp2_unit (Quntal, Kilogram, etc)
# Quantity: fp2_quant
# Mobile signal strnght (Scale):  pho3_signal
# Mobile signal strnght (Bars): pho4_bars
# Has a phone/tablet : ed_smart
# Uses sms in phone: ed_sms
# Proportion urbar: cs4_sector

##################################################

setwd("C:/Users/HP/OneDrive/Documentos/UChicago/1. Courses/3. Spring Quarter 2025/ECON 35530 Microeconomics of Development/Memo/Memo 3/ETH_2020-2024_HFPS_v15_M_CSV")

data_micro <- read.csv("r10_wb_lsms_hfpm_hh_survey_public_microdata.csv")

data_work <- read.csv("r8_wb_lsms_hfpm_hh_survey_public_microdata.csv")


data_round11 <- read.csv("wb_lsms_hfpm_hh_survey_round11_clean_microdata.csv")
data_round13 <- read.csv("wb_lsms_hfpm_hh_survey_round13_price_public.csv")
data_round14 <- read.csv("wb_lsms_hfpm_hh_survey_round14_price_public.csv")
data_round15 <- read.csv("wb_lsms_hfpm_hh_survey_round15_price_public.csv")
data_round16 <- read.csv("wb_lsms_hfpm_hh_survey_round16_price_public.csv")
data_round17 <- read.csv("wb_lsms_hfpm_hh_survey_round17_price_public.csv")
data_round18 <- read.csv("wb_lsms_hfpm_hh_survey_round18_price_public.csv")
data_round19 <- read.csv("wb_lsms_hfpm_hh_survey_round19_price_public.csv")


###################################################
# DESCRIPTIVE STATISTICS (For data limitations)


# 1. Share of individuals with a smartphone
smartphone_stats <- data_micro %>%
  filter(!is.na(ed_smart)) %>%
  summarise(
    n = n(),
    n_with_smartphone = sum(ed_smart == 1),
    pct_with_smartphone = mean(ed_smart == 1) * 100
  )

sms_stats <- data_micro %>%
  filter(!is.na(ed_sms)) %>%
  summarise(
    n = n(),
    n_with_sms = sum(ed_sms == 1),
    pct_with_sms = mean(ed_sms == 1) * 100
  )

# 2. Urban vs Rural distribution (urban = 2, rural = 1)
urban_stats <- data_round11 %>%
  filter(!is.na(cs4_sector)) %>%
  count(cs4_sector) %>%
  mutate(
    label = case_when(
      cs4_sector == 1 ~ "Rural",
      cs4_sector == 2 ~ "Urban",
      TRUE ~ "Other"
    ),
    pct = 100 * n / sum(n)
  )

# 3. Gender (female = 2, male = 1)
gender_stats <- data_round11 %>%
  filter(!is.na(ii4_resp_gender)) %>%
  count(ii4_resp_gender) %>%
  mutate(
    label = case_when(
      ii4_resp_gender == 1 ~ "Male",
      ii4_resp_gender == 2 ~ "Female",
      TRUE ~ "Other"
    ),
    pct = 100 * n / sum(n)
  )

# 4. Work activity breakdown (exclude NA)
work_activity_stats <- data_work %>%
  filter(!is.na(em6_work_cur_act)) %>%
  count(em6_work_cur_act) %>%
  mutate(
    activity = case_when(
      em6_work_cur_act == 1 ~ "Agriculture",
      em6_work_cur_act == 2 ~ "Industry/manufacturing",
      em6_work_cur_act == 3 ~ "Wholesale/Retail trade",
      em6_work_cur_act == 4 ~ "Transport services",
      em6_work_cur_act == 5 ~ "Restaurant, hotel, bars",
      em6_work_cur_act == 6 ~ "Government",
      em6_work_cur_act == 7 ~ "Personal services",
      TRUE ~ "Other"
    ),
    pct = 100 * n / sum(n)
  )


###########################################################
# WITHIN HOUSEHOLD VARIABILITY IN PRICE

# These are approximate measures for some units. The units that could not be 
# transformed to kg are:
#   Piece/number
#   Litres (is ambiguous for grains)
#   Sini, Jog, Tasa/Tanik
#   Zelela (translates as "a bunch")
#   Birchiko (a liquid measurement)
#   Sahin (a liquid measurement)
#   Kubaya/Cup (a liquid measurement)


#--- Unit conversion dictionary ---
unit_conversion <- c(
  "Kilogram" = 1,
  "Quntal" = 100
)

#Function to clean and standardize a single round
prepare_round <- function(data, round_num) {
  data %>%
    mutate(
      fp3_price = as.numeric(fp3_price),
      fp2_quant = as.numeric(fp2_quant),
      fp2_unit = as.character(fp2_unit),
      round = round_num,
      weight = as.numeric(!!sym(paste0("phw", round_num)))  # Sampling weight
    ) %>%
    filter(fp_01 == "Maize grain/flour") %>%
    select(household_id, fp3_price, fp2_unit, fp2_quant, weight, round)
}

#--- Prepare all rounds ---
price_data <- bind_rows(
  prepare_round(data_round13, 13),
  prepare_round(data_round14, 14),
  prepare_round(data_round15, 15),
  prepare_round(data_round16, 16),
  prepare_round(data_round17, 17),
  prepare_round(data_round18, 18),
  prepare_round(data_round19, 19)
)

#--- Convert to price per kg and remove invalid or extreme values ---
price_data_clean <- price_data %>%
  mutate(
    unit_kg = unit_conversion[fp2_unit],
    total_kg = case_when(
      fp2_unit == "Quntal" & fp2_quant == 100 ~ 100,
      !is.na(unit_kg) ~ fp2_quant * unit_kg,
      TRUE ~ NA_real_
    ),
    price_per_kg = fp3_price / total_kg
  ) %>%
  filter(!is.na(price_per_kg), price_per_kg > 0) %>%
  group_by(round) %>%
  filter(
    price_per_kg >= quantile(price_per_kg, 0.01, na.rm = TRUE),
    price_per_kg <= quantile(price_per_kg, 0.99, na.rm = TRUE)
  ) %>%
  ungroup()



##################################################
#   The distribution of within-household 
#   variability across survey round in Maize 
#   grain/flour price (collected in round 13-19)
##################################################

#--- Compute within-household price variability ---
household_variability <- price_data_clean %>%
  group_by(household_id) %>%
  summarise(
    n_rounds = n(),
    mean_price = weighted.mean(price_per_kg, weight, na.rm = TRUE),
    price_sd = ifelse(n_rounds > 1,
                      sqrt(weighted.mean((price_per_kg - mean_price)^2, weight, na.rm = TRUE)),
                      0),
    price_cv = ifelse(mean_price > 0, price_sd / mean_price, NA),
    .groups = 'drop'
  ) %>%
  filter(n_rounds >= 2)

#--- Summary statistics per round ---
variability_by_round <- price_data_clean %>%
  group_by(round) %>%
  summarise(
    n_households = n_distinct(household_id),
    mean_price = weighted.mean(price_per_kg, weight, na.rm = TRUE),
    weighted_sd = sqrt(weighted.mean((price_per_kg - mean_price)^2, weight, na.rm = TRUE)),
    cv = weighted_sd / mean_price,
    .groups = 'drop'
  )

#--- Plot: Coefficient of Variation across rounds ---
ggplot(variability_by_round, aes(x = factor(round), y = cv)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  geom_text(aes(label = paste("n =", n_households)), 
            vjust = -0.5, size = 3) +
  labs(
    title = "Maize Price Variability Across Survey Rounds",
    subtitle = "Coefficient of Variation (weighted)",
    x = "Survey Round",
    y = "Coefficient of Variation"
  ) +
  theme_minimal()

#--- Keep households that appear in at least 2 rounds ---
within_variability <- price_data_clean %>%
  group_by(household_id) %>%
  filter(n() >= 2) %>%
  summarise(
    mean_price = weighted.mean(price_per_kg, weight, na.rm = TRUE),
    sd_price = sqrt(weighted.mean((price_per_kg - mean_price)^2, weight, na.rm = TRUE)),
    cv_price = ifelse(mean_price > 0, sd_price / mean_price, NA),
    n_rounds = n(),
    .groups = 'drop'
  )

# --- Plot distribution of household-level CVs ---
ggplot(within_variability, aes(x = cv_price)) +
  geom_histogram(binwidth = 0.05, fill = "steelblue", alpha = 0.8) +
  labs(
    title = "Distribution of Within-Household Price Variability",
    subtitle = "Coefficient of Variation in Maize Prices Across Rounds (weighted)",
    x = "Coefficient of Variation (CV)",
    y = "Number of Households"
  ) +
  theme_minimal()

# Round-level CV and mean
variability_by_round <- price_data_clean %>%
  group_by(round) %>%
  summarise(
    n = n(),
    n_households = n_distinct(household_id),
    mean_price = weighted.mean(price_per_kg, weight, na.rm = TRUE),
    se_price = sqrt(weighted.mean((price_per_kg - mean_price)^2, weight, na.rm = TRUE)) / sqrt(n),
    weighted_sd = sqrt(weighted.mean((price_per_kg - mean_price)^2, weight, na.rm = TRUE)),
    cv = weighted_sd / mean_price,
    .groups = 'drop'
  )


# Keep households observed in at least 2 rounds
within_variability <- price_data_clean %>%
  group_by(household_id) %>%
  filter(n() >= 2) %>%
  summarise(
    mean_price = weighted.mean(price_per_kg, weight, na.rm = TRUE),
    sd_price = sqrt(weighted.mean((price_per_kg - mean_price)^2, weight, na.rm = TRUE)),
    cv_price = ifelse(mean_price > 0, sd_price / mean_price, NA),
    n_rounds = n(),
    .groups = 'drop'
  )

# Panel 1: Round-level CV
p1 <- ggplot(variability_by_round, aes(x = factor(round), y = mean_price)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  geom_errorbar(aes(ymin = mean_price - se_price, ymax = mean_price + se_price), width = 0.2) +
  labs(
    title = "Mean Maize Price Across Rounds",
    subtitle = "With standard error (weighted)",
    x = "Round",
    y = "Mean Price per Kg"
  ) +
  theme_minimal()

# Panel 2: Within-household CV
p2 <- ggplot(within_variability, aes(x = cv_price)) +
  geom_histogram(binwidth = 0.05, fill = "darkorange", color = "white", alpha = 0.8) +
  labs(
    title = "Within-Household Price Variability",
    subtitle = "Coefficient of Variation across rounds (households with ≥ 2 rounds)",
    x = "Coefficient of Variation (CV)",
    y = "Number of Households"
  ) +
  theme_minimal()

# Combine both plots
p1 + p2 + plot_layout(ncol = 1, heights = c(1, 1.2))


#Checks
price_data_clean %>%
  group_by(round) %>%
  summarise(n_obs = n())

price_data_clean %>%
  group_by(round) %>%
  summarise(
    mean_price = weighted.mean(price_per_kg, weight),
    sd_price = sqrt(weighted.mean((price_per_kg - mean_price)^2, weight))
  )

geom_errorbar(aes(ymin = mean_price - 2*se_price, ymax = mean_price + 2*se_price), width = 0.2)


##################################################
#   The relationship between maize price 
#   variability and mobile phone signal strength 
#   (collected only in round 11 of the survey)
##################################################

# 1. Calculate price variability per household (from rounds 13–19)
price_variability <- price_data_clean %>%
  filter(round %in% 13:19) %>%
  group_by(household_id) %>%
  summarise(
    mean_price = mean(price_per_kg, na.rm = TRUE),
    sd_price = sd(price_per_kg, na.rm = TRUE),
    cv_price = ifelse(mean_price > 0, sd_price / mean_price, NA),
    n_rounds = n(),
    .groups = "drop"
  ) %>%
  filter(n_rounds >= 2, !is.na(cv_price))

# 2. Extract mobile phone signal data from round 11
signal_data <- data_round11 %>%
  select(household_id, pho4_bars, pho3_signal) %>%
  distinct()

# 3. Merge with price variability data
price_signal <- price_variability %>%
  left_join(signal_data, by = "household_id") %>%
  filter(!is.na(pho4_bars) | !is.na(pho3_signal))

# 4. Plot relationship between signal strength and price variability (using pho4_bars)
ggplot(price_signal, aes(x = as.factor(pho4_bars), y = cv_price)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  labs(
    title = "Maize Price Variability vs. Mobile Signal Strength",
    subtitle = "Households with price data (rounds 13–19) and signal data (round 11)",
    x = "Mobile Signal Bars (pho4_bars)",
    y = "Price per kg Coefficient of Variation"
  ) +
  theme_minimal()

ggplot(price_signal, aes(x = pho3_signal, y = cv_price)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(
    title = "Relationship Between Signal Quality and Price Variability",
    x = "Signal Quality (pho3_signal)",
    y = "Price Coefficient of Variation"
  ) +
  theme_minimal()




