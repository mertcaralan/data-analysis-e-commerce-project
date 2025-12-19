library(tidyverse)
library(stringr)

eu_countries <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia",
  "Denmark", "Estonia", "Finland", "Germany", "Greece",
  "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg",
  "Malta", "Netherlands", "Poland", "Portugal", "Romania",
  "Slovakia", "Slovenia", "Spain", "Sweden"
)

before_raw <- read_csv("internet_purchases_by_individuals_before_2020.csv")
after_raw  <- read_csv("internet_purchases_by_individuals_after_2020.csv")

netacc_raw <- read_csv("level_of_internet_access.csv")
gdp_raw    <- read_csv("real_gdp_per_capita.csv")
broad_raw  <- read_csv("broadband_internet_coverage_by_speed.csv")
skills_raw <- read_csv("individuals_level_of_digital_skills.csv")

make_ecom_12m <- function(df) {
  df <- df %>% mutate(year = as.integer(TIME_PERIOD))
  has_direct <- any(
    df$indic_is == "Last online purchase: in the 12 months" &
      df$unit == "Percentage of individuals",
    na.rm = TRUE
  )
  if (has_direct) {
    df %>%
      filter(
        indic_is == "Last online purchase: in the 12 months",
        unit == "Percentage of individuals",
        year >= 2013, year <= 2024
      ) %>%
      group_by(geo, year) %>%
      summarise(ecom_12m = mean(OBS_VALUE, na.rm = TRUE), .groups = "drop")
  } else {
    df %>%
      filter(
        indic_is %in% c(
          "Last online purchase: in the last 3 months",
          "Last online purchase: between 3 and 12 months ago"
        ),
        unit == "Percentage of individuals",
        year >= 2013, year <= 2024
      ) %>%
      group_by(geo, year) %>%
      summarise(ecom_12m = sum(OBS_VALUE, na.rm = TRUE), .groups = "drop")
  }
}

ecom_before <- make_ecom_12m(before_raw)
ecom_after  <- make_ecom_12m(after_raw)

ecom_all <- bind_rows(ecom_before, ecom_after) %>%
  arrange(geo, year) %>%
  distinct(geo, year, .keep_all = TRUE) %>%
  filter(geo %in% eu_countries)

netacc <- netacc_raw %>%
  mutate(year = as.integer(TIME_PERIOD)) %>%
  filter(year >= 2013, year <= 2024, geo %in% eu_countries) %>%
  select(geo, year, netacc_value = OBS_VALUE)

gdp <- gdp_raw %>%
  mutate(year = as.integer(TIME_PERIOD)) %>%
  filter(year >= 2013, year <= 2024, geo %in% eu_countries) %>%
  select(geo, year, gdp_value = OBS_VALUE)

broad <- broad_raw %>%
  mutate(year = as.integer(TIME_PERIOD)) %>%
  filter(year >= 2013, year <= 2024, geo %in% eu_countries) %>%
  select(geo, year, broad_value = OBS_VALUE)

skills <- skills_raw %>%
  mutate(year = as.integer(TIME_PERIOD)) %>%
  filter(year >= 2013, year <= 2019, geo %in% eu_countries) %>%
  select(geo, year, skills_value = OBS_VALUE)

merged <- ecom_all %>%
  left_join(netacc, by = c("geo", "year")) %>%
  left_join(gdp,    by = c("geo", "year")) %>%
  left_join(broad,  by = c("geo", "year")) %>%
  left_join(skills, by = c("geo", "year"))

library(viridis)

ggplot(ecom_all,
       aes(x = year,
           y = reorder(geo, ecom_12m, FUN = mean),
           fill = ecom_12m)) +
  geom_tile() +
  scale_fill_viridis(
    option = "C",
    name = "Percent"
  ) +
  theme_minimal() +
  labs(
    title = "Online purchases in the last 12 months (EU, 2013–2024)",
    x = "Year",
    y = "Country"
  )


ggplot(ecom_all, aes(x = factor(year), y = ecom_12m)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Distribution of online purchases across EU countries (2013–2024)",
    x = "Year",
    y = "Percent"
  )

ecom_last_year <- ecom_all %>%
  filter(year == max(year, na.rm = TRUE))

ggplot(ecom_last_year,
       aes(x = reorder(geo, ecom_12m), y = ecom_12m, fill = geo)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(
    title = paste0("Online purchases in the last 12 months (EU, ", max(ecom_last_year$year), ")"),
    x = "Country",
    y = "Percent"
  )

# ============================
# RELATIONSHIPS
# ============================


merged <- merged %>%
  mutate(
    ecom_12m     = as.numeric(ecom_12m),
    netacc_value = as.numeric(netacc_value),
    gdp_value    = as.numeric(gdp_value),
    broad_value  = as.numeric(broad_value),
    skills_value = as.numeric(skills_value)
  )

# Internet access vs online shopping
ggplot(merged %>% drop_na(netacc_value, ecom_12m),
       aes(x = netacc_value, y = ecom_12m)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(
    title = "Internet access vs online shopping (EU, 2013–2024)",
    x = "Internet access (%)",
    y = "Online shopping in last 12 months (%)"
  )

# GDP per capita vs online shopping
ggplot(merged %>% drop_na(gdp_value, ecom_12m),
       aes(x = gdp_value, y = ecom_12m)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(
    title = "GDP per capita vs online shopping (EU, 2013–2024)",
    x = "Real GDP per capita (PPS)",
    y = "Online shopping in last 12 months (%)"
  )

# Broadband coverage vs online shopping
ggplot(merged %>% drop_na(broad_value, ecom_12m),
       aes(x = broad_value, y = ecom_12m)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(
    title = "Broadband coverage vs online shopping (EU, 2013–2024)",
    x = "Broadband coverage (%)",
    y = "Online shopping in last 12 months (%)"
  )

rel_skills <- merged %>%
  drop_na(skills_value, ecom_12m) %>%
  filter(year <= 2019) %>%
  group_by(geo, year) %>%
  summarise(
    skills = mean(skills_value, na.rm = TRUE),
    ecom   = mean(ecom_12m, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(rel_skills, aes(x = skills, y = ecom)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
  theme_minimal() +
  labs(
    title = "Digital skills vs Online shopping (EU, 2013–2019)",
    x = "Digital skills (%)",
    y = "Online shopping in last 12 months (%)"
  )


# Korelasyon tablosu
cor_table <- merged %>%
  select(ecom_12m, netacc_value, gdp_value, broad_value, skills_value) %>%
  cor(use = "pairwise.complete.obs")

cor_table

library(psych)

numeric_vars <- merged %>%
  select(ecom_12m, netacc_value, gdp_value, broad_value, skills_value)

descriptives <- psych::describe(numeric_vars)

descriptives

write_csv(merged, "merged_dataset.csv")

# ============================
# BACK-TO-BACK (PYRAMID) PLOT
# Bins on Y-axis, counts mirrored on X-axis
# ============================

# ============================
# BACK-TO-BACK (PYRAMID) PLOT
# Bins on Y-axis, counts mirrored on X-axis
# ============================

library(tidyverse)

ecom_pyramid <- ecom_all %>%
  mutate(period = case_when(
    year <= 2019 ~ "Pre-COVID (2013–2019)",
    year >= 2020 ~ "Post-COVID (2020–2024)",
    TRUE ~ NA_character_
  )) %>%
  drop_na(period, ecom_12m) %>%
  mutate(
    ecom_12m = as.numeric(ecom_12m),
    bin = cut(
      ecom_12m,
      breaks = seq(0, 100, by = 5),
      include.lowest = TRUE,
      right = FALSE
    )
  ) %>%
  count(bin, period) %>%
  mutate(count_mirror = if_else(period == "Pre-COVID (2013–2019)", -n, n))

ggplot(ecom_pyramid, aes(x = count_mirror, y = bin, fill = period)) +
  geom_col(width = 0.9) +
  scale_x_continuous(labels = function(x) abs(x)) +
  theme_minimal() +
  labs(
    title = "Back-to-back pyramid: Online shopping adoption (EU)",
    subtitle = "Pre-COVID (2013–2019) vs Post-COVID (2020–2024)",
    x = "Number of observations (mirrored)",
    y = "Online shopping in last 12 months (%)",
    fill = "Period"
  )

