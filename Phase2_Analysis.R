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

country_order <- ecom_all %>%
  group_by(geo) %>%
  summarise(avg_ecom = mean(ecom_12m, na.rm = TRUE)) %>%
  arrange(avg_ecom) %>%
  pull(geo)

ggplot(ecom_all,
       aes(
         x = factor(year),
         y = factor(geo, levels = country_order),
         fill = ecom_12m
       )) +
  geom_tile(color = "white", linewidth = 0.2) +
  scale_fill_viridis(
    option = "C",
    name = "Percent"
  ) +
  theme_minimal() + 
  theme(legend.position = "bottom") + 
  labs(
    title = "Temporal evolution of online shopping adoption across EU countries",
    subtitle = "Percentage of individuals who purchased online in the last 12 months (2013–2024)",
    x = "Year",
    y = "Country"
  )






library(tidyverse)

# Make sure year is ordered (not alphabetical)
ecom_all_box <- ecom_all %>%
  mutate(
    year = as.integer(year),
    year_f = factor(year, levels = sort(unique(year))),
    ecom_12m = as.numeric(ecom_12m)
  )

# Median per year (for a clean trend overlay)
year_median <- ecom_all_box %>%
  group_by(year_f) %>%
  summarise(med = median(ecom_12m, na.rm = TRUE), .groups = "drop")

ggplot(ecom_all_box, aes(x = year_f, y = ecom_12m)) +
  geom_boxplot(
    width = 0.6,
    outlier.alpha = 0.15,
    outlier.size = 1,
    linewidth = 0.6
  ) +
  geom_point(
    data = year_median,
    aes(x = year_f, y = med),
    inherit.aes = FALSE,
    size = 2.2
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Distribution of online purchases across EU countries (2013–2024)",
    subtitle = "Boxplots show yearly variability; points indicate the yearly median",
    x = "Year",
    y = "Online shopping in last 12 months (%)"
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


library(tidyverse)

rel_netacc <- merged %>%
  drop_na(netacc_value, ecom_12m) %>%
  group_by(geo, year) %>%
  summarise(
    netacc = mean(as.numeric(netacc_value), na.rm = TRUE),
    ecom   = mean(as.numeric(ecom_12m), na.rm = TRUE),
    .groups = "drop"
  )

ggplot(rel_netacc, aes(x = netacc, y = ecom)) +
  geom_point(alpha = 0.55, size = 2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Internet access is associated with online shopping adoption",
    subtitle = "EU country-year averages (2013–2024)",
    x = "Internet access (%)",
    y = "Online shopping in last 12 months (%)"
  )


library(tidyverse)

rel_gdp <- merged %>%
  drop_na(gdp_value, ecom_12m) %>%
  mutate(
    gdp = as.numeric(gdp_value),
    ecom = as.numeric(ecom_12m)
  ) %>%
  filter(gdp > 0)

ggplot(rel_gdp, aes(x = gdp, y = ecom)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1) +
  scale_x_log10() +
  theme_minimal(base_size = 12) +
  labs(
    title = "GDP per capita relates to online shopping adoption",
    subtitle = "Log scale improves readability across countries (2013–2024)",
    x = "Real GDP per capita (PPS, log scale)",
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

#----Butterfly Chart----#
ecom_butterfly <- ecom_all %>%
  filter(year %in% c(2019, 2024)) %>%
  mutate(
    value = ifelse(year == 2019, -ecom_12m, ecom_12m),
    year = factor(year)
  )

ggplot(ecom_butterfly,
       aes(x = reorder(geo, ecom_12m),
           y = value,
           fill = year)) +
  geom_col(width = 0.8) +
  coord_flip() +
  scale_y_continuous(labels = abs) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal"
  ) +
  labs(
    title = "Online purchases in the last 12 months (EU, 2019 vs 2024)",
    x = "Country",
    y = "Percent",
    fill = "Year"
  )


