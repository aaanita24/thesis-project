library(tidyr)
library(readxl)
library(dplyr)
library(tidyverse)
library(stringr)
library(readr)
library(DataExplorer)

# visualizations
library(ggplot2)
library(stargazer) # for tables
library(sf)
library(giscoR)
library(showtext)
library(scales)
library(ggforce)  # for facet_col
library(showtext) # for Google Fonts

# imputation
library(mice)
library(httr)
library(httr2)
library(jsonlite)

# this will read the index sheet
# we can see that the first two are indexes - so skip
ses_2022 <- read_excel("data/Tavole_Struttura-delle-retribuzioni_2022.xlsx",
                       skip = 2) # skip blank rows on top


all_sheets <- excel_sheets("data/Tavole_Struttura-delle-retribuzioni_2022.xlsx") 
# list of all sheets
print(ses_2022)

tav1_raw <- read_excel("data/Tavole_Struttura-delle-retribuzioni_2022.xlsx",
                       sheet = "Tav.1",
                       col_names = FALSE,
                       col_types = "text")   # force everything to text and avoid type-guessing issues



# rename
tav1_col_names <- c( "row_label", "F_educ_low", "F_educ_med", "F_educ_high", "F_total", "sep1",
                     "M_educ_low", "M_educ_med", "M_educ_high", "M_total", "sep2",
                     "T_educ_low", "T_educ_med", "T_educ_high", "T_total")

tav1_raw <- read_excel("data/Tavole_Struttura-delle-retribuzioni_2022.xlsx",
                       sheet = "Tav.1",
                       col_names = tav1_col_names,
                       col_types = "text",
                       skip = 4)

sections <- c("ATTIVITÀ ECONOMICA","ATTIVITÀ ECONOMICA ",   # ISTAT has a trailing space in some tables so ATTIVITA' ECONOMICA is repeated with a space afterwards too
              "RIPARTIZIONE GEOGRAFICA",
              "CLASSE DIMENSIONALE", 
              "TIPO DI CONTROLLO ECONOMICO")

tav1_clean <- tav1_raw |>
  mutate(
    # when col 1 is NA and col 2 matches a known marker, record the section name
    strat_type = if_else(
      is.na(row_label) & str_trim(F_educ_low) %in% sections,
      str_trim(F_educ_low),
      NA_character_
    )
  ) |>
  # add section label downward to all rows belonging to that section
  fill(strat_type, .direction = "down") |>
  # drop the section marker rows (row_label is NA) and footnote rows
  filter(
    !is.na(row_label),
    !str_detect(row_label, "^legenda|^Fonte"),
    !str_detect(row_label, "^None$")   # in case of leftover "None" strings
  )
 
# shorten the section labels to something clean
tav1_clean <- tav1_clean |>
  mutate(strat_type = recode(str_trim(strat_type),
    "ATTIVITÀ ECONOMICA"          = "sector",
    "ATTIVITÀ ECONOMICA "         = "sector",
    "RIPARTIZIONE GEOGRAFICA"     = "geography",
    "CLASSE DIMENSIONALE"         = "firm_size",
    "TIPO DI CONTROLLO ECONOMICO" = "control_type"
  ))

# rename the levels of education without specifying they refer to education, as I will address it as the table's breakdown category
tav1_clean <- tav1_clean |>
  rename(
    F_low  = F_educ_low,
    F_med  = F_educ_med,
    F_high = F_educ_high,
    M_low  = M_educ_low,
    M_med  = M_educ_med,
    M_high = M_educ_high,
    T_low  = T_educ_low,
    T_med  = T_educ_med,
    T_high = T_educ_high,
  )

view(tav1_clean)

tav1_clean <- tav1_clean |>
  select(-c(sep1, sep2)) |>
  mutate(across(.cols = c(F_low:T_total),
                .fns  = ~ as.numeric(if_else(str_trim(.) == "..", NA_character_, .))
                ))

# pivot and mutate
tav1_final <- tav1_clean |>
  pivot_longer(
    cols      = c(F_low:T_total),
    names_to  = c("sex", "breakdown_cat"),
    names_sep = "_",          # split on first underscore 
    values_to = "value") |>
  mutate(sex = recode(sex,
                      "F" = "female",
                      "M" = "male",
                      "T" = "total"),
    breakdown_var = "education",
    outcome_var = "hourly_wage") 
# label what the breakdown variable is and the outcome variable 
  

tav1_final

# education (ISCED)
meta_education_level <- list(raw_names = c("row_label","F_low", "F_med", "F_high", 
                                           "F_total", "sep1",
                                           "M_low", "M_med", "M_high", "M_total", "sep2",
                                           "T_low", "T_med", "T_high", "T_total"),
  breakdown_var = "education",
  cat_labels = c(low = "\u2264 lower secondary",
                    med = "upper secondary",
                    high = "tertiary",
                    total = "total"))
 
# contract type (Tav.2, 7, 12)
meta_contract_type <- list(raw_names = c(
  "row_label",
  "F_permanent", "F_temporary", "F_total", "sep1",
  "M_permanent", "M_temporary", "M_total", "sep2",
  "T_permanent", "T_temporary", "T_total"),
  breakdown_var = "contract_type",
  cat_labels = c(permanent = "permanent", temporary = "temporary", total = "total"))
 
# hours regime (Tav.3, 8, 13)
meta_hours <- list(raw_names = c(
  "row_label",
  "F_parttime", "F_fulltime", "F_total", "sep1",
   "M_parttime", "M_fulltime", "M_total", "sep2",
  "T_parttime", "T_fulltime", "T_total"),
  breakdown_var = "hours_regime",
  cat_labels = c(parttime = "part-time", fulltime = "full-time", total = "total"))
 
# occupation (ISCO) (Tav.4, 9, 15)
# 10 ISCO major groups + total for each sex
# (Forze Armate is almost always ".." but it was included it as its own thing)
meta_ISCO <- list(raw_names = c(
    "row_label",
    "F_armed", "F_manager", "F_intellectual", "F_technical",
    "F_clerical", "F_sales", "F_agriculture", "F_craft",
    "F_operator", "F_elementary", "F_total", "sep1",
    "M_armed", "M_manager", "M_intellectual", "M_technical",
    "M_clerical", "M_sales", "M_agriculture", "M_craft",
    "M_operator", "M_elementary", "M_total", "sep2",
    "T_armed", "T_manager", "T_intellectual", "T_technical",
    "T_clerical", "T_sales", "T_agriculture", "T_craft",
    "T_operator", "T_elementary", "T_total"),
  breakdown_var = "occupation",
  cat_labels  = c(
    armed       = "Armed Forces",
    manager     = "Managers",
    intellectual = "Intellectual professions",
    technical   = "Technical professions",
    clerical    = "Clerks",
    sales       = "Sales & services",
    agriculture = "Agriculture",
    craft       = "Craft & trades",
    operator    = "Machine operators",
    elementary  = "Elementary occupations",
    total       = "total"))
 
# seniority brackets (Tav.5, 10, 16)
# 8 brackets (0-4, 5-9, 10-14, 15-19, 20-24, 25-29, 30-34, 35+) + total
meta_seniority <- list(
  raw_names = c(
    "row_label",
    "F_yr0004", "F_yr0509", "F_yr1014", "F_yr1519",
    "F_yr2024", "F_yr2529", "F_yr3034", "F_yr35plus", "F_total", "sep1",
    "M_yr0004", "M_yr0509", "M_yr1014", "M_yr1519",
    "M_yr2024", "M_yr2529", "M_yr3034", "M_yr35plus", "M_total", "sep2",
    "T_yr0004", "T_yr0509", "T_yr1014", "T_yr1519",
    "T_yr2024", "T_yr2529", "T_yr3034", "T_yr35plus", "T_total"),
  breakdown_var = "tenure",
  cat_labels    = c(
    yr0004 = "0-4 years", yr0509 = "5-9 years",
    yr1014 = "10-14 years", yr1519 = "15-19 years",
    yr2024 = "20-24 years", yr2529 = "25-29 years",
    yr3034 = "30-34 years", yr35plus = "35+ years",
    total = "total"))

# age brackets (table 14) 
# three brackets - 14 to 29, 30 to 49 and 50+
meta_age <- list(
  raw_names = c("row_label",
    "F_young", "F_mid", "F_old", "F_total", "sep1",
    "M_young", "M_mid", "M_old", "M_total", "sep2",
    "T_young", "T_mid", "T_old", "T_total"),
  breakdown_var = "age",
  cat_labels    = c(young = "14-29 years", mid = "30-49 years",
                    old   = "50+ years",   total = "total"))

parse_ses_table <- function(sheet_name, meta, outcome_label) {
  n_target <- length(meta$raw_names)

  raw <- read_excel(
    "data/Tavole_Struttura-delle-retribuzioni_2022.xlsx",
    sheet     = sheet_name,
    col_names = FALSE,
    col_types = "text",
    skip      = 4
  )

  # keep only the columns described by the meta; rename
  raw <- raw[, seq_len(n_target)]
  names(raw) <- meta$raw_names
 
  # detect and propagate section types
  cleaned <- raw |>
    mutate(
      strat_type = if_else(
        is.na(row_label) & str_trim(.data[[meta$raw_names[2] ]]) %in% sections,
        str_trim(.data[[ meta$raw_names[2] ]]),
        NA_character_
      )
    ) |>
    fill(strat_type, .direction = "down") |>
    # remove section marker rows, footnotes, and trailing blank rows
    filter(
      !is.na(row_label),
      !str_detect(row_label, fixed("legenda")),
      !str_detect(row_label, fixed("Fonte"))
    ) |>
    # recode section labels
    mutate(strat_type = recode(str_trim(strat_type),
      "ATTIVITÀ ECONOMICA"          = "sector",
      "ATTIVITÀ ECONOMICA "         = "sector",
      "RIPARTIZIONE GEOGRAFICA"     = "geography",
      "CLASSE DIMENSIONALE"         = "firm_size",
      "TIPO DI CONTROLLO ECONOMICO" = "control_type"
    ))
 
  # drop separator columns
  cleaned <- cleaned |> select(-c(sep1, sep2))
 
  # convert values to numeric and ".." to NA
  value_cols <- setdiff(names(cleaned), c("row_label", "strat_type"))
  cleaned <- cleaned |>
    mutate(across(
      all_of(value_cols),
      ~ as.numeric(if_else(str_trim(.) == "..", NA_character_, .))
    ))
 
  # pivot to long 
  pivot_long <- cleaned |>
    pivot_longer(
      cols      = all_of(value_cols),
      names_to  = c("sex", "breakdown_cat"),
      names_sep = "_",
      values_to = "value") |>
    mutate(
      sex = recode(sex, "F" = "Female", "M" = "Male", "T" = "Total"),
      # map short breakdown_cat codes to readable labels
      breakdown_cat_label = recode(breakdown_cat, !!!meta$cat_labels),
      breakdown_var       = meta$breakdown_var,
      outcome_var         = outcome_label,
      table_id            = sheet_name)
 
  pivot_long
}

tav2_test <- parse_ses_table(
  sheet_name  = "Tav.2",
  meta = meta_contract_type,
  outcome_label = "hourly_wage"
)

table_map <- tribble(
  ~sheet,    ~meta,             ~outcome,
  "Tav.1",   "education",         "hourly_wage",
  "Tav.2",   "contract_type",     "hourly_wage",
  "Tav.3",   "hours_regime",      "hourly_wage",
  "Tav.4",   "occupation",        "hourly_wage",
  "Tav.5",   "tenure",            "hourly_wage",
  "Tav.6",   "education",         "annual_wage",
  "Tav.7",   "contract_type",     "annual_wage",
  "Tav.8",   "hours_regime",      "annual_wage",
  "Tav.9",   "occupation",        "annual_wage",
  "Tav.10",  "tenure",            "annual_wage",
  "Tav.11",  "education",         "annual_hours",
  "Tav.12",  "contract_type",     "annual_hours",
  "Tav.13",  "hours_regime",      "annual_hours",
  "Tav.14",  "age",               "annual_hours",
  "Tav.15",  "occupation",        "annual_hours",
  "Tav.16",  "tenure",            "annual_hours"
)
 
# named list of all meta vars for lookup
meta_list <- list(
  education = meta_education_level,
  contract_type = meta_contract_type,
  hours_regime = meta_hours,
  occupation   = meta_ISCO,
  tenure = meta_seniority,
  age = meta_age
)

ses_data <- pmap(
  table_map,
  function(sheet, meta, outcome) {
    parse_ses_table(
      sheet_name = sheet,
      meta = meta_list[[meta]],
      outcome_label = outcome
    )
  }
) |>
  bind_rows()

ses_data |> count(table_id, breakdown_var, outcome_var)

ses_data <- ses_data |>
  mutate(breakdown_cat = breakdown_cat_label) |>
  select(-breakdown_cat_label) |>
  # normalize the double-encoded ≤ in the education labels
  mutate(breakdown_cat = str_replace(breakdown_cat,
                                     fixed("\u00e2\u2030\u00a4"), "\u2264"))

# mutate the regional labeling so that it matches the RACLI organization
ses_data <- ses_data |> mutate(
    nuts1 = case_when(
      strat_type == "geography" & str_detect(row_label, "Nord-Ovest|Nord-ovest") ~ "Nord-Ovest",
      strat_type == "geography" & str_detect(row_label, "Nord-Est|Nord-est")     ~ "Nord-Est",
      strat_type == "geography" & str_detect(row_label, "Centro")                ~ "Centro",
      strat_type == "geography" & str_detect(row_label, "Sud")                   ~ "Sud",
      strat_type == "geography" & str_detect(row_label, "Isole")                 ~ "Isole",
      TRUE ~ NA_character_
    ))


# write_csv(ses_data, file = "ses_data.csv")

library(readr)
racli_raw <- read_csv("data/Sesso - prov (IT1,533_957_DF_DCSC_RACLI_8,1.0).csv",
                      quote = "")

racli_raw <- racli_raw |>
  filter(TIME_PERIOD == 2022) # filter 2022

racli_clean <- racli_raw |>
  select(geo_code  = REF_AREA,        # ITC, ITC1, ITC11 etc.
         geo_label  = Territorio,      # specific geographical references
         indicator = DATA_TYPE,       # HOUWAG_ENTEMP_AV_MI / FIRD / MED 
         sex_code = SEX,             # 1 males, 2 females, 9 total
         year  = TIME_PERIOD,
         value = Osservazione) |>
  mutate(
    # indicator codes turned into short readable names
    indicator = recode(
      indicator,
      "HOUWAG_ENTEMP_AV_MI" = "mean",
      "HOUWAG_ENTEMP_FIRD_MI" = "p10",
      "HOUWAG_ENTEMP_MED_MI"  = "median",
      "HOUWAG_ENTEMP_NIND_MI" = "p90"
    ),
    # sex codes to match labels in ses_clean
    sex = recode(
      as.character(sex_code),
      "1" = "Male",
      "2" = "Female",
      "9" = "Total"
    ),
    # classify each territory by its NUTS code length:
    # 2 chars (IT)       -> country
    # 3 chars (ITC)      -> NUTS-1 macro-area
    # 4 chars (ITC1)     -> NUTS-2 region
    # 5 chars (ITC11)    -> NUTS-3 province
    geo_level = case_when(
      nchar(geo_code) == 2 ~ "country",
      nchar(geo_code) == 3 ~ "nuts1",
      nchar(geo_code) == 4 ~ "nuts2",
      nchar(geo_code) == 5 ~ "nuts3",
      TRUE                 ~ NA_character_),
    # derive the parent NUTS-1 code for every row (truncate to first 3 chars)
    nuts1_code = if_else(nchar(geo_code) >= 3, substr(geo_code, 1, 3), NA_character_)) |>
  select(geo_code, geo_label, geo_level, nuts1_code, sex, year, indicator, value)

racli_clean

# building directly from the RACLI data so the codes are consistent
regional_codes <- racli_clean |>
  filter(geo_level == "nuts2") |>
  distinct(
    nuts2_code  = geo_code,
    nuts2_label = geo_label,
    nuts1_code) |>
  mutate(
    nuts1_label = recode(
      nuts1_code,
      "ITC" = "Nord-Ovest / North-West",
      "ITD" = "Nord-Est / North-East",   # old NUTS code (used by RACLI)
      "ITH" = "Nord-Est / North-East",   # current NUTS code (post-2021 revision)
      "ITE" = "Centro / Center",     # old NUTS code (used by RACLI)
      "ITI" = "Centro / Center",     # current NUTS code (post-2021 revision)
      "ITF" = "Sud / South",
      "ITG" = "Isole / Islands")) |>
  arrange(nuts1_code, nuts2_code)

# write_csv(regional_codes, file = "regional_codes.csv")
# write_csv(racli_clean, file = "racli_clean.csv")

introduce(ses_data)
plot_intro(ses_data)
plot_missing(ses_data)

# bar plot
plot_bar(ses_data |> select(strat_type, sex, breakdown_var, outcome_var))

# histogram
plot_histogram(ses_data |> select(value))

# boxplot
ses_data |>
  filter(sex %in% c("Female", "Male")) |>
  ggplot(aes(x = outcome_var, y = value, fill = sex)) +
  geom_boxplot() +
  labs(x = "Outcome variable", y = "Value")

ses_data |>
  filter(strat_type == "geography", sex == "Total",
         row_label != "Totale", breakdown_cat != "total") |>
  select(nuts1, breakdown_var, breakdown_cat, outcome_var, value) |>
  pivot_wider(names_from = outcome_var, values_from = value) |>
  select(hourly_wage, annual_wage, annual_hours) |>
  drop_na() |>
  plot_correlation()

ses_data |>
  filter(strat_type == "geography", outcome_var == "hourly_wage",
         sex %in% c("Female", "Male"),
         row_label != "Totale", breakdown_cat != "total") |>
  group_by(sex, breakdown_var) |>
  summarise(
    mean_wage = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    min_val = min(value, na.rm = TRUE),
    max_val = max(value, na.rm = TRUE))

# font 
font_add_google("EB Garamond", "ebgaramond")
showtext_auto()
showtext_opts(dpi = 300)   # to match the ggsave() dpi 

# headers 
var_labels <- c(
  education     = "Education",
  contract_type = "Contract type",
  hours_regime  = "Hours regime",
  occupation    = "Occupation",
  tenure        = "Tenure")


plot_data <- ses_data |>
  filter(strat_type == "geography",
         outcome_var   == "hourly_wage",
         sex %in% c("Female", "Male"),
         row_label  != "Totale",
         breakdown_cat != "total") |>
  mutate(breakdown_cat = breakdown_cat |>
           str_replace_all("_", " ") |>
           str_to_sentence()) |>
  group_by(breakdown_var, breakdown_cat, sex) |>
  summarise(mean_wage = mean(value, na.rm = TRUE),
            .groups = "drop")


p <- ggplot(plot_data, aes(mean_wage,
                           fct_reorder(breakdown_cat, mean_wage),
                           fill = sex)) +
  # slimmer bars 
  geom_col(position = position_dodge(.65),
           width = .55) +
  
  ggforce::facet_col(
    facets   = vars(breakdown_var),
    scales   = "free_y",
    space    = "free",
    labeller = labeller(breakdown_var = var_labels)) +
  
  scale_fill_manual(values = c(Female = "#D870D6", Male   = "#0A7A0A")) +
  scale_x_continuous(labels = label_number(prefix = "€", accuracy = 1),
                     breaks = breaks_width(5),  # tick every €5
                     expand = expansion(mult = c(0, .05))) +
  
  scale_y_discrete(labels = function(x) dplyr::recode(x, # reconde longer occupations that create overlap
    "Intellectual professions" = "Intellectuals",
    "Elementary occupations"   = "Elementary occ.")) +
  
  labs(title = "Mean hourly wage by worker characteristic and sex",
       subtitle = "Structure of Earnings Survey - 2022",
       x = "Mean hourly wage (€)",
       y = NULL,
       fill = NULL) +
  
  theme_minimal(base_size = 13, base_family = "ebgaramond") +
  theme(
    legend.position      = "top",
    legend.justification = "left",
    plot.title.position  = "plot",
    plot.title    = element_text(face = "bold", size = 17),
    plot.subtitle = element_text(colour = "grey40", size = 12,
                                 margin = margin(b = 12)),
    strip.text = element_text(face = "bold", size = 12,
                              colour = "#3E6B7E", hjust = 0),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.spacing      = unit(1.1, "lines"),
    # smaller lineheight keeps wrapped labels from crowding neighbours
    axis.text.y  = element_text(size = 9.5, lineheight = .9),
    axis.title.x = element_text(margin = margin(t = 10)),
    plot.margin  = margin(14, 18, 10, 14))

p


nuts1_labels <- c(
  "Nord-Ovest" = "North-west",
  "Nord-Est"   = "North-east",
  "Centro"     = "Center",
  "Sud"        = "South",
  "Isole"      = "Islands")

# data 
plot_macro <- ses_data |>
  filter(
    strat_type    == "geography",
    outcome_var   == "hourly_wage",
    sex %in% c("Female", "Male"),
    row_label     != "Totale",
    breakdown_var == "education",
    breakdown_cat == "total"
  ) |>
  mutate(nuts1 = recode(nuts1, !!!nuts1_labels)) |>
  group_by(nuts1, sex) |>
  summarise(mean_wage = mean(value, na.rm = TRUE), .groups = "drop")

# plot 
p2 <- ggplot(plot_macro, aes(mean_wage,
                             fct_reorder(nuts1, mean_wage),   # order areas by wage
                             fill = sex)) +
  geom_col(position = position_dodge(.65),
           width = .6) +
  
  scale_fill_manual(values = c(Female = "#b22222", Male = "darkblue")) +
  scale_x_continuous(labels = label_number(prefix = "€", accuracy = 1),
                     breaks = breaks_width(5),
                     expand = expansion(mult = c(0, .05))) +
  labs(title = "Mean hourly wage by macro-area and sex",
       subtitle = "Structure of Earnings Survey - NUTS-1 macro-areas, 2022",
       x = "Mean hourly wage (€)",
       y = NULL,
       fill = NULL) +
  theme_minimal(base_size = 13, base_family = "ebgaramond") +
  theme(
    legend.position      = "top",
    legend.justification = "left",
    plot.title.position  = "plot",
    plot.title    = element_text(face = "bold", size = 17),
    plot.subtitle = element_text(colour = "grey40", size = 12,
                                 margin = margin(b = 12)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.y  = element_text(size = 11),
    axis.title.x = element_text(margin = margin(t = 10)),
    plot.margin  = margin(14, 18, 10, 14)
  )
p2

within_gaps <- ses_data |>
  filter(strat_type == "geography",
         outcome_var == "hourly_wage",
         row_label != "Totale",             # drop the national-total row
         breakdown_cat != "total",    # drop within-breakdown totals
         sex %in% c("Female", "Male")) |>
  select(area = row_label,
         breakdown_var, breakdown_cat,
         sex, value) |>
  pivot_wider(names_from = sex, values_from = value) |>
  filter(!is.na(Female), !is.na(Male), Male > 0) |>
  mutate(gap = 1 - Female / Male)

within_gaps

wGPG_area_breakdown <- within_gaps |>
  group_by(area, breakdown_var) |>
  mutate(weight = Female / sum(Female, na.rm = TRUE)) |>
  summarise(wGPG = sum(weight * gap, na.rm = TRUE),
            n_categories = n(),
            .groups = "drop") |>
  arrange(area, breakdown_var)

wGPG_area_breakdown

wGPG_area <- wGPG_area_breakdown |>
  group_by(area) |>
  summarise(wGPG = mean(wGPG), .groups = "drop") |>
  arrange(desc(wGPG))

wGPG_area

# NUTS-1 codes for the 5 macro-areas (2021 classification, used by GISCO)
nuts1_codes <- tibble(
  nuts1   = c("Nord-Ovest", "Nord-Est", "Centro", "Sud", "Isole"),
  NUTS_ID = c("ITC", "ITH", "ITI", "ITF", "ITG"))

gap_by_area <- ses_data |>
  filter(strat_type == "geography", outcome_var == "hourly_wage",
         sex %in% c("Female", "Male"),
         row_label != "Totale", breakdown_var == "education",
         breakdown_cat == "total") |>
  select(nuts1, sex, value) |>
  pivot_wider(names_from = sex, values_from = value) |>
  mutate(gap = Male - Female) |>
  left_join(nuts1_codes, by = "nuts1")

# NUTS-1 boundaries for Italy
italy_nuts1 <- gisco_get_nuts(country = "IT", nuts_level = 1, year = "2021", resolution = "20")

# wGPG for the map
wGPG_map <- wGPG_area |>
  left_join(nuts1_codes, by = c("area" = "nuts1"))

p3 <- italy_nuts1 |>
  left_join(wGPG_map, by = "NUTS_ID") |>
  ggplot(aes(fill = wGPG)) +
  geom_sf(color = "white") +
  scale_fill_gradient2(low = "#542788", mid = "#f7f7f7", high = "#b2182b",
                       midpoint = 0, limits = c(-0.12, 0.12),
                       labels = scales::percent_format(accuracy = 1),
                       name = "Adjusted gap\n(% of male pay)") +
  
  labs(title = "Within-category gender wage gap by macro-area",
       subtitle = "Gender pay gap accounting for differences in worker and job characteristics") +
  
  theme_void(base_size = 11, base_family = "ebgaramond") +
  theme(plot.title.position = "plot",
        plot.title = element_text(face = "bold", size = 15, hjust = 0,
                                 margin = margin(b = 4)),
        plot.subtitle = element_text(colour = "grey40", size = 12, hjust = 0,
                                 margin = margin(b = 14)),
        # extra room above the title
        plot.margin = margin(t = 18, r = 6, b = 6, l = 6),  
        # legend 
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9),
        legend.key.height = unit(0.9, "lines"),
        legend.key.width  = unit(0.7, "lines"))
  
p3


reg_dis <- racli_clean |>
  filter(geo_level == "nuts2",
         sex == "Total",
         indicator %in% c("mean", "p10", "p90")) |>
  select(geo_code, geo_label, nuts1_code, indicator, value) |>
  pivot_wider(names_from = indicator, values_from = value) |>
  mutate(
    p90_p10 = p90 / p10,
    # z-standardize each indicator; low wage and high dispersion = disadvantaged
    z_wage  = -as.numeric(scale(mean)),     # reverse-signed: low wage -> high score
    z_disp  =  as.numeric(scale(p90_p10)),  # "high = bad" direction
    #  average of the two standardized indicators
    reg_dis = (z_wage + z_disp) / 2,
    # labels
    reg_dis_lvl = cut(reg_dis,
                      breaks = quantile(reg_dis, c(0, 1/3, 2/3, 1)),
                      labels = c("low", "moderate", "high"),
                      include.lowest = TRUE)) |>
  arrange(desc(reg_dis))

# adapted to NUTS-1 for SES
# RACLI codes -> SES macro-area labels, average 'reg_dis' within each macro-area, then re-cut into tiers 
reg_dis <- reg_dis |>
  mutate(
    nuts1 = recode(nuts1_code,
      "ITC" = "Nord-Ovest",
      "ITD" = "Nord-Est", "ITH" = "Nord-Est",
      "ITE" = "Centro",   "ITI" = "Centro",
      "ITF" = "Sud",
      "ITG" = "Isole")) |>
  group_by(nuts1) |>
  summarise(reg_dis = mean(reg_dis), .groups = "drop") |>
  mutate(reg_dis_lvl = cut(reg_dis,
                      breaks = quantile(reg_dis, c(0, 1/3, 2/3, 1)),
                      labels = c("low", "moderate", "high"),
                      include.lowest = TRUE)) |>
  arrange(desc(reg_dis))

set.seed(240603) 

# pivoted dataset and filtered to only keep the rows where NUTS1 are specified
ses_wide <- ses_data |>  
  filter(strat_type == "geography",
         outcome_var == "hourly_wage",
         row_label != "Totale",
         breakdown_cat != "total",
         sex %in% c("Female", "Male")) |>
  select(area = row_label,
         breakdown_var, breakdown_cat,
         sex, value) |>
  pivot_wider(names_from = sex, values_from = value) |>
  filter(!is.na(Female), !is.na(Male), Male > 0)

# create the baseline corrupted MCAR dataset - 20% random missingness
miss_rate_ses <- 0.20

ses_missing <- ses_wide |> 
  mutate(
    Female = if_else(runif(n()) < miss_rate_ses, NA_real_, Female),
    Male   = if_else(runif(n()) < miss_rate_ses, NA_real_, Male)
  )

# 'ses_wide' as ground truth
# 'ses_missing' is MCAR missingness

# convert descriptors to factors so MICE can utilize them as predictors
mice_ses_input <- ses_missing |>
mutate(across(c(area, breakdown_var, breakdown_cat), as.factor)) |>
select(area, breakdown_var, breakdown_cat, Female, Male)

# run the imputation over 5 iterations
mice_ses_fit <- mice(mice_ses_input, m = 5, method = "pmm", printFlag =
FALSE)

# pooling the 5 imputed datasets
imp_female_matrix <- sapply(1:5, function(i) complete(mice_ses_fit,
i)$Female)
imp_male_matrix <- sapply(1:5, function(i) complete(mice_ses_fit,
i)$Male)

ses_imputed_mcar <- ses_missing |>
mutate(
  Female = rowMeans(imp_female_matrix),
  Male = rowMeans(imp_male_matrix))

set.seed(240306) # seed for reproducibility

# MNAR
tier_p <- c(low = 0.10, moderate = 0.25, high = 0.45)
beta   <- 0.05  # value dependence: extra drop prob per EUR/hr below the 
# within-tier median

ses_exp_mnar <- ses_wide |>
  rename(nuts1 = area) |>
  filter(!is.na(nuts1)) |>
  left_join(reg_dis |> select(nuts1, reg_dis_lvl), by = "nuts1") |>
  group_by(reg_dis_lvl) |>
  mutate(
    base   = tier_p[as.character(reg_dis_lvl)],
    pF  = pmin(pmax(base + beta * (median(Female, na.rm = TRUE) - Female), 
                       0.02), 0.95),
    pM  = pmin(pmax(base + beta * (median(Male,   na.rm = TRUE) - Male),   
                       0.02), 0.95),
    Female = if_else(runif(n()) < pF, NA_real_, Female),
    Male   = if_else(runif(n()) < pM, NA_real_, Male)) |>
  ungroup() |>
  select(-base, -pF, -pM)

plot_missing(ses_exp_mnar)

# sanity check
ses_exp_mnar |>
  pivot_longer(c(Female, Male), names_to = "sex", values_to = "value") |>
  group_by(reg_dis_lvl) |>
  summarise(missing_rate = mean(is.na(value)), n_cells = n(), .groups = "drop") 

set.seed(240603)

mice_ses_mnar_input <- ses_exp_mnar |>
  mutate(across(c(nuts1, breakdown_var, breakdown_cat), as.factor)) |>
  select(nuts1, breakdown_var, breakdown_cat, Female, Male)

mice_ses_mnar_fit <- mice(mice_ses_mnar_input, m = 5, method = "pmm", printFlag = FALSE)

# pool: average imputed Female and Male columns
imp_female_mnar <- sapply(1:5, function(i) complete(mice_ses_mnar_fit, i)$Female)
imp_male_mnar   <- sapply(1:5, function(i) complete(mice_ses_mnar_fit, i)$Male)

ses_mice_mnar <- ses_exp_mnar |>
  mutate(
    Female = rowMeans(imp_female_mnar),
    Male   = rowMeans(imp_male_mnar)) |>
  rename(area = nuts1)   # match column name expected by evaluate_imputation

metrics_tracker <- tibble(
  method    = character(),
  metric    = character(),
  n         = integer(),
  mean_bias = double(),
  MAE       = double(),
  RMSE      = double(),
  spearman  = double())

# collect the per-call findings
fairness_tracker <- tibble()
tier_tracker     <- tibble()

eval_ses <- function(ses_imp, label) {

  # 1. wGPG-level accuracy (25 area x breakdown cells) 
  imp <- ses_imp |>
    group_by(area, breakdown_var) |>
    mutate(gap    = 1 - Female / Male,
           weight = Female / sum(Female, na.rm = TRUE)) |>
    summarise(value_imp = sum(weight * gap, na.rm = TRUE), .groups = "drop") |>
    unite("unit", area, breakdown_var, sep = " | ")

  truth <- wGPG_area_breakdown |>
    select(area, breakdown_var, value_true = wGPG) |>
    unite("unit", area, breakdown_var, sep = " | ")

  scores <- truth |>
    inner_join(imp, by = "unit") |>
    mutate(bias = value_imp - value_true, abs_error = abs(bias)) |>
    summarise(
      method    = label,
      metric    = "wGPG",
      n         = sum(!is.na(bias)),
      mean_bias = mean(bias, na.rm = TRUE),
      MAE       = mean(abs_error, na.rm = TRUE),
      RMSE      = sqrt(mean(bias^2, na.rm = TRUE)),
      spearman  = cor(value_true, value_imp, method = "spearman", use = "complete.obs"))

  metrics_tracker <<- bind_rows(metrics_tracker, scores)
  cat(sprintf("\n--- %s ---\n", label))
  print(scores)

  # 2. Cell-level distributional fidelity
  se <- ses_imp |>
    pivot_longer(c(Female, Male), names_to = "sex", values_to = "value_imp") |>
    left_join(
      ses_wide |> pivot_longer(c(Female, Male), names_to = "sex", values_to = "value_true"),
      by = c("area", "breakdown_var", "breakdown_cat", "sex")) |>
    mutate(bias = value_imp - value_true, abs_error = abs(bias))

  cat("\n Cell-level \n")
  print(se |> arrange(desc(abs_error)) |>
    select(area, breakdown_var, breakdown_cat, sex, value_true, value_imp, bias))

  ks <- ks.test(se$value_imp, se$value_true)
  cat(sprintf("\nK-S: D=%.4f  p=%.4f\n", ks$statistic, ks$p.value))
  cat(sprintf("Bias direction: %.1f%% of cells over-estimated\n",
              mean(se$bias > 0, na.rm = TRUE) * 100))

  # 3. Fairness by breakdown variable
  cat("\n Fairness by breakdown variable \n")
  fairness <- se |>
    group_by(breakdown_var) |>
    summarise(bias = mean(bias, na.rm = TRUE),
              MAE  = mean(abs_error, na.rm = TRUE),
              n    = n(), .groups = "drop") |>
    arrange(breakdown_var)
  print(fairness)
  
  fairness_tracker <<- bind_rows(fairness_tracker, fairness |> 
                                   mutate(method = label))
  cat(sprintf("MAE ratio (worst / best): %.3f\n", max(fairness$MAE) / min(fairness$MAE)))

  # 4. Fairness by disadvantage tier (MNAR datasets only)
  if ("reg_dis_lvl" %in% names(se)) {
    cat("\n Fairness by disadvantage tier \n")
    tier_fair <- se |>
      group_by(reg_dis_lvl) |>
      summarise(bias = mean(bias, na.rm = TRUE),
                MAE  = mean(abs_error, na.rm = TRUE),
                n    = n(), .groups = "drop") |>
      arrange(reg_dis_lvl)
    print(tier_fair)
    
    tier_tracker <<- bind_rows(tier_tracker, tier_fair |> 
                                 mutate(method = label))
    cat(sprintf("MAE ratio (high / low tier): %.3f\n",
                max(tier_fair$MAE) / min(tier_fair$MAE)))
  }

  # 5. wGPG MAE at two aggregation levels 
  wGPG_cells <- ses_imp |>
    group_by(area, breakdown_var) |>
    mutate(gap    = 1 - Female / Male,
           weight = Female / sum(Female, na.rm = TRUE)) |>
    summarise(wGPG_imp = sum(weight * gap, na.rm = TRUE), .groups = "drop") |>
    left_join(wGPG_area_breakdown |> select(area, breakdown_var, wGPG_true = wGPG),
              by = c("area", "breakdown_var")) |>
    mutate(wGPG_error = abs(wGPG_imp - wGPG_true))

  wGPG_area_res <- wGPG_cells |>
    group_by(area) |>
    summarise(wGPG_imp = mean(wGPG_imp), .groups = "drop") |>
    left_join(wGPG_area |> select(area, wGPG_true = wGPG), by = "area") |>
    mutate(wGPG_error = abs(wGPG_imp - wGPG_true))

  cat(sprintf("\nwGPG MAE (25 cells): %.4f\n", mean(wGPG_cells$wGPG_error, na.rm = TRUE)))
  cat(sprintf("wGPG MAE (5 NUTS-1): %.4f\n",  mean(wGPG_area_res$wGPG_error, na.rm = TRUE)))

  if (exists("mcar_mae_ses")) {
    cat(sprintf("MNAR/MCAR MAE ratio: %.2fx\n", scores$MAE / mcar_mae_ses))
  }

  invisible(metrics_tracker)
}

# evaluating MNAR and MCAR MICE retreival
eval_ses(ses_mice_mnar, "MICE_MNAR")
eval_ses(ses_imputed_mcar, "MICE_MCAR")

llm_chat <- function(prompt,
                     model = "qwen/qwen3-32b",
                     system = NULL,
                     temperature = 0.6,
                     top_p = 0.9,
                     max_tokens = 8192,
                     retries = 5) {

  msgs <- list()
  if (!is.null(system)) msgs <- c(msgs, list(list(role = "system", content = system)))
  msgs <- c(msgs, list(list(role = "user", content = prompt)))

  for (attempt in seq_len(retries)) {
    resp <- POST(
      url = "https://openrouter.ai/api/v1/chat/completions",
      add_headers(
        Authorization = paste("Bearer", Sys.getenv("OPENROUTER_API_KEY")),
        `Content-Type` = "application/json"),
      body = toJSON(list(
        model       = model,
        messages    = msgs,
        temperature = temperature,
        top_p       = top_p,
        max_tokens  = max_tokens), auto_unbox = TRUE),
      encode = "raw",
      timeout(600))

    sc <- status_code(resp)

    if (sc == 200)
      return(content(resp, as = "parsed")$choices[[1]]$message$content)

    if (sc %in% c(429L, 503L, 504L) && attempt < retries) {
      message("HF API ", sc, " on attempt ", attempt, " -- retrying in ",
              20 * attempt, "s ...")
      Sys.sleep(20 * attempt)
      next
    }

    stop("API error ", sc, ": ",
         content(resp, as = "text", encoding = "UTF-8"))
  }
  stop("All ", retries, " retries exhausted.")
}

# shared LLM extraction config + helpers (used by EVERY LLM condition)
wage_lower        <- 6       # plausible EUR/hr bounds 
wage_higher        <- 50

strip_think <- function(x) {
  if (length(x) == 0 || is.na(x)) return(NA_character_)
  x <- gsub("(?s)<think>.*?</think>", "", x, perl = TRUE) # closed trace
  if (grepl("</think>", x)) x <- sub("(?s).*</think>", "", x, perl = TRUE)  # keep text after last </think>
  trimws(x)
}

# primary: first number after stripping the think trace; fallback: LAST number in the raw reply
extract_number <- function(raw) {
  if (length(raw) == 0 || is.na(raw)) return(NA_real_)
  pick <- function(s, last = FALSE) {
    nums <- regmatches(s, gregexpr("-?[0-9]+(?:[.,][0-9]+)?", s, perl = TRUE))[[1]]
    nums <- nums[!grepl("^(19|20)[0-9]{2}$", nums)]   # drop year-like tokens
    if (length(nums) == 0) return(NA_real_)
    as.numeric(gsub(",", ".", if (last) tail(nums, 1) else nums[1]))
  }
  val <- pick(strip_think(raw), last = FALSE)
  if (is.na(val)) val <- pick(raw, last = TRUE)
  val
}

# one call + parse + validate path
llm_impute_value <- function(prompt, system, sleep = 2,
                             lo = wage_lower, hi = wage_higher,
                             max_tokens = 8192) {
  Sys.sleep(sleep)
  raw <- tryCatch(
    llm_chat(prompt, system = system, max_tokens = max_tokens),
    error = function(e) { message("API error: ", conditionMessage(e)); NULL })
  if (is.null(raw)) return(NA_real_)
  val <- extract_number(raw)
  if (is.na(val))          { message("extract -> NA"); return(NA_real_) }
  if (val < lo | val > hi) { message("out of range: ", val); return(NA_real_) }
  val
}

# d: a redrawn MNAR dataset shaped like ses_exp_mnar
# returns a tibble shaped like ses_wide (area, breakdown_var, breakdown_cat,
# Female, Male) with missing cells filled in by the LLM
oneshot_run <- function(d, condition = c("uninformed", "anon", "blind"),
                           chat = llm_chat) {
  condition <- match.arg(condition)

  long <- d |>
    rename(area = nuts1) |>
    select(area, breakdown_var, breakdown_cat, Female, Male,
           any_of("reg_dis_lvl")) |>
    pivot_longer(c(Female, Male), names_to = "sex", values_to = "val") |>
    mutate(cell_id = row_number(),
           display = if_else(is.na(val), paste0("MISSING_", cell_id),
                             as.character(round(val, 2))))

  # missing_keys keeps the real area names + full breakdown labels for merge-back
  missing_keys <- long |>
    filter(is.na(val)) |>
    select(cell_id, area, breakdown_var, breakdown_cat, sex)

  if (condition == "anon") {
    area_levels <- sort(unique(long$area))
    area_map    <- setNames(paste0("Area_", seq_along(area_levels)), area_levels)
    long <- long |> mutate(area = area_map[area])
  }

  # blind: hide the breakdown covariates from the model (area + sex + value only)
  if (condition == "blind") {
    long <- long |>
      mutate(row = paste(cell_id, area, sex, display, sep = ","))
    cols_hdr <- "cell_id,area,sex,value"
  } else {
    long <- long |>
      mutate(row = paste(cell_id, area, breakdown_var, breakdown_cat,
                         sex, display, sep = ","))
    cols_hdr <- "cell_id,area,breakdown_var,breakdown_cat,sex,value"
  }
  target_csv <- paste(long$row, collapse = "\n")

  sys_prompt <- paste(
    "You are completing missing wage cells in an ISTAT Structure of Earnings survey table.",
    "All values are mean gross hourly wages in EUR/hour, Italy 2022.",
    "You are given one target table stratified by NUTS-1 macro-area, with some cells missing.",
    "Base your outputs strictly on what is present in the given data, learning from the observed cells in the table.",
    "Each missing cell in the target table is marked MISSING_<id> where <id> is a number.",
    "Return ONLY a two-column CSV with header: cell_id,value",
    "One row per missing cell, where cell_id is the number from MISSING_<id> and value is your imputed wage.",
    "Values must be between 6 and 50. No markdown, no explanation.")

  prompt <- paste0("TARGET TABLE (geography-stratified wages: impute every 
                   MISSING_<id> cell)\n",
    paste0(cols_hdr, "\n"), target_csv, "\n\n",
    "Return a CSV with header: cell_id,value: one row per MISSING_<id> cell only.")

  raw <- chat(prompt, system = sys_prompt, max_tokens = 32768)

  # parse: find the cell_id, value header
  clean_resp <- strip_think(raw)
  clean_resp <- gsub("```[a-z]*\n?|```", "", clean_resp) # strip markdown
  resp_lines <- strsplit(trimws(clean_resp), "\n")[[1]]
  resp_lines <- resp_lines[nchar(trimws(resp_lines)) > 0]

  if (length(resp_lines) == 0 || all(is.na(resp_lines)))
    stop("LLM returned empty response")

  hdr_idx <- which(grepl("^cell_id,", resp_lines, ignore.case = TRUE))[1]
  if (is.na(hdr_idx)) hdr_idx <- 1
  csv_text <- paste(resp_lines[hdr_idx:length(resp_lines)], collapse = "\n")

  # I() forces read_csv to treat csv_text as literal text, never as a file path
  llm_df <- read_csv(I(csv_text), show_col_types = FALSE) |>
    mutate(value = suppressWarnings(as.numeric(value)),
           value = if_else(value < wage_lower | value > wage_higher, NA_real_, value)) |>
    left_join(missing_keys, by = "cell_id") # join by id -> back to real area names

  # merged: fill the original missing cells with the LLM's values
  d |>
    rename(area = nuts1) |>
    select(area, breakdown_var, breakdown_cat, Female, Male,
           any_of("reg_dis_lvl")) |>
    pivot_longer(c(Female, Male), names_to = "sex", values_to = "value") |>
    left_join(llm_df |> select(area, breakdown_var, breakdown_cat, sex,
                               value_imp = value),
              by = c("area", "breakdown_var", "breakdown_cat", "sex")) |>
    mutate(value = if_else(is.na(value), value_imp, value)) |>
    select(-value_imp) |>
    pivot_wider(names_from = sex, values_from = value)
}

set.seed(240603)
llm_baseline_oneshot_merged <- oneshot_run(ses_exp_mnar, "uninformed")

#write_csv(llm_baseline_oneshot_merged, "llm_baseline_oneshot_merged.csv")

set.seed(240603)
llm_anon_merged <- oneshot_run(ses_exp_mnar, "anon")

# write_csv(ses_llm_anon_merged, "ses_llm_anon_merged.csv")

# uninformed 
eval_ses(llm_baseline_oneshot_merged, "LLM_oneshot_baseline")

# anonymized geography 
eval_ses(llm_anon_merged, "LLM_anonymized_geo")

metrics_tracker |>
  filter(method %in% c("LLM_oneshot_baseline", "LLM_anonymized_geo",
                        "MICE_MNAR", "MICE_MCAR"),
         metric == "wGPG")

metrics_tracker |>
  filter(method %in% c("LLM_oneshot_baseline", "LLM_anonymized_geo",
                        "MICE_MNAR", "MICE_MCAR")) |>
  knitr::kable(digits = 3)

# tables
ord <- c("MICE_MCAR", "MICE_MNAR", "LLM_oneshot_baseline", "LLM_anonymized_geo")
labels <- c(MICE_MCAR = "MICE (MCAR)",
          MICE_MNAR = "MICE (MNAR)",
          LLM_oneshot_baseline = "LLM uninformed",
          LLM_anonymized_geo = "LLM anonymised geography")

# wGPG recovery 
tab_accuracy <- metrics_tracker |>
  filter(method %in% ord, metric == "wGPG") |>
  distinct() |>
  mutate(method = factor(method, ord)) |>
  arrange(method) |>
  mutate(method = labels[as.character(method)])

stargazer(as.data.frame(tab_accuracy), type = "latex", summary = FALSE,
          rownames = FALSE, digits = 3, label = "tab:accuracy",
          title = "wGPG recovery accuracy by method (MNAR SES)")

# MAE by breakdown variable
breakdown_labels <- c(contract_type = "Contract type",
                    education  = "Education",
                    hours_regime  = "Hours regime",
                    occupation  = "Occupation",
                    tenure  = "Tenure")

tab_breakdown <- fairness_tracker |>
  filter(method %in% ord) |>
  distinct() |>
  mutate(method = labels[method],
         breakdown_var = breakdown_labels[breakdown_var]) |>
  select(method, breakdown_var, MAE) |>
  pivot_wider(names_from = method, values_from = MAE) |>
  select(`Breakdown variable` = breakdown_var, any_of(unname(labels)))

stargazer(as.data.frame(tab_breakdown), type = "latex", summary = FALSE,
          rownames = FALSE, digits = 3, label = "tab:breakdown",
          title = "Imputation MAE by breakdown variable",
          float.env = "table", font.size = "small")

# MAE by disadvantage tier
tab_tier <- tier_tracker |>
  filter(method %in% ord) |>
  distinct() |>
  mutate(method = labels[method]) |>
  select(method, reg_dis_lvl, MAE) |>
  pivot_wider(names_from = method, values_from = MAE) |>
  arrange(reg_dis_lvl) |>
  select(reg_dis_lvl, any_of(unname(labels)))

stargazer(as.data.frame(tab_tier), type = "latex", summary = FALSE,
          rownames = FALSE, digits = 3, label = "tab:tier",
          title = "Imputation MAE by regional disadvantage tier")

# redraw MNAR for a given seed 
redraw_mnar <- function(seed, data = ses_wide) {
  set.seed(seed); tier_p <- c(low=.10, moderate=.25, high=.45); beta <- .05
  data |> rename(nuts1 = area) |> filter(!is.na(nuts1)) |>
    left_join(reg_dis |> select(nuts1, reg_dis_lvl), by="nuts1") |>
    group_by(reg_dis_lvl) |>
    mutate(b = tier_p[as.character(reg_dis_lvl)],
           pF = pmin(pmax(b + beta*(median(Female,na.rm=T)-Female),.02),.95),
           pM = pmin(pmax(b + beta*(median(Male,na.rm=T)-Male),.02),.95),
           Female = if_else(runif(n())<pF, NA_real_, Female),
           Male   = if_else(runif(n())<pM, NA_real_, Male)) |>
    ungroup() |> select(-b,-pF,-pM)
}

# wGPG MAE + Spearman vs ground truth from ses_wide
wgpg <- function(d) d |> group_by(area, breakdown_var) |>
  mutate(gap = 1 - Female/Male, wt = Female/sum(Female, na.rm = TRUE)) |>
  summarise(g = sum(wt * gap, na.rm = TRUE), .groups = "drop")

score <- function(imp) {
  j <- inner_join(wgpg(imp), wgpg(ses_wide), by = c("area","breakdown_var"),
                  suffix = c("_i","_t"))
  tibble(MAE = mean(abs(j$g_i - j$g_t)),
         rho = cor(j$g_t, j$g_i, method = "spearman"))
}

# MICE: 50 redraws
rep_mice <- map_dfr(1:50, \(s) {
  d   <- redraw_mnar(s)
  fit <- mice(d |> mutate(across(c(nuts1, breakdown_var, 
                                   breakdown_cat), as.factor)) |>
                select(nuts1, breakdown_var, breakdown_cat, Female, Male),
              m = 5, method = "pmm", printFlag = FALSE)
  imp <- d |> mutate(Female = rowMeans(sapply(1:5, \(i) complete(fit, i)$Female)),
                     Male = rowMeans(sapply(1:5, \(i) complete(fit, i)$Male))) |>
    rename(area = nuts1)
  score(imp) |> mutate(seed = s)
})

# summary
rep_mice |>
  summarise(method   = "MICE",
            n_ok     = sum(!is.na(MAE)),
            MAE_mean = mean(MAE, na.rm = TRUE),
            MAE_sd   = sd(MAE,   na.rm = TRUE),
            rho_mean = mean(rho, na.rm = TRUE),
            rho_sd   = sd(rho,   na.rm = TRUE))

# updated LLM_chat for iterations
llm_chat_rep <- function(prompt,
                         model = "qwen/qwen3-32b",
                         system = NULL,
                         temperature = 0.6,
                         top_p = 0.9,
                         max_tokens = 8192,
                         retries = 6) {

  msgs <- list()
  if (!is.null(system)) msgs <- c(msgs, list(list(role = "system", 
                                                  content = system)))
  msgs <- c(msgs, list(list(role = "user", content = prompt)))

  for (attempt in seq_len(retries)) {
    resp <- tryCatch(
      POST(
        url = "https://openrouter.ai/api/v1/chat/completions",
        add_headers(
          Authorization = paste("Bearer", Sys.getenv("OPENROUTER_API_KEY")),
          `Content-Type` = "application/json"),
        body = toJSON(list(
          model = model, messages = msgs,
          temperature = temperature, top_p = top_p,
          max_tokens = max_tokens), auto_unbox = TRUE),
        encode = "raw",
        timeout(600)),
      error = function(e) e)  # capture timeout

    # network-level failure -> retry
    if (inherits(resp, "error")) {
      if (attempt < retries) {
        message("network error (", conditionMessage(resp), ") on attempt ",
                attempt, " -- retrying in ", 20 * attempt, "s ...")
        Sys.sleep(20 * attempt); next
      }
      stop("network error after ", retries, " retries: ", conditionMessage(resp))
    }

    sc <- status_code(resp)
    if (sc == 200)
      return(content(resp, as = "parsed")$choices[[1]]$message$content)

    if (sc %in% c(429L, 503L, 504L) && attempt < retries) {
      message("API ", sc, " on attempt ", attempt, " -- retrying in ", 
              20 * attempt, "s ...")
      Sys.sleep(20 * attempt); next
    }

    stop("API error ", sc, ": ", content(resp, as = "text", encoding = "UTF-8"))
  }
  stop("All ", retries, " retries exhausted.")
}

# LLM one-shot: 8 redraws on both conditions - 16 total
rep_llm <- map_dfr(1:8, \(s) {
  d <- redraw_mnar(s)
  bind_rows(
    score(oneshot_run(d, "uninformed", chat = llm_chat_rep)) |>
      mutate(method = "LLM_uninformed"),
    score(oneshot_run(d, "anon", chat = llm_chat_rep)) |>
      mutate(method = "LLM_anon")) |>
    mutate(seed = s)
})

# all runs 
rep_summary <- bind_rows(rep_mice |> mutate(method = "MICE"), rep_llm) |>
  group_by(method) |>
  summarise(n_ok     = sum(!is.na(MAE)),
            MAE_mean = mean(MAE, na.rm = TRUE),
            MAE_sd   = sd(MAE,   na.rm = TRUE),
            rho_mean = mean(rho, na.rm = TRUE),
            rho_sd   = sd(rho,   na.rm = TRUE),
            .groups  = "drop")

set.seed(240603)

# shifts both wage levels and the gender gap -> resulting figures do not match any published ISTAT table
ses_test <- ses_wide |> mutate(Female = Female * 1.12, Male = Male * 1.18)

# raise the parser clamp for this run, then restore so no change to original 
wage_higher_orig <- wage_higher
wage_higher <<- 75  # covers ~€62 perturbed max with headroom

ses_test_mnar <- redraw_mnar(seed = 240603, data = ses_test)
imp_test <- oneshot_run(ses_test_mnar, "uninformed", chat = llm_chat_rep)

wage_higher <<- wage_higher_orig # restore so previous runs are unaffected on re-run

score_test <- function(imp, truth) {
  j <- inner_join(wgpg(imp), wgpg(truth), by = c("area","breakdown_var"),
                  suffix = c("_i","_t")); mean(abs(j$g_i - j$g_t))
}

# retrieval if error-vs-ORIGINAL < error-vs-PERTURBED; estimation if the reverse
unseen_tbl <- tibble(vs_test = score_test(imp_test, ses_test),
                     vs_original  = score_test(imp_test, ses_wide))

# iteration robustness (mean +/- SD across redraws)
stargazer(as.data.frame(rep_summary), type = "latex", summary = FALSE,
          rownames = FALSE, digits = 3, label = "tab:iteration",
          title = "wGPG MAE and Spearman across redraws and iterations")

# retrieval vs. estimation on altered data
stargazer(as.data.frame(unseen_tbl), type = "latex", summary = FALSE,
          rownames = FALSE, digits = 3, label = "tab:unseen",
          title = "Imputation error vs. altered (unseen) table and original 2022 figures")

set.seed(240603)

llm_blind_merged <- oneshot_run(ses_exp_mnar, "blind")

set.seed(240603)

mice_blind_input <- ses_exp_mnar |>
  mutate(nuts1 = as.factor(nuts1)) |>
  select(nuts1, Female, Male)

pred <- make.predictorMatrix(mice_blind_input)
pred[c("Female", "Male"), ] <- 0 # drop breakdown covariates + cross-sex
pred["Female", "nuts1"] <- 1
pred["Male",   "nuts1"] <- 1

mice_blind_fit <- mice(mice_blind_input, m = 5, method = "pmm",
                       predictorMatrix = pred, printFlag = FALSE)

imp_f <- sapply(1:5, \(i) complete(mice_blind_fit, i)$Female)
imp_m <- sapply(1:5, \(i) complete(mice_blind_fit, i)$Male)

ses_mice_blind <- ses_exp_mnar |>
  mutate(Female = rowMeans(imp_f), Male = rowMeans(imp_m)) |>
  rename(area = nuts1)

# for comparison
masked_cells <- ses_exp_mnar |>
  rename(area = nuts1) |>
  pivot_longer(c(Female, Male), names_to = "sex", values_to = "val") |>
  filter(is.na(val)) |>
  select(area, breakdown_var, breakdown_cat, sex)

truth_long <- ses_wide |>
  pivot_longer(c(Female, Male), names_to = "sex", values_to = "truth")

to_long <- function(imp, label)
  imp |>
    pivot_longer(c(Female, Male), names_to = "sex", values_to = "imp") |>
    transmute(area, breakdown_var, breakdown_cat, sex, imp, method = label)

scored <- bind_rows(
  to_long(llm_baseline_oneshot_merged, "LLM_full"),
  to_long(llm_blind_merged,  "LLM_blind"),
  to_long(ses_mice_mnar, "MICE_full"),
  to_long(ses_mice_blind, "MICE_blind")) |>
  inner_join(masked_cells, by = c("area", "breakdown_var", "breakdown_cat", "sex")) |>
  inner_join(truth_long,   by = c("area", "breakdown_var", "breakdown_cat", "sex")) |>
  group_by(area, breakdown_var, breakdown_cat, sex) |>
  filter(all(!is.na(imp))) |>          # keep only cells all four returned
  ungroup()

cell_mae <- function(imp)
  imp |>
    pivot_longer(c(Female, Male), names_to = "sex", values_to = "imp") |>
    inner_join(masked_cells, by = c("area", "breakdown_var", "breakdown_cat", "sex")) |>
    inner_join(truth_long,   by = c("area", "breakdown_var", "breakdown_cat", "sex")) |>
    summarise(n = sum(!is.na(imp)), MAE = mean(abs(imp - truth), na.rm = TRUE))

# table
blind_tab <- scored |>
  group_by(method) |>
  summarise(N = n(), MAE = mean(abs(imp - truth)), .groups = "drop") |>
  mutate(method = factor(method,
           levels = c("LLM_full", "LLM_blind", "MICE_full", "MICE_blind"),
           labels = c("LLM (with covariates)", "LLM (blind)",
                      "MICE (with covariates)", "MICE (blind)"))) |>
  arrange(method) |>
  rename(Method = method)

stargazer(as.data.frame(blind_tab),
          type     = "latex",
          summary  = FALSE,
          rownames = FALSE,
          digits   = 2,
          title    = "Covariate deletion: cell-level MAE (EUR/hour) on the common set of masked cells",
          label    = "tab:ablation")
