## ----results=FALSE-------------------------------------------------------------------------------------------------------
library(tidyr)
library(readxl)
library(dplyr)
library(tidyverse)
library(stringr)
library(readr)
library(ggplot2)
library(DataExplorer)


## ------------------------------------------------------------------------------------------------------------------------
# this will read the index sheet
# we can see that the first two are indexes - so skip
ses_2022 <- read_excel("data/Tavole_Struttura-delle-retribuzioni_2022.xlsx",
                       skip = 2) # skip blank rows on top


all_sheets <- excel_sheets("data/Tavole_Struttura-delle-retribuzioni_2022.xlsx") # list of all sheets
print(ses_2022)


## ------------------------------------------------------------------------------------------------------------------------
tav1_raw <- read_excel("data/Tavole_Struttura-delle-retribuzioni_2022.xlsx",
                       sheet = "Tav.1",
                       col_names = FALSE,
                       col_types = "text")   # force everything to text and avoids type-guessing issues



# rename
tav1_col_names <- c( "row_label", "F_educ_low", "F_educ_med", "F_educ_high", "F_total", "sep1",
                     "M_educ_low", "M_educ_med", "M_educ_high", "M_total", "sep2",
                     "T_educ_low", "T_educ_med", "T_educ_high", "T_total")

tav1_raw <- read_excel("data/Tavole_Struttura-delle-retribuzioni_2022.xlsx",
                       sheet = "Tav.1",
                       col_names = tav1_col_names,
                       col_types = "text",
                       skip = 4)


## ------------------------------------------------------------------------------------------------------------------------
sections <- c("ATTIVITÃ€ ECONOMICA","ATTIVITÃ€ ECONOMICA ",   # ISTAT has a trailing space in some tables so ATTIVITA' ECONOMICA is repeated with a space afterwards too
              "RIPARTIZIONE GEOGRAFICA",
              "CLASSE DIMENSIONALE", 
              "TIPO DI CONTROLLO ECONOMICO")

tav1_clean <- tav1_raw |>
  mutate(
    # when col 1 is NA and col 2 matches a known marker, record the section name
    strat_type = if_else(
      is.na(row_label) & str_trim(F_educ_low) %in% sections,
      str_trim(F_educ_low),
      NA_character_)) |>
  # add section label downward to all rows belonging to that section
  fill(strat_type, .direction = "down") |>
  # drop the section marker rows (row_label is NA) and footnote rows
  filter(!is.na(row_label),
         !str_detect(row_label, "^legenda|^Fonte"),
         !str_detect(row_label, "^None$"))  # in case of leftover "None" strings
  
 
# shorten the section labels to something clean
tav1_clean <- tav1_clean |>
  mutate(strat_type = recode(str_trim(strat_type),
    "ATTIVITÃ€ ECONOMICA" = "sector",
    "ATTIVITÃ€ ECONOMICA "  = "sector",
    "RIPARTIZIONE GEOGRAFICA" = "geography",
    "CLASSE DIMENSIONALE" = "firm_size",
    "TIPO DI CONTROLLO ECONOMICO" = "control_type"))

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
    T_high = T_educ_high)

tav1_clean


## ------------------------------------------------------------------------------------------------------------------------
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


## ----recoding------------------------------------------------------------------------------------------------------------
# education (ISCED)
meta_education_level <- list(raw_names = c("row_label","F_low", "F_med", "F_high", 
                                           "F_total", "sep1",
                                           "M_low", "M_med", "M_high", "M_total", "sep2",
                                           "T_low", "T_med", "T_high", "T_total"),
  breakdown_var = "education",
  cat_labels = c(low = "â‰¤ lower secondary",
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


## ----general function----------------------------------------------------------------------------------------------------
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
    mutate(strat_type = if_else(
      is.na(row_label) & str_trim(.data[[meta$raw_names[2] ]]) %in% sections,
      str_trim(.data[[ meta$raw_names[2] ]]),
        NA_character_)) |>
    fill(strat_type, .direction = "down") |>
    # remove section marker rows, footnotes, and trailing blank rows
    filter(
      !is.na(row_label),
      !str_detect(row_label, fixed("legenda")),
      !str_detect(row_label, fixed("Fonte"))) |>
    # recode section labels
    mutate(strat_type = recode(str_trim(strat_type),
      "ATTIVITÃ€ ECONOMICA"          = "sector",
      "ATTIVITÃ€ ECONOMICA "         = "sector",
      "RIPARTIZIONE GEOGRAFICA"     = "geography",
      "CLASSE DIMENSIONALE"         = "firm_size",
      "TIPO DI CONTROLLO ECONOMICO" = "control_type"))
 
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
      cols = all_of(value_cols),
      names_to  = c("sex", "breakdown_cat"),
      names_sep = "_",
      values_to = "value") |>
    mutate(
      sex = recode(sex, "F" = "Female", "M" = "Male", "T" = "Total"),
      # map short breakdown_cat codes to readable labels
      breakdown_cat_label = recode(breakdown_cat, !!!meta$cat_labels),
      breakdown_var = meta$breakdown_var,
      outcome_var  = outcome_label,
      table_id = sheet_name)
 
  pivot_long
}


## ------------------------------------------------------------------------------------------------------------------------
tav2_test <- parse_ses_table(
  sheet_name  = "Tav.2",
  meta = meta_contract_type,
  outcome_label = "hourly_wage"
)


## ------------------------------------------------------------------------------------------------------------------------
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


## ------------------------------------------------------------------------------------------------------------------------
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


## ------------------------------------------------------------------------------------------------------------------------
ses_data <- ses_data |>
  mutate(breakdown_cat = breakdown_cat_label) |>  
  select(-breakdown_cat_label)

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


## ------------------------------------------------------------------------------------------------------------------------
library(readr)
racli_raw <- read_csv("data/Sesso - prov (IT1,533_957_DF_DCSC_RACLI_8,1.0).csv",
                      quote = "")

racli_raw <- racli_raw |>
  filter(TIME_PERIOD == 2022) # filter 2022


## ------------------------------------------------------------------------------------------------------------------------
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
    #   2 chars (IT)       -> country
    #   3 chars (ITC)      -> NUTS-1 macro-area
    #   4 chars (ITC1)     -> NUTS-2 region
    #   5 chars (ITC11)    -> NUTS-3 province
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


## ------------------------------------------------------------------------------------------------------------------------
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


## ----eda-ses-intro-------------------------------------------------------------------------------------------------------
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


## ----eda-ses-corr--------------------------------------------------------------------------------------------------------
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


## ----gender-gap-sections-------------------------------------------------------------------------------------------------
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


## ----adj-GPG-area-breakdown----------------------------------------------------------------------------------------------
agpg_area_breakdown <- within_gaps |>
  group_by(area, breakdown_var) |>
  mutate(weight = Female / sum(Female, na.rm = TRUE)) |>
  summarise(aGPG = sum(weight * gap, na.rm = TRUE),
            n_categories = n(),
            .groups = "drop") |>
  arrange(area, breakdown_var)

agpg_area_breakdown

# write_csv(agpg_area_breakdown, file = "agpg_area_breakdown.csv")


## ----adj-GPG-area--------------------------------------------------------------------------------------------------------
agpg_area <- agpg_area_breakdown |>
  group_by(area) |>
  summarise(aGPG = mean(aGPG), .groups = "drop") |>
  arrange(desc(aGPG))

agpg_area

# write_csv(agpg_area, file = "agpg_area.csv")


## ----regional-disadv-score-----------------------------------------------------------------------------------------------
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
    # composite: simple average of the two standardized indicators
    reg_dis = (z_wage + z_disp) / 2,
    # labels
    reg_dis_lvl = cut(reg_dis,
                      breaks = quantile(reg_dis, c(0, 1/3, 2/3, 1)),
                      labels = c("low", "moderate", "high"),
                      include.lowest = TRUE)) |>
  arrange(desc(reg_dis))

# adapted to NUTS-1 for SES
# RACLI codes -> SES macro-area labels, average the composite reg_dis within each macro-area, then re-cut into tiers 
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

reg_dis 

# write_csv(reg_dis, file = "reg_dis.csv")


## ------------------------------------------------------------------------------------------------------------------------
library(mice)
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

# create the baseline corrupted MCAR dataset
miss_rate_ses <- 0.20

ses_missing <- ses_wide |> 
  mutate(
    Female = if_else(runif(n()) < miss_rate_ses, NA_real_, Female),
    Male   = if_else(runif(n()) < miss_rate_ses, NA_real_, Male)
  )

# SAVE ses_wide as ground truth - MNAR missingness will be then applied to it
# write_csv(ses_wide, file = "ses_wide_original.csv")

# SAVE ses_missing as this is MCAR missingness
# write_csv(ses_missing, file = "ses_missing_mcar.csv")


## ------------------------------------------------------------------------------------------------------------------------
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

# write_csv(ses_imputed_mcar, file = "ses_imputed_mcar.csv")


## ----MNAR-ses------------------------------------------------------------------------------------------------------------
set.seed(240306) # seed for reproducibility

# MNAR: missingness probability rises with disadvantage tier
tier_p <- c(low = 0.10, moderate = 0.25, high = 0.45)

ses_exp_mnar <- ses_wide |>
  rename(nuts1 = area) |>
  filter(!is.na(nuts1)) |>   # keep defined NUTS-1 areas
  left_join(reg_dis |> select(nuts1, reg_dis_lvl), by = "nuts1") |>
  mutate(p_miss = tier_p[as.character(reg_dis_lvl)],             # per-row drop probability
         drop_F = runif(n()) < p_miss, 
         drop_M = runif(n()) < p_miss,  
         Female = if_else(drop_F, NA_real_, Female),
         Male   = if_else(drop_M, NA_real_, Male)) |> 
  select(-drop_F, -drop_M, -p_miss)

plot_missing(ses_exp_mnar)

# sanity check
ses_exp_mnar |>
  pivot_longer(c(Female, Male), names_to = "sex", values_to = "value") |>
  group_by(reg_dis_lvl) |>
  summarise(missing_rate = mean(is.na(value)), n_cells = n(), .groups = "drop")

# SAVE EXPERIMENTAL DATASET
# write_csv(ses_exp_mnar, file = "ses_exp_mnar.csv")


## ----mice-mnar-ses-------------------------------------------------------------------------------------------------------
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

# write_csv(ses_mice_mnar, file = "ses_mice_mnar.csv")


## ----eval-ses------------------------------------------------------------------------------------------------------------
metrics_tracker <- tibble(
  method    = character(),
  metric    = character(),
  n         = integer(),
  mean_bias = double(),
  MAE       = double(),
  RMSE      = double(),
  spearman  = double())

eval_ses <- function(ses_imp, label) {

  # 1. aGPG-level accuracy (25 area x breakdown cells) 
  imp <- ses_imp |>
    group_by(area, breakdown_var) |>
    mutate(gap    = 1 - Female / Male,
           weight = Female / sum(Female, na.rm = TRUE)) |>
    summarise(value_imp = sum(weight * gap, na.rm = TRUE), .groups = "drop") |>
    unite("unit", area, breakdown_var, sep = " | ")

  truth <- agpg_area_breakdown |>
    select(area, breakdown_var, value_true = aGPG) |>
    unite("unit", area, breakdown_var, sep = " | ")

  scores <- truth |>
    inner_join(imp, by = "unit") |>
    mutate(bias = value_imp - value_true, abs_error = abs(bias)) |>
    summarise(
      method    = label,
      metric    = "agpg",
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

  cat("\n=== Cell-level ===\n")
  print(se |> arrange(desc(abs_error)) |>
    select(area, breakdown_var, breakdown_cat, sex, value_true, value_imp, bias))

  ks <- ks.test(se$value_imp, se$value_true)
  cat(sprintf("\nK-S: D=%.4f  p=%.4f\n", ks$statistic, ks$p.value))
  cat(sprintf("Bias direction: %.1f%% of cells over-estimated\n",
              mean(se$bias > 0, na.rm = TRUE) * 100))

  # 3. Fairness by breakdown variable
  cat("\n=== Fairness by breakdown variable ===\n")
  fairness <- se |>
    group_by(breakdown_var) |>
    summarise(bias = mean(bias, na.rm = TRUE),
              MAE  = mean(abs_error, na.rm = TRUE),
              n    = n(), .groups = "drop") |>
    arrange(breakdown_var)
  print(fairness)
  cat(sprintf("MAE ratio (worst / best): %.3f\n", max(fairness$MAE) / min(fairness$MAE)))

  # 4. Fairness by disadvantage tier (MNAR datasets only)
  if ("reg_dis_lvl" %in% names(se)) {
    cat("\n=== Fairness by disadvantage tier ===\n")
    tier_fair <- se |>
      group_by(reg_dis_lvl) |>
      summarise(bias = mean(bias, na.rm = TRUE),
                MAE  = mean(abs_error, na.rm = TRUE),
                n    = n(), .groups = "drop") |>
      arrange(reg_dis_lvl)
    print(tier_fair)
    cat(sprintf("MAE ratio (high / low tier): %.3f\n",
                max(tier_fair$MAE) / min(tier_fair$MAE)))
  }

  # 5. aGPG MAE at two aggregation levels 
  agpg_cells <- ses_imp |>
    group_by(area, breakdown_var) |>
    mutate(gap    = 1 - Female / Male,
           weight = Female / sum(Female, na.rm = TRUE)) |>
    summarise(aGPG_imp = sum(weight * gap, na.rm = TRUE), .groups = "drop") |>
    left_join(agpg_area_breakdown |> select(area, breakdown_var, aGPG_true = aGPG),
              by = c("area", "breakdown_var")) |>
    mutate(agpg_error = abs(aGPG_imp - aGPG_true))

  agpg_area_res <- agpg_cells |>
    group_by(area) |>
    summarise(aGPG_imp = mean(aGPG_imp), .groups = "drop") |>
    left_join(agpg_area |> select(area, aGPG_true = aGPG), by = "area") |>
    mutate(agpg_error = abs(aGPG_imp - aGPG_true))

  cat(sprintf("\naGPG MAE (25 cells): %.4f\n", mean(agpg_cells$agpg_error, na.rm = TRUE)))
  cat(sprintf("aGPG MAE (5 NUTS-1): %.4f\n",  mean(agpg_area_res$agpg_error, na.rm = TRUE)))

  if (exists("mcar_mae_ses")) {
    cat(sprintf("MNAR/MCAR MAE ratio: %.2fx\n", scores$MAE / mcar_mae_ses))
  }

  invisible(metrics_tracker)
}

# evaluating MNAR and MCAR MICE retreival
eval_ses(ses_mice_mnar, "MICE_MNAR")
eval_ses(ses_imputed_mcar, "MICE_MCAR")


## ------------------------------------------------------------------------------------------------------------------------
library(httr)
library(httr2)
library(jsonlite)

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
      timeout(300))

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
wage_lower        <- 6       # plausible EUR/hr bounds for RACLI
wage_higher        <- 45

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


## ----llm-baseline-ses----------------------------------------------------------------------------------------------------
set.seed(240603)

sys_ses_base <- "You are an economic estimator. Reply with only a single number."

ses_exp_mnar2 <- ses_exp_mnar |>
  pivot_longer(cols = c(Female, Male), names_to = "sex", values_to = "value") |>
  filter(is.na(value)) |>
  select(nuts1, breakdown_var, breakdown_cat, sex)

llm_baseline_ses <- ses_exp_mnar2 |>
  mutate(
    prompt = paste0(
      "What was the mean gross hourly wage for ", sex,
      " workers in ", nuts1, ", Italy, in 2022,",
      " in the ", breakdown_var, " category '", breakdown_cat, "'?",
      " Reply with a single number in EUR per hour, no explanation."),
    value_imp = map_dbl(prompt, \(p) llm_impute_value(p, system = sys_ses_base, hi = 50)))

llm_baseline_ses |> select(nuts1, breakdown_var, breakdown_cat, sex, value_imp)

# write_csv(llm_baseline_ses, file = "llm_baseline_ses.csv")


## ----llm-ses-------------------------------------------------------------------------------------------------------------
ses_llm_baseline_merged <- ses_exp_mnar |>
  pivot_longer(cols = c(Female, Male), names_to = "sex", values_to = "value") |>
  left_join(
    llm_baseline_ses |> select(nuts1, breakdown_var, breakdown_cat, sex, value_imp),
    by = c("nuts1", "breakdown_var", "breakdown_cat", "sex")
  ) |>
  mutate(value = if_else(is.na(value), value_imp, value)) |>
  select(-value_imp) |>
  pivot_wider(names_from = sex, values_from = value) |>
  rename(area = nuts1)

# SAVE
# write_csv(ses_llm_baseline_merged, file = "ses_llm_baseline_merged.csv")


## ----ses-ctx-prep--------------------------------------------------------------------------------------------------------
nat_avg_ses <- ses_wide |>
  pivot_longer(c(Female, Male), names_to = "sex", values_to = "value") |>
  group_by(breakdown_var, breakdown_cat, sex) |>
  summarise(nat_mean = round(mean(value, na.rm = TRUE), 2), .groups = "drop")

area_avg_ses <- ses_exp_mnar |>
  pivot_longer(c(Female, Male), names_to = "sex", values_to = "value") |>
  filter(!is.na(value)) |>
  group_by(nuts1, sex) |>
  summarise(area_mean = round(mean(value), 2), .groups = "drop")

other_areas_ses <- ses_exp_mnar |>
  pivot_longer(c(Female, Male), names_to = "sex", values_to = "value") |>
  filter(!is.na(value)) |>
  group_by(breakdown_var, breakdown_cat, sex) |>
  summarise(
    other_areas = paste0(nuts1, ": ", round(value, 2), " EUR/hr", collapse = "; "),
    .groups = "drop")

ses_obs_long <- ses_exp_mnar |>
  pivot_longer(c(Female, Male), names_to = "sex", values_to = "value") |>
  filter(!is.na(value))

ses_missing_ctx <- ses_exp_mnar |>
  pivot_longer(c(Female, Male), names_to = "sex", values_to = "value") |>
  filter(is.na(value)) |>
  select(nuts1, breakdown_var, breakdown_cat, sex) |>
  mutate(counterpart_sex = if_else(sex == "Female", "Male", "Female")) |>
  left_join(
    ses_obs_long |> rename(counterpart_sex = sex, counterpart_val = value),
    by = c("nuts1", "breakdown_var", "breakdown_cat", "counterpart_sex")) |>
  left_join(nat_avg_ses,     by = c("breakdown_var", "breakdown_cat", "sex")) |>
  left_join(area_avg_ses,    by = c("nuts1", "sex")) |>
  left_join(other_areas_ses, by = c("breakdown_var", "breakdown_cat", "sex"))


## ----ses-ctx-impute, cache=FALSE-----------------------------------------------------------------------------------------
sys_ctx <- paste(
  "You have to recover the missing cells in a data set on Italian wages through imputation.",
  "Only use the provided quantile and macro context.",
  "Do not reproduce memorized ISTAT statistics or exact 2022 official wage figures,",
  "strictly basing your outputs only on what is present in the given data.",
  "Be concise in your reasoning. Return one number in EUR/hour, nothing else.")

llm_ctx_ses <- ses_missing_ctx |>
  mutate(
    ctr_line = ifelse(!is.na(counterpart_val),
      paste0("- Observed ", counterpart_sex, " wage (same area and category): ", counterpart_val, " EUR/hr"),
      paste0("- ", counterpart_sex, " wage (same area and category): not observed")),
    prompt = paste0(
      "Macro-area: ", nuts1, " | Sex: ", sex, " ",
      "Category: ", breakdown_var, " - ", breakdown_cat, " ",
      ctr_line, " ",
      "National avg (", sex, ", this category): ", nat_mean, " EUR/hr",
      "Macro-area mean (", sex, ", all categories): ", area_mean, " EUR/hr",
      "Other areas (", sex, ", same category): ", other_areas, " ",
      "Impute: mean gross hourly wage in EUR/hr."),
    value_imp = map_dbl(prompt, \(p) llm_impute_value(p, system = sys_ctx, hi = 50))
  ) |>
  select(-ctr_line)

llm_ctx_ses |> select(nuts1, breakdown_var, breakdown_cat, sex, counterpart_val, value_imp)
# write_csv(llm_ctx_ses, file = "llm_ctx_ses.csv")


## ----ses-ctx-eval--------------------------------------------------------------------------------------------------------
ses_llm_ctx_merged <- ses_exp_mnar |>
  pivot_longer(c(Female, Male), names_to = "sex", values_to = "value") |>
  left_join(
    llm_ctx_ses |> select(nuts1, breakdown_var, breakdown_cat, sex, value_imp),
    by = c("nuts1", "breakdown_var", "breakdown_cat", "sex")) |>
  mutate(value = if_else(is.na(value), value_imp, value)) |>
  select(-value_imp) |>
  pivot_wider(names_from = sex, values_from = value) |>
  rename(area = nuts1)

# write_csv(ses_llm_ctx_merged, "ses_llm_ctx_merged.csv")


## ----tbl-ctx-eval--------------------------------------------------------------------------------------------------------
# the prompt
sys_tbl <- paste(
  "You are completing a table of Italian gross hourly wages.",
  "Use only the values visible in the table as context.",
  "Do not reproduce memorized ISTAT statistics.",
  "Return only valid JSON with the missing value, nothing else.")

llm_tbl_ses <- ses_missing_ctx |>
  mutate(
    ctr_val = ifelse(!is.na(counterpart_val), paste0(counterpart_val, " EUR/hr"), "MISSING"),
    
      # table-like structure
    prompt = paste0(
      "Area: ", nuts1, " | Sex to impute: ", sex,
      " | Category: ", breakdown_var, " - ", breakdown_cat, "

",
      "| variable | value |
|---|---|
",
      "| ", counterpart_sex, " wage (same area/category) | ", ctr_val, " |",
      "| national mean (", sex, ", this category) | ", nat_mean, " EUR/hr |",
      "| area mean (", sex, ", all categories) | ", area_mean, " EUR/hr |",
      "| other areas (", sex, ", same category) | ", other_areas, " |",
      "| ", sex, " wage | MISSING |",
      'Return {"value": <number>}'),
    value_imp = map_dbl(prompt, \(p) llm_impute_value(p, system = sys_tbl, hi = 50))
  ) |>
  select(-ctr_val)

llm_tbl_ses |> select(nuts1, breakdown_var, breakdown_cat, sex, counterpart_val, value_imp)

# write_csv(llm_tbl_ses, file = "llm_tbl_ses.csv")

ses_llm_tbl_merged <- ses_exp_mnar |>
  pivot_longer(c(Female, Male), names_to = "sex", values_to = "value") |>
  left_join(llm_tbl_ses |> select(nuts1, breakdown_var, breakdown_cat, sex, value_imp),
            by = c("nuts1", "breakdown_var", "breakdown_cat", "sex")) |>
  mutate(value = if_else(is.na(value), value_imp, value)) |>
  select(-value_imp) |>
  pivot_wider(names_from = sex, values_from = value) |>
  rename(area = nuts1)

# write_csv(ses_llm_tbl_merged, file = "ses_llm_tbl_merged.csv")


## ----eval-ses-llm--------------------------------------------------------------------------------------------------------
# baseline
eval_ses(ses_llm_baseline_merged, "LLM_uninformed")

# contextual
eval_ses(ses_llm_ctx_merged, "LLM_ctx_v1")

# structured table completion
eval_ses(ses_llm_tbl_merged, "LLM_tbl_v1")


## ----llm-oneshot-impute, cache=FALSE-------------------------------------------------------------------------------------
# reference table to give the LLM context: education / occupation / tenure / contract-type
ref_csv <- ses_data |>
  filter(strat_type != "geography",
         outcome_var == "hourly_wage",
         breakdown_var %in% c("education", "occupation", "tenure", 
                              "contract_type", "hours_regime"),
         breakdown_cat != "total",
         sex %in% c("Female", "Male"),
         !is.na(value)) |>
  select(strat_type, strat_value = row_label, breakdown_var, 
         breakdown_cat, sex, value) |>
  mutate(value = round(value, 2),
         row   = paste(strat_type, strat_value, breakdown_var, 
                       breakdown_cat, sex, value, 
                       sep = ",")) |>
  pull(row) |>
  paste(collapse = "\n")

# target: MNAR table -> tag each MISSING cell with a numeric ID so the join is ID-based, to aviod mismatch
mnar_long <- ses_exp_mnar |>
  rename(area = nuts1) |>
  select(area, breakdown_var, breakdown_cat, Female, Male) |>
  pivot_longer(c(Female, Male), names_to = "sex", values_to = "val") |>
  mutate(cell_id = row_number(),
         display = if_else(is.na(val), paste0("MISSING_", cell_id), 
                           as.character(round(val, 2))),
         row = paste(cell_id, area, breakdown_var, breakdown_cat, 
                     sex, display, sep = ","))

mnar_csv  <- paste(mnar_long$row, collapse = "\n")
missing_keys <- mnar_long |> 
  filter(is.na(val)) |> 
  select(cell_id, area, breakdown_var, breakdown_cat, sex)

# prompt
sys_oneshot <- paste(
  "You are completing missing wage cells in an ISTAT Structure of Earnings survey table.",
  "All values are mean gross hourly wages in EUR/hour, Italy 2022.",
  "You are given two tables: a reference table and a target table stratified by NUTS-1 macro-area.",
  "Learn ONLY from the observed reference table, strictly basing your outputs on what is present in the given data.",
  "Each missing cell in the target table is marked MISSING_<id> where <id> is a number.",
  "Return ONLY a two-column CSV with header: cell_id,value",
  "One row per missing cell, where cell_id is the number from MISSING_<id> and value is your imputed wage.",
  "Values must be between 6 and 50. No markdown, no explanation.")

prompt_oneshot <- paste0(
  "REFERENCE TABLE (Italian wage patterns by sector, firm size, and control type)\n",
  "strat_type,strat_value,breakdown_var,breakdown_cat,sex,value\n", ref_csv, "\n\n",
  "TARGET TABLE (geography-stratified wages: impute every MISSING_<id> cell)\n",
  "cell_id,area,breakdown_var,breakdown_cat,sex,value\n", mnar_csv, "\n\n",
  "Return a CSV with header: cell_id,value: one row per MISSING_<id> cell only.")

# imputation
set.seed(240603)
raw_oneshot <- llm_chat(prompt_oneshot, system = sys_oneshot, max_tokens = 32768)

# parse: find the cell_id,value header and read from there
clean_resp <- strip_think(raw_oneshot)
clean_resp <- gsub("```[a-z]*\n?|```", "", clean_resp) # strip markdown fences
lines <- strsplit(trimws(clean_resp), "\n")[[1]]
lines <- lines[nchar(trimws(lines)) > 0]  # drop blank lines

# error catch
if (length(lines) == 0 || all(is.na(lines)))
  stop("LLM returned empty response")

hdr_idx  <- which(grepl("^cell_id,", lines, ignore.case = TRUE))[1]
if (is.na(hdr_idx)) hdr_idx <- 1
csv_text <- paste(lines[hdr_idx:length(lines)], collapse = "\n")

# I() forces read_csv to treat csv_text as literal text, never as a file path
llm_oneshot_df <- read_csv(I(csv_text), show_col_types = FALSE) |>
  mutate(value = suppressWarnings(as.numeric(value)),
         value = if_else(value < wage_lower | value > wage_higher, NA_real_, 
                         value)) |>
  left_join(missing_keys, by = "cell_id") # join by id

# checking the work
returned <- llm_oneshot_df |> filter(!is.na(cell_id), !is.na(value))
cat(sprintf("masked: %d | returned: %d | usable: %d | still NA: %d\n",
            nrow(missing_keys), nrow(llm_oneshot_df),
            nrow(returned), nrow(missing_keys) - nrow(returned)))

print(llm_oneshot_df)

# merged
ses_llm_oneshot_merged <- ses_exp_mnar |>
  pivot_longer(c(Female, Male), names_to = "sex", values_to = "value") |>
  left_join(llm_oneshot_df |> rename(nuts1 = area, value_imp = value) |>
              select(nuts1, breakdown_var, breakdown_cat, sex, value_imp),
            by = c("nuts1", "breakdown_var", "breakdown_cat", "sex")) |>
  mutate(value = if_else(is.na(value), value_imp, value)) |>
  select(-value_imp) |>
  pivot_wider(names_from = sex, values_from = value) |>
  rename(area = nuts1)

# write_csv(llm_oneshot_df, file = "llm_oneshot_df.csv")
# write_csv(ses_llm_oneshot_merged, "ses_llm_oneshot_merged.csv")

plot_missing(ses_llm_oneshot_merged)


## ----eval-oneshot--------------------------------------------------------------------------------------------------------
eval_ses(ses_llm_oneshot_merged, "LLM_oneshot_v1")

