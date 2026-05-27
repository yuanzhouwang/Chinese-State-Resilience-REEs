library(dplyr)
library(tidyr)
library(stringr)
library(gsynth)
library(readr)
library(tibble)
library(panelView)
library(Amelia)

# Load data ---------------------------------------------------------------


# Renamed, downloaded from EPS China Data
infile <- "China_Macro_Economy-Yearly_by_Province_clean_v2.csv"
twofile <- "China_RR-Yearly_(by_Province).csv"

macro_long <- read_csv(
  infile,
  show_col_types = FALSE
) %>%
  mutate(
    region = str_trim(region),
    indicator = str_trim(indicator),
    time = as.integer(time),
    Val = as.numeric(Val)
  )

macro_two <- read_csv(
  twofile,
  show_col_types = FALSE
) %>%
  mutate(
    region = str_trim(region),
    indicator = str_trim(indicator),
    time = as.integer(time),
    Val = as.numeric(Val)
  )

macro <- bind_rows(macro_long, macro_two)

panel <- macro %>%
  pivot_wider(names_from = indicator, values_from = Val) %>%
  arrange(region, time)

# Helper: rename columns only when the source column exists.
rename_if_present <- function(df, old, new) {
  if (old %in% names(df) && !(new %in% names(df))) {
    names(df)[names(df) == old] <- new
  }
  df
}

panel <- panel %>%
  rename_if_present("government_expenditure_100m_yuan", "govexp_old") %>%
  rename_if_present("general_budgetary_expenditure_100m_yuan", "govexp_new") %>%
  rename_if_present("railways_length_km", "rail_km") %>%
  rename_if_present("railways_freight_10k_tons", "freight_rail") %>%
  rename_if_present("disposable_income_yuan", "pc_income") %>%
  rename_if_present("educational_funds_10k_yuan", "edu_funds") %>%
  rename_if_present("total_population_10k_persons", "population") %>%
  rename_if_present("National Railways (100 million passenger-km)", "rail_ppl") %>%
  rename_if_present("National Railways (100 million ton-km)", "rail_freight")

# Try to recover a GDP column if the exact EPS/NBS variable name differs across downloads.
gdp_candidates <- c(
  "gdp",
  "GDP",
  "gross_regional_product_100m_yuan",
  "regional_gross_domestic_product_100m_yuan",
  "gross_domestic_product_100m_yuan",
  "regional_gdp_100m_yuan",
  "GRP_100m_yuan"
)

gdp_candidates <- gdp_candidates[gdp_candidates %in% names(panel)]
if (length(gdp_candidates) > 0 && !"gdp" %in% names(panel)) {
  panel$gdp <- panel[[gdp_candidates[1]]]
}

panel <- panel %>%
  mutate(
    year = as.integer(time),
    provexp = case_when(
      "govexp_old" %in% names(.) & !is.na(govexp_old) ~ govexp_old,
      "govexp_new" %in% names(.) & !is.na(govexp_new) ~ govexp_new,
      TRUE ~ NA_real_
    )
  )


# Province size and population --------------------------------------------

province_lookup <- tribble(
  ~region, ~area_10k_sqkm,
  "Beijing", 1.641,
  "Tianjin", 1.194,
  "Hebei", 18.88,
  "Shanxi", 15.67,
  "Inner Mongolia", 118.30,
  "Liaoning", 14.80,
  "Jilin", 18.74,
  "Heilongjiang", 47.30,
  "Shanghai", 0.634,
  "Jiangsu", 10.72,
  "Zhejiang", 10.55,
  "Anhui", 14.01,
  "Fujian", 12.40,
  "Jiangxi", 16.69,
  "Shandong", 15.80,
  "Henan", 16.70,
  "Hubei", 18.59,
  "Hunan", 21.18,
  "Guangdong", 17.98,
  "Guangxi", 23.76,
  "Hainan", 3.54,
  "Chongqing", 8.24,
  "Sichuan", 48.60,
  "Guizhou", 17.62,
  "Yunnan", 39.41,
  "Tibet", 122.84,
  "Shaanxi", 20.56,
  "Gansu", 42.59,
  "Qinghai", 72.23,
  "Ningxia", 6.64,
  "Xinjiang", 166.49
)

panel <- panel %>%
  left_join(province_lookup, by = "region") %>%
  mutate(
    id = as.numeric(factor(region)),
    treat = ifelse(region == "Inner Mongolia" & year >= 2010, 1, 0)
  )

# Exclude problematic provinces from prior design.
panel_main <- panel %>%
  filter(region != "Tibet", region != "Tianjin")


# Multiple Imputation -----------------------------------------------------

set.seed(202605)

imp_vars <- c(
  "region", "year",
  "rail_km",
  "freight_rail",
  "rail_freight",
  "rail_ppl",
  "provexp",
  "pc_income",
  "population",
  "area_10k_sqkm",
  "edu_funds",
  "gdp"
)

imp_base <- panel_main %>%
  select(any_of(imp_vars)) %>%
  arrange(region, year)

cat("\nMissingness before imputation:\n")
print(sapply(imp_base, function(x) sum(is.na(x))))

missmap(imp_base, main = "Missingness before Amelia imputation")

amelia_out <- amelia(
  x = imp_base,
  m = 20,                  # number of multiply imputed datasets
  cs = "region",           # panel unit
  ts = "year",             # time variable
  polytime = 2,            # flexible common time trend
  intercs = TRUE,          # region-specific time trends
  p2s = 2                  # print progress
)


# Analysis ----------------------------------------------------------------

make_analysis_df <- function(df) {
  out <- df %>%
    group_by(region) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(
      rail_new_km = rail_km - lag(rail_km, 1),
      rail_new_km_l1 = lag(rail_new_km, 1),
      rail_new_km_l2 = lag(rail_new_km, 2),
      rail_new_km_l3 = lag(rail_new_km, 3),
      rail_new_km_l5 = lag(rail_new_km, 5),
      freight_rail_l3 = if ("freight_rail" %in% names(.)) lag(freight_rail, 3) else NA_real_,
      rail_freight_l3 = if ("rail_freight" %in% names(.)) lag(rail_freight, 3) else NA_real_,
      rail_ppl_l3 = if ("rail_ppl" %in% names(.)) lag(rail_ppl, 3) else NA_real_,
      provexp_l3 = if ("provexp" %in% names(.)) lag(provexp, 3) else NA_real_,
      pc_income_l3 = if ("pc_income" %in% names(.)) lag(pc_income, 3) else NA_real_,
      pop_density = population / area_10k_sqkm,
      pop_density_l3 = lag(pop_density, 3)
    ) %>%
    ungroup()

  if ("gdp" %in% names(out)) {
    out <- out %>%
      group_by(region) %>%
      arrange(year, .by_group = TRUE) %>%
      mutate(
        # Conventional GDP growth: denominator is prior-year GDP.
        gdp_growth = (gdp - lag(gdp, 1)) / lag(gdp, 1),
        # Log growth is often better behaved and approximately equals percent growth.
        gdp_log_growth = log(gdp) - log(lag(gdp, 1)),
        gdp_growth_l3 = lag(gdp_growth, 3),
        gdp_log_growth_l3 = lag(gdp_log_growth, 3)
      ) %>%
      ungroup()
  } else {
    out <- out %>%
      mutate(
        gdp = NA_real_,
        gdp_growth = NA_real_,
        gdp_log_growth = NA_real_,
        gdp_growth_l3 = NA_real_,
        gdp_log_growth_l3 = NA_real_
      )
  }

  out %>%
    mutate(
      id = as.numeric(factor(region)),
      treat = ifelse(region == "Inner Mongolia" & year >= 2010, 1, 0)
    )
}

analysis_imputed_list <- lapply(amelia_out$imputations, make_analysis_df)

# Example: use the first imputed dataset.
analysis_df <- analysis_imputed_list[[1]]

cat("\nMissingness after imputation and lag construction, first imputed panel:\n")
print(sapply(analysis_df, function(x) sum(is.na(x))))


# Helpers for gsynth ------------------------------------------------------

make_gsynth_formula <- function(outcome, covariates, data) {
  covariates <- covariates[covariates %in% names(data)]
  covariates <- covariates[sapply(data[covariates], function(x) sum(!is.na(x)) > 0)]
  as.formula(paste(outcome, "~ treat +", paste(covariates, collapse = " + ")))
}

prep_est_df <- function(df, outcome, covariates, min_year = NULL, max_year = NULL) {
  keep <- c("region", "id", "year", "treat", outcome, covariates)
  keep <- keep[keep %in% names(df)]

  out <- df %>%
    select(all_of(keep))

  if (!is.null(min_year)) {
    out <- out %>% filter(year >= min_year)
  }
  if (!is.null(max_year)) {
    out <- out %>% filter(year <= max_year)
  }

  out %>%
    filter(complete.cases(.))
}

# main model ------------------------------------------------------------

main_covars <- c(
  "pop_density_l3",
  "provexp_l3",
  "freight_rail_l3",
  "rail_freight_l3",
  "rail_ppl_l3",
  "pc_income_l3",
  "gdp_log_growth_l3"
)

main_formula <- make_gsynth_formula(
  outcome = "rail_new_km",
  covariates = main_covars,
  data = analysis_df
)

est_df <- prep_est_df(
  df = analysis_df,
  outcome = "rail_new_km",
  covariates = all.vars(main_formula)[-1],
  min_year = min(analysis_df$year, na.rm = TRUE) + 4
)

cat("\nMain formula:\n")
print(main_formula)

set.seed(20260409)

model_main <- gsynth(
  formula = main_formula,
  data = est_df,
  index = c("id", "year"),
  force = "two-way",
  CV = TRUE,
  r = c(0, 5),
  se = TRUE,
  inference = "parametric",
  nboots = 200,
  parallel = FALSE,
  min.T0 = 5
)

print(summary(model_main))
plot(model_main)
plot(model_main, type = "counterfactual", raw = "all")


# Placebo model: treat in 2008 --------------------------------------------

est_df_placebo <- est_df %>%
  mutate(treat = ifelse(region == "Inner Mongolia" & year >= 2008, 1, 0))

set.seed(20260409)

model_placebo_2008 <- gsynth(
  formula = main_formula,
  data = est_df_placebo,
  index = c("id", "year"),
  force = "two-way",
  CV = TRUE,
  r = c(0, 5),
  se = TRUE,
  inference = "parametric",
  nboots = 200,
  parallel = FALSE,
  min.T0 = 5
)

print(summary(model_placebo_2008))
plot(model_placebo_2008, type = "gap")
plot(model_placebo_2008, type = "counterfactual", raw = "all")


# Sensitivity model -------------------------------------------------------

# This includes short lags of the outcome only as predictors for persistence

lean_covars <- c(
  "rail_new_km_l1",
  "rail_new_km_l2",
  "pop_density_l3",
  "provexp_l3",
  "freight_rail_l3",
  "gdp_log_growth_l3"
)

lean_formula <- make_gsynth_formula(
  outcome = "rail_new_km",
  covariates = lean_covars,
  data = analysis_df
)

est_df_lean <- prep_est_df(
  df = analysis_df,
  outcome = "rail_new_km",
  covariates = all.vars(lean_formula)[-1],
  min_year = min(analysis_df$year, na.rm = TRUE) + 4
)

cat("\nLean formula:\n")
print(lean_formula)

set.seed(123)

model_lean <- gsynth(
  formula = lean_formula,
  data = est_df_lean,
  index = c("id", "year"),
  force = "two-way",
  CV = TRUE,
  r = c(0, 5),
  se = TRUE,
  inference = "parametric",
  nboots = 200,
  parallel = FALSE,
  min.T0 = 5
)

print(summary(model_lean))
plot(model_lean)
plot(model_lean, type = "counterfactual", raw = "all")


# Run main specification on all imputations -------------------------------

run_main_on_imputation <- function(df, imp_id) {
  fml <- make_gsynth_formula("rail_new_km", main_covars, df)
  dat <- prep_est_df(
    df = df,
    outcome = "rail_new_km",
    covariates = all.vars(fml)[-1],
    min_year = min(df$year, na.rm = TRUE) + 4
  )

  set.seed(20260409 + imp_id)

  gsynth(
    formula = fml,
    data = dat,
    index = c("id", "year"),
    force = "two-way",
    CV = TRUE,
    r = c(0, 5),
    se = FALSE,
    inference = "none",
    parallel = FALSE,
    min.T0 = 5
  )
}

# Uncomment if you want to run all 20 imputed panels.
# models_all_imputations <- lapply(seq_along(analysis_imputed_list), function(j) {
#   run_main_on_imputation(analysis_imputed_list[[j]], j)
# })

# -----------------------------
# 10. Export cleaned analysis panel
# -----------------------------

write.csv(analysis_df, "inner_mongolia_gsynth_panel_v5.csv", row.names = FALSE)

cat("\nSaved cleaned panel to inner_mongolia_gsynth_panel_v5.csv\n")
