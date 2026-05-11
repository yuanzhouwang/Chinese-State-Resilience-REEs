library(dplyr)
library(tidyr)
library(stringr)
library(gsynth)
library(readr)
library(tibble)
library(gsynth)
library(panelView)
library(Amelia)
library(purrr)
library(ggplot2)

load("MI_df_AYWMATHESIS.Rdata")


make_did_panel <- function(df) {
  df %>%
    as_tibble() %>%
    mutate(
      region = as.character(region),
      year = as.integer(year)
    ) %>%
    group_by(region) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(
      rail_new_km     = rail_km - lag(rail_km, 1),
      rail_new_km_l1  = lag(rail_new_km, 1),
      rail_new_km_l2  = lag(rail_new_km, 2),
      rail_new_km_l3  = lag(rail_new_km, 3),
      rail_new_km_l5  = lag(rail_new_km, 5),
      freight_rail_l3 = lag(freight_rail, 3),
      rail_freight_l3 = lag(rail_freight, 3),
      rail_ppl_l3     = lag(rail_ppl, 3),
      rail_ppl_l5     = lag(rail_ppl, 5),
      pop_density     = population / area_10k_sqkm
    ) %>%
    ungroup() %>%
    mutate(
      id    = as.numeric(factor(region)),
      treat = ifelse(region == "Inner Mongolia" & year >= 2010, 1, 0)
    )
}

imputed_panels <- lapply(amelia_fit$imputations, make_did_panel)
