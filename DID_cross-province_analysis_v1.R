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
library(readxl)

load("MI_df_AYWMATHESIS.Rdata")

prov_translations <- list("北京" = "Beijing",
                          "天津" = "Tianjin",
                          "河北省" = "Hebei",
                          "山西省" = "Shanxi",
                          "内蒙古自治区" = "Inner Mongolia",
                          "辽宁省" = "Liaoning",
                          "大连市" = "Dalian",
                          "吉林省" = "Jilin",
                          "黑龙江省" = "Heilongjiang",
                          "上海" = "Shanghai",
                          "江苏省" = "Jiangsu",
                          "浙江省" = "Zhejiang",
                          "宁波市" = "Ningbo",
                          "安徽省" = "Anhui",
                          "福建省" = "Fujian",
                          "厦门市" = "Xiamen",
                          "江西省" = "Jiangxi",
                          "山东省" = "Shandong",
                          "青岛市" = "Qingdao",
                          "河南省" = "Henan",
                          "湖北省" = "Hubei",
                          "湖南省" = "Hunan",
                          "广东省" = "Guangdong",
                          "深圳市" = "Shenzhen",
                          "广西壮族自治区" = "Guangxi",
                          "海南省" = "Hainan",
                          "重庆市" = "Chongqing",
                          "四川省" = "Sichuan",
                          "贵州省" = "Guizhou",
                          "云南省" = "Yunnan",
                          "西藏自治区" = "Tibet",
                          "陕西省" = "Shaanxi",
                          "甘肃省" = "Gansu",
                          "青海省" = "Qinghai",
                          "宁夏回族自治区" = "Ningxia",
                          "新疆维吾尔自治区" = "Xinjiang",
                          
                          "河北" = "Hebei",
                          "山西" = "Shanxi",
                          "内蒙古" = "Inner Mongolia",
                          "辽宁" = "Liaoning",
                          "大连" = "Dalian",
                          "吉林" = "Jilin",
                          "黑龙江" = "Heilongjiang",
                          "上海" = "Shanghai",
                          "江苏" = "Jiangsu",
                          "浙江" = "Zhejiang",
                          "宁波" = "Ningbo",
                          "安徽" = "Anhui",
                          "福建" = "Fujian",
                          "厦门" = "Xiamen",
                          "江西" = "Jiangxi",
                          "山东" = "Shandong",
                          "青岛" = "Qingdao",
                          "河南" = "Henan",
                          "湖北" = "Hubei",
                          "湖南" = "Hunan",
                          "广东" = "Guangdong",
                          "深圳" = "Shenzhen",
                          "广西" = "Guangxi",
                          "海南" = "Hainan",
                          "重庆" = "Chongqing",
                          "四川" = "Sichuan",
                          "贵州" = "Guizhou",
                          "云南" = "Yunnan",
                          "西藏" = "Tibet",
                          "陕西" = "Shaanxi",
                          "甘肃" = "Gansu",
                          "青海" = "Qinghai",
                          "宁夏" = "Ningxia",
                          "新疆" = "Xinjiang")

borders <- list("Heilongjiang",
                "Jilin",
                "Liaoning",
                "Yunnan")
autonomous <- list("Inner Mongolia", "Xinjiang", "Tibet", "Guangxi", "Gansu")

area <- as.data.frame(
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

population_2010 <- read_xlsx("CHN_Pop.xlsx") %>%
  filter(time == 2010) %>%
  mutate(pop_10k = as.numeric(Val)) %>%
  select(region, pop_10k)

zscore <- function(x) {
  as.numeric(scale(x))
}


# Political Sensitivity ---------------------------------------------------

sensitivity <- read_xlsx("CHN_Han.xlsx") %>%
  na.omit() %>%
  mutate(region = 地区) %>%
  select(指标, region, 数值) %>%
  pivot_wider(names_from = 指标, values_from = 数值) %>%
  select(region, Population, `Han Population`) %>%
  mutate(region = do.call(recode, append(list(region), prov_translations))) %>%
  mutate(totalpop = as.integer(Population), hanpop = as.integer(`Han Population`)) %>%
  select(region, totalpop, hanpop) %>%
  mutate(minorityshare = 1 - hanpop / totalpop,
         border = ifelse(region %in% borders | region %in% autonomous, 1, 0),
         autonomous = ifelse(region %in% autonomous, 1, 0))

sensitivity <- sensitivity %>%
  mutate(minorityshare_adj = zscore(minorityshare),
         border_adj = zscore(border),
         autonomous_adj = zscore(autonomous),
         sensitivity_index = minorityshare_adj + autonomous_adj + border_adj)

# MFR ---------------------------------------------------------------------

mfr_p <- read_xlsx("CHN_PublicGoods.xlsx") %>%
  filter(!is.na(region), !is.na(time), !is.na(indicator), !is.na(Val)) %>%
  pivot_wider(names_from = indicator, values_from = Val) %>%
  select(
    region, time,
    `Length of Railways in Operation (km)`,
    `Expressways and Classified Highways (km)`
  ) %>%
  mutate(
    time = as.integer(time),
    raillength = as.numeric(`Length of Railways in Operation (km)`),
    roadlength = as.numeric(`Expressways and Classified Highways (km)`)
  ) %>%
  select(region, time, raillength, roadlength)

mfr <- read_xlsx("CHN_MFR_P.xlsx") %>%
  mutate(
    region = 地区,
    time = as.integer(时间)
  ) %>%
  select(指标, region, time, 数值) %>%
  filter(region != "地方合计") %>%
  pivot_wider(names_from = 指标, values_from = 数值) %>%
  mutate(
    lfr = as.numeric(`税收收入（亿元）`),
    gdp = as.numeric(`地区生产总值（当年价）（亿元）`)
  ) %>%
  select(region, time, lfr, gdp) %>%
  mutate(
    region = do.call(recode, append(list(region), prov_translations))
  ) %>%
  filter(!is.na(region), !is.na(time), !is.na(lfr), !is.na(gdp))

mfr_all <- mfr_p %>%
  inner_join(mfr, by = c("region", "time")) %>%
  inner_join(population_2010, by = "region") %>%
  arrange(region, time) %>%
  group_by(region) %>%
  mutate(
    rail_pc = raillength / pop_10k,
    road_pc = roadlength / pop_10k,
    local_fiscal_return = lfr / gdp,
    gdp_growth = (gdp - lag(gdp)) / lag(gdp)
  ) %>%
  ungroup()

mfr_final <- mfr_all %>%
  filter(time >= 2005, time <= 2009) %>%
  group_by(region) %>%
  summarise(
    local_fiscal_return_pre = mean(local_fiscal_return, na.rm = TRUE),
    gdp_growth_pre = mean(gdp_growth, na.rm = TRUE),
    rail_pc_pre = mean(rail_pc, na.rm = TRUE),
    road_pc_pre = mean(road_pc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    z_local_fiscal_return = zscore(local_fiscal_return_pre),
    z_gdp_growth = zscore(gdp_growth_pre),
    z_rail_pc = zscore(rail_pc_pre),
    z_road_pc = zscore(road_pc_pre),
    mfr_index =
      z_local_fiscal_return +
      z_gdp_growth -
      z_rail_pc
  )
# Transfers ---------------------------------------------------------------

transfers <- read_xlsx("CHN_Transfers.xlsx") %>%
  mutate(region = 地区,
         year = 时间,
         transfer_size = 数值) %>%
  select(region, year, transfer_size) %>%
  mutate(region = do.call(recode, append(list(region), prov_translations)),
         transfer_size = as.numeric(transfer_size))


# Rare-Earth Exposure -----------------------------------------------------

reeExp <- tribble(
  ~region, ~reeExp,
  "Beijing", 0,
  "Tianjin", 0,
  "Hebei", 0,
  "Shanxi", 0,
  "Inner Mongolia", 55,
  "Liaoning", 0,
  "Dalian", 0,
  "Jilin", 0,
  "Heilongjiang", 0,
  "Shanghai", 0,
  "Jiangsu", 0,
  "Zhejiang", 0,
  "Ningbo", 0,
  "Anhui", 0,
  "Fujian", 6,
  "Xiamen", 0,
  "Jiangxi", 6,
  "Shandong", 0,
  "Qingdao", 0,
  "Henan", 0,
  "Hubei", 0,
  "Hunan", 0,
  "Guangdong", 6,
  "Shenzhen", 0,
  "Guangxi", 0,
  "Hainan", 0,
  "Chongqing", 0,
  "Sichuan", 27,
  "Guizhou", 0,
  "Yunnan", 0,
  "Tibet", 0,
  "Shaanxi", 0,
  "Gansu", 0,
  "Qinghai", 0,
  "Ningxia", 0,
  "Xinjiang", 0
)

# Control - Public Goods --------------------------------------------------

pg <- read_xlsx("CHN_PublicGoods.xlsx") %>%
  filter(!is.na(region), !is.na(time), !is.na(indicator), !is.na(Val)) %>%
  pivot_wider(names_from = indicator, values_from = Val) %>%
  select(
    region, time,
    `Educational Funds (10,000 yuan)`,
    `Beds in Hospitals and Health Centers (10,000 units)`
  ) %>%
  mutate(
    time = as.integer(time),
    edufunds = as.numeric(`Educational Funds (10,000 yuan)`),
    healthbeds = as.numeric(`Beds in Hospitals and Health Centers (10,000 units)`)
  ) %>%
  select(region, time, edufunds, healthbeds)

pg_all <- pg %>%
  inner_join(population_2010, by = "region") %>%
  mutate(edu_pc = edufunds / pop_10k,
         beds_pc = healthbeds / pop_10k)
  

# Other Controls ----------------------------------------------------------



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
