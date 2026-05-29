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
library(fixest)

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

nonprovinces <- c("Dalian", "Ningbo", "Xiamen", "Qingdao", "Shenzhen")

borders <- c("Heilongjiang", "Jilin", "Liaoning", "Fujian", "Yunnan",
                "Xinjiang", "Tibet", "Guangxi", "Inner Mongolia", "Gansu")
autonomous <- c("Inner Mongolia", "Xinjiang", "Tibet", "Guangxi", "Gansu")


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
         border = ifelse(region %in% borders, 1, 0),
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
    time = as.numeric(time),
    raillength = as.numeric(`Length of Railways in Operation (km)`),
    roadlength = as.numeric(`Expressways and Classified Highways (km)`)
  ) %>%
  select(region, time, raillength, roadlength)

mfr <- read_xlsx("CHN_MFR_P.xlsx") %>%
  mutate(
    region = 地区,
    time = as.numeric(时间)
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
         time = as.numeric(时间),
         transfer_size = 数值) %>%
  select(region, time, transfer_size) %>%
  mutate(region = do.call(recode, append(list(region), prov_translations)),
         transfer_size = as.numeric(transfer_size))

totaltransfers <- transfers %>%
  group_by(time) %>%
  summarise(total_transfers = sum(transfer_size),
            .groups = "drop")


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
    time = as.numeric(time),
    edufunds = as.numeric(`Educational Funds (10,000 yuan)`),
    healthbeds = as.numeric(`Beds in Hospitals and Health Centers (10,000 units)`)
  ) %>%
  select(region, time, edufunds, healthbeds)

pg_all <- pg %>%
  inner_join(population_2010, by = "region") %>%
  mutate(edu_pc = edufunds / pop_10k,
         beds_pc = healthbeds / pop_10k) %>%
  group_by(region) %>%
  summarise(
    edu_pc_pre = mean(edu_pc, na.rm = TRUE),
    beds_pc_pre = mean(beds_pc, na.rm = TRUE),
    .groups = "drop"
  )

pg_final <- pg_all %>%
  mutate(z_edu_pc = zscore(edu_pc_pre),
         z_beds_pc = zscore(beds_pc_pre),
         pg_index = z_edu_pc + z_beds_pc)
  

# Other Controls ----------------------------------------------------------

urb <- read_xlsx("CHN_Urb.xlsx") %>%
  mutate(urb_rate = as.numeric(Val),
         time = as.numeric(time)) %>%
  select(region, time, urb_rate) %>%
  na.omit()

logpop <- read_xlsx("CHN_Pop_all.xlsx") %>%
  mutate(log_pop10k = log(as.numeric(Val)),
         pop10k = as.numeric(Val),
         time = as.numeric(time)) %>%
  select(region, time, log_pop10k, pop10k) %>%
  na.omit()

gdppc <- read_xlsx("CHN_Pop_all.xlsx") %>%
  mutate(time = as.numeric(time)) %>%
  inner_join(mfr, by = c("region", "time")) %>%
  mutate(gdppc = as.numeric(gdp) / as.numeric(Val),
         log_gdppc = log(gdppc)) %>%
  select(region, time, log_gdppc, gdppc) %>%
  na.omit()

# Main DID analysis -------------------------------------------------------

panel <- left_join(transfers, mfr_final, by = "region") %>%
  left_join(sensitivity, by = "region") %>%
  left_join(reeExp, by = "region") %>%
  left_join(pg_final, by = "region") %>%
  left_join(urb, by = c("region", "time")) %>%
  left_join(logpop, by = c("region", "time")) %>%
  left_join(gdppc, by = c("region", "time")) %>%
  left_join(totaltransfers, by = "time") %>%
  filter(!(region %in% nonprovinces)) %>%
  filter(time <= 2013) %>%
  mutate(post = ifelse(time >= 2010, 1, 0),
         transfer_pc = transfer_size / pop10k,
         log_transfer_pc = log(transfer_pc),
         transfer_share = transfer_size / total_transfers,
         ree_post = reeExp * post,
         post_mfr = post * mfr_index,
         post_sensitivity = post * sensitivity_index,
         post_pg = post * pg_index)

mainmodel <- transfer_pc ~
  post_mfr +
  post_sensitivity |
  time + region

reeexpmodel <- transfer_share ~
  post_mfr +
  post_sensitivity +
  reeExp +
  log_gdppc |
  time

feols(mainmodel, data = panel, cluster = ~region)
feols(reeexpmodel, data = panel, cluster = ~region)

# set.seed(202605)
# boottest(
#   mainmodel,
#   clustid = "region",
#   param = "post_sensitivity",
#   B = 9999
# )


# Sensitivity analysis ----------------------------------------------------

