
# 2/4/2023
# This script generates market integration scenario for palm trade

# Note that the integration scenario for palm oil will be only for 
# international markets; this rely on the bilateral trade structure.
# Previously, in the hindcast, only regional home bias erosion is tested
# However, palm oil production and export is centered in a few key regions

# The most important integration will be as the international competition 
# that is bias erosion at the international market 

# Demand drivers will also be important, e.g., substitution and demand responses


library(dplyr)
library(gcamdata)
source("R/funcs.R")


INPUT_DIR <- "input/PalmBiTrade/"
OUTPUT_DIR <- "output/PalmBiTrade"
AG_TRADE_DIR <- "input/PalmBiTrade/agtrade_gcamoutput/"

MODEL_BASE_YEARS        <- c(1975, 1990, 2005, 2010, 2015)
MODEL_FUTURE_YEARS      <- seq(2020, 2100, 5)
convergence.period <- length(MODEL_FUTURE_YEARS)


# Step1 Load data ----
readRDS(file.path(INPUT_DIR, "/ag_trade_xml_Module_Inputs.rds")) -> all_data
# rds came from driver_drake

MODULE_INPUTS <-
  c("L240.Supplysector_tra",
    "L240.SectorUseTrialMarket_tra",
    "L240.SubsectorAll_tra",
    "L240.TechShrwt_tra",
    "L240.TechCost_tra",
    "L240.TechCoef_tra",
    "L240.Production_tra",
    "L240.Supplysector_reg",
    "L240.SubsectorAll_reg",
    "L240.TechShrwt_reg",
    "L240.TechCoef_reg",
    "L240.Production_reg_imp",
    "L240.Production_reg_dom")
# Load required inputs 
get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

# Step1.1 Generate the default ag_trade.xml & ensure reconciliation ----
outxml_name <- "0_ag_trade.xml"

gcamdata::create_xml(file.path(OUTPUT_DIR, outxml_name)) %>%
  add_logit_tables_xml(L240.Supplysector_tra, "Supplysector") %>%
  add_xml_data(L240.SectorUseTrialMarket_tra, "SectorUseTrialMarket") %>%
  add_logit_tables_xml(L240.SubsectorAll_tra, "SubsectorAll", base_logit_header = "SubsectorLogit") %>%
  add_xml_data(L240.TechShrwt_tra, "TechShrwt") %>%
  add_xml_data(L240.TechCost_tra, "TechCost") %>%
  add_xml_data(L240.TechCoef_tra, "TechCoef") %>%
  add_xml_data(L240.Production_tra, "Production") %>%
  add_logit_tables_xml(L240.Supplysector_reg, "Supplysector") %>%
  add_logit_tables_xml(L240.SubsectorAll_reg, "SubsectorAll", base_logit_header = "SubsectorLogit") %>%
  add_xml_data(L240.TechShrwt_reg, "TechShrwt") %>%
  add_xml_data(L240.TechCoef_reg, "TechCoef") %>%
  add_xml_data(L240.Production_reg_imp, "Production") %>%
  add_xml_data(L240.Production_reg_dom, "Production")  ->
  ag_trade.xml

ag_trade.xml %>% gcamdata::run_xml_conversion()


# Step2 check data and visualize ----

L240.Production_tra %>% filter(year == 2015, supplysector == "traded oilpalm") %>% 
  filter(calOutputValue >0 ) %>% 
  mutate(subsector= gsub(" traded oilpalm", " ", subsector)) %>% 
  bind_rows(L240.Production_reg_dom %>%
              filter(supplysector == "regional oilpalm", year == 2015,
                     calOutputValue > 0) %>% 
              mutate(subsector = "Domestic", calOutputValue = - calOutputValue)) %>% 
  ggplot() +
  geom_hline(yintercept = 0) +
  geom_bar(aes(x = region, y = calOutputValue, fill = subsector), 
           color = "black", stat="identity") +
  labs(x = "Importer", y = "Palm fruit equivalent, Mt", fill = "Exporter",
       title = "GCAM OilPalm consumption by exporter and domestic (negative) ") + 
  theme_bw() +
  theme(
    text = element_text(size = 14, color = "black"),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) -> p

ggsave(file.path(OUTPUT_DIR, "BilateralTrade_GCAMBaseYear.png"), width = 12, height = 10  )



# Step3 Update ag_trade.xml with changing share-weights ----
# We will only change parameters related to OilPalm at the international market
# We will regenerate the xml to avoid interpolation overwriting issues

IntegrationScenario = "High"
IntegrationScenario = "Low"

if (IntegrationScenario == "Low") {
  integrate.sw.rate = 1.08; outxml_name = "ag_trade_LowIntegration.xml" # default
} else 
  if  (IntegrationScenario == "High") {
    integrate.sw.rate = 1.22; outxml_name = "ag_trade_HighIntegration.xml" # high 
  }




# Load logit exponents for calculating share-weights
read.csv(file.path(INPUT_DIR, "A_agRegionalSector.csv"), comment.char = "#") -> A_agRegionalSector
read.csv(file.path(INPUT_DIR, "A_agTradedSector.csv"), comment.char = "#") -> A_agTradedSector

# Load producer price which is also used as export price
readRDS(file.path(INPUT_DIR, "/L2012.AgSupplySector.rds")) %>% 
  filter(AgSupplySector == "OilPalm") %>% 
  transmute(subsector = paste(region, "traded", tolower(AgSupplySector), sep = " "), 
            imp_P = calPrice) -> 
  ImportPrice

L240.Production_tra %>% filter(year == 2015, supplysector == "traded oilpalm") %>% 
  rename(imp_Q = calOutputValue) %>% 
  left_join(ImportPrice, by = "subsector") %>% 
  left_join(A_agTradedSector %>% 
              select(supplysector, logit.exponent), by = "supplysector") %>% 
  mutate(sw = ifelse(is.finite(imp_P * imp_Q^(-1/logit.exponent)), 
                     imp_P * imp_Q^(-1/logit.exponent), 0)) %>% 
  arrange(region, supplysector, desc(imp_Q)) %>% 
  group_by(region, supplysector) %>% 
  mutate(
    sw = sw / first(sw),
    #sw = sw / sw[subsector == "Indonesia traded oilpalm"],
    sw_GCAM = sw^(-logit.exponent)) %>% 
  ungroup()-> data.sw0
  

# share-weight converges 
data.sw0 %>% 
  select(region, supplysector, subsector, sw, logit.exponent) %>% 
  as_tibble() %>% 
  repeat_add_columns(tibble(year = c(2015, MODEL_FUTURE_YEARS))) %>% 
  group_by(region, supplysector, subsector) %>% 
  mutate(sw = first(sw)^(1/integrate.sw.rate^((year - first(year))/5))) %>% 
  replace(is.na(.), 0) %>% 
  mutate(sw_GCAM = sw^(-logit.exponent)) %>% 
  ungroup()-> data.sw

library(ggplot2)
## convergence plot ----
data.sw %>% filter(sw_GCAM >0, sw_GCAM <5) %>% 
  ggplot() + facet_wrap(~region) +
  geom_line(aes(x = year, y = sw_GCAM, color = subsector), size = 1, alpha = 0.8) +
  labs(x = "Year", y = "Preference (share-weight), largest importer = 1",
      color = "Exporter",
      title = paste0("Convergence of trade preference;", IntegrationScenario) ) +
  theme_bw() +
  theme(text = element_text(size = 14))-> p
ggsave(file.path(OUTPUT_DIR, paste0("Integration_", IntegrationScenario, ".png")), width = 15, height = 10  )



L240.TechShrwt_tra %>% 
  filter(!(year > 2015 & supplysector == "traded oilpalm")) %>% 
  bind_rows(
    L240.TechShrwt_tra %>% 
      filter(year > 2015, supplysector == "traded oilpalm") %>% 
      left_join_error_no_match(
        data.sw %>% filter(year > 2015) %>% 
          select(region, subsector, year, sw_GCAM), 
        by = c("region", "subsector", "year")
      ) %>% mutate(share.weight = sw_GCAM) %>% 
      select(-sw_GCAM)
  ) -> L240.TechShrwt_tra_Updated




gcamdata::create_xml(file.path(OUTPUT_DIR, outxml_name)) %>%
  add_logit_tables_xml(L240.Supplysector_tra, "Supplysector") %>%
  add_xml_data(L240.SectorUseTrialMarket_tra, "SectorUseTrialMarket") %>%
  add_logit_tables_xml(L240.SubsectorAll_tra, 
                       "SubsectorLogit",  #header changed here 
                       base_logit_header = "SubsectorLogit") %>%
  add_xml_data(L240.TechShrwt_tra_Updated, "TechShrwt") %>%
  add_xml_data(L240.TechCost_tra, "TechCost") %>%
  add_xml_data(L240.TechCoef_tra, "TechCoef") %>%
  add_xml_data(L240.Production_tra, "Production") %>%
  add_logit_tables_xml(L240.Supplysector_reg, "Supplysector") %>%
  add_logit_tables_xml(L240.SubsectorAll_reg, "SubsectorAll", base_logit_header = "SubsectorLogit") %>%
  add_xml_data(L240.TechShrwt_reg, "TechShrwt") %>%
  add_xml_data(L240.TechCoef_reg, "TechCoef") %>%
  add_xml_data(L240.Production_reg_imp, "Production") %>%
  add_xml_data(L240.Production_reg_dom, "Production")  ->
  ag_trade.xml

ag_trade.xml %>% gcamdata::run_xml_conversion()


## Done ----



# If adding trade cost, e.g., transportation and tariffs ----
# we will need to adjust import price calculation in module_aglu_L202.an_input

