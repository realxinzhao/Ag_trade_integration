library(dplyr)
library(tidyr)
library(gcamdata)
source("R/funcs.R")


#Two scenario parameters
integrate.sw.rate = 1.08; outxml_name = "ag_trade_sw_default.xml" # default
integrate.sw.rate = 1.22; outxml_name = "ag_trade_sw_high.xml" # high 


INPUT_DIR <- "input/Zhao2021EAP/"
OUTPUT_DIR <- "output/Zhao2021EAP/"
AG_TRADE_DIR <- "input/Zhao2021EAP/agtrade_gcamoutput/"

MODEL_BASE_YEARS        <- c(1975, 1990, 2005, 2010, 2015)
MODEL_FUTURE_YEARS      <- seq(2020, 2100, 5)
convergence.period <- length(MODEL_FUTURE_YEARS)

#Read output csvs files from the orginal default data system
list.files(path = AG_TRADE_DIR, pattern="*.csv$")
read_file(path = AG_TRADE_DIR, pattern = "*.csv$", assigngsub = ".csv", nskip = 0)

list.files(path = INPUT_DIR, pattern="*ref.csv$")
read_file(path = INPUT_DIR, pattern = "*ref.csv$", 
          assigngsub = "_ref.csv", nskip = 1, proc = TRUE)

read.csv(file.path(INPUT_DIR, "A_agRegionalSector.csv"), comment.char = "#") -> A_agRegionalSector
read.csv(file.path(INPUT_DIR, "A_agTradedSector.csv"), comment.char = "#") -> A_agTradedSector

A_agRegionalSector %>% 
  transmute(sector = gsub("regional ", "", supplysector))  %>% 
  unlist() %>% as.character() %>% unique() -> Livecrop

# Pakistan only import pork. There is no domestically produced pork.
Price_trade_reg %>% filter(region == "Pakistan", grepl("pork", sector))

Price_trade_reg%>% 
  dplyr::select(-Units) %>%  mutate(variable = "imp.P") %>% 
  bind_rows(Trade_source_reg %>% 
              dplyr::select(-input, -Units) %>% 
              mutate(variable = "imp.Q")) %>% 
  mutate(subsector = if_else(grepl("imported", subsector), "imported", "domestic"),
         sector = gsub("regional ", "", sector)) %>% 
  filter(sector %in% Livecrop,
         year == 2015) %>% 
  spread(variable, value) %>% 
  replace(is.na(.), 0) %>% 
  left_join(A_agRegionalSector %>% 
              transmute(sector = gsub("regional ", "", supplysector), 
                        logit.exponent = logit.exponent)) %>% 
  mutate(sw = ifelse(is.finite(imp.P * imp.Q^(-1/logit.exponent)), imp.P * imp.Q^(-1/logit.exponent), 0)) %>% 
  arrange(region, sector, desc(imp.Q)) %>% 
  group_by(region, sector) %>% 
  mutate(sw = sw / first(sw),
         sw_GCAM = sw^(-logit.exponent)) %>% 
  ungroup()-> data.sw0

#share-weight converges only for crops
data.sw0 %>% within(rm(year)) %>% 
  as_tibble() %>% 
  repeat_add_columns(tibble(year = c(2015, MODEL_FUTURE_YEARS))) %>% 
  group_by(scenario, region, sector, subsector) %>% 
  mutate(sw = if_else(sector %in% c("beef", "dairy", "pork", "poultry" , "sheepgoat"), sw, 
                      first(sw)^(1/integrate.sw.rate^((year - first(year))/5)))) %>% 
  replace(is.na(.), 0) %>% 
  mutate(sw_GCAM = sw^(-logit.exponent)) %>% 
  ungroup()-> data.sw


###################
#update files needed for ag_trade.xml

L243.SubsectorShrwtFllt_reg_addedupdate <- data.sw %>% 
  filter(year != 2015) %>% 
  transmute(region, 
            supplysector = paste("regional", sector), 
            subsector = paste(subsector, sector),
            year.fillout = year,
            share.weight = sw_GCAM) 

gcamdata::create_xml(file.path(OUTPUT_DIR, outxml_name)) %>%
  add_logit_tables_xml(L240.Supplysector_tra, "Supplysector") %>%
  add_xml_data(L240.SectorUseTrialMarket_tra, "SectorUseTrialMarket") %>%
  add_logit_tables_xml(L240.SubsectorAll_tra, "SubsectorAll", 
                       base_logit_header = "SubsectorLogit") %>%
  add_xml_data(L240.TechShrwt_tra, "TechShrwt") %>%
  add_xml_data(L240.TechCost_tra, "TechCost") %>%
  add_xml_data(L240.TechCoef_tra, "TechCoef") %>%
  add_xml_data(L240.Production_tra, "Production") %>%
  add_logit_tables_xml(L240.Supplysector_reg, "Supplysector") %>%
  add_logit_tables_xml(L240.SubsectorAll_reg_revised, "SubsectorLogit",  #header changed here
                       base_logit_header = "SubsectorLogit") %>%
  add_xml_data(L240.TechShrwt_reg, "TechShrwt") %>%
  add_xml_data(L240.TechCoef_reg, "TechCoef") %>%
  add_xml_data(L240.Production_reg_imp, "Production") %>%
  add_xml_data(L240.Production_reg_dom, "Production") %>%
  add_xml_data(L243.SubsectorShrwtFllt_reg_addedupdate, "SubsectorShrwtFllt") %>% #added share weight
  gcamdata::run_xml_conversion()

