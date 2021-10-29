library(zoo)
library(tidyr)
library(dplyr)
library(gcamdata)

L2323.StubTechCoef_iron_steel <- read.csv("C:/Users/blah822/Downloads/branches/steel-decarbonization-2/input/gcamdata/outputs/L2323.StubTechCoef_iron_steel.csv", skip=1)
L2323.GlobalTechCoef_iron_steel <- read.csv("C:/Users/blah822/Downloads/branches/steel-decarbonization-2/input/gcamdata/outputs/L2323.GlobalTechCoef_iron_steel.csv", skip=1)


max_impvt = .4

# adjust stubTechCoef
base_df <- L2323.StubTechCoef_iron_steel %>%
  select(-coefficient) %>%
  filter(!minicam.energy.input == "scrap")

L2323.StubTechCoef_iron_steel_AdvEff <- L2323.StubTechCoef_iron_steel %>%
  filter(year %in% c(1975:2020, 2100),
         !minicam.energy.input == "scrap") %>%
  mutate(coefficient = if_else(year == 2100, coefficient * (1-max_impvt), coefficient)) %>%
  right_join(base_df, by= c("region", "supplysector", "subsector", "stub.technology", "minicam.energy.input", "market.name", "year")) %>%
  dplyr::group_by(region, supplysector, subsector,stub.technology,minicam.energy.input, market.name) %>%
  arrange(region,stub.technology,minicam.energy.input,year)%>%
  mutate(coefficient = zoo::na.approx(coefficient))

# adjust GlobalTechCoef
base_df <- L2323.GlobalTechCoef_iron_steel %>%
  select(-coefficient) %>%
  filter(!minicam.energy.input == "scrap")

L2323.GlobalTechCoef_iron_steel_AdvEff <- L2323.GlobalTechCoef_iron_steel %>%
  filter(year %in% c(1975:2020, 2100),
         !minicam.energy.input == "scrap") %>%
  mutate(coefficient = if_else(year == 2100, coefficient * (1-max_impvt), coefficient)) %>%
  right_join(base_df, by= c("sector.name", "subsector.name", "technology", "minicam.energy.input", "year")) %>%
  dplyr::group_by(subsector.name,technology,minicam.energy.input) %>%
  arrange(subsector.name,technology,minicam.energy.input,year)%>%
  mutate(coefficient = na.approx(coefficient))


# Create add-on file
filename <- "steel_coefs_advEff_40pct_2100"

gcamdata::create_xml(paste0("C:/Users/blah822/Downloads/branches/steel-decarbonization-2/input/steel/", filename, ".xml")) %>%
  add_xml_data(L2323.StubTechCoef_iron_steel_AdvEff, "StubTechCoef")%>%
  add_xml_data(L2323.GlobalTechCoef_iron_steel_AdvEff, "GlobalTechCoef") %>%
  run_xml_conversion()
