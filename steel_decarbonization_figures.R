library(plyr)
#library(tidyverse)
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(devtools)
library(cowplot)
library(RColorBrewer)
library(readxl)
library(rgcam)
library(jgcricolors)
library(ggsci)
library(zoo)

options(scipen=999)

fig_dir <- "./figures"
run_dir <- "./output/6-29_linear_subsector_shwts" # SET MANUALLY FOR EACH RUN

#if(!dir.exists(paste0(run_dir,"/figures"))){dir.create(paste0(run_dir,"/figures"))}

source("functions.R")
source("diag_util_functions.R")

# ===============================================================================
# Load GCAM results 
# ===============================================================================
# Download batch queries from PIC into run directory

queries <- list.files(run_dir, pattern='queryout')

for (i in queries) {
  filename <- gsub('.csv','', i) %>% gsub('queryoutall_','', .)
  assign(filename, readr::read_csv(paste0(run_dir, "/", i), skip = 1))
}


# Tidy data and add global

CO2_emissions <- CO2_emissions %>% parse_output_scenario %>% add_global_sum()
CO2_emissions_sector_nobio <- CO2_emissions_sector_nobio %>% parse_output_scenario %>% add_global_sum()
LU_CO2_emissions <- LU_CO2_emissions %>% parse_output_scenario %>% add_global_sum()
CO2_prices <- readr::read_csv(paste0(run_dir, "/queryoutall_CO2_prices.csv"), skip = 2) %>% 
  parse_output_scenario #first scenario has no results, so need to read in this one separately to skip first row
nonCO2_emissions <- nonCO2_emissions %>% parse_output_scenario %>% add_global_sum()
nonCO2_em_sector <- nonCO2_em_sector %>% parse_output_scenario %>% add_global_sum()
final_ene_sect_fuel <- final_ene_sect_fuel %>% parse_output_scenario %>% add_global_sum()
pri_ene_ccs <- pri_ene_ccs %>% parse_output_scenario %>% add_global_sum()
industry_energy <- industry_energy %>% parse_output_scenario %>% add_global_sum()
industry_energy_tech_fuel <- industry_energy_tech_fuel %>% parse_output_scenario %>% add_global_sum()
cost_industry_techs <- cost_industry_techs %>% parse_output_scenario 
ironsteel_production <- ironsteel_production %>% parse_output_scenario %>% add_global_sum()
ironsteel_production_tech <- ironsteel_production_tech %>% parse_output_scenario %>% add_global_sum() 
ironsteel_production_tech_vintage <- ironsteel_production_tech_vintage %>% 
  parse_vintages() %>% parse_output_scenario %>% add_global_sum()
ironsteel_input_tech <- ironsteel_input_tech %>% parse_output_scenario %>% add_global_sum()
reg_pri_ene_prices <- reg_pri_ene_prices %>% parse_output_scenario 
elec_prices_sector <- elec_prices_sector %>% parse_output_scenario
final_energy_prices <- final_energy_prices %>% parse_output_scenario
demands_all_markets <- demands_all_markets %>% parse_output_scenario
global_mean_temp <- global_mean_temp %>% parse_output_scenario
forcing <- forcing %>% parse_output_scenario
pop <- pop %>% parse_output_scenario %>% add_global_sum()
gdp <- gdp %>% parse_output_scenario %>% add_global_sum()
  

# List scenarios in order we want them in figures
scenarios <- c("Ref", "constraint_1p5", "constraint_1p5_delay", "constraint_1p5_delay_v2")


# Constants --------------------------------------------------------------------
C_to_CO2 <- 44/12
EJ_to_TWh <- 277.778
USD1990_to_2015 <- 1.64753738
USD1975_to_2015 <- 3.508771929
THOUS_to_MILL <- 0.001

# Unit conversions -------------------------------------------------------------

# Emissions - C to CO2
CO2_emissions <- CO2_emissions %>% dplyr::mutate(value = if_else(Units == "MTC", value * C_to_CO2, value),
                                         Units = if_else(Units == "MTC", "MTCO2", Units))
LU_CO2_emissions <- LU_CO2_emissions %>% dplyr::mutate(value = if_else(Units == "MtC/yr", value * C_to_CO2, value),
                                         Units = if_else(Units == "MtC/yr", "MtCO2/yr", Units))
CO2_emissions_sector_nobio <- CO2_emissions_sector_nobio %>% dplyr::mutate(value = if_else(Units == "MTC", value * C_to_CO2, value),
                                                                           Units = if_else(Units == "MTC", "MTCO2", Units))

# Population - thousands to millions
pop <- pop %>%  dplyr::mutate(value = value * THOUS_to_MILL, 
                Units = "millions")

# Aggregate to deep dive regions and ROW --------------------------------------------------------------------------------

region_mapping <- read.csv("steel_region_mapping.csv")

regions_aggregated <- unique(region_mapping$steel_region)

# aggregate to deep dive regions and ROW
CO2_emissions <- CO2_emissions %>% aggregate_regions(region_mapping, colname = "steel_region")
CO2_emissions_sector_nobio <- CO2_emissions_sector_nobio %>% aggregate_regions(region_mapping, colname = "steel_region")
LU_CO2_emissions <- LU_CO2_emissions %>% aggregate_regions(region_mapping, colname = "steel_region")
nonCO2_emissions <- nonCO2_emissions %>% aggregate_regions(region_mapping, colname = "steel_region")
nonCO2_em_sector <- nonCO2_em_sector %>% aggregate_regions(region_mapping, colname = "steel_region")
final_ene_sect_fuel <- final_ene_sect_fuel %>% aggregate_regions(region_mapping, colname = "steel_region")
pri_ene_ccs <- pri_ene_ccs %>% aggregate_regions(region_mapping, colname = "steel_region")
industry_energy <- industry_energy %>% aggregate_regions(region_mapping, colname = "steel_region")
industry_energy_tech_fuel <- industry_energy_tech_fuel %>% aggregate_regions(region_mapping, colname = "steel_region")
ironsteel_production <- ironsteel_production %>% aggregate_regions(region_mapping, colname = "steel_region")
ironsteel_production_tech <- ironsteel_production_tech %>% aggregate_regions(region_mapping, colname = "steel_region")
ironsteel_production_tech_vintage <- ironsteel_production_tech_vintage %>% aggregate_regions(region_mapping, colname = "steel_region")
ironsteel_input_tech <- ironsteel_input_tech %>% aggregate_regions(region_mapping, colname = "steel_region")
pop <- pop %>% aggregate_regions(region_mapping, colname = "steel_region")
gdp <- gdp %>% aggregate_regions(region_mapping, colname = "steel_region")


# -------------------------------------------------------------------------------------------------------------

# per capita steel demand
steel_demand_pc <- demands_all_markets %>%
  filter(grepl("iron and steel", market)) %>%
  separate(market, into=c("region", "market"), sep="iron and steel") %>%
  dplyr::mutate(market = paste0("iron and steel", market)) %>%
  aggregate_regions(region_mapping, colname = "steel_region") %>%
  left_join(pop %>% dplyr::rename(pop=value), by = c("scenario", "region", "year")) %>%
  dplyr::mutate(value_pc = value / pop) 


# Net co2
net_co2 <- CO2_emissions %>%
  bind_rows(LU_CO2_emissions %>% filter(year %in% unique(CO2_emissions$year))) %>%
  group_by(scenario, region, year) %>%
  dplyr::summarise(value = sum(value)) %>% 
  ungroup

# Cumulative emissions by year - 2020 to end of century
cumulative_co2_to2100 <- net_co2 %>%
  group_by(scenario, region) %>%
  complete(year = seq(2010, 2100)) %>%
  mutate(value = na.approx(value),
         value = value / 1000) %>%
  ungroup %>%
  filter(year >= 2020) %>%
  group_by(scenario, region) %>%
  mutate(value = cumsum(value)) %>%
  ungroup() 

# Cumulative emissions by year - peak budget (2020 to net zero point)
cumulative_co2_peak_budget <- net_co2 %>%
  group_by(scenario, region) %>%
  complete(year = seq(2010, 2100)) %>%
  mutate(value = if_else(value < 0, 0, value),
         value = na.approx(value),
         value = value / 1000) %>%
  ungroup %>%
  filter(year >= 2020) %>%
  group_by(scenario, region) %>%
  mutate(value = cumsum(value)) %>%
  ungroup() 

# convert GHGs to CO2 equivalent
ghg_emiss <- nonCO2_emissions %>%
  conv_ghg_co2e() %>%
  select(-sector) %>%
  bind_rows(LU_CO2_emissions %>% mutate(variable = "CO2_LUC", Units = "CO2e") %>% select(-LandLeaf)) %>%
  group_by(scenario, region, year, variable, Units) %>% 
  dplyr::summarise(value = sum(value))

# Total GHG in CO2e
ghg_emiss_co2e <- ghg_emiss %>%
  group_by(scenario, region, year, Units) %>%
  dplyr::summarise(value = sum(value))

# group by GHG (original units)
nonCO2_emissions <- nonCO2_emissions %>%
  separate(GHG, c("GHG", "GHG_sector"), sep="_") %>%
  select(-GHG_sector) %>%
  group_by(scenario, region, year, GHG, Units) %>%
  dplyr::summarise(value = sum(value))

# ghgs (co2e) by sector and species
ghg_sector_species <- nonCO2_em_sector %>%
  dplyr::rename(sector_keep = sector) %>%
  conv_ghg_co2e() %>%
  group_by(scenario, region, year, variable, sector_keep, Units) %>%
  dplyr::summarise(value = sum(value)) %>%
  dplyr::rename(sector=sector_keep)

# ghgs by sector only (total co2e)
ghg_sector <- ghg_sector_species %>%
  group_by(scenario, region, year, sector, Units) %>%
  dplyr::summarise(value=sum(value))

# non-co2s (original units) by sector
nonCO2_em_sector <- nonCO2_em_sector %>%
  filter(!GHG == "CO2") %>%
  separate(GHG, c("GHG", "GHG_sector"), sep="_") %>%
  select(-GHG_sector) %>%
  group_by(scenario, region, year, sector, GHG, Units) %>%
  dplyr::summarise(value = sum(value))




# Plot themes -----------------------------------------------------------------------------------------------------------------

plot_theme <- theme_bw() +
  theme(legend.text = element_text(size = 12, vjust = 0.5)) +
  theme(legend.title = element_text(size = 12, vjust = 2)) +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 12, face = "bold")) +
  theme(plot.title = element_text(size = 12, face = "bold", vjust = 1)) +
  theme(plot.subtitle = element_text(size = 10, face = "bold", vjust = 1))+ 
  theme(strip.text = element_text(size = 8))+
  theme(strip.text.x = element_text(size = 9, face = "bold"))+
  #theme(legend.position = "bottom")+
  theme(legend.text = element_text(size = 12))+
  theme(legend.title = element_text(size = 12,color = "black",face="bold"))+
  theme(axis.text.x= element_text(angle=60,hjust=1))+
  theme(legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))

plot_years <- c(seq(2015,2100))

# color scheme from jgricolor package
pal_all <- jgcricol()$pal_all

# manually add missing colors
# add missing colors
pal_all <- c(pal_all,
             "iron and steel" = "red3",
             "aluminum" = "steelblue", 
             "alumina" = "steelblue1", 
             "chemical energy use" = "lightgoldenrod2", 
             "chemical feedstocks" = "darkgoldenrod2",
             "chemical" = "goldenrod2",
             "construction energy use" = "darkorange2", 
             "construction feedstocks" = "chocolate4", 
             "construction" = "chocolate3",
             "mining energy use" = "#E15A97",
             "mining" = "#E15A97",
             "agricultural energy use" = "#97DB4F",
             "cement"= "plum", 
             "process heat cement" = "plum4",
             "N fertilizer" = "gold3",
             "industrial energy use" = "slateblue2",
             "industrial feedstocks" = "springgreen4",
             "BLASTFUR CCS" = "#FB9A99",
             "BLASTFUR" = "#E31A1C",
             "EAF with scrap CCS" = "#bcbddc",
             "EAF with scrap" = "#756bb1",
             "EAF with DRI CCS" = "#9ecae1",
             "EAF with DRI" = "#3182bd",
             "Biomass-based" = "#33A02C",
             "Hydrogen-based DRI" = "#FCD581",
             "delivered biomass" = "#00931d",
             "delivered coal" = "gray20",
             "H2 enduse" = "peachpuff2",
             "refined liquids industrial" = "#d01c2a", 
             "scrap" = "#666666")

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
               "#333333", "#FFCC00", "#CC6600", "#006600", "#3333CC", "#CC0033", "#0099CC", "#999966")

scenario_colors <- c("Ref"="#E69F00",
                     "constraint_1p5"="#7DDE92",
                     "constraint_1p5_delay"="#3083DC",
                     "constraint_1p5_delay_v2"="#4E4187")


# Plots ==================================================================================================================================


# global climate forcing
forcing$scenario <- factor(forcing$scenario , levels = scenarios)

ggplot(data=filter(forcing, year %in% plot_years),
       aes(x=year, y=value, color = scenario)) +
  geom_line(size = 1.3)+
  labs(title = "Global climate forcing", x="", y="W/m^2")+
  scale_color_manual(values = scenario_colors) +
  scale_y_continuous(limits = c(0, NA)) +
  plot_theme +
  ggsave(paste0(fig_dir, "/global_forcing.png"), height = 6, width = 9, units = "in")

# global mean temperature
global_mean_temp$scenario <- factor(global_mean_temp$scenario , levels = scenarios)

ggplot(data=filter(global_mean_temp, year %in% plot_years),
       aes(x=year, y=value, color = scenario)) +
  geom_line(size = 1.2)+
  geom_hline(yintercept = 1.5, linetype = "dashed", color = "black", size=1) +
  geom_hline(yintercept = 2, linetype = "dashed", color = "black", size=1) +
  labs(title = "Global mean temperature", x="", y="Degrees C")+
  scale_color_manual(values = scenario_colors) +
  scale_y_continuous(limits = c(0, NA)) +
  plot_theme +
  ggsave(paste0(fig_dir, "/global_mean_temp.png"), height = 6, width = 9, units = "in")


# Total CO2 emissions
CO2_emissions$scenario <- factor(CO2_emissions$scenario, levels = scenarios)

ggplot(data=filter(CO2_emissions, year %in% plot_years),
       aes(x=year, y=value / 1000, color=scenario)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  facet_wrap(~region, scales = "free") +
  labs(title = "Total CO2 emissions (FFI)", x="", y="GTCO2") +
  scale_color_manual(values = scenario_colors) +
  plot_theme +
  ggsave(paste0(fig_dir, "/co2_emissions_allregions.png"), height = 6, width = 9, units = "in")

for (i in regions_aggregated) {
  ggplot(data=filter(CO2_emissions, region == i, year %in% plot_years),
         aes(x=year, y=value / 1000, color=scenario)) +
    geom_line(size = 1.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +
    labs(title = paste(i, "total CO2 emissions (FFI)"), x="", y="GTCO2") +
    scale_color_manual(values = scenario_colors) +
    plot_theme +
    ggsave(paste0(fig_dir, "/co2_emissions_", i, ".png"), height = 6, width = 9, units = "in")
}


# Net co2 emissions
net_co2$scenario <- factor(net_co2$scenario, levels = scenarios)

ggplot(data=filter(net_co2, year %in% plot_years),
       aes(x=year, y=value / 1000, color=scenario)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  facet_wrap(~region, scales = "free") +
  labs(title = "Net CO2 emissions", x="", y="GTCO2") +
  scale_color_manual(values = scenario_colors) +
  plot_theme +
  ggsave(paste0(fig_dir, "/net_co2_emissions_allregions.png"), height = 6, width = 9, units = "in")

for (i in regions_aggregated) {
  ggplot(data=filter(net_co2, region == i, year %in% plot_years),
         aes(x=year, y=value / 1000, color=scenario)) +
    geom_line(size = 1.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +
    labs(title = paste(i, "net CO2 emissions"), x="", y="GTCO2") +
    scale_color_manual(values = scenario_colors) +
    plot_theme +
    ggsave(paste0(fig_dir, "/net_co2_emissions_", i, ".png"), height = 6, width = 9, units = "in")
}

# Cumulative co2 emissions - peak budget
cumulative_co2_peak_budget$scenario <- factor(cumulative_co2_peak_budget$scenario, levels = scenarios)

ggplot(data=filter(cumulative_co2_peak_budget, year %in% plot_years),
       aes(x=year, y=value, color=scenario)) +
  geom_line(size = 1) +
  facet_wrap(~region, scales = "free") +
  labs(title = "Cumulative CO2 emissions (peak budget)", x="", y="GTCO2") +
  scale_color_manual(values = scenario_colors) +
  plot_theme +
  ggsave(paste0(fig_dir, "/cumulative_co2_peak_budget_allregions.png"), height = 6, width = 9, units = "in")

for (i in regions_aggregated) {
  ggplot(data=filter(cumulative_co2_peak_budget, region == i, year %in% plot_years),
         aes(x=year, y=value, color=scenario)) +
    geom_line(size = 1.2) +
    labs(title = paste(i, "cumulative CO2 emissions (peak budget)"), x="", y="GTCO2") +
    scale_color_manual(values = scenario_colors) +
    plot_theme +
    ggsave(paste0(fig_dir, "/cumulative_co2_peak_budget_", i, ".png"), height = 6, width = 9, units = "in")
}

# Cumulative co2 emissions - end-of-century
cumulative_co2_to2100$scenario <- factor(cumulative_co2_to2100$scenario, levels = scenarios)

ggplot(data=filter(cumulative_co2_to2100, year %in% plot_years),
       aes(x=year, y=value, color=scenario)) +
  geom_line(size = 1) +
  facet_wrap(~region, scales = "free") +
  labs(title = "Cumulative CO2 emissions (peak budget)", x="", y="GTCO2") +
  scale_color_manual(values = scenario_colors) +
  plot_theme +
  ggsave(paste0(fig_dir, "/cumulative_co2_end_of_century_allregions.png"), height = 6, width = 9, units = "in")

for (i in regions_aggregated) {
  ggplot(data=filter(cumulative_co2_to2100, region == i, year %in% plot_years),
         aes(x=year, y=value, color=scenario)) +
    geom_line(size = 1.2) +
    labs(title = paste(i, "cumulative CO2 emissions (peak budget)"), x="", y="GTCO2") +
    scale_color_manual(values = scenario_colors) +
    plot_theme +
    ggsave(paste0(fig_dir, "/cumulative_co2_end_of_century_", i, ".png"), height = 6, width = 9, units = "in")
}


# CO2 prices
CO2_prices$scenario <- factor(CO2_prices$scenario, levels = scenarios)

ggplot(data=filter(CO2_prices, year %in% plot_years, market=="globalCO2"),
       aes(x=year, y=value, color = scenario)) +
  geom_line(size = 1.2) +
  labs(title = "Global CO2 prices", x="", y="1990$/tC") +
  scale_color_manual(values = scenario_colors) +
  plot_theme +
  ggsave(paste0(fig_dir, "/co2_prices_global.png"), height = 6, width = 9, units = "in")

#without super high CO2 price scenario
ggplot(data=filter(CO2_prices, year %in% plot_years, market=="globalCO2", !scenario == "constraint_1p5_delay"),
       aes(x=year, y=value, color = scenario)) +
  geom_line(size = 1.2) +
  labs(title = "Global CO2 prices", x="", y="1990$/tC") +
  scale_color_manual(values = scenario_colors) +
  plot_theme +
  ggsave(paste0(fig_dir, "/co2_prices_global_nz2050_scenarios.png"), height = 6, width = 9, units = "in")


# Total  GHG emissions (total co2e)

ggplot(data=filter(ghg_emiss_co2e, year %in% plot_years),
       aes(x=year, y=value / 1000, color = scenario)) +
  geom_line(size = 1) +
  facet_wrap(~region, scales = "free") +
  labs(title = "Total GHG emissions", x="", y="GTCO2e") +
  scale_color_manual(values = scenario_colors) +
  plot_theme +
  ggsave(paste0(fig_dir, "/total_ghg_co2e_allregions.png"), height = 6, width = 9, units = "in")


for (i in regions_aggregated) {
  ggplot(filter(ghg_emiss_co2e, region == i, year %in% plot_years), aes(x=year, y=value / 1000, color = scenario)) +
    geom_line(size=1.2) +
    labs(title = paste(i, "total GHG emissions"), x="", y="GTCO2e") +
    scale_color_manual(values = scenario_colors) +
    plot_theme +
    ggsave(paste0(fig_dir, "/total_ghg_co2e_", i, ".png"), height = 6, width = 9, units = "in")
  
}



#Ironsteel production
ironsteel_production$scenario <- factor(ironsteel_production$scenario, levels = scenarios)


# deep dive regions + global
for (i in regions_aggregated) {
  ggplot(data=filter(ironsteel_production, region == i, year %in% plot_years),
         aes(x=year, y=value, color = scenario)) +
    geom_line(size = 1.2) +
    labs(title = paste(i, " iron and steel production"), x="", y="Mt") +
    scale_y_continuous(limits = c(0, NA)) +
    scale_color_manual(values = scenario_colors) +
    plot_theme +
    ggsave(paste0(fig_dir, "/ironsteel_production_", i, ".png"), height = 6, width = 9, units = "in")
}

# all regions on same plot
ggplot(data=filter(ironsteel_production, year %in% plot_years, !region == "Global"),
       aes(x=year, y=value, color = scenario)) +
  geom_line(size = 1) +
  facet_wrap(~region) +
  labs(title = "Iron and steel production by region", x="", y="Mt") +
  scale_y_continuous(limits = c(0, NA)) +
  scale_color_manual(values = scenario_colors) +
  plot_theme +
  theme(strip.text.x = element_text(size = 10)) +
  ggsave(paste0(fig_dir, "/ironsteel_production_regions.png"), height = 9, width = 11, units = "in")

#Ironsteel production by tech
ironsteel_production_tech$scenario <- factor(ironsteel_production_tech$scenario, levels = scenarios)

for (i in regions_aggregated) {
  ggplot(data=filter(ironsteel_production_tech, region == i, year %in% plot_years),
         aes(x=year, y=value, fill=technology)) +
    geom_col()+
    facet_wrap(~scenario) +
    labs(title = paste(i, "iron and steel production by technology"), x="", y="Mt") +
    scale_fill_manual(values = pal_all)+
    plot_theme +
    ggsave(paste0(fig_dir, "/ironsteel_production_tech_", i, ".png"), height = 6, width = 9, units = "in")
}

#Ironsteel production by tech and vintage
ironsteel_production_tech_vintage$scenario <- factor(ironsteel_production_tech_vintage$scenario, levels = scenarios)

for (i in regions_aggregated) {
  ggplot(data=filter(ironsteel_production_tech_vintage, region == i, year %in% plot_years, year == vintage),
         aes(x=year, y=value, fill=technology)) +
    geom_col()+
    facet_wrap(~scenario) +
    labs(title = paste(i, "iron and steel production - new vintages"), x="", y="Mt") +
    scale_fill_manual(values = pal_all)+
    plot_theme +
    ggsave(paste0(fig_dir, "/ironsteel_production_tech_newvintage", i, ".png"), height = 6, width = 9, units = "in")
}

# Iron/steel inputs by tech
ironsteel_input_tech$scenario <- factor(ironsteel_input_tech$scenario, levels = scenarios)

for (i in regions_aggregated) {
  ggplot(data=filter(ironsteel_input_tech, region == i, year %in% plot_years),
       aes(x=year, y=value, fill=input)) +
  geom_col()+
  facet_grid(Units~scenario, scale = "free") +
  labs(title = paste(i, "iron and steel inputs"), x="", y="EJ or Mt") +
  scale_fill_manual(values = pal_all) +
  plot_theme +
    ggsave(paste0(fig_dir, "/ironsteel_inputs_", i, ".png"), height = 6, width = 9, units = "in")
}

# Global industrial energy by fuel, iron and steel sector
industry_energy_tech_fuel$scenario <- factor(industry_energy_tech_fuel$scenario, levels = scenarios)

for (i in regions_aggregated) {
ggplot(data=filter(industry_energy_tech_fuel, region == i, year %in% plot_years,
                   sector == "iron and steel"),
       aes(x=year, y=value, fill=input)) +
  geom_col() +
  facet_wrap(~scenario) +
  labs(title = paste(i, "iron and steel energy use by fuel"), x="", y="EJ") +
  scale_fill_manual(values = pal_all) +
  plot_theme +
  ggsave(paste0(fig_dir, "/ironsteel_energy_fuel_", i, ".png"), height = 6, width = 9, units = "in")
}

# iron and steel energy by subsector and fuel

for (i in regions_aggregated) {
  ggplot(data=filter(industry_energy_tech_fuel, region == i, year %in% plot_years,
                     sector == "iron and steel"),
         aes(x=year, y=value, fill=input)) +
    geom_col() +
    facet_grid(subsector~scenario) +
    labs(title = paste(i, "iron and steel energy use by subsector and fuel"), x="", y="EJ") +
    scale_fill_manual(values = pal_all) +
    plot_theme +
    ggsave(paste0(fig_dir, "/ironsteel_energy_subsector_fuel_", i, ".png"), height = 6, width = 9, units = "in")
}


for (i in regions_aggregated) {
  ggplot(data=filter(industry_energy_tech_fuel, region == i, year %in% plot_years,
                     sector == "iron and steel"),
         aes(x=year, y=value, fill=input)) +
    geom_col() +
    facet_grid(technology~scenario) +
    labs(title = paste(i, "iron and steel energy use by tech and fuel"), x="", y="EJ") +
    scale_fill_manual(values = pal_all) +
    plot_theme +
    theme(strip.text.y = element_text(size = 10))+
    ggsave(paste0(fig_dir, "/ironsteel_energy_tech_fuel_", i, ".png"), height = 8.5, width = 11, units = "in")
}

# steel demand per capita
steel_demand_pc$scenario <- factor(steel_demand_pc$scenario, levels = scenarios)

for (i in regions_aggregated) {
  ggplot(data=filter(steel_demand_pc, region == i, year %in% plot_years, !grepl("-tfe", market)),
         aes(x=year, y=value, color=scenario)) +
    geom_line(size=1.2) +
    labs(title = paste(i, "iron and steel demand per capita"), x="", y="tons per capita") +
    scale_color_manual(values = scenario_colors) +
    scale_y_continuous(limits = c(0, NA)) +
    plot_theme +
    theme(strip.text.y = element_text(size = 10))+
    ggsave(paste0(fig_dir, "/ironsteel_demand_percapita_", i, ".png"), height = 6, width = 9, units = "in")
}

# iron and steel CO2 emissions
CO2_emissions_sector_nobio$scenario <- factor(CO2_emissions_sector_nobio$scenario, levels = scenarios)

for (i in regions_aggregated) {
  ggplot(data=filter(CO2_emissions_sector_nobio, sector == "iron and steel", region == i, year %in% plot_years),
         aes(x=year, y=value/1000, color=scenario)) +
    geom_line(size = 1.2) +
    labs(title = paste(i, "iron and steel CO2 emissions"), x="", y="GTCO2") +
    scale_color_manual(values = scenario_colors) +
    plot_theme +
    ggsave(paste0(fig_dir, "/ironsteel_co2_emissions_", i, ".png"), height = 6, width = 9, units = "in")
}

