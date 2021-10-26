#emissions by sector
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


run_dir <- "C:/Users/blah822/Documents/steelDatabase/8-10.2" # SET MANUALLY FOR EACH RUN

if(!dir.exists(paste0(run_dir,"/figures"))){dir.create(paste0(run_dir,"/figures"))}
fig_dir <- "C:/Users/blah822/Documents/steelDatabase/8-10.2/figures"
setwd("C:/Users/blah822/Documents/steelDatabase")
source("functions.R")
source("diag_util_functions.R")


# ===============================================================================
# Load GCAM results
# ===============================================================================
# Download batch queries from PIC into run directory

queries <- list.files(run_dir, pattern='queryoutall')

for (i in queries) {
  filename <- gsub('.csv','', i) %>% gsub('queryoutall_','', .)
  assign(filename, readr::read_csv(paste0(run_dir, "/", i), skip = 1))
}


# Tidy data and add global
CO2_emissions <- CO2_emissions %>% parse_output_scenario %>% add_global_sum()
CO2_emissions_sector_nobio <- CO2_emissions_sector_nobio_v2 %>% parse_output_scenario %>% add_global_sum()
LU_CO2_emissions <- LU_CO2_emissions %>% parse_output_scenario %>% add_global_sum()
#CO2_prices <- readr::read_csv(paste0(run_dir, "/queryoutall_CO2_prices.csv"), skip = 2) %>%
# parse_output_scenario #first scenario has no results, so need to read in this one separately to skip first row
nonCO2_emissions <- nonCO2_emissions %>% parse_output_scenario %>% add_global_sum()
nonCO2_em_sector <- nonCO2_em_sector %>% parse_output_scenario %>% add_global_sum()

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

# Aggregate to deep dive regions and ROW --------------------------------------------------------------------------------

region_mapping <- read.csv("steel_region_mapping.csv")

regions_aggregated <- unique(region_mapping$steel_region)

# aggregate to deep dive regions and ROW
CO2_emissions <- CO2_emissions %>% aggregate_regions(region_mapping, colname = "steel_region")
CO2_emissions_sector_nobio <- CO2_emissions_sector_nobio %>% aggregate_regions(region_mapping, colname = "steel_region")
LU_CO2_emissions <- LU_CO2_emissions %>% aggregate_regions(region_mapping, colname = "steel_region")
nonCO2_emissions <- nonCO2_emissions %>% aggregate_regions(region_mapping, colname = "steel_region")
nonCO2_em_sector <- nonCO2_em_sector %>% aggregate_regions(region_mapping, colname = "steel_region")
# -------------------------------------------------------------------------------------------------------------
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

plot_years <- c(seq(2015,2050))

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
             "scrap" = "#666666",
             "BLASTFUR with H2" = "yellow")

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
               "#333333", "#FFCC00", "#CC6600", "#006600", "#3333CC", "#CC0033", "#0099CC", "#999966")

scenario_colors <- c("Ref"="#E69F00",
                     "constraint_1p5_lin_bio_limit"="#7DDE92",
                     "constraint_1p5_cons_bio_limit"="#3083DC",
                     "constraint_1p5_noCCS"="#4E4187",
                     "constraint_1p5_lowCCS" ="maroon",
                     "constraint_1p5" = "dark green",
                     "constraint_1p5_delay" = "blue")

scenario_colors <- c("Ref"="#E69F00",
                     "constraint_1p5_lin_bio_limit"="dark green",
                     "constraint_1p5_cons_bio_limit"="blue",
                     "constraint_1p5_noCCS"="#4E4187",
                     "constraint_1p5_lowCCS" ="maroon",
                     "constraint_1p5" = "#7DDE92",
                     "constraint_1p5_delay" = "#3083DC")


# Plots ==================================================================================================================================

#combine: CO2_emissions_sector_nobio, LU_CO2_emissions, GHG sector species
