library(plyr)
library(tidyverse)
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
library(gcammaptools)
library(ggsci)
library(zoo)
library(extrafont)
loadfonts(quiet = T)
options(scipen=999)

#set directories SET MANUALLY FOR EACH RUN -------
run_dir <- "C:/Users/blah822/Documents/steelDatabase/10-23" 
if(!dir.exists(paste0(run_dir,"/figures"))){dir.create(paste0(run_dir,"/"))}
fig_dir <- "C:/Users/blah822/Documents/steelDatabase/test"
results_dir <- "C:/Users/blah822/Documents/steelDatabase/test"
setwd("C:/Users/blah822/OneDrive - PNNL/Documents/GitHub/steel-decarbonization")
source("functions.R")
source("diag_util_functions.R")

#mapping files ----------------
region_mapping <- read.csv("steel_region_mapping.csv")
regions_aggregated <- unique(region_mapping$steel_region)
CO2_sector_mapping <- read.csv("CO2_nonbio_sector_mapping.csv")
hydrogen_mapping <- read.csv("hydrogen_production_mapping.csv")
elec_mapping <-read.csv("elec_mapping.csv")


#Download batch queries from PIC into run directory -----------------------------------
queries <- list.files(run_dir, pattern='queryoutall')

for (i in queries) {
  filename <- gsub('.csv','', i) %>% gsub('queryoutall_','', .)
  assign(filename, readr::read_csv(paste0(run_dir, "/", i), skip = 1))
}


# Tidy data and add global
#final_ene_sect_fuel <- final_ene_sect_fuel %>% parse_output_scenario %>% add_global_sum()
prices_sector <- prices_sector %>% parse_output_scenario %>% add_global_sum()
electricity <- electricity %>% parse_output_scenario %>% add_global_sum()
CO2_emissions <- CO2_emissions %>% parse_output_scenario %>% add_global_sum()
CO2_emissions_sector_nobio <- CO2_emissions_sector_nobio_v2 %>% parse_output_scenario %>% add_global_sum()
LU_CO2_emissions <- LU_CO2_emissions %>% parse_output_scenario %>% add_global_sum()
#CO2_prices <- readr::read_csv(paste0(run_dir, "/queryoutall_CO2_prices.csv"), skip = 2) %>%
# parse_output_scenario #first scenario has no results, so need to read in this one separately to skip first row
#nonCO2_emissions <- nonCO2_emissions %>% parse_output_scenario %>% add_global_sum()
#nonCO2_em_sector <- nonCO2_em_sector %>% parse_output_scenario %>% add_global_sum()
final_ene_sect_fuel <- final_ene_sect_fuel %>% parse_output_scenario %>% add_global_sum()
#pri_ene_ccs <- pri_ene_ccs %>% parse_output_scenario %>% add_global_sum()
#industry_energy <- industry_energy %>% parse_output_scenario %>% add_global_sum()
industry_energy_tech_fuel <- industry_energy_tech_fuel %>% parse_output_scenario %>% add_global_sum()
#cost_industry_techs <- cost_industry_techs %>% parse_output_scenario
ironsteel_production <- ironsteel_production %>% parse_output_scenario %>% add_global_sum()
ironsteel_production_tech <- ironsteel_production_tech %>% parse_output_scenario %>% add_global_sum()
ironsteel_production_tech_vintage <- ironsteel_production_tech_vintage %>%
  parse_vintages() %>% parse_output_scenario %>% add_global_sum()
ironsteel_input_tech <- ironsteel_input_tech %>% parse_output_scenario %>% add_global_sum()
#reg_pri_ene_prices <- reg_pri_ene_prices %>% parse_output_scenario
#elec_prices_sector <- elec_prices_sector %>% parse_output_scenario
#final_energy_prices <- final_energy_prices %>% parse_output_scenario
#demands_all_markets <- demands_all_markets %>% parse_output_scenario
#global_mean_temp <- global_mean_temp %>% parse_output_scenario
#forcing <- forcing %>% parse_output_scenario
#pop <- pop %>% parse_output_scenario %>% add_global_sum()
#gdp <- gdp %>% parse_output_scenario %>% add_global_sum()

# Constants --------------------------------------------------------------------
C_to_CO2 <- 44/12
EJ_to_TWh <- 277.778
USD1990_to_2015 <- 1.64753738
USD1975_to_2015 <- 3.508771929
THOUS_to_MILL <- 0.001

# Emissions - C to CO2-------------
CO2_emissions <- CO2_emissions %>% dplyr::mutate(value = if_else(Units == "MTC", value * C_to_CO2, value),
                                                 Units = if_else(Units == "MTC", "MTCO2", Units))
LU_CO2_emissions <- LU_CO2_emissions %>% dplyr::mutate(value = if_else(Units == "MtC/yr", value * C_to_CO2, value),
                                                       Units = if_else(Units == "MtC/yr", "MtCO2/yr", Units))
CO2_emissions_sector_nobio <- CO2_emissions_sector_nobio %>% dplyr::mutate(value = if_else(Units == "MTC", value * C_to_CO2, value),
                                                                           Units = if_else(Units == "MTC", "MTCO2", Units))

# Population - thousands to millions
#pop <- pop %>%  dplyr::mutate(value = value * THOUS_to_MILL,
#Units = "millions")

# Aggregate to deep dive regions and ROW --------------------------------------------------------------------------------
#final_ene_sect_fuel <- final_ene_sect_fuel %>% aggregate_regions(region_mapping, colname = "steel_region")
electricity <- electricity %>% aggregate_regions(region_mapping, colname = "steel_region")
CO2_emissions <- CO2_emissions %>% aggregate_regions(region_mapping, colname = "steel_region")
CO2_emissions_sector_nobio <- CO2_emissions_sector_nobio %>% aggregate_regions(region_mapping, colname = "steel_region")
LU_CO2_emissions <- LU_CO2_emissions %>% aggregate_regions(region_mapping, colname = "steel_region")
prices_sector <- prices_sector %>% aggregate_regions(region_mapping, colname = "steel_region")
#nonCO2_emissions <- nonCO2_emissions %>% aggregate_regions(region_mapping, colname = "steel_region")
#nonCO2_em_sector <- nonCO2_em_sector %>% aggregate_regions(region_mapping, colname = "steel_region")
final_ene_sect_fuel <- final_ene_sect_fuel %>% aggregate_regions(region_mapping, colname = "steel_region")
#pri_ene_ccs <- pri_ene_ccs %>% aggregate_regions(region_mapping, colname = "steel_region")
#industry_energy <- industry_energy %>% aggregate_regions(region_mapping, colname = "steel_region")
industry_energy_tech_fuel <- industry_energy_tech_fuel %>% aggregate_regions(region_mapping, colname = "steel_region")
ironsteel_production <- ironsteel_production %>% aggregate_regions(region_mapping, colname = "steel_region")
ironsteel_production_tech <- ironsteel_production_tech %>% aggregate_regions(region_mapping, colname = "steel_region")
ironsteel_production_tech_vintage <- ironsteel_production_tech_vintage %>% aggregate_regions(region_mapping, colname = "steel_region")
ironsteel_input_tech <- ironsteel_input_tech %>% aggregate_regions(region_mapping, colname = "steel_region")
#pop <- pop %>% aggregate_regions(region_mapping, colname = "steel_region")
#gdp <- gdp %>% aggregate_regions(region_mapping, colname = "steel_region")
hydrogen_production_tech <- hydrogen_production_tech %>% parse_output_scenario %>% add_global_sum()%>% aggregate_regions(region_mapping, colname = "steel_region")

#convert emissions -------------------------------------------------------------------------------------------------------------
# Net co2
net_co2 <- CO2_emissions %>%
  bind_rows(LU_CO2_emissions %>% filter(year %in% unique(CO2_emissions$year))) %>%
  group_by(scenario, region, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  ungroup

# Cumulative emissions by year - 2020 to end of century
# cumulative_co2_to2100 <- net_co2 %>%
#   group_by(scenario, region) %>%
#   complete(year = seq(2010, 2100)) %>%
#   mutate(value = na.approx(value),
#          value = value / 1000) %>%
#   ungroup %>%
#   filter(year >= 2020) %>%
#   group_by(scenario, region) %>%
#   mutate(value = cumsum(value)) %>%
#   ungroup()

# Cumulative emissions by year - peak budget (2020 to net zero point)
# cumulative_co2_peak_budget <- net_co2 %>%
#   group_by(scenario, region) %>%
#   complete(year = seq(2010, 2100)) %>%
#   mutate(value = if_else(value < 0, 0, value),
#          value = na.approx(value),
#          value = value / 1000) %>%
#   ungroup %>%
#   filter(year >= 2020) %>%
#   group_by(scenario, region) %>%
#   mutate(value = cumsum(value)) %>%
#   ungroup()

# convert GHGs to CO2 equivalent
# ghg_emiss <- nonCO2_emissions %>%
#   conv_ghg_co2e() %>%
#   select(-sector) %>%
#   bind_rows(LU_CO2_emissions %>% mutate(variable = "CO2_LUC", Units = "CO2e") %>% select(-LandLeaf)) %>%
#   group_by(scenario, region, year, variable, Units) %>%
#   dplyr::summarise(value = sum(value))

# Total GHG in CO2e
# ghg_emiss_co2e <- ghg_emiss %>%
#   group_by(scenario, region, year, Units) %>%
#   dplyr::summarise(value = sum(value))

# group by GHG (original units)
# nonCO2_emissions <- nonCO2_emissions %>%
#   separate(GHG, c("GHG", "GHG_sector"), sep="_") %>%
#   select(-GHG_sector) %>%
#   group_by(scenario, region, year, GHG, Units) %>%
#   dplyr::summarise(value = sum(value))
# 
# # ghgs (co2e) by sector and species
# ghg_sector_species <- nonCO2_em_sector %>%
#   dplyr::rename(sector_keep = sector) %>%
#   conv_ghg_co2e() %>%
#   group_by(scenario, region, year, variable, sector_keep, Units) %>%
#   dplyr::summarise(value = sum(value)) %>%
#   dplyr::rename(sector=sector_keep)
# 
# # ghgs by sector only (total co2e)
# ghg_sector <- ghg_sector_species %>%
#   group_by(scenario, region, year, sector, Units) %>%
#   dplyr::summarise(value=sum(value))
# 
# # non-co2s (original units) by sector
# nonCO2_em_sector <- nonCO2_em_sector %>%
#   filter(!GHG == "CO2") %>%
#   separate(GHG, c("GHG", "GHG_sector"), sep="_") %>%
#   select(-GHG_sector) %>%
#   group_by(scenario, region, year, sector, GHG, Units) %>%
#   dplyr::summarise(value = sum(value))


# assign CO2 from other industry and cement to aggregate industry sector
# TODO: fix this in query instead
#CO2_emissions_assigned_sector_nobio <- CO2_emissions_assigned_sector_nobio %>%
#  dplyr::mutate(sector = if_else(sector %in% c("other industry", "cement"), "industry", sector)) %>%
#  group_by(scenario, region, year, sector, Units) %>%
#  dplyr::summarise(value = sum(value)) %>%
#  ungroup

#Plot themes -----------------------------------------------------------------------------------------------------------------
plot_theme <- theme_bw() +
  theme(legend.text = element_text(size = 15, vjust = 0.5)) +
  theme(legend.title = element_text(size = 15, vjust = 2)) +
  theme(axis.text = element_text(size = 15)) +
  theme(axis.title = element_text(size = 15, face = "bold")) +
  theme(plot.title = element_text(size = 15, face = "bold", vjust = 1)) +
  theme(plot.subtitle = element_text(size = 15, face = "bold", vjust = 1))+
  theme(strip.text = element_text(size = 15))+
  theme(strip.text.x = element_text(size = 15, face = "bold"))+
  #theme(legend.position = "bottom")+
  theme(legend.text = element_text(size = 15))+
  theme(legend.title = element_text(size = 15,color = "black",face="bold"))+
  theme(axis.text.x= element_text(angle=60,hjust=1))+
  theme(legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())+
  theme(text=element_text(family="Calibri", size=16))

pie_theme <- theme_bw() +
  theme(legend.text = element_text(size = 14, vjust = 0.5)) +
  theme(legend.title = element_text(size = 14, vjust = 2)) +
  theme(axis.text = element_text(size = 14)) +
  theme(axis.title = element_text(size = 14, face = "bold")) +
  theme(plot.title = element_text(size = 14, face = "bold", vjust = 1)) +
  theme(plot.subtitle = element_text(size = 14, face = "bold", vjust = 1))+
  theme(strip.text = element_text(size = 14))+
  #theme(strip.text.x = element_text(size = 9, face = "bold"))+
  #theme(legend.position = "bottom")+
  theme(legend.text = element_text(size = 14))+
  theme(legend.title = element_text(size = 14,color = "black",face="bold"))+
  #theme(axis.text.x= element_text(angle=60,hjust=1))+
  theme(legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))+
  theme(text=element_text(family="Calibri", size=16))

blank_theme <- pie_theme+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_rect(colour="black",size=1),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    plot.title=element_text(size=14, face="bold"),
  )
plot_years <- c(seq(2015,2050))

#Colors ---------------------------------------------------------------------------
# add missing colors
input_colors  <- c(pal_all,"delivered biomass" = "#00931d",
                   "delivered coal" = "gray20",
                   "H2 enduse" = "darkgoldenrod3",
                   "hydrogen" = "peachpuff2",
                   "refined liquids industrial" = "#d01c2a",
                   "gas" = "#d01c2a",
                   "gas with CCS" ="peachpuff2",
                   "refined liquids" = "#756bb1",
                   "refined liquids with CCS" = "#bcbddc",
                   "scrap" = "#666666",
                   "coal" = "grey20",
                   "coal with CCS" = "grey85")

scenario_colors <- c("Ref"= "#E31A1C",
                     "1p5 delay" = "#3182bd",
                     "1p5" = "#33A02C")


tech_colors  <- c("BF-CCUS" = "#FB9A99",
                  "BF-BOF" = "#E31A1C",
                  "EAF-scrap" = "#756bb1",
                  "DRI-EAF-CCUS" = "#9ecae1",
                  "DRI-EAF-Fossil" = "#3182bd",
                  "BF-biomass" = "#33A02C",
                  "DRI-EAF-H2" = "#FCD581",
                  "BF-H2" = "darkgoldenrod3")

tech_colors  <- c("BF-biomass" = "#33A02C",
                  "BF-BOF" = "#E31A1C",
                  "BF-CCUS" = "#FB9A99",
                  "BF-H2" = "darkgoldenrod3",
                  "DRI-EAF-Fossil" = "#3182bd",
                  "DRI-EAF-CCUS" = "#9ecae1",
                  "EAF-scrap" = "#756bb1",
                  "DRI-EAF-H2" = "#FCD581")
                  
mat_eff_colors <- c("Direct reuse" = "#33A02C" , 
                    'Optimized building design' ="#E31A1C", 
                    'High-strength steel'="#FB9A99", 
                    'Lifetime extension'="darkgoldenrod3", 
                    'Post-use recycling'="#3182bd", 
                    'Lightweighting'="#9ecae1",
                    'Improved semi-manufacturing yields'="#756bb1",
                    'Improved product manufacturing yields'="#FCD581")

regions <- c("China" = "#E31A1C","Europe"="#FB9A99","India"="#756bb1",
             "Japan"="#9ecae1","ROW"="#3182bd","South Korea"="#33A02C",
             "US"="#FCD581")

fuel_colors <- c("biomass" = "#33A02C", "coal" = "gray27",
                 "coal with CCS" = "gray57", "electricity" = "#756bb1","gas" = "#3182bd", 
                 "gas with CCS" = "#9ecae1","hydrogen" = "#FCD581",
                 "refined liquids" = "#E31A1C", "refined liquids with CCS" = "#FB9A99")                

hydrogen_pal <- c("blue"="#3182bd", "grey"="gray57", "green"="#33A02C")

#Scenario labels ------------------------------------------------------------------
scenarios = c("current_policies","1p5C", "1p5C_delay")
scenario_labels = c("Ref","1p5", "1p5 delay")

#Global net CO2 emissions----------------------------------------------------------
# Net co2 emissions
net_co2$scenario <- factor(net_co2$scenario, levels = scenarios, labels=scenario_labels)

ggplot(data=filter(net_co2, year %in% plot_years,scenario!="NA", region!="ROW", region!="Global",scenario!="NA"),aes(x=year, y=value / 1000, color=scenario))+
  geom_line(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  facet_wrap(~region, scales = "free") +
  labs(title = "Net CO2 emissions", x="", y="GTCO2") +
  scale_color_manual(values = scenario_colors) +
  plot_theme 
  ggsave(paste0(fig_dir, "/net_co2_emissions_allregions.png"), height = 6, width = 9, units = "in")


for (i in regions_aggregated) {
  ggplot(data=filter(net_co2, region == i, year %in% plot_years, region!="NA",scenario!="NA"),
         aes(x=year, y=value / 1000, color=scenario)) +
    geom_line(size = 1.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +
    labs(title = paste(expression(Net~CO[2]~emissions)), x="", y=bquote(GtCO[2])) +
    scale_color_manual(values = scenario_colors) +
    plot_theme 
    ggsave(paste0(fig_dir, "/net_co2_emissions_", i, ".png"), height = 6, width = 9, units = "in")
}
  

#save data 
net_co2_data <- filter(net_co2, region %in% regions_aggregated,
                      year %in% plot_years,scenario!="NA")
write.csv(net_co2_data, paste0(results_dir, '/net_co2.csv'))

#Pie chart ---------------------------------------------------------------------------
CO2_emissions_sector_nobio$scenario <- factor(CO2_emissions_sector_nobio$scenario,
                                              levels= scenarios, labels = scenario_labels)

ironsteel_CO2 <- filter(CO2_emissions_sector_nobio, sector=="iron and steel",
                        scenario!="NA", region != "Global")
ironsteel_CO2_total <- filter(ironsteel_CO2) %>% 
  mutate(value = value) %>%
  group_by(year, scenario) %>% 
  summarize(sum = sum(value))

ironsteel_CO2 <- ironsteel_CO2 %>%
  left_join(ironsteel_CO2_total, by = c("year","scenario"))%>%
  mutate(share = value/sum*100)

pie_years <- c(2020,2030,2050)
ggplot(filter(ironsteel_CO2, year%in%pie_years, scenario=="1p5"), 
       aes(x= "", y = share , fill = region)) +
  geom_bar(stat = "identity", width = 1) + 
  scale_fill_manual(values = regions) +
  facet_wrap(~year)+
  coord_polar("y", start = 0) + 
  plot_theme +
  theme(axis.text.x=element_blank()) +
  blank_theme + 
  labs(title = expression(bold(paste(Regional~contributions~to~total~iron~and~steel~CO[2]~emissions,", 1p5 scenario"))))

ggsave(paste0(fig_dir, "/ironsteel_CO2_pie",  ".png"), height = 6, width = 9, units = "in")

#save underlying pie chart data as csv 
write.csv(ironsteel_CO2, paste0(results_dir, "/iron_steel_CO2_pie.csv"))

#material efficiency pie chart

mat_eff <- data.frame(cbind(c("Direct reuse", 'Optimized building design', 'High-strength steel', 'Lifetime extension', 'Post-use recycling', 'Lightweighting',
                              'Improved semi-manufacturing yields', 'Improved product manufacturing yields'),
                            c(.15,.09,.05,.17,.17,.18,.07,.12)))
colnames(mat_eff) <- c("Material efficiency measure", "value")
ggplot(filter(mat_eff), 
       aes(x= "", y = value , fill = `Material efficiency measure`)) +
  geom_bar(stat = "identity", width = 1) + 
  scale_fill_manual(values = mat_eff_colors) +
  coord_polar("y", start = 0) + 
  plot_theme +
  theme(axis.text.x=element_blank()) +
  blank_theme + 
  labs(title = "Material Efficiecy Measures")

ggsave(paste0(fig_dir, "/mat_eff_pie",  ".png"), height = 6, width = 9, units = "in")


#reference iron and steel production stacked line chart ---------------------------------------------
ironsteel_production$scenario <- factor(ironsteel_production$scenario,levels= scenarios, labels = scenario_labels)


  ggplot(data=filter(ironsteel_production, year %in% plot_years, scenario=="Ref", region!="Global"),
         aes(x=year, y=value / 1000, fill=region)) +
    geom_area() +
    labs(title = "Iron and steel production, ref scenario", x="", y="Mt") +
    scale_fill_manual(values = regions)+
    plot_theme 
    ggsave(paste0(fig_dir, "/iron-steel-prod-stacked.png"), height = 6, width = 6, units = "in")


ironsteel_production_data <- filter(ironsteel_production, 
                                    region %in% regions_aggregated, 
                                    year %in% plot_years, scenario != "NA")

write.csv(ironsteel_production_data, paste0(results_dir, "/ironsteel_production_data.csv"))

#iron steel production by tech ----------------------------------------------------------------
ironsteel_production_tech$scenario <- factor(ironsteel_production_tech$scenario, levels = scenarios, labels=scenario_labels)
ironsteel_production_tech$technology <- factor(ironsteel_production_tech$technology, 
                                             levels = unique(ironsteel_production_tech$technology),
                                             labels = c('BF-biomass',
                                                        'BF-BOF','BF-CCUS','BF-H2','DRI-EAF-Fossil','DRI-EAF-CCUS',
                                                        'EAF-scrap','DRI-EAF-H2'))

for (i in regions_aggregated) {
  ggplot(data=filter(ironsteel_production_tech, region == i,
                     year %in% plot_years, scenario != "NA"),
         aes(x=year, y=value, fill=technology)) +
    geom_col()+
    facet_wrap(~scenario, ncol=3) +
    labs(title = paste(i, "iron and steel production by technology"), x="", y="Mt") +
    scale_fill_manual(values = tech_colors)+
    plot_theme

  ggsave(paste0(fig_dir, "/ironsteel_production_tech_", i, ".png"), height = 7, width = 11, units = "in")
}

ggplot(data=filter(ironsteel_production_tech,region %in% regions_aggregated, 
                   year %in% plot_years, scenario!="NA",
                   scenario=="1p5", region!="Global", region!="ROW"),
       aes(x=year, y=value, fill=technology)) +
  geom_col()+
  facet_wrap(~region, scale="free") +
  labs(title = paste("Iron and steel production by technology, 1p5 scenario"), x="", y="Mt") +
  scale_fill_manual(values = tech_colors)+
  plot_theme

ggsave(paste0(fig_dir, "/ironsteel_production_tech_all_regions", ".png"), height = 6, width = 10, units = "in")

#save data to csv 
ironsteel_production_tech_data <- filter(ironsteel_production_tech, region %in% regions_aggregated, 
                                              year %in% plot_years, scenario!="NA")
write.csv(ironsteel_production_tech_data , paste0(results_dir, "/ironsteel_production_tech_data.csv"))


#total CO2 emissions----------------------------------------------------------------------------
CO2_emissions$scenario <- factor(CO2_emissions$scenario, levels = scenarios, labels=scenario_labels)

# ggplot(data=filter(CO2_emissions, year %in% plot_years, scenario!="NA"),
#        aes(x=year, y=value / 1000, color=scenario)) +
#   geom_line(size = 1) +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
#   facet_wrap(~region, scales = "free") +
#   labs(title = "Total CO2 emissions (FFI)", x="", y="GTCO2") +
#   scale_color_manual(values = scenario_colors) +
#   plot_theme
#   ggsave(paste0(fig_dir, "/co2_emissions_allregions.png"), height = 6, width = 9, units = "in")

# for (i in regions_aggregated) {
#   ggplot(data=filter(CO2_emissions, region == i, year %in% plot_years, scenario!="NA"),
#          aes(x=year, y=value / 1000, color=scenario)) +
#     geom_line(size = 1.2) +
#     geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +
#     labs(title = paste(i, "total CO2 emissions (FFI)"), x="", y="GTCO2") +
#     scale_color_manual(values = scenario_colors) +
#     plot_theme +
#     ggsave(paste0(fig_dir, "/co2_emissions_", i, ".png"), height = 6, width = 9, units = "in")
# }


#energy use by fuel -------------------------------------------------------------

#separate coal with CCS
industry_energy_tech_fuel$scenario <- factor(industry_energy_tech_fuel$scenario,
                                             levels = scenarios, labels = scenario_labels)
x<-filter(industry_energy_tech_fuel, sector=="iron and steel")
#rename fuels to recognizable names, rename fuels with CCS 
x <- x %>% mutate(input = if_else(input == "delivered coal", "coal", input))
x <- x %>% mutate(input = if_else(input == "delivered biomass", "biomass", input))
x <- x %>% mutate(input = if_else(input == "elect_td_ind", "electricity", input))
x <- x %>% mutate(input = if_else(input == "H2 enduse", "hydrogen", input))
x <- x %>% mutate(input = if_else(input == "refined liquids industrial", "refined liquids", input))
x <- x %>% mutate(input = if_else(input == "wholesale gas", "gas", input))
x <- x %>% mutate(input2 = if_else((technology == "BLASTFUR CCS" & input != "biomass" & input != "electricity" & input != "hydrogen")|(technology=="EAF with DRI CCS" & input != "biomass" & input != "electricity" & input != "hydrogen"), paste0(input, " with CCS"), input))
x <- x %>% mutate(input = input2)
#convert from EJ to TWh
#x <- x %>% mutate(value = value * 277.778, Units = "TWh")
# for (i in regions_aggregated) {
#   ggplot(data=filter(x, region == i, year %in% plot_years,sector == "iron and steel", scenario!="Ref"),aes(x=year, y=value, fill=input)) +
#     geom_col() +
#     facet_wrap(~scenario, ncol=2) +
#     labs(title = paste(i, "iron and steel energy use by fuel"), x="", y="EJ") +
#     scale_fill_manual(values = fuel_colors) +
#     plot_theme 
#   
#   ggsave(paste0(fig_dir, "/ironsteel_energy_fuel_", i, ".png"), height = 6, width = 9, units = "in")
# }


ggplot(data=filter(x, region %in% regions_aggregated, region!="Global", 
                   region!="ROW", year %in% plot_years,sector == "iron and steel",
                   scenario=="1p5"),aes(x=year, y=value, fill=input)) +
  geom_col() +
  facet_wrap(~region, ncol=3, scale="free") +
  labs(title = paste("Iron and steel energy use by fuel, 1p5 scenario"), x="", y="EJ") +
  scale_fill_manual(values = fuel_colors) +
  plot_theme
  ggsave(paste0(fig_dir, "/ironsteel_energy_fuel_all",  ".png"), height = 6, width = 10, units = "in")

  
ggplot(data=filter(x, region=="Global", year %in% plot_years,sector == "iron and steel",
                     scenario!="NA"),aes(x=year, y=value, fill=input)) +
    geom_col() +
  facet_wrap(~scenario, ncol=3) +
    labs(title = paste("Global iron and steel energy use by fuel"), x="", y="EJ") +
    scale_fill_manual(values = fuel_colors) +
    plot_theme
  ggsave(paste0(fig_dir, "/ironsteel_energy_fuel_global",  ".png"), height = 7, width = 11, units = "in")
  
  
  #save data 
industry_energy_tech_fuel_data <- filter(x, region %in% regions_aggregated, 
                                         year %in% plot_years, sector == "iron and steel",
                                         scenario != "NA") %>%
                                  select(-input2)
write.csv(industry_energy_tech_fuel_data, paste0(results_dir, "/industry_energy_tech_fuel_data.csv"))


#plot indirect emissions ----------------------------------------------------------
final_ene_sect_fuel$scenario <- factor(final_ene_sect_fuel$scenario,levels = scenarios, labels = scenario_labels)
final_ene_sect_fuel <- filter(final_ene_sect_fuel, scenario!="NA")
#CO2_emissions_sector_nobio$scenario  <- factor(CO2_emissions_sector_nobio$scenario, levels = scenarios, labels=scenario_labels)
CO2_emissions_sector_nobio <- filter(CO2_emissions_sector_nobio, scenario!="NA")

#get total fuel use in all sectors 
final_ene_sect_fuel_total <- final_ene_sect_fuel %>%
  group_by(region, year, scenario, input) %>%
  summarize(total = sum(value)) %>%
  ungroup()

#filter out portion of iron and steel electricity use
ironsteel_fuel <- final_ene_sect_fuel %>%
 filter(sector == "iron and steel")%>%
 group_by(region, scenario, input, year) %>%
 ungroup()

#calculate share of fuel use from iron/steel 
ironsteel_fuel <- ironsteel_fuel %>%
  left_join(final_ene_sect_fuel_total, by = c("region","year","scenario","input")) %>%
 mutate(share = value/total)


CO2_emissions_sector_nobio_assigned <- CO2_emissions_sector_nobio %>%
 left_join(CO2_sector_mapping, by = c("sector"="GCAM_sector"))%>%
  mutate(sector=sector_mapping)

CO2_emissions_sector_nobio_assigned <- CO2_emissions_sector_nobio_assigned %>%
 group_by(region, scenario, sector, year) %>%
 summarize(total_emissions = sum(value)) %>%
 ungroup()

electricity_emissions <- filter(CO2_emissions_sector_nobio_assigned, 
                                sector=="electricity", total_emissions<0)
write.csv(electricity_emissions, paste0(results_dir,"/electricity_emissiosn.csv"))
colnames(CO2_emissions_sector_nobio_assigned) <- c("region","scenario",
                                              "input","year", "total_emissions")
#multiply iron steel portion * electricity emissions 
ironsteel_fuel <- ironsteel_fuel %>% 
 left_join(CO2_emissions_sector_nobio_assigned, 
by = c("region","scenario","year","input")) %>%
  mutate(indirect = share*total_emissions) %>%
  filter(total_emissions!="NA")

ironsteel_indirect_emisisons <- ironsteel_fuel %>% 
  filter(indirect !="NaN") %>%
  select(c("year","region","indirect","scenario")) %>%
  group_by(year, region, scenario) %>%
  summarize(total_indirect = sum(indirect)) %>%
  ungroup() 

#get direct CO2 emissions 
CO2_emissions_iron_steel <- filter(CO2_emissions_sector_nobio, 
                                   sector=="iron and steel") %>%
  left_join(ironsteel_indirect_emisisons, by=c("year", "scenario", "region"))

CO2_emissions_iron_steel <- gather(CO2_emissions_iron_steel,emission,value,6:7)

CO2_emissions_iron_steel <- CO2_emissions_iron_steel %>%
  mutate(`year `= as.character(year))
#plot 
for (i in regions_aggregated) {
  ggplot(data=filter(CO2_emissions_iron_steel, region == i, year %in% c(2020,2030,2050), scenario=="1p5" | scenario=="Ref"),
         aes(x=`year `, y=value / 1000 , fill=emission)) +
    geom_bar(stat = "identity", width =.7) +
    facet_wrap(~scenario)+
    scale_fill_manual(values = c("value"="#E31A1C", "total_indirect"="#FB9A99"), labels = c("direct","indirect")) +
    plot_theme +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    labs(y=expression(GtCO[2]), title = bquote(Global~iron~and~steel~direct~and~indirect~CO[2]~emissions), face="bold", x)+
    theme(legend.title = element_blank())
    ggsave(paste0(fig_dir, "/co2_indirect_1p5", i, ".png"), height = 6, width = 9, units = "in")
}

write.csv(CO2_emissions_iron_steel, paste0(results_dir,"/direct_indirect_emissions.csv"))

#hydrogen plotting by tech --------------------------------------------------------------------------------------
#hydrogen green, blue, grey calcs
hydrogen_production_tech$scenario <- factor(hydrogen_production_tech$scenario, levels = scenarios, labels=scenario_labels)
hydrogen_production <- filter(hydrogen_production_tech, scenario!="NA")
hydrogen_production <- hydrogen_production %>% left_join(hydrogen_mapping, by=c("technology"="technology"))

for (i in regions_aggregated) {
  ggplot(data=filter(hydrogen_production, region == i, year %in% plot_years, scenario!="NA"),
         aes(x=year, y=value, fill=color)) +
    labs(fill="Type of hydrogen")+
    geom_col()+
    facet_wrap(~scenario, ncol=3) +
    labs(title = paste(i, "hydrogen by production method"), x="", y="Mt") +
    scale_fill_manual(values = hydrogen_pal)+
    plot_theme 
    ggsave(paste0(fig_dir, "/hydrogen_production_tech_", i, ".png"), height = 6, width = 10, units = "in")
}

#save data with shares green,blue,grey hydrogen 
hydrogen_production_2 <- hydrogen_production %>%
  group_by(year, region, scenario) %>%
  summarize(total = sum(value)) %>%
  ungroup()
  
hydrogen_production_data <- hydrogen_production %>%
  group_by(year, region, scenario, color) %>%
  summarize(value=sum(value)) %>%
  left_join(hydrogen_production_2, by=c("year", "region","scenario"))
  
hydrogen_production_data <- hydrogen_production_data %>%
  mutate(share = value/total*100) %>%
  filter(year > 2015)

write.csv(hydrogen_production_data, paste0(results_dir, "/hydrogen_production.csv"))

#iron and steel CO2 emissions-------------------------------------------------------
#CO2_emissions_sector_nobio$scenario  <- factor(CO2_emissions_sector_nobio$scenario, levels = scenarios, labels=scenario_labels)

for (i in regions_aggregated) {
  ggplot(data=filter(CO2_emissions_sector_nobio, sector == "iron and steel", region == i, year %in% plot_years, scenario!="NA"),
         aes(x=year, y=value/1000, color=scenario)) +
    geom_line(size = 1.4) +
    labs(title = paste(i, "iron and steel CO2 emissions"), x="", y="GTCO2") +
    #scale_color_manual(values = scenario_colors) +
    plot_theme
  
  ggsave(paste0(fig_dir, "/ironsteel_co2_emissions_", i, ".png"), height = 6, width = 8, units = "in")
}

ggplot(data=filter(CO2_emissions_sector_nobio, sector == "iron and steel", region !="Global", region!="ROW", year %in% plot_years, scenario!="NA"),
       aes(x=year, y=value/1000, color=scenario)) +
  geom_line(size = 1.2) +
  facet_wrap(~region, scale="free", ncol=2)+
  labs(title = paste("Iron and steel CO2 emissions"), x="", y="GTCO2") +
  scale_color_manual(values = scenario_colors) +
  plot_theme +
  ggsave(paste0(fig_dir, "/ironsteel_co2_emissions_", i, ".png"), height = 6, width = 6, units = "in")

ironsteel_CO2_data <- filter(CO2_emissions_sector_nobio, sector == "iron and steel",
                             region %in% regions_aggregated, year %in% plot_years, scenario!="NA")
            
#calculations-------------------------------------------------------------------------------------

#inputs calcs -------------------------------------------------------------------------
industry_energy_tech_fuel_data <- industry_energy_tech_fuel_data %>%
  select(-technology) %>%
  group_by(region, year, scenario, input)%>%
  summarize(value = sum(value)) %>%
  ungroup()

industry_energy_tech_fuel_data_2 <- industry_energy_tech_fuel_data %>%
  group_by(region, year, scenario) %>%
  summarize(total_energy = sum(value)) %>%
  ungroup()

industry_energy_tech_fuel_data <- industry_energy_tech_fuel_data %>%
  left_join(industry_energy_tech_fuel_data_2, by = c("region", "year", "scenario")) %>%
  mutate(share = value/total_energy*100)


industry_energy_tech_fuel_share_data <- spread(industry_energy_tech_fuel_data, scenario, share)
write.csv(industry_energy_tech_fuel_data, paste0(results_dir, "/fuel_use.csv"))

industry_energy_tech_fuel_data <- spread(industry_energy_tech_fuel_data, scenario, value)
write.csv(industry_energy_tech_fuel_data, paste0(results_dir, "/fuel_use.csv"))

fuel_total <- filter(x, scenario!="NA") %>%
  group_by(region, year, scenario) %>%
  summarize(total = sum(value)) %>%
  ungroup()

fuel <- filter(x, scenario!="NA") %>%
  group_by(region, year, scenario, input) %>%
  summarise(value = sum(value))%>%
  ungroup() %>%
  left_join(fuel_total,by=c("region","year","scenario"))%>%
  mutate(share = value/total*100) %>%
  select(region, year, scenario, input, value, share) %>%
  filter(year%in%plot_years)
write.csv(fuel, paste0(results_dir, "/fuel.csv"))

#coal phaseout 
coal<-filter(fuel, input=="coal")
write.csv(coal, paste0(results_dir, "/coal.csv"))

#production by tech 
#production tech calcs --------------------------------------------------------------
ironsteel_production_tech_data2 <- ironsteel_production_tech_data %>%
  group_by(region, scenario, year) %>%
  summarize(total = sum(value)) %>%
  ungroup()

ironsteel_production_tech_data <- ironsteel_production_tech_data %>%
  left_join(ironsteel_production_tech_data2, by = c("region","year","scenario"))

ironsteel_production_tech_data <- ironsteel_production_tech_data %>%
  mutate(share = value / total * 100)

ironsteel_production_tech_data <- ironsteel_production_tech_data %>%
  select(-c(value,total)) 
  
write.csv(ironsteel_production_tech_data, paste0(results_dir, "/ironsteel_production_tech_share_data.csv"))
  
#iron and steel CO2 emissions calcs------------------------- 
ironsteel_CO2_data_1p5 <- filter(ironsteel_CO2_data, scenario == "1p5")
write.csv(ironsteel_CO2_data_1p5, paste0(results_dir, "/co2_reductions.csv"))
ironsteel_CO2_data_1p5_2020 <- filter(ironsteel_CO2_data, scenario == "1p5", year == 2020)%>%
  select(-year)
ironsteel_CO2_data_1p5 <- ironsteel_CO2_data_1p5 %>%
  left_join(ironsteel_CO2_data_1p5_2020, 
            by = c("region", "scenario","sector","Units")) %>%
  mutate(percent_reduction = (value.x-value.y)/value.y*100) %>%
  mutate(absolute_reduction = value.x - value.y) %>%
  filter(year>2020)
  
write.csv(ironsteel_CO2_data_1p5, paste0(results_dir, "/co2_reductions.csv"))


ggplot(data=ironsteel_CO2_data_1p5, aes(x=year, y = percent_reduction, color=region))+
  geom_line(size = 1) +
  labs(title = "iron steel CO2 percent reduction", x="", y="tCO2/tsteel") +
  plot_theme 
ggsave(paste0(results_dir, "/percent_reduction.png"), height = 6, width = 9, units = "in")

#iron and steel CO2 emissions intensity calcs------------------------- 
emissions_intensity <-  ironsteel_CO2_data %>%
  left_join(ironsteel_production_data,
            by = c("scenario","year","region", "sector")) %>%
  mutate(em_intensity = (value.x/value.y)) %>%
  filter(scenario=="1p5", year>2015)

emissions_intensity_2020 <- filter(emissions_intensity, year == 2020) %>%
  select(c(em_intensity, region))

colnames(emissions_intensity_2020) <- c("em_intensity_2020", "region")

emissions_intensity <- emissions_intensity %>%
  left_join(emissions_intensity_2020, by = "region") %>%
  mutate(abs_reduction = em_intensity_2020 - em_intensity,
         perc_reduction =  (em_intensity - em_intensity_2020)/em_intensity_2020 * 100,
         Units = "tCO2/t steel")

emissions_intensity <- emissions_intensity %>%
  select(region, year, scenario, em_intensity,
         em_intensity_2020, abs_reduction, perc_reduction, Units)

write.csv(emissions_intensity, paste0(results_dir, "/emissions_intensity.csv"))

ggplot(data=emissions_intensity, aes(x=year, y = em_intensity, color=region))+
  geom_line(size = 1) +
  labs(title = "iron steel CO2 emissions intensity", x="", y="tCO2/tsteel") +
  plot_theme 
ggsave(paste0(results_dir, "/emissions_intensity.png"), height = 6, width = 9, units = "in")





#cumulative co2 --------------

CO2_emissions_sector_nobio_assigned <- CO2_emissions_sector_nobio %>%
  left_join(CO2_sector_mapping, by = c("sector"="GCAM_sector"))%>%
  mutate(sector=sector_mapping)

a_cumulative_ironsteel_co2_to2100 <- CO2_emissions_sector_nobio %>%
  filter(sector=="iron and steel") %>%
  filter(scenario!="NA")%>%
  group_by(scenario, region) %>%
  complete(year = seq(2010, 2100)) %>%
  mutate(value = na.approx(value),
         value = value / 1000,
         Units = "GtCO2") %>%
  ungroup %>%
  filter(year >= 2020) %>%
  group_by(scenario, region) %>%
  mutate(value = cumsum(value)) %>%
  ungroup() %>%
  filter(year==2050|year==2030)

a_cumulative_ironsteel_co2_to2100 <- CO2_emissions_sector_nobio %>%
  filter(sector=="iron and steel", sector!="NA") %>%
  filter(scenario=="1p5")%>%
  group_by(scenario, region) %>%
  complete(year = seq(2010, 2100)) %>%
  mutate(value = na.approx(value),
         value = value / 1000,
         Units = "GtCO2") %>%
  ungroup %>%
  filter(year >= 2020 & year <=2050, region=="China") %>%
  mutate(value = sum(value))


a_cumulative_ironsteel_co2_to2100 <- spread(a_cumulative_ironsteel_co2_to2100, scenario, value)
write.csv(a_cumulative_ironsteel_co2_to2100,paste0(results_dir,"/cumulativeCO2.csv"))

# ggplot(data=emissions_intensity, aes(x=year, y = em_intensity, color=region))+
#   geom_line(size = 1) +
#   labs(title = "emissions intensity", x="", y="tCO2/tsteel") +
#   plot_theme 
# ggsave(paste0(results_dir, "/em_intensity.png"), height = 6, width = 9, units = "in")

#iron and steel tech rate of retirement calcs ------------------------------
# blastfurnace_ROR <- filter(ironsteel_production_tech, 
#                            technology %in% c("BF-BOF", "BF-CCUS"), scenario == "1p5") %>%
#                     spread(technology, value)
# 
# blastfurnace_ROR_last <- blastfurnace_ROR %>%
#   mutate(year = year + 5) %>%
#   select(c(year, region, "BF-BOF", "BF-CCUS"))
# 
# blastfurnace_ROR <- blastfurnace_ROR %>%
#   left_join(blastfurnace_ROR_last, by = c("region","year")) %>%
#   filter(year>2015, year <2055)
#   
# blastfurnace_ROR <- blastfurnace_ROR %>% 
#   mutate(BF_BOF_ROR = (`BF-BOF.x` - `BF-BOF.y`)/ `BF-BOF.y` / 5 * 100,
#          BF_CCUS_ROR = (`BF-CCUS.x` - `BF-CCUS.y`)/ `BF-CCUS.y` / 5 * 100)
# 
# 
# blastfurnace_ROR <- blastfurnace_ROR %>% 
#   select(c(region, scenario, Units, year, 'BF-BOF.x', 'BF-CCUS.x',BF_BOF_ROR, BF_CCUS_ROR))
# 
# ggplot(data=blastfurnace_ROR, aes(x=year, y = BF_BOF_ROR, color=region))+
#   geom_line(size = 1) +
#   labs(title = "emissions intensity", x="", y="BF ROR") +
#   plot_theme 
# ggsave(paste0(results_dir, "/BF_BOF_ROR.png"), height = 6, width = 9, units = "in")
# 
# write.csv(blastfurnace_ROR, paste0(results_dir, "/blastfurnace_ROR.csv"))
colnames(elec_mapping) <- c("technology","generation", "generation technology")
electricity <- electricity %>%
  left_join(elec_mapping)
#electricity generation -----------
elec <- electricity %>%
  group_by(region,scenario,year,generation) %>%
  dplyr::summarize(value = sum(value)) %>%
  dplyr::ungroup()

elec2 <- elec %>%
  group_by(region,scenario,year) %>%
  dplyr::summarize(sum = sum(value)) %>%
  dplyr::ungroup()

elec <- elec %>%
  left_join(elec2, c("region","year", "scenario"))

elec <- elec %>%
  mutate(share = value/sum*100)

elec <- elec %>%
  select(c("region","year","share", "scenario", "generation"))
write.csv(elec, paste0(results_dir,"/elec_gen.csv"))




#relative price increase calculationg 
a<-prices_sector
prices_sector$scenario <- factor(prices_sector$scenario, levels = scenarios, labels=scenario_labels)
prices_sector<-a
prices_sector <- prices_sector %>%
  filter(scenario != "NA", sector == "iron and steel")

prices_sector_2050 <- prices_sector 

prices_sector_2020 <- prices_sector %>%
  filter(year==2020)


prices_increase <- prices_sector_2020 %>%
  left_join(prices_sector_2050, by = c("scenario", "region"))%>%
  mutate(change = (value.x-value.y)/value.x)

prices_increase <- prices_increase %>% select(region, scenario, year.y, change)
prices_increase <- spread(prices_increase, scenario, change)
write.csv(prices_increase, paste0(results_dir, "/prices.csv"))


x<-filter(ironsteel_production_tech_vintage, region=="Global", vintage == year, year >2025, technology == "BLASTFUR", scenario == "1p5C" | scenario == "1p5C_delay")
x <- x %>% group_by(scenario) %>%
  summarize(total = sum(value))
