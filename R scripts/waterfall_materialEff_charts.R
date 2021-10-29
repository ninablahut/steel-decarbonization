install.packages("waterfalls")
library(waterfalls)

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
library(ggsci)
library(zoo)

library(extrafont)
loadfonts(quiet = T)



#set wd----------------------
options(scipen=999)
run_dir <- "C:/Users/blah822/Documents/steelDatabase/10-23" # SET MANUALLY FOR EACH RUN
if(!dir.exists(paste0(run_dir,"/figures"))){dir.create(paste0(run_dir,"/figures.2"))}
fig_dir <- "C:/Users/blah822/Documents/steelDatabase/test"
results_dir <- "C:/Users/blah822/Documents/steelDatabase/test"
setwd("C:/Users/blah822/Documents/steelDatabase")
source("functions.R")
source("diag_util_functions.R")
# Download batch queries from PIC into run directory===========

queries <- list.files(run_dir, pattern='queryoutall')
for (i in queries) {
  filename <- gsub('.csv','', i) %>% gsub('queryoutall_','', .)
  assign(filename, readr::read_csv(paste0(run_dir, "/", i), skip = 1))
}


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
  
# Aggregate to deep dive regions and ROW --------------------------------------------------------------------------------

region_mapping <- read.csv("steel_region_mapping.csv")

regions_aggregated <- unique(region_mapping$steel_region)

C_to_CO2 <- 44/12

#scenarios & scenario labels --------------------------------------------------------
scenarios = c("current_policies","1p5C", "current_policies_advEE","current_policies_advMEF" )
scenario_labels = c("Ref", "1p5", "ref_advEE", "ref_MEF")

#import and aggregate data -------------------------------------------------------

#import total emissions data and rename scenarios
CO2_emissions_sector_nobio <- CO2_emissions_sector_nobio_v2 %>% parse_output_scenario %>% add_global_sum()
CO2_emissions_sector_nobio <- CO2_emissions_sector_nobio %>% dplyr::mutate(value = if_else(Units == "MTC", value * C_to_CO2, value),
                                                                           Units = if_else(Units == "MTC", "MTCO2", Units))

CO2_emissions_sector_nobio <- CO2_emissions_sector_nobio %>% aggregate_regions(region_mapping, colname = "steel_region")

CO2_emissions_sector_nobio$scenario  <- factor(CO2_emissions_sector_nobio$scenario, levels = scenarios, labels=scenario_labels)
CO2_emissions_sector_nobio <- filter(CO2_emissions_sector_nobio, sector=="iron and steel")
#import production and rename sceanrios
ironsteel_production <- ironsteel_production %>% parse_output_scenario %>% add_global_sum()
ironsteel_production <- ironsteel_production %>% aggregate_regions(region_mapping, colname = "steel_region")
ironsteel_production$scenario  <- factor(ironsteel_production$scenario, levels = scenarios, labels=scenario_labels)

#import emissions by subsector data and rename scenarios
CO2_emissions_subsector <- CO2_emissions_subsector %>% parse_output_scenario %>% add_global_sum()%>% dplyr::mutate(value = if_else(Units == "MTC", value * C_to_CO2, value),
                                                                                                                   Units = if_else(Units == "MTC", "MTCO2", Units))
CO2_emissions_subsector <- CO2_emissions_subsector %>% aggregate_regions(region_mapping, colname = "steel_region")
CO2_emissions_subsector$scenario  <- factor(CO2_emissions_subsector$scenario, levels = scenarios, labels=scenario_labels)

#import production by tech
ironsteel_production_tech <- ironsteel_production_tech %>% parse_output_scenario %>% add_global_sum()
ironsteel_production_tech <- ironsteel_production_tech %>% aggregate_regions(region_mapping, colname = "steel_region")
ironsteel_production_tech$scenario  <- factor(ironsteel_production_tech$scenario, levels = scenarios, labels=scenario_labels)


#import emissions by tech
CO2_emissions_tech <- CO2_emissions_tech %>% parse_output_scenario %>% add_global_sum() %>% dplyr::mutate(value = if_else(Units == "MTC", value * C_to_CO2, value),
                                                                                                          Units = if_else(Units == "MTC", "MTCO2", Units))
CO2_emissions_tech <- CO2_emissions_tech %>% aggregate_regions(region_mapping, colname = "steel_region")
CO2_emissions_tech$scenario  <- factor(CO2_emissions_tech$scenario, levels = scenarios, labels=scenario_labels)

#import CCS sequestration data
CO2_squestration_subsector <- CO2_squestration_subsector %>% parse_output_scenario %>% add_global_sum() %>% dplyr::mutate(value = if_else(Units == "MTC", value * C_to_CO2, value),
                                                                                                                          Units = if_else(Units == "MTC", "MTCO2", Units))
CO2_squestration_subsector <- CO2_squestration_subsector %>% aggregate_regions(region_mapping, colname = "steel_region")
CO2_squestration_subsector$scenario  <- factor(CO2_squestration_subsector$scenario, levels = scenarios, labels=scenario_labels)

#


results4 <- c("Mitigation measure", "region","reduction","year")
for (selected_year in seq(2020, 2050, 5)){
  results <- c("Ref emissions", "Energy efficiency contribution",
               "Material efficiency contribution", "Price-induced contribution",
               "High scrap use contribution", "H2 contribution", "CCUS contribution",
               "1p5 emissions")
for (i in regions_aggregated) {
  #set region
  selected_region <- i
  
  #ref emissions and 1p5 emissions
  ref_emissions <- filter(CO2_emissions_sector_nobio, region == selected_region, year == selected_year, scenario=="Ref")$value
  emissions_1p5 <- filter(CO2_emissions_sector_nobio, region == selected_region, year == selected_year, scenario=="1p5")$value
  total_reduction <- emissions_1p5 - ref_emissions
  
  #1
  #a.	Contribution of adv EE to emissions reduction: emissions reduction from ref + adv EE (see below)
  
  #calculate adv EE emissions
  adv_EE_emissions <- filter(CO2_emissions_sector_nobio, region == selected_region, year == selected_year, scenario=="ref_advEE")$value
  adv_EE_contribution <- adv_EE_emissions - ref_emissions
  
  #b.	Contribution of adv MEF to emissions reduction: emissions reduction from ref + adv MEF (see below)
  adv_MEF_emissions <- filter(CO2_emissions_sector_nobio, region == selected_region, year == selected_year, scenario=="ref_MEF")$value
  adv_MEF_contribution <- adv_MEF_emissions - ref_emissions
  
  #c.	Contribution of price-induced demand reduction to emissions reduction: (price-induced demand reduction/MEF)* emissions reduction from adv MEF (bullet point b)
  MEF_production <- filter(ironsteel_production, region == selected_region, year == selected_year, scenario == "ref_MEF")$value
  ref_production <- filter(ironsteel_production, region == selected_region, year == selected_year, scenario == "Ref")$value
  MEF_reduction <- ref_production - MEF_production
  
  price_induced_production <- filter(ironsteel_production, region == selected_region, year == selected_year, scenario == "1p5")$value
  price_induced_reduction <-  price_induced_production - MEF_production
  
  price_induced_contribution <- (price_induced_reduction / MEF_reduction) * adv_MEF_contribution * -1
  
  #d.	Contribution of high scrap to emissions reduction: diff in steel production
  #from scrap between ref and 1.5C * (average emission intensity of other techs in
  #ref - emission intensity of scrap production in 1.5C)
  scrap_1p5 <- filter(ironsteel_production_tech, region == selected_region, year == selected_year, scenario == "1p5", subsector == "EAF with scrap")$value
  scrap_REF <- filter(ironsteel_production_tech, region == selected_region, year == selected_year, scenario == "Ref", subsector == "EAF with scrap")$value
  scrap_dif <- scrap_REF - scrap_1p5
  
  other_tech_REF<- filter(ironsteel_production_tech, region == selected_region, year == selected_year, scenario == "Ref", subsector != "EAF with scrap")
  emissions_other_tech <- filter(CO2_emissions_subsector, region == selected_region, year == selected_year, scenario == "Ref", subsector != "EAF with scrap", sector == "iron and steel")
  other_tech_emissions_intensity <- sum(emissions_other_tech$value)/sum(other_tech_REF$value)
  
  emissions_scrap_1p5 <- filter(CO2_emissions_subsector, region == selected_region, year == selected_year, scenario == "1p5", subsector == "EAF with scrap")$value
  scrap_emissions_intensity <- emissions_scrap_1p5 / scrap_1p5
  
  high_scrap_contribution <- scrap_dif * (other_tech_emissions_intensity - scrap_emissions_intensity)
  
  #e.	Contribution of hydrogen:
  #steel production from H2 in 1.5C * (average emission intensity in ref - emission
  #intensity of H2-based production in 1.5C) - we might need separate calculations for H2-BF and H2-DRI
  prod_HDRI_1p5 <-  filter(ironsteel_production_tech, region == selected_region, year == selected_year, scenario == "1p5", technology == "Hydrogen-based DRI")$value
  
  HDRI_emissions_intensity <- 0
  
  prod_ref <-  filter(ironsteel_production, region == selected_region, year == selected_year, scenario == "Ref")$value
  emissions_average <- filter(CO2_emissions_subsector, region == selected_region, year == selected_year, scenario == "Ref", sector == "iron and steel")
  average_emissions_intensity <- sum(emissions_average$value)/prod_ref
  
  H2_contribution <- prod_HDRI_1p5 * average_emissions_intensity * -1
  
  
  #f.	Contribution of CCS - actual carbon sequestration amount
  CCS_contribution <- filter(CO2_squestration_subsector, sector == "iron and steel", region ==  selected_region, scenario == "1p5", year == selected_year)$value * -1
  
  
  val <- c(ref_emissions, adv_EE_contribution, adv_MEF_contribution, price_induced_contribution,
           high_scrap_contribution, H2_contribution,  CCS_contribution, emissions_1p5)
  
  results <- data.frame(cbind(results, val))
}
  colnames(results)<-c("Mitigation measure", regions_aggregated)
  results2 <- gather(results, region, reduction, US:Global)
  results3 <- results2 %>% mutate(year=selected_year)
  results4 <- rbind(results4, results3)
}
# colnames(results)<-c("Mitigation measure", regions_aggregated)
# 
# #produce waterfall charts ------------------
# waterfall_colors <- c( "#E31A1C","#756bb1","#3182bd","#33A02C", "#FCD581","#FB9A99")
# spaces <- c(" "," "," "," "," "," "," ")
# for (i in regions_aggregated){
#   x <- gather(results,key="region", value="reduction", 2:9)
#   x <- x %>% mutate(reduction=as.numeric(reduction))
#   x <- filter(x, region==i)
#   x <- x %>% mutate (share = reduction / (sum(x$reduction)-x[8,3]-x[1,3]))
#   percents <- c("",paste0(round(x[2:7,]$share*100),"%"))
#   x <- x %>% mutate (adj_value = share * (x[8,3]-x[1,3]))
#   x <- x %>% mutate (new = (if_else(`Mitigation measure` == "Ref emissions" | `Mitigation measure` == "1p5 emissions", ceiling(reduction),ceiling(adj_value))))
#   x <- x %>% select(c("Mitigation measure", "new"))
#   waterfall(x[1:7,], calc_total = TRUE,total_rect_text_color = "black", total_axis_text = "1p5 emissions", fill_colours = waterfall_colors, fill_by_sign = FALSE, rect_text_labels = percents)+
#     theme_minimal()+  labs(title = paste0(i, " - contributions from mitigation measures"),
#                            y="MtCO2", x=" " )+
#     theme(axis.text.x = element_text(angle = 60, hjust = 1))+
#     theme(text = element_text(size=18))
#   
#   ggsave(paste0(fig_dir, "/waterfall_chart_values", i, ".png"), height = 7, width = 8, units = "in")
# }

#save underlying waterfall chart data -------------------------------
results4 <- results4[-1,] %>%
  mutate(reduction = abs(as.numeric(reduction)))

results2 <- results4 

spread <- spread(results2, `Mitigation measure`, reduction) %>%
  select(c(`1p5 emissions`,`Ref emissions`, region, year))

total <- results2 %>%
   group_by(region, year)%>%
   mutate(reduction = as.numeric(reduction))%>%
   filter(`Mitigation measure` != "1p5 emissions" & `Mitigation measure` != "Ref emissions") %>%
   summarize(total = sum(reduction)) %>%
   left_join(spread, by = c("region","year"))

results2 <- results2 %>%
  left_join(total, by =c("region","year")) %>%
  mutate(difference = `Ref emissions` - `1p5 emissions` ) %>%
  mutate(share = reduction / total) %>%
  mutate(adjusted = share * difference) %>%
  mutate(adjusted = if_else(`Mitigation measure` == "1p5 emissions", `1p5 emissions`, adjusted)) %>%
  mutate(adjusted = if_else(`Mitigation measure` == "Ref emissions", `Ref emissions`, adjusted))%>%
  mutate(reduction = adjusted) %>%
  select(-adjusted)

write.csv(results2, paste0(results_dir, "/waterfall_data.csv"))

spaces <- c(" "," "," "," "," "," "," ")


results2$`Mitigation measure` <- factor(results2$`Mitigation measure`, levels = c("Energy efficiency contribution",
             "Material efficiency contribution", "Price-induced contribution",
             "High scrap use contribution", "H2 contribution", "CCUS contribution",
             "1p5 emissions","Ref emissions"))

for (i in regions_aggregated) {
  ggplot(data=filter(results2, region == i, `Mitigation measure` != "Ref emissions"),
         aes(x=as.numeric(year), y=abs(reduction), fill=`Mitigation measure`)) +
    geom_area()+
    labs(title = paste(i, "CO2 reduction contributions from mitigation measures"), x="", y="MtCO2") +
    scale_fill_manual(values = reduction_colors)+
    plot_theme
  
  ggsave(paste0(fig_dir, "/reduction_contributions_", i, ".png"), height = 6, width = 11, units = "in")
}

results4$`Mitigation measure` <- factor(results4$`Mitigation measure`,
                                        levels = c("Ref emissions","CCUS","H2 contribution",
                                                   "Energy efficiency","High scrap use","Price-induced demand reduction",
                                                   "Material efficiency", "1p5 emissions"),
                                        labels = c("Ref emissions","CCUS contribution","H2 contribution",
                                                   "Energy efficiency contribution","High scrap use contribution",
                                                   "Price-induced contribution",
                                                   "Material efficiency contribution","1p5 emissions"))

reduction_colors <- c( "Energy efficiency contribution" = "gray50",
                       "Material efficiency contribution" = "darkturquoise",
                       "Price-induced contribution" = "forestgreen",
                       "High scrap use contribution" = "#756bb1",
                       "H2 contribution" = "darkgoldenrod3",
                       "CCUS contribution" ="dodgerblue3",
                       "1p5 emissions" = "firebrick3")

results2 <- results2 %>% 
  mutate(reduction = if_else(`Mitigation measure` == "Ref emissions", 
                            reduction, reduction *-1))
for (i in regions_aggregated) {
  ggplot(data=filter(results2, `Mitigation measure` != "Ref emissions", 
                     region %in% regions_aggregated, region != "ROW", region != "Global"),
         aes(x=as.numeric(year), y=reduction, fill=`Mitigation measure`)) +
    geom_area()+
    facet_wrap(~region, ncol=3, scale = "free")+
    labs(title = expression(bold(paste(Regional~CO[2]~reduction~contributions~from~mitigation~measures, ", 1p5 scenario"))), 
                            x="", y=expression(bold(MtCO[2]))) +
    scale_fill_manual(values = reduction_colors)+
    plot_theme
  
  ggsave(paste0(fig_dir, "/reduction_contriburtions_all", ".png"), height = 6, width = 11, units = "in")
}

spaces <- c(" "," "," "," "," "," "," ")
for (i in regions_aggregated){
  waterfall_data <- data.frame(filter(results2,region==i,`Mitigation measure` != "1p5 emissions", year == 2050))
  waterfall(select(waterfall_data, c(Mitigation.measure, reduction)), calc_total = TRUE,
            total_rect_text_color = "black", total_axis_text = "1p5 emissions",
            fill_colours = 2:7, fill_by_sign = FALSE, rect_text_labels = c("", round(waterfall_data$share[2:7]*100)))+
    theme_minimal()+
    labs(title = paste0(i, " - contributions from mitigation measures"),
                           y="MtCO2", x=" " )+
    theme(axis.text.x = element_text(angle = 60, hjust = 1))+
    theme(text = element_text(size=18))

  ggsave(paste0(fig_dir, "/waterfall_chart_values", i, ".png"), height = 7, width = 8, units = "in")
}


#plot ref, MEF induced, price induced production with historical and save underlying data ---------------

global_historical <- read_csv("C:/Users/blah822/OneDrive - PNNL/Industry/globalHistorical.csv")
global_historical <- gather(global_historical, year, value, 2:22) %>%
  mutate(year=as.numeric(year),value=as.numeric(value)) %>%
  mutate(scenario="Ref",sector="iron and steel",Units="Mt" )

ironsteel_production2 <- rbind(global_historical, filter(ironsteel_production, year > 2015))

ironsteel_production2 <- ironsteel_production2 %>%
  filter(year <2055, scenario!="NA", scenario != 'ref_advEE')

ironsteel_production3 <- ironsteel_production2


for (i in regions_aggregated) {
  ggplot(data=filter(ironsteel_production2, region == i, year %in% c(2000:2050), scenario=="ref_MEF" | scenario=="Ref"|scenario=="1p5"),
         aes(x=year, y=value, color = scenario)) +
    geom_line(size = 1.2) +
    labs(title = paste(i, " iron and steel production"), x="", y="Mt") +
    scale_y_continuous(limits = c(0, NA)) +
    scale_color_manual(labels = c("Reference","Material efficiency","Material efficiency and price increase") , values = c("Ref" = "#E31A1C", "ref_MEF"="#3182bd", "1p5"="#33A02C")) +
    plot_theme
  
  ggsave(paste0(fig_dir, "/ironsteel_production", i, ".png"), height = 8, width = 10, units = "in")
}

#write csv
scenarios_MEF = c("Ref","ref_MEF","1p5")
scenario_labels = c("Reference","Material efficiency","Material efficiency and price increase")
ironsteel_production3$scenario <- factor(ironsteel_production3$scenario, levels = scenarios_MEF, labels = scenario_labels)
write.csv(ironsteel_production3, paste0(results_dir, "/material_efficency.csv"))


