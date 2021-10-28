#plotting global steel demand
library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)
library(data.table)
install.packages("ggrepel")
install.packages("reshape")
install.packages("read.table")
library(reshape)
install.packages("devtools")
devtools::install_github("JGCRI/jgcricolors")
devtools::install_github("jgcricol")
library(jgcricolors)
library(ggrepel)
setwd("C:/Users/blah822/OneDrive - PNNL/Industry")

plot_theme <- theme_bw() + theme(text = element_text(size = 18),
                                 plot.title = element_text(size = 18, margin = margin(0, 0, 10, 0), hjust=.5),
                                 strip.text = element_text(size = 20),
                                 axis.title.x = element_text(vjust = 0.5))


##plot true steel use, finished equivalent
data <- read_excel("toplineSteelDemand.xlsx")
#factor and label
data$Projection <- factor(data$Projection, levels=c("Accenture Baseline",
                                                    "Accenture Radical Reduction",
                                                    "BNEF 2039-2040 GR after 2040",
                                                    "IEA net zero demand",
                                                    "IEA net zero w/o resource efficiency",
                                                    "Posco",
                                                    "Ref", "MEF"), labels=c("Accenture Baseline", "Accenture Radical Reduction", "BNEF", "IEA net zero demand", "IEA net zero w/o resource efficiency", "Posco","Reference (our analysis)", "Material Efficiency (our analysis)"))
cbPalette <- c("Accenture Baseline"="#999999",
               "Accenture Radical Reduction"="#E69F00",
               "IEA net zero demand"="#56B4E9",
               "IEA net zero w/o resource efficiency"="#009E73",
               "BNEF"= "#F0E442",
               "Posco" = "maroon",
               "Reference (our analysis)"="black","Material Efficiency (our analysis)"="black")
lineType <- c("Accenture Baseline"="solid",
               "Accenture Radical Reduction"="solid",
               "IEA net zero demand"="solid",
               "IEA net zero w/o resource efficiency"="solid",
               "BNEF"= "solid",
                "Posco" ="solid",
               "Reference (our analysis)"="dotted","Material Efficiency (our analysis)"="solid")
lineThick <- c("Accenture Baseline"=1,
              "Accenture Radical Reduction"=1,
              "IEA net zero demand"=1,
              "IEA net zero w/o resource efficiency"=1,
              "BNEF"= 1,
              "Posco" =1,
              "Reference (our analysis)"=2,"Material Efficiency (our analysis)"=2)

data <- gather(data, key= "year", value="crude steel production (million metric tons)", 2:52) %>% mutate(year = as.numeric(year))
data <- na.omit(data)
ggplot(data = data, mapping=aes(x=year, y=`crude steel production (million metric tons)`))+
  geom_line(aes(colour=Projection, linetype=Projection, size=Projection))+
  scale_color_manual(values = cbPalette)+
  scale_linetype_manual(values = lineType)+
  scale_size_manual(values=lineThick)+
  plot_theme+
  labs(title="Global crude steel production", y="Mt")+
  ggsave("C:/Users/blah822/OneDrive - PNNL/Industry/figures/globalDemand.png", height = 8.5, width = 11, units = "in")

setwd("C:/Users/blah822/Documents/steelDatabase/demand projections")

##plot true steel use, finished equivalent
data <- read_excel("chinaDemand.xlsx")
data <- gather(data, key= "year", value="crude steel production (million metric tons)", 2:54) %>% mutate(year = as.numeric(year))
#factor and label
data$scenario <- factor(data$scenario, levels=c("Accenture Radical Reduction",
                                                    "BNEF",
                                                    "IEA Sustainable Development",
                                                    "McKinsey",
                                                    "Ref", "MEF"), labels=c("Accenture Radical Reduction", "BNEF", "IEA Sustainable Development","McKinsey", "Reference (our analysis)", "Material Efficiency (our analysis)"))
cbPalette <- c("Accenture Radical Reduction"="#E69F00",
               "IEA Sustainable Development"="#56B4E9",
               "BNEF"= "#F0E442",
               "McKinsey" = "maroon",
               "Reference (our analysis)"="black","Material Efficiency (our analysis)"="black")
okie <- c("Accenture Radical Reduction"="solid",
              "IEA Sustainable Development"="solid",
              "BNEF"= "solid",
              "McKinsey" = "solid",
              "Reference (our analysis)"="dotted",
              "Material Efficiency (our analysis)"="solid")

data <- na.omit(data)

ggplot(data = filter(data,year%in%plot_years ), mapping=aes(x=year, y=`crude steel production (Mt)`))+
  geom_line(aes(colour=scenario, linetype=scenario), size=1)+
  scale_color_manual(values = cbPalette)+
  scale_linetype_manual(values = okie)+
  plot_theme+
  labs(title="China steel production (million metric tons)")+
  ggsave(fig_dir, height = 8.5, width = 11, units = "in")

