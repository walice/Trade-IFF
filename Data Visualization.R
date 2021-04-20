# Data Visualization
# Alice Lepissier
# alice.lepissier@gmail.com
# Originally prepared for the United Nations Economic Commission for Africa (UNECA)

## ## ## ## ## ## ## ## ## ## ##
# INDEX                     ####
## ## ## ## ## ## ## ## ## ## ##
# Preamble
# Codes Masterlist
# Yearly IFF Bar Charts
# .. Africa
# .. LMIC
# .. Developing
# .. Low-HDI
# Line Charts
# .. Line chart for conduits in Africa
# .. Line chart for conduits in LMIC
# .. Line chart for conduits in Developing
# Choropleth Maps
# .. Africa
# .. World
# Pie and Doughnut Charts
# .. Africa
# .. World
# .. Sunburst charts for World
# Treemap Charts
# .. Africa, yearly average
# .. LMIC, yearly average
# .. Developing, yearly average
# Stacked Bar Charts
# .. Stacked bar charts of commodities in top sectors
# .. Horizontal stacked bar charts of top sectors in conduits
# .. Stacked bar charts of top yearly average destinations in groups
# Flow Maps
# .. Destination flow maps of conduits
# .. Provenance flow maps of top inflows in World
# Lollipop Charts
# .. Top conduits in World
# .. Top conduits in Africa
# .. Top conduits in LMIC
# .. Top conduits in Developing
# Sankey Diagram
# .. Sankey diagram by GNI per capita and sector in LMIC
# .. Sankey diagram by reporter and partner GNI per capita in World
# .. Sankey diagram by reporter and partner GNI per capita in LMIC
# .. Sankey diagram by region and sector in World
# .. Sankey diagram by natural resource dependence and SITC sectors
# .. Sankey diagram by extreme natural resource dependence and partner region
# .. Sankey diagram by natural resource dependence and partner GNI per capita
# .. Sankey diagram by natural resource dependence and partner income group
# .. Sankey diagram by reporter and partner income group



## ## ## ## ## ## ## ## ## ## ##
# PREAMBLE                  ####
## ## ## ## ## ## ## ## ## ## ##

setwd("/home/alepissier/IFFe/") # Virtual server
data.disk <- "/scratch/alepissier/IFFe/"
# source("Scripts/Aggregate Results.R")
library(broom)
library(cartogram)
library(disco)
library(geosphere)
library(ggalluvial)
library(ggmap)
library(ggpubr)
library(ggrepel)
library(ggsunburst)
library(ggthemes)
library(mapproj)
library(maptools)
library(paletteer)
library(rcartocolor)
library(RColorBrewer)
library(readxl)
library(reshape2)
library(scales)
library(stringr)
library(tidyverse)
library(treemapify)
library(wesanderson)
library(xlsx)



## ## ## ## ## ## ## ## ## ## ##
# CODES MASTERLIST          ####
## ## ## ## ## ## ## ## ## ## ##

codes <- read_excel("/scratch/alepissier/IFFe/Data/Codes_Masterlist.xlsx", sheet = "Codes") %>%
  mutate_all(as.character)



## ## ## ## ## ## ## ## ## ## ##
# YEARLY IFF BAR CHARTS     ####
## ## ## ## ## ## ## ## ## ## ##

# .. Africa ####
load("Results/Summary data-sets/GER_Year_Africa.Rdata")
load("Results/Summary data-sets/Net_Year_Africa.Rdata")

viz <- full_join(GER_Year_Africa %>% 
                   select(year, 
                          GER_Tot_IFF = Tot_IFF, 
                          GER_Tot_IFF_GDP = Tot_IFF_GDP, 
                          GER_Tot_IFF_Trade = Tot_IFF_trade),
                 Net_Year_Africa %>%
                   select(year, 
                          Net_Tot_IFF = Tot_IFF, 
                          Net_Tot_IFF_GDP = Tot_IFF_GDP, 
                          Net_Tot_IFF_Trade = Tot_IFF_trade),
                 by = c("year"))

# GER and Net dollar value
g <- ggplot(viz %>% 
              mutate(year = as.character(year)) %>% 
              melt(id.vars = "year") %>%
              filter(variable == "Net_Tot_IFF" | variable == "GER_Tot_IFF"), 
            aes(x = year, y = value, fill = fct_rev(variable))) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = dollar_format(scale = 1/10^9, accuracy = 1)) +
  scale_fill_discrete(name = "Estimate", labels = c("Net", "Gross")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Trade mis-invoicing in Africa",
       subtitle = "Net and gross outflows",
       x = "", y = "Illicit flow in billion USD") +
  geom_text(aes(label = round(value/10^9)),
            size = 2.5, position = position_dodge(1), vjust = -0.4) +
  scale_x_discrete(expand = c(0.05, 0.05))
ggsave(g,
       file = "Figures/GER and Net_Yearly_Dollars_Africa.png",
       width = 6, height = 5, units = "in")

# GER and Net as % of GDP
g <- ggplot(viz %>% 
              mutate(year = as.character(year)) %>% 
              melt(id.vars = "year") %>%
              filter(variable == "Net_Tot_IFF_GDP" | variable == "GER_Tot_IFF_GDP"), 
            aes(x = year, y = value, fill = fct_rev(variable))) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_discrete(name = "Estimate", labels = c("Net", "Gross")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Trade mis-invoicing in Africa",
       subtitle = "Net and gross outflows",
       x = "", y = "Illicit flow as % of GDP") +
  geom_text(aes(label = format(round(value*100, 1), nsmall = 1)),
            size = 2.5, position = position_dodge(1), vjust = -0.4) +
  scale_x_discrete(expand = c(0.05, 0.05))
ggsave(g,
       file = "Figures/GER and Net_Yearly_Percent GDP_Africa.png",
       width = 6, height = 5, units = "in")

# GER and Net as % of trade
g <- ggplot(viz %>% 
              mutate(year = as.character(year)) %>% 
              melt(id.vars = "year") %>%
              filter(variable == "Net_Tot_IFF_Trade" | variable == "GER_Tot_IFF_Trade"), 
            aes(x = year, y = value, fill = fct_rev(variable))) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_discrete(name = "Estimate", labels = c("Net", "Gross")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Trade mis-invoicing in Africa",
       subtitle = "Net and gross outflows",
       x = "", y = "Illicit flow as % of trade") +
  geom_text(aes(label = format(round(value*100, 1), nsmall = 1)),
            size = 2.5, position = position_dodge(1), vjust = -0.4) +
  scale_x_discrete(expand = c(0.05, 0.05))
ggsave(g,
       file = "Figures/GER and Net_Yearly_Percent Trade_Africa.png",
       width = 6, height = 5, units = "in")


# .. LMIC ####
load("Results/Summary data-sets/GER_Year_LMIC.Rdata")
load("Results/Summary data-sets/Net_Year_LMIC.Rdata")
load("Results/Summary data-sets/Inflow_GER_Year_LMIC.Rdata")

viz <- full_join(GER_Year_LMIC %>% 
                   select(year, 
                          GER_Tot_IFF = Tot_IFF, 
                          GER_Tot_IFF_GDP = Tot_IFF_GDP, 
                          GER_Tot_IFF_Trade = Tot_IFF_trade),
                 Net_Year_LMIC %>%
                   select(year, 
                          Net_Tot_IFF = Tot_IFF, 
                          Net_Tot_IFF_GDP = Tot_IFF_GDP, 
                          Net_Tot_IFF_Trade = Tot_IFF_trade),
                 by = c("year"))

viz <- full_join(viz, Inflow_GER_Year_LMIC %>%
                   select(year, 
                          In_GER_Tot_IFF = Tot_IFF, 
                          In_GER_Tot_IFF_GDP = Tot_IFF_GDP, 
                          In_GER_Tot_IFF_Trade = Tot_IFF_trade),
                 by = c("year"))

# GER and Net dollar value
g <- ggplot(viz %>% 
              mutate(year = as.character(year)) %>% 
              melt(id.vars = "year") %>%
              filter(variable == "Net_Tot_IFF" | variable == "GER_Tot_IFF"), 
            aes(x = year, y = value, fill = fct_rev(variable))) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = dollar_format(scale = 1/10^9, accuracy = 1)) +
  scale_fill_discrete(name = "Estimate", labels = c("Net", "Gross")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Trade mis-invoicing in low and lower-middle income",
       subtitle = "Net and gross outflows",
       x = "", y = "Illicit flow in billion USD") +
  geom_text(aes(label = round(value/10^9)),
            size = 2.5, position = position_dodge(1), vjust = -0.4) +
  scale_x_discrete(expand = c(0.05, 0.05))
ggsave(g,
       file = "Figures/GER and Net_Yearly_Dollars_LMIC.png",
       width = 6, height = 5, units = "in")

# GER and Net as % of GDP
g <- ggplot(viz %>% 
              mutate(year = as.character(year)) %>% 
              melt(id.vars = "year") %>%
              filter(variable == "Net_Tot_IFF_GDP" | variable == "GER_Tot_IFF_GDP"), 
            aes(x = year, y = value, fill = fct_rev(variable))) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_discrete(name = "Estimate", labels = c("Net", "Gross")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Trade mis-invoicing in low and lower-middle income",
       subtitle = "Net and gross outflows",
       x = "", y = "Illicit flow as % of GDP") +
  geom_text(aes(label = format(round(value*100, 1), nsmall = 1)),
            size = 2.5, position = position_dodge(1), vjust = -0.4) +
  scale_x_discrete(expand = c(0.05, 0.05))
ggsave(g,
       file = "Figures/GER and Net_Yearly_Percent GDP_LMIC.png",
       width = 6, height = 5, units = "in")

# GER and Net as % of trade
g <- ggplot(viz %>% 
              mutate(year = as.character(year)) %>% 
              melt(id.vars = "year") %>%
              filter(variable == "Net_Tot_IFF_Trade" | variable == "GER_Tot_IFF_Trade"), 
            aes(x = year, y = value, fill = fct_rev(variable))) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_discrete(name = "Estimate", labels = c("Net", "Gross")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Trade mis-invoicing in low and lower-middle income",
       subtitle = "Net and gross outflows",
       x = "", y = "Illicit flow as % of trade") +
  geom_text(aes(label = format(round(value*100, 1), nsmall = 1)),
            size = 2.5, position = position_dodge(1), vjust = -0.4) +
  scale_x_discrete(expand = c(0.05, 0.05))
ggsave(g,
       file = "Figures/GER and Net_Yearly_Percent Trade_LMIC.png",
       width = 6, height = 5, units = "in")

# GER In, GER Out and Net dollar value
g <- ggplot(viz %>% 
              mutate(year = as.character(year)) %>% 
              melt(id.vars = "year") %>%
              filter(variable == "Net_Tot_IFF" | 
                       variable == "GER_Tot_IFF" |
                       variable == "In_GER_Tot_IFF"), 
            aes(x = year, y = value, fill = fct_relevel(variable,
                                                        "GER_Tot_IFF",
                                                        "In_GER_Tot_IFF",
                                                        "Net_Tot_IFF"))) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = dollar_format(scale = 1/10^9, accuracy = 1)) +
  scale_fill_brewer(name = "Estimate", 
                    labels = c("Gross outflows", "Gross inflows", "Net flows"),
                    type = "qual", palette = "Set2", direction = -1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Trade mis-invoicing in low and lower-middle income",
       subtitle = "Net and gross outflows",
       x = "", y = "Illicit flow in billion USD") +
  geom_text(aes(label = round(value/10^9)),
            size = 2.5, position = position_dodge(1), vjust = -0.4) +
  scale_x_discrete(expand = c(0.05, 0.05))
ggsave(g,
       file = "Figures/GER Out, In and Net_Yearly_Dollars_LMIC.png",
       width = 6, height = 5, units = "in")


# .. Developing ####
load("Results/Summary data-sets/GER_Year_Developing.Rdata")
load("Results/Summary data-sets/Net_Year_Developing.Rdata")

viz <- full_join(GER_Year_Developing %>% 
                   select(year, 
                          GER_Tot_IFF = Tot_IFF, 
                          GER_Tot_IFF_GDP = Tot_IFF_GDP, 
                          GER_Tot_IFF_Trade = Tot_IFF_trade),
                 Net_Year_Developing %>%
                   select(year, 
                          Net_Tot_IFF = Tot_IFF, 
                          Net_Tot_IFF_GDP = Tot_IFF_GDP, 
                          Net_Tot_IFF_Trade = Tot_IFF_trade),
                 by = c("year"))

# GER and Net dollar value
g <- ggplot(viz %>% 
              mutate(year = as.character(year)) %>% 
              melt(id.vars = "year") %>%
              filter(variable == "Net_Tot_IFF" | variable == "GER_Tot_IFF"), 
            aes(x = year, y = value, fill = fct_rev(variable))) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = dollar_format(scale = 1/10^9, accuracy = 1)) +
  scale_fill_discrete(name = "Estimate", labels = c("Net", "Gross")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Trade mis-invoicing in developing countries",
       subtitle = "Net and gross outflows",
       x = "", y = "Illicit flow in billion USD") +
  geom_text(aes(label = round(value/10^9)),
            size = 2.5, position = position_dodge(1), vjust = -0.4) +
  scale_x_discrete(expand = c(0.05, 0.05))
ggsave(g,
       file = "Figures/GER and Net_Yearly_Dollars_Developing.png",
       width = 6, height = 5, units = "in")

# GER and Net as % of GDP
g <- ggplot(viz %>% 
              mutate(year = as.character(year)) %>% 
              melt(id.vars = "year") %>%
              filter(variable == "Net_Tot_IFF_GDP" | variable == "GER_Tot_IFF_GDP"), 
            aes(x = year, y = value, fill = fct_rev(variable))) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_discrete(name = "Estimate", labels = c("Net", "Gross")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Trade mis-invoicing in developing countries",
       subtitle = "Net and gross outflows",
       x = "", y = "Illicit flow as % of GDP") +
  geom_text(aes(label = format(round(value*100, 1), nsmall = 1)),
            size = 2.5, position = position_dodge(1), vjust = -0.4) +
  scale_x_discrete(expand = c(0.05, 0.05))
ggsave(g,
       file = "Figures/GER and Net_Yearly_Percent GDP_Developing.png",
       width = 6, height = 5, units = "in")

# GER and Net as % of trade
g <- ggplot(viz %>% 
              mutate(year = as.character(year)) %>% 
              melt(id.vars = "year") %>%
              filter(variable == "Net_Tot_IFF_Trade" | variable == "GER_Tot_IFF_Trade"), 
            aes(x = year, y = value, fill = fct_rev(variable))) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_discrete(name = "Estimate", labels = c("Net", "Gross")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Trade mis-invoicing in developing countries",
       subtitle = "Net and gross outflows",
       x = "", y = "Illicit flow as % of trade") +
  geom_text(aes(label = format(round(value*100, 1), nsmall = 1)),
            size = 2.5, position = position_dodge(1), vjust = -0.4) +
  scale_x_discrete(expand = c(0.05, 0.05))
ggsave(g,
       file = "Figures/GER and Net_Yearly_Percent Trade_Developing.png",
       width = 6, height = 5, units = "in")


# .. Low-HDI ####
load("Results/Summary data-sets/GER_Year_LowHDI.Rdata")
load("Results/Summary data-sets/Net_Year_LowHDI.Rdata")

viz <- full_join(GER_Year_LowHDI %>% 
                   select(year, 
                          GER_Tot_IFF = Tot_IFF, 
                          GER_Tot_IFF_GDP = Tot_IFF_GDP, 
                          GER_Tot_IFF_Trade = Tot_IFF_trade),
                 Net_Year_LowHDI %>%
                   select(year, 
                          Net_Tot_IFF = Tot_IFF, 
                          Net_Tot_IFF_GDP = Tot_IFF_GDP, 
                          Net_Tot_IFF_Trade = Tot_IFF_trade),
                 by = c("year"))

# GER and Net dollar value
g <- ggplot(viz %>% 
              mutate(year = as.character(year)) %>% 
              melt(id.vars = "year") %>%
              filter(variable == "Net_Tot_IFF" | variable == "GER_Tot_IFF"), 
            aes(x = year, y = value, fill = fct_rev(variable))) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = dollar_format(scale = 1/10^9, accuracy = 1)) +
  scale_fill_discrete(name = "Estimate", labels = c("Net", "Gross")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Trade mis-invoicing in low HDI countries",
       subtitle = "Net and gross outflows",
       x = "", y = "Illicit flow in billion USD") +
  geom_text(aes(label = round(value/10^9)),
            size = 2.5, position = position_dodge(1), vjust = -0.4) +
  scale_x_discrete(expand = c(0.05, 0.05))
ggsave(g,
       file = "Figures/GER and Net_Yearly_Dollars_Low HDI.png",
       width = 6, height = 5, units = "in")

# GER and Net as % of GDP
g <- ggplot(viz %>% 
              mutate(year = as.character(year)) %>% 
              melt(id.vars = "year") %>%
              filter(variable == "Net_Tot_IFF_GDP" | variable == "GER_Tot_IFF_GDP"), 
            aes(x = year, y = value, fill = fct_rev(variable))) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_discrete(name = "Estimate", labels = c("Net", "Gross")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Trade mis-invoicing in low HDI countries",
       subtitle = "Net and gross outflows",
       x = "", y = "Illicit flow as % of GDP") +
  geom_text(aes(label = format(round(value*100, 1), nsmall = 1)),
            size = 2.5, position = position_dodge(1), vjust = -0.4) +
  scale_x_discrete(expand = c(0.05, 0.05))
ggsave(g,
       file = "Figures/GER and Net_Yearly_Percent GDP_Low HDI.png",
       width = 6, height = 5, units = "in")

# GER and Net as % of trade
g <- ggplot(viz %>% 
              mutate(year = as.character(year)) %>% 
              melt(id.vars = "year") %>%
              filter(variable == "Net_Tot_IFF_Trade" | variable == "GER_Tot_IFF_Trade"), 
            aes(x = year, y = value, fill = fct_rev(variable))) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_discrete(name = "Estimate", labels = c("Net", "Gross")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Trade mis-invoicing in low HDI countries",
       subtitle = "Net and gross outflows",
       x = "", y = "Illicit flow as % of trade") +
  geom_text(aes(label = format(round(value*100, 1), nsmall = 1)),
            size = 2.5, position = position_dodge(1), vjust = -0.4) +
  scale_x_discrete(expand = c(0.05, 0.05))
ggsave(g,
       file = "Figures/GER and Net_Yearly_Percent Trade_Low HDI.png",
       width = 6, height = 5, units = "in")


# .. World ####
load("Results/Summary data-sets/GER_Year.Rdata")
load("Results/Summary data-sets/Inflow_GER_Year.Rdata")
load("Results/Summary data-sets/Net_Year.Rdata")

viz <- full_join(GER_Year %>% 
                   select(year, 
                          GER_Tot_IFF = Tot_IFF, 
                          GER_Tot_IFF_GDP = Tot_IFF_GDP, 
                          GER_Tot_IFF_Trade = Tot_IFF_trade,
                          GER_Imp_IFF = Imp_IFF,
                          GER_Exp_IFF = Exp_IFF),
                 Net_Year %>%
                   select(year, 
                          Net_Tot_IFF = Tot_IFF, 
                          Net_Tot_IFF_GDP = Tot_IFF_GDP, 
                          Net_Tot_IFF_Trade = Tot_IFF_trade,
                          Net_Imp_IFF = Imp_IFF,
                          Net_Exp_IFF = Exp_IFF),
                 by = c("year"))

viz <- full_join(viz, Inflow_GER_Year %>%
                   select(year, 
                          In_GER_Tot_IFF = Tot_IFF, 
                          In_GER_Tot_IFF_GDP = Tot_IFF_GDP, 
                          In_GER_Tot_IFF_Trade = Tot_IFF_trade,
                          In_GER_Imp_IFF = Imp_IFF,
                          In_GER_Exp_IFF = Exp_IFF),
                 by = c("year"))

# GER and Net dollar value
g <- ggplot(viz %>% 
              mutate(year = as.character(year)) %>% 
              melt(id.vars = "year") %>%
              filter(variable == "Net_Tot_IFF" | variable == "GER_Tot_IFF"), 
            aes(x = year, y = value, fill = fct_rev(variable))) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = dollar_format(scale = 1/10^9, accuracy = 1)) +
  scale_fill_discrete(name = "Estimate", labels = c("Net", "Gross")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Trade mis-invoicing globally",
       subtitle = "Net and gross outflows",
       x = "", y = "Illicit flow in billion USD") +
  geom_text(aes(label = round(value/10^9)),
            size = 2.5, position = position_dodge(1), vjust = -0.4) +
  scale_x_discrete(expand = c(0.05, 0.05))
ggsave(g,
       file = "Figures/GER and Net_Yearly_Dollars_World.png",
       width = 6, height = 5, units = "in")

# Imports and Exports, GER and Net, dollar value in 2005
g <- ggplot(viz %>% 
              mutate(year = as.character(year)) %>% 
              melt(id.vars = "year") %>%
              filter(variable == "GER_Imp_IFF" | variable == "GER_Exp_IFF" |
                       variable == "In_GER_Imp_IFF" | variable == "In_GER_Exp_IFF" |
                       variable == "Net_Imp_IFF" | variable == "Net_Exp_IFF") %>%
              mutate(flowtype = ifelse(str_detect(variable, "Imp"), 
                                       "Imports", "Exports")) %>%
              mutate(flowtype = fct_rev(flowtype)) %>%
              filter(year == 2005), 
            aes(x = year, y = value, fill = fct_relevel(variable,
                                                        "GER_Imp_IFF",
                                                        "In_GER_Imp_IFF",
                                                        "Net_Imp_IFF",
                                                        "GER_Exp_IFF",
                                                        "In_GER_Exp_IFF",
                                                        "Net_Exp_IFF"))) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = dollar_format(scale = 1/10^9, accuracy = 1)) +
  scale_fill_brewer(name = "Estimate",
                    labels = c("Outflows in Imports",
                               "Inflows in Imports",
                               "Net flows in Imports",
                               "Outflows in Exports",
                               "Inflows in Exports",
                               "Net flows in Exports"),
                    type = "qual", palette = "Set2") +
  facet_wrap(~flowtype) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Trade mis-invoicing globally in 2005",
       subtitle = "Net and gross flows in imports and exports",
       x = "", y = "Illicit flow in billion USD") +
  geom_text(aes(label = round(value/10^9)),
            size = 2.5, position = position_dodge(1), vjust = -0.4) +
  scale_x_discrete(expand = c(0.05, 0.05))
ggsave(g,
       file = "Figures/GER and Net Imports, Exports_2005_Dollars_World.png",
       width = 6, height = 5, units = "in")

# Imports and Exports, GER and Net, dollar value, sum the country averages across the years
load("Results/Summary data-sets/GER_Orig_Avg.Rdata")
load("Results/Summary data-sets/Inflow_GER_Orig_Avg.Rdata")
load("Results/Summary data-sets/Net_Orig_Avg.Rdata")

GER_Avg <- GER_Orig_Avg %>%
  summarize(GER_Imp_IFF_Avg = sum(Imp_IFF),
            GER_Exp_IFF_Avg = sum(Exp_IFF))
Inflow_GER_Avg <- Inflow_GER_Orig_Avg %>%
  summarize(In_GER_Imp_IFF_Avg = sum(Imp_IFF),
            In_GER_Exp_IFF_Avg = sum(Exp_IFF))
Net_Avg <- Net_Orig_Avg %>%
  summarize(Net_Imp_IFF_Avg = sum(Imp_IFF),
            Net_Exp_IFF_Avg = sum(Exp_IFF))

viz <- cbind(GER_Avg, Inflow_GER_Avg, Net_Avg)

g <- ggplot(viz %>% 
              melt() %>%
              mutate(flowtype = ifelse(str_detect(variable, "Imp"), 
                                       "Imports", "Exports")) %>%
              mutate(flowtype = fct_rev(flowtype)), 
            aes(x = "", y = value, fill = fct_relevel(variable,
                                                        "GER_Imp_IFF_Avg",
                                                        "In_GER_Imp_IFF_Avg",
                                                        "Net_Imp_IFF_Avg",
                                                        "GER_Exp_IFF_Avg",
                                                        "In_GER_Exp_IFF_Avg",
                                                        "Net_Exp_IFF_Avg"))) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = dollar_format(scale = 1/10^9, accuracy = 1)) +
  scale_fill_brewer(name = "Estimate",
                    labels = c("Outflows in Imports",
                               "Inflows in Imports",
                               "Net flows in Imports",
                               "Outflows in Exports",
                               "Inflows in Exports",
                               "Net flows in Exports"),
                    type = "qual", palette = "Set2") +
  facet_wrap(~flowtype) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Trade mis-invoicing globally (yearly average)",
       subtitle = "Net and gross flows in imports and exports",
       x = "", y = "Illicit flow in billion USD") +
  geom_text(aes(label = round(value/10^9)),
            size = 2.5, position = position_dodge(1), vjust = -0.4) +
  scale_x_discrete(expand = c(0.05, 0.05))
ggsave(g,
       file = "Figures/GER and Net Imports, Exports_Yearly Average_Dollars_World.png",
       width = 6, height = 5, units = "in")

# Instead of summing up the country averages, take the yearly average of the sums
load("Results/Summary data-sets/GER_Year.Rdata")
load("Results/Summary data-sets/Inflow_GER_Year.Rdata")
load("Results/Summary data-sets/Net_Year.Rdata")

GER_Avg <- GER_Year %>%
  summarize(GER_Imp_IFF_Avg = mean(Imp_IFF),
            GER_Exp_IFF_Avg = mean(Exp_IFF))
Inflow_GER_Avg <- Inflow_GER_Year %>%
  summarize(In_GER_Imp_IFF_Avg = mean(Imp_IFF),
            In_GER_Exp_IFF_Avg = mean(Exp_IFF))
Net_Avg <- Net_Year %>%
  summarize(Net_Imp_IFF_Avg = mean(Imp_IFF),
            Net_Exp_IFF_Avg = mean(Exp_IFF))

viz <- cbind(GER_Avg, Inflow_GER_Avg, Net_Avg)

g <- ggplot(viz %>% 
              melt() %>%
              mutate(flowtype = ifelse(str_detect(variable, "Imp"), 
                                       "Imports", "Exports")) %>%
              mutate(flowtype = fct_rev(flowtype)), 
            aes(x = "", y = value, fill = fct_relevel(variable,
                                                      "GER_Imp_IFF_Avg",
                                                      "In_GER_Imp_IFF_Avg",
                                                      "Net_Imp_IFF_Avg",
                                                      "GER_Exp_IFF_Avg",
                                                      "In_GER_Exp_IFF_Avg",
                                                      "Net_Exp_IFF_Avg"))) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = dollar_format(scale = 1/10^9, accuracy = 1)) +
  scale_fill_brewer(name = "Estimate",
                    labels = c("Outflows in Imports",
                               "Inflows in Imports",
                               "Net flows in Imports",
                               "Outflows in Exports",
                               "Inflows in Exports",
                               "Net flows in Exports"),
                    type = "qual", palette = "Set2") +
  facet_wrap(~flowtype) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Trade mis-invoicing globally (yearly average)",
       subtitle = "Net and gross flows in imports and exports",
       x = "", y = "Illicit flow in billion USD") +
  geom_text(aes(label = round(value/10^9)),
            size = 2.5, position = position_dodge(1), vjust = -0.4) +
  scale_x_discrete(expand = c(0.05, 0.05))
ggsave(g,
       file = "Figures/GER and Net Imports, Exports_Yearly Average2_Dollars_World.png",
       width = 6, height = 5, units = "in")



## ## ## ## ## ## ## ## ## ## ##
# LINE CHARTS               ####
## ## ## ## ## ## ## ## ## ## ##

load("Results/Summary data-sets/GER_Orig_Avg.Rdata")
labels.grne <- c(Net_Tot_IFF = "Net estimates",
                 GER_Tot_IFF = "Gross estimates", 
                 Net_Tot_IFF_GDP = "Net estimates",
                 GER_Tot_IFF_GDP = "Gross estimates")


# .. Line chart for conduits in Africa ####
load("Results/Summary data-sets/GER_Orig_Year_Africa.Rdata")
load("Results/Summary data-sets/Net_Orig_Year_Africa.Rdata")

conduits <- GER_Orig_Avg %>%
  filter(rRegion == "Africa") %>%
  arrange(desc(Tot_IFF_GDP)) %>%
  head(5) %>%
  select(reporter.ISO) %>%
  pull

Conduits_Gross <- GER_Orig_Year_Africa %>%
  filter(reporter.ISO %in% conduits)
Conduits_Net <- Net_Orig_Year_Africa %>%
  filter(reporter.ISO %in% conduits)

Conduits_Year <- full_join(Conduits_Gross %>%
                             rename_at(vars(starts_with("Tot")), funs(paste0("GER_", .))), 
                           Conduits_Net %>%
                             rename_at(vars(starts_with("Tot")), funs(paste0("Net_", .))),
                           by = c("reporter" = "reporter",
                                  "reporter.ISO" = "reporter.ISO",
                                  "year" = "year"))

g <- ggplot(Conduits_Year %>% 
              melt(id.vars = c("year", "reporter")) %>%
              filter(variable == "GER_Tot_IFF_GDP" | variable == "Net_Tot_IFF_GDP") %>%
              mutate(
                value = as.numeric(value)
              )) +
  geom_line(aes(x = year, y = value*100, color = reporter)) +
  scale_color_viridis_d(option = "inferno") +
  facet_wrap(~variable, labeller = labeller(variable = labels.grne)) +
  labs(title = "Yearly flows in conduit countries",
       x = "Year",
       y = "Illicit flow as % of GDP",
       color = "")
ggsave(g,
       file = "Figures/Conduits_GER and Net_Yearly Lines_Percent GDP_Africa.png",
       width = 6, height = 5, units = "in")


# .. Line chart for conduits in LMIC ####
load("Results/Summary data-sets/GER_Orig_Year_LMIC.Rdata")
load("Results/Summary data-sets/Net_Orig_Year_LMIC.Rdata")

conduits <- GER_Orig_Avg %>%
  filter(rIncome == "LIC" | rIncome == "LMC") %>%
  arrange(desc(Tot_IFF_GDP)) %>%
  head(5) %>%
  select(reporter.ISO) %>%
  pull

Conduits_Gross <- GER_Orig_Year_LMIC %>%
  filter(reporter.ISO %in% conduits)
Conduits_Net <- Net_Orig_Year_LMIC %>%
  filter(reporter.ISO %in% conduits)

Conduits_Year <- full_join(Conduits_Gross %>%
                             rename_at(vars(starts_with("Tot")), funs(paste0("GER_", .))), 
                           Conduits_Net %>%
                             rename_at(vars(starts_with("Tot")), funs(paste0("Net_", .))),
                           by = c("reporter" = "reporter",
                                  "reporter.ISO" = "reporter.ISO",
                                  "year" = "year"))

g <- ggplot(Conduits_Year %>% 
              melt(id.vars = c("year", "reporter")) %>%
              filter(variable == "GER_Tot_IFF_GDP" | variable == "Net_Tot_IFF_GDP") %>%
              mutate(
                value = as.numeric(value)
              )) +
  geom_line(aes(x = year, y = value*100, color = reporter)) +
  scale_color_viridis_d(option = "inferno") +
  facet_wrap(~variable, labeller = labeller(variable = labels.grne)) +
  labs(title = "Yearly flows in conduit countries",
       x = "Year",
       y = "Illicit flow as % of GDP",
       color = "")
ggsave(g,
       file = "Figures/Conduits_GER and Net_Yearly Lines_Percent GDP_LMIC.png",
       width = 6, height = 5, units = "in")


# .. Line chart for conduits in Developing ####
load("Results/Summary data-sets/GER_Orig_Year_Developing.Rdata")
load("Results/Summary data-sets/Net_Orig_Year_Developing.Rdata")

conduits <- GER_Orig_Avg %>%
  filter(rDev == "Developing") %>%
  arrange(desc(Tot_IFF_GDP)) %>%
  head(5) %>%
  select(reporter.ISO) %>%
  pull

Conduits_Gross <- GER_Orig_Year_Developing %>%
  filter(reporter.ISO %in% conduits)
Conduits_Net <- Net_Orig_Year_Developing %>%
  filter(reporter.ISO %in% conduits)

Conduits_Year <- full_join(Conduits_Gross %>%
                             rename_at(vars(starts_with("Tot")), funs(paste0("GER_", .))), 
                           Conduits_Net %>%
                             rename_at(vars(starts_with("Tot")), funs(paste0("Net_", .))),
                           by = c("reporter" = "reporter",
                                  "reporter.ISO" = "reporter.ISO",
                                  "year" = "year"))

g <- ggplot(Conduits_Year %>% 
              melt(id.vars = c("year", "reporter")) %>%
              filter(variable == "GER_Tot_IFF_GDP" | variable == "Net_Tot_IFF_GDP") %>%
              mutate(
                value = as.numeric(value)
              )) +
  geom_line(aes(x = year, y = value*100, color = reporter)) +
  scale_color_viridis_d(option = "inferno") +
  facet_wrap(~variable, labeller = labeller(variable = labels.grne)) +
  labs(title = "Yearly flows in conduit countries",
       x = "Year",
       y = "Illicit flow as % of GDP",
       color = "")
ggsave(g,
       file = "Figures/Conduits_GER and Net_Yearly Lines_Percent GDP_Developing.png",
       width = 6, height = 5, units = "in")



## ## ## ## ## ## ## ## ## ## ##
# CHOROPLETH MAPS           ####
## ## ## ## ## ## ## ## ## ## ##

# .. Africa ####
map <- map_data("world")
map <- left_join(map, codes %>% dplyr::select(Country, ISO3166.3, UN_Region),
                 by = c("region" = "Country")) %>%
  dplyr::select(-subregion) %>%
  filter(UN_Region == "Africa")

ditch_axes <- theme(axis.title.x = element_blank(),
                    axis.text.x = element_blank(),
                    axis.ticks.x = element_blank(),
                    axis.title.y = element_blank(),
                    axis.text.y = element_blank(),
                    axis.ticks.y = element_blank(),
                    panel.border = element_blank(),
                    panel.grid = element_blank())

# Average gross IFF dollar value
load("Results/Summary data-sets/GER_Orig_Avg_Africa.Rdata")

viz <- left_join(map, GER_Orig_Avg_Africa,
                 by = c("ISO3166.3" = "reporter.ISO"))

g <- ggplot() +
  geom_polygon(data = viz,
               aes(x = long, y = lat, group = group,
                   fill = Tot_IFF_bn), color = "white", lwd = 0.2) +
  coord_fixed(1.3) +
  theme_bw() +
  ditch_axes +
  scale_fill_viridis_c("IFF (billion USD)", direction = -1) +
  labs(title = "Total gross outflows averaged over 2000-2018") +
  theme(legend.position = "bottom",
        legend.margin = margin(t = -2, r = 0, b = 0, l = 0, unit = "cm")) +
  guides(fill = guide_colourbar(title.vjust = 0.8))
ggsave(g,
       file = "Figures/Choro_GER Out_Yearly Average_Dollars_Africa.png",
       width = 6, height = 5, units = "in")

# Average gross IFF as % of GDP
g <- ggplot() + 
  geom_polygon(data = viz,
               aes(x = long, y = lat, group = group, 
                   fill = Tot_IFF_GDP*100), color = "white", lwd = 0.2) + 
  coord_fixed(1.3) +
  theme_bw() + 
  ditch_axes +
  scale_fill_viridis_c("IFF (% GDP)", direction = -1) +
  labs(title = "Total gross outflows averaged over 2000-2018") +
  theme(legend.position = "bottom",
        legend.margin = margin(t = -2, r = 0, b = 0, l = 0, unit = "cm")) + 
  guides(fill = guide_colourbar(title.vjust = 0.8))
ggsave(g,
       file = "Figures/Choro_GER Out_Yearly Average_Percent GDP_Africa.png",
       width = 6, height = 5, units = "in")

# Average gross IFF as % of trade
g <- ggplot() + 
  geom_polygon(data = viz,
               aes(x = long, y = lat, group = group, 
                   fill = Tot_IFF_trade*100), color = "white", lwd = 0.2) + 
  coord_fixed(1.3) +
  theme_bw() + 
  ditch_axes +
  scale_fill_viridis_c("IFF (% trade)", direction = -1) +
  labs(title = "Total gross outflows averaged over 2000-2018") +
  theme(legend.position = "bottom",
        legend.margin = margin(t = -2, r = 0, b = 0, l = 0, unit = "cm")) + 
  guides(fill = guide_colourbar(title.vjust = 0.8))
ggsave(g,
       file = "Figures/Choro_GER Out_Yearly Average_Percent Trade_Africa.png",
       width = 6, height = 5, units = "in")

# Average net IFF dollar value
load("Results/Summary data-sets/Net_Orig_Avg_Africa.Rdata")

viz <- left_join(map, Net_Orig_Avg_Africa,
                 by = c("ISO3166.3" = "reporter.ISO"))

g <- ggplot() + 
  geom_polygon(data = viz,
               aes(x = long, y = lat, group = group, 
                   fill = Tot_IFF_bn), color = "white", lwd = 0.2) + 
  coord_fixed(1.3) +
  theme_bw() + 
  ditch_axes +
  scale_fill_distiller(name = "IFF (billion USD)", 
                       type = "div", palette = "Spectral") +
  labs(title = "Total flows averaged over 2000-2018",
       subtitle = "Net flows")
ggsave(g,
       file = "Figures/Choro_Net_Yearly Average_Dollars_Africa.png",
       width = 6, height = 5, units = "in")


# .. World ####
map <- map_data("world")
map <- left_join(map, codes %>% dplyr::select(Country, ISO3166.3),
                 by = c("region" = "Country")) %>%
  dplyr::select(-subregion) %>%
  filter(region != "Antarctica")

ditch_axes <- theme(axis.title.x = element_blank(),
                    axis.text.x = element_blank(),
                    axis.ticks.x = element_blank(),
                    axis.title.y = element_blank(),
                    axis.text.y = element_blank(),
                    axis.ticks.y = element_blank(),
                    panel.border = element_blank(),
                    panel.grid = element_blank())

# Average gross IFF dollar value
load("Results/Summary data-sets/GER_Orig_Avg.Rdata")

viz <- left_join(map, GER_Orig_Avg,
                 by = c("ISO3166.3" = "reporter.ISO"))

g <- ggplot() + 
  geom_polygon(data = viz,
               aes(x = long, y = lat, group = group, 
                   fill = Tot_IFF_bn), color = "white", lwd = 0.2) + 
  coord_fixed(1.3) +
  theme_bw() + 
  ditch_axes +
  scale_fill_viridis_c("IFF (billion USD)", direction = -1,
                       breaks = c(50, 150)) +
  labs(title = "Total gross outflows averaged over 2000-2018") +
  theme(legend.position = "bottom") + 
  guides(fill = guide_colourbar(title.vjust = 0.8))
ggsave(g,
       file = "Figures/Choro_GER Out_Yearly Average_Dollars_World.png",
       width = 6, height = 5, units = "in")

# Average gross IFF as % of GDP
g <- ggplot() + 
  geom_polygon(data = viz,
               aes(x = long, y = lat, group = group, 
                   fill = Tot_IFF_GDP*100), color = "white", lwd = 0.2) + 
  coord_fixed(1.3) +
  theme_bw() + 
  ditch_axes +
  scale_fill_viridis_c("IFF (% GDP)", direction = -1) +
  labs(title = "Total gross outflows averaged over 2000-2018") +
  theme(legend.position = "bottom") + 
  guides(fill = guide_colourbar(title.vjust = 0.8))
ggsave(g,
       file = "Figures/Choro_GER Out_Yearly Average_Percent GDP_World.png",
       width = 6, height = 5, units = "in")

# Average gross IFF as % of trade
g <- ggplot() + 
  geom_polygon(data = viz,
               aes(x = long, y = lat, group = group, 
                   fill = Tot_IFF_trade*100), color = "white", lwd = 0.2) + 
  coord_fixed(1.3) +
  theme_bw() + 
  ditch_axes +
  scale_fill_viridis_c("IFF (% trade)", direction = -1) +
  labs(title = "Total gross outflows averaged over 2000-2018") +
  theme(legend.position = "bottom") + 
  guides(fill = guide_colourbar(title.vjust = 0.8))
ggsave(g,
       file = "Figures/Choro_GER Out_Yearly Average_Percent Trade_World.png",
       width = 6, height = 5, units = "in")

# Average net IFF dollar value
load("Results/Summary data-sets/Net_Orig_Avg.Rdata")

viz <- left_join(map, Net_Orig_Avg,
                 by = c("ISO3166.3" = "reporter.ISO"))

g <- ggplot() + 
  geom_polygon(data = viz,
               aes(x = long, y = lat, group = group, 
                   fill = Tot_IFF_bn), color = "white", lwd = 0.2) + 
  coord_fixed(1.3) +
  theme_bw() + 
  ditch_axes +
  scale_fill_distiller(name = "IFF (billion USD)", 
                       type = "div", palette = "Spectral") +
  labs(title = "Total outflows averaged over 2000-2018",
       subtitle = "Net flows") +
  theme(legend.position = "bottom") + 
  guides(fill = guide_colourbar(title.vjust = 0.8))
ggsave(g,
       file = "Figures/Choro_Net_Yearly Average_Dollars_World.png",
       width = 6, height = 5, units = "in")

# Average gross inflow IFF dollar value
load("Results/Summary data-sets/Inflow_GER_Orig_Avg.Rdata")

viz <- left_join(map, Inflow_GER_Orig_Avg,
                 by = c("ISO3166.3" = "reporter.ISO"))

g <- ggplot() + 
  geom_polygon(data = viz,
               aes(x = long, y = lat, group = group, 
                   fill = Tot_IFF_bn), color = "white", lwd = 0.2) + 
  coord_fixed(1.3) +
  theme_bw() + 
  ditch_axes +
  scale_fill_viridis_c("IFF (billion USD)", option = "C") +
  labs(title = "Total gross inflows averaged over 2000-2018") +
  theme(legend.position = "bottom") + 
  guides(fill = guide_colourbar(title.vjust = 0.8))
ggsave(g,
       file = "Figures/Choro_GER In_Yearly Average_Dollars_World.png",
       width = 6, height = 5, units = "in")

# Average net inflow IFF dollar value
load("Results/Summary data-sets/Net_Orig_Avg.Rdata")

viz <- left_join(map, Net_Orig_Avg %>%
                   filter(Tot_IFF_bn < 0) %>%
                   mutate(Tot_IFF_bn = abs(Tot_IFF_bn)),
                 by = c("ISO3166.3" = "reporter.ISO"))

g <- ggplot() + 
  geom_polygon(data = viz,
               aes(x = long, y = lat, group = group, 
                   fill = Tot_IFF_bn), color = "white", lwd = 0.2) + 
  coord_fixed(1.3) +
  theme_bw() + 
  ditch_axes +
  scale_fill_viridis_c("IFF (billion USD)", option = "C", direction = -1) +
  labs(title = "Total net inflows averaged over 2000-2018") +
  theme(legend.position = "bottom") + 
  guides(fill = guide_colourbar(title.vjust = 0.8))
ggsave(g,
       file = "Figures/Choro_Net In_Yearly Average_Dollars_World.png",
       width = 6, height = 5, units = "in")



## ## ## ## ## ## ## ## ## ## ##
# PIE AND DOUGHNUT CHARTS   ####
## ## ## ## ## ## ## ## ## ## ##

# .. Africa ####
# Origins pie chart, cumulative IFF
load("Results/Summary data-sets/GER_Orig_Sum_Africa.Rdata")

Origins <- left_join(GER_Orig_Sum_Africa, codes %>% 
                       select(ISO3166.3, `UN_Sub-region`, UN_Intermediate_Region) %>%
                       distinct(ISO3166.3, .keep_all = TRUE),
                     by = c("reporter.ISO" = "ISO3166.3")) %>%
  mutate(Region = ifelse(is.na(UN_Intermediate_Region), `UN_Sub-region`, UN_Intermediate_Region),
         Region = ifelse(Region == "Middle Africa", "Central Africa", Region)) %>%
  group_by(Region) %>%
  summarize(Tot_IFF = sum(Tot_IFF, na.rm = T)) %>%
  ungroup() %>%
  mutate(Pct_IFF = Tot_IFF / sum(Tot_IFF) * 100)

g <- ggplot(Origins,
            aes(x = "", y = Pct_IFF, fill = Region)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  geom_text(aes(label = paste0(round(Pct_IFF), "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Origins of African outflows, 2000-2018") +
  theme_classic() +
  scale_fill_brewer(type = "qual", palette = "Pastel1") +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave(g,
       file = "Figures/Pie_GER Origins_Cumulative_Africa.png",
       width = 6, height = 5, units = "in")

# Destinations pie chart, cumulative IFF
load("Results/Summary data-sets/GER_Dest_Sum_Africa.Rdata")

Destinations <- GER_Dest_Sum_Africa %>%
  filter(pRegion != "") %>%
  group_by(pRegion) %>%
  summarize(Tot_IFF = sum(Tot_IFF, na.rm = T)) %>%
  ungroup() %>%
  mutate(Pct_IFF = Tot_IFF / sum(Tot_IFF) * 100)

g <- ggplot(Destinations,
            aes(x = "", y = Pct_IFF, fill = pRegion)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  geom_text(aes(label = ifelse(Pct_IFF < 3, round(Pct_IFF), paste0(round(Pct_IFF), "%"))), 
            position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Destinations of African outflows, 2000-2018") +
  theme_classic() +
  scale_fill_brewer(type = "qual", palette = "Pastel1") +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave(g,
       file = "Figures/Pie_GER Destinations_Cumulative_Africa.png",
       width = 6, height = 5, units = "in")


# .. World ####
# Origins pie chart, cumulative IFF
load("Results/Summary data-sets/GER_Orig_Sum.Rdata")

Origins <- GER_Orig_Sum %>%
  filter(rRegion != "") %>%
  group_by(rRegion) %>%
  summarize(Tot_IFF = sum(Tot_IFF, na.rm = T)) %>%
  ungroup() %>%
  mutate(Pct_IFF = Tot_IFF / sum(Tot_IFF) * 100)

g <- ggplot(Origins,
            aes(x = "", y = Pct_IFF, fill = rRegion)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  geom_text(aes(label = ifelse(Pct_IFF < 3, round(Pct_IFF), paste0(round(Pct_IFF), "%"))), 
            position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Origins of global outflows, 2000-2018") +
  theme_classic() +
  scale_fill_brewer(type = "qual", palette = "Pastel1") +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave(g,
       file = "Figures/Pie_GER Origins_Cumulative_World.png",
       width = 6, height = 5, units = "in")

# Destinations pie chart, cumulative IFF
load("Results/Summary data-sets/GER_Dest_Sum.Rdata")

Destinations <- GER_Dest_Sum %>%
  filter(pRegion != "") %>%
  group_by(pRegion) %>%
  summarize(Tot_IFF = sum(Tot_IFF, na.rm = T)) %>%
  ungroup() %>%
  mutate(Pct_IFF = Tot_IFF / sum(Tot_IFF) * 100)

g <- ggplot(Destinations,
            aes(x = "", y = Pct_IFF, fill = pRegion)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  geom_text(aes(label = ifelse(Pct_IFF < 4, round(Pct_IFF), paste0(round(Pct_IFF), "%"))), 
            position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Destinations of global outflows, 2000-2018") +
  theme_classic() +
  scale_fill_brewer(type = "qual", palette = "Pastel1") +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave(g,
       file = "Figures/Pie_GER Destinations_Cumulative_World.png",
       width = 6, height = 5, units = "in")


# .. Sunburst charts for World ####
# Origins, cumulative IFF
load("Results/Summary data-sets/GER_Orig_Sum.Rdata")

Origins <- GER_Orig_Sum %>%
  filter(rRegion != "" & rIncome != "") %>%
  group_by(rRegion, rIncome) %>%
  summarize(Tot_IFF = sum(Tot_IFF, na.rm = T)) %>%
  ungroup() %>%
  mutate(Pct_IFF = Tot_IFF / sum(Tot_IFF) * 100) %>%
  mutate(rRegion = factor(rRegion),
         parent = rRegion) %>%
  rename(node = rIncome,
         size = Pct_IFF) %>%
  select(-Tot_IFF) %>%
  mutate(node = factor(node, levels = c("LIC", "LMC", "UMC", "HIC"))) %>%
  arrange(parent, node) %>%
  mutate(size = ifelse(rRegion == "Africa" & node == "LIC", 0.51, size),
         size = ifelse(rRegion == "Americas" & node == "LMC", 0.51, size)) # Just for rounding display, everything is correct

write.table(Origins, file = "Figures/Origins.csv", row.names = F, sep = ",")
sb <- sunburst_data("Figures/Origins.csv", sep = ",", type = "node_parent",
                    node_attributes = c("rRegion", "size"))
sb$rects[!sb$rects$leaf, ]$rRegion <- sb$rects[!sb$rects$leaf, ]$name
sb$node_labels$size <- Origins %>% group_by(rRegion) %>% summarize(size = sum(size)) %>% select(size) %>% pull

g <- sunburst(sb, rects.fill.aes = "rRegion", leaf_labels = F, node_labels.min = 15) +
  geom_text(data = sb$leaf_labels %>%
              filter(!(label == "HIC" & rRegion == "Africa")) %>%
              filter(!(label == "LMC" & rRegion == "Oceania")),
            aes(x = x, y = 0.1, 
                label = paste0(label, " ", round(size), "%"),
                angle = angle, hjust = hjust), size = 2.5) +
  geom_text(data = sb$node_labels,
            aes(x = x, y = y,
                label = ifelse(label != "Oceania", paste0(round(size), "%"), round(size))),
            size = 3) +
  scale_fill_brewer(type = "qual", palette = "Set3") +
  labs(title = "Top origins",
       subtitle = "Cumulative gross outflows during 2000-2018",
       fill = "") +
  theme(plot.margin = unit(c(0.5, 0, 0, 0), "cm"))
ggsave(g,
       file = "Figures/Sunburst_GER Origins_Cumulative_World.png",
       width = 6, height = 5, units = "in")

# Destinations, cumulative IFF
load("Results/Summary data-sets/GER_Dest_Sum.Rdata")

Destinations <- GER_Dest_Sum %>%
  filter(pRegion != "" & pIncome != "") %>%
  group_by(pRegion, pIncome) %>%
  summarize(Tot_IFF = sum(Tot_IFF, na.rm = T)) %>%
  ungroup() %>%
  mutate(Pct_IFF = Tot_IFF / sum(Tot_IFF) * 100) %>%
  mutate(pRegion = factor(pRegion),
         parent = pRegion) %>%
  rename(node = pIncome,
         size = Pct_IFF) %>%
  select(-Tot_IFF) %>%
  mutate(node = factor(node, levels = c("LIC", "LMC", "UMC", "HIC"))) %>%
  arrange(parent, node) %>%
  mutate(size = ifelse(pRegion == "Americas" & node == "LMC", 0.51, size),
         size = ifelse(pRegion == "Europe" & node == "LMC", 0.51, size),
         size = ifelse(pRegion == "Africa" & node == "LIC", 0.51, size),
         size = ifelse(pRegion == "Asia" & node == "LIC", 0.51, size)) # Just for rounding display, everything is correct

write.table(Destinations, file = "Figures/Destinations.csv", row.names = F, sep = ",")
sb <- sunburst_data("Figures/Destinations.csv", sep = ",", type = "node_parent",
                    node_attributes = c("pRegion", "size"))
sb$rects[!sb$rects$leaf, ]$pRegion <- sb$rects[!sb$rects$leaf, ]$name
sb$node_labels$size <- Destinations %>% group_by(pRegion) %>% summarize(size = sum(size)) %>% select(size) %>% pull

g <- sunburst(sb, rects.fill.aes = "pRegion", leaf_labels = F, node_labels.min = 15) +
  geom_text(data = sb$leaf_labels %>%
              filter(!(label == "HIC" & pRegion == "Africa")) %>%
              filter(!(label == "UMC" & pRegion == "Oceania")),
            aes(x = x, y = 0.1, 
                label = paste0(label, " ", round(size), "%"),
                angle = angle, hjust = hjust), size = 2.5) +
  geom_text(data = sb$node_labels,
            aes(x = x, y = y,
                label = ifelse(label != "Oceania", paste0(round(size), "%"), floor(size))), # Floor is just for rounding display
            size = 3) +
  scale_fill_brewer(type = "qual", palette = "Set3") +
  labs(title = "Top destinations",
       subtitle = "Cumulative gross outflows during 2000-2018",
       fill = "") +
  theme(plot.margin = unit(c(0.5, 0, 0, 0), "cm"))
ggsave(g,
       file = "Figures/Sunburst_GER Destinations_Cumulative_World.png",
       width = 6, height = 5, units = "in")



## ## ## ## ## ## ## ## ## ## ##
# TREEMAP CHARTS            ####
## ## ## ## ## ## ## ## ## ## ##

tol21rainbow <- c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#117744", "#44AA77", "#88CCAA", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788")
gdocs20 <- c("#3366CC", "#DC3912", "#FF9900", "#109618", "#990099", "#0099C6", "#DD4477", "#66AA00", "#B82E2E", "#316395", "#994499", "#22AA99", "#AAAA11", "#6633CC", "#E67300", "#8B0707", "#651067", "#329262", "#5574A6", "#3B3EAC")


# .. Africa, yearly average ####
load("Results/Summary data-sets/GER_Sect_Avg_Africa.Rdata")

g <- ggplot(GER_Sect_Avg_Africa %>%
              arrange(desc(Tot_IFF_bn)),
            aes(area = Tot_IFF_bn, fill = forcats::fct_inorder(section), label = section)) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "topleft", reflow = T) +
  geom_treemap_text(data = GER_Sect_Avg_Africa,
                    aes(label = ifelse(Tot_IFF_bn >= sort(Tot_IFF_bn, decreasing = T)[6],
                                       paste0("$", round(Tot_IFF_bn), " bn"),
                                       "")),
                    colour = "white", place = "bottomright", size = 12) +
  theme(legend.position = "none") +
  scale_fill_manual(values = tol21rainbow) +
  labs(title = "Top sectors in Africa",
       subtitle = "Average gross yearly outflow during 2000-2018")
ggsave(g,
       file = "Figures/Treemap sectors_Yearly Average_Africa.png",
       width = 6, height = 5, units = "in")


# .. LMIC, yearly average ####
load("Results/Summary data-sets/GER_Sect_Avg_LMIC.Rdata")

g <- ggplot(GER_Sect_Avg_LMIC %>%
              arrange(desc(Tot_IFF_bn)),
            aes(area = Tot_IFF_bn, fill = forcats::fct_inorder(section), label = section)) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "topleft", reflow = T) +
  geom_treemap_text(data = GER_Sect_Avg_LMIC,
                    aes(label = ifelse(Tot_IFF_bn >= sort(Tot_IFF_bn, decreasing = T)[8],
                                       paste0("$", round(Tot_IFF_bn), " bn"),
                                       "")),
                    colour = "white", place = "bottomright", size = 12) +
  theme(legend.position = "none") +
  scale_fill_manual(values = tol21rainbow) +
  labs(title = "Top sectors in low and lower middle income countries",
       subtitle = "Average gross yearly outflow during 2000-2018")
ggsave(g,
       file = "Figures/Treemap sectors_Yearly Average_LMIC.png",
       width = 6, height = 5, units = "in")


# .. Developing, yearly average ####
load("Results/Summary data-sets/GER_Sect_Avg_Developing.Rdata")

g <- ggplot(GER_Sect_Avg_Developing %>%
              arrange(desc(Tot_IFF_bn)),
            aes(area = Tot_IFF_bn, fill = forcats::fct_inorder(section), label = section)) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "topleft", reflow = T) +
  geom_treemap_text(data = GER_Sect_Avg_Developing,
                    aes(label = ifelse(Tot_IFF_bn >= sort(Tot_IFF_bn, decreasing = T)[8],
                                       paste0("$", round(Tot_IFF_bn), " bn"),
                                       "")),
                    colour = "white", place = "bottomright", size = 12) +
  theme(legend.position = "none") +
  scale_fill_manual(values = tol21rainbow) +
  labs(title = "Top sectors in developing countries",
       subtitle = "Average gross yearly outflow during 2000-2018")
ggsave(g,
       file = "Figures/Treemap sectors_Yearly Average_Developing.png",
       width = 6, height = 5, units = "in")



## ## ## ## ## ## ## ## ## ## ##
# STACKED BAR CHARTS        ####
## ## ## ## ## ## ## ## ## ## ##


# .. Stacked bar charts of commodities in top sectors ####
load("Results/Summary data-sets/GER_Sect_Avg_Africa_disag.Rdata")
load("Results/Summary data-sets/GER_Sect_Avg_LMIC_disag.Rdata")
load("Results/Summary data-sets/GER_Sect_Avg_Developing_disag.Rdata")
load("/scratch/alepissier/IFFe/Data/UN Stats/HS.Rdata")

# Africa, yearly average
viz <- left_join(GER_Sect_Avg_Africa_disag, HS %>% select(-chapter.description),
                 by = c("commodity.code" = "chapter")) %>%
  filter(section == "Mineral Products" | section == "Machinery and Electrical") %>%
  arrange(desc(Tot_IFF)) %>%
  mutate(commodity = factor(commodity,
                            levels = commodity[order(Tot_IFF, decreasing = T)]))

g <- ggplot(viz,
            aes(x = section, y = Tot_IFF/10^9, 
                fill = fct_inorder(str_wrap(commodity, 20)))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = ifelse(order(Tot_IFF) == min(order(Tot_IFF)),
                               "", paste0("$", round(Tot_IFF/10^9), " billion"))), 
            position = position_stack(vjust = 0.5),
            size = 3) +
  labs(x = NULL, 
       y = "Illicit flow in billion USD", 
       fill = NULL, 
       title = "Breakdown of top sectors in Africa",
       subtitle = "Yearly average outflows during 2000-2018") +
  theme(legend.text = element_text(size = 8)) +
  scale_fill_brewer(type = "qual", palette = "Accent")
ggsave(g,
       file = "Figures/Stacked_Top commodities in top sectors_Yearly Average_Dollars_Africa.png",
       width = 6, height = 5, units = "in")

# LMIC, yearly average
viz <- left_join(GER_Sect_Avg_LMIC_disag, HS %>% select(-chapter.description),
                 by = c("commodity.code" = "chapter")) %>%
  filter(section == "Mineral Products" | section == "Machinery and Electrical") %>%
  arrange(desc(Tot_IFF)) %>%
  mutate(commodity = factor(commodity,
                            levels = commodity[order(Tot_IFF, decreasing = T)]))

g <- ggplot(viz,
            aes(x = fct_rev(section), y = Tot_IFF/10^9, 
                fill = fct_inorder(str_wrap(commodity, 20)))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = ifelse(order(Tot_IFF) == min(order(Tot_IFF)),
                               "", paste0("$", round(Tot_IFF/10^9), " billion"))), 
            position = position_stack(vjust = 0.5),
            size = 3) +
  labs(x = NULL, 
       y = "Illicit flow in billion USD", 
       fill = NULL, 
       title = "Breakdown of top sectors in low and lower middle income",
       subtitle = "Yearly average outflows during 2000-2018") +
  theme(legend.text = element_text(size = 8)) +
  scale_fill_brewer(type = "qual", palette = "Accent")
ggsave(g,
       file = "Figures/Stacked_Top commodities in top sectors_Yearly Average_Dollars_LMIC.png",
       width = 6, height = 5, units = "in")

# Developing, yearly average
viz <- left_join(GER_Sect_Avg_Developing_disag, HS %>% select(-chapter.description),
                 by = c("commodity.code" = "chapter")) %>%
  filter(section == "Mineral Products" | section == "Machinery and Electrical") %>%
  arrange(desc(Tot_IFF)) %>%
  mutate(commodity = factor(commodity,
                            levels = commodity[order(Tot_IFF, decreasing = T)]))

g <- ggplot(viz,
            aes(x = section, y = Tot_IFF/10^9, 
                fill = fct_inorder(str_wrap(commodity, 20)))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = ifelse(order(Tot_IFF) == min(order(Tot_IFF)),
                               "", paste0("$", round(Tot_IFF/10^9), " billion"))),
            position = position_stack(vjust = 0.5),
            size = 3) +
  labs(x = NULL, 
       y = "Illicit flow in billion USD", 
       fill = NULL, 
       title = "Breakdown of top sectors in developing countries",
       subtitle = "Yearly average outflows during 2000-2018") +
  theme(legend.text = element_text(size = 8)) +
  scale_fill_brewer(type = "qual", palette = "Accent")
ggsave(g,
       file = "Figures/Stacked_Top commodities in top sectors_Yearly Average_Dollars_Developing.png",
       width = 6, height = 5, units = "in")


# .. Horizontal stacked bar charts of top sectors in conduits ####
load("Results/Summary data-sets/GER_Orig_Sect_Avg.Rdata")

load("Results/Summary data-sets/GER_Orig_Avg.Rdata")
conduits_Africa <- GER_Orig_Avg %>%
  filter(rRegion == "Africa") %>%
  arrange(desc(Tot_IFF_GDP)) %>%
  head(10) %>%
  select(reporter.ISO) %>%
  pull

conduits_LMIC <- GER_Orig_Avg %>%
  filter(rIncome == "LIC" | rIncome == "LMC") %>%
  arrange(desc(Tot_IFF_GDP)) %>%
  head(10) %>%
  select(reporter.ISO) %>%
  pull

conduits_Developing <- GER_Orig_Avg %>%
  filter(rDev == "Developing") %>%
  arrange(desc(Tot_IFF_GDP)) %>%
  head(10) %>%
  select(reporter.ISO) %>%
  pull

# Africa
top <- GER_Orig_Sect_Avg %>%
  filter(reporter.ISO %in% conduits_Africa) %>%
  group_by(reporter) %>%
  summarize(Tot_IFF = sum(Tot_IFF, na.rm = T)) %>%
  arrange(Tot_IFF) %>%
  pull(reporter)

viz <- GER_Orig_Sect_Avg %>%
  filter(reporter.ISO %in% conduits_Africa) %>%
  group_by(reporter.ISO) %>%
  top_n(5, Tot_IFF) %>%
  ungroup() %>%
  mutate(reporter = factor(reporter,
                           levels = top))

g <- ggplot(viz,
       aes(x = reporter, y = Tot_IFF, fill = str_wrap(section, 20))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = dollar_format(scale = 1/10^9, accuracy = 1)) +
  labs(title = "Top 5 sectors in top African countries",
       subtitle = "Yearly average outflows during 2000-2018",
       x = NULL,
       y = "Illicit flow in billion USD",
       fill = NULL) +
  coord_flip() +
  scale_fill_disco(palette = "rainbow") +
  theme(legend.text = element_text(size = 8))
ggsave(g,
       file = "Figures/Stacked_Top 5 sectors in GDP conduits_Yearly Average_Dollars_Africa.png",
       width = 6, height = 5, units = "in")

# LMIC
top <- GER_Orig_Sect_Avg %>%
  filter(reporter.ISO %in% conduits_LMIC) %>%
  group_by(reporter) %>%
  summarize(Tot_IFF = sum(Tot_IFF, na.rm = T)) %>%
  arrange(Tot_IFF) %>%
  pull(reporter)

viz <- GER_Orig_Sect_Avg %>%
  filter(reporter.ISO %in% conduits_LMIC) %>%
  group_by(reporter.ISO) %>%
  top_n(5, Tot_IFF) %>%
  ungroup() %>%
  mutate(reporter = factor(reporter,
                           levels = top))

g <- ggplot(viz,
       aes(x = reporter, y = Tot_IFF, fill = str_wrap(section, 20))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = dollar_format(scale = 1/10^9, accuracy = 1)) +
  labs(title = "Top 5 sectors in top low and lower middle income",
       subtitle = "Yearly average outflows during 2000-2018",
       x = NULL,
       y = "Illicit flow in billion USD",
       fill = NULL) +
  coord_flip() +
  scale_fill_disco(palette = "rainbow") +
  theme(legend.text = element_text(size = 8))
ggsave(g,
       file = "Figures/Stacked_Top 5 sectors in GDP conduits_Yearly Average_Dollars_LMIC.png",
       width = 6, height = 5, units = "in")

# Developing
top <- GER_Orig_Sect_Avg %>%
  filter(reporter.ISO %in% conduits_Developing) %>%
  group_by(reporter) %>%
  summarize(Tot_IFF = sum(Tot_IFF, na.rm = T)) %>%
  arrange(Tot_IFF) %>%
  pull(reporter)

viz <- GER_Orig_Sect_Avg %>%
  filter(reporter.ISO %in% conduits_Developing) %>%
  group_by(reporter.ISO) %>%
  top_n(5, Tot_IFF) %>%
  ungroup() %>%
  mutate(reporter = factor(reporter,
                           levels = top))

g <- ggplot(viz,
       aes(x = reporter, y = Tot_IFF, fill = str_wrap(section, 20))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = dollar_format(scale = 1/10^9, accuracy = 1)) +
  labs(title = "Top 5 sectors in top developing countries",
       subtitle = "Yearly average outflows during 2000-2018",
       x = NULL,
       y = "Illicit flow in billion USD",
       fill = NULL) +
  coord_flip() +
  scale_fill_disco(palette = "rainbow") +
  theme(legend.text = element_text(size = 8))
ggsave(g,
       file = "Figures/Stacked_Top 5 sectors in GDP conduits_Yearly Average_Dollars_Developing.png",
       width = 6, height = 5, units = "in")


# .. Stacked bar charts of top yearly average destinations in groups ####
# Africa
load("Results/Summary data-sets/GER_Dest_Avg_Africa.Rdata")

g <- ggplot(GER_Dest_Avg_Africa %>%
              top_n(10, Tot_IFF),
            aes(x = "", y = Tot_IFF/10^9, fill = fct_reorder(partner, Tot_IFF, .desc = T))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0("$", round(Tot_IFF/10^9), " billion")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "Top 10 destinations from Africa",
       subtitle = "Yearly average outflows during 2000-2018") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave(g,
       file = "Figures/Stacked_Top 10 destinations_Yearly Average_Dollars_Africa.png",
       width = 6, height = 5, units = "in")

# LMIC
load("Results/Summary data-sets/GER_Dest_Avg_LMIC.Rdata")

g <- ggplot(GER_Dest_Avg_LMIC %>%
              top_n(10, Tot_IFF),
            aes(x = "", y = Tot_IFF/10^9, fill = fct_reorder(partner, Tot_IFF, .desc = T))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0("$", round(Tot_IFF/10^9), " billion")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "Top 10 destinations from low & lower middle income countries",
       subtitle = "Yearly average outflows during 2000-2018") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave(g,
       file = "Figures/Stacked_Top 10 destinations_Yearly Average_Dollars__LMIC.png",
       width = 6, height = 5, units = "in")

# Developing
load("Results/Summary data-sets/GER_Dest_Avg_Developing.Rdata")

g <- ggplot(GER_Dest_Avg_Developing %>%
              top_n(10, Tot_IFF),
            aes(x = "", y = Tot_IFF/10^9, fill = fct_reorder(partner, Tot_IFF, .desc = T))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0("$", round(Tot_IFF/10^9), " billion")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "Top 10 destinations from developing countries",
       subtitle = "Yearly average outflows during 2000-2018") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave(g,
       file = "Figures/Stacked_Top 10 destinations_Yearly Average_Dollars__Developing.png",
       width = 6, height = 5, units = "in")



## ## ## ## ## ## ## ## ## ## ##
# FLOW MAPS                 ####
## ## ## ## ## ## ## ## ## ## ##

# .. Top 5 destinations of % GDP conduits ####
load("Results/Summary data-sets/GER_Orig_Dest_Avg_Africa.Rdata")
load("Results/Summary data-sets/GER_Orig_Dest_Avg_LMIC.Rdata")
load("Results/Summary data-sets/GER_Orig_Dest_Avg_Developing.Rdata")

load("Results/Summary data-sets/GER_Orig_Avg.Rdata")
conduits_Africa <- GER_Orig_Avg %>%
  filter(rRegion == "Africa") %>%
  arrange(desc(Tot_IFF_GDP)) %>%
  head(10) %>%
  select(reporter.ISO) %>%
  pull

conduits_LMIC <- GER_Orig_Avg %>%
  filter(rIncome == "LIC" | rIncome == "LMC") %>%
  arrange(desc(Tot_IFF_GDP)) %>%
  head(10) %>%
  select(reporter.ISO) %>%
  pull

conduits_Developing <- GER_Orig_Avg %>%
  filter(rDev == "Developing") %>%
  arrange(desc(Tot_IFF_GDP)) %>%
  head(10) %>%
  select(reporter.ISO) %>%
  pull

ditch_axes <- theme(axis.title.x = element_blank(),
                    axis.text.x = element_blank(),
                    axis.ticks.x = element_blank(),
                    axis.title.y = element_blank(),
                    axis.text.y = element_blank(),
                    axis.ticks.y = element_blank(),
                    panel.border = element_blank(),
                    panel.grid = element_blank()) 

centroids <- codes %>%
  dplyr::select(ISO3166.3, Centroid_Longitude, Centroid_Latitude) %>%
  mutate_at(vars(Centroid_Longitude, Centroid_Latitude),
            funs(as.numeric))

# Africa, yearly average dollar value
GER_Orig_Dest_Avg_Africa <- GER_Orig_Dest_Avg_Africa %>%
  left_join(centroids %>% distinct(ISO3166.3, .keep_all = T), by = c("reporter.ISO" = "ISO3166.3")) %>%
  dplyr::rename(rLongitude = Centroid_Longitude,
                rLatitude = Centroid_Latitude) %>%
  left_join(centroids %>% distinct(ISO3166.3, .keep_all = T), by = c("partner.ISO" = "ISO3166.3"))%>%
  dplyr::rename(pLongitude = Centroid_Longitude,
                pLatitude = Centroid_Latitude) %>%
  filter(reporter.ISO %in% conduits_Africa) %>%
  group_by(reporter.ISO) %>%
  top_n(5, Tot_IFF) %>%
  ungroup() %>%
  mutate(scale = round((10 - 1) * (Tot_IFF - min(Tot_IFF))/(max(Tot_IFF) - min(Tot_IFF)) + 1))

map <- map_data("world")
map <- left_join(map, codes %>% dplyr::select(Country, ISO3166.3),
                 by = c("region" = "Country")) %>%
  dplyr::select(-subregion) %>%
  filter(region != "Antarctica")

viz <- left_join(GER_Orig_Dest_Avg_Africa %>% filter(reporter.ISO %in% conduits_Africa),
                 map,
                 by = c("reporter.ISO" = "ISO3166.3"))

g <- ggplot() + 
  geom_polygon(data = map,
               aes(x = long, y = lat, group = group), fill = "grey90", col = "white", lwd = 0.2) + 
  coord_fixed(1.3) +
  theme_bw() + 
  geom_curve(data = viz, 
             aes(x = rLongitude, y = rLatitude, 
                 xend = pLongitude, yend = pLatitude, col = reporter),
             curvature = -0.2, lineend = "round", ncp = 20) +
  geom_point(data = viz %>% distinct(reporter.ISO, .keep_all = T),
             aes(x = rLongitude, y = rLatitude, col = reporter),
             size = 4) +
  geom_label_repel(data = viz %>% distinct(reporter.ISO, .keep_all = T),
                   aes(label = reporter, x = rLongitude, y = rLatitude, fill = reporter),
                   size = 2, fontface = "bold", alpha = 0.5, seed = 1509) +
  geom_label_repel(data = viz %>% distinct(reporter.ISO, .keep_all = T),
                   aes(label = reporter, x = rLongitude, y = rLatitude),
                   size = 2, fontface = "bold", alpha = 1, fill = NA, seed = 1509) +
  ditch_axes +
  guides(col = FALSE, fill = FALSE) +
  scale_color_manual(values = carto_pal(10, "Bold")) +
  scale_fill_manual(values = carto_pal(10, "Bold")) +
  labs(title = "Destinations of top origins in Africa",
       subtitle = "Top 10 origin countries by % of GDP")
ggsave(g,
       file = "Figures/Flow map_Top 5 destinations in GDP conduits_Yearly Average_Africa.png",
       width = 6, height = 5, units = "in")

# LMIC, yearly average dollar value
GER_Orig_Dest_Avg_LMIC <- GER_Orig_Dest_Avg_LMIC %>%
  left_join(centroids %>% distinct(ISO3166.3, .keep_all = T), by = c("reporter.ISO" = "ISO3166.3")) %>%
  dplyr::rename(rLongitude = Centroid_Longitude,
                rLatitude = Centroid_Latitude) %>%
  left_join(centroids %>% distinct(ISO3166.3, .keep_all = T), by = c("partner.ISO" = "ISO3166.3"))%>%
  dplyr::rename(pLongitude = Centroid_Longitude,
                pLatitude = Centroid_Latitude) %>%
  filter(reporter.ISO %in% conduits_LMIC) %>%
  group_by(reporter.ISO) %>%
  top_n(5, Tot_IFF) %>%
  ungroup() %>%
  mutate(scale = round((10 - 1) * (Tot_IFF - min(Tot_IFF))/(max(Tot_IFF) - min(Tot_IFF)) + 1))

map <- map_data("world")
map <- left_join(map, codes %>% dplyr::select(Country, ISO3166.3),
                 by = c("region" = "Country")) %>%
  dplyr::select(-subregion) %>%
  filter(region != "Antarctica")

viz <- left_join(GER_Orig_Dest_Avg_LMIC %>% filter(reporter.ISO %in% conduits_LMIC),
                 map,
                 by = c("reporter.ISO" = "ISO3166.3"))

g <- ggplot() + 
  geom_polygon(data = map,
               aes(x = long, y = lat, group = group), fill = "grey90", col = "white", lwd = 0.2) + 
  coord_fixed(1.3) +
  theme_bw() + 
  geom_curve(data = viz, 
             aes(x = rLongitude, y = rLatitude, 
                 xend = pLongitude, yend = pLatitude, col = reporter),
             curvature = -0.2, lineend = "round", ncp = 20) +
  geom_point(data = viz %>% distinct(reporter.ISO, .keep_all = T),
             aes(x = rLongitude, y = rLatitude, col = reporter),
             size = 4) +
  geom_label_repel(data = viz %>% distinct(reporter.ISO, .keep_all = T),
                   aes(label = reporter, x = rLongitude, y = rLatitude, fill = reporter),
                   size = 2, fontface = "bold", alpha = 0.5, seed = 1509) +
  geom_label_repel(data = viz %>% distinct(reporter.ISO, .keep_all = T),
                   aes(label = reporter, x = rLongitude, y = rLatitude),
                   size = 2, fontface = "bold", alpha = 1, fill = NA, seed = 1509) +
  ditch_axes +
  guides(col = FALSE, fill = FALSE) +
  scale_color_manual(values = carto_pal(10, "Bold")) +
  scale_fill_manual(values = carto_pal(10, "Bold")) +
  labs(title = "Destinations of top origins in low and lower middle income",
       subtitle = "Top 10 origin countries by % of GDP")
ggsave(g,
       file = "Figures/Flow map_Top 5 destinations in GDP conduits_Yearly Average_LMIC.png",
       width = 6, height = 5, units = "in")

# Developing, yearly average dollar value
GER_Orig_Dest_Avg_Developing <- GER_Orig_Dest_Avg_Developing %>%
  left_join(centroids %>% distinct(ISO3166.3, .keep_all = T), by = c("reporter.ISO" = "ISO3166.3")) %>%
  dplyr::rename(rLongitude = Centroid_Longitude,
                rLatitude = Centroid_Latitude) %>%
  left_join(centroids %>% distinct(ISO3166.3, .keep_all = T), by = c("partner.ISO" = "ISO3166.3"))%>%
  dplyr::rename(pLongitude = Centroid_Longitude,
                pLatitude = Centroid_Latitude) %>%
  filter(reporter.ISO %in% conduits_Developing) %>%
  group_by(reporter.ISO) %>%
  top_n(5, Tot_IFF) %>%
  ungroup() %>%
  mutate(scale = round((10 - 1) * (Tot_IFF - min(Tot_IFF))/(max(Tot_IFF) - min(Tot_IFF)) + 1))

map <- map_data("world")
map <- left_join(map, codes %>% dplyr::select(Country, ISO3166.3),
                 by = c("region" = "Country")) %>%
  dplyr::select(-subregion) %>%
  filter(region != "Antarctica")

viz <- left_join(GER_Orig_Dest_Avg_Developing %>% filter(reporter.ISO %in% conduits_Developing),
                 map,
                 by = c("reporter.ISO" = "ISO3166.3"))

g <- ggplot() + 
  geom_polygon(data = map,
               aes(x = long, y = lat, group = group), fill = "grey90", col = "white", lwd = 0.2) + 
  coord_fixed(1.3) +
  theme_bw() + 
  geom_curve(data = viz, 
             aes(x = rLongitude, y = rLatitude, 
                 xend = pLongitude, yend = pLatitude, col = reporter),
             curvature = -0.2, lineend = "round", ncp = 20) +
  geom_point(data = viz %>% distinct(reporter.ISO, .keep_all = T),
             aes(x = rLongitude, y = rLatitude, col = reporter),
             size = 4) +
  geom_label_repel(data = viz %>% distinct(reporter.ISO, .keep_all = T),
                   aes(label = reporter, x = rLongitude, y = rLatitude, fill = reporter),
                   size = 2, fontface = "bold", alpha = 0.5, seed = 1509) +
  geom_label_repel(data = viz %>% distinct(reporter.ISO, .keep_all = T),
                   aes(label = reporter, x = rLongitude, y = rLatitude),
                   size = 2, fontface = "bold", alpha = 1, fill = NA, seed = 1509) +
  ditch_axes +
  guides(col = FALSE, fill = FALSE) +
  scale_color_manual(values = carto_pal(10, "Bold")) +
  scale_fill_manual(values = carto_pal(10, "Bold")) +
  labs(title = "Destinations of top origins in developing countries",
       subtitle = "Top 10 origin countries by % of GDP")
ggsave(g,
       file = "Figures/Flow map_Top 5 destinations in GDP conduits_Yearly Average_Developing.png",
       width = 6, height = 5, units = "in")


# .. Provenance flow maps of top inflows in World ####
load("Results/Summary data-sets/Net_Orig_Dest_Avg.Rdata")
load("Results/Summary data-sets/Net_Orig_Avg.Rdata")

top_inflows_dollar <- Net_Orig_Avg %>%
  filter(Tot_IFF < 0) %>%
  top_n(10, abs(Tot_IFF)) %>%
  arrange(desc(abs(Tot_IFF))) %>%
  pull(reporter.ISO)

top_inflows_GDP <- Net_Orig_Avg %>%
  filter(Tot_IFF < 0) %>%
  top_n(10, abs(Tot_IFF_GDP)) %>%
  arrange(desc(abs(Tot_IFF_GDP))) %>%
  pull(reporter.ISO)
  
ditch_axes <- theme(axis.title.x = element_blank(),
                    axis.text.x = element_blank(),
                    axis.ticks.x = element_blank(),
                    axis.title.y = element_blank(),
                    axis.text.y = element_blank(),
                    axis.ticks.y = element_blank(),
                    panel.border = element_blank(),
                    panel.grid = element_blank()) 

centroids <- codes %>%
  dplyr::select(ISO3166.3, Centroid_Longitude, Centroid_Latitude) %>%
  mutate_at(vars(Centroid_Longitude, Centroid_Latitude),
            funs(as.numeric))

map <- map_data("world")
map <- left_join(map, codes %>% dplyr::select(Country, ISO3166.3),
                 by = c("region" = "Country")) %>%
  dplyr::select(-subregion) %>%
  filter(region != "Antarctica")

# Top inflows, dollar value
viz <- Net_Orig_Dest_Avg %>%
  left_join(centroids %>% distinct(ISO3166.3, .keep_all = T), by = c("reporter.ISO" = "ISO3166.3")) %>%
  dplyr::rename(rLongitude = Centroid_Longitude,
                rLatitude = Centroid_Latitude) %>%
  left_join(centroids %>% distinct(ISO3166.3, .keep_all = T), by = c("partner.ISO" = "ISO3166.3"))%>%
  dplyr::rename(pLongitude = Centroid_Longitude,
                pLatitude = Centroid_Latitude) %>%
  filter(reporter.ISO %in% top_inflows_dollar) %>%
  filter(Tot_IFF < 0) %>%
  group_by(reporter.ISO) %>%
  top_n(5, abs(Tot_IFF)) %>%
  ungroup() %>%
  mutate(scale = round((10 - 1) * (Tot_IFF - min(Tot_IFF))/(max(Tot_IFF) - min(Tot_IFF)) + 1))

viz <- left_join(viz %>% filter(reporter.ISO %in% top_inflows_dollar),
                 map,
                 by = c("reporter.ISO" = "ISO3166.3"))

g <- ggplot() + 
  geom_polygon(data = map,
               aes(x = long, y = lat, group = group), fill = "grey80", col = "white", lwd = 0.2) + 
  coord_fixed(1.3) +
  theme_bw() + 
  geom_curve(data = viz, 
             aes(x = rLongitude, y = rLatitude, 
                 xend = pLongitude, yend = pLatitude, col = reporter),
             curvature = -0.2, lineend = "round", ncp = 20) +
  geom_point(data = viz %>% distinct(reporter.ISO, .keep_all = T),
             aes(x = rLongitude, y = rLatitude, col = reporter),
             size = 4) +
  geom_label_repel(data = viz %>% distinct(reporter.ISO, .keep_all = T),
                   aes(label = reporter, x = rLongitude, y = rLatitude, fill = reporter),
                   size = 2, fontface = "bold", alpha = 0.5, seed = 1509) +
  geom_label_repel(data = viz %>% distinct(reporter.ISO, .keep_all = T),
                   aes(label = reporter, x = rLongitude, y = rLatitude),
                   size = 2, fontface = "bold", alpha = 1, fill = NA, seed = 1509) +
  ditch_axes +
  guides(col = FALSE, fill = FALSE) +
  scale_color_brewer(type = "qual", palette = "Paired") +
  scale_fill_brewer(type = "qual", palette = "Paired") +
  labs(title = "Provenance of inflows for top recipients of net inflows",
       subtitle = "Top 10 origin countries for net inflows in $")
ggsave(g,
       file = "Figures/Flow map_Provenance of top 10 inflows_Dollars_Yearly Average_World.png",
       width = 6, height = 5, units = "in")

# Top inflows, % of GDP
viz <- Net_Orig_Dest_Avg %>%
  left_join(centroids %>% distinct(ISO3166.3, .keep_all = T), by = c("reporter.ISO" = "ISO3166.3")) %>%
  dplyr::rename(rLongitude = Centroid_Longitude,
                rLatitude = Centroid_Latitude) %>%
  left_join(centroids %>% distinct(ISO3166.3, .keep_all = T), by = c("partner.ISO" = "ISO3166.3"))%>%
  dplyr::rename(pLongitude = Centroid_Longitude,
                pLatitude = Centroid_Latitude) %>%
  filter(reporter.ISO %in% top_inflows_GDP) %>%
  filter(Tot_IFF < 0) %>%
  group_by(reporter.ISO) %>%
  top_n(5, abs(Tot_IFF)) %>%
  ungroup() %>%
  mutate(scale = round((10 - 1) * (Tot_IFF - min(Tot_IFF))/(max(Tot_IFF) - min(Tot_IFF)) + 1))

viz <- left_join(viz %>% filter(reporter.ISO %in% top_inflows_GDP),
                 map,
                 by = c("reporter.ISO" = "ISO3166.3"))

g <- ggplot() + 
  geom_polygon(data = map,
               aes(x = long, y = lat, group = group), fill = "grey80", col = "white", lwd = 0.2) + 
  coord_fixed(1.3) +
  theme_bw() + 
  geom_curve(data = viz, 
             aes(x = rLongitude, y = rLatitude, 
                 xend = pLongitude, yend = pLatitude, col = reporter),
             curvature = -0.2, lineend = "round", ncp = 20) +
  geom_point(data = viz %>% distinct(reporter.ISO, .keep_all = T),
             aes(x = rLongitude, y = rLatitude, col = reporter),
             size = 4) +
  geom_label_repel(data = viz %>% distinct(reporter.ISO, .keep_all = T),
                   aes(label = reporter, x = rLongitude, y = rLatitude, fill = reporter),
                   size = 2, fontface = "bold", alpha = 0.5, seed = 1509) +
  geom_label_repel(data = viz %>% distinct(reporter.ISO, .keep_all = T),
                   aes(label = reporter, x = rLongitude, y = rLatitude),
                   size = 2, fontface = "bold", alpha = 1, fill = NA, seed = 1509) +
  ditch_axes +
  guides(col = FALSE, fill = FALSE) +
  scale_color_brewer(type = "qual", palette = "Paired") +
  scale_fill_brewer(type = "qual", palette = "Paired") +
  labs(title = "Provenance of inflows for top recipients of net inflows",
       subtitle = "Top 10 origin countries for net inflows by % of GDP")
ggsave(g,
       file = "Figures/Flow map_Provenance of top 10 inflows_Percent GDP_Yearly Average_World.png",
       width = 6, height = 5, units = "in")



## ## ## ## ## ## ## ## ## ## ##
# LOLLIPOP CHARTS           ####
## ## ## ## ## ## ## ## ## ## ##

# .. Top conduits in World ####
load("Results/Summary data-sets/GER_Orig_Avg.Rdata")

g <- ggplot(GER_Orig_Avg %>%
              select(reporter, Tot_IFF_GDP) %>%
              top_n(10, Tot_IFF_GDP),
            aes(x = fct_reorder(reporter, Tot_IFF_GDP), y = Tot_IFF_GDP*100)) +
  geom_segment(aes(xend = reporter, y = 0, yend = Tot_IFF_GDP*100), color = "skyblue") +
  geom_point(size = 4, color = "cornflowerblue") +
  coord_flip() +
  labs(title = "Top origin countries worldwide",
       subtitle = "Average yearly gross outflow as % of GDP",
       x = "", y = "Illicit flow as % of GDP")
ggsave(g,
       file = "Figures/Top 10 origin countries_Yearly Average_Percent GDP_World.png",
       width = 6, height = 5, units = "in")

g <- ggplot(GER_Orig_Avg %>%
              select(reporter, Tot_IFF_trade) %>%
              top_n(10, Tot_IFF_trade),
            aes(x = fct_reorder(reporter, Tot_IFF_trade), y = Tot_IFF_trade*100)) +
  geom_segment(aes(xend = reporter, y = 0, yend = Tot_IFF_trade*100), color = "skyblue") +
  geom_point(size = 4, color = "cornflowerblue") +
  coord_flip() +
  labs(title = "Top origin countries worldwide",
       subtitle = "Average yearly gross outflow as % of trade",
       x = "", y = "Illicit flow as % of trade")
ggsave(g,
       file = "Figures/Top 10 origin countries_Yearly Average_Percent Trade_World.png",
       width = 6, height = 5, units = "in")


# .. Top conduits in Africa ####
load("Results/Summary data-sets/GER_Orig_Avg_Africa.Rdata")

g <- ggplot(GER_Orig_Avg_Africa %>%
              select(reporter, Tot_IFF_GDP) %>%
              top_n(10, Tot_IFF_GDP),
            aes(x = fct_reorder(reporter, Tot_IFF_GDP), y = Tot_IFF_GDP*100)) +
  geom_segment(aes(xend = reporter, y = 0, yend = Tot_IFF_GDP*100), color = "skyblue") +
  geom_point(size = 4, color = "cornflowerblue") +
  coord_flip() +
  labs(title = "Top origin countries in Africa",
       subtitle = "Average yearly gross outflow as % of GDP",
       x = "", y = "Illicit flow as % of GDP")
ggsave(g,
       file = "Figures/Top 10 origin countries_Yearly Average_Percent GDP_Africa.png",
       width = 6, height = 5, units = "in")

g <- ggplot(GER_Orig_Avg_Africa %>%
              select(reporter, Tot_IFF_trade) %>%
              top_n(10, Tot_IFF_trade),
            aes(x = fct_reorder(reporter, Tot_IFF_trade), y = Tot_IFF_trade*100)) +
  geom_segment(aes(xend = reporter, y = 0, yend = Tot_IFF_trade*100), color = "skyblue") +
  geom_point(size = 4, color = "cornflowerblue") +
  coord_flip() +
  labs(title = "Top origin countries in Africa",
       subtitle = "Average yearly gross outflow as % of trade",
       x = "", y = "Illicit flow as % of trade")
ggsave(g,
       file = "Figures/Top 10 origin countries_Yearly Average_Percent Trade_Africa.png",
       width = 6, height = 5, units = "in")


# .. Top conduits in LMIC ####
load("Results/Summary data-sets/GER_Orig_Avg_LMIC.Rdata")

g <- ggplot(GER_Orig_Avg_LMIC %>%
              select(reporter, Tot_IFF_GDP) %>%
              top_n(10, Tot_IFF_GDP),
            aes(x = fct_reorder(reporter, Tot_IFF_GDP), y = Tot_IFF_GDP*100)) +
  geom_segment(aes(xend = reporter, y = 0, yend = Tot_IFF_GDP*100), color = "skyblue") +
  geom_point(size = 4, color = "cornflowerblue") +
  coord_flip() +
  labs(title = "Top origins in low and lower-middle income",
       subtitle = "Average yearly gross outflow as % of GDP",
       x = "", y = "Illicit flow as % of GDP")
ggsave(g,
       file = "Figures/Top 10 origin countries_Yearly Average_Percent GDP_LMIC.png",
       width = 6, height = 5, units = "in")

g <- ggplot(GER_Orig_Avg_LMIC %>%
              select(reporter, Tot_IFF_trade) %>%
              top_n(10, Tot_IFF_trade),
            aes(x = fct_reorder(reporter, Tot_IFF_trade), y = Tot_IFF_trade*100)) +
  geom_segment(aes(xend = reporter, y = 0, yend = Tot_IFF_trade*100), color = "skyblue") +
  geom_point(size = 4, color = "cornflowerblue") +
  coord_flip() +
  labs(title = "Top origins in low and lower-middle income",
       subtitle = "Average yearly gross outflow as % of trade",
       x = "", y = "Illicit flow as % of trade")
ggsave(g,
       file = "Figures/Top 10 origin countries_Yearly Average_Percent Trade_LMIC.png",
       width = 6, height = 5, units = "in")


# .. Top conduits in Developing ####
load("Results/Summary data-sets/GER_Orig_Avg_Developing.Rdata")

g <- ggplot(GER_Orig_Avg_Developing %>%
              select(reporter, Tot_IFF_GDP) %>%
              top_n(10, Tot_IFF_GDP),
            aes(x = fct_reorder(reporter, Tot_IFF_GDP), y = Tot_IFF_GDP*100)) +
  geom_segment(aes(xend = reporter, y = 0, yend = Tot_IFF_GDP*100), color = "skyblue") +
  geom_point(size = 4, color = "cornflowerblue") +
  coord_flip() +
  labs(title = "Top origins in developing countries",
       subtitle = "Average yearly gross outflow as % of GDP",
       x = "", y = "Illicit flow as % of GDP")
ggsave(g,
       file = "Figures/Top 10 origin countries_Yearly Average_Percent GDP_Developing.png",
       width = 6, height = 5, units = "in")

g <- ggplot(GER_Orig_Avg_Developing %>%
              select(reporter, Tot_IFF_trade) %>%
              top_n(10, Tot_IFF_trade),
            aes(x = fct_reorder(reporter, Tot_IFF_trade), y = Tot_IFF_trade*100)) +
  geom_segment(aes(xend = reporter, y = 0, yend = Tot_IFF_trade*100), color = "skyblue") +
  geom_point(size = 4, color = "cornflowerblue") +
  coord_flip() +
  labs(title = "Top origins in developing countries",
       subtitle = "Average yearly gross outflow as % of trade",
       x = "", y = "Illicit flow as % of trade")
ggsave(g,
       file = "Figures/Top 10 origin countries_Yearly Average_Percent Trade_Developing.png",
       width = 6, height = 5, units = "in")



## ## ## ## ## ## ## ## ## ## ##
# SANKEY DIAGRAM            ####
## ## ## ## ## ## ## ## ## ## ##

# .. Sankey diagram by GNI per capita and sector in LMIC ####
load("Results/Summary data-sets/GER_Orig_Sect_Avg.Rdata")
load("Results/Summary data-sets/GER_Sect_Avg_LMIC.Rdata")

top_sectors <- GER_Sect_Avg_LMIC %>%
  top_n(10, Tot_IFF_bn) %>%
  arrange(desc(Tot_IFF_bn)) %>%
  pull(section)

viz <- GER_Orig_Sect_Avg %>%
  filter(rIncome == "LIC" | rIncome == "LMC") %>%
  filter(section %in% top_sectors) %>%
  mutate(cut = cut(GNPpc, breaks = c(0, 1000, 2000, 3000, 4000),
                   labels = c("0$-1,000$", "1,001$-2,000$", "2,001$-3,000$", "3,001$-4000$"))) %>%
  group_by(cut, section, section.code) %>%
  summarize(Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup() %>%
  filter(!is.na(cut)) %>%
  arrange(factor(section, levels = GER_Sect_Avg_LMIC %>%
                   arrange(desc(Tot_IFF_bn)) %>%
                   pull(section)))

g <- ggplot(viz,
            aes(y = Tot_IFF_bn, axis1 = cut, axis2 = fct_inorder(section),
                label = after_stat(stratum))) +
  geom_alluvium(aes(fill = cut)) +
  geom_stratum(width = 1/12, color = "black", alpha = 0.5) +
  geom_label(stat = "stratum", size = 3, hjust = "inward") +
  scale_x_discrete(limits = c("GNI per capita", "Sector"), expand = c(0.075, 0.075)) +
  scale_y_continuous(labels = dollar_format()) +
  scale_fill_manual(values = wes_palette("BottleRocket2")) +
  labs(title = "Trade mis-invoicing in low and lower middle income",
       subtitle = "according to GNI per capita and top 10 sectors",
       y = "Yearly average outflow in billion USD") +
  theme(legend.position = "none")
ggsave(g,
       file = "Figures/Sankey_Reporter GNI by Reporter Sector_LMIC.png",
       width = 6, height = 5, units = "in")


# .. Sankey diagram by reporter and partner GNI per capita in World ####
load("Results/Summary data-sets/GER_Orig_Dest_Avg.Rdata")

viz <- GER_Orig_Dest_Avg %>%
  mutate(rGNPpc = rGNPpc / 10^3,
         pGNPpc = pGNPpc / 10^3) %>%
  mutate(rcut = cut(rGNPpc, breaks = c(0, 5, 30, 50, 105),
                    labels = c("up to $5,000", "$5,001-$30,000",
                               "$30,001-$50,000", "above $50,000")),
         pcut = cut(pGNPpc, breaks = c(0, 5, 30, 50, 105),
                    labels = c("up to $5,000", "$5,001-$30,000",
                               "$30,001-$50,000", "above $50,000"))) %>%
  filter(is.finite(rcut) & is.finite(pcut)) %>%
  group_by(rcut, pcut) %>%
  summarize(Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()

g <- ggplot(viz,
            aes(y = Tot_IFF_bn, axis1 = rcut, axis2 = pcut,
                label = after_stat(stratum))) +
  geom_alluvium(aes(fill = rcut)) +
  geom_stratum(width = 1/12, color = "black", alpha = 0.5) +
  geom_label(stat = "stratum", size = 3, hjust = "inward") +
  scale_x_discrete(limits = c("Origin GNI/capita", "Destination GNI/capita"), expand = c(0.075, 0.075)) +
  scale_y_continuous(labels = dollar_format()) +
  scale_fill_manual(values = wes_palette("GrandBudapest1")) +
  labs(title = "Trade mis-invoicing",
       subtitle = "according to GNI per capita",
       y = "Yearly average outflow in billion USD") +
  theme(legend.position = "none")
ggsave(g,
       file = "Figures/Sankey_Reporter GNI by Partner GNI_World.png",
       width = 6, height = 5, units = "in")


# .. Sankey diagram by reporter and partner GNI per capita in LMIC ####
load("Results/Summary data-sets/GER_Orig_Dest_Avg_LMIC.Rdata")

viz <- GER_Orig_Dest_Avg_LMIC %>%
  mutate(rcut = cut(rGNPpc, breaks = c(0, 1000, 2000, 3000, 4000),
                    labels = c("0$-1,000$", "1,001$-2,000$", "2,001$-3,000$", "3,001$-4,000$")),
         pcut = cut(pGNPpc, breaks = c(0, 15000, 30000, 45000, 60000, 75000),
                    labels = c("0$-15,000$", "15,001$-30,000$", "30,001$-45,000$", "45,001$-60,000$", "60,001$-75,000$"))) %>%
  filter(is.finite(rcut) & is.finite(pcut)) %>%
  group_by(rcut, pcut) %>%
  summarize(Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()

g <- ggplot(viz,
            aes(y = Tot_IFF_bn, axis1 = rcut, axis2 = pcut,
                label = after_stat(stratum))) +
  geom_alluvium(aes(fill = rcut)) +
  geom_stratum(width = 1/12, color = "black", alpha = 0.5) +
  geom_label(stat = "stratum", size = 3, hjust = "inward") +
  scale_x_discrete(limits = c("Origin GNI/capita", "Destination GNI/capita"), expand = c(0.075, 0.075)) +
  scale_y_continuous(labels = dollar_format()) +
  scale_fill_manual(values = wes_palette("Darjeeling1")) +
  labs(title = "Trade mis-invoicing in low and lower middle income",
       subtitle = "according to GNI per capita",
       y = "Yearly average outflow in billion USD") +
  theme(legend.position = "none")
ggsave(g,
       file = "Figures/Sankey_Reporter GNI by Partner GNI_LMIC.png",
       width = 6, height = 5, units = "in")


# .. Sankey diagram by region and sector in World ####
# HS sections
load("Results/Summary data-sets/GER_Sect_Avg.Rdata")

top_sectors <- GER_Sect_Avg %>%
  top_n(10, Tot_IFF_bn) %>%
  arrange(desc(Tot_IFF_bn)) %>%
  pull(section)

viz <- GER_Orig_Sect_Avg %>%
  filter(section %in% top_sectors) %>%
  group_by(rRegion, section, section.code) %>%
  summarize(Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup() %>%
  arrange(factor(section, levels = GER_Sect_Avg %>%
                   arrange(desc(Tot_IFF_bn)) %>%
                   pull(section)))

g <- ggplot(viz,
            aes(y = Tot_IFF_bn, axis1 = rRegion, axis2 = fct_inorder(section),
                label = after_stat(stratum))) +
  geom_alluvium(aes(fill = rRegion)) +
  geom_stratum(width = 1/12, color = "black", alpha = 0.5) +
  geom_label(stat = "stratum", size = 3, hjust = "inward") +
  scale_x_discrete(limits = c("Region", "Sector"), expand = c(0.075, 0.075)) +
  scale_y_continuous(labels = dollar_format()) +
  scale_fill_manual(values = wes_palette("Rushmore1")) +
  labs(title = "Trade mis-invoicing",
       subtitle = "according to region and top 10 sectors",
       y = "Yearly average outflow in billion USD") +
  theme(legend.position = "none")
ggsave(g,
       file = "Figures/Sankey_Reporter Region by Origin Sector_World.png",
       width = 6, height = 5, units = "in")

# SITC sectors
load("Results/Summary data-sets/GER_Orig_Sect_Avg_SITC.Rdata")
load("Results/Summary data-sets/GER_Sect_Avg_SITC.Rdata")

viz <- GER_Orig_Sect_Avg_SITC %>%
  group_by(rRegion, SITC.code, SITC.section) %>%
  summarize(Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup() %>%
  arrange(factor(SITC.section, levels = GER_Sect_Avg_SITC %>% 
                   arrange(desc(Tot_IFF_bn)) %>%
                   pull(SITC.section)))

g <- ggplot(viz,
            aes(y = Tot_IFF_bn, axis1 = rRegion, axis2 = fct_inorder(SITC.section),
                label = after_stat(stratum))) +
  geom_alluvium(aes(fill = rRegion)) +
  geom_stratum(width = 1/12, color = "black", alpha = 0.5) +
  geom_label(stat = "stratum", size = 3, hjust = "inward") +
  scale_x_discrete(limits = c("Region", "Sector"), expand = c(0.075, 0.075)) +
  scale_y_continuous(labels = dollar_format()) +
  scale_fill_manual(values = wes_palette("Rushmore1")) +
  labs(title = "Trade mis-invoicing",
       subtitle = "according to region and SITC sectors",
       y = "Yearly average outflow in billion USD") +
  theme(legend.position = "none")
ggsave(g,
       file = "Figures/Sankey_Reporter Region by Origin SITC Sector_World.png",
       width = 6, height = 5, units = "in")


# .. Sankey diagram by natural resource dependence and SITC sectors ####
load(paste0(data.disk, "Data/natural_resources.Rdata"))
load("Results/Summary data-sets/GER_Orig_Sect_Avg_SITC.Rdata")
load("Results/Summary data-sets/GER_Sect_Avg_SITC.Rdata")

natural_resources <- natural_resources %>%
  summarize(nat_ratio = mean(nat_ratio, na.rm = T))

viz <- left_join(GER_Orig_Sect_Avg_SITC, natural_resources,
                 by = c("reporter.ISO")) %>%
  mutate(nat_cut = cut(nat_ratio, breaks = c(0, .20, .30, .40, 1),
                       labels = c("up to 20%", "21-30%",
                                  "31-40%", "more than 40%"))) %>%
  group_by(nat_cut, SITC.code, SITC.section) %>%
  summarize(Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup() %>%
  arrange(factor(SITC.section, levels = GER_Sect_Avg_SITC %>% 
                   arrange(desc(Tot_IFF_bn)) %>% 
                   pull(SITC.section)))

g <- ggplot(viz,
            aes(y = Tot_IFF_bn, axis1 = nat_cut, axis2 = fct_inorder(SITC.section),
                label = after_stat(stratum))) +
  geom_alluvium(aes(fill = nat_cut)) +
  geom_stratum(width = 1/12, color = "black", alpha = 0.5) +
  geom_label(stat = "stratum", size = 3, hjust = "inward") +
  scale_x_discrete(limits = c("Natural resource ratio", "Sector"), expand = c(0.075, 0.075)) +
  scale_y_continuous(labels = dollar_format()) +
  scale_fill_manual(values = wes_palette("Cavalcanti1")) +
  labs(title = "Trade mis-invoicing",
       subtitle = "according to natural resource dependence and SITC sectors",
       y = "Yearly average outflow in billion USD") +
  theme(legend.position = "none")
ggsave(g,
       file = "Figures/Sankey_Reporter Natural Resources by Origin SITC Sector_World.png",
       width = 6, height = 5, units = "in")


# .. Sankey diagram by extreme natural resource dependence and partner region ####
load("Results/Summary data-sets/GER_Orig_Dest_Avg.Rdata")

viz <- left_join(GER_Orig_Dest_Avg, natural_resources,
                 by = c("reporter.ISO")) %>%
  filter(nat_ratio >= 0.5) %>%
  mutate(nat_cut = cut(nat_ratio, breaks = c(0, .6, .7, .8),
                       labels = c("50-60%", "61-70%", "more than 70%"))) %>%
  group_by(nat_cut, pRegion) %>%
  summarize(Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()

g <- ggplot(viz,
            aes(y = Tot_IFF_bn, axis1 = nat_cut, axis2 = pRegion,
                label = after_stat(stratum))) +
  geom_alluvium(aes(fill = nat_cut)) +
  geom_stratum(width = 1/12, color = "black", alpha = 0.5) +
  geom_label(stat = "stratum", size = 3, hjust = "inward") +
  scale_x_discrete(limits = c("Natural resource ratio", "Destination"), expand = c(0.075, 0.075)) +
  scale_y_continuous(labels = dollar_format()) +
  scale_fill_manual(values = wes_palette("Royal1")) +
  labs(title = "Trade mis-invoicing",
       subtitle = "according to extreme natural resource dependence and destination region",
       y = "Yearly average outflow in billion USD") +
  theme(legend.position = "none")
ggsave(g,
       file = "Figures/Sankey_Reporter Extreme Natural Resources by Partner Region_World.png",
       width = 6, height = 5, units = "in")


# .. Sankey diagram by natural resource dependence and partner GNI per capita ####
load("Results/Summary data-sets/GER_Orig_Dest_Avg.Rdata")

viz <- left_join(GER_Orig_Dest_Avg, natural_resources,
                 by = c("reporter.ISO")) %>%
  mutate(nat_cut = cut(nat_ratio, breaks = c(0, .20, .30, .40, 1),
                       labels = c("up to 20%", "21-30%",
                                  "31-40%", "more than 40%")),
         pcut = cut(pGNPpc, 
                    breaks = c(0, 15000, 30000, 45000, 60000, 125000),
                    labels = c("0$-15,000$", "15,001$-30,000$", 
                               "30,001$-45,000$", "45,001$-60,000$", 
                               "+60,001$"))) %>%
  group_by(nat_cut, pcut) %>%
  summarize(Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup() %>%
  filter(is.finite(nat_cut) & is.finite(pcut))

g <- ggplot(viz,
            aes(y = Tot_IFF_bn, axis1 = nat_cut, axis2 = pcut,
                label = after_stat(stratum))) +
  geom_alluvium(aes(fill = nat_cut)) +
  geom_stratum(width = 1/12, color = "black", alpha = 0.5) +
  geom_label(stat = "stratum", size = 3, hjust = "inward") +
  scale_x_discrete(limits = c("Natural resource ratio", "Destination GNI/capita"), expand = c(0.075, 0.075)) +
  scale_y_continuous(labels = dollar_format()) +
  scale_fill_manual(values = wes_palette("Darjeeling2")) +
  labs(title = "Trade mis-invoicing",
       subtitle = "according to natural resource dependence and destination GNI per capita",
       y = "Yearly average outflow in billion USD") +
  theme(legend.position = "none")
ggsave(g,
       file = "Figures/Sankey_Reporter Natural Resources by Partner GNI_World.png",
       width = 6, height = 5, units = "in")


# .. Sankey diagram by natural resource dependence and partner income group ####
load("Results/Summary data-sets/GER_Orig_Dest_Avg.Rdata")

viz <- left_join(GER_Orig_Dest_Avg, natural_resources,
                 by = c("reporter.ISO")) %>%
  mutate(nat_cut = cut(nat_ratio, breaks = c(0, .20, .30, .40, 1),
                       labels = c("up to 20%", "21-30%",
                                  "31-40%", "more than 40%")),
         pcut = cut(pGNPpc, 
                    breaks = c(0, 1035, 4045, 12535, 125000),
                    labels = c("Low income", "Lower-middle income", 
                               "Upper-middle income", "High income"))) %>%
  group_by(nat_cut, pcut) %>%
  summarize(Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup() %>%
  filter(is.finite(nat_cut) & is.finite(pcut))

g <- ggplot(viz,
            aes(y = Tot_IFF_bn, axis1 = nat_cut, axis2 = pcut,
                label = after_stat(stratum))) +
  geom_alluvium(aes(fill = pcut)) +
  geom_stratum(width = 1/12, color = "black", alpha = 0.5) +
  geom_label(stat = "stratum", size = 3, hjust = "inward") +
  scale_x_discrete(limits = c("Natural resource ratio", "Destination income group"), expand = c(0.075, 0.075)) +
  scale_y_continuous(labels = dollar_format()) +
  scale_fill_manual(values = wes_palette("GrandBudapest2")) +
  labs(title = "Trade mis-invoicing",
       subtitle = "according to natural resource dependence and destination income group",
       y = "Yearly average outflow in billion USD") +
  theme(legend.position = "none")
ggsave(g,
       file = "Figures/Sankey_Reporter Natural Resources by Partner Income Group_World.png",
       width = 6, height = 5, units = "in")


# .. Sankey diagram by reporter and partner income group ####
load("Results/Summary data-sets/GER_Orig_Dest_Avg.Rdata")

viz <- left_join(GER_Orig_Dest_Avg, natural_resources,
                 by = c("reporter.ISO")) %>%
  mutate(rcut = cut(rGNPpc, 
                    breaks = c(0, 1035, 4045, 12535, 125000),
                    labels = c("Low income", "Lower-middle income", 
                               "Upper-middle income", "High income")),
         pcut = cut(pGNPpc, 
                    breaks = c(0, 1035, 4045, 12535, 125000),
                    labels = c("Low income", "Lower-middle income", 
                               "Upper-middle income", "High income"))) %>%
  group_by(rcut, pcut) %>%
  summarize(Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup() %>%
  filter(is.finite(rcut) & is.finite(pcut))

g <- ggplot(viz,
            aes(y = Tot_IFF_bn, axis1 = rcut, axis2 = pcut,
                label = after_stat(stratum))) +
  geom_alluvium(aes(fill = rcut)) +
  geom_stratum(width = 1/12, color = "black", alpha = 0.5) +
  geom_label(stat = "stratum", size = 3, hjust = "inward") +
  scale_x_discrete(limits = c("Origin income group", "Destination income group"), expand = c(0.075, 0.075)) +
  scale_y_continuous(labels = dollar_format()) +
  # scale_fill_manual(values = wes_palette("IsleofDogs1")) +
  labs(title = "Trade mis-invoicing",
       subtitle = "according to origin and destination income group",
       y = "Yearly average outflow in billion USD") +
  theme(legend.position = "none")
ggsave(g,
       file = "Figures/Sankey_Reporter Income Group by Partner Income Group_World.png",
       width = 6, height = 5, units = "in")
