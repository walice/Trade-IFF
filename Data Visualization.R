# Data Visualization
# Alice Lepissier
# alice.lepissier@gmail.com
# Prepared for UNECA

## ## ## ## ## ## ## ## ## ## ##
# INDEX                     ####
## ## ## ## ## ## ## ## ## ## ##
# Preamble
# Codes Masterlist
# Yearly IFF Africa
# Yearly IFF LMIC
# Yearly IFF Developing
# Yearly IFF Pilots
# .. Line chart for gross estimates
# .. Line chart for net estimates
# .. Line chart for gross and net estimates
# Yearly IFF Conduits
# .. Line chart for conduits in Africa
# .. Line chart for conduits in LMIC
# .. Line chart for conduits in Developing
# Average IFF Africa
# .. Merge geographic data
# .. Average gross IFF
# .. Average net IFF
# Average IFF World
# .. Merge geographic data
# .. Average gross IFF
# Origins and Destinations
# .. Origins pie chart Africa
# .. Origins pie chart World
# .. Destinations pie chart Africa
# .. Destinations pie chart World
# .. Origins sunburst World
# .. Destinations sunburst World
# Sector Charts
# .. Treemap in Africa
# .. Treemap in LMIC
# .. Treemap in Developing
# .. Stacked bar chart of commodities in top sector in LMIC
# .. Stacked bar charts of top average outflows in pilots
# .. Pie charts of top cumulative outflows in pilots
# Destination Charts
# .. Stacked bar charts of top average outflows in Africa
# .. Stacked bar charts of top average outflows in LMIC
# .. Stacked bar charts of top average outflows in Developing
# .. Stacked bar charts of top average outflows in pilots
# .. Flow maps of top total destinations in pilots
# .. Flow maps of top destinations in conduits
# .. Flow maps of top inflows
# Conduits Charts
# .. Top conduits in World
# .. Top conduits in Africa
# .. Top conduits in LMIC
# .. Top conduits in Developing
# Sankey Diagram
# .. Sankey diagram by GNI per capita and sector
# .. Sankey diagram by reporter and partner GNI per capita
# Country Outlines



## ## ## ## ## ## ## ## ## ## ##
# PREAMBLE                  ####
## ## ## ## ## ## ## ## ## ## ##

#setwd("C:/cloudstorage/googledrive/Projects/UN Consultancy/Illicit Financial Flows/IFF estimates") # Alice work
#setwd("D:/Google Drive/Projects/UN Consultancy/Illicit Financial Flows/IFF estimates") # Alice laptop
setwd("/home/alice/IFFe/") # Virtual server
library(geosphere)
library(ggalluvial)
library(ggmap)
library(ggpubr)
library(ggrepel)
library(ggsunburst)
library(mapproj)
library(RColorBrewer)
library(reshape2)
library(scales)
library(stringr)
library(tidyverse)
library(treemapify)
library(xlsx)



## ## ## ## ## ## ## ## ## ## ##
# CODES MASTERLIST          ####
## ## ## ## ## ## ## ## ## ## ##

codes <- read.xlsx2("Data/Codes_Masterlist.xlsx", sheetName = "Codes") %>%
  mutate_all(as.character)



## ## ## ## ## ## ## ## ## ## ##
# YEARLY IFF AFRICA         ####
## ## ## ## ## ## ## ## ## ## ##

load("Results/Summary data-sets/GER_Year_Africa.Rdata")
load("Results/Summary data-sets/Net_Year_Africa.Rdata")

g <- ggplot(GER_Year_Africa %>% 
              mutate(year = as.character(year)) %>%
              melt(id.vars = "year") %>%
              filter(str_detect(variable, "Imp")), 
            aes(x = year, y = value, fill = variable)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = dollar_format(scale = 1/10^9, accuracy = 1)) +
  scale_fill_brewer(name = "Estimate", labels = c("Low", "High"),
                    type = "qual", palette = "Set2", direction = -1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Trade mis-invoicing in imports in Africa",
       subtitle = "Gross outflows",
       x = "Year", y = "Illicit flow in billion USD")
ggsave(g,
       file = "Figures/GER Africa Import.png",
       width = 6, height = 5, units = "in")

g <- ggplot(GER_Year_Africa %>% 
              mutate(year = as.character(year)) %>%
              melt(id.vars = "year") %>%
              filter(str_detect(variable, "Exp")), 
            aes(x = year, y = value, fill = variable)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = dollar_format(scale = 1/10^9, accuracy = 1)) +
  scale_fill_brewer(name = "Estimate", labels = c("Low", "High"),
                    type = "qual", palette = "Set2", direction = -1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Trade mis-invoicing in exports in Africa",
       subtitle = "Gross outflows",
       x = "Year", y = "Illicit flow in billion USD")
ggsave(g,
       file = "Figures/GER Africa Export.png",
       width = 6, height = 5, units = "in")

g <- ggplot(GER_Year_Africa %>% 
              mutate(year = as.character(year)) %>%
              melt(id.vars = "year") %>%
              filter(variable == "Tot_IFF_lo" | variable == "Tot_IFF_hi"), 
            aes(x = year, y = value, fill = variable)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = dollar_format(scale = 1/10^9, accuracy = 1)) +
  scale_fill_brewer(name = "Estimate", labels = c("Low", "High"),
                    type = "qual", palette = "Set2", direction = -1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Trade mis-invoicing in Africa",
       subtitle = "Gross outflows",
       x = "Year", y = "Illicit flow in billion USD") +
  geom_text(aes(label = round(value/10^9)),
            size = 2.5, position = position_dodge(1), vjust = -0.4) +
  scale_x_discrete(expand = c(0.05, 0.05))
ggsave(g,
       file = "Figures/GER Africa Total.png",
       width = 6, height = 5, units = "in")

g <- ggplot(GER_Year_Africa %>% 
              mutate(year = as.character(year)) %>% 
              melt(id.vars = "year") %>%
              filter(variable == "Tot_IFF_lo_GDP" | variable == "Tot_IFF_hi_GDP"), 
            aes(x = year, y = value, fill = variable)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_brewer(name = "Estimate", labels = c("Low", "High"),
                    type = "qual", palette = "Set2", direction = -1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Trade mis-invoicing in Africa",
       subtitle = "Gross outflows",
       x = "Year", y = "Illicit flow as % of GDP") +
  geom_text(aes(label = format(round(value*100, 1), nsmall = 1)),
            size = 2.5, position = position_dodge(1), vjust = -0.4) +
  scale_x_discrete(expand = c(0.05, 0.05))
ggsave(g,
       file = "Figures/GER Africa Total Percent GDP.png",
       width = 6, height = 5, units = "in")

g <- ggplot(GER_Year_Africa %>% 
              mutate(year = as.character(year)) %>% 
              melt(id.vars = "year") %>%
              filter(variable == "Tot_IFF_hi"), 
            aes(x = year, y = value, fill = variable)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = dollar_format(scale = 1/10^9, accuracy = 1)) +
  scale_fill_brewer(name = "Estimate", labels = c("High"),
                    type = "qual", palette = "Set2", direction = -1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  labs(title = "Trade mis-invoicing in Africa",
       subtitle = "Gross outflows",
       x = "", y = "Illicit flow in billion USD") +
  geom_text(aes(label = round(value/10^9)),
            size = 3, position = position_dodge(1), vjust = -0.4)
ggsave(g,
       file = "Figures/GER Africa Total high.png",
       width = 6, height = 5, units = "in")

g <- ggplot(GER_Year_Africa %>% 
              mutate(year = as.character(year)) %>% 
              melt(id.vars = "year") %>%
              filter(variable == "Tot_IFF_hi_GDP"), 
            aes(x = year, y = value, fill = variable)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_brewer(name = "Estimate", labels = c("High"),
                    type = "qual", palette = "Set2", direction = -1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  labs(title = "Trade mis-invoicing in Africa",
       subtitle = "Gross outflows",
       x = "", y = "Illicit flow as % of GDP") +
  geom_text(aes(label = format(round(value*100, 1), nsmall = 1)),
            size = 3, position = position_dodge(1), vjust = -0.4)
ggsave(g,
       file = "Figures/GER Africa Total Percent GDP high.png",
       width = 6, height = 5, units = "in")

g <- ggplot(Net_Year_Africa %>% 
              mutate(year = as.character(year)) %>% 
              melt(id.vars = "year") %>%
              filter(str_detect(variable, "Imp")), 
            aes(x = year, y = value, fill = variable)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = dollar_format(scale = 1/10^9, accuracy = 1))+
  scale_fill_brewer(name = "Estimate", labels = c("Low", "High"),
                    type = "qual", palette = "Set2", direction = -1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Trade mis-invoicing in imports in Africa",
       subtitle = "Net illicit flows",
       x = "Year", y = "Illicit flow in billion USD")
ggsave(g,
       file = "Figures/Net Africa Import.png",
       width = 6, height = 5, units = "in")

g <- ggplot(Net_Year_Africa %>% 
              mutate(year = as.character(year)) %>%
              melt(id.vars = "year") %>%
              filter(str_detect(variable, "Exp")), 
            aes(x = year, y = value, fill = variable)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = dollar_format(scale = 1/10^9, accuracy = 1)) +
  scale_fill_brewer(name = "Estimate", labels = c("Low", "High"),
                    type = "qual", palette = "Set2", direction = -1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Trade mis-invoicing in exports in Africa",
       subtitle = "Net illicit flows",
       x = "Year", y = "Illicit flow in billion USD")
ggsave(g,
       file = "Figures/Net Africa Export.png",
       width = 6, height = 5, units = "in")

g <- ggplot(Net_Year_Africa %>% 
              mutate(year = as.character(year)) %>% 
              melt(id.vars = "year") %>%
              filter(variable == "Tot_IFF_lo" | variable == "Tot_IFF_hi"), 
            aes(x = year, y = value, fill = variable)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = dollar_format(scale = 1/10^9, accuracy = 1)) +
  scale_fill_brewer(name = "Estimate", labels = c("Low", "High"),
                    type = "qual", palette = "Set2", direction = -1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Trade mis-invoicing in Africa",
       subtitle = "Net illicit flows",
       x = "Year", y = "Illicit flow in billion USD") +
  geom_text(aes(label = round(value/10^9)),
            size = 2.5, position = position_dodge(1), vjust = -0.4) +
  scale_x_discrete(expand = c(0.05, 0.05))
ggsave(g,
       file = "Figures/Net Africa Total.png",
       width = 6, height = 5, units = "in")

g <- ggplot(Net_Year_Africa %>% 
              mutate(year = as.character(year)) %>% 
              melt(id.vars = "year") %>%
              filter(variable == "Tot_IFF_lo_GDP" | variable == "Tot_IFF_hi_GDP"), 
            aes(x = year, y = value, fill = variable)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_brewer(name = "Estimate", labels = c("Low", "High"),
                    type = "qual", palette = "Set2", direction = -1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Trade mis-invoicing in Africa",
       subtitle = "Net illicit flows",
       x = "Year", y = "Illicit flow as % of GDP") +
  geom_text(aes(label = format(round(value*100, 1), nsmall = 1)),
            size = 2.5, position = position_dodge(1), vjust = -0.4) +
  scale_x_discrete(expand = c(0.05, 0.05))
ggsave(g,
       file = "Figures/Net Africa Total Percent GDP.png",
       width = 6, height = 5, units = "in")

g <- ggplot(Net_Year_Africa %>% 
              mutate(year = as.character(year)) %>% 
              melt(id.vars = "year") %>%
              filter(variable == "Tot_IFF_hi"), 
            aes(x = year, y = value, fill = variable)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = dollar_format(scale = 1/10^9, accuracy = 1)) +
  scale_fill_brewer(name = "Estimate", labels = c("High"),
                    type = "qual", palette = "Set2", direction = -1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  labs(title = "Trade mis-invoicing in Africa",
       subtitle = "Net illicit flows",
       x = "", y = "Illicit flow in billion USD") +
  geom_text(aes(label = round(value/10^9)),
            size = 3, position = position_dodge(1), vjust = -0.4)
ggsave(g,
       file = "Figures/Net Africa Total high.png",
       width = 6, height = 5, units = "in")

g <- ggplot(Net_Year_Africa %>% 
              mutate(year = as.character(year)) %>% 
              melt(id.vars = "year") %>%
              filter(variable == "Tot_IFF_hi_GDP"), 
            aes(x = year, y = value, fill = variable)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_brewer(name = "Estimate", labels = c("High"),
                    type = "qual", palette = "Set2", direction = -1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  labs(title = "Trade mis-invoicing in Africa",
       subtitle = "Net illicit flows",
       x = "", y = "Illicit flow as % of GDP") +
  geom_text(aes(label = format(round(value*100, 1), nsmall = 1)),
            size = 3, position = position_dodge(1), vjust = -0.4)
ggsave(g,
       file = "Figures/Net Africa Total Percent GDP high.png",
       width = 6, height = 5, units = "in")



## ## ## ## ## ## ## ## ## ## ##
# YEARLY IFF LMIC           ####
## ## ## ## ## ## ## ## ## ## ##

load("Results/Summary data-sets/GER_Year_LMIC.Rdata")
load("Results/Summary data-sets/Net_Year_LMIC.Rdata")

viz <- full_join(GER_Year_LMIC %>% 
                   select(year, 
                          GER_Tot_IFF_hi = Tot_IFF_hi, 
                          GER_Tot_IFF_hi_GDP = Tot_IFF_hi_GDP, 
                          GER_Tot_IFF_hi_Trade = Tot_IFF_hi_trade),
                 Net_Year_LMIC %>%
                   select(year, 
                          Net_Tot_IFF_hi = Tot_IFF_hi, 
                          Net_Tot_IFF_hi_GDP = Tot_IFF_hi_GDP, 
                          Net_Tot_IFF_hi_Trade = Tot_IFF_hi_trade),
                 by = c("year"))

g <- ggplot(viz %>% 
              mutate(year = as.character(year)) %>% 
              melt(id.vars = "year") %>%
              filter(variable == "Net_Tot_IFF_hi" | variable == "GER_Tot_IFF_hi"), 
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
       file = "Figures/GER and Net LMIC Total high.png",
       width = 6, height = 5, units = "in")

g <- ggplot(viz %>% 
              mutate(year = as.character(year)) %>% 
              melt(id.vars = "year") %>%
              filter(variable == "Net_Tot_IFF_hi_GDP" | variable == "GER_Tot_IFF_hi_GDP"), 
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
       file = "Figures/GER and Net LMIC Total Percent GDP high.png",
       width = 6, height = 5, units = "in")

g <- ggplot(viz %>% 
              mutate(year = as.character(year)) %>% 
              melt(id.vars = "year") %>%
              filter(variable == "Net_Tot_IFF_hi_Trade" | variable == "GER_Tot_IFF_hi_Trade"), 
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
       file = "Figures/GER and Net LMIC Total Percent Trade high.png",
       width = 6, height = 5, units = "in")



## ## ## ## ## ## ## ## ## ## ##
# YEARLY IFF DEVELOPING     ####
## ## ## ## ## ## ## ## ## ## ##

load("Results/Summary data-sets/GER_Year_Developing.Rdata")
load("Results/Summary data-sets/Net_Year_Developing.Rdata")

viz <- full_join(GER_Year_Developing %>% 
                   select(year, 
                          GER_Tot_IFF_hi = Tot_IFF_hi, 
                          GER_Tot_IFF_hi_GDP = Tot_IFF_hi_GDP, 
                          GER_Tot_IFF_hi_Trade = Tot_IFF_hi_trade),
                 Net_Year_Developing %>%
                   select(year, 
                          Net_Tot_IFF_hi = Tot_IFF_hi, 
                          Net_Tot_IFF_hi_GDP = Tot_IFF_hi_GDP, 
                          Net_Tot_IFF_hi_Trade = Tot_IFF_hi_trade),
                 by = c("year"))

g <- ggplot(viz %>% 
              mutate(year = as.character(year)) %>% 
              melt(id.vars = "year") %>%
              filter(variable == "Net_Tot_IFF_hi" | variable == "GER_Tot_IFF_hi"), 
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
       file = "Figures/GER and Net Developing Total high.png",
       width = 6, height = 5, units = "in")

g <- ggplot(viz %>% 
              mutate(year = as.character(year)) %>% 
              melt(id.vars = "year") %>%
              filter(variable == "Net_Tot_IFF_hi_GDP" | variable == "GER_Tot_IFF_hi_GDP"), 
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
       file = "Figures/GER and Net Developing Total Percent GDP high.png",
       width = 6, height = 5, units = "in")

g <- ggplot(viz %>% 
              mutate(year = as.character(year)) %>% 
              melt(id.vars = "year") %>%
              filter(variable == "Net_Tot_IFF_hi_Trade" | variable == "GER_Tot_IFF_hi_Trade"), 
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
       file = "Figures/GER and Net Developing Total Percent Trade high.png",
       width = 6, height = 5, units = "in")



## ## ## ## ## ## ## ## ## ## ##
# YEARLY IFF PILOTS         ####
## ## ## ## ## ## ## ## ## ## ##

pilots <- c("EGY", "NGA", "SEN", "ZAF", "TZA", "TUN")
labels.hilo <- c(Tot_IFF_lo = "Low estimates", 
                 Tot_IFF_hi = "High estimates",
                 Tot_IFF_lo_GDP = "Low estimates", 
                 Tot_IFF_hi_GDP = "High estimates")
labels.grne <- c(Net_Tot_IFF_hi = "Net estimates",
                 GER_Tot_IFF_hi = "Gross estimates", 
                 Net_Tot_IFF_hi_GDP = "Net estimates",
                 GER_Tot_IFF_hi_GDP = "Gross estimates")


# .. Line chart for gross estimates ####
load("Results/Summary data-sets/GER_Orig_Year_Africa.Rdata")

Pilot_Year <- GER_Orig_Year_Africa %>%
  filter(reporter.ISO %in% pilots)

g <- ggplot(Pilot_Year %>% 
              melt(id.vars = c("year", "reporter")) %>%
              filter(variable == "Tot_IFF_lo" | variable == "Tot_IFF_hi") %>%
              mutate(year = as.numeric(year),
                     value = as.numeric(value),
                     reporter = as.factor(reporter))) +
  geom_line(aes(x = year, y = value/10^9, color = reporter)) +
  scale_color_viridis_d(option = "inferno") +
  facet_wrap(~variable, labeller = labeller(variable = labels.hilo)) +
  labs(title = "Gross yearly outflows in pilot countries",
       x = "Year",
       y = "Illicit flow in billion USD",
       color = "")
ggsave(g,
       file = "Figures/Pilots/Gross Yearly Pilots.png",
       width = 6, height = 5, units = "in")

g <- ggplot(Pilot_Year %>% 
              melt(id.vars = c("year", "reporter")) %>%
              filter(variable == "Tot_IFF_lo_GDP" | variable == "Tot_IFF_hi_GDP") %>%
              mutate(year = as.numeric(year),
                     value = as.numeric(value),
                     reporter = as.factor(reporter))) +
  geom_line(aes(x = year, y = value*100, color = reporter)) +
  scale_color_viridis_d(option = "inferno") +
  facet_wrap(~variable, labeller = labeller(variable = labels.hilo)) +
  labs(title = "Gross yearly outflows in pilot countries",
       x = "Year",
       y = "Illicit flow as % of GDP",
       color = "")
ggsave(g,
       file = "Figures/Pilots/Gross Yearly Pilots Percent GDP.png",
       width = 6, height = 5, units = "in")


# .. Line chart for net estimates ####
load("Results/Summary data-sets/Net_Orig_Year_Africa.Rdata")

Pilot_Year <- Net_Orig_Year_Africa %>%
  filter(reporter.ISO %in% pilots)

g <- ggplot(Pilot_Year %>% 
              melt(id.vars = c("year", "reporter")) %>%
              filter(variable == "Tot_IFF_lo" | variable == "Tot_IFF_hi") %>%
              mutate(year = as.numeric(year),
                     value = as.numeric(value),
                     reporter = as.factor(reporter))) +
  geom_line(aes(x = year, y = value/10^9, color = reporter)) +
  scale_color_viridis_d(option = "inferno") +
  facet_wrap(~variable, labeller = labeller(variable = labels.hilo)) +
  labs(title = "Net yearly flows in pilot countries",
       x = "Year",
       y = "Illicit flow in billion USD",
       color = "")
ggsave(g,
       file = "Figures/Pilots/Net Yearly Pilots.png",
       width = 6, height = 5, units = "in")

g <- ggplot(Pilot_Year %>% 
              melt(id.vars = c("year", "reporter")) %>%
              filter(variable == "Tot_IFF_lo_GDP" | variable == "Tot_IFF_hi_GDP") %>%
              mutate(year = as.numeric(year),
                     value = as.numeric(value),
                     reporter = as.factor(reporter))) +
  geom_line(aes(x = year, y = value*100, color = reporter)) +
  scale_color_viridis_d(option = "inferno") +
  facet_wrap(~variable, labeller = labeller(variable = labels.hilo)) +
  labs(title = "Net yearly flows in pilot countries",
       x = "Year",
       y = "Illicit flow as % of GDP",
       color = "")
ggsave(g,
       file = "Figures/Pilots/Net Yearly Pilots Percent GDP.png",
       width = 6, height = 5, units = "in") 


# .. Line chart for gross and net estimates ####
Pilot_Gross <- GER_Orig_Year_Africa %>%
  filter(reporter.ISO %in% pilots)
Pilot_Net <- Net_Orig_Year_Africa %>%
  filter(reporter.ISO %in% pilots)

Pilot_Year <- full_join(Pilot_Gross %>%
                          rename_at(vars(starts_with("Tot")), funs(paste0("GER_", .))), 
                        Pilot_Net %>%
                          rename_at(vars(starts_with("Tot")), funs(paste0("Net_", .))),
                        by = c("reporter" = "reporter",
                               "reporter.ISO" = "reporter.ISO",
                               "year" = "year"))

g <- ggplot(Pilot_Year %>% 
              melt(id.vars = c("year", "reporter")) %>%
              filter(variable == "Net_Tot_IFF_hi" | variable == "GER_Tot_IFF_hi") %>%
              mutate(year = as.numeric(year),
                     value = as.numeric(value),
                     reporter = as.factor(reporter))) +
  geom_line(aes(x = year, y = value/10^9, color = reporter)) +
  scale_color_viridis_d(option = "inferno") +
  facet_wrap(~variable, labeller = labeller(variable = labels.grne)) +
  labs(title = "Yearly flows in pilot countries",
       x = "Year",
       y = "Illicit flow in billion USD",
       color = "")
ggsave(g,
       file = "Figures/Pilots/Gross and Net Yearly Pilots.png",
       width = 6, height = 5, units = "in") 

g <- ggplot(Pilot_Year %>% 
              melt(id.vars = c("year", "reporter")) %>%
              filter(variable == "GER_Tot_IFF_hi_GDP" | variable == "Net_Tot_IFF_hi_GDP") %>%
              mutate(year = as.numeric(year),
                     value = as.numeric(value),
                     reporter = as.factor(reporter))) +
  geom_line(aes(x = year, y = value*100, color = reporter)) +
  scale_color_viridis_d(option = "inferno") +
  facet_wrap(~variable, labeller = labeller(variable = labels.grne)) +
  labs(title = "Yearly flows in pilot countries",
       x = "Year",
       y = "Illicit flow as % of GDP",
       color = "")
ggsave(g,
       file = "Figures/Pilots/Gross and Net Yearly Pilots Percent GDP.png",
       width = 6, height = 5, units = "in")



## ## ## ## ## ## ## ## ## ## ##
# YEARLY IFF CONDUITS       ####
## ## ## ## ## ## ## ## ## ## ##

labels.grne <- c(GER_Tot_IFF_hi_GDP = "Gross estimates", 
                 Net_Tot_IFF_hi_GDP = "Net estimates")


# .. Line chart for conduits in Africa ####
load("Results/Summary data-sets/GER_Orig_Year_Africa.Rdata")
load("Results/Summary data-sets/Net_Orig_Year_Africa.Rdata")
conduits <- c("MUS", "UGA", "MWI", "SYC", "MLI")

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
              filter(variable == "GER_Tot_IFF_hi_GDP" | variable == "Net_Tot_IFF_hi_GDP") %>%
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
       file = "Figures/Gross and Net Yearly Conduits Africa Percent GDP.png",
       width = 6, height = 5, units = "in")


# .. Line chart for conduits in LMIC ####
load("Results/Summary data-sets/GER_Orig_Year_LMIC.Rdata")
load("Results/Summary data-sets/Net_Orig_Year_LMIC.Rdata")
conduits <- c("MDA", "PNG", "VNM", "NIC", "SLV")

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
              filter(variable == "GER_Tot_IFF_hi_GDP" | variable == "Net_Tot_IFF_hi_GDP") %>%
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
       file = "Figures/Gross and Net Yearly Conduits LMIC Percent GDP.png",
       width = 6, height = 5, units = "in")


# .. Line chart for conduits in Developing ####
load("Results/Summary data-sets/GER_Orig_Year_Developing.Rdata")
load("Results/Summary data-sets/Net_Orig_Year_Developing.Rdata")
conduits <- c("SGP", "HKG", "MDV", "GUY", "PLW")

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
              filter(variable == "GER_Tot_IFF_hi_GDP" | variable == "Net_Tot_IFF_hi_GDP") %>%
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
       file = "Figures/Gross and Net Yearly Conduits Developing Percent GDP.png",
       width = 6, height = 5, units = "in")



## ## ## ## ## ## ## ## ## ## ##
# AVERAGE IFF AFRICA        ####
## ## ## ## ## ## ## ## ## ## ##

# .. Merge geographic data ####
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


# .. Average gross IFF ####
load("Results/Summary data-sets/GER_Orig_Avg_Africa.Rdata")

viz <- left_join(map, GER_Orig_Avg_Africa,
                 by = c("ISO3166.3" = "reporter.ISO"))

g <- ggplot() + 
  geom_polygon(data = viz,
               aes(x = long, y = lat, group = group, 
                   fill = Tot_IFF_lo_bn), color = "white", lwd = 0.2) + 
  coord_fixed(1.3) +
  theme_bw() + 
  ditch_axes +
  scale_fill_viridis_c("IFF (billion USD)", direction = -1) +
  labs(title = "Total outflows averaged over 2000-2016",
       subtitle = "Gross outflows, low estimate")
ggsave(g,
       file = "Figures/GER Total Average IFF low.png",
       width = 6, height = 5, units = "in")

g <- ggplot() + 
  geom_polygon(data = viz,
               aes(x = long, y = lat, group = group, 
                   fill = Tot_IFF_hi_bn), color = "white", lwd = 0.2) + 
  coord_fixed(1.3) +
  theme_bw() + 
  ditch_axes +
  scale_fill_viridis_c("IFF (billion USD)", direction = -1) +
  labs(title = "Total outflows averaged over 2000-2016",
       subtitle = "Gross outflows, high estimate")
ggsave(g,
       file = "Figures/GER Total Average IFF high.png",
       width = 6, height = 5, units = "in")


# .. Average net IFF ####
load("Results/Summary data-sets/Net_Orig_Avg_Africa.Rdata")

viz <- left_join(map, Net_Orig_Avg_Africa,
                 by = c("ISO3166.3" = "reporter.ISO"))

g <- ggplot() + 
  geom_polygon(data = viz,
               aes(x = long, y = lat, group = group, 
                   fill = Tot_IFF_lo_bn), color = "white", lwd = 0.2) + 
  coord_fixed(1.3) +
  theme_bw() + 
  ditch_axes +
  scale_fill_viridis_c("IFF (billion USD)", direction = -1) +
  labs(title = "Total net flows averaged over 2000-2016",
       subtitle = "Low estimate")
ggsave(g,
       file = "Figures/Net Total Average IFF low.png",
       width = 6, height = 5, units = "in")

g <- ggplot() + 
  geom_polygon(data = viz,
               aes(x = long, y = lat, group = group, 
                   fill = Tot_IFF_hi_bn), color = "white", lwd = 0.2) + 
  coord_fixed(1.3) +
  theme_bw() + 
  ditch_axes +
  scale_fill_viridis_c("IFF (billion USD)", direction = -1) +
  labs(title = "Total net flows averaged over 2000-2016",
       subtitle = "High estimate")
ggsave(g,
       file = "Figures/Net Total Average IFF high.png",
       width = 6, height = 5, units = "in")



## ## ## ## ## ## ## ## ## ## ##
# AVERAGE IFF WORLD         ####
## ## ## ## ## ## ## ## ## ## ##

# .. Merge geographic data ####
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


# .. Average gross IFF ####
load("Results/Summary data-sets/GER_Orig_Avg.Rdata")

viz <- left_join(map, GER_Orig_Avg,
                 by = c("ISO3166.3" = "reporter.ISO"))

g <- ggplot() + 
  geom_polygon(data = viz,
               aes(x = long, y = lat, group = group, 
                   fill = Tot_IFF_hi_bn), color = "white", lwd = 0.2) + 
  coord_fixed(1.3) +
  theme_bw() + 
  ditch_axes +
  scale_fill_viridis_c("IFF (billion USD)", direction = -1,
                       breaks = c(50, 150)) +
  labs(title = "Total gross outflows averaged over 2000-2016") +
  theme(legend.position = "bottom") + 
  guides(fill = guide_colourbar(title.vjust = 0.8))
ggsave(g,
       file = "Figures/GER Total Average World IFF high.png",
       width = 6, height = 5, units = "in")

g <- ggplot() + 
  geom_polygon(data = viz,
               aes(x = long, y = lat, group = group, 
                   fill = Tot_IFF_hi_GDP*100), color = "white", lwd = 0.2) + 
  coord_fixed(1.3) +
  theme_bw() + 
  ditch_axes +
  scale_fill_viridis_c("IFF (% GDP)", direction = -1) +
  labs(title = "Total gross outflows averaged over 2000-2016") +
  theme(legend.position = "bottom") + 
  guides(fill = guide_colourbar(title.vjust = 0.8))
ggsave(g,
       file = "Figures/GER Total Average World IFF Percent GDP high.png",
       width = 6, height = 5, units = "in")

g <- ggplot() + 
  geom_polygon(data = viz,
               aes(x = long, y = lat, group = group, 
                   fill = Tot_IFF_hi_trade*100), color = "white", lwd = 0.2) + 
  coord_fixed(1.3) +
  theme_bw() + 
  ditch_axes +
  scale_fill_viridis_c("IFF (% trade)", direction = -1) +
  labs(title = "Total gross outflows averaged over 2000-2016") +
  theme(legend.position = "bottom") + 
  guides(fill = guide_colourbar(title.vjust = 0.8))
ggsave(g,
       file = "Figures/GER Total Average World IFF Percent Trade high.png",
       width = 6, height = 5, units = "in")



## ## ## ## ## ## ## ## ## ## ##
# ORIGINS AND DESTINATIONS  ####
## ## ## ## ## ## ## ## ## ## ##

# .. Origins pie chart Africa ####
load("Results/Summary data-sets/GER_Orig_Sum_Africa.Rdata")

Origins <- left_join(GER_Orig_Sum_Africa, codes %>% 
                       select(ISO3166.3, UN_Sub.region, UN_Intermediate_Region) %>%
                       distinct(ISO3166.3, .keep_all = TRUE),
                     by = c("reporter.ISO" = "ISO3166.3")) %>%
  mutate(Region = ifelse(UN_Intermediate_Region == "", UN_Sub.region, UN_Intermediate_Region),
         Region = ifelse(UN_Intermediate_Region == "Middle Africa", "Central Africa", Region)) %>%
  group_by(Region) %>%
  summarize(Tot_IFF_lo = sum(Tot_IFF_lo, na.rm = T),
            Tot_IFF_hi = sum(Tot_IFF_hi, na.rm = T)) %>%
  ungroup() %>%
  mutate(Pct_IFF_lo = Tot_IFF_lo / sum(Tot_IFF_lo) * 100,
         Pct_IFF_hi = Tot_IFF_hi / sum(Tot_IFF_hi) * 100)

g <- ggplot(Origins,
            aes(x = "", y = Pct_IFF_hi, fill = Region)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  geom_text(aes(label = paste0(round(Pct_IFF_hi), "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Origins of African outflows, 2000-2016") +
  theme_classic() +
  scale_fill_brewer(type = "qual", palette = "Pastel1") +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave(g,
       file = "Figures/Origins pie chart Africa.png",
       width = 6, height = 5, units = "in")


# .. Origins pie chart World ####
load("Results/Summary data-sets/GER_Orig_Sum.Rdata")

Origins <- GER_Orig_Sum %>%
  filter(rRegion != "") %>%
  group_by(rRegion) %>%
  summarize(Tot_IFF_lo = sum(Tot_IFF_lo, na.rm = T),
            Tot_IFF_hi = sum(Tot_IFF_hi, na.rm = T)) %>%
  ungroup() %>%
  mutate(Pct_IFF_lo = Tot_IFF_lo / sum(Tot_IFF_lo) * 100,
         Pct_IFF_hi = Tot_IFF_hi / sum(Tot_IFF_hi) * 100)

g <- ggplot(Origins,
            aes(x = "", y = Pct_IFF_hi, fill = rRegion)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  geom_text(aes(label = ifelse(Pct_IFF_hi < 3, round(Pct_IFF_hi), paste0(round(Pct_IFF_hi), "%"))), 
            position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Origins of global outflows, 2000-2016") +
  theme_classic() +
  scale_fill_brewer(type = "qual", palette = "Pastel1") +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave(g,
       file = "Figures/Origins pie chart World.png",
       width = 6, height = 5, units = "in")


# .. Destinations pie chart Africa ####
load("Results/Summary data-sets/GER_Dest_Africa.Rdata")

Destinations <- GER_Dest_Africa %>%
  filter(pRegion != "") %>%
  group_by(pRegion) %>%
  summarize(Tot_IFF_lo = sum(Tot_IFF_lo, na.rm = T),
            Tot_IFF_hi = sum(Tot_IFF_hi, na.rm = T)) %>%
  ungroup() %>%
  mutate(Pct_IFF_lo = Tot_IFF_lo / sum(Tot_IFF_lo) * 100,
         Pct_IFF_hi = Tot_IFF_hi / sum(Tot_IFF_hi) * 100)

g <- ggplot(Destinations,
            aes(x = "", y = Pct_IFF_hi, fill = pRegion)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  geom_text(aes(label = ifelse(Pct_IFF_hi < 3, round(Pct_IFF_hi), paste0(round(Pct_IFF_hi), "%"))), 
            position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Destinations of African outflows, 2000-2016") +
  theme_classic() +
  scale_fill_brewer(type = "qual", palette = "Pastel1") +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave(g,
       file = "Figures/Destinations pie chart Africa.png",
       width = 6, height = 5, units = "in")


# .. Destinations pie chart World ####
load("Results/Summary data-sets/GER_Dest.Rdata")

Destinations <- GER_Dest %>%
  filter(pRegion != "") %>%
  group_by(pRegion) %>%
  summarize(Tot_IFF_lo = sum(Tot_IFF_lo, na.rm = T),
            Tot_IFF_hi = sum(Tot_IFF_hi, na.rm = T)) %>%
  ungroup() %>%
  mutate(Pct_IFF_lo = Tot_IFF_lo / sum(Tot_IFF_lo) * 100,
         Pct_IFF_hi = Tot_IFF_hi / sum(Tot_IFF_hi) * 100)

g <- ggplot(Destinations,
            aes(x = "", y = Pct_IFF_hi, fill = pRegion)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  geom_text(aes(label = ifelse(Pct_IFF_hi < 4, round(Pct_IFF_hi), paste0(round(Pct_IFF_hi), "%"))), 
            position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Destinations of global outflows, 2000-2016") +
  theme_classic() +
  scale_fill_brewer(type = "qual", palette = "Pastel1") +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave(g,
       file = "Figures/Destinations pie chart World.png",
       width = 6, height = 5, units = "in")


# .. Origins sunburst World ####
load("Results/Summary data-sets/GER_Orig_Sum.Rdata")

Origins <- GER_Orig_Sum %>%
  filter(rRegion != "" & rIncome != "") %>%
  group_by(rRegion, rIncome) %>%
  summarize(Tot_IFF_hi = sum(Tot_IFF_hi, na.rm = T)) %>%
  ungroup() %>%
  mutate(Pct_IFF_hi = Tot_IFF_hi / sum(Tot_IFF_hi) * 100) %>%
  mutate(rRegion = factor(rRegion),
         parent = rRegion) %>%
  rename(node = rIncome,
         size = Pct_IFF_hi) %>%
  select(-Tot_IFF_hi) %>%
  mutate(node = factor(node, levels = c("LIC", "LMC", "UMC", "HIC"))) %>%
  arrange(parent, node)

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
       subtitle = "Cumulative gross outflows during 2000-2016",
       fill = "") +
  theme(plot.margin = unit(c(0.5, 0, 0, 0), "cm"))
ggsave(g,
       file = "Figures/Origins sunburst World.png",
       width = 6, height = 5, units = "in")


# .. Destinations sunburst World ####
load("Results/Summary data-sets/GER_Dest.Rdata")

Destinations <- GER_Dest %>%
  filter(pRegion != "" & pIncome != "") %>%
  group_by(pRegion, pIncome) %>%
  summarize(Tot_IFF_hi = sum(Tot_IFF_hi, na.rm = T)) %>%
  ungroup() %>%
  mutate(Pct_IFF_hi = Tot_IFF_hi / sum(Tot_IFF_hi) * 100) %>%
  mutate(pRegion = factor(pRegion),
         parent = pRegion) %>%
  rename(node = pIncome,
         size = Pct_IFF_hi) %>%
  select(-Tot_IFF_hi) %>%
  mutate(node = factor(node, levels = c("LIC", "LMC", "UMC", "HIC"))) %>%
  arrange(parent, node) %>%
  mutate(size = ifelse(pRegion == "Americas" & node == "LMC", 0.51, size)) # Just for rounding display, everything is correct

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
                label = ifelse(label != "Oceania" & label != "Africa", paste0(round(size), "%"), round(size))),
            size = 3) +
  scale_fill_brewer(type = "qual", palette = "Set3") +
  labs(title = "Top destinations",
       subtitle = "Cumulative gross outflows during 2000-2016",
       fill = "") +
  theme(plot.margin = unit(c(0.5, 0, 0, 0), "cm"))
ggsave(g,
       file = "Figures/Destinations sunburst World.png",
       width = 6, height = 5, units = "in")



## ## ## ## ## ## ## ## ## ## ##
# SECTOR CHARTS             ####
## ## ## ## ## ## ## ## ## ## ##


tol21rainbow <- c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#117744", "#44AA77", "#88CCAA", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788")


# .. Treemap in Africa ####
load("Results/Summary data-sets/GER_Sect_Avg_Africa.Rdata")

g <- ggplot(GER_Sect_Avg_Africa %>%
              arrange(desc(Tot_IFF_hi_bn)),
            aes(area = Tot_IFF_hi_bn, fill = forcats::fct_inorder(section), label = section)) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "topleft", reflow = T) +
  geom_treemap_text(data = GER_Sect_Avg_Africa,
                    aes(label = ifelse(Tot_IFF_hi_bn >= sort(GER_Sect_Avg_Africa$Tot_IFF_hi_bn, decreasing = T)[6],
                                       paste0("$", round(Tot_IFF_hi_bn), " bn"),
                                       "")),
                    colour = "white", place = "bottomright", size = 12) +
  theme(legend.position = "none") +
  scale_fill_manual(values = tol21rainbow) +
  labs(title = "Top sectors in Africa",
       subtitle = "Average gross yearly outflow during 2000-2016")
ggsave(g,
       file = "Figures/Treemap sectors average Africa.png",
       width = 6, height = 5, units = "in")


# .. Treemap in LMIC ####
load("Results/Summary data-sets/GER_Sect_Avg_LMIC.Rdata")

g <- ggplot(GER_Sect_Avg_LMIC %>%
              arrange(desc(Tot_IFF_hi_bn)),
            aes(area = Tot_IFF_hi_bn, fill = forcats::fct_inorder(section), label = section)) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "topleft", reflow = T) +
  geom_treemap_text(data = GER_Sect_Avg_LMIC,
                    aes(label = ifelse(Tot_IFF_hi_bn >= sort(GER_Sect_Avg_LMIC$Tot_IFF_hi_bn, decreasing = T)[8],
                                       paste0("$", round(Tot_IFF_hi_bn), " bn"),
                                       "")),
                    colour = "white", place = "bottomright", size = 12) +
  theme(legend.position = "none") +
  scale_fill_manual(values = tol21rainbow) +
  labs(title = "Top sectors in low and lower middle income countries",
       subtitle = "Average gross yearly outflow during 2000-2016")
ggsave(g,
       file = "Figures/Treemap sectors average LMIC.png",
       width = 6, height = 5, units = "in")


# .. Treemap in Developing ####
load("Results/Summary data-sets/GER_Sect_Avg_Developing.Rdata")

g <- ggplot(GER_Sect_Avg_Developing %>%
              arrange(desc(Tot_IFF_hi_bn)),
            aes(area = Tot_IFF_hi_bn, fill = forcats::fct_inorder(section), label = section)) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "topleft", reflow = T) +
  geom_treemap_text(data = GER_Sect_Avg_Developing,
                    aes(label = ifelse(Tot_IFF_hi_bn >= sort(GER_Sect_Avg_Developing$Tot_IFF_hi_bn, decreasing = T)[8],
                                       paste0("$", round(Tot_IFF_hi_bn), " bn"),
                                       "")),
                    colour = "white", place = "bottomright", size = 12) +
  theme(legend.position = "none") +
  scale_fill_manual(values = tol21rainbow) +
  labs(title = "Top sectors in developing countries",
       subtitle = "Average gross yearly outflow during 2000-2016")
ggsave(g,
       file = "Figures/Treemap sectors average Developing.png",
       width = 6, height = 5, units = "in")


# .. Stacked bar chart of commodities in top sector in LMIC ####
load("Results/Summary data-sets/GER_Sect_Avg_LMIC_disag.Rdata")
load("Data/UN Stats/HS.Rdata")

viz <- left_join(GER_Sect_Avg_LMIC_disag, HS %>% select(-chapter.description),
                 by = c("commodity.code" = "chapter")) %>%
  filter(section == "Mineral Products" | section == "Machinery and Electrical") %>%
  arrange(desc(Tot_IFF_hi)) %>%
  mutate(commodity = factor(commodity,
                            levels = commodity[order(Tot_IFF_hi, decreasing = T)]))

g <- ggplot(viz,
            aes(x = fct_rev(section), y = Tot_IFF_hi/10^9, 
                fill = fct_inorder(str_wrap(commodity, 20)))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0("$", round(Tot_IFF_hi/10^9), " billion")), 
            position = position_stack(vjust = 0.5),
            size = 3) +
  labs(x = NULL, 
       y = "Illicit flow in billion USD", 
       fill = NULL, 
       title = "Breakdown of top sectors in low and lower middle income",
       subtitle = "Yearly average outflows during 2000-2016") +
  theme(legend.text = element_text(size = 8)) +
  scale_fill_brewer(type = "qual", palette = "Accent")
ggsave(g,
       file = "Figures/Top commodities in LMIC.png",
       width = 6, height = 5, units = "in")


# .. Stacked bar charts of top average outflows in pilots ####
load("Results/Summary data-sets/GER_Orig_Sect_Avg_Africa.Rdata")

# Egypt
viz <- GER_Orig_Sect_Avg_Africa %>%
  filter(reporter.ISO == "EGY") %>%
  top_n(5, Tot_IFF_hi) %>%
  arrange(desc(Tot_IFF_hi)) %>%
  mutate(section = factor(section,
                          levels = section[order(Tot_IFF_hi_bn, decreasing = T)]))

g <- ggplot(viz,
            aes(x = "", y = Tot_IFF_hi/10^6, fill = section)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0("$", round(Tot_IFF_hi/10^6), " million")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "Top 5 sectors in Egypt",
       subtitle = "Yearly average outflows during 2000-2016, high estimate") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave(g,
       file = "Figures/Pilots/Egypt top 5 sectors GER average high.png",
       width = 6, height = 5, units = "in")

# Nigeria
viz <- GER_Orig_Sect_Avg_Africa %>%
  filter(reporter.ISO == "NGA") %>%
  top_n(5, Tot_IFF_hi) %>%
  arrange(desc(Tot_IFF_hi)) %>%
  mutate(section = factor(section,
                          levels = section[order(Tot_IFF_hi_bn, decreasing = T)]))

g <- ggplot(viz,
            aes(x = "", y = Tot_IFF_hi/10^6, fill = section)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0("$", round(Tot_IFF_hi/10^6), " million")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "Top 5 sectors in Nigeria",
       subtitle = "Yearly average outflows during 2000-2016, high estimate") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave(g,
       file = "Figures/Pilots/Nigeria top 5 sectors GER average high.png",
       width = 6, height = 5, units = "in")

# Senegal
viz <- GER_Orig_Sect_Avg_Africa %>%
  filter(reporter.ISO == "SEN") %>%
  top_n(5, Tot_IFF_hi) %>%
  arrange(desc(Tot_IFF_hi)) %>%
  mutate(section = factor(section,
                          levels = section[order(Tot_IFF_hi_bn, decreasing = T)]))

g <- ggplot(viz,
            aes(x = "", y = Tot_IFF_hi/10^6, fill = section)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0("$", round(Tot_IFF_hi/10^6), " million")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "Top 5 sectors in Senegal",
       subtitle = "Yearly average outflows during 2000-2016, high estimate") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave(g,
       file = "Figures/Pilots/Senegal top 5 sectors GER average high.png",
       width = 6, height = 5, units = "in")

# South Africa
viz <- GER_Orig_Sect_Avg_Africa %>%
  filter(reporter.ISO == "ZAF") %>%
  top_n(5, Tot_IFF_hi) %>%
  arrange(desc(Tot_IFF_hi)) %>%
  mutate(section = factor(section,
                          levels = section[order(Tot_IFF_hi_bn, decreasing = T)]))

g <- ggplot(viz,
            aes(x = "", y = Tot_IFF_hi/10^6, fill = section)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0("$", round(Tot_IFF_hi/10^6), " million")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "Top 5 sectors in South Africa",
       subtitle = "Yearly average outflows during 2000-2016, high estimate") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave(g,
       file = "Figures/Pilots/South Africa top 5 sectors GER average high.png",
       width = 6, height = 5, units = "in")

# Tanzania
viz <- GER_Orig_Sect_Avg_Africa %>%
  filter(reporter.ISO == "TZA") %>%
  top_n(5, Tot_IFF_hi) %>%
  arrange(desc(Tot_IFF_hi)) %>%
  mutate(section = factor(section,
                          levels = section[order(Tot_IFF_hi_bn, decreasing = T)]))

g <- ggplot(viz,
            aes(x = "", y = Tot_IFF_hi/10^6, fill = section)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0("$", round(Tot_IFF_hi/10^6), " million")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "Top 5 sectors in Tanzania",
       subtitle = "Yearly average outflows during 2000-2016, high estimate") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave(g,
       file = "Figures/Pilots/Tanzania top 5 sectors GER average high.png",
       width = 6, height = 5, units = "in")

# Tunisia
viz <- GER_Orig_Sect_Avg_Africa %>%
  filter(reporter.ISO == "TUN") %>%
  top_n(5, Tot_IFF_hi) %>%
  arrange(desc(Tot_IFF_hi)) %>%
  mutate(section = factor(section,
                          levels = section[order(Tot_IFF_hi_bn, decreasing = T)]))

g <- ggplot(viz,
            aes(x = "", y = Tot_IFF_hi/10^6, fill = section)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0("$", round(Tot_IFF_hi/10^6), " million")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "Top 5 sectors in Tunisia",
       subtitle = "Yearly average outflows during 2000-2016, high estimate") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave(g,
       file = "Figures/Pilots/Tunisia top 5 sectors GER average high.png",
       width = 6, height = 5, units = "in")

# Sudan
viz <- GER_Orig_Sect_Avg_Africa %>%
  filter(reporter.ISO == "SDN") %>%
  top_n(5, Tot_IFF_hi) %>%
  arrange(desc(Tot_IFF_hi)) %>%
  mutate(section = factor(section,
                          levels = section[order(Tot_IFF_hi_bn, decreasing = T)]))

g <- ggplot(viz,
            aes(x = "", y = Tot_IFF_hi/10^6, fill = section)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0("$", round(Tot_IFF_hi/10^6), " million")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "Top 5 sectors in Sudan",
       subtitle = "Yearly average outflows during 2000-2016, high estimate") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave(g,
       file = "Figures/Pilots/Sudan top 5 sectors GER average high.png",
       width = 6, height = 5, units = "in")


# .. Pie charts of top cumulative outflows in pilots ####
load("Results/Summary data-sets/GER_Orig_Sect_Sum_Africa.Rdata")

# Egypt
EGY <- GER_Orig_Sect_Sum_Africa %>%
  filter(reporter.ISO == "EGY")

viz <- EGY %>%
  top_n(5, Tot_IFF_hi)

g <- ggplot(viz,
            aes(x = "", y = Tot_IFF_hi/10^9, fill = section)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0("$", round(Tot_IFF_hi/10^9), "bn")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "Top 5 sectors in Egypt",
       subtitle = "Cumulative gross outflows over 2000-2016, high estimate") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave(g,
       file = "Figures/Pilots/Egypt top 5 sectors GER sum high pie chart.png",
       width = 6, height = 5, units = "in")

# Nigeria
NGA <- GER_Orig_Sect_Sum_Africa %>%
  filter(reporter.ISO == "NGA")

viz <- NGA %>%
  top_n(5, Tot_IFF_hi)

g <- ggplot(viz,
            aes(x = "", y = Tot_IFF_hi/10^9, fill = section)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0("$", round(Tot_IFF_hi/10^9), "bn")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "Top 5 sectors in Nigeria",
       subtitle = "Cumulative gross outflows over 2000-2016, high estimate") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave(g,
       file = "Figures/Pilots/Nigeria top 5 sectors GER sum high pie chart.png",
       width = 6, height = 5, units = "in")

# Senegal
SEN <- GER_Orig_Sect_Sum_Africa %>%
  filter(reporter.ISO == "SEN")

viz <- SEN %>%
  top_n(5, Tot_IFF_hi)

g <- ggplot(viz,
            aes(x = "", y = Tot_IFF_hi/10^9, fill = section)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0("$", round(Tot_IFF_hi/10^9), "bn")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "Top 5 sectors in Senegal",
       subtitle = "Cumulative gross outflows over 2000-2016, high estimate") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave(g,
       file = "Figures/Pilots/Senegal top 5 sectors GER sum high pie chart.png",
       width = 6, height = 5, units = "in")

# South Africa
ZAF <- GER_Orig_Sect_Sum_Africa %>%
  filter(reporter.ISO == "ZAF")

viz <- ZAF %>%
  top_n(5, Tot_IFF_hi)

g <- ggplot(viz,
            aes(x = "", y = Tot_IFF_hi/10^9, fill = section)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0("$", round(Tot_IFF_hi/10^9), "bn")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "Top 5 sectors in South Africa",
       subtitle = "Cumulative gross outflows over 2000-2016, high estimate") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave(g,
       file = "Figures/Pilots/South Africa top 5 sectors GER sum high pie chart.png",
       width = 6, height = 5, units = "in")

# Tanzania
TZA <- GER_Orig_Sect_Sum_Africa %>%
  filter(reporter.ISO == "TZA")

viz <- TZA %>%
  top_n(5, Tot_IFF_hi)

g <- ggplot(viz,
            aes(x = "", y = Tot_IFF_hi/10^9, fill = section)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0("$", round(Tot_IFF_hi/10^9), "bn")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "Top 5 sectors in Tanzania",
       subtitle = "Cumulative gross outflows over 2000-2016, high estimate") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave(g,
       file = "Figures/Pilots/Tanzania top 5 sectors GER sum high pie chart.png",
       width = 6, height = 5, units = "in")

# Tunisia
TUN <- GER_Orig_Sect_Sum_Africa %>%
  filter(reporter.ISO == "TUN")

viz <- TUN %>%
  top_n(5, Tot_IFF_hi)

g <- ggplot(viz,
            aes(x = "", y = Tot_IFF_hi/10^9, fill = section)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0("$", round(Tot_IFF_hi/10^9), "bn")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "Top 5 sectors in Tunisia",
       subtitle = "Cumulative gross outflows over 2000-2016, high estimate") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave(g,
       file = "Figures/Pilots/Tunisia top 5 sectors GER sum high pie chart.png",
       width = 6, height = 5, units = "in")

# Sudan
SDN <- GER_Orig_Sect_Sum_Africa %>%
  filter(reporter.ISO == "SDN")

viz <- SDN %>%
  top_n(5, Tot_IFF_hi)

g <- ggplot(viz,
            aes(x = "", y = Tot_IFF_hi/10^9, fill = section)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0("$", round(Tot_IFF_hi/10^9), "bn")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "Top 5 sectors in Sudan",
       subtitle = "Cumulative gross outflows over 2000-2016, high estimate") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave(g,
       file = "Figures/Pilots/Sudan top 5 sectors GER sum high pie chart.png",
       width = 6, height = 5, units = "in")



## ## ## ## ## ## ## ## ## ## ##
# DESTINATIONS CHARTS       ####
## ## ## ## ## ## ## ## ## ## ##

# .. Stacked bar charts of top average outflows in Africa ####
load("Results/Summary data-sets/GER_Dest_Avg_Africa.Rdata")

g <- ggplot(GER_Dest_Avg_Africa %>%
              top_n(10, Tot_IFF_hi),
            aes(x = "", y = Tot_IFF_hi/10^9, fill = fct_reorder(partner, Tot_IFF_hi, .desc = T))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0("$", round(Tot_IFF_hi/10^9), " billion")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "Top 10 destinations in Africa",
       subtitle = "Yearly average outflows during 2000-2016") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave(g,
       file = "Figures/Top 10 destinations GER average Africa.png",
       width = 6, height = 5, units = "in")


# .. Stacked bar charts of top average outflows in LMIC ####
load("Results/Summary data-sets/GER_Dest_Avg_LMIC.Rdata")

g <- ggplot(GER_Dest_Avg_LMIC %>%
              top_n(10, Tot_IFF_hi),
            aes(x = "", y = Tot_IFF_hi/10^9, fill = fct_reorder(partner, Tot_IFF_hi, .desc = T))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0("$", round(Tot_IFF_hi/10^9), " billion")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "Top 10 destinations in low and lower middle income countries",
       subtitle = "Yearly average outflows during 2000-2016") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave(g,
       file = "Figures/Top 10 destinations GER average LMIC.png",
       width = 6, height = 5, units = "in")


# .. Stacked bar charts of top average outflows in Developing ####
load("Results/Summary data-sets/GER_Dest_Avg_Developing.Rdata")

g <- ggplot(GER_Dest_Avg_Developing %>%
              top_n(10, Tot_IFF_hi),
            aes(x = "", y = Tot_IFF_hi/10^9, fill = fct_reorder(partner, Tot_IFF_hi, .desc = T))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0("$", round(Tot_IFF_hi/10^9), " billion")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "Top 10 destinations in developing countries",
       subtitle = "Yearly average outflows during 2000-2016") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave(g,
       file = "Figures/Top 10 destinations GER average Developing.png",
       width = 6, height = 5, units = "in")


# .. Stacked bar charts of top average outflows in pilots ####
load("Results/Summary data-sets/GER_Orig_Dest_Avg_Africa.Rdata")

# Egypt
viz <- GER_Orig_Dest_Avg_Africa %>%
  filter(reporter.ISO == "EGY") %>%
  top_n(5, Tot_IFF_hi) %>%
  arrange(desc(Tot_IFF_hi)) %>%
  mutate(partner = factor(partner,
                          levels = partner[order(Tot_IFF_hi_bn, decreasing = T)]))

g <- ggplot(viz,
            aes(x = "", y = Tot_IFF_hi/10^6, fill = partner)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0("$", round(Tot_IFF_hi/10^6), " million")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "Top 5 destinations in Egypt",
       subtitle = "Yearly average outflows during 2000-2016, high estimate") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave(g,
       file = "Figures/Pilots/Egypt top 5 destinations GER average high.png",
       width = 6, height = 5, units = "in")

# Nigeria
viz <- GER_Orig_Dest_Avg_Africa %>%
  filter(reporter.ISO == "NGA") %>%
  top_n(5, Tot_IFF_hi) %>%
  arrange(desc(Tot_IFF_hi)) %>%
  mutate(partner = factor(partner,
                          levels = partner[order(Tot_IFF_hi_bn, decreasing = T)]))

g <- ggplot(viz,
            aes(x = "", y = Tot_IFF_hi/10^6, fill = partner)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0("$", round(Tot_IFF_hi/10^6), " million")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "Top 5 destinations in Nigeria",
       subtitle = "Yearly average outflows during 2000-2016, high estimate") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave(g,
       file = "Figures/Pilots/Nigeria top 5 destinations GER average high.png",
       width = 6, height = 5, units = "in")

# Senegal
viz <- GER_Orig_Dest_Avg_Africa %>%
  filter(reporter.ISO == "SEN") %>%
  top_n(5, Tot_IFF_hi) %>%
  arrange(desc(Tot_IFF_hi)) %>%
  mutate(partner = factor(partner,
                          levels = partner[order(Tot_IFF_hi_bn, decreasing = T)]))

g <- ggplot(viz,
            aes(x = "", y = Tot_IFF_hi/10^6, fill = partner)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0("$", round(Tot_IFF_hi/10^6), " million")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "Top 5 destinations in Senegal",
       subtitle = "Yearly average outflows during 2000-2016, high estimate") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave(g,
       file = "Figures/Pilots/Senegal top 5 destinations GER average high.png",
       width = 6, height = 5, units = "in")

# South Africa
viz <- GER_Orig_Dest_Avg_Africa %>%
  filter(reporter.ISO == "ZAF") %>%
  top_n(5, Tot_IFF_hi) %>%
  arrange(desc(Tot_IFF_hi)) %>%
  mutate(partner = factor(partner,
                          levels = partner[order(Tot_IFF_hi_bn, decreasing = T)]))

g <- ggplot(viz,
            aes(x = "", y = Tot_IFF_hi/10^6, fill = partner)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0("$", round(Tot_IFF_hi/10^6), " million")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "Top 5 destinations in South Africa",
       subtitle = "Yearly average outflows during 2000-2016, high estimate") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave(g,
       file = "Figures/Pilots/South Africa top 5 destinations GER average high.png",
       width = 6, height = 5, units = "in")

# Tanzania
viz <- GER_Orig_Dest_Avg_Africa %>%
  filter(reporter.ISO == "TZA") %>%
  top_n(5, Tot_IFF_hi) %>%
  arrange(desc(Tot_IFF_hi)) %>%
  mutate(partner = factor(partner,
                          levels = partner[order(Tot_IFF_hi_bn, decreasing = T)]))

g <- ggplot(viz,
            aes(x = "", y = Tot_IFF_hi/10^6, fill = partner)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0("$", round(Tot_IFF_hi/10^6), " million")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "Top 5 destinations in Tanzania",
       subtitle = "Yearly average outflows during 2000-2016, high estimate") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave(g,
       file = "Figures/Pilots/Tanzania top 5 destinations GER average high.png",
       width = 6, height = 5, units = "in")

# Tunisia
viz <- GER_Orig_Dest_Avg_Africa %>%
  filter(reporter.ISO == "TUN") %>%
  top_n(5, Tot_IFF_hi) %>%
  arrange(desc(Tot_IFF_hi)) %>%
  mutate(partner = factor(partner,
                          levels = partner[order(Tot_IFF_hi_bn, decreasing = T)]))

g <- ggplot(viz,
            aes(x = "", y = Tot_IFF_hi/10^6, fill = partner)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0("$", round(Tot_IFF_hi/10^6), " million")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "Top 5 destinations in Tunisia",
       subtitle = "Yearly average outflows during 2000-2016, high estimate") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave(g,
       file = "Figures/Pilots/Tunisia top 5 destinations GER average high.png",
       width = 6, height = 5, units = "in")

# Sudan
viz <- GER_Orig_Dest_Avg_Africa %>%
  filter(reporter.ISO == "SDN") %>%
  top_n(5, Tot_IFF_hi) %>%
  arrange(desc(Tot_IFF_hi)) %>%
  mutate(partner = factor(partner,
                          levels = partner[order(Tot_IFF_hi_bn, decreasing = T)]))

g <- ggplot(viz,
            aes(x = "", y = Tot_IFF_hi/10^6, fill = partner)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0("$", round(Tot_IFF_hi/10^6), " million")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "Top 5 destinations in Sudan",
       subtitle = "Yearly average outflows during 2000-2016, high estimate") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave(g,
       file = "Figures/Pilots/Sudan top 5 destinations GER average high.png",
       width = 6, height = 5, units = "in")


# .. Flow maps of top total destinations in pilots ####
load("Results/Summary data-sets/GER_Orig_Dest_Sum_Africa.Rdata")

centroids <- codes %>%
  dplyr::select(ISO3166.3, Longitude, Latitude) %>%
  mutate_at(vars(Longitude, Latitude),
            funs(as.numeric))

GER_Orig_Dest_Sum_Africa <- GER_Orig_Dest_Sum_Africa %>%
  left_join(centroids %>% distinct(ISO3166.3, .keep_all = T), by = c("reporter.ISO" = "ISO3166.3")) %>%
  rename(rLongitude = Longitude,
         rLatitude = Latitude) %>%
  left_join(centroids %>% distinct(ISO3166.3, .keep_all = T), by = c("partner.ISO" = "ISO3166.3"))%>%
  rename(pLongitude = Longitude,
         pLatitude = Latitude)

plot_my_connection = function( dep_lon, dep_lat, arr_lon, arr_lat, ...){
  inter <- gcIntermediate(c(dep_lon, dep_lat), c(arr_lon, arr_lat), n = 50, addStartEnd = TRUE, breakAtDateLine = F)             
  inter <- data.frame(inter)
  diff_of_lon = abs(dep_lon) + abs(arr_lon)
  if (diff_of_lon > 180) {
    lines(subset(inter, lon >= 0), ...)
    lines(subset(inter, lon < 0), ...)
  } else {
    lines(inter, ...)
  }
}

# Egypt
viz <- GER_Orig_Dest_Sum_Africa %>%
  filter(reporter.ISO == "EGY") %>%
  top_n(10, Tot_IFF_hi) %>%
  mutate(scale = round((10 - 1) * (Tot_IFF_hi - min(Tot_IFF_hi))/(max(Tot_IFF_hi) - min(Tot_IFF_hi)) + 1))

pdf("Figures/Pilots/Flow map Egypt.pdf", 
    height = 4.5, width = 6)
maps::map("world", col = "#f2f2f2", fill = TRUE, bg = "white", lwd = 0.05,
          mar = rep(0, 4), border = 0, wrap = c(-180, 180, NA)) 
for(i in 1:nrow(viz)){
  plot_my_connection(viz$rLongitude[i], viz$rLatitude[i], viz$pLongitude[i], viz$pLatitude[i], 
                     col = "skyblue", lwd = viz$scale[i])
}
points(x = viz$rLongitude, y = viz$rLatitude, col = "slateblue", cex = 2, pch = 20)
points(x = viz$pLongitude, y = viz$pLatitude, col = "slateblue", cex = 2, pch = 20)
viz2 <- viz[-c(1,2,5,7),]
text(viz2$partner, x = viz2$pLongitude, y = viz2$pLatitude, col = "slateblue", cex = 0.7, pos = 4) # right
viz2 <- viz[c(1,2,5),]
text(viz2$partner, x = viz2$pLongitude, y = viz2$pLatitude, col = "slateblue", cex = 0.7, pos = 3) # above
viz2 <- viz[c(7),]
text(viz2$partner, x = viz2$pLongitude, y = viz2$pLatitude, col = "slateblue", cex = 0.7, pos = 1) # below
title("Top 10 destinations of gross outflows, 2000-2016", cex.main = 0.8)
dev.off()

# Nigeria
viz <- GER_Orig_Dest_Sum_Africa %>%
  filter(reporter.ISO == "NGA") %>%
  top_n(10, Tot_IFF_hi) %>%
  mutate(scale = round((10 - 1) * (Tot_IFF_hi - min(Tot_IFF_hi))/(max(Tot_IFF_hi) - min(Tot_IFF_hi)) + 1))

pdf("Figures/Pilots/Flow map Nigeria.pdf", 
    height = 4.5, width = 6)
maps::map("world", col = "#f2f2f2", fill = TRUE, bg = "white", lwd = 0.05,
          mar = rep(0, 4), border = 0, wrap = c(-180, 180, NA)) 
for(i in 1:nrow(viz)){
  plot_my_connection(viz$rLongitude[i], viz$rLatitude[i], viz$pLongitude[i], viz$pLatitude[i], 
                     col = "skyblue", lwd = viz$scale[i])
}
points(x = viz$rLongitude, y = viz$rLatitude, col = "slateblue", cex = 2, pch = 20)
points(x = viz$pLongitude, y = viz$pLatitude, col = "slateblue", cex = 2, pch = 20)
viz2 <- viz[-c(3,8,9,1),]
text(viz2$partner, x = viz2$pLongitude, y = viz2$pLatitude, col = "slateblue", cex = 0.7, pos = 4) # right
viz2 <- viz[c(3,9),]
text(viz2$partner, x = viz2$pLongitude, y = viz2$pLatitude, col = "slateblue", cex = 0.7, pos = 3) # above
viz2 <- viz[c(1,8),]
text(viz2$partner, x = viz2$pLongitude, y = viz2$pLatitude, col = "slateblue", cex = 0.7, pos = 2) # left
title("Top 10 destinations of gross outflows, 2000-2016", cex.main = 0.8)
dev.off()

# Senegal
viz <- GER_Orig_Dest_Sum_Africa %>%
  filter(reporter.ISO == "SEN") %>%
  top_n(10, Tot_IFF_hi) %>%
  mutate(scale = round((10 - 1) * (Tot_IFF_hi - min(Tot_IFF_hi))/(max(Tot_IFF_hi) - min(Tot_IFF_hi)) + 1))

pdf("Figures/Pilots/Flow map Senegal.pdf", 
    height = 4.5, width = 6)
maps::map("world", col = "#f2f2f2", fill = TRUE, bg = "white", lwd = 0.05,
          mar = rep(0, 4), border = 0, wrap = c(-180, 180, NA)) 
for(i in 1:nrow(viz)){
  plot_my_connection(viz$rLongitude[i], viz$rLatitude[i], viz$pLongitude[i], viz$pLatitude[i], 
                     col = "skyblue", lwd = viz$scale[i])
}
points(x = viz$rLongitude, y = viz$rLatitude, col = "slateblue", cex = 2, pch = 20)
points(x = viz$pLongitude, y = viz$pLatitude, col = "slateblue", cex = 2, pch = 20)
viz2 <- viz[-c(7),]
text(viz2$partner, x = viz2$pLongitude, y = viz2$pLatitude, col = "slateblue", cex = 0.7, pos = 4) # right
viz2 <- viz[c(7),]
text(viz2$partner, x = viz2$pLongitude, y = viz2$pLatitude, col = "slateblue", cex = 0.7, pos = 2) # left
title("Top 10 destinations of gross outflows, 2000-2016", cex.main = 0.8)
dev.off()

# South Africa
viz <- GER_Orig_Dest_Sum_Africa %>%
  filter(reporter.ISO == "ZAF") %>%
  top_n(10, Tot_IFF_hi) %>%
  mutate(scale = round((10 - 1) * (Tot_IFF_hi - min(Tot_IFF_hi))/(max(Tot_IFF_hi) - min(Tot_IFF_hi)) + 1))

pdf("Figures/Pilots/Flow map South Africa.pdf", 
    height = 4.5, width = 6)
maps::map("world", col = "#f2f2f2", fill = TRUE, bg = "white", lwd = 0.05,
          mar = rep(0, 4), border = 0, wrap = c(-180, 180, NA)) 
for(i in 1:nrow(viz)){
  plot_my_connection(viz$rLongitude[i], viz$rLatitude[i], viz$pLongitude[i], viz$pLatitude[i], 
                     col = "skyblue", lwd = viz$scale[i])
}
points(x = viz$rLongitude, y = viz$rLatitude, col = "slateblue", cex = 2, pch = 20)
points(x = viz$pLongitude, y = viz$pLatitude, col = "slateblue", cex = 2, pch = 20)
viz2 <- viz[-c(3,9,4,10),]
text(viz2$partner, x = viz2$pLongitude, y = viz2$pLatitude, col = "slateblue", cex = 0.7, pos = 4) # right
viz2 <- viz[c(4,9),]
text(viz2$partner, x = viz2$pLongitude, y = viz2$pLatitude, col = "slateblue", cex = 0.7, pos = 2) # left
viz2 <- viz[c(3,10),]
text(viz2$partner, x = viz2$pLongitude, y = viz2$pLatitude, col = "slateblue", cex = 0.7, pos = 1) # below
title("Top 10 destinations of gross outflows, 2000-2016", cex.main = 0.8)
dev.off()

# Tanzania
viz <- GER_Orig_Dest_Sum_Africa %>%
  filter(reporter.ISO == "TZA") %>%
  top_n(10, Tot_IFF_hi) %>%
  mutate(scale = round((10 - 1) * (Tot_IFF_hi - min(Tot_IFF_hi))/(max(Tot_IFF_hi) - min(Tot_IFF_hi)) + 1))

pdf("Figures/Pilots/Flow map Tanzania.pdf", 
    height = 4.5, width = 6)
maps::map("world", col = "#f2f2f2", fill = TRUE, bg = "white", lwd = 0.05,
          mar = rep(0, 4), border = 0, wrap = c(-180, 180, NA)) 
for(i in 1:nrow(viz)){
  plot_my_connection(viz$rLongitude[i], viz$rLatitude[i], viz$pLongitude[i], viz$pLatitude[i], 
                     col = "skyblue", lwd = viz$scale[i])
}
points(x = viz$rLongitude, y = viz$rLatitude, col = "slateblue", cex = 2, pch = 20)
points(x = viz$pLongitude, y = viz$pLatitude, col = "slateblue", cex = 2, pch = 20)
viz2 <- viz[-c(8,9),]
text(viz2$partner, x = viz2$pLongitude, y = viz2$pLatitude, col = "slateblue", cex = 0.7, pos = 4) # right
viz2 <- viz[c(8,9),]
text(viz2$partner, x = viz2$pLongitude, y = viz2$pLatitude, col = "slateblue", cex = 0.7, pos = 3) # above
title("Top 10 destinations of gross outflows, 2000-2016", cex.main = 0.8)
dev.off()

# Tunisia
viz <- GER_Orig_Dest_Sum_Africa %>%
  filter(reporter.ISO == "TUN") %>%
  top_n(10, Tot_IFF_hi) %>%
  mutate(scale = round((10 - 1) * (Tot_IFF_hi - min(Tot_IFF_hi))/(max(Tot_IFF_hi) - min(Tot_IFF_hi)) + 1))

pdf("Figures/Pilots/Flow map Tunisia.pdf", 
    height = 4.5, width = 6)
maps::map("world", col = "#f2f2f2", fill = TRUE, bg = "white", lwd = 0.05,
          mar = rep(0, 4), border = 0, wrap = c(-180, 180, NA)) 
for(i in 1:nrow(viz)){
  plot_my_connection(viz$rLongitude[i], viz$rLatitude[i], viz$pLongitude[i], viz$pLatitude[i], 
                     col = "skyblue", lwd = viz$scale[i])
}
points(x = viz$rLongitude, y = viz$rLatitude, col = "slateblue", cex = 2, pch = 20)
points(x = viz$pLongitude, y = viz$pLatitude, col = "slateblue", cex = 2, pch = 20)
viz2 <- viz[-c(2,8,6),]
text(viz2$partner, x = viz2$pLongitude, y = viz2$pLatitude, col = "slateblue", cex = 0.7, pos = 4) # right
viz2 <- viz[c(2),]
text(viz2$partner, x = viz2$pLongitude, y = viz2$pLatitude, col = "slateblue", cex = 0.7, pos = 2) # left
viz2 <- viz[c(8,6),]
text(viz2$partner, x = viz2$pLongitude, y = viz2$pLatitude, col = "slateblue", cex = 0.7, pos = 1) # below
title("Top 10 destinations of gross outflows, 2000-2016", cex.main = 0.8)
dev.off()

# Sudan
viz <- GER_Orig_Dest_Sum_Africa %>%
  filter(reporter.ISO == "SDN") %>%
  top_n(10, Tot_IFF_hi) %>%
  mutate(scale = round((10 - 1) * (Tot_IFF_hi - min(Tot_IFF_hi))/(max(Tot_IFF_hi) - min(Tot_IFF_hi)) + 1))

pdf("Figures/Pilots/Flow map Sudan.pdf", 
    height = 4.5, width = 6)
maps::map("world", col = "#f2f2f2", fill = TRUE, bg = "white", lwd = 0.05,
          mar = rep(0, 4), border = 0, wrap = c(-180, 180, NA)) 
for(i in 1:nrow(viz)){
  plot_my_connection(viz$rLongitude[i], viz$rLatitude[i], viz$pLongitude[i], viz$pLatitude[i], 
                     col = "skyblue", lwd = viz$scale[i])
}
points(x = viz$rLongitude, y = viz$rLatitude, col = "slateblue", cex = 2, pch = 20)
points(x = viz$pLongitude, y = viz$pLatitude, col = "slateblue", cex = 2, pch = 20)
viz2 <- viz[-c(10,4,3,9,7),]
text(viz2$partner, x = viz2$pLongitude, y = viz2$pLatitude, col = "slateblue", cex = 0.7, pos = 4) # right
viz2 <- viz[c(3),]
text(viz2$partner, x = viz2$pLongitude, y = viz2$pLatitude, col = "slateblue", cex = 0.7, pos = 2) # left
viz2 <- viz[c(10,9),]
text(viz2$partner, x = viz2$pLongitude, y = viz2$pLatitude, col = "slateblue", cex = 0.7, pos = 3) # above
viz2 <- viz[c(4,7),]
text(viz2$partner, x = viz2$pLongitude, y = viz2$pLatitude, col = "slateblue", cex = 0.7, pos = 1) # below
title("Top 10 destinations of gross outflows, 2000-2016", cex.main = 0.8)
dev.off()


# .. Flow maps of top destinations in conduits ####
load("Results/Summary data-sets/GER_Orig_Dest_Avg_Africa.Rdata")
load("Results/Summary data-sets/GER_Orig_Dest_Avg_LMIC.Rdata")
load("Results/Summary data-sets/GER_Orig_Dest_Avg_Developing.Rdata")

conduits_Africa <- c("MUS", "UGA", "MWI", "SYC", "MLI", "AGO", "ZWE", "NER", "CIV", "KEN")
conduits_LMIC <- c("MDA", "PNG", "VNM", "NIC", "SLV", "KGZ", "VUT", "NPL", "HND", "UGA")
conduits_Developing <- c("SGP", "HKG", "MDV", "GUY", "PLW", "MYS", "PNG", "VNM", "THA", "NIC")

ditch_axes <- theme(axis.title.x = element_blank(),
                    axis.text.x = element_blank(),
                    axis.ticks.x = element_blank(),
                    axis.title.y = element_blank(),
                    axis.text.y = element_blank(),
                    axis.ticks.y = element_blank(),
                    panel.border = element_blank(),
                    panel.grid = element_blank()) 

centroids <- codes %>%
  dplyr::select(ISO3166.3, Longitude, Latitude) %>%
  mutate_at(vars(Longitude, Latitude),
            funs(as.numeric))

GER_Orig_Dest_Avg_Africa <- GER_Orig_Dest_Avg_Africa %>%
  left_join(centroids %>% distinct(ISO3166.3, .keep_all = T), by = c("reporter.ISO" = "ISO3166.3")) %>%
  dplyr::rename(rLongitude = Longitude,
                rLatitude = Latitude) %>%
  left_join(centroids %>% distinct(ISO3166.3, .keep_all = T), by = c("partner.ISO" = "ISO3166.3"))%>%
  dplyr::rename(pLongitude = Longitude,
                pLatitude = Latitude) %>%
  filter(reporter.ISO %in% conduits_Africa) %>%
  group_by(reporter.ISO) %>%
  top_n(5, Tot_IFF_hi) %>%
  ungroup() %>%
  mutate(scale = round((10 - 1) * (Tot_IFF_hi - min(Tot_IFF_hi))/(max(Tot_IFF_hi) - min(Tot_IFF_hi)) + 1))

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
  labs(title = "Destinations of top origins in Africa",
       subtitle = "Top 10 origin countries by % of GDP")
ggsave(g,
       file = "Figures/Flow map top destinations Africa.png",
       width = 6, height = 5, units = "in")

GER_Orig_Dest_Avg_LMIC <- GER_Orig_Dest_Avg_LMIC %>%
  left_join(centroids %>% distinct(ISO3166.3, .keep_all = T), by = c("reporter.ISO" = "ISO3166.3")) %>%
  dplyr::rename(rLongitude = Longitude,
                rLatitude = Latitude) %>%
  left_join(centroids %>% distinct(ISO3166.3, .keep_all = T), by = c("partner.ISO" = "ISO3166.3"))%>%
  dplyr::rename(pLongitude = Longitude,
                pLatitude = Latitude) %>%
  filter(reporter.ISO %in% conduits_LMIC) %>%
  group_by(reporter.ISO) %>%
  top_n(5, Tot_IFF_hi) %>%
  ungroup() %>%
  mutate(scale = round((10 - 1) * (Tot_IFF_hi - min(Tot_IFF_hi))/(max(Tot_IFF_hi) - min(Tot_IFF_hi)) + 1))

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
  labs(title = "Destinations of top origins in low and lower middle income",
       subtitle = "Top 10 origin countries by % of GDP")
ggsave(g,
       file = "Figures/Flow map top destinations LMIC.png",
       width = 6, height = 5, units = "in")

GER_Orig_Dest_Avg_Developing <- GER_Orig_Dest_Avg_Developing %>%
  left_join(centroids %>% distinct(ISO3166.3, .keep_all = T), by = c("reporter.ISO" = "ISO3166.3")) %>%
  dplyr::rename(rLongitude = Longitude,
                rLatitude = Latitude) %>%
  left_join(centroids %>% distinct(ISO3166.3, .keep_all = T), by = c("partner.ISO" = "ISO3166.3"))%>%
  dplyr::rename(pLongitude = Longitude,
                pLatitude = Latitude) %>%
  filter(reporter.ISO %in% conduits_Developing) %>%
  group_by(reporter.ISO) %>%
  top_n(5, Tot_IFF_hi) %>%
  ungroup() %>%
  mutate(scale = round((10 - 1) * (Tot_IFF_hi - min(Tot_IFF_hi))/(max(Tot_IFF_hi) - min(Tot_IFF_hi)) + 1))

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
  labs(title = "Destinations of top origins in developing countries",
       subtitle = "Top 10 origin countries by % of GDP")
ggsave(g,
       file = "Figures/Flow map top destinations Developing.png",
       width = 6, height = 5, units = "in")


# .. Flow maps of top inflows ####
load("Results/Summary data-sets/Net_Orig_Dest_Avg.Rdata")
load("Results/Summary data-sets/Net_Orig_Avg.Rdata")

# conduits_Africa <- c("MUS", "UGA", "MWI", "SYC", "MLI", "AGO", "ZWE", "NER", "CIV", "KEN")
# conduits_LMIC <- c("MDA", "PNG", "VNM", "NIC", "SLV", "KGZ", "VUT", "NPL", "HND", "UGA")
# conduits_Developing <- c("SGP", "HKG", "MDV", "GUY", "PLW", "MYS", "PNG", "VNM", "THA", "NIC")

top_inflows <- Net_Orig_Avg %>%
  filter(Tot_IFF_hi < 0) %>%
  top_n(10, abs(Tot_IFF_hi)) %>%
  arrange(desc(abs(Tot_IFF_hi))) %>%
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
  dplyr::select(ISO3166.3, Longitude, Latitude) %>%
  mutate_at(vars(Longitude, Latitude),
            funs(as.numeric))

Net_Orig_Dest_Avg <- Net_Orig_Dest_Avg %>%
  left_join(centroids %>% distinct(ISO3166.3, .keep_all = T), by = c("reporter.ISO" = "ISO3166.3")) %>%
  dplyr::rename(rLongitude = Longitude,
                rLatitude = Latitude) %>%
  left_join(centroids %>% distinct(ISO3166.3, .keep_all = T), by = c("partner.ISO" = "ISO3166.3"))%>%
  dplyr::rename(pLongitude = Longitude,
                pLatitude = Latitude) %>%
  filter(reporter.ISO %in% top_inflows) %>%
  filter(Tot_IFF_hi < 0) %>%
  group_by(reporter.ISO) %>%
  top_n(5, abs(Tot_IFF_hi)) %>%
  ungroup() %>%
  mutate(scale = round((10 - 1) * (Tot_IFF_hi - min(Tot_IFF_hi))/(max(Tot_IFF_hi) - min(Tot_IFF_hi)) + 1))

map <- map_data("world")
map <- left_join(map, codes %>% dplyr::select(Country, ISO3166.3),
                 by = c("region" = "Country")) %>%
  dplyr::select(-subregion) %>%
  filter(region != "Antarctica")

viz <- left_join(Net_Orig_Dest_Avg %>% filter(reporter.ISO %in% top_inflows),
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
       file = "Figures/Flow map provenance top inflows World.png",
       width = 6, height = 5, units = "in")



## ## ## ## ## ## ## ## ## ## ##
# CONDUITS CHARTS           ####
## ## ## ## ## ## ## ## ## ## ##

# .. Top conduits in World ####
load("Results/Summary data-sets/GER_Orig_Avg.Rdata")

g <- ggplot(GER_Orig_Avg %>%
              select(reporter, Tot_IFF_hi_GDP) %>%
              top_n(10, Tot_IFF_hi_GDP),
            aes(x = fct_reorder(reporter, Tot_IFF_hi_GDP), y = Tot_IFF_hi_GDP*100)) +
  geom_segment(aes(xend = reporter, y = 0, yend = Tot_IFF_hi_GDP*100), color = "skyblue") +
  geom_point(size = 4, color = "cornflowerblue") +
  coord_flip() +
  labs(title = "Top origin countries worldwide",
       subtitle = "Average yearly gross outflow as % of GDP",
       x = "", y = "Illicit flow as % of GDP")
ggsave(g,
       file = "Figures/Top origin countries World Percent GDP.png",
       width = 6, height = 5, units = "in")

g <- ggplot(GER_Orig_Avg %>%
              select(reporter, Tot_IFF_hi_trade) %>%
              top_n(10, Tot_IFF_hi_trade),
            aes(x = fct_reorder(reporter, Tot_IFF_hi_trade), y = Tot_IFF_hi_trade*100)) +
  geom_segment(aes(xend = reporter, y = 0, yend = Tot_IFF_hi_trade*100), color = "skyblue") +
  geom_point(size = 4, color = "cornflowerblue") +
  coord_flip() +
  labs(title = "Top origin countries worldwide",
       subtitle = "Average yearly gross outflow as % of trade",
       x = "", y = "Illicit flow as % of trade")
ggsave(g,
       file = "Figures/Top origin countries World Percent Trade.png",
       width = 6, height = 5, units = "in")


# .. Top conduits in Africa ####
load("Results/Summary data-sets/GER_Orig_Avg_Africa.Rdata")

g <- ggplot(GER_Orig_Avg_Africa %>%
              select(reporter, Tot_IFF_hi_GDP) %>%
              top_n(10, Tot_IFF_hi_GDP),
            aes(x = fct_reorder(reporter, Tot_IFF_hi_GDP), y = Tot_IFF_hi_GDP*100)) +
  geom_segment(aes(xend = reporter, y = 0, yend = Tot_IFF_hi_GDP*100), color = "skyblue") +
  geom_point(size = 4, color = "cornflowerblue") +
  coord_flip() +
  labs(title = "Top origin countries in Africa",
       subtitle = "Average yearly gross outflow as % of GDP",
       x = "", y = "Illicit flow as % of GDP")
ggsave(g,
       file = "Figures/Top origin countries Africa Percent GDP.png",
       width = 6, height = 5, units = "in")

g <- ggplot(GER_Orig_Avg_Africa %>%
              select(reporter, Tot_IFF_hi_trade) %>%
              top_n(10, Tot_IFF_hi_trade),
            aes(x = fct_reorder(reporter, Tot_IFF_hi_trade), y = Tot_IFF_hi_trade*100)) +
  geom_segment(aes(xend = reporter, y = 0, yend = Tot_IFF_hi_trade*100), color = "skyblue") +
  geom_point(size = 4, color = "cornflowerblue") +
  coord_flip() +
  labs(title = "Top origin countries in Africa",
       subtitle = "Average yearly gross outflow as % of trade",
       x = "", y = "Illicit flow as % of trade")
ggsave(g,
       file = "Figures/Top origin countries Africa Percent Trade.png",
       width = 6, height = 5, units = "in")


# .. Top conduits in LMIC ####
load("Results/Summary data-sets/GER_Orig_Avg_LMIC.Rdata")

g <- ggplot(GER_Orig_Avg_LMIC %>%
              select(reporter, Tot_IFF_hi_GDP) %>%
              top_n(10, Tot_IFF_hi_GDP),
            aes(x = fct_reorder(reporter, Tot_IFF_hi_GDP), y = Tot_IFF_hi_GDP*100)) +
  geom_segment(aes(xend = reporter, y = 0, yend = Tot_IFF_hi_GDP*100), color = "skyblue") +
  geom_point(size = 4, color = "cornflowerblue") +
  coord_flip() +
  labs(title = "Top origins in low and lower-middle income",
       subtitle = "Average yearly gross outflow as % of GDP",
       x = "", y = "Illicit flow as % of GDP")
ggsave(g,
       file = "Figures/Top origin countries LMIC Percent GDP.png",
       width = 6, height = 5, units = "in")

g <- ggplot(GER_Orig_Avg_LMIC %>%
              select(reporter, Tot_IFF_hi_trade) %>%
              top_n(10, Tot_IFF_hi_trade),
            aes(x = fct_reorder(reporter, Tot_IFF_hi_trade), y = Tot_IFF_hi_trade*100)) +
  geom_segment(aes(xend = reporter, y = 0, yend = Tot_IFF_hi_trade*100), color = "skyblue") +
  geom_point(size = 4, color = "cornflowerblue") +
  coord_flip() +
  labs(title = "Top origins in low and lower-middle income",
       subtitle = "Average yearly gross outflow as % of trade",
       x = "", y = "Illicit flow as % of trade")
ggsave(g,
       file = "Figures/Top origin countries LMIC Percent Trade.png",
       width = 6, height = 5, units = "in")


# .. Top conduits in Developing ####
load("Results/Summary data-sets/GER_Orig_Avg_Developing.Rdata")

g <- ggplot(GER_Orig_Avg_Developing %>%
              select(reporter, Tot_IFF_hi_GDP) %>%
              top_n(10, Tot_IFF_hi_GDP),
            aes(x = fct_reorder(reporter, Tot_IFF_hi_GDP), y = Tot_IFF_hi_GDP*100)) +
  geom_segment(aes(xend = reporter, y = 0, yend = Tot_IFF_hi_GDP*100), color = "skyblue") +
  geom_point(size = 4, color = "cornflowerblue") +
  coord_flip() +
  labs(title = "Top origins in developing countries",
       subtitle = "Average yearly gross outflow as % of GDP",
       x = "", y = "Illicit flow as % of GDP")
ggsave(g,
       file = "Figures/Top origin countries Developing Percent GDP.png",
       width = 6, height = 5, units = "in")

g <- ggplot(GER_Orig_Avg_Developing %>%
              select(reporter, Tot_IFF_hi_trade) %>%
              top_n(10, Tot_IFF_hi_trade),
            aes(x = fct_reorder(reporter, Tot_IFF_hi_trade), y = Tot_IFF_hi_trade*100)) +
  geom_segment(aes(xend = reporter, y = 0, yend = Tot_IFF_hi_trade*100), color = "skyblue") +
  geom_point(size = 4, color = "cornflowerblue") +
  coord_flip() +
  labs(title = "Top origins in developing countries",
       subtitle = "Average yearly gross outflow as % of trade",
       x = "", y = "Illicit flow as % of trade")
ggsave(g,
       file = "Figures/Top origin countries Developing Percent Trade.png",
       width = 6, height = 5, units = "in")



## ## ## ## ## ## ## ## ## ## ##
# SANKEY DIAGRAM            ####
## ## ## ## ## ## ## ## ## ## ##

# .. Sankey diagram by GNI per capita and sector ####
load("Results/Summary data-sets/GER_Orig_Sect_Avg.Rdata")
load("Results/Summary data-sets/GER_Sect_Avg_LMIC.Rdata")

top_sectors <- GER_Sect_Avg_LMIC %>%
  top_n(10, Tot_IFF_hi_bn) %>%
  arrange(desc(Tot_IFF_hi_bn)) %>%
  pull(section)

viz <- GER_Orig_Sect_Avg %>%
  filter(rIncome == "LIC" | rIncome == "LMC") %>%
  filter(section %in% top_sectors) %>%
  mutate(cut = cut(GNPpc, breaks = c(0, 1000, 2000, 3000, 4000),
                   labels = c("0$-1,000$", "1,001$-2,000$", "2,001$-3,000$", "3,001$-4000$"))) %>%
  group_by(cut, section, section.code) %>%
  summarize(Tot_IFF_hi_bn = sum(Tot_IFF_hi_bn, na.rm = T)) %>%
  ungroup() %>%
  filter(!is.na(cut)) %>%
  arrange(factor(section, levels = GER_Sect_Avg_LMIC %>%
                   arrange(desc(Tot_IFF_hi_bn)) %>%
                   pull(section)))

g <- ggplot(viz,
            aes(y = Tot_IFF_hi_bn, axis1 = cut, axis2 = fct_inorder(section))) +
  geom_alluvium(aes(fill = cut)) +
  geom_stratum(width = 1/12, color = "black", alpha = 0.5) +
  geom_label(stat = "stratum", label.strata = TRUE, size = 3, hjust = "inward") +
  scale_x_discrete(limits = c("GNI per capita", "Sector"), expand = c(0.075, 0.075)) +
  scale_y_continuous(labels = dollar_format()) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  labs(title = "Trade mis-invoicing in low and lower middle income",
       subtitle = "according to GNI per capita and top 10 sectors",
       y = "Yearly average outflow in billion USD") +
  theme(legend.position = "none")
ggsave(g,
       file = "Figures/Sankey GNIpc sector LMIC.png",
       width = 6, height = 5, units = "in")


# .. Sankey diagram by reporter and partner GNI per capita ####
load("Results/Summary data-sets/GER_Orig_Dest_Avg_LMIC.Rdata")

# viz <- GER_Orig_Dest_Avg_LMIC %>%
#   mutate(rcut = cut(rGNPpc, breaks = c(0, 1000, 2000, 3000, 4000),
#                     labels = c("0$-1,000$", "1,001$-2,000$", "2,001$-3,000$", "3,001$-4,000$")),
#          pcut = cut(pGNPpc, breaks = c(0, 995, 3895, 12055, 75000),
#                     labels = c("0$-995$", "996$-3,895$", "3,896$-12,055$", "above 12,055$"))) %>%
#   filter(is.finite(rcut) & is.finite(pcut)) %>%
#   group_by(rcut, pcut) %>%
#   summarize(Tot_IFF_hi_bn = sum(Tot_IFF_hi_bn, na.rm = T)) %>%
#   ungroup()

viz <- GER_Orig_Dest_Avg_LMIC %>%
  mutate(rcut = cut(rGNPpc, breaks = c(0, 1000, 2000, 3000, 4000),
                    labels = c("0$-1,000$", "1,001$-2,000$", "2,001$-3,000$", "3,001$-4,000$")),
         pcut = cut(pGNPpc, breaks = c(0, 15000, 30000, 45000, 60000, 75000),
                    labels = c("0$-15,000$", "15,001$-30,000$", "30,001$-45,000$", "45,001$-60,000$", "60,001$-75,000$"))) %>%
  filter(is.finite(rcut) & is.finite(pcut)) %>%
  group_by(rcut, pcut) %>%
  summarize(Tot_IFF_hi_bn = sum(Tot_IFF_hi_bn, na.rm = T)) %>%
  ungroup()

g <- ggplot(viz,
            aes(y = Tot_IFF_hi_bn, axis1 = rcut, axis2 = pcut)) +
  geom_alluvium(aes(fill = rcut)) +
  geom_stratum(width = 1/12, color = "black", alpha = 0.5) +
  geom_label(stat = "stratum", label.strata = TRUE, size = 3, hjust = "inward") +
  scale_x_discrete(limits = c("Origin GNI/capita", "Destination GNI/capita"), expand = c(0.075, 0.075)) +
  scale_y_continuous(labels = dollar_format()) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  labs(title = "Trade mis-invoicing in low and lower middle income",
       subtitle = "according to GNI per capita",
       y = "Yearly average outflow in billion USD") +
  theme(legend.position = "none")
ggsave(g,
       file = "Figures/Sankey GNIpc reporter partner LMIC.png",
       width = 6, height = 5, units = "in")



## ## ## ## ## ## ## ## ## ## ##
# COUNTRY OUTLINES          ####
## ## ## ## ## ## ## ## ## ## ##

g <- ggplot() + 
  geom_polygon(data = map %>% filter(ISO3166.3 == "EGY"),
               aes(x = long, y = lat, group = group), color = "white") + 
  coord_fixed(1.3) +
  theme_bw() + 
  ditch_axes 
ggsave(g,
       file = "Figures/Pilots/Outline Egypt.png",
       width = 6, height = 5, units = "in")

g <- ggplot() + 
  geom_polygon(data = map %>% filter(ISO3166.3 == "NGA"),
               aes(x = long, y = lat, group = group), color = "white") + 
  coord_fixed(1.3) +
  theme_bw() + 
  ditch_axes 
ggsave(g,
       file = "Figures/Pilots/Outline Nigeria.png",
       width = 6, height = 5, units = "in")

g <- ggplot() + 
  geom_polygon(data = map %>% filter(ISO3166.3 == "SEN"),
               aes(x = long, y = lat, group = group), color = "white") + 
  coord_fixed(1.3) +
  theme_bw() + 
  ditch_axes 
ggsave(g,
       file = "Figures/Pilots/Outline Senegal.png",
       width = 6, height = 5, units = "in")

g <- ggplot() + 
  geom_polygon(data = map %>% filter(ISO3166.3 == "ZAF"),
               aes(x = long, y = lat, group = group), color = "white") + 
  coord_fixed(1.3) +
  theme_bw() + 
  ditch_axes 
ggsave(g,
       file = "Figures/Pilots/Outline South Africa.png",
       width = 6, height = 5, units = "in")

g <- ggplot() + 
  geom_polygon(data = map %>% filter(ISO3166.3 == "TZA"),
               aes(x = long, y = lat, group = group), color = "white") + 
  coord_fixed(1.3) +
  theme_bw() + 
  ditch_axes 
ggsave(g,
       file = "Figures/Pilots/Outline Tanzania.png",
       width = 6, height = 5, units = "in")

g <- ggplot() + 
  geom_polygon(data = map %>% filter(ISO3166.3 == "TUN"),
               aes(x = long, y = lat, group = group), color = "white") + 
  coord_fixed(1.3) +
  theme_bw() + 
  ditch_axes 
ggsave(g,
       file = "Figures/Pilots/Outline Tunisia.png",
       width = 6, height = 5, units = "in")

g <- ggplot() + 
  geom_polygon(data = map %>% filter(ISO3166.3 == "SDN"),
               aes(x = long, y = lat, group = group), color = "white") + 
  coord_fixed(1.3) +
  theme_bw() + 
  ditch_axes 
ggsave(g,
       file = "Figures/Pilots/Outline Sudan.png",
       width = 6, height = 5, units = "in")