# Data Visualization
# Alice Lepissier
# alice.lepissier@gmail.com
# Prepared for UNECA

## ## ## ## ## ## ## ## ## ## ##
# INDEX                     ####
## ## ## ## ## ## ## ## ## ## ##
# Preamble



## ## ## ## ## ## ## ## ## ## ##
# PREAMBLE                  ####
## ## ## ## ## ## ## ## ## ## ##

#setwd("C:/cloudstorage/googledrive/Projects/UN Consultancy/Illicit Financial Flows/IFF estimates") # Alice work
setwd("/home/alice/IFFe/") # Virtual server
library(geosphere)
library(ggmap)
# library(raster) # For raster data
# library(sf) # For vector data
# library(spData)
# library(spDataLarge)
# library(tidyverse)
# library(tmap)
library(xlsx)



## ## ## ## ## ## ## ## ## ## ##
# BAR CHARTS                ####
## ## ## ## ## ## ## ## ## ## ##

load("Results/Current Version/GER_Year_Africa.Rdata")
load("Results/Current Version/Net_Year_Africa.Rdata")

g <- ggplot(GER_Year_Africa %>% 
              melt(id.vars = "year") %>%
              filter(str_detect(variable, "Imp")), 
            aes(x = year, y = value, fill = variable)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = dollar_format(scale = 1/10^9, accuracy = 1)) +
  scale_fill_discrete(name = "Estimate", labels = c("Low", "High")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Trade mis-invoicing in imports in Africa",
       subtitle = "Gross outflows",
       x = "Year", y = "Illicit flow in billion USD")
ggsave(g,
       file = "Figures/GER_Africa_Import.png",
       width = 6, height = 5, units = "in")

g <- ggplot(GER_Year_Africa %>% 
              melt(id.vars = "year") %>%
              filter(str_detect(variable, "Exp")), 
            aes(x = year, y = value, fill = variable)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = dollar_format(scale = 1/10^9, accuracy = 1)) +
  scale_fill_discrete(name = "Estimate", labels = c("Low", "High")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Trade mis-invoicing in exports in Africa",
       subtitle = "Gross outflows",
       x = "Year", y = "Illicit flow in billion USD")
ggsave(g,
       file = "Figures/GER_Africa_Export.png",
       width = 6, height = 5, units = "in")

g <- ggplot(GER_Year_Africa %>% 
              melt(id.vars = "year") %>%
              filter(variable == "Tot_IFF_lo" | variable == "Tot_IFF_hi"), 
            aes(x = year, y = value, fill = variable)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = dollar_format(scale = 1/10^9, accuracy = 1)) +
  scale_fill_discrete(name = "Estimate", labels = c("Low", "High")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Trade mis-invoicing in Africa",
       subtitle = "Gross outflows",
       x = "Year", y = "Illicit flow in billion USD") +
  geom_text(aes(label = round(value/10^9)),
            size = 3, position = position_dodge(1), vjust = -0.4)
ggsave(g,
       file = "Figures/GER_Africa_Total.png",
       width = 6, height = 5, units = "in")

g <- ggplot(Net_Year_Africa %>% 
              melt(id.vars = "year") %>%
              filter(str_detect(variable, "Imp")), 
            aes(x = year, y = value, fill = variable)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = dollar_format(scale = 1/10^9, accuracy = 1))+
  scale_fill_discrete(name = "Estimate", labels = c("Low", "High")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Trade mis-invoicing in imports in Africa",
       subtitle = "Net illicit flows",
       x = "Year", y = "Illicit flow in billion USD")
ggsave(g,
       file = "Figures/Net_Africa_Import.png",
       width = 6, height = 5, units = "in")

g <- ggplot(Net_Year_Africa %>% 
              melt(id.vars = "year") %>%
              filter(str_detect(variable, "Exp")), 
            aes(x = year, y = value, fill = variable)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = dollar_format(scale = 1/10^9, accuracy = 1)) +
  scale_fill_discrete(name = "Estimate", labels = c("Low", "High")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Trade mis-invoicing in exports in Africa",
       subtitle = "Net illicit flows",
       x = "Year", y = "Illicit flow in billion USD")
ggsave(g,
       file = "Figures/Net_Africa_Export.png",
       width = 6, height = 5, units = "in")

g <- ggplot(Net_Year_Africa %>% 
              melt(id.vars = "year") %>%
              filter(variable == "Tot_IFF_lo" | variable == "Tot_IFF_hi"), 
            aes(x = year, y = value, fill = variable)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = dollar_format(scale = 1/10^9, accuracy = 1)) +
  scale_fill_discrete(name = "Estimate", labels = c("Low", "High")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Trade mis-invoicing in Africa",
       subtitle = "Net illicit flows",
       x = "Year", y = "Illicit flow in billion USD") +
  geom_text(aes(label = round(value/10^9)),
            size = 3, position = position_dodge(1), vjust = -0.4)
ggsave(g,
       file = "Figures/Net_Africa_Total.png",
       width = 6, height = 5, units = "in")



## ## ## ## ## ## ## ## ## ## ##
# LINE CHARTS               ####
## ## ## ## ## ## ## ## ## ## ##

pilots <- c("EGY", "NGA", "SEN", "ZAF", "TZA", "TUN")
labels <- c(Tot_IFF_lo = "Low estimate", Tot_IFF_hi = "High estimate")

load("Results/Current Version/GER_Orig_Year_Africa.Rdata")

Pilot_Year <- GER_Orig_Year_Africa %>%
  filter(reporter.ISO %in% pilots)

g <- ggplot(Pilot_Year %>% 
         melt(id.vars = c("year", "reporter")) %>%
         filter(variable == "Tot_IFF_lo" | variable == "Tot_IFF_hi") %>%
         mutate(year = as.numeric(year),
                value = as.numeric(value),
                reporter = as.factor(reporter))) +
  geom_line(aes(x = year, y = value/10^9, color = reporter)) +
  facet_wrap(~variable, labeller = labeller(variable = labels)) +
  labs(title = "Gross yearly outflows in pilot countries",
       x = "Year",
       y = "Illicit flow in billion USD",
       color = "")
ggsave(g,
       file = "Figures/Gross Yearly Africa.png",
       width = 6, height = 5, units = "in")

load("Results/Current Version/Net_Orig_Year_Africa.Rdata")

Pilot_Year <- Net_Orig_Year_Africa %>%
  filter(reporter.ISO %in% pilots)

g <- ggplot(Pilot_Year %>% 
              melt(id.vars = c("year", "reporter")) %>%
              filter(variable == "Tot_IFF_lo" | variable == "Tot_IFF_hi") %>%
              mutate(year = as.numeric(year),
                     value = as.numeric(value),
                     reporter = as.factor(reporter))) +
  geom_line(aes(x = year, y = value/10^9, color = reporter)) +
  facet_wrap(~variable, labeller = labeller(variable = labels)) +
  labs(title = "Net yearly flows in pilot countries",
       x = "Year",
       y = "Illicit flow in billion USD",
       color = "")
ggsave(g,
       file = "Figures/Net Yearly Africa.png",
       width = 6, height = 5, units = "in") 



## ## ## ## ## ## ## ## ## ## ##
# CHOROPOLETHS              ####
## ## ## ## ## ## ## ## ## ## ##

# .. Merge geographic data ####
codes <- read.xlsx2("Data/Codes_Masterlist.xlsx", sheetName = "Codes") %>%
  mutate_all(as.character)
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


# .. Average Gross IFF
load("Results/Current Version/GER_Orig_Avg_Africa.Rdata")

viz <- left_join(map, GER_Orig_Avg_Africa,
                 by = c("ISO3166.3" = "reporter.ISO"))

g <- ggplot() + 
  geom_polygon(data = viz,
               aes(x = long, y = lat, group = group, 
                   fill = Tot_IFF_lo_bn), color = "white") + 
  coord_fixed(1.3) +
  theme_bw() + 
  ditch_axes +
  scale_fill_gradient("IFF (billion USD)", low = "#56B1F7", high = "#132B43") +
  labs(title = "Total outflows averaged over 2000-2016",
       subtitle = "Gross outflows, low estimate")
ggsave(g,
       file = "Figures/GER Total Average IFF low.png",
       width = 6, height = 5, units = "in")

g <- ggplot() + 
  geom_polygon(data = viz,
               aes(x = long, y = lat, group = group, 
                   fill = Tot_IFF_hi_bn), color = "white") + 
  coord_fixed(1.3) +
  theme_bw() + 
  ditch_axes +
  scale_fill_gradient("IFF (billion USD)", low = "#56B1F7", high = "#132B43") +
  labs(title = "Total outflows averaged over 2000-2016",
       subtitle = "Gross outflows, high estimate")
ggsave(g,
       file = "Figures/GER Total Average IFF high.png",
       width = 6, height = 5, units = "in")


# .. Average Net IFF
load("Results/Current Version/Net_Orig_Avg_Africa.Rdata")

viz <- left_join(map, Net_Orig_Avg_Africa,
                 by = c("ISO3166.3" = "reporter.ISO"))

g <- ggplot() + 
  geom_polygon(data = viz,
               aes(x = long, y = lat, group = group, 
                   fill = Tot_IFF_lo_bn), color = "white") + 
  coord_fixed(1.3) +
  theme_bw() + 
  ditch_axes +
  scale_fill_gradient("IFF (billion USD)", low = "#56B1F7", high = "#132B43") +
  labs(title = "Total net flows averaged over 2000-2016",
       subtitle = "Low estimate")
ggsave(g,
       file = "Figures/Net Total Average IFF low.png",
       width = 6, height = 5, units = "in")

g <- ggplot() + 
  geom_polygon(data = viz,
               aes(x = long, y = lat, group = group, 
                   fill = Tot_IFF_hi_bn), color = "white") + 
  coord_fixed(1.3) +
  theme_bw() + 
  ditch_axes +
  scale_fill_gradient("IFF (billion USD)", low = "#56B1F7", high = "#132B43") +
  labs(title = "Total net flows averaged over 2000-2016",
       subtitle = "High estimate")
ggsave(g,
       file = "Figures/Net Total Average IFF high.png",
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
       file = "Figures/Maps/Egypt outline.png",
       width = 6, height = 5, units = "in")

g <- ggplot() + 
  geom_polygon(data = map %>% filter(ISO3166.3 == "NGA"),
               aes(x = long, y = lat, group = group), color = "white") + 
  coord_fixed(1.3) +
  theme_bw() + 
  ditch_axes 
ggsave(g,
       file = "Figures/Maps/Nigeria outline.png",
       width = 6, height = 5, units = "in")

g <- ggplot() + 
  geom_polygon(data = map %>% filter(ISO3166.3 == "SEN"),
               aes(x = long, y = lat, group = group), color = "white") + 
  coord_fixed(1.3) +
  theme_bw() + 
  ditch_axes 
ggsave(g,
       file = "Figures/Maps/Senegal outline.png",
       width = 6, height = 5, units = "in")

g <- ggplot() + 
  geom_polygon(data = map %>% filter(ISO3166.3 == "ZAF"),
               aes(x = long, y = lat, group = group), color = "white") + 
  coord_fixed(1.3) +
  theme_bw() + 
  ditch_axes 
ggsave(g,
       file = "Figures/Maps/South Africa outline.png",
       width = 6, height = 5, units = "in")

g <- ggplot() + 
  geom_polygon(data = map %>% filter(ISO3166.3 == "TUN"),
               aes(x = long, y = lat, group = group), color = "white") + 
  coord_fixed(1.3) +
  theme_bw() + 
  ditch_axes 
ggsave(g,
       file = "Figures/Maps/Tunisia outline.png",
       width = 6, height = 5, units = "in")

g <- ggplot() + 
  geom_polygon(data = map %>% filter(ISO3166.3 == "TZA"),
               aes(x = long, y = lat, group = group), color = "white") + 
  coord_fixed(1.3) +
  theme_bw() + 
  ditch_axes 
ggsave(g,
       file = "Figures/Maps/Tanzania outline.png",
       width = 6, height = 5, units = "in")



## ## ## ## ## ## ## ## ## ## ##
# DESTINATION GRAPHS        ####
## ## ## ## ## ## ## ## ## ## ##

load("Results/Current Version/GER_Dest_Africa.Rdata")

Destinations <- GER_Dest_Africa %>%
  group_by(pRegion) %>%
  summarize(Tot_IFF_lo = sum(Tot_IFF_lo, na.rm = T),
            Tot_IFF_hi = sum(Tot_IFF_hi, na.rm = T)) %>%
  ungroup() %>%
  mutate(Pct_IFF_lo = Tot_IFF_lo / sum(Tot_IFF_lo) * 100,
         Pct_IFF_hi = Tot_IFF_hi / sum(Tot_IFF_hi) * 100)

g <- ggplot(Destinations,
       aes(x = "", y = Pct_IFF_lo, fill = pRegion)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(Pct_IFF_lo), "%")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Destinations of outflows, 2000-2016") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave(g,
       file = "Figures/Destinations pie chart.png",
       width = 6, height = 5, units = "in")



## ## ## ## ## ## ## ## ## ## ##
# SECTOR GRAPHS             ####
## ## ## ## ## ## ## ## ## ## ##

load("Results/Current Version/GER_Sect_Africa.Rdata")

ggplot(GER_Sect_Africa,
       aes(x = "", y = Tot_IFF_lo_bn, fill = section)) +
  geom_bar(width = 1, stat = "identity") +
  theme(legend.position="bottom")



## ## ## ## ## ## ## ## ## ## ##
# TOP SECTORS FOR PILOTS    ####
## ## ## ## ## ## ## ## ## ## ##

load("Results/Current Version/GER_Orig_Sect_Africa.Rdata")


# .. Egypt sectors ####
EGY <- GER_Orig_Sect_Africa %>%
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
       file = "Figures/Egypt top 5 sectors GER high.png",
       width = 6, height = 5, units = "in")


# .. Nigeria sectors ####
NGA <- GER_Orig_Sect_Africa %>%
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
       file = "Figures/Nigeria top 5 sectors GER high.png",
       width = 6, height = 5, units = "in")


# .. Senegal sectors ####
SEN <- GER_Orig_Sect_Africa %>%
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
       file = "Figures/Senegal top 5 sectors GER high.png",
       width = 6, height = 5, units = "in")


# .. South Africa sectors ####
ZAF <- GER_Orig_Sect_Africa %>%
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
       file = "Figures/South Africa top 5 sectors GER high.png",
       width = 6, height = 5, units = "in")


# .. Tanzania sectors ####
TZA <- GER_Orig_Sect_Africa %>%
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
       file = "Figures/Tanzania top 5 sectors GER high.png",
       width = 6, height = 5, units = "in")


# .. Tunisia sectors ####
TUN <- GER_Orig_Sect_Africa %>%
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
       file = "Figures/Tunisia top 5 sectors GER high.png",
       width = 6, height = 5, units = "in")



## ## ## ## ## ## ## ## ## ## ##
# TOP DESTINATIONS PILOTS   ####
## ## ## ## ## ## ## ## ## ## ##

load("Results/Current Version/GER_Orig_Dest_Africa.Rdata")

centroids <- codes %>%
  dplyr::select(ISO3166.3, Longitude, Latitude) %>%
  mutate_at(vars(Longitude, Latitude),
            funs(as.numeric))

GER_Orig_Dest_Africa <- GER_Orig_Dest_Africa %>%
  left_join(centroids %>% distinct(ISO3166.3, .keep_all = T), by = c("reporter.ISO" = "ISO3166.3")) %>%
  rename(rLongitude = Longitude,
         rLatitude = Latitude) %>%
  left_join(centroids %>% distinct(ISO3166.3, .keep_all = T), by = c("partner.ISO" = "ISO3166.3"))%>%
  rename(pLongitude = Longitude,
         pLatitude = Latitude)

plot_my_connection=function( dep_lon, dep_lat, arr_lon, arr_lat, ...){
  inter <- gcIntermediate(c(dep_lon, dep_lat), c(arr_lon, arr_lat), n=50, addStartEnd=TRUE, breakAtDateLine=F)             
  inter=data.frame(inter)
  diff_of_lon=abs(dep_lon) + abs(arr_lon)
  if(diff_of_lon > 180){
    lines(subset(inter, lon>=0), ...)
    lines(subset(inter, lon<0), ...)
  }else{
    lines(inter, ...)
  }
}

# .. Egypt destinations ####
viz <- GER_Orig_Dest_Africa %>%
  filter(reporter.ISO == "EGY") %>%
  top_n(10, Tot_IFF_hi) %>%
  mutate(scale = round((10 - 1) * (Tot_IFF_hi - min(Tot_IFF_hi))/(max(Tot_IFF_hi) - min(Tot_IFF_hi)) + 1))

pdf("Figures/Flow map Egypt.pdf", 
    height = 4.5, width = 6)
map("world", col = "#f2f2f2", fill = TRUE, bg = "white", lwd = 0.05,
    mar = rep(0, 4), border = 0, wrap = c(-180, 180, NA)) 
for(i in 1:nrow(viz)){
  plot_my_connection(viz$rLongitude[i], viz$rLatitude[i], viz$pLongitude[i], viz$pLatitude[i], 
                     col = "skyblue", lwd = viz$scale[i])
}
points(x = viz$rLongitude, y = viz$rLatitude, col = "slateblue", cex = 2, pch = 20)
points(x = viz$pLongitude, y = viz$pLatitude, col = "slateblue", cex = 2, pch = 20)
viz2 <- viz[-c(1,2,5),]
text(viz2$partner, x = viz2$pLongitude, y = viz2$pLatitude, col = "slateblue", cex = 0.7, pos = 4) # right
viz2 <- viz[c(1,2,5),]
text(viz2$partner, x = viz2$pLongitude, y = viz2$pLatitude, col = "slateblue", cex = 0.7, pos = 3) # above
title("Top 10 destinations of gross outflows, 2000-2016", cex.main = 0.8)
dev.off()


# .. Nigeria destinations ####
viz <- GER_Orig_Dest_Africa %>%
  filter(reporter.ISO == "NGA") %>%
  top_n(10, Tot_IFF_hi) %>%
  mutate(scale = round((10 - 1) * (Tot_IFF_hi - min(Tot_IFF_hi))/(max(Tot_IFF_hi) - min(Tot_IFF_hi)) + 1))

pdf("Figures/Flow map Nigeria.pdf", 
    height = 4.5, width = 6)
map("world", col = "#f2f2f2", fill = TRUE, bg = "white", lwd = 0.05,
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


# .. Senegal destinations ####
viz <- GER_Orig_Dest_Africa %>%
  filter(reporter.ISO == "SEN") %>%
  top_n(10, Tot_IFF_hi) %>%
  mutate(scale = round((10 - 1) * (Tot_IFF_hi - min(Tot_IFF_hi))/(max(Tot_IFF_hi) - min(Tot_IFF_hi)) + 1))

pdf("Figures/Flow map Senegal.pdf", 
    height = 4.5, width = 6)
map("world", col = "#f2f2f2", fill = TRUE, bg = "white", lwd = 0.05,
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


# .. South Africa destinations ####
viz <- GER_Orig_Dest_Africa %>%
  filter(reporter.ISO == "ZAF") %>%
  top_n(10, Tot_IFF_hi) %>%
  mutate(scale = round((10 - 1) * (Tot_IFF_hi - min(Tot_IFF_hi))/(max(Tot_IFF_hi) - min(Tot_IFF_hi)) + 1))

pdf("Figures/Flow map South Africa.pdf", 
    height = 4.5, width = 6)
map("world", col = "#f2f2f2", fill = TRUE, bg = "white", lwd = 0.05,
    mar = rep(0, 4), border = 0, wrap = c(-180, 180, NA)) 
for(i in 1:nrow(viz)){
  plot_my_connection(viz$rLongitude[i], viz$rLatitude[i], viz$pLongitude[i], viz$pLatitude[i], 
                     col = "skyblue", lwd = viz$scale[i])
}
points(x = viz$rLongitude, y = viz$rLatitude, col = "slateblue", cex = 2, pch = 20)
points(x = viz$pLongitude, y = viz$pLatitude, col = "slateblue", cex = 2, pch = 20)
viz2 <- viz[-c(3,9),]
text(viz2$partner, x = viz2$pLongitude, y = viz2$pLatitude, col = "slateblue", cex = 0.7, pos = 4) # right
viz2 <- viz[c(9),]
text(viz2$partner, x = viz2$pLongitude, y = viz2$pLatitude, col = "slateblue", cex = 0.7, pos = 3) # above
viz2 <- viz[c(3),]
text(viz2$partner, x = viz2$pLongitude, y = viz2$pLatitude, col = "slateblue", cex = 0.7, pos = 2) # left
title("Top 10 destinations of gross outflows, 2000-2016", cex.main = 0.8)
dev.off()


# .. Tanzania destinations ####
viz <- GER_Orig_Dest_Africa %>%
  filter(reporter.ISO == "TZA") %>%
  top_n(10, Tot_IFF_hi) %>%
  mutate(scale = round((10 - 1) * (Tot_IFF_hi - min(Tot_IFF_hi))/(max(Tot_IFF_hi) - min(Tot_IFF_hi)) + 1))

pdf("Figures/Flow map Tanzania.pdf", 
    height = 4.5, width = 6)
map("world", col = "#f2f2f2", fill = TRUE, bg = "white", lwd = 0.05,
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


# .. Tunisia destinations ####
viz <- GER_Orig_Dest_Africa %>%
  filter(reporter.ISO == "TUN") %>%
  top_n(10, Tot_IFF_hi) %>%
  mutate(scale = round((10 - 1) * (Tot_IFF_hi - min(Tot_IFF_hi))/(max(Tot_IFF_hi) - min(Tot_IFF_hi)) + 1))

pdf("Figures/Flow map Tunisia.pdf", 
    height = 4.5, width = 6)
map("world", col = "#f2f2f2", fill = TRUE, bg = "white", lwd = 0.05,
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
