# Data Preparation
# Alice Lepissier
# alice.lepissier@gmail.com
# Prepared for UNECA

## ## ## ## ## ## ## ## ## ## ##
# INDEX                     ####
## ## ## ## ## ## ## ## ## ## ##
# Preamble
# Codes Masterlist
# Import Comtrade
# .. Generate unique identifier
# .. Convert to wide
# .. Create mirror variables
# Import CEPII
# .. Merge dyadic distance and contiguity data
# .. Merge country geographic data
# Import WITS
# .. Import average tariff line data
# .. Generate unique identifier
# .. Merge with panel
# Import Groupings
# .. Merge reporters
# .. Merge partners
# .. Import country names
# Export Clean Data



## ## ## ## ## ## ## ## ## ## ##
# PREAMBLE                  ####
## ## ## ## ## ## ## ## ## ## ##

#setwd("C:/cloudstorage/googledrive/Projects/UN Consultancy/Illicit Financial Flows/IFF estimates") # Alice work
setwd("/home/alice/IFFe/") # Virtual server
library(stringr)
library(tidyverse)
library(xlsx)



## ## ## ## ## ## ## ## ## ## ##
# CODES MASTERLIST          ####
## ## ## ## ## ## ## ## ## ## ##

codes <- read.xlsx2("Data/Codes_Masterlist.xlsx", sheetName = "Codes") %>%
  mutate_all(as.character)



## ## ## ## ## ## ## ## ## ## ##
# IMPORT COMTRADE           ####
## ## ## ## ## ## ## ## ## ## ##

#source("Scripts/Process Comtrade.R")
load("Data/Comtrade/comtrade.Rdata")

comtrade <- comtrade %>%
  select(-c(Aggregate.Level, Commodity)) %>%
  rename_all(tolower) %>%
  rename(reporter.ISO = reporter.iso, partner.ISO = partner.iso,
         value = trade.value..us.., flow = trade.flow, quantity = qty)

noreporter <- subset(comtrade, reporter.ISO == "")
noreporter %>% distinct(reporter)
# Other Asia, nes
nopartner <- subset(comtrade, partner.ISO == "")
nopartner %>% distinct(partner)
# Other Asia, nes; Areas, nes; Other Europe, nes;
# Free Zones; Bunkers; Other Africa, nes;
# Oceania, nes; Special Categories; LAIA, nes;
# North America and Central America, nes; Neutral Zone; US Misc. Pacific Isds
# Br. Antarctic Terr.; Europe EU, nes
rm(noreporter, nopartner)
comtrade <- subset(comtrade,
                   reporter.ISO != "" & partner.ISO != "")

comtrade <- subset(comtrade,
                   flow != "Re-Import")


# .. Generate unique identifier ####
comtrade$id <- paste(comtrade$reporter.ISO, 
                     comtrade$partner.ISO, 
                     comtrade$commodity.code, 
                     comtrade$year, sep = "_")


# .. Convert to wide ####
comtrade <- comtrade %>% 
  gather(variable, value, -c("year", "flow", "reporter", "reporter.ISO", "partner", "partner.ISO", "commodity.code", "id"))%>% 
  unite(temp, flow, variable) %>% 
  spread(temp, value, fill = 0)
colnames(comtrade)[colnames(comtrade) == "Re-Export_value"] <- "ReExport_value"
colnames(comtrade)[colnames(comtrade) == "Re-Export_quantity"] <- "ReExport_quantity"


# .. Create mirror variables ####
comtrade <- comtrade %>%
  mutate_at(vars(reporter, reporter.ISO, partner, partner.ISO,
                 commodity.code),
            funs(as.character(.)))

comtrade_mirror <- comtrade

comtrade_mirror$id <- paste(comtrade_mirror$partner.ISO, 
                            comtrade_mirror$reporter.ISO, 
                            comtrade_mirror$commodity.code,
                            comtrade_mirror$year, sep = "_")

comtrade_mirror <- comtrade_mirror %>% 
  rename(pImport_value = Import_value,
         pImport_quantity = Import_quantity,
         pExport_value = Export_value,
         pExport_quantity = Export_quantity,
         pReExport_value = ReExport_value,
         pReExport_quantity = ReExport_quantity)

comtrade <- full_join(comtrade, comtrade_mirror,
                      by = c("id" = "id",
                             "reporter" = "partner",
                             "reporter.ISO" = "partner.ISO",
                             "partner.ISO" = "reporter.ISO",
                             "partner" = "reporter",
                             "year" = "year",
                             "commodity.code" = "commodity.code"))

rm(comtrade_mirror)
save(comtrade, file = "Data/Comtrade/comtrade_clean.Rdata")


# .. Checking coverage of quantities ####
Import_quantity <- comtrade %>% 
  filter(!is.na(Import_quantity) & Import_quantity != 0) %>%
  mutate_at(vars(reporter:commodity.code),
            funs(as.factor))
nrow(Import_quantity)
# 783
levels(Import_quantity$reporter)
# "China, Macao SAR", "Czechia", "Hungary", "Poland", "Sweden"
range(Import_quantity$year)
# 2000 2012

Export_quantity <- comtrade %>% 
  filter(!is.na(Export_quantity) & Export_quantity != 0) %>%
  mutate_at(vars(reporter:commodity.code),
            funs(as.factor))
nrow(Export_quantity)
# 1161
levels(Export_quantity$reporter)
# "China, Macao SAR", "Czechia", "Hungary", "Poland", "Sweden"
range(Export_quantity$year)
# 2000 2012

ReExport_quantity <- comtrade %>% 
  filter(!is.na(ReExport_quantity) & ReExport_quantity != 0) %>%
  mutate_at(vars(reporter:commodity.code),
            funs(as.factor))
nrow(ReExport_quantity)
# 110
levels(ReExport_quantity$reporter)
# "China, Macao SAR"
range(ReExport_quantity$year)
# 2012

rm(Import_quantity, Export_quantity, ReExport_quantity)



## ## ## ## ## ## ## ## ## ## ##
# IMPORT CEPII              ####
## ## ## ## ## ## ## ## ## ## ##

# .. Merge dyadic distance and contiguity data ####
dist <- read.xlsx2("Data/CEPII/dist_cepii.xls", 
                   sheetName = "dist_cepii") %>%
  mutate_at(vars(iso_o, iso_d),
            funs(as.character(.))) %>%
  mutate(dist = as.numeric(dist))

panel <- left_join(comtrade, dist %>% 
                     select(iso_o, iso_d, contig, dist),
                      by = c("reporter.ISO" = "iso_o", 
                             "partner.ISO" = "iso_d"))
rm(comtrade)


# .. Merge country geographic data ####
geo <- read.xlsx2("Data/CEPII/geo_cepii.xls", sheetName = "geo_cepii") %>% 
  select(iso3, landlocked) %>%
  mutate(iso3 = as.character(iso3))

panel <- left_join(panel, geo,
                   by = c("reporter.ISO" = "iso3")) %>%
  rename(rLandlocked = landlocked)

panel <- left_join(panel, geo,
                   by = c("partner.ISO" = "iso3")) %>%
  rename(pLandlocked = landlocked)

rm(dist, geo)



## ## ## ## ## ## ## ## ## ## ##
# IMPORT WITS               ####
## ## ## ## ## ## ## ## ## ## ##

# .. Import average tariff line data ####
tariff <- read.csv("Data/WITS/DataJobID-1514178_1514178_IFFtriffs.csv") %>%
  select(Reporter.Name, Partner.Name, Product, Tariff.Year, Simple.Average) %>%
  rename(reporter = Reporter.Name,
         partner = Partner.Name,
         year = Tariff.Year,
         commodity.code = Product,
         tariff = Simple.Average) %>%
  mutate_at(vars(reporter, partner, commodity.code),
            funs(as.character(.)))

tariff <- left_join(tariff, codes %>%
                      select(Country, ISO3166.3),
                    by = c("reporter" = "Country")) %>%
  rename(reporter.ISO = ISO3166.3)

tariff %>% 
  filter(is.na(reporter.ISO)) %>%
  distinct(reporter)
# European Union; All countries  All --- All

tariff <- left_join(tariff, codes %>%
                      select(Country, ISO3166.3),
                    by = c("partner" = "Country")) %>%
  rename(partner.ISO = ISO3166.3)

tariff %>% 
  filter(is.na(partner.ISO)) %>%
  distinct(partner)
# All countries  All --- All; Bunkers; Unspecified;
# Special Categories; Free Zones; Neutral Zone; Other Asia, nes

tariff <- tariff %>%
  filter(!is.na(reporter.ISO) & !is.na(partner.ISO))


# .. Generate unique identifier ####
tariff <- tariff %>%
  mutate(commodity.code = str_pad(str_trim(commodity.code), 
                                  width = 2, side = "left", pad = "0"))

tariff$id <- paste(tariff$reporter.ISO, 
                   tariff$partner.ISO, 
                   tariff$commodity.code, 
                   tariff$year, sep = "_")


# .. Merge with panel ####
panel <- left_join(panel, tariff,
                   by = c("id" = "id",
                          "reporter" = "reporter",
                          "reporter.ISO" = "reporter.ISO",
                          "partner" = "partner",
                          "partner.ISO" = "partner.ISO",
                          "commodity.code" = "commodity.code",
                          "year" = "year"))

rm(tariff)
save(panel, file = "Data/Panel/panel.Rdata")



## ## ## ## ## ## ## ## ## ## ##
# IMPORT GROUPINGS          ####
## ## ## ## ## ## ## ## ## ## ##

# .. Merge reporters ####
panel <- left_join(panel, codes %>% 
                   select(c(ISO3166.3, UN_Region, WB_Income_Group_Code)) %>%
                   distinct(ISO3166.3, .keep_all = T),
                 by = c("reporter.ISO" = "ISO3166.3")) %>%
  rename(rRegion = UN_Region,
         rIncome = WB_Income_Group_Code)


# .. Merge partners ####
panel <- left_join(panel, codes %>% 
                     select(c(ISO3166.3, UN_Region, WB_Income_Group_Code)) %>%
                     distinct(ISO3166.3, .keep_all = T),
                   by = c("partner.ISO" = "ISO3166.3")) %>%
  rename(pRegion = UN_Region,
         pIncome = WB_Income_Group_Code)


# .. Import country names ####
panel <- left_join(panel, aggregate(Country ~ ISO3166.3, 
                                     data = codes, head, 1),
                   by = c("reporter.ISO" = "ISO3166.3")) %>%
  select(-reporter) %>%
  rename(reporter = Country)

noreporter <- panel %>%
  filter(is.na(reporter)) %>%
  distinct(reporter.ISO)
# WLD
panel <- panel %>% filter(!is.na(reporter))

panel <- left_join(panel, aggregate(Country ~ ISO3166.3, 
                                    data = codes, head, 1),
                   by = c("partner.ISO" = "ISO3166.3")) %>%
  select(-partner) %>%
  rename(partner = Country)

nopartner <- panel %>%
  filter(is.na(partner)) %>%
  distinct(partner.ISO)
# WLD
panel <- panel %>% filter(!is.na(partner))

rm(codes, noreporter, nopartner)



## ## ## ## ## ## ## ## ## ## ##
# EXPORT CLEAN DATA         ####
## ## ## ## ## ## ## ## ## ## ##

panel <- panel %>%
  select(id, reporter.ISO, partner.ISO, commodity.code, year,
         Import_value, Export_value, ReExport_value,
         Import_quantity, Export_quantity, ReExport_quantity,
         pImport_value, pExport_value, pReExport_value,
         pImport_quantity, pExport_quantity, pReExport_quantity,
         contig, dist, rLandlocked, pLandlocked, tariff,
         reporter, partner, rRegion, rIncome, pRegion, pIncome)

missing <- panel %>% 
  filter(is.na(Import_value) & is.na(Export_value) & is.na(ReExport_value) &
           is.na(pImport_value) & is.na(pExport_value) & is.na(pReExport_value))
rm(missing)

duplicates <- panel %>%
  filter(duplicated(panel$id))

panel <- panel %>%
  filter(!duplicated(panel$id))
rm(duplicates)

save(panel, file = "Data/Panel/panel.Rdata")