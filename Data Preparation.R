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
# .. Checking coverage of quantities and weights
# Import CEPII
# .. Merge dyadic distance and contiguity data
# .. Merge country geographic data
# Import WGI
# .. Interpolate missing data
# .. Merge with reporters and partners
# Import WITS
# .. Import average tariff line data
# .. Generate unique identifier
# .. Merge with panel
# Import HS Codes
# .. Merge commodity and section codes
# Import SITC Codes
# .. Merge SITC with HS codes
# .. Merge SITC with panel
# Import WDI
# .. Merge with reporters
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
library(data.table)
library(stringr)
library(tidyverse)
library(WDI)
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
         value = trade.value..us.., flow = trade.flow, quantity = qty, weight = netweight..kg.)

noreporter <- subset(comtrade, reporter.ISO == "")
noreporter %>% distinct(reporter)
# Other Asia, nes
nopartner <- subset(comtrade, partner.ISO == "")
nopartner %>% distinct(partner)
# Areas, nes; US Misc. Pacific Isds; Neutral Zone;
# Other Asia, nes; Free Zones; Other Europe, nes;
# Oceania, nes; Caribbean, nes; Other Africa, nes;
# Special Categories; Bunkers; Europe EU, nes;
# LAIA, nes; Rest of America, nes; Br. Antarctic Terr.;
# Western Asia, nes; CACM, nes; Northern Africa, nes;
# Eastern Europe, nes; North America and Central America, nes;
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
  gather(variable, value, -c("year", "flow", "reporter", "reporter.ISO", "partner", "partner.ISO", "commodity.code", "id")) %>% 
  unite(temp, flow, variable) %>% 
  spread(temp, value, fill = 0)
colnames(comtrade)[colnames(comtrade) == "Re-Export_value"] <- "ReExport_value"
colnames(comtrade)[colnames(comtrade) == "Re-Export_quantity"] <- "ReExport_quantity"
colnames(comtrade)[colnames(comtrade) == "Re-Export_weight"] <- "ReExport_weight"
nrow(comtrade)
# 16486069


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
         pImport_weight = Import_weight,
         pExport_value = Export_value,
         pExport_quantity = Export_quantity,
         pExport_weight = Export_weight,
         pReExport_value = ReExport_value,
         pReExport_quantity = ReExport_quantity,
         pReExport_weight = ReExport_weight)

comtrade <- full_join(comtrade, comtrade_mirror,
                      by = c("id" = "id",
                             "reporter" = "partner",
                             "reporter.ISO" = "partner.ISO",
                             "partner.ISO" = "reporter.ISO",
                             "partner" = "reporter",
                             "year" = "year",
                             "commodity.code" = "commodity.code"))

nrow(comtrade)
# 21463108
rm(comtrade_mirror)

comtrade %>% distinct(reporter.ISO) %>% nrow
# 237
comtrade %>% distinct(partner.ISO) %>% nrow
# 237
save(comtrade, file = "Data/Comtrade/comtrade_clean.Rdata")


# .. Checking coverage of quantities and weights ####
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

Import_weight <- comtrade %>% 
  filter(!is.na(Import_weight) & Import_weight != 0) %>%
  mutate_at(vars(reporter:commodity.code),
            funs(as.factor))
nrow(Import_weight)
# 1189
levels(Import_weight$reporter)
# "China, Macao SAR", "Czechia", "Hungary", "Poland", "Sweden"
range(Import_weight$year)
# 2000 2012

Export_weight <- comtrade %>% 
  filter(!is.na(Export_weight) & Export_weight != 0) %>%
  mutate_at(vars(reporter:commodity.code),
            funs(as.factor))
nrow(Export_weight)
# 1551
levels(Export_weight$reporter)
# "China, Macao SAR", "Czechia", "Hungary", "Poland", "Sweden"
range(Export_weight$year)
# 2000 2012

ReExport_weight <- comtrade %>% 
  filter(!is.na(ReExport_weight) & ReExport_weight != 0) %>%
  mutate_at(vars(reporter:commodity.code),
            funs(as.factor))
nrow(ReExport_weight)
# 148
levels(ReExport_weight$reporter)
# "China, Macao SAR"
range(ReExport_weight$year)
# 2012

rm(Import_quantity, Export_quantity, ReExport_quantity,
   Import_weight, Export_weight, ReExport_weight)



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
# IMPORT WGI                ####
## ## ## ## ## ## ## ## ## ## ##

WGI <- read.csv("Data/World Bank/World Governance Indicators.csv") %>%
  rename(corruption = Control.of.Corruption..Percentile.Rank..CC.PER.RNK.,
         regulatory.qual = Regulatory.Quality..Percentile.Rank..RQ.PER.RNK.) %>%
  mutate(Country.Code = as.character(Country.Code)) %>%
  mutate_at(vars(corruption, regulatory.qual, Time),
          funs(as.numeric(as.character(.)))) %>%
  filter(complete.cases(.)) %>%
  select(-c(Time.Code, Country.Name))


# .. Interpolate missing data ####
WGI %>% distinct(Time)
# Missing data for 1999 and 2001, so interpolate from either year

WGI98 <- WGI %>%
  filter(Time == 1998)
WGI00 <- WGI %>%
  filter(Time == 2000)
WGI02 <- WGI %>%
  filter(Time == 2002)

WGI99 <- full_join(WGI98, WGI00,
                   by = "Country.Code") %>%
  mutate(Time = 1999)
WGI99$corruption <- rowMeans(WGI99[, c("corruption.x", "corruption.y")], na.rm = TRUE)
WGI99$regulatory.qual <- rowMeans(WGI99[, c("regulatory.qual.x", "regulatory.qual.y")], na.rm = TRUE)
WGI99 <- WGI99 %>% select(Country.Code, Time, corruption, regulatory.qual)

WGI01 <- full_join(WGI00, WGI02,
                   by = "Country.Code") %>%
  mutate(Time = 2001)
WGI01$corruption <- rowMeans(WGI01[, c("corruption.x", "corruption.y")], na.rm = TRUE)
WGI01$regulatory.qual <- rowMeans(WGI01[, c("regulatory.qual.x", "regulatory.qual.y")], na.rm = TRUE)
WGI01 <- WGI01 %>% select(Country.Code, Time, corruption, regulatory.qual)

WGI <- rbind(WGI, WGI99, WGI01)
rm(WGI98, WGI99, WGI00, WGI01, WGI02)


# .. Merge with reporters and partners ####
panel <- left_join(panel, WGI,
                   by = c("reporter.ISO" = "Country.Code",
                          "year" = "Time")) %>%
  rename(rCorruption = corruption,
         rRegulatory.qual = regulatory.qual)

panel <- left_join(panel, WGI,
                   by = c("partner.ISO" = "Country.Code",
                          "year" = "Time")) %>%
  rename(pCorruption = corruption,
         pRegulatory.qual = regulatory.qual)

rm(WGI)



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

tariff %>% distinct(reporter.ISO) %>% nrow
# 181
tariff %>% distinct(partner.ISO) %>% nrow
# 240


# .. Generate unique identifier ####
tariff <- tariff %>%
  mutate(commodity.code = str_pad(str_trim(commodity.code), 
                                  width = 2, side = "left", pad = "0"))

tariff$id <- paste(tariff$reporter.ISO, 
                   tariff$partner.ISO, 
                   tariff$commodity.code, 
                   tariff$year, sep = "_")


# .. Merge with panel ####
panel <- left_join(panel, tariff %>% select(-c(reporter, partner)),
                   by = c("id" = "id",
                          "reporter.ISO" = "reporter.ISO",
                          "partner.ISO" = "partner.ISO",
                          "commodity.code" = "commodity.code",
                          "year" = "year"))

rm(tariff)



## ## ## ## ## ## ## ## ## ## ##
# IMPORT HS CODES           ####
## ## ## ## ## ## ## ## ## ## ##

# .. Merge commodity and section codes ####
HS <- read.xlsx2("Data/UN Stats/HS Commodity Codes.xlsx", sheetName = "HS Codes") %>%
  mutate_all(as.character) %>%
  rename_all(tolower)

HS <- HS  %>%
  mutate(chapter = str_pad(str_trim(chapter), 
                           width = 2, side = "left", pad = "0"))

panel <- left_join(panel, HS,
                   by = c("commodity.code" = "chapter"))

panel %>% filter(is.na(section.code)) %>% distinct(commodity.code)
# 99

rm(HS)



## ## ## ## ## ## ## ## ## ## ##
# IMPORT SITC CODES         ####
## ## ## ## ## ## ## ## ## ## ##

# .. Merge SITC with HS codes ####
SITC <- read.xlsx2("Data/UN Stats/SITC4 Codes.xlsx", sheetName = "SITC4 Codes") %>%
  mutate_all(as.character) %>%
  rename(SITC.section = Section)

HStoSITC <- read.xlsx2("Data/UN Stats/HS07S4.xls", sheetName = "HS07S4",
                       startRow = 9) %>%
  mutate_all(as.character) %>%
  select(HS07, X...S4) %>%
  rename(SITC.code = X...S4) %>%
  mutate(HS07 = substr(HS07, 1, 2),
         SITC.code = substr(SITC.code, 1, 1))

HStoSITC <- left_join(HStoSITC, SITC,
                      by = c("SITC.code" = "SITC.Code"))

HStoSITC %>% filter(is.na(SITC.section))
# SITC codes I and II


# .. Merge SITC with panel ####
HStoSITC %>% distinct(HS07, SITC.code, SITC.section)
HStoSITC <- HStoSITC %>% distinct(HS07, SITC.code, SITC.section) %>%
  filter(HS07 %in% str_pad(seq(1:97), width = 2, side = "left", pad = "0"))

HStoSITC %>% distinct(HS07, SITC.code, SITC.section)
# To perform the accurate correspondence I would need to merge from HS at the 6-digit level.
# Just pick the first SITC that matches the HS code and merge on that.

HStoSITC <- HStoSITC %>% distinct(HS07, .keep_all = TRUE)
# No HS code 77 in the SITC4 correspondence table.

panel <- left_join(panel, HStoSITC,
                   by = c("commodity.code" = "HS07"))

table(panel$section, panel$SITC.section)

rm(SITC, HStoSITC)



## ## ## ## ## ## ## ## ## ## ##
# IMPORT WDI                ####
## ## ## ## ## ## ## ## ## ## ##

WDI <- WDI(indicator = "NY.GDP.MKTP.CD", start = 1999) %>%
  mutate(NY.GDP.MKTP.CD = as.numeric(NY.GDP.MKTP.CD))
WDI <- left_join(WDI, codes %>%
                   select(ISO3166.2, ISO3166.3) %>% 
                   distinct(ISO3166.2, .keep_all = T),
                 by = c("iso2c" = "ISO3166.2"))
WDI %>% filter(is.na(ISO3166.3)) %>% distinct(country)
# Only aggregates remain
WDI <- WDI %>%
  filter(!is.na(ISO3166.3)) %>%
  select(-c(iso2c,country)) %>%
  rename(GDP = NY.GDP.MKTP.CD)

save(WDI, file = "Data/WDI/WDI.Rdata")


# .. Merge with reporters ####
panel <- left_join(panel, WDI,
                   by = c("reporter.ISO" = "ISO3166.3",
                          "year" = "year"))

rm(WDI)



## ## ## ## ## ## ## ## ## ## ##
# IMPORT GROUPINGS          ####
## ## ## ## ## ## ## ## ## ## ##

# .. Merge reporters ####
panel <- left_join(panel, codes %>% 
                   select(c(ISO3166.3, UN_Region, WB_Income_Group_Code, UN_Developing.Developed)) %>%
                   distinct(ISO3166.3, .keep_all = T),
                 by = c("reporter.ISO" = "ISO3166.3")) %>%
  rename(rRegion = UN_Region,
         rIncome = WB_Income_Group_Code,
         rDev = UN_Developing.Developed)


# .. Merge partners ####
panel <- left_join(panel, codes %>% 
                     select(c(ISO3166.3, UN_Region, WB_Income_Group_Code, UN_Developing.Developed)) %>%
                     distinct(ISO3166.3, .keep_all = T),
                   by = c("partner.ISO" = "ISO3166.3")) %>%
  rename(pRegion = UN_Region,
         pIncome = WB_Income_Group_Code,
         pDev = UN_Developing.Developed)


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
         section.code, section, SITC.code, SITC.section,
         Import_value, Export_value, ReExport_value,
         Import_quantity, Export_quantity, ReExport_quantity,
         Import_weight, Export_weight, ReExport_weight,
         pImport_value, pExport_value, pReExport_value,
         pImport_quantity, pExport_quantity, pReExport_quantity,
         pImport_weight, pExport_weight, pReExport_weight,
         contig, dist, rLandlocked, pLandlocked, tariff,
         rCorruption, pCorruption, rRegulatory.qual, pRegulatory.qual,
         reporter, partner, rRegion, rDev, rIncome, pRegion, pIncome, pDev,
         GDP)

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