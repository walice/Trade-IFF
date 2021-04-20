# Data Preparation
# Alice Lepissier
# alice.lepissier@gmail.com
# Originally prepared for the United Nations Economic Commission for Africa (UNECA)

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
# .. Process total trade
# .. Convert to wide
# .. Compute total trade
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
# Natural Resource Ratio
# .. Compute proportion of natural resources in total trade
# Export Clean Data



## ## ## ## ## ## ## ## ## ## ##
# PREAMBLE                  ####
## ## ## ## ## ## ## ## ## ## ##

setwd("/scratch/alepissier/IFFe/") # Virtual server
# source("Scripts/Process Comtrade.R")
library(data.table)
library(readxl)
library(stringr)
library(tidyverse)
library(WDI)
library(xlsx)



## ## ## ## ## ## ## ## ## ## ##
# CODES MASTERLIST          ####
## ## ## ## ## ## ## ## ## ## ##

download.file("https://github.com/walice/Codes-Masterlist/raw/master/Codes_Masterlist.xlsx",
              "Data/Codes_Masterlist.xlsx")

codes <- read_excel("Data/Codes_Masterlist.xlsx", sheet = "Codes") %>%
  mutate_all(as.character)



## ## ## ## ## ## ## ## ## ## ##
# IMPORT COMTRADE           ####
## ## ## ## ## ## ## ## ## ## ##

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
# 18335160


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
# 23920190
rm(comtrade_mirror)

comtrade %>% distinct(reporter.ISO) %>% nrow
# 237
comtrade %>% distinct(partner.ISO) %>% nrow
# 237

comtrade %>% filter(duplicated(comtrade$id)) %>% nrow
# 2

comtrade <- comtrade %>%
  filter(!duplicated(comtrade$id))

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
# 2012 2012

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
# 2012 2012

rm(Import_quantity, Export_quantity, ReExport_quantity,
   Import_weight, Export_weight, ReExport_weight)


# .. Process total trade ####
load("Data/Comtrade/comtrade_total.Rdata")

comtrade_total <- comtrade_total %>%
  select(-c(Aggregate.Level, Commodity, Qty, Netweight..kg.)) %>%
  rename_all(tolower) %>%
  rename(reporter.ISO = reporter.iso, partner.ISO = partner.iso,
         value = trade.value..us.., flow = trade.flow)

noreporter <- subset(comtrade_total, reporter.ISO == "")
noreporter %>% distinct(reporter)
# Other Asia, nes
rm(noreporter)
comtrade_total <- comtrade_total %>%
  filter(reporter.ISO != "")

comtrade_total %>% distinct(partner)
# World
comtrade_total %>% distinct(commodity.code)
# TOTAL
comtrade_total <- comtrade_total %>%
  select(-c(partner, partner.ISO, commodity.code))


# .. Convert to wide ####
comtrade_total <- comtrade_total %>% 
  gather(variable, value, -c("year", "flow", "reporter", "reporter.ISO")) %>% 
  unite(temp, flow, variable) %>% 
  spread(temp, value, fill = 0)

nrow(comtrade_total)
# 3412


# .. Compute total trade ####
comtrade_total <- comtrade_total %>%
  mutate(Total_value = Import_value + Export_value + `Re-Import_value` + `Re-Export_value`) %>%
  select(reporter.ISO, year, Total_value) %>%
  mutate(reporter.ISO = as.character(reporter.ISO))
comtrade_total %>% distinct(reporter.ISO) %>% nrow
# 198

save(comtrade_total, file = "Data/Comtrade/comtrade_total_clean.Rdata")
rm(comtrade_total)



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

panel %>% filter(duplicated(panel$id)) %>% nrow
# 0

rm(comtrade)


# .. Merge country geographic data ####
geo <- read.xlsx2("Data/CEPII/geo_cepii.xls", sheetName = "geo_cepii") %>% 
  select(iso3, landlocked) %>%
  mutate(iso3 = as.character(iso3)) %>%
  filter(!duplicated(iso3))

panel <- left_join(panel, geo,
                   by = c("reporter.ISO" = "iso3")) %>%
  rename(rLandlocked = landlocked)

panel <- left_join(panel, geo,
                   by = c("partner.ISO" = "iso3")) %>%
  rename(pLandlocked = landlocked)

panel %>% filter(duplicated(panel$id)) %>% nrow
# 0

rm(dist, geo)



## ## ## ## ## ## ## ## ## ## ##
# IMPORT WGI                ####
## ## ## ## ## ## ## ## ## ## ##

WGI <- WDI(indicator = c(corruption = "CC.PER.RNK", 
                         regulatory.qual = "RQ.PER.RNK"),
           start = 1998, extra = TRUE) %>%
  select(year, iso3c, corruption, regulatory.qual) %>%
  filter(!is.na(iso3c)) %>%
  filter(!(is.na(corruption) & is.na(regulatory.qual))) %>%
  mutate(iso3c = as.character(iso3c)) %>%
  arrange(year, iso3c)
save(WGI, file = "Data/World Bank/WGI.Rdata")


# .. Interpolate missing data ####
WGI %>% distinct(year)
# Missing data for 1999 and 2001, so interpolate from either year

WGI98 <- WGI %>%
  filter(year == 1998)
WGI00 <- WGI %>%
  filter(year == 2000)
WGI02 <- WGI %>%
  filter(year == 2002)

WGI99 <- full_join(WGI98, WGI00,
                   by = "iso3c") %>%
  mutate(year = 1999)
WGI99$corruption <- rowMeans(WGI99[, c("corruption.x", "corruption.y")], na.rm = TRUE)
WGI99$regulatory.qual <- rowMeans(WGI99[, c("regulatory.qual.x", "regulatory.qual.y")], na.rm = TRUE)
WGI99 <- WGI99 %>% select(iso3c, year, corruption, regulatory.qual)

WGI01 <- full_join(WGI00, WGI02,
                   by = "iso3c") %>%
  mutate(year = 2001)
WGI01$corruption <- rowMeans(WGI01[, c("corruption.x", "corruption.y")], na.rm = TRUE)
WGI01$regulatory.qual <- rowMeans(WGI01[, c("regulatory.qual.x", "regulatory.qual.y")], na.rm = TRUE)
WGI01 <- WGI01 %>% select(iso3c, year, corruption, regulatory.qual)

WGI <- rbind(WGI, WGI99, WGI01)
rm(WGI98, WGI99, WGI00, WGI01, WGI02)


# .. Merge with reporters and partners ####
panel <- left_join(panel, WGI,
                   by = c("reporter.ISO" = "iso3c",
                          "year" = "year")) %>%
  rename(rCorruption = corruption,
         rRegulatory.qual = regulatory.qual)

panel <- left_join(panel, WGI,
                   by = c("partner.ISO" = "iso3c",
                          "year" = "year")) %>%
  rename(pCorruption = corruption,
         pRegulatory.qual = regulatory.qual)

panel %>% filter(duplicated(panel$id)) %>% nrow
# 0

rm(WGI)



## ## ## ## ## ## ## ## ## ## ##
# IMPORT WITS               ####
## ## ## ## ## ## ## ## ## ## ##

# .. Import average tariff line data ####
tariff <- read.csv("Data/WITS/DataJobID-2109586_2109586_2digitmisinvoicingnon.csv") %>%
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
# European Union

tariff <- left_join(tariff, codes %>%
                      select(Country, ISO3166.3),
                    by = c("partner" = "Country")) %>%
  rename(partner.ISO = ISO3166.3)

tariff %>% 
  filter(is.na(partner.ISO)) %>%
  distinct(partner)
# Bunkers; Unspecified;
# Special Categories; Free Zones; Neutral Zone; Other Asia, nes

tariff <- tariff %>%
  filter(!is.na(reporter.ISO) & !is.na(partner.ISO))

tariff %>% distinct(reporter.ISO) %>% nrow
# 195
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

panel %>% filter(duplicated(panel$id)) %>% nrow
# 0

rm(tariff)



## ## ## ## ## ## ## ## ## ## ##
# IMPORT HS CODES           ####
## ## ## ## ## ## ## ## ## ## ##

# .. Merge commodity and section codes ####
HS <- read.xlsx2("Data/UN Stats/HS Commodity Codes.xlsx", sheetName = "HS Codes") %>%
  mutate_all(as.character) %>%
  rename_all(tolower) %>%
  mutate(chapter = str_pad(str_trim(chapter), 
                           width = 2, side = "left", pad = "0"))

panel <- left_join(panel, HS,
                   by = c("commodity.code" = "chapter"))

panel %>% filter(is.na(section.code)) %>% distinct(commodity.code)
# 99, reserved for special use by contracting parties

panel %>% filter(commodity.code == "77") %>% nrow
# 0, HS code 77 is reserved for possible future use

panel %>% filter(commodity.code == "98") %>% nrow
# 0, HS code 98 is reserved for special use by contracting parties

panel %>% filter(duplicated(panel$id)) %>% nrow
# 0

save(HS, file = "Data/UN Stats/HS.Rdata")
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
# SITC code I


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

panel %>% filter(duplicated(panel$id)) %>% nrow
# 0

rm(SITC, HStoSITC)



## ## ## ## ## ## ## ## ## ## ##
# IMPORT WDI                ####
## ## ## ## ## ## ## ## ## ## ##

WDI <- WDI(indicator = c("NY.GDP.MKTP.CD", "NY.GNP.PCAP.CD"), start = 1999) %>%
  mutate(NY.GDP.MKTP.CD = as.numeric(NY.GDP.MKTP.CD),
         NY.GNP.PCAP.CD = as.numeric(NY.GNP.PCAP.CD))
WDI <- left_join(WDI, codes %>%
                   select(ISO3166.2, ISO3166.3) %>% 
                   distinct(ISO3166.2, .keep_all = T),
                 by = c("iso2c" = "ISO3166.2"))
WDI %>% filter(is.na(ISO3166.3)) %>% distinct(country)
# Only aggregates remain
WDI <- WDI %>%
  filter(!is.na(ISO3166.3)) %>%
  select(-c(iso2c,country)) %>%
  rename(GDP = NY.GDP.MKTP.CD,
         GNPpc = NY.GNP.PCAP.CD)

save(WDI, file = "Data/WDI/WDI.Rdata")


# .. Merge with reporters ####
panel <- left_join(panel, WDI,
                   by = c("reporter.ISO" = "ISO3166.3",
                          "year" = "year"))

panel %>% filter(duplicated(panel$id)) %>% nrow
# 0

rm(WDI)



## ## ## ## ## ## ## ## ## ## ##
# IMPORT GROUPINGS          ####
## ## ## ## ## ## ## ## ## ## ##

# .. Merge reporters ####
panel <- left_join(panel, codes %>% 
                   select(c(ISO3166.3, UN_Region, WB_Income_Group, 
                            `UN_Developing-Developed`, UNDP_HDI_Group)) %>%
                   distinct(ISO3166.3, .keep_all = T),
                 by = c("reporter.ISO" = "ISO3166.3")) %>%
  rename(rRegion = UN_Region,
         rIncome = WB_Income_Group,
         rDev = `UN_Developing-Developed`,
         rHDI = UNDP_HDI_Group)


# .. Merge partners ####
panel <- left_join(panel, codes %>% 
                     select(c(ISO3166.3, UN_Region, WB_Income_Group, 
                              `UN_Developing-Developed`, UNDP_HDI_Group)) %>%
                     distinct(ISO3166.3, .keep_all = T),
                   by = c("partner.ISO" = "ISO3166.3")) %>%
  rename(pRegion = UN_Region,
         pIncome = WB_Income_Group,
         pDev = `UN_Developing-Developed`,
         pHDI = UNDP_HDI_Group)


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

panel %>% filter(duplicated(panel$id)) %>% nrow
# 0

rm(codes, noreporter, nopartner)



## ## ## ## ## ## ## ## ## ## ##
# NATURAL RESOURCE RATIO    ####
## ## ## ## ## ## ## ## ## ## ##

# .. Compute proportion of natural resources in total trade ####
load("Data/Comtrade/comtrade_total_clean.Rdata")

panel <- left_join(panel, comtrade_total,
                   by = c("reporter.ISO", "year"))

natural_resources <- panel %>%
  filter(natural.resource == "1") %>%
  mutate(Total_nat.res = Import_value + Export_value + ReExport_value) %>%
  group_by(reporter.ISO, year) %>%
  summarize(Total_nat.res = sum(Total_nat.res, na.rm = TRUE)) %>%
  filter(!is.na(Total_nat.res))

natural_resources <- left_join(natural_resources, comtrade_total,
                               by = c("reporter.ISO", "year")) %>%
  mutate(nat_ratio = Total_nat.res / Total_value) %>%
  select(reporter.ISO, year, nat_ratio) %>%
  filter(is.finite(nat_ratio))

hist(natural_resources$nat_ratio)

save(natural_resources, file = "Data/natural_resources.Rdata")
rm(natural_resources)



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
         reporter, partner, rRegion, rDev, rHDI, rIncome, pRegion, pIncome, pDev, pHDI,
         GDP)

missing <- panel %>% 
  filter(is.na(Import_value) & is.na(Export_value) & is.na(ReExport_value) &
           is.na(pImport_value) & is.na(pExport_value) & is.na(pReExport_value))
rm(missing)

duplicates <- panel %>%
  filter(duplicated(panel$id))
rm(duplicates)

save(panel, file = "Data/Panel/panel.Rdata")
