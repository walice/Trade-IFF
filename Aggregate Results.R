# Aggregate Results
# Alice Lepissier
# alice.lepissier@gmail.com
# Originally prepared for the United Nations Economic Commission for Africa (UNECA)

## ## ## ## ## ## ## ## ## ## ##
# INDEX                     ####
## ## ## ## ## ## ## ## ## ## ##
# Preamble
# GER Out by Destination
# .. Aggregate outflows using Gross Excluding Reversals
# .. GER Import/Export IFF for Reporter-Partner-Year
# .. GER IFF for Reporter-Year (sum over Partner)
# .. GER IFF for Reporter (sum over Partner, average across Year)
# .. GER IFF for Reporter (sum over Partner, average across recent Years)
# .. GER IFF for Reporter (sum over Partner, sum over Year)
# .. GER IFF for Reporter-Partner (average across Year)
# .. GER IFF for Reporter-Partner (average across recent Years)
# .. GER IFF for Reporter-Partner (sum over Year)
# .. GER IFF for Partner (sum over Reporter, average across Year)
# .. GER IFF for Partner (sum over Reporter, average across recent Years)
# .. GER IFF for Partner (sum over Reporter, sum over Year)
# .. Total GER IFF outflows per year
# .. Headline: cumulative GER IFF during 2000-2018
# GER In by Destination
# .. Aggregate inflows using Gross Excluding Reversals
# .. GER Import/Export IFF for Reporter-Partner-Year
# .. GER IFF for Reporter-Year (sum over Partner)
# .. GER IFF for Reporter (sum over Partner, average across Year)
# .. GER IFF for Reporter-Partner (average across Year)
# .. Total GER IFF inflows per year
# Net by Destination
# .. Net Import/Export IFF for Reporter-Partner-Year
# .. Net IFF for Reporter-Year (sum over Partner)
# .. Net IFF for Reporter (sum over Partner, average across Year)
# .. Net IFF for Reporter (sum over Partner, sum over Year)
# .. Net IFF for Reporter-Partner (average across Year)
# .. Net IFF for Reporter-Partner (sum over Year)
# .. Net IFF for Partner (sum over Reporter, sum over Year)
# .. Total Net IFF per year
# .. Headline: cumulative Net IFF during 2000-2018
# GER Out by Sector
# .. Aggregate outflows using Gross Excluding Reversals
# .. GER Import/Export IFF for Reporter-Sector-Year
# .. GER IFF for Reporter-Sector (average across Year)
# .. GER IFF for Reporter-Sector (average across recent Years)
# .. GER IFF for Reporter-Sector (sum over Year)
# .. GER IFF for Sector-Year (sum over Reporter)
# .. GER IFF for Sector (sum over Reporter, average across Year)
# .. GER IFF for Sector (sum over Reporter, average across recent Years)
# .. GER IFF for Sector (sum over Reporter, sum over Year)
# GER Out for Top Sectors
# .. Aggregate outflows using Gross Excluding Reversals
# .. GER Import/Export IFF for Reporter-Partner-TopSector-Year
# .. GER IFF for Reporter-TopSector-Year (sum over Partner)
# .. Top origins in top sectors
# .. GER IFF for Reporter-TopSector (sum over Partner, average across Year)
# Net by Sector
# .. Net Import/Export IFF for Reporter-Sector-Year
# .. Net IFF for Reporter-Sector (average across Year)
# .. Net IFF for Reporter-Sector (sum over Year)
# .. Net IFF for Sector (sum over Reporter, sum over Year)
# Headline Results
# .. For Africa
# .. For low and lower-middle income countries
# .. For developing countries
# .. For low-HDI countries
# Regular Trade Gaps



## ## ## ## ## ## ## ## ## ## ##
# PREAMBLE                  ####
## ## ## ## ## ## ## ## ## ## ##

setwd("/home/alepissier/IFFe/") # Virtual server
data.disk <- "/scratch/alepissier/IFFe/"
# source("Scripts/Compute IFF Estimates.R")
library(car)
library(kableExtra)
library(reshape2)
library(stargazer)
library(tidyverse)
options(scipen = 999)



## ## ## ## ## ## ## ## ## ## ##
# GER OUT BY DESTINATION    ####
## ## ## ## ## ## ## ## ## ## ##

load("Results/panel_results.Rdata")
load(paste0(data.disk, "Data/WDI/WDI.Rdata"))
load(paste0(data.disk, "Data/Comtrade/comtrade_total_clean.Rdata"))


# .. Aggregate outflows using Gross Excluding Reversals ####
GER_Imp_Dest <- panel %>%
  filter(Imp_IFF > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI,
           year,
           partner, partner.ISO, pRegion, pIncome, pDev, pHDI) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T)) %>%
  ungroup()

GER_Exp_Dest <- panel %>%
  filter(Exp_IFF > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI,
           year,
           partner, partner.ISO, pRegion, pIncome, pDev, pHDI) %>%
  summarize(Exp_IFF = sum(Exp_IFF, na.rm = T)) %>%
  ungroup()


# .. GER Import/Export IFF for Reporter-Partner-Year ####
GER_Orig_Dest_Year <- full_join(GER_Imp_Dest, GER_Exp_Dest,
                                by = c("reporter" = "reporter",
                                       "reporter.ISO" = "reporter.ISO",
                                       "rRegion" = "rRegion",
                                       "rIncome" = "rIncome",
                                       "rDev" = "rDev",
                                       "rHDI" = "rHDI",
                                       "year" = "year",
                                       "partner" = "partner",
                                       "partner.ISO" = "partner.ISO",
                                       "pRegion" = "pRegion",
                                       "pIncome" = "pIncome",
                                       "pDev" = "pDev",
                                       "pHDI" = "pHDI"))
save(GER_Orig_Dest_Year, file = "Results/Summary data-sets/GER_Orig_Dest_Year.Rdata")
write.csv(GER_Orig_Dest_Year, file = "Results/Summary data-sets/GER_Orig_Dest_Year.csv",
          row.names = F)
rm(GER_Imp_Dest, GER_Exp_Dest)

# Africa
GER_Orig_Dest_Year_Africa <- GER_Orig_Dest_Year %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)
save(GER_Orig_Dest_Year_Africa, file = "Results/Summary data-sets/GER_Orig_Dest_Year_Africa.Rdata")
write.csv(GER_Orig_Dest_Year_Africa, file = "Results/Summary data-sets/GER_Orig_Dest_Year_Africa.csv",
          row.names = F)

# Standardized by GDP (for import only)
GER_Orig_Dest_Year_std <- left_join(GER_Orig_Dest_Year %>% mutate(year = as.integer(year)),
                                    WDI,
                                    by = c("reporter.ISO" = "ISO3166.3", 
                                           "year")) %>%
  rename(rGDP = GDP,
         rGNPpc = GNPpc)
GER_Orig_Dest_Year_std <- left_join(GER_Orig_Dest_Year_std %>% mutate(year = as.integer(year)),
                                    WDI,
                                    by = c("partner.ISO" = "ISO3166.3", 
                                           "year")) %>%
  rename(pGDP = GDP,
         pGNPpc = GNPpc)
GER_Orig_Dest_Year_std <- GER_Orig_Dest_Year_std %>%
  mutate(rImp_IFF_GDP = Imp_IFF / rGDP,
         pImp_IFF_GDP = Imp_IFF / pGDP)
save(GER_Orig_Dest_Year_std, file = "Results/Summary data-sets/GER_Orig_Dest_Year_std.Rdata")
write.csv(GER_Orig_Dest_Year_std, file = "Results/Summary data-sets/GER_Orig_Dest_Year_std.csv",
          row.names = F)


# .. GER IFF for Reporter-Year (sum over Partner) ####
GER_Orig_Year <- GER_Orig_Dest_Year %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI, year) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF = Imp_IFF + Exp_IFF,
         Tot_IFF_bn = Tot_IFF / 10^9)
GER_Orig_Year <- left_join(GER_Orig_Year %>% mutate(year = as.integer(year)),
                           WDI,
                           by = c("reporter.ISO" = "ISO3166.3", 
                                  "year")) %>%
  mutate(Tot_IFF_GDP = Tot_IFF / GDP)
GER_Orig_Year <- left_join(GER_Orig_Year,
                           comtrade_total,
                           by = c("reporter.ISO", "year")) %>%
  mutate(Tot_IFF_trade = Tot_IFF / Total_value)
save(GER_Orig_Year, file = "Results/Summary data-sets/GER_Orig_Year.Rdata")
write.csv(GER_Orig_Year, file = "Results/Summary data-sets/GER_Orig_Year.csv",
          row.names = F)

# Africa
GER_Orig_Year_Africa <- GER_Orig_Year %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)
save(GER_Orig_Year_Africa, file = "Results/Summary data-sets/GER_Orig_Year_Africa.Rdata")
write.csv(GER_Orig_Year_Africa, file = "Results/Summary data-sets/GER_Orig_Year_Africa.csv",
          row.names = F)

# Low and lower-middle income countries
GER_Orig_Year_LMIC <- GER_Orig_Year %>%
  filter(rIncome == "LIC" | rIncome == "LMC") %>%
  select(-rIncome)
save(GER_Orig_Year_LMIC, file = "Results/Summary data-sets/GER_Orig_Year_LMIC.Rdata")
write.csv(GER_Orig_Year_LMIC, file = "Results/Summary data-sets/GER_Orig_Year_LMIC.csv",
          row.names = F)

# Developing countries
GER_Orig_Year_Developing <- GER_Orig_Year %>%
  filter(rDev == "Developing") %>%
  select(-rDev)
save(GER_Orig_Year_Developing, file = "Results/Summary data-sets/GER_Orig_Year_Developing.Rdata")
write.csv(GER_Orig_Year_Developing, file = "Results/Summary data-sets/GER_Orig_Year_Developing.csv",
          row.names = F)

# Low-HDI countries
GER_Orig_Year_LowHDI <- GER_Orig_Year %>%
  filter(rHDI == "Lo HDI" | rHDI == "Med HDI") %>%
  select(-rHDI)
save(GER_Orig_Year_LowHDI, file = "Results/Summary data-sets/GER_Orig_Year_LowHDI.Rdata")
write.csv(GER_Orig_Year_LowHDI, file = "Results/Summary data-sets/GER_Orig_Year_LowHDI.csv",
          row.names = F)


# .. GER IFF for Reporter (sum over Partner, average across Year) ####
GER_Orig_Avg <- GER_Orig_Year %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI) %>%
  summarize(Imp_IFF = mean(Imp_IFF, na.rm = T),
            Exp_IFF = mean(Exp_IFF, na.rm = T),
            Tot_IFF = mean(Tot_IFF, na.rm = T),
            Tot_IFF_bn = mean(Tot_IFF_bn, na.rm = T),
            Tot_IFF_GDP = mean(Tot_IFF_GDP, na.rm = T),
            Tot_IFF_trade = mean(Tot_IFF_trade, na.rm = T)) %>%
  ungroup()
save(GER_Orig_Avg, file = "Results/Summary data-sets/GER_Orig_Avg.Rdata")
write.csv(GER_Orig_Avg, file = "Results/Summary data-sets/GER_Orig_Avg.csv",
          row.names = F)

# Africa
GER_Orig_Avg_Africa <- GER_Orig_Avg %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)
save(GER_Orig_Avg_Africa, file = "Results/Summary data-sets/GER_Orig_Avg_Africa.Rdata")
write.csv(GER_Orig_Avg_Africa, file = "Results/Summary data-sets/GER_Orig_Avg_Africa.csv",
          row.names = F)

# Low and lower-middle income countries
GER_Orig_Avg_LMIC <- GER_Orig_Avg %>%
  filter(rIncome == "LIC" | rIncome == "LMC") %>%
  select(-rIncome)
save(GER_Orig_Avg_LMIC, file = "Results/Summary data-sets/GER_Orig_Avg_LMIC.Rdata")
write.csv(GER_Orig_Avg_LMIC, file = "Results/Summary data-sets/GER_Orig_Avg_LMIC.csv",
          row.names = F)

# Developing countries
GER_Orig_Avg_Developing <- GER_Orig_Avg %>%
  filter(rDev == "Developing") %>%
  select(-rDev)
save(GER_Orig_Avg_Developing, file = "Results/Summary data-sets/GER_Orig_Avg_Developing.Rdata")
write.csv(GER_Orig_Avg_Developing, file = "Results/Summary data-sets/GER_Orig_Avg_Developing.csv",
          row.names = F)

# Low-HDI countries
GER_Orig_Avg_LowHDI <- GER_Orig_Avg %>%
  filter(rHDI == "Lo HDI" | rHDI == "Med HDI") %>%
  select(-rHDI)
save(GER_Orig_Avg_LowHDI, file = "Results/Summary data-sets/GER_Orig_Avg_LowHDI.Rdata")
write.csv(GER_Orig_Avg_LowHDI, file = "Results/Summary data-sets/GER_Orig_Avg_LowHDI.csv",
          row.names = F)


# .. GER IFF for Reporter (sum over Partner, average across recent Years) ####
missing <- right_join(GER_Orig_Year, GER_Orig_Year %>% 
                        expand(reporter.ISO, year),
                      by = c("reporter.ISO", "year")) %>%
  filter(year >= 2016) %>%
  group_by(reporter.ISO) %>% 
  summarize(missing = sum(is.na(reporter))) %>%
  filter(missing == 3) %>%
  pull(reporter.ISO)

GER_Orig_Avg_last3 <- GER_Orig_Year %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI) %>%
  top_n(3, year) %>%
  summarize(Imp_IFF = mean(Imp_IFF, na.rm = T),
            Exp_IFF = mean(Exp_IFF, na.rm = T),
            Tot_IFF = mean(Tot_IFF, na.rm = T),
            Tot_IFF_bn = mean(Tot_IFF_bn, na.rm = T),
            Tot_IFF_GDP = mean(Tot_IFF_GDP, na.rm = T),
            Tot_IFF_trade = mean(Tot_IFF_trade, na.rm = T)) %>%
  ungroup()
save(GER_Orig_Avg_last3, file = "Results/Summary data-sets/GER_Orig_Avg_last3.Rdata")
write.csv(GER_Orig_Avg_last3, file = "Results/Summary data-sets/GER_Orig_Avg_last3.csv",
          row.names = F)


# .. GER IFF for Reporter (sum over Partner, sum over Year) ####
GER_Orig_Sum <- GER_Orig_Year %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()
save(GER_Orig_Sum, file = "Results/Summary data-sets/GER_Orig_Sum.Rdata")
write.csv(GER_Orig_Sum, file = "Results/Summary data-sets/GER_Orig_Sum.csv",
          row.names = F)

# Africa
GER_Orig_Sum_Africa <- GER_Orig_Sum %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)
save(GER_Orig_Sum_Africa, file = "Results/Summary data-sets/GER_Orig_Sum_Africa.Rdata")
write.csv(GER_Orig_Sum_Africa, file = "Results/Summary data-sets/GER_Orig_Sum_Africa.csv",
          row.names = F)


# .. GER IFF for Reporter-Partner (average across Year) ####
GER_Orig_Dest_Avg <- GER_Orig_Dest_Year %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI, 
           partner, partner.ISO, pRegion, pIncome, pDev, pHDI) %>%
  summarize(Imp_IFF = mean(Imp_IFF, na.rm = T),
            Exp_IFF = mean(Exp_IFF, na.rm = T)) %>%
  ungroup() %>%
  mutate_at(c("Imp_IFF", "Exp_IFF"), ~replace(., is.nan(.), 0)) %>%
  mutate(Tot_IFF = Imp_IFF + Exp_IFF,
         Tot_IFF_bn = Tot_IFF / 10^9)
weights <- GER_Orig_Dest_Year %>%
  distinct(reporter.ISO, year, .keep_all = T) %>%
  group_by(reporter.ISO) %>%
  mutate(weight = 1/n()) %>%
  ungroup()
WDI <- left_join(WDI, weights %>% 
                   mutate(year = as.integer(year)) %>%
                   select(reporter.ISO, year, weight),
                 by = c("ISO3166.3" = "reporter.ISO",
                        "year")) %>%
  mutate(weight = ifelse(is.na(weight), 0, weight)) %>%
  group_by(ISO3166.3) %>%
  summarize(GDP = weighted.mean(GDP, weight),
            GNPpc = weighted.mean(GNPpc, weight))
GER_Orig_Dest_Avg <- left_join(GER_Orig_Dest_Avg,
                               WDI,
                               by = c("reporter.ISO" = "ISO3166.3")) %>%
  rename(rGDP = GDP,
         rGNPpc = GNPpc)
GER_Orig_Dest_Avg <- left_join(GER_Orig_Dest_Avg,
                               WDI,
                               by = c("partner.ISO" = "ISO3166.3")) %>%
  rename(pGDP = GDP,
         pGNPpc = GNPpc)
save(GER_Orig_Dest_Avg, file = "Results/Summary data-sets/GER_Orig_Dest_Avg.Rdata")
write.csv(GER_Orig_Dest_Avg, file = "Results/Summary data-sets/GER_Orig_Dest_Avg.csv",
          row.names = F)

# Africa
GER_Orig_Dest_Avg_Africa <- GER_Orig_Dest_Avg %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)
save(GER_Orig_Dest_Avg_Africa, file = "Results/Summary data-sets/GER_Orig_Dest_Avg_Africa.Rdata")
write.csv(GER_Orig_Dest_Avg_Africa, file = "Results/Summary data-sets/GER_Orig_Dest_Avg_Africa.csv",
          row.names = F)

# Low and lower-middle income countries
GER_Orig_Dest_Avg_LMIC <- GER_Orig_Dest_Avg %>%
  filter(rIncome == "LIC" | rIncome == "LMC") %>%
  select(-rIncome)
save(GER_Orig_Dest_Avg_LMIC, file = "Results/Summary data-sets/GER_Orig_Dest_Avg_LMIC.Rdata")
write.csv(GER_Orig_Dest_Avg_LMIC, file = "Results/Summary data-sets/GER_Orig_Dest_Avg_LMIC.csv",
          row.names = F)

# Developing countries
GER_Orig_Dest_Avg_Developing <- GER_Orig_Dest_Avg %>%
  filter(rDev == "Developing") %>%
  select(-rDev)
save(GER_Orig_Dest_Avg_Developing, file = "Results/Summary data-sets/GER_Orig_Dest_Avg_Developing.Rdata")
write.csv(GER_Orig_Dest_Avg_Developing, file = "Results/Summary data-sets/GER_Orig_Dest_Avg_Developing.csv",
          row.names = F)

# Low-HDI countries
GER_Orig_Dest_Avg_LowHDI <- GER_Orig_Dest_Avg %>%
  filter(rHDI == "Lo HDI" | rHDI == "Med HDI") %>%
  select(-rHDI)
save(GER_Orig_Dest_Avg_LowHDI, file = "Results/Summary data-sets/GER_Orig_Dest_Avg_LowHDI.Rdata")
write.csv(GER_Orig_Dest_Avg_LowHDI, file = "Results/Summary data-sets/GER_Orig_Dest_Avg_LowHDI.csv",
          row.names = F)


# .. GER IFF for Reporter-Partner (average across recent Years) ####
GER_Orig_Dest_Avg_last3 <- GER_Orig_Dest_Year %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI, 
           partner, partner.ISO, pRegion, pIncome, pDev, pHDI) %>%
  top_n(3, year) %>%
  summarize(Imp_IFF = mean(Imp_IFF, na.rm = T),
            Exp_IFF = mean(Exp_IFF, na.rm = T)) %>%
  ungroup() %>%
  mutate_at(c("Imp_IFF", "Exp_IFF"), ~replace(., is.nan(.), 0)) %>%
  mutate(Tot_IFF = Imp_IFF + Exp_IFF,
         Tot_IFF_bn = Tot_IFF / 10^9)
save(GER_Orig_Dest_Avg_last3, file = "Results/Summary data-sets/GER_Orig_Dest_Avg_last3.Rdata")
write.csv(GER_Orig_Dest_Avg_last3, file = "Results/Summary data-sets/GER_Orig_Dest_Avg_last3.csv",
          row.names = F)

# Low and lower-middle income countries
GER_Orig_Dest_Avg_last3_LMIC <- GER_Orig_Dest_Avg_last3 %>%
  filter(rIncome == "LIC" | rIncome == "LMC") %>%
  select(-rIncome)
save(GER_Orig_Dest_Avg_last3_LMIC, file = "Results/Summary data-sets/GER_Orig_Dest_Avg_last3_LMIC.Rdata")
write.csv(GER_Orig_Dest_Avg_last3_LMIC, file = "Results/Summary data-sets/GER_Orig_Dest_Avg_last3_LMIC.csv",
          row.names = F)


# .. GER IFF for Reporter-Partner (sum over Year) ####
GER_Orig_Dest_Sum <- GER_Orig_Dest_Year %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI,
           partner, partner.ISO, pRegion, pIncome, pDev, pHDI) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF = Imp_IFF + Exp_IFF,
         Tot_IFF_bn = Tot_IFF / 10^9)

# Africa
GER_Orig_Dest_Sum_Africa <- GER_Orig_Dest_Sum %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)
save(GER_Orig_Dest_Sum_Africa, file = "Results/Summary data-sets/GER_Orig_Dest_Sum_Africa.Rdata")
write.csv(GER_Orig_Dest_Sum_Africa, file = "Results/Summary data-sets/GER_Orig_Dest_Sum_Africa.csv",
          row.names = F)


# .. GER IFF for Partner (sum over Reporter, average across Year) ####
GER_Dest_Avg <- GER_Orig_Dest_Avg %>%
  group_by(partner, partner.ISO, pRegion, pIncome, pDev, pHDI) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()
save(GER_Dest_Avg, file = "Results/Summary data-sets/GER_Dest_Avg.Rdata")
write.csv(GER_Dest_Avg, file = "Results/Summary data-sets/GER_Dest_Avg.csv",
          row.names = F)

# Africa
GER_Dest_Avg_Africa <- GER_Orig_Dest_Avg_Africa %>%
  group_by(partner, partner.ISO, pRegion, pIncome, pDev, pHDI) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()
save(GER_Dest_Avg_Africa, file = "Results/Summary data-sets/GER_Dest_Avg_Africa.Rdata")
write.csv(GER_Dest_Avg_Africa, file = "Results/Summary data-sets/GER_Dest_Avg_Africa.csv",
          row.names = F)

# Low and lower-middle income countries
GER_Dest_Avg_LMIC <- GER_Orig_Dest_Avg_LMIC %>%
  group_by(partner, partner.ISO, pRegion, pIncome, pDev, pHDI) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()
save(GER_Dest_Avg_LMIC, file = "Results/Summary data-sets/GER_Dest_Avg_LMIC.Rdata")
write.csv(GER_Dest_Avg_LMIC, file = "Results/Summary data-sets/GER_Dest_Avg_LMIC.csv",
          row.names = F)

# Developing countries
GER_Dest_Avg_Developing <- GER_Orig_Dest_Avg_Developing %>%
  group_by(partner, partner.ISO, pRegion, pIncome, pDev, pHDI) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()
save(GER_Dest_Avg_Developing, file = "Results/Summary data-sets/GER_Dest_Avg_Developing.Rdata")
write.csv(GER_Dest_Avg_Developing, file = "Results/Summary data-sets/GER_Dest_Avg_Developing.csv",
          row.names = F)

# Low-HDI countries
GER_Dest_Avg_LowHDI <- GER_Orig_Dest_Avg_LowHDI %>%
  group_by(partner, partner.ISO, pRegion, pIncome, pDev, pHDI) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()
save(GER_Dest_Avg_LowHDI, file = "Results/Summary data-sets/GER_Dest_Avg_LowHDI.Rdata")
write.csv(GER_Dest_Avg_LowHDI, file = "Results/Summary data-sets/GER_Dest_Avg_LowHDI.csv",
          row.names = F)


# .. GER IFF for Partner (sum over Reporter, average across recent Years) ####
GER_Dest_Avg_last3 <- GER_Orig_Dest_Avg_last3 %>%
  group_by(partner, partner.ISO, pRegion, pIncome, pDev, pHDI) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()
save(GER_Dest_Avg_last3, file = "Results/Summary data-sets/GER_Dest_Avg_last3.Rdata")
write.csv(GER_Dest_Avg_last3, file = "Results/Summary data-sets/GER_Dest_Avg_last3.csv",
          row.names = F)

# Low and lower-middle income countries
GER_Dest_Avg_last3_LMIC <- GER_Orig_Dest_Avg_last3_LMIC %>%
  group_by(partner, partner.ISO, pRegion, pIncome, pDev, pHDI) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()
save(GER_Dest_Avg_last3_LMIC, file = "Results/Summary data-sets/GER_Dest_Avg_last3_LMIC.Rdata")
write.csv(GER_Dest_Avg_last3_LMIC, file = "Results/Summary data-sets/GER_Dest_Avg_last3_LMIC.csv",
          row.names = F)


# .. GER IFF for Partner (sum over Reporter, sum over Year) ####
GER_Dest_Sum <- GER_Orig_Dest_Sum %>%
  group_by(partner, partner.ISO, pRegion, pIncome, pDev, pHDI) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()
save(GER_Dest_Sum, file = "Results/Summary data-sets/GER_Dest_Sum.Rdata")
write.csv(GER_Dest_Sum, file = "Results/Summary data-sets/GER_Dest_Sum.csv",
          row.names = F)

# Africa
GER_Dest_Sum_Africa <- GER_Orig_Dest_Sum_Africa %>%
  group_by(partner, partner.ISO, pRegion, pIncome, pDev, pHDI) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()
save(GER_Dest_Sum_Africa, file = "Results/Summary data-sets/GER_Dest_Sum_Africa.Rdata")
write.csv(GER_Dest_Sum_Africa, file = "Results/Summary data-sets/GER_Dest_Sum_Africa.csv",
          row.names = F)


# .. Total GER IFF outflows per year ####
# World
GER_Year <- GER_Orig_Year %>%
  group_by(year) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T),
            GDP = sum(GDP, na.rm = T),
            Total_value = sum(Total_value, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF_GDP = Tot_IFF / GDP,
         Tot_IFF_trade = Tot_IFF / Total_value)
save(GER_Year, file = "Results/Summary data-sets/GER_Year.Rdata")
write.csv(GER_Year, file = "Results/Summary data-sets/GER_Year.csv",
          row.names = F)

# Africa
GER_Year_Africa <- GER_Orig_Year_Africa %>%
  group_by(year) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T),
            GDP = sum(GDP, na.rm = T),
            Total_value = sum(Total_value, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF_GDP = Tot_IFF / GDP,
         Tot_IFF_trade = Tot_IFF / Total_value)
save(GER_Year_Africa, file = "Results/Summary data-sets/GER_Year_Africa.Rdata")
write.csv(GER_Year_Africa, file = "Results/Summary data-sets/GER_Year_Africa.csv",
          row.names = F)

# Low and lower-middle income countries
GER_Year_LMIC <- GER_Orig_Year_LMIC %>%
  group_by(year) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T),
            GDP = sum(GDP, na.rm = T),
            Total_value = sum(Total_value, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF_GDP = Tot_IFF / GDP,
         Tot_IFF_trade = Tot_IFF / Total_value)
save(GER_Year_LMIC, file = "Results/Summary data-sets/GER_Year_LMIC.Rdata")
write.csv(GER_Year_LMIC, file = "Results/Summary data-sets/GER_Year_LMIC.csv",
          row.names = F)

# Developing countries
GER_Year_Developing <- GER_Orig_Year_Developing %>%
  group_by(year) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T),
            GDP = sum(GDP, na.rm = T),
            Total_value = sum(Total_value, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF_GDP = Tot_IFF / GDP,
         Tot_IFF_trade = Tot_IFF / Total_value)
save(GER_Year_Developing, file = "Results/Summary data-sets/GER_Year_Developing.Rdata")
write.csv(GER_Year_Developing, file = "Results/Summary data-sets/GER_Year_Developing.csv",
          row.names = F)

# Low-HDI countries
GER_Year_LowHDI <- GER_Orig_Year_LowHDI %>%
  group_by(year) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T),
            GDP = sum(GDP, na.rm = T),
            Total_value = sum(Total_value, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF_GDP = Tot_IFF / GDP,
         Tot_IFF_trade = Tot_IFF / Total_value)
save(GER_Year_LowHDI, file = "Results/Summary data-sets/GER_Year_LowHDI.Rdata")
write.csv(GER_Year_LowHDI, file = "Results/Summary data-sets/GER_Year_LowHDI.csv",
          row.names = F)


# .. Headline: cumulative GER IFF during 2000-2018 ####
# Africa
GER_Africa <- GER_Year_Africa %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T))
save(GER_Africa, file = "Results/Summary data-sets/GER_Africa.Rdata")
write.csv(GER_Africa, file = "Results/Summary data-sets/GER_Africa.csv",
          row.names = F)

# Low and lower-middle income countries
GER_LMIC <- GER_Year_LMIC %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T))
save(GER_LMIC, file = "Results/Summary data-sets/GER_LMIC.Rdata")
write.csv(GER_LMIC, file = "Results/Summary data-sets/GER_LMIC.csv",
          row.names = F)

# Developing countries
GER_Developing <- GER_Year_Developing %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T))
save(GER_Developing, file = "Results/Summary data-sets/GER_Developing.Rdata")
write.csv(GER_Developing, file = "Results/Summary data-sets/GER_Developing.csv",
          row.names = F)

# Low-HDI countries during 2000-2018
GER_LowHDI <- GER_Year_LowHDI %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T))
save(GER_LowHDI, file = "Results/Summary data-sets/GER_LowHDI.Rdata")
write.csv(GER_LowHDI, file = "Results/Summary data-sets/GER_LowHDI.csv",
          row.names = F)



## ## ## ## ## ## ## ## ## ## ##
# GER IN BY DESTINATION     ####
## ## ## ## ## ## ## ## ## ## ##

load(paste0(data.disk, "Data/WDI/WDI.Rdata"))
load(paste0(data.disk, "Data/Comtrade/comtrade_total_clean.Rdata"))

# .. Aggregate inflows using Gross Excluding Reversals ####
GER_Imp_Dest <- panel %>%
  filter(Imp_IFF < 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI,
           year,
           partner, partner.ISO, pRegion, pIncome, pDev, pHDI) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T)) %>%
  ungroup()

GER_Exp_Dest <- panel %>%
  filter(Exp_IFF < 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI,
           year,
           partner, partner.ISO, pRegion, pIncome, pDev, pHDI) %>%
  summarize(Exp_IFF = sum(Exp_IFF, na.rm = T)) %>%
  ungroup()


# .. GER Import/Export IFF for Reporter-Partner-Year ####
Inflow_GER_Orig_Dest_Year <- full_join(GER_Imp_Dest, GER_Exp_Dest,
                                       by = c("reporter" = "reporter",
                                              "reporter.ISO" = "reporter.ISO",
                                              "rRegion" = "rRegion",
                                              "rIncome" = "rIncome",
                                              "rDev" = "rDev",
                                              "rHDI" = "rHDI",
                                              "year" = "year",
                                              "partner" = "partner",
                                              "partner.ISO" = "partner.ISO",
                                              "pRegion" = "pRegion",
                                              "pIncome" = "pIncome",
                                              "pDev" = "pDev",
                                              "pHDI" = "pHDI"))
save(Inflow_GER_Orig_Dest_Year, file = "Results/Summary data-sets/Inflow_GER_Orig_Dest_Year.Rdata")
write.csv(Inflow_GER_Orig_Dest_Year, file = "Results/Summary data-sets/Inflow_GER_Orig_Dest_Year.csv",
          row.names = F)
rm(GER_Imp_Dest, GER_Exp_Dest)


# .. GER IFF for Reporter-Year (sum over Partner) ####
Inflow_GER_Orig_Year <- Inflow_GER_Orig_Dest_Year %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI, year) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF = Imp_IFF + Exp_IFF,
         Tot_IFF_bn = Tot_IFF / 10^9)
Inflow_GER_Orig_Year <- left_join(Inflow_GER_Orig_Year %>% mutate(year = as.integer(year)),
                                  WDI,
                                  by = c("reporter.ISO" = "ISO3166.3", 
                                         "year")) %>%
  mutate(Tot_IFF_GDP = Tot_IFF / GDP)
Inflow_GER_Orig_Year <- left_join(Inflow_GER_Orig_Year,
                                  comtrade_total,
                                  by = c("reporter.ISO", "year")) %>%
  mutate(Tot_IFF_trade = Tot_IFF / Total_value)
save(Inflow_GER_Orig_Year, file = "Results/Summary data-sets/Inflow_GER_Orig_Year.Rdata")
write.csv(Inflow_GER_Orig_Year, file = "Results/Summary data-sets/Inflow_GER_Orig_Year.csv",
          row.names = F)


# .. GER IFF for Reporter (sum over Partner, average across Year) ####
Inflow_GER_Orig_Avg <- Inflow_GER_Orig_Year %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI) %>%
  summarize(Imp_IFF = mean(Imp_IFF, na.rm = T),
            Exp_IFF = mean(Exp_IFF, na.rm = T),
            Tot_IFF = mean(Tot_IFF, na.rm = T),
            Tot_IFF_bn = mean(Tot_IFF_bn, na.rm = T),
            Tot_IFF_GDP = mean(Tot_IFF_GDP, na.rm = T),
            Tot_IFF_trade = mean(Tot_IFF_trade, na.rm = T)) %>%
  ungroup()
save(Inflow_GER_Orig_Avg, file = "Results/Summary data-sets/Inflow_GER_Orig_Avg.Rdata")
write.csv(Inflow_GER_Orig_Avg, file = "Results/Summary data-sets/Inflow_GER_Orig_Avg.csv",
          row.names = F)


# .. GER IFF for Reporter-Partner (average across Year) ####
Inflow_GER_Orig_Dest_Avg <- Inflow_GER_Orig_Dest_Year %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI, 
           partner, partner.ISO, pRegion, pIncome, pDev, pHDI) %>%
  summarize(Imp_IFF = mean(Imp_IFF, na.rm = T),
            Exp_IFF = mean(Exp_IFF, na.rm = T)) %>%
  ungroup() %>%
  mutate_at(c("Imp_IFF", "Exp_IFF"), ~replace(., is.nan(.), 0)) %>%
  mutate(Tot_IFF = Imp_IFF + Exp_IFF,
         Tot_IFF_bn = Tot_IFF / 10^9)
weights <- Inflow_GER_Orig_Dest_Year %>%
  distinct(reporter.ISO, year, .keep_all = T) %>%
  group_by(reporter.ISO) %>%
  mutate(weight = 1/n()) %>%
  ungroup()
WDI <- left_join(WDI, weights %>% 
                   mutate(year = as.integer(year)) %>%
                   select(reporter.ISO, year, weight),
                 by = c("ISO3166.3" = "reporter.ISO",
                        "year")) %>%
  mutate(weight = ifelse(is.na(weight), 0, weight)) %>%
  group_by(ISO3166.3) %>%
  summarize(GDP = weighted.mean(GDP, weight),
            GNPpc = weighted.mean(GNPpc, weight))
Inflow_GER_Orig_Dest_Avg <- left_join(Inflow_GER_Orig_Dest_Avg,
                                      WDI,
                                      by = c("reporter.ISO" = "ISO3166.3")) %>%
  rename(rGDP = GDP,
         rGNPpc = GNPpc)
Inflow_GER_Orig_Dest_Avg <- left_join(Inflow_GER_Orig_Dest_Avg,
                                      WDI,
                                      by = c("partner.ISO" = "ISO3166.3")) %>%
  rename(pGDP = GDP,
         pGNPpc = GNPpc)
save(Inflow_GER_Orig_Dest_Avg, file = "Results/Summary data-sets/Inflow_GER_Orig_Dest_Avg.Rdata")
write.csv(Inflow_GER_Orig_Dest_Avg, file = "Results/Summary data-sets/Inflow_GER_Orig_Dest_Avg.csv",
          row.names = F)


# .. Total GER IFF inflows per year ####
# World
Inflow_GER_Year <- Inflow_GER_Orig_Year %>%
  group_by(year) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T),
            GDP = sum(GDP, na.rm = T),
            Total_value = sum(Total_value, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF_GDP = Tot_IFF / GDP,
         Tot_IFF_trade = Tot_IFF / Total_value)
save(Inflow_GER_Year, file = "Results/Summary data-sets/Inflow_GER_Year.Rdata")
write.csv(Inflow_GER_Year, file = "Results/Summary data-sets/Inflow_GER_Year.csv",
          row.names = F)

# Africa
Inflow_GER_Year_Africa <- Inflow_GER_Orig_Year %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion) %>%
  group_by(year) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T),
            GDP = sum(GDP, na.rm = T),
            Total_value = sum(Total_value, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF_GDP = Tot_IFF / GDP,
         Tot_IFF_trade = Tot_IFF / Total_value)
save(Inflow_GER_Year_Africa, file = "Results/Summary data-sets/Inflow_GER_Year_Africa.Rdata")
write.csv(Inflow_GER_Year_Africa, file = "Results/Summary data-sets/Inflow_GER_Year_Africa.csv",
          row.names = F)

# LMIC
Inflow_GER_Year_LMIC <- Inflow_GER_Orig_Year %>%
  filter(rIncome == "LIC" | rIncome == "LMC") %>%
  select(-rIncome) %>%
  group_by(year) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T),
            GDP = sum(GDP, na.rm = T),
            Total_value = sum(Total_value, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF_GDP = Tot_IFF / GDP,
         Tot_IFF_trade = Tot_IFF / Total_value)
save(Inflow_GER_Year_LMIC, file = "Results/Summary data-sets/Inflow_GER_Year_LMIC.Rdata")
write.csv(Inflow_GER_Year_LMIC, file = "Results/Summary data-sets/Inflow_GER_Year_LMIC.csv",
          row.names = F)



## ## ## ## ## ## ## ## ## ## ##
# NET BY DESTINATION        ####
## ## ## ## ## ## ## ## ## ## ##

load(paste0(data.disk, "Data/WDI/WDI.Rdata"))
load(paste0(data.disk, "Data/Comtrade/comtrade_total_clean.Rdata"))


# .. Net Import/Export IFF for Reporter-Partner-Year ####
Net_Orig_Dest_Year <- panel %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI,
           partner, partner.ISO, pRegion, pIncome, pDev, pHDI,
           year) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T)) %>%
  ungroup()
save(Net_Orig_Dest_Year, file = "Results/Summary data-sets/Net_Orig_Dest_Year.Rdata")
write.csv(Net_Orig_Dest_Year, file = "Results/Summary data-sets/Net_Orig_Dest_Year.csv",
          row.names = F)

# Africa
Net_Orig_Dest_Year_Africa <- Net_Orig_Dest_Year %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)
save(Net_Orig_Dest_Year_Africa, file = "Results/Summary data-sets/Net_Orig_Dest_Year_Africa.Rdata")
write.csv(Net_Orig_Dest_Year_Africa, file = "Results/Summary data-sets/Net_Orig_Dest_Year_Africa.csv",
          row.names = F)


# .. Net IFF for Reporter-Year (sum over Partner) ####
Net_Orig_Year <- Net_Orig_Dest_Year %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI, year) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF = Imp_IFF + Exp_IFF,
         Tot_IFF_bn = Tot_IFF / 10^9)
Net_Orig_Year <- left_join(Net_Orig_Year %>% mutate(year = as.integer(year)),
                           WDI,
                           by = c("reporter.ISO" = "ISO3166.3", 
                                  "year")) %>%
  mutate(Tot_IFF_GDP = Tot_IFF / GDP)
Net_Orig_Year <- left_join(Net_Orig_Year,
                           comtrade_total,
                           by = c("reporter.ISO", "year")) %>%
  mutate(Tot_IFF_trade = Tot_IFF / Total_value)
save(Net_Orig_Year, file = "Results/Summary data-sets/Net_Orig_Year.Rdata")
write.csv(Net_Orig_Year, file = "Results/Summary data-sets/Net_Orig_Year.csv",
          row.names = F)

# Africa
Net_Orig_Year_Africa <- Net_Orig_Year %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)
save(Net_Orig_Year_Africa, file = "Results/Summary data-sets/Net_Orig_Year_Africa.Rdata")
write.csv(Net_Orig_Year_Africa, file = "Results/Summary data-sets/Net_Orig_Year_Africa.csv",
          row.names = F)

# Low and lower-middle income countries
Net_Orig_Year_LMIC <- Net_Orig_Year %>%
  filter(rIncome == "LIC" | rIncome == "LMC") %>%
  select(-rIncome)
save(Net_Orig_Year_LMIC, file = "Results/Summary data-sets/Net_Orig_Year_LMIC.Rdata")
write.csv(Net_Orig_Year_LMIC, file = "Results/Summary data-sets/Net_Orig_Year_LMIC.csv",
          row.names = F)

# Developing countries
Net_Orig_Year_Developing <- Net_Orig_Year %>%
  filter(rDev == "Developing") %>%
  select(-rDev)
save(Net_Orig_Year_Developing, file = "Results/Summary data-sets/Net_Orig_Year_Developing.Rdata")
write.csv(Net_Orig_Year_Developing, file = "Results/Summary data-sets/Net_Orig_Year_Developing.csv",
          row.names = F)

# Low-HDI countries
Net_Orig_Year_LowHDI <- Net_Orig_Year %>%
  filter(rHDI == "Lo HDI" | rHDI == "Med HDI") %>%
  select(-rHDI)
save(Net_Orig_Year_LowHDI, file = "Results/Summary data-sets/Net_Orig_Year_LowHDI.Rdata")
write.csv(Net_Orig_Year_LowHDI, file = "Results/Summary data-sets/Net_Orig_Year_LowHDI.csv",
          row.names = F)


# .. Net IFF for Reporter (sum over Partner, average across Year) ####
Net_Orig_Avg <- Net_Orig_Year %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI) %>%
  summarize(Imp_IFF = mean(Imp_IFF, na.rm = T),
            Exp_IFF = mean(Exp_IFF, na.rm = T),
            Tot_IFF = mean(Tot_IFF, na.rm = T),
            Tot_IFF_bn = mean(Tot_IFF_bn, na.rm = T),
            Tot_IFF_GDP = mean(Tot_IFF_GDP, na.rm = T),
            Tot_IFF_trade = mean(Tot_IFF_trade, na.rm = T)) %>%
  ungroup()
save(Net_Orig_Avg, file = "Results/Summary data-sets/Net_Orig_Avg.Rdata")
write.csv(Net_Orig_Avg, file = "Results/Summary data-sets/Net_Orig_Avg.csv",
          row.names = F)

# Africa
Net_Orig_Avg_Africa <- Net_Orig_Avg %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)
save(Net_Orig_Avg_Africa, file = "Results/Summary data-sets/Net_Orig_Avg_Africa.Rdata")
write.csv(Net_Orig_Avg_Africa, file = "Results/Summary data-sets/Net_Orig_Avg_Africa.csv",
          row.names = F)

# Low and lower-middle income countries
Net_Orig_Avg_LMIC <- Net_Orig_Avg %>%
  filter(rIncome == "LIC" | rIncome == "LMC") %>%
  select(-rIncome)
save(Net_Orig_Avg_LMIC, file = "Results/Summary data-sets/Net_Orig_Avg_LMIC.Rdata")
write.csv(Net_Orig_Avg_LMIC, file = "Results/Summary data-sets/Net_Orig_Avg_LMIC.csv",
          row.names = F)

# Developing countries
Net_Orig_Avg_Developing <- Net_Orig_Avg %>%
  filter(rDev == "Developing") %>%
  select(-rDev)
save(Net_Orig_Avg_Developing, file = "Results/Summary data-sets/Net_Orig_Avg_Developing.Rdata")
write.csv(Net_Orig_Avg_Developing, file = "Results/Summary data-sets/Net_Orig_Avg_Developing.csv",
          row.names = F)

# Low-HDI countries
Net_Orig_Avg_LowHDI <- Net_Orig_Avg %>%
  filter(rHDI == "Lo HDI" | rHDI == "Med HDI") %>%
  select(-rHDI)
save(Net_Orig_Avg_LowHDI, file = "Results/Summary data-sets/Net_Orig_Avg_LowHDI.Rdata")
write.csv(Net_Orig_Avg_LowHDI, file = "Results/Summary data-sets/Net_Orig_Avg_LowHDI.csv",
          row.names = F)


# .. Net IFF for Reporter (sum over Partner, sum over Year) ####
Net_Orig_Sum <- Net_Orig_Year %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()
save(Net_Orig_Sum, file = "Results/Summary data-sets/Net_Orig_Sum.Rdata")
write.csv(Net_Orig_Sum, file = "Results/Summary data-sets/Net_Orig_Sum.csv",
          row.names = F)

# Africa
Net_Orig_Sum_Africa <- Net_Orig_Sum %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)
save(Net_Orig_Sum_Africa, file = "Results/Summary data-sets/Net_Orig_Sum_Africa.Rdata")
write.csv(Net_Orig_Sum_Africa, file = "Results/Summary data-sets/Net_Orig_Sum_Africa.csv",
          row.names = F)


# .. Net IFF for Reporter-Partner (average across Year) ####
Net_Orig_Dest_Avg <- Net_Orig_Dest_Year %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI,
           partner, partner.ISO, pRegion, pIncome, pDev, pHDI) %>%
  summarize(Imp_IFF = mean(Imp_IFF, na.rm = T),
            Exp_IFF = mean(Exp_IFF, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF = Imp_IFF + Exp_IFF,
         Tot_IFF_bn = Tot_IFF / 10^9)
weights <- Net_Orig_Dest_Year %>%
  distinct(reporter.ISO, year, .keep_all = T) %>%
  group_by(reporter.ISO) %>%
  mutate(weight = 1/n()) %>%
  ungroup()
WDI <- left_join(WDI, weights %>% 
                   mutate(year = as.integer(year)) %>%
                   select(reporter.ISO, year, weight),
                 by = c("ISO3166.3" = "reporter.ISO",
                        "year")) %>%
  mutate(weight = ifelse(is.na(weight), 0, weight)) %>%
  group_by(ISO3166.3) %>%
  summarize(GDP = weighted.mean(GDP, weight),
            GNPpc = weighted.mean(GNPpc, weight))
Net_Orig_Dest_Avg <- left_join(Net_Orig_Dest_Avg,
                               WDI,
                               by = c("reporter.ISO" = "ISO3166.3")) %>%
  rename(rGDP = GDP,
         rGNPpc = GNPpc)
Net_Orig_Dest_Avg <- left_join(Net_Orig_Dest_Avg,
                               WDI,
                               by = c("partner.ISO" = "ISO3166.3")) %>%
  rename(pGDP = GDP,
         pGNPpc = GNPpc)
save(Net_Orig_Dest_Avg, file = "Results/Summary data-sets/Net_Orig_Dest_Avg.Rdata")
write.csv(Net_Orig_Dest_Avg, file = "Results/Summary data-sets/Net_Orig_Dest_Avg.csv",
          row.names = F)


# .. Net IFF for Reporter-Partner (sum over Year) ####
Net_Orig_Dest_Sum <- Net_Orig_Dest_Year %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI,
           partner, partner.ISO, pRegion, pIncome, pDev, pHDI) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF = Imp_IFF + Exp_IFF,
         Tot_IFF_bn = Tot_IFF / 10^9)

# Africa
Net_Orig_Dest_Sum_Africa <- Net_Orig_Dest_Sum %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)
save(Net_Orig_Dest_Sum_Africa, file = "Results/Summary data-sets/Net_Orig_Dest_Sum_Africa.Rdata")
write.csv(Net_Orig_Dest_Sum_Africa, file = "Results/Summary data-sets/Net_Orig_Dest_Sum_Africa.csv",
          row.names = F)


# .. Net IFF for Partner (sum over Reporter, sum over Year) ####
Net_Dest_Sum <- Net_Orig_Dest_Sum %>%
  group_by(partner, partner.ISO, pRegion, pIncome, pDev, pHDI) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()
save(Net_Dest_Sum, file = "Results/Summary data-sets/Net_Dest_Sum.Rdata")
write.csv(Net_Dest_Sum, file = "Results/Summary data-sets/Net_Dest_Sum.csv",
          row.names = F)

# Africa
Net_Dest_Sum_Africa <- Net_Orig_Dest_Sum_Africa %>%
  group_by(partner, partner.ISO, pRegion, pIncome, pDev, pHDI) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()
save(Net_Dest_Sum_Africa, file = "Results/Summary data-sets/Net_Dest_Sum_Africa.Rdata")
write.csv(Net_Dest_Sum_Africa, file = "Results/Summary data-sets/Net_Dest_Sum_Africa.csv",
          row.names = F)


# .. Total Net IFF per year ####
# World
Net_Year <- Net_Orig_Year %>%
  group_by(year) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T),
            GDP = sum(GDP, na.rm = T),
            Total_value = sum(Total_value, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF_GDP = Tot_IFF / GDP,
         Tot_IFF_trade = Tot_IFF / Total_value)
save(Net_Year, file = "Results/Summary data-sets/Net_Year.Rdata")
write.csv(Net_Year, file = "Results/Summary data-sets/Net_Year.csv",
          row.names = F)

# Africa
Net_Year_Africa <- Net_Orig_Year_Africa %>%
  group_by(year) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T),
            GDP = sum(GDP, na.rm = T),
            Total_value = sum(Total_value, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF_GDP = Tot_IFF / GDP,
         Tot_IFF_trade = Tot_IFF / Total_value)
save(Net_Year_Africa, file = "Results/Summary data-sets/Net_Year_Africa.Rdata")
write.csv(Net_Year_Africa, file = "Results/Summary data-sets/Net_Year_Africa.csv",
          row.names = F)

# Low and lower-middle income countries
Net_Year_LMIC <- Net_Orig_Year_LMIC %>%
  group_by(year) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T),
            GDP = sum(GDP, na.rm = T),
            Total_value = sum(Total_value, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF_GDP = Tot_IFF / GDP,
         Tot_IFF_trade = Tot_IFF / Total_value)
save(Net_Year_LMIC, file = "Results/Summary data-sets/Net_Year_LMIC.Rdata")
write.csv(Net_Year_LMIC, file = "Results/Summary data-sets/Net_Year_LMIC.csv",
          row.names = F)

# Developing countries
Net_Year_Developing <- Net_Orig_Year_Developing %>%
  group_by(year) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T),
            GDP = sum(GDP, na.rm = T),
            Total_value = sum(Total_value, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF_GDP = Tot_IFF / GDP,
         Tot_IFF_trade = Tot_IFF / Total_value)
save(Net_Year_Developing, file = "Results/Summary data-sets/Net_Year_Developing.Rdata")
write.csv(Net_Year_Developing, file = "Results/Summary data-sets/Net_Year_Developing.csv",
          row.names = F)

# Low-HDI countries
Net_Year_LowHDI <- Net_Orig_Year_LowHDI %>%
  group_by(year) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T),
            GDP = sum(GDP, na.rm = T),
            Total_value = sum(Total_value, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF_GDP = Tot_IFF / GDP,
         Tot_IFF_trade = Tot_IFF / Total_value)
save(Net_Year_LowHDI, file = "Results/Summary data-sets/Net_Year_LowHDI.Rdata")
write.csv(Net_Year_LowHDI, file = "Results/Summary data-sets/Net_Year_LowHDI.csv",
          row.names = F)


# .. Headline: cumulative Net IFF during 2000-2018 ####
# Africa
Net_Africa <- Net_Year_Africa %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T))
save(Net_Africa, file = "Results/Summary data-sets/Net_Africa.Rdata")
write.csv(Net_Africa, file = "Results/Summary data-sets/Net_Africa.csv",
          row.names = F)

# Low and lower-middle income countries
Net_LMIC <- Net_Year_LMIC %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T))
save(Net_LMIC, file = "Results/Summary data-sets/Net_LMIC.Rdata")
write.csv(Net_LMIC, file = "Results/Summary data-sets/Net_LMIC.csv",
          row.names = F)

# Developing countries
Net_Developing <- Net_Year_Developing %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T))
save(Net_Developing, file = "Results/Summary data-sets/Net_Developing.Rdata")
write.csv(Net_Developing, file = "Results/Summary data-sets/Net_Developing.csv",
          row.names = F)

# Low-HDI countries
Net_LowHDI <- Net_Year_LowHDI %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T))
save(Net_LowHDI, file = "Results/Summary data-sets/Net_LowHDI.Rdata")
write.csv(Net_LowHDI, file = "Results/Summary data-sets/Net_LowHDI.csv",
          row.names = F)



## ## ## ## ## ## ## ## ## ## ##
# GER OUT BY SECTOR         ####
## ## ## ## ## ## ## ## ## ## ##

load(paste0(data.disk, "Data/WDI/WDI.Rdata"))
load(paste0(data.disk, "Data/UN Stats/HS.Rdata"))
panel <- left_join(panel, HS %>% select(chapter, chapter.description),
                   by = c("commodity.code" = "chapter")) %>%
  rename(commodity = chapter.description)


# .. Aggregate outflows using Gross Excluding Reversals ####
GER_Imp_Sect_SITC <- panel %>%
  filter(Imp_IFF > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI,
           year, 
           SITC.code, SITC.section) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T)) %>%
  ungroup()

GER_Imp_Sect <- panel %>%
  filter(Imp_IFF > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI,
           year, 
           section.code, section) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T)) %>%
  ungroup()

GER_Imp_Sect_disag <- panel %>%
  filter(Imp_IFF > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI,
           year, 
           commodity.code, commodity) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T)) %>%
  ungroup()

GER_Exp_Sect_SITC <- panel %>%
  filter(Exp_IFF > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI,
           year, 
           SITC.code, SITC.section) %>%
  summarize(Exp_IFF = sum(Exp_IFF, na.rm = T)) %>%
  ungroup()

GER_Exp_Sect <- panel %>%
  filter(Exp_IFF > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI,
           year, 
           section.code, section) %>%
  summarize(Exp_IFF = sum(Exp_IFF, na.rm = T)) %>%
  ungroup()

GER_Exp_Sect_disag <- panel %>%
  filter(Exp_IFF > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI,
           year, 
           commodity.code, commodity) %>%
  summarize(Exp_IFF = sum(Exp_IFF, na.rm = T)) %>%
  ungroup()


# .. GER Import/Export IFF for Reporter-Sector-Year ####
# SITC sectors
GER_Orig_Sect_Year_SITC <- full_join(GER_Imp_Sect_SITC, GER_Exp_Sect_SITC,
                                     by = c("reporter" = "reporter",
                                            "reporter.ISO" = "reporter.ISO",
                                            "rRegion" = "rRegion",
                                            "rIncome" = "rIncome",
                                            "rDev" = "rDev",
                                            "rHDI" = "rHDI",
                                            "year" = "year",
                                            "SITC.code" = "SITC.code",
                                            "SITC.section" = "SITC.section"))
save(GER_Orig_Sect_Year_SITC, file = "Results/Summary data-sets/GER_Orig_Sect_Year_SITC.Rdata")
write.csv(GER_Orig_Sect_Year_SITC, file = "Results/Summary data-sets/GER_Orig_Sect_Year_SITC.csv",
          row.names = F)
rm(GER_Imp_Sect_SITC, GER_Exp_Sect_SITC)

# HS sections
GER_Orig_Sect_Year <- full_join(GER_Imp_Sect, GER_Exp_Sect,
                                by = c("reporter" = "reporter",
                                       "reporter.ISO" = "reporter.ISO",
                                       "rRegion" = "rRegion",
                                       "rIncome" = "rIncome",
                                       "rDev" = "rDev",
                                       "rHDI" = "rHDI",
                                       "year" = "year",
                                       "section.code" = "section.code",
                                       "section" = "section"))
save(GER_Orig_Sect_Year, file = "Results/Summary data-sets/GER_Orig_Sect_Year.Rdata")
write.csv(GER_Orig_Sect_Year, file = "Results/Summary data-sets/GER_Orig_Sect_Year.csv",
          row.names = F)
rm(GER_Imp_Sect, GER_Exp_Sect)

# Africa
GER_Orig_Sect_Year_Africa <- GER_Orig_Sect_Year %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)
save(GER_Orig_Sect_Year_Africa, file = "Results/Summary data-sets/GER_Orig_Sect_Year_Africa.Rdata")
write.csv(GER_Orig_Sect_Year_Africa, file = "Results/Summary data-sets/GER_Orig_Sect_Year_Africa.csv",
          row.names = F)

# HS chapters
GER_Orig_Sect_Year_disag <- full_join(GER_Imp_Sect_disag, GER_Exp_Sect_disag,
                                      by = c("reporter" = "reporter",
                                             "reporter.ISO" = "reporter.ISO",
                                             "rRegion" = "rRegion",
                                             "rIncome" = "rIncome",
                                             "rDev" = "rDev",
                                             "rHDI" = "rHDI",
                                             "year" = "year",
                                             "commodity.code" = "commodity.code",
                                             "commodity" = "commodity"))
save(GER_Orig_Sect_Year_disag, file = "Results/Summary data-sets/GER_Orig_Sect_Year_disag.Rdata")
write.csv(GER_Orig_Sect_Year_disag, file = "Results/Summary data-sets/GER_Orig_Sect_Year_disag.csv",
          row.names = F)
rm(GER_Imp_Sect_disag, GER_Exp_Sect_disag)


# .. GER IFF for Reporter-Sector (average across Year) ####
# SITC sectors
GER_Orig_Sect_Avg_SITC <- GER_Orig_Sect_Year_SITC %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI, SITC.code, SITC.section) %>%
  summarize(Imp_IFF = mean(Imp_IFF, na.rm = T),
            Exp_IFF = mean(Exp_IFF, na.rm = T)) %>%
  ungroup() %>%
  mutate_at(c("Imp_IFF", "Exp_IFF"), ~replace(., is.nan(.), 0)) %>%
  mutate(Tot_IFF = Imp_IFF + Exp_IFF,
         Tot_IFF_bn = Tot_IFF / 10^9)
save(GER_Orig_Sect_Avg_SITC, file = "Results/Summary data-sets/GER_Orig_Sect_Avg_SITC.Rdata")
write.csv(GER_Orig_Sect_Avg_SITC, file = "Results/Summary data-sets/GER_Orig_Sect_Avg_SITC.csv",
          row.names = F)

# HS sections
GER_Orig_Sect_Avg <- GER_Orig_Sect_Year %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI, section.code, section) %>%
  summarize(Imp_IFF = mean(Imp_IFF, na.rm = T),
            Exp_IFF = mean(Exp_IFF, na.rm = T)) %>%
  ungroup() %>%
  mutate_at(c("Imp_IFF", "Exp_IFF"), ~replace(., is.nan(.), 0)) %>%
  mutate(Tot_IFF = Imp_IFF + Exp_IFF,
         Tot_IFF_bn = Tot_IFF / 10^9)
weights <- GER_Orig_Sect_Year %>%
  distinct(reporter.ISO, year, .keep_all = T) %>%
  group_by(reporter.ISO) %>%
  mutate(weight = 1/n()) %>%
  ungroup()
WDI <- left_join(WDI, weights %>% 
                   mutate(year = as.integer(year)) %>%
                   select(reporter.ISO, year, weight),
                 by = c("ISO3166.3" = "reporter.ISO",
                        "year")) %>%
  mutate(weight = ifelse(is.na(weight), 0, weight)) %>%
  group_by(ISO3166.3) %>%
  summarize(GDP = weighted.mean(GDP, weight),
            GNPpc = weighted.mean(GNPpc, weight))
GER_Orig_Sect_Avg <- left_join(GER_Orig_Sect_Avg,
                               WDI,
                               by = c("reporter.ISO" = "ISO3166.3"))
save(GER_Orig_Sect_Avg, file = "Results/Summary data-sets/GER_Orig_Sect_Avg.Rdata")
write.csv(GER_Orig_Sect_Avg, file = "Results/Summary data-sets/GER_Orig_Sect_Avg.csv",
          row.names = F)

# Africa
GER_Orig_Sect_Avg_Africa <- GER_Orig_Sect_Avg %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)
save(GER_Orig_Sect_Avg_Africa, file = "Results/Summary data-sets/GER_Orig_Sect_Avg_Africa.Rdata")
write.csv(GER_Orig_Sect_Avg_Africa, file = "Results/Summary data-sets/GER_Orig_Sect_Avg_Africa.csv",
          row.names = F)

# Low and lower-middle income countries
GER_Orig_Sect_Avg_LMIC <- GER_Orig_Sect_Avg %>%
  filter(rIncome == "LIC" | rIncome == "LMC") %>%
  select(-rIncome)

# Developing countries
GER_Orig_Sect_Avg_Developing <- GER_Orig_Sect_Avg %>%
  filter(rDev == "Developing") %>%
  select(-rDev)

# Low-HDI countries
GER_Orig_Sect_Avg_LowHDI <- GER_Orig_Sect_Avg %>%
  filter(rHDI == "Lo HDI" | rHDI == "Med HDI") %>%
  select(-rHDI)

# HS chapters
GER_Orig_Sect_Avg_disag <- GER_Orig_Sect_Year_disag %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI, commodity.code, commodity) %>%
  summarize(Imp_IFF = mean(Imp_IFF, na.rm = T),
            Exp_IFF = mean(Exp_IFF, na.rm = T)) %>%
  ungroup() %>%
  mutate_at(c("Imp_IFF", "Exp_IFF"), ~replace(., is.nan(.), 0)) %>%
  mutate(Tot_IFF = Imp_IFF + Exp_IFF,
         Tot_IFF_bn = Tot_IFF / 10^9)
save(GER_Orig_Sect_Avg_disag, file = "Results/Summary data-sets/GER_Orig_Sect_Avg_disag.Rdata")
write.csv(GER_Orig_Sect_Avg_disag, file = "Results/Summary data-sets/GER_Orig_Sect_Avg_disag.csv",
          row.names = F)

# Africa (HS chapters)
GER_Orig_Sect_Avg_Africa_disag <- GER_Orig_Sect_Avg_disag %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

# Low and lower-middle income countries (HS chapters)
GER_Orig_Sect_Avg_LMIC_disag <- GER_Orig_Sect_Avg_disag %>%
  filter(rIncome == "LIC" | rIncome == "LMC") %>%
  select(-rIncome)

# Developing countries (HS chapters)
GER_Orig_Sect_Avg_Developing_disag <- GER_Orig_Sect_Avg_disag %>%
  filter(rDev == "Developing") %>%
  select(-rDev)

# Low-HDI countries (HS chapters)
GER_Orig_Sect_Avg_LowHDI_disag <- GER_Orig_Sect_Avg_disag %>%
  filter(rHDI == "Lo HDI" | rHDI == "Med HDI") %>%
  select(-rHDI)


# .. GER IFF for Reporter-Sector (average across recent Years) ####
# SITC sectors
GER_Orig_Sect_Avg_last3_SITC <- GER_Orig_Sect_Year_SITC %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI, SITC.code, SITC.section) %>%
  top_n(3, year) %>%
  summarize(Imp_IFF = mean(Imp_IFF, na.rm = T),
            Exp_IFF = mean(Exp_IFF, na.rm = T)) %>%
  ungroup() %>%
  mutate_at(c("Imp_IFF", "Exp_IFF"), ~replace(., is.nan(.), 0)) %>%
  mutate(Tot_IFF = Imp_IFF + Exp_IFF,
         Tot_IFF_bn = Tot_IFF / 10^9)
save(GER_Orig_Sect_Avg_last3_SITC, file = "Results/Summary data-sets/GER_Orig_Sect_Avg_last3_SITC.Rdata")
write.csv(GER_Orig_Sect_Avg_last3_SITC, file = "Results/Summary data-sets/GER_Orig_Sect_Avg_last3_SITC.csv",
          row.names = F)

# HS sections
GER_Orig_Sect_Avg_last3 <- GER_Orig_Sect_Year %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI, section.code, section) %>%
  top_n(3, year) %>%
  summarize(Imp_IFF = mean(Imp_IFF, na.rm = T),
            Exp_IFF = mean(Exp_IFF, na.rm = T)) %>%
  ungroup() %>%
  mutate_at(c("Imp_IFF", "Exp_IFF"), ~replace(., is.nan(.), 0)) %>%
  mutate(Tot_IFF = Imp_IFF + Exp_IFF,
         Tot_IFF_bn = Tot_IFF / 10^9)
save(GER_Orig_Sect_Avg_last3, file = "Results/Summary data-sets/GER_Orig_Sect_Avg_last3.Rdata")
write.csv(GER_Orig_Sect_Avg_last3, file = "Results/Summary data-sets/GER_Orig_Sect_Avg_last3.csv",
          row.names = F)


# .. GER IFF for Reporter-Sector (sum over Year) ####
GER_Orig_Sect_Sum <- GER_Orig_Sect_Year %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI, section.code, section) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF = Imp_IFF + Exp_IFF,
         Tot_IFF_bn = Tot_IFF / 10^9)
save(GER_Orig_Sect_Sum, file = "Results/Summary data-sets/GER_Orig_Sect_Sum.Rdata")
write.csv(GER_Orig_Sect_Sum, file = "Results/Summary data-sets/GER_Orig_Sect_Sum.csv",
          row.names = F)

# Africa
GER_Orig_Sect_Sum_Africa <- GER_Orig_Sect_Sum %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)
save(GER_Orig_Sect_Sum_Africa, file = "Results/Summary data-sets/GER_Orig_Sect_Sum_Africa.Rdata")
write.csv(GER_Orig_Sect_Sum_Africa, file = "Results/Summary data-sets/GER_Orig_Sect_Sum_Africa.csv",
          row.names = F)

# Low and lower-middle income countries
GER_Orig_Sect_Sum_LMIC <- GER_Orig_Sect_Sum %>%
  filter(rIncome == "LIC" | rIncome == "LMC") %>%
  select(-rIncome)

# Developing countries
GER_Orig_Sect_Sum_Developing <- GER_Orig_Sect_Sum %>%
  filter(rDev == "Developing") %>%
  select(-rDev)

# Low-HDI countries
GER_Orig_Sect_Sum_LowHDI <- GER_Orig_Sect_Sum %>%
  filter(rHDI == "Lo HDI" | rHDI == "Med HDI") %>%
  select(-rHDI)


# .. GER IFF for Sector-Year (sum over Reporter) ####
GER_Sect_Year <- GER_Orig_Sect_Year %>%
  group_by(section.code, section, year) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF = Imp_IFF + Exp_IFF,
         Tot_IFF_bn = Tot_IFF / 10^9)


# .. GER IFF for Sector (sum over Reporter, average across Year) ####
# SITC sectors
GER_Sect_Avg_SITC <- GER_Orig_Sect_Avg_SITC %>%
  group_by(SITC.code, SITC.section) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()
save(GER_Sect_Avg_SITC, file = "Results/Summary data-sets/GER_Sect_Avg_SITC.Rdata")
write.csv(GER_Sect_Avg_SITC, file = "Results/Summary data-sets/GER_Sect_Avg_SITC.csv",
          row.names = F)

# HS sections
GER_Sect_Avg <- GER_Orig_Sect_Avg %>%
  group_by(section.code, section) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()
save(GER_Sect_Avg, file = "Results/Summary data-sets/GER_Sect_Avg.Rdata")
write.csv(GER_Sect_Avg, file = "Results/Summary data-sets/GER_Sect_Avg.csv",
          row.names = F)

# Africa
GER_Sect_Avg_Africa <- GER_Orig_Sect_Avg_Africa %>%
  group_by(section.code, section) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()
save(GER_Sect_Avg_Africa, file = "Results/Summary data-sets/GER_Sect_Avg_Africa.Rdata")
write.csv(GER_Sect_Avg_Africa, file = "Results/Summary data-sets/GER_Sect_Avg_Africa.csv",
          row.names = F)

# HS chapters
GER_Sect_Avg_disag <- GER_Orig_Sect_Avg_disag %>%
  group_by(commodity.code, commodity) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()
save(GER_Sect_Avg_disag, file = "Results/Summary data-sets/GER_Sect_Avg_disag.Rdata")
write.csv(GER_Sect_Avg_disag, file = "Results/Summary data-sets/GER_Sect_Avg_disag.csv",
          row.names = F)

# Africa (HS chapters)
GER_Sect_Avg_Africa_disag <- GER_Orig_Sect_Avg_Africa_disag %>%
  group_by(commodity.code, commodity) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()
save(GER_Sect_Avg_Africa_disag, file = "Results/Summary data-sets/GER_Sect_Avg_Africa_disag.Rdata")
write.csv(GER_Sect_Avg_Africa_disag, file = "Results/Summary data-sets/GER_Sect_Avg_Africa_disag.csv",
          row.names = F)

# Low and lower-middle income countries
GER_Sect_Avg_LMIC <- GER_Orig_Sect_Avg_LMIC %>%
  group_by(section.code, section) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()
save(GER_Sect_Avg_LMIC, file = "Results/Summary data-sets/GER_Sect_Avg_LMIC.Rdata")
write.csv(GER_Sect_Avg_LMIC, file = "Results/Summary data-sets/GER_Sect_Avg_LMIC.csv",
          row.names = F)

# Low and lower-middle income countries (HS chapters)
GER_Sect_Avg_LMIC_disag <- GER_Orig_Sect_Avg_LMIC_disag %>%
  group_by(commodity.code, commodity) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()
save(GER_Sect_Avg_LMIC_disag, file = "Results/Summary data-sets/GER_Sect_Avg_LMIC_disag.Rdata")
write.csv(GER_Sect_Avg_LMIC_disag, file = "Results/Summary data-sets/GER_Sect_Avg_LMIC_disag.csv",
          row.names = F)

# Developing countries
GER_Sect_Avg_Developing <- GER_Orig_Sect_Avg_Developing %>%
  group_by(section.code, section) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()
save(GER_Sect_Avg_Developing, file = "Results/Summary data-sets/GER_Sect_Avg_Developing.Rdata")
write.csv(GER_Sect_Avg_Developing, file = "Results/Summary data-sets/GER_Sect_Avg_Developing.csv",
          row.names = F)

# Developing countries (HS chapters)
GER_Sect_Avg_Developing_disag <- GER_Orig_Sect_Avg_Developing_disag %>%
  group_by(commodity.code, commodity) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()
save(GER_Sect_Avg_Developing_disag, file = "Results/Summary data-sets/GER_Sect_Avg_Developing_disag.Rdata")
write.csv(GER_Sect_Avg_Developing_disag, file = "Results/Summary data-sets/GER_Sect_Avg_Developing_disag.csv",
          row.names = F)

# Low-HDI countries
GER_Sect_Avg_LowHDI <- GER_Orig_Sect_Avg_LowHDI %>%
  group_by(section.code, section) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()
save(GER_Sect_Avg_LowHDI, file = "Results/Summary data-sets/GER_Sect_Avg_LowHDI.Rdata")
write.csv(GER_Sect_Avg_LowHDI, file = "Results/Summary data-sets/GER_Sect_Avg_LowHDI.csv",
          row.names = F)

# Low-HDI countries (HS chapters)
GER_Sect_Avg_LowHDI_disag <- GER_Orig_Sect_Avg_LowHDI_disag %>%
  group_by(commodity.code, commodity) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()
save(GER_Sect_Avg_LowHDI_disag, file = "Results/Summary data-sets/GER_Sect_Avg_LowHDI_disag.Rdata")
write.csv(GER_Sect_Avg_LowHDI_disag, file = "Results/Summary data-sets/GER_Sect_Avg_LowHDI_disag.csv",
          row.names = F)


# .. GER IFF for Sector (sum over Reporter, average across recent Years) ####
# HS sections, last 3 years
GER_Sect_Avg_last3 <- GER_Orig_Sect_Avg_last3 %>%
  group_by(section.code, section) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()
save(GER_Sect_Avg_last3, file = "Results/Summary data-sets/GER_Sect_Avg_last3.Rdata")
write.csv(GER_Sect_Avg_last3, file = "Results/Summary data-sets/GER_Sect_Avg_last3.csv",
          row.names = F)


# .. GER IFF for Sector (sum over Reporter, sum over Year) ####
# Africa
GER_Sect_Sum_Africa <- GER_Orig_Sect_Sum_Africa %>%
  group_by(section.code, section) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()
save(GER_Sect_Sum_Africa, file = "Results/Summary data-sets/GER_Sect_Sum_Africa.Rdata")
write.csv(GER_Sect_Sum_Africa, file = "Results/Summary data-sets/GER_Sect_Sum_Africa.csv",
          row.names = F)

# Low and lower-middle income countries
GER_Sect_Sum_LMIC <- GER_Orig_Sect_Sum_LMIC %>%
  group_by(section.code, section) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()
save(GER_Sect_Sum_LMIC, file = "Results/Summary data-sets/GER_Sect_Sum_LMIC.Rdata")
write.csv(GER_Sect_Sum_LMIC, file = "Results/Summary data-sets/GER_Sect_Sum_LMIC.csv",
          row.names = F)

# Developing countries
GER_Sect_Sum_Developing <- GER_Orig_Sect_Sum_Developing %>%
  group_by(section.code, section) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()
save(GER_Sect_Sum_Developing, file = "Results/Summary data-sets/GER_Sect_Sum_Developing.Rdata")
write.csv(GER_Sect_Sum_Developing, file = "Results/Summary data-sets/GER_Sect_Sum_Developing.csv",
          row.names = F)

# Low-HDI countries
GER_Sect_Sum_LowHDI <- GER_Orig_Sect_Sum_LowHDI %>%
  group_by(section.code, section) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()
save(GER_Sect_Sum_LowHDI, file = "Results/Summary data-sets/GER_Sect_Sum_LowHDI.Rdata")
write.csv(GER_Sect_Sum_LowHDI, file = "Results/Summary data-sets/GER_Sect_Sum_LowHDI.csv",
          row.names = F)



## ## ## ## ## ## ## ## ## ## ##
# GER OUT FOR TOP SECTORS   ####
## ## ## ## ## ## ## ## ## ## ##

load("Results/panel_results.Rdata")
load(paste0(data.disk, "Data/UN Stats/HS.Rdata"))
panel <- left_join(panel, HS %>% select(chapter, chapter.description),
                   by = c("commodity.code" = "chapter")) %>%
  rename(commodity = chapter.description)

load(paste0(data.disk, "Data/WDI/WDI.Rdata"))
load(paste0(data.disk, "Data/Comtrade/comtrade_total_clean.Rdata"))
load(paste0(data.disk, "Data/UN Stats/HS.Rdata"))
load("Results/Summary data-sets/GER_Sect_Avg_disag.Rdata")

GER_Sect_Avg_disag <- left_join(GER_Sect_Avg_disag,
                                HS %>% select(chapter, nice.label),
                                by = c("commodity.code" = "chapter"))

top_sectors <- list(code = GER_Sect_Avg_disag %>%
                      slice_max(Tot_IFF_bn, n = 10) %>%
                      pull(commodity.code),
                    chapter = GER_Sect_Avg_disag %>%
                      slice_max(Tot_IFF_bn, n = 10) %>%
                      pull(nice.label))
save(top_sectors, file = "Results/top_sectors.Rdata")


# .. Aggregate outflows using Gross Excluding Reversals ####
GER_Imp_Dest_TopSect <- panel %>%
  filter(commodity.code %in% top_sectors$code) %>%
  filter(Imp_IFF > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI,
           year, commodity.code, commodity,
           partner, partner.ISO, pRegion, pIncome, pDev, pHDI) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T)) %>%
  ungroup()

GER_Exp_Dest_TopSect <- panel %>%
  filter(commodity.code %in% top_sectors$code) %>%
  filter(Exp_IFF > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI,
           year, commodity.code, commodity,
           partner, partner.ISO, pRegion, pIncome, pDev, pHDI) %>%
  summarize(Exp_IFF = sum(Exp_IFF, na.rm = T)) %>%
  ungroup()


# .. GER Import/Export IFF for Reporter-Partner-TopSector-Year ####
GER_Orig_Dest_TopSect_Year <- full_join(GER_Imp_Dest_TopSect, GER_Exp_Dest_TopSect,
                                        by = c("reporter" = "reporter",
                                               "reporter.ISO" = "reporter.ISO",
                                               "rRegion" = "rRegion",
                                               "rIncome" = "rIncome",
                                               "rDev" = "rDev",
                                               "rHDI" = "rHDI",
                                               "year" = "year",
                                               "partner" = "partner",
                                               "partner.ISO" = "partner.ISO",
                                               "pRegion" = "pRegion",
                                               "pIncome" = "pIncome",
                                               "pDev" = "pDev",
                                               "pHDI" = "pHDI",
                                               "commodity.code" = "commodity.code",
                                               "commodity" = "commodity"))


# .. GER IFF for Reporter-TopSector-Year (sum over Partner) ####
GER_Orig_TopSect_Year <- GER_Orig_Dest_TopSect_Year %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI, year,
           commodity.code, commodity) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF = Imp_IFF + Exp_IFF,
         Tot_IFF_bn = Tot_IFF / 10^9)
GER_Orig_TopSect_Year <- left_join(GER_Orig_TopSect_Year %>% 
                                     mutate(year = as.integer(year)),
                                   WDI,
                                   by = c("reporter.ISO" = "ISO3166.3", 
                                          "year")) %>%
  mutate(Tot_IFF_GDP = Tot_IFF / GDP)
GER_Orig_TopSect_Year <- left_join(GER_Orig_TopSect_Year,
                                   comtrade_total,
                                   by = c("reporter.ISO", "year")) %>%
  mutate(Tot_IFF_trade = Tot_IFF / Total_value)


# .. GER IFF for Reporter-TopSector (sum over Partner, average across Year) ####
GER_Orig_TopSect_Avg <- GER_Orig_TopSect_Year %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI,
           commodity.code, commodity) %>%
  summarize(Imp_IFF = mean(Imp_IFF, na.rm = T),
            Exp_IFF = mean(Exp_IFF, na.rm = T),
            Tot_IFF = mean(Tot_IFF, na.rm = T),
            Tot_IFF_bn = mean(Tot_IFF_bn, na.rm = T),
            Tot_IFF_GDP = mean(Tot_IFF_GDP, na.rm = T),
            Tot_IFF_trade = mean(Tot_IFF_trade, na.rm = T)) %>%
  ungroup()
save(GER_Orig_TopSect_Avg, file = "Results/Summary data-sets/GER_Orig_TopSect_Avg.Rdata")
write.csv(GER_Orig_TopSect_Avg, file = "Results/Summary data-sets/GER_Orig_TopSect_Avg.csv",
          row.names = F)


# .. Top origins in top sectors ####
top_origins_sect <- list(code = NULL,
                         chapter = NULL,
                         dollar.orig = NULL,
                         GDP.orig = NULL,
                         trade.orig = NULL)

for (i in 1:length(top_sectors$code)){
  top_origins_sect$code[[i]] <- top_sectors$code[i]
  top_origins_sect$chapter[[i]] <- top_sectors$chapter[i]
  top_origins_sect$dollar.orig[[i]] <- GER_Orig_TopSect_Avg %>%
    filter(commodity.code == top_sectors$code[i]) %>%
    slice_max(Tot_IFF_bn, n = 5) %>%
    pull(reporter.ISO)
  top_origins_sect$GDP.orig[[i]] <- GER_Orig_TopSect_Avg %>%
    filter(commodity.code == top_sectors$code[i]) %>%
    slice_max(Tot_IFF_GDP, n = 5) %>%
    pull(reporter.ISO)
  top_origins_sect$trade.orig[[i]] <- GER_Orig_TopSect_Avg %>%
    filter(commodity.code == top_sectors$code[i]) %>%
    slice_max(Tot_IFF_trade, n = 5) %>%
    pull(reporter.ISO)
}
save(top_origins_sect, file = "Results/top_origins_sect.Rdata")

# .. GER IFF for Reporter-Partner in top Sectors (average across Year) ####
GER_Orig_Dest_TopSect_Avg <- GER_Orig_Dest_TopSect_Year %>%
  filter(commodity.code %in% top_sectors$code) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI, 
           partner, partner.ISO, pRegion, pIncome, pDev, pHDI,
           commodity.code, commodity) %>%
  summarize(Imp_IFF = mean(Imp_IFF, na.rm = T),
            Exp_IFF = mean(Exp_IFF, na.rm = T)) %>%
  ungroup() %>%
  mutate_at(c("Imp_IFF", "Exp_IFF"), ~replace(., is.nan(.), 0)) %>%
  mutate(Tot_IFF = Imp_IFF + Exp_IFF,
         Tot_IFF_bn = Tot_IFF / 10^9)
save(GER_Orig_Dest_TopSect_Avg, file = "Results/Summary data-sets/GER_Orig_Dest_TopSect_Avg.Rdata")
write.csv(GER_Orig_Dest_TopSect_Avg, file = "Results/Summary data-sets/GER_Orig_Dest_TopSect_Avg.csv",
          row.names = F)



## ## ## ## ## ## ## ## ## ## ##
# NET BY SECTOR             ####
## ## ## ## ## ## ## ## ## ## ##

# .. Net Import/Export IFF for Reporter-Sector-Year ####
Net_Orig_Sect_Year <- panel %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rHDI,
           year, section.code, section) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T)) %>%
  ungroup()
save(Net_Orig_Sect_Year, file = "Results/Summary data-sets/Net_Orig_Sect_Year.Rdata")
write.csv(Net_Orig_Sect_Year, file = "Results/Summary data-sets/Net_Orig_Sect_Year.csv",
          row.names = F)

# Africa
Net_Orig_Sect_Year_Africa <- Net_Orig_Sect_Year %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)
save(Net_Orig_Sect_Year_Africa, file = "Results/Summary data-sets/Net_Orig_Sect_Year_Africa.Rdata")
write.csv(GER_Orig_Sect_Year_Africa, file = "Results/Summary data-sets/Net_Orig_Sect_Year_Africa.csv",
          row.names = F)


# .. Net IFF for Reporter-Sector (average across Year) ####
Net_Orig_Sect_Avg <- Net_Orig_Sect_Year %>%
  group_by(reporter, reporter.ISO, rRegion, section.code, section) %>%
  summarize(Imp_IFF = mean(Imp_IFF, na.rm = T),
            Exp_IFF = mean(Exp_IFF, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF = Imp_IFF + Exp_IFF,
         Tot_IFF_bn = Tot_IFF / 10^9)

# Africa
Net_Orig_Sect_Avg_Africa <- Net_Orig_Sect_Avg %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)
save(Net_Orig_Sect_Avg_Africa, file = "Results/Summary data-sets/Net_Orig_Sect_Avg_Africa.Rdata")
write.csv(Net_Orig_Sect_Avg_Africa, file = "Results/Summary data-sets/Net_Orig_Sect_Avg_Africa.csv",
          row.names = F)


# .. Net IFF for Reporter-Sector (sum over Year) ####
Net_Orig_Sect_Sum <- Net_Orig_Sect_Year %>%
  group_by(reporter, reporter.ISO, rRegion, section.code, section) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF = Imp_IFF + Exp_IFF,
         Tot_IFF_bn = Tot_IFF / 10^9)

# Africa
Net_Orig_Sect_Sum_Africa <- Net_Orig_Sect_Sum %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)
save(Net_Orig_Sect_Sum_Africa, file = "Results/Summary data-sets/Net_Orig_Sect_Sum_Africa.Rdata")
write.csv(Net_Orig_Sect_Sum_Africa, file = "Results/Summary data-sets/Net_Orig_Sect_Sum_Africa.csv",
          row.names = F)


# .. Net IFF for Sector (sum over Reporter, sum over Year) ####
Net_Sect_Sum_Africa <- Net_Orig_Sect_Sum_Africa %>%
  group_by(section.code, section) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()
save(Net_Sect_Sum_Africa, file = "Results/Summary data-sets/Net_Sect_Sum_Africa.Rdata")
write.csv(Net_Sect_Sum_Africa, file = "Results/Summary data-sets/Net_Sect_Sum_Africa.csv",
          row.names = F)



## ## ## ## ## ## ## ## ## ## ##
# HEADLINE RESULTS          ####
## ## ## ## ## ## ## ## ## ## ##

# .. For Africa ####
(Cumulative.gross <- sum(GER_Year_Africa$Tot_IFF_bn))
# 220.823

(Cumulative.gross.GDP <- Cumulative.gross / (sum(GER_Year_Africa$GDP) / 10^9)) * 100
# 1.018336

(Cumulative.gross.trade <- Cumulative.gross / (sum(GER_Year_Africa$Total_value) / 10^9)) * 100
# 2.216521

(Cumulative.net <- sum(Net_Year_Africa$Tot_IFF_bn))
# -63.73985

(Cumulative.net.GDP <- Cumulative.net / (sum(Net_Year_Africa$GDP) / 10^9)) * 100
# -0.2935652

(Gross.IFF.per.year <- sum(GER_Orig_Avg_Africa$Tot_IFF_bn))
# 15.62286

(Net.IFF.per.year <- sum(Net_Orig_Avg_Africa$Tot_IFF_bn))
# -4.81792


# .. For low and lower-middle income countries ####
(Cumulative.gross <- sum(GER_Year_LMIC$Tot_IFF_bn))
# 785.7972

(Cumulative.gross.GDP <- Cumulative.gross / (sum(GER_Year_LMIC$GDP) / 10^9)) * 100
# 1.440952

(Cumulative.gross.trade <- Cumulative.gross / (sum(GER_Year_LMIC$Total_value) / 10^9)) * 100
# 3.257313

(Cumulative.net <- sum(Net_Year_LMIC$Tot_IFF_bn))
# -280.7992

(Cumulative.net.GDP <- Cumulative.net / (sum(Net_Year_LMIC$GDP) / 10^9)) * 100
# -0.5144647

(Gross.IFF.per.year <- sum(GER_Orig_Avg_LMIC$Tot_IFF_bn))
# 50.02923

(Net.IFF.per.year <- sum(Net_Orig_Avg_LMIC$Tot_IFF_bn))
# -18.27376


# .. For developing countries ####
(Cumulative.gross <- sum(GER_Year_Developing$Tot_IFF_bn))
# 6659.458

(Cumulative.gross.GDP <- Cumulative.gross / (sum(GER_Year_Developing$GDP) / 10^9)) * 100
# 2.162262

(Cumulative.gross.trade <- Cumulative.gross / (sum(GER_Year_Developing$Total_value) / 10^9)) * 100
# 3.791425

(Cumulative.net <- sum(Net_Year_Developing$Tot_IFF_bn))
# -375.017

(Cumulative.net.GDP <- Cumulative.net / (sum(Net_Year_Developing$GDP) / 10^9)) * 100
# -0.1217456

(Gross.IFF.per.year <- sum(GER_Orig_Avg_Developing$Tot_IFF_bn))
# 387.812

(Net.IFF.per.year <- sum(Net_Orig_Avg_Developing$Tot_IFF_bn))
# -30.28294


# .. For low-HDI countries ####
(Cumulative.gross <- sum(GER_Year_LowHDI$Tot_IFF_bn))
# 578.1485

(Cumulative.gross.GDP <- Cumulative.gross / (sum(GER_Year_LowHDI$GDP) / 10^9)) * 100
# 1.384093

(Cumulative.gross.trade <- Cumulative.gross / (sum(GER_Year_LowHDI$Total_value) / 10^9)) * 100
# 3.746342

(Cumulative.net <- sum(Net_Year_LowHDI$Tot_IFF_bn))
# -125.8819

(Cumulative.net.GDP <- Cumulative.net / (sum(Net_Year_LowHDI$GDP) / 10^9)) * 100
# -0.3010193

(Gross.IFF.per.year <- sum(GER_Orig_Avg_LowHDI$Tot_IFF_bn))
# 36.82828

(Net.IFF.per.year <- sum(Net_Orig_Avg_LowHDI$Tot_IFF_bn))
# -8.846453



## ## ## ## ## ## ## ## ## ## ##
# REGULAR TRADE GAPS        ####
## ## ## ## ## ## ## ## ## ## ##

trade <- panel %>%
  select(reporter.ISO, partner.ISO, year, commodity.code,
         Import_value, pNetExport_value, pExport_value,
         Export_value, pImport_value) %>%
  group_by(reporter.ISO, partner.ISO, year) %>%
  summarize(Import_value = sum(Import_value, na.rm = T),
            pNetExport_value = sum(pNetExport_value, na.rm = T),
            pExport_value = sum(pExport_value, na.rm = T),
            Export_value = sum(Export_value, na.rm = T),
            pImport_value = sum(pImport_value, na.rm = T)) %>%
  ungroup %>%
  mutate(Import_value = Import_value*-1,
         pImport_value = pImport_value*-1) %>%
  mutate(GapM = Import_value + pNetExport_value,
         GapX = Export_value + pImport_value)

gaps <- trade %>%
  group_by(year) %>%
  summarize(GapM_bn = sum(GapM, na.rm = T)/10^9,
            GapX_bn = sum(GapX, na.rm = T)/10^9)

CIF <- trade %>%
  mutate(Import_value = Import_value*-1,
         pImport_value = pImport_value*-1) %>%
  mutate(GapM = Import_value - pNetExport_value,
         GapX = pImport_value - Export_value) %>%
  group_by(year) %>%
  summarize(GapM_bn = sum(GapM, na.rm = T)/10^9,
            GapX_bn = sum(GapX, na.rm = T)/10^9,
            CIF = GapM_bn / GapX_bn,
            Percentage = (GapM_bn - GapX_bn) / GapX_bn * 100)
