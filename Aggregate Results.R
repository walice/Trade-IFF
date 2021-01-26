# Aggregate Results
# Alice Lepissier
# alice.lepissier@gmail.com
# Prepared for UNECA

## ## ## ## ## ## ## ## ## ## ##
# INDEX                     ####
## ## ## ## ## ## ## ## ## ## ##
# Preamble
# Aggregate by Destination
# .. Aggregate results using Gross Excluding Reversals
# .. Aggregate results using Net Aggregation
# Aggregate by Sector
# .. Aggregate results using Gross Excluding Reversals
# .. Aggregate results using Net Aggregation
# Headline Results
# .. For Africa
# .. For low and lower-middle income countries
# .. For developing countries
# .. For low HDI countries
# LMIC Results



## ## ## ## ## ## ## ## ## ## ##
# PREAMBLE                  ####
## ## ## ## ## ## ## ## ## ## ##

setwd("/home/alice/IFFe/") # Virtual server
library(car)
library(kableExtra)
library(reshape2)
library(scales)
library(stargazer)
library(tidyverse)
options(scipen = 999)



## ## ## ## ## ## ## ## ## ## ##
# AGGREGATE BY DESTINATION  ####
## ## ## ## ## ## ## ## ## ## ##

#source("Scripts/Compute IFF Estimates.R")
load("Results/panel_results.Rdata")

# .. Aggregate results using Gross Excluding Reversals ####
load(paste0(data.disk, "Data/WDI/WDI.Rdata"))
load(paste0(data.disk, "Data/Comtrade/comtrade_total_clean.Rdata"))

GER_Imp_Dest <- panel %>%
  filter(Imp_IFF > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI,
           year,
           partner, partner.ISO, pRegion, pIncome, pDev, pHDI) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T)) %>%
  ungroup()

GER_Exp_Dest <- panel %>%
  filter(pExp_IFF > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI,
           year,
           partner, partner.ISO, pRegion, pIncome, pDev, pHDI) %>%
  summarize(Exp_IFF = sum(pExp_IFF, na.rm = T)) %>%
  ungroup()

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
rm(GER_Imp_Dest, GER_Exp_Dest)

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

GER_Orig_Avg <- GER_Orig_Year %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI) %>%
  summarize(Imp_IFF = mean(Imp_IFF, na.rm = T),
            Exp_IFF = mean(Exp_IFF, na.rm = T),
            Tot_IFF = mean(Tot_IFF, na.rm = T),
            Tot_IFF_bn = mean(Tot_IFF_bn, na.rm = T),
            Tot_IFF_GDP = mean(Tot_IFF_GDP, na.rm = T),
            Tot_IFF_trade = mean(Tot_IFF_trade, na.rm = T)) %>%
  ungroup()

GER_Orig_Sum <- GER_Orig_Year %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()

GER_Orig_Dest_Avg <- GER_Orig_Dest_Year %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI, 
           partner, partner.ISO, pRegion, pIncome, pDev, pHDI) %>%
  summarize(Imp_IFF = mean(Imp_IFF, na.rm = T),
            Exp_IFF = mean(Exp_IFF, na.rm = T)) %>%
  ungroup() %>%
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

GER_Orig_Dest_Sum <- GER_Orig_Dest_Year %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI,
           partner, partner.ISO, pRegion, pIncome, pDev, pHDI) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF = Imp_IFF + Exp_IFF,
         Tot_IFF_bn = Tot_IFF / 10^9)

GER_Orig_Dest_Year_Africa <- GER_Orig_Dest_Year %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

GER_Orig_Dest_Avg_Africa <- GER_Orig_Dest_Avg %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

GER_Orig_Dest_Avg_LMIC <- GER_Orig_Dest_Avg %>%
  filter(rIncome == "LIC" | rIncome == "LMC") %>%
  select(-rIncome)

GER_Orig_Dest_Avg_Developing <- GER_Orig_Dest_Avg %>%
  filter(rDev == "Developing") %>%
  select(-rDev)

GER_Orig_Dest_Avg_LowHDI <- GER_Orig_Dest_Avg %>%
  filter(rHDI == "Lo HDI" | rHDI == "Med HDI") %>%
  select(-rHDI)

GER_Orig_Dest_Sum_Africa <- GER_Orig_Dest_Sum %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

GER_Dest <- GER_Orig_Dest_Sum %>%
  group_by(partner, partner.ISO, pRegion, pIncome, pDev, pHDI) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()

GER_Dest_Sum_Africa <- GER_Orig_Dest_Sum_Africa %>%
  group_by(partner, partner.ISO, pRegion, pIncome, pDev, pHDI) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()

GER_Dest_Avg_Africa <- GER_Orig_Dest_Avg_Africa %>%
  group_by(partner, partner.ISO, pRegion, pIncome, pDev, pHDI) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()

GER_Dest_Avg_LMIC <- GER_Orig_Dest_Avg_LMIC %>%
  group_by(partner, partner.ISO, pRegion, pIncome, pDev, pHDI) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()

GER_Dest_Avg_Developing <- GER_Orig_Dest_Avg_Developing %>%
  group_by(partner, partner.ISO, pRegion, pIncome, pDev, pHDI) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()

GER_Dest_Avg_LowHDI <- GER_Orig_Dest_Avg_LowHDI %>%
  group_by(partner, partner.ISO, pRegion, pIncome, pDev, pHDI) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()

GER_Orig_Year_Africa <- GER_Orig_Year %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

GER_Orig_Year_LMIC <- GER_Orig_Year %>%
  filter(rIncome == "LIC" | rIncome == "LMC") %>%
  select(-rIncome)

GER_Orig_Year_Developing <- GER_Orig_Year %>%
  filter(rDev == "Developing") %>%
  select(-rDev)

GER_Orig_Year_LowHDI <- GER_Orig_Year %>%
  filter(rHDI == "Lo HDI" | rHDI == "Med HDI") %>%
  select(-rHDI)

GER_Orig_Avg_Africa <- GER_Orig_Avg %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

GER_Orig_Avg_LMIC <- GER_Orig_Avg %>%
  filter(rIncome == "LIC" | rIncome == "LMC") %>%
  select(-rIncome)

GER_Orig_Avg_Developing <- GER_Orig_Avg %>%
  filter(rDev == "Developing") %>%
  select(-rDev)

GER_Orig_Avg_LowHDI <- GER_Orig_Avg %>%
  filter(rHDI == "Lo HDI" | rHDI == "Med HDI") %>%
  select(-rHDI)

GER_Orig_Sum_Africa <- GER_Orig_Sum %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

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

GER_Africa <- GER_Year_Africa %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T))

GER_LMIC <- GER_Year_LMIC %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T))

GER_Developing <- GER_Year_Developing %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T))

GER_LowHDI <- GER_Year_LowHDI %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T))

save(GER_Orig_Dest_Year, file = "Results/Summary data-sets/GER_Orig_Dest_Year.Rdata")
write.csv(GER_Orig_Dest_Year, file = "Results/Summary data-sets/GER_Orig_Dest_Year.csv",
          row.names = F)
save(GER_Orig_Dest_Year_std, file = "Results/Summary data-sets/GER_Orig_Dest_Year_std.Rdata")
write.csv(GER_Orig_Dest_Year_std, file = "Results/Summary data-sets/GER_Orig_Dest_Year_std.csv",
          row.names = F)

save(GER_Orig_Year, file = "Results/Summary data-sets/GER_Orig_Year.Rdata")
write.csv(GER_Orig_Year, file = "Results/Summary data-sets/GER_Orig_Year.csv",
          row.names = F)
save(GER_Orig_Avg, file = "Results/Summary data-sets/GER_Orig_Avg.Rdata")
write.csv(GER_Orig_Avg, file = "Results/Summary data-sets/GER_Orig_Avg.csv",
          row.names = F)
save(GER_Orig_Sum, file = "Results/Summary data-sets/GER_Orig_Sum.Rdata")
write.csv(GER_Orig_Sum, file = "Results/Summary data-sets/GER_Orig_Sum.csv",
          row.names = F)

save(GER_Orig_Dest_Year_Africa, file = "Results/Summary data-sets/GER_Orig_Dest_Year_Africa.Rdata")
write.csv(GER_Orig_Dest_Year_Africa, file = "Results/Summary data-sets/GER_Orig_Dest_Year_Africa.csv",
          row.names = F)

save(GER_Orig_Year_Africa, file = "Results/Summary data-sets/GER_Orig_Year_Africa.Rdata")
write.csv(GER_Orig_Year_Africa, file = "Results/Summary data-sets/GER_Orig_Year_Africa.csv",
          row.names = F)
save(GER_Orig_Year_LMIC, file = "Results/Summary data-sets/GER_Orig_Year_LMIC.Rdata")
write.csv(GER_Orig_Year_LMIC, file = "Results/Summary data-sets/GER_Orig_Year_LMIC.csv",
          row.names = F)
save(GER_Orig_Year_Developing, file = "Results/Summary data-sets/GER_Orig_Year_Developing.Rdata")
write.csv(GER_Orig_Year_Developing, file = "Results/Summary data-sets/GER_Orig_Year_Developing.csv",
          row.names = F)
save(GER_Orig_Year_LowHDI, file = "Results/Summary data-sets/GER_Orig_Year_LowHDI.Rdata")
write.csv(GER_Orig_Year_LowHDI, file = "Results/Summary data-sets/GER_Orig_Year_LowHDI.csv",
          row.names = F)

save(GER_Orig_Avg_Africa, file = "Results/Summary data-sets/GER_Orig_Avg_Africa.Rdata")
write.csv(GER_Orig_Avg_Africa, file = "Results/Summary data-sets/GER_Orig_Avg_Africa.csv",
          row.names = F)
save(GER_Orig_Avg_LMIC, file = "Results/Summary data-sets/GER_Orig_Avg_LMIC.Rdata")
write.csv(GER_Orig_Avg_LMIC, file = "Results/Summary data-sets/GER_Orig_Avg_LMIC.csv",
          row.names = F)
save(GER_Orig_Avg_Developing, file = "Results/Summary data-sets/GER_Orig_Avg_Developing.Rdata")
write.csv(GER_Orig_Avg_Developing, file = "Results/Summary data-sets/GER_Orig_Avg_Developing.csv",
          row.names = F)
save(GER_Orig_Avg_LowHDI, file = "Results/Summary data-sets/GER_Orig_Avg_LowHDI.Rdata")
write.csv(GER_Orig_Avg_LowHDI, file = "Results/Summary data-sets/GER_Orig_Avg_LowHDI.csv",
          row.names = F)

save(GER_Orig_Sum_Africa, file = "Results/Summary data-sets/GER_Orig_Sum_Africa.Rdata")
write.csv(GER_Orig_Sum_Africa, file = "Results/Summary data-sets/GER_Orig_Sum_Africa.csv",
          row.names = F)

save(GER_Orig_Dest_Avg, file = "Results/Summary data-sets/GER_Orig_Dest_Avg.Rdata")
write.csv(GER_Orig_Dest_Avg, file = "Results/Summary data-sets/GER_Orig_Dest_Avg.csv",
          row.names = F)
save(GER_Orig_Dest_Avg_Africa, file = "Results/Summary data-sets/GER_Orig_Dest_Avg_Africa.Rdata")
write.csv(GER_Orig_Dest_Avg_Africa, file = "Results/Summary data-sets/GER_Orig_Dest_Avg_Africa.csv",
          row.names = F)
save(GER_Orig_Dest_Avg_LMIC, file = "Results/Summary data-sets/GER_Orig_Dest_Avg_LMIC.Rdata")
write.csv(GER_Orig_Dest_Avg_LMIC, file = "Results/Summary data-sets/GER_Orig_Dest_Avg_LMIC.csv",
          row.names = F)
save(GER_Orig_Dest_Avg_Developing, file = "Results/Summary data-sets/GER_Orig_Dest_Avg_Developing.Rdata")
write.csv(GER_Orig_Dest_Avg_Developing, file = "Results/Summary data-sets/GER_Orig_Dest_Avg_Developing.csv",
          row.names = F)
save(GER_Orig_Dest_Avg_LowHDI, file = "Results/Summary data-sets/GER_Orig_Dest_Avg_LowHDI.Rdata")
write.csv(GER_Orig_Dest_Avg_LowHDI, file = "Results/Summary data-sets/GER_Orig_Dest_Avg_LowHDI.csv",
          row.names = F)

save(GER_Orig_Dest_Sum_Africa, file = "Results/Summary data-sets/GER_Orig_Dest_Sum_Africa.Rdata")
write.csv(GER_Orig_Dest_Sum_Africa, file = "Results/Summary data-sets/GER_Orig_Dest_Sum_Africa.csv",
          row.names = F)

save(GER_Dest, file = "Results/Summary data-sets/GER_Dest.Rdata")
write.csv(GER_Dest, file = "Results/Summary data-sets/GER_Dest.csv",
          row.names = F)
save(GER_Dest_Sum_Africa, file = "Results/Summary data-sets/GER_Dest_Sum_Africa.Rdata")
write.csv(GER_Dest_Sum_Africa, file = "Results/Summary data-sets/GER_Dest_Sum_Africa.csv",
          row.names = F)

save(GER_Dest_Avg_Africa, file = "Results/Summary data-sets/GER_Dest_Avg_Africa.Rdata")
write.csv(GER_Dest_Avg_Africa, file = "Results/Summary data-sets/GER_Dest_Avg_Africa.csv",
          row.names = F)
save(GER_Dest_Avg_LMIC, file = "Results/Summary data-sets/GER_Dest_Avg_LMIC.Rdata")
write.csv(GER_Dest_Avg_LMIC, file = "Results/Summary data-sets/GER_Dest_Avg_LMIC.csv",
          row.names = F)
save(GER_Dest_Avg_Developing, file = "Results/Summary data-sets/GER_Dest_Avg_Developing.Rdata")
write.csv(GER_Dest_Avg_Developing, file = "Results/Summary data-sets/GER_Dest_Avg_Developing.csv",
          row.names = F)
save(GER_Dest_Avg_LowHDI, file = "Results/Summary data-sets/GER_Dest_Avg_LowHDI.Rdata")
write.csv(GER_Dest_Avg_LowHDI, file = "Results/Summary data-sets/GER_Dest_Avg_LowHDI.csv",
          row.names = F)

save(GER_Year_Africa, file = "Results/Summary data-sets/GER_Year_Africa.Rdata")
write.csv(GER_Year_Africa, file = "Results/Summary data-sets/GER_Year_Africa.csv",
          row.names = F)
save(GER_Year_LMIC, file = "Results/Summary data-sets/GER_Year_LMIC.Rdata")
write.csv(GER_Year_LMIC, file = "Results/Summary data-sets/GER_Year_LMIC.csv",
          row.names = F)
save(GER_Year_Developing, file = "Results/Summary data-sets/GER_Year_Developing.Rdata")
write.csv(GER_Year_Developing, file = "Results/Summary data-sets/GER_Year_Developing.csv",
          row.names = F)
save(GER_Year_LowHDI, file = "Results/Summary data-sets/GER_Year_LowHDI.Rdata")
write.csv(GER_Year_LowHDI, file = "Results/Summary data-sets/GER_Year_LowHDI.csv",
          row.names = F)

save(GER_Africa, file = "Results/Summary data-sets/GER_Africa.Rdata")
write.csv(GER_Africa, file = "Results/Summary data-sets/GER_Africa.csv",
          row.names = F)
save(GER_LMIC, file = "Results/Summary data-sets/GER_LMIC.Rdata")
write.csv(GER_LMIC, file = "Results/Summary data-sets/GER_LMIC.csv",
          row.names = F)
save(GER_Developing, file = "Results/Summary data-sets/GER_Developing.Rdata")
write.csv(GER_Developing, file = "Results/Summary data-sets/GER_Developing.csv",
          row.names = F)
save(GER_LowHDI, file = "Results/Summary data-sets/GER_LowHDI.Rdata")
write.csv(GER_LowHDI, file = "Results/Summary data-sets/GER_LowHDI.csv",
          row.names = F)


# .. Aggregate results using Net Aggregation ####
load(paste0(data.disk, "Data/WDI/WDI.Rdata"))
load(paste0(data.disk, "Data/Comtrade/comtrade_total_clean.Rdata"))

Net_Orig_Dest_Year <- panel %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI,
           partner, partner.ISO, pRegion, pIncome, pDev, pHDI,
           year) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(pExp_IFF, na.rm = T)) %>%
  ungroup()

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

Net_Orig_Avg <- Net_Orig_Year %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI) %>%
  summarize(Imp_IFF = mean(Imp_IFF, na.rm = T),
            Exp_IFF = mean(Exp_IFF, na.rm = T),
            Tot_IFF = mean(Tot_IFF, na.rm = T),
            Tot_IFF_bn = mean(Tot_IFF_bn, na.rm = T),
            Tot_IFF_GDP = mean(Tot_IFF_GDP, na.rm = T),
            Tot_IFF_trade = mean(Tot_IFF_trade, na.rm = T)) %>%
  ungroup()

Net_Orig_Sum <- Net_Orig_Year %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()

Net_Orig_Dest_Sum <- Net_Orig_Dest_Year %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI,
           partner, partner.ISO, pRegion, pIncome, pDev, pHDI) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF = Imp_IFF + Exp_IFF,
         Tot_IFF_bn = Tot_IFF / 10^9)

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

Net_Orig_Dest_Year_Africa <- Net_Orig_Dest_Year %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

Net_Orig_Dest_Africa <- Net_Orig_Dest_Sum %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

Net_Dest <- Net_Orig_Dest_Sum %>%
  group_by(partner, partner.ISO, pRegion, pIncome, pDev, pHDI) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()

Net_Dest_Africa <- Net_Orig_Dest_Africa %>%
  group_by(partner, partner.ISO, pRegion, pIncome, pDev, pHDI) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()

Net_Orig_Year_Africa <- Net_Orig_Year %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

Net_Orig_Year_LMIC <- Net_Orig_Year %>%
  filter(rIncome == "LIC" | rIncome == "LMC") %>%
  select(-rIncome)

Net_Orig_Year_Developing <- Net_Orig_Year %>%
  filter(rDev == "Developing") %>%
  select(-rDev)

Net_Orig_Year_LowHDI <- Net_Orig_Year %>%
  filter(rHDI == "Lo HDI" | rHDI == "Med HDI") %>%
  select(-rHDI)

Net_Orig_Avg_Africa <- Net_Orig_Avg %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

Net_Orig_Avg_LMIC <- Net_Orig_Avg %>%
  filter(rIncome == "LIC" | rIncome == "LMC") %>%
  select(-rIncome)

Net_Orig_Avg_Developing <- Net_Orig_Avg %>%
  filter(rDev == "Developing") %>%
  select(-rDev)

Net_Orig_Avg_LowHDI <- Net_Orig_Avg %>%
  filter(rHDI == "Lo HDI" | rHDI == "Med HDI") %>%
  select(-rHDI)

Net_Orig_Sum_Africa <- Net_Orig_Sum %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

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

Net_Africa <- Net_Year_Africa %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T))

Net_LMIC <- Net_Year_LMIC %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T))

Net_Developing <- Net_Year_Developing %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T))

Net_LowHDI <- Net_Year_LowHDI %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T))

save(Net_Orig_Dest_Year, file = "Results/Summary data-sets/Net_Orig_Dest_Year.Rdata")
write.csv(Net_Orig_Dest_Year, file = "Results/Summary data-sets/Net_Orig_Dest_Year.csv",
          row.names = F)

save(Net_Orig_Year, file = "Results/Summary data-sets/Net_Orig_Year.Rdata")
write.csv(Net_Orig_Year, file = "Results/Summary data-sets/Net_Orig_Year.csv",
          row.names = F)

save(Net_Orig_Avg, file = "Results/Summary data-sets/Net_Orig_Avg.Rdata")
write.csv(Net_Orig_Avg, file = "Results/Summary data-sets/Net_Orig_Avg.csv",
          row.names = F)
save(Net_Orig_Sum, file = "Results/Summary data-sets/Net_Orig_Sum.Rdata")
write.csv(Net_Orig_Sum, file = "Results/Summary data-sets/Net_Orig_Sum.csv",
          row.names = F)

save(Net_Orig_Dest_Avg, file = "Results/Summary data-sets/Net_Orig_Dest_Avg.Rdata")
write.csv(Net_Orig_Dest_Avg, file = "Results/Summary data-sets/Net_Orig_Dest_Avg.csv",
          row.names = F)
save(Net_Orig_Dest_Year_Africa, file = "Results/Summary data-sets/Net_Orig_Dest_Year_Africa.Rdata")
write.csv(Net_Orig_Dest_Year_Africa, file = "Results/Summary data-sets/Net_Orig_Dest_Year_Africa.csv",
          row.names = F)

save(Net_Orig_Year_Africa, file = "Results/Summary data-sets/Net_Orig_Year_Africa.Rdata")
write.csv(Net_Orig_Year_Africa, file = "Results/Summary data-sets/Net_Orig_Year_Africa.csv",
          row.names = F)
save(Net_Orig_Year_LMIC, file = "Results/Summary data-sets/Net_Orig_Year_LMIC.Rdata")
write.csv(Net_Orig_Year_LMIC, file = "Results/Summary data-sets/Net_Orig_Year_LMIC.csv",
          row.names = F)
save(Net_Orig_Year_Developing, file = "Results/Summary data-sets/Net_Orig_Year_Developing.Rdata")
write.csv(Net_Orig_Year_Developing, file = "Results/Summary data-sets/Net_Orig_Year_Developing.csv",
          row.names = F)
save(Net_Orig_Year_LowHDI, file = "Results/Summary data-sets/Net_Orig_Year_LowHDI.Rdata")
write.csv(Net_Orig_Year_LowHDI, file = "Results/Summary data-sets/Net_Orig_Year_LowHDI.csv",
          row.names = F)

save(Net_Orig_Avg_Africa, file = "Results/Summary data-sets/Net_Orig_Avg_Africa.Rdata")
write.csv(Net_Orig_Avg_Africa, file = "Results/Summary data-sets/Net_Orig_Avg_Africa.csv",
          row.names = F)
save(Net_Orig_Avg_LMIC, file = "Results/Summary data-sets/Net_Orig_Avg_LMIC.Rdata")
write.csv(Net_Orig_Avg_LMIC, file = "Results/Summary data-sets/Net_Orig_Avg_LMIC.csv",
          row.names = F)
save(Net_Orig_Avg_Developing, file = "Results/Summary data-sets/Net_Orig_Avg_Developing.Rdata")
write.csv(Net_Orig_Avg_Developing, file = "Results/Summary data-sets/Net_Orig_Avg_Developing.csv",
          row.names = F)
save(Net_Orig_Avg_LowHDI, file = "Results/Summary data-sets/Net_Orig_Avg_LowHDI.Rdata")
write.csv(Net_Orig_Avg_LowHDI, file = "Results/Summary data-sets/Net_Orig_Avg_LowHDI.csv",
          row.names = F)

save(Net_Orig_Sum_Africa, file = "Results/Summary data-sets/Net_Orig_Sum_Africa.Rdata")
write.csv(Net_Orig_Sum_Africa, file = "Results/Summary data-sets/Net_Orig_Sum_Africa.csv",
          row.names = F)
save(Net_Orig_Dest_Africa, file = "Results/Summary data-sets/Net_Orig_Dest_Africa.Rdata")
write.csv(Net_Orig_Dest_Africa, file = "Results/Summary data-sets/Net_Orig_Dest_Africa.csv",
          row.names = F)

save(Net_Dest, file = "Results/Summary data-sets/Net_Dest.Rdata")
write.csv(Net_Dest, file = "Results/Summary data-sets/Net_Dest.csv",
          row.names = F)
save(Net_Dest_Africa, file = "Results/Summary data-sets/Net_Dest_Africa.Rdata")
write.csv(Net_Dest_Africa, file = "Results/Summary data-sets/Net_Dest_Africa.csv",
          row.names = F)

save(Net_Year_Africa, file = "Results/Summary data-sets/Net_Year_Africa.Rdata")
write.csv(Net_Year_Africa, file = "Results/Summary data-sets/Net_Year_Africa.csv",
          row.names = F)
save(Net_Year_LMIC, file = "Results/Summary data-sets/Net_Year_LMIC.Rdata")
write.csv(Net_Year_LMIC, file = "Results/Summary data-sets/Net_Year_LMIC.csv",
          row.names = F)
save(Net_Year_Developing, file = "Results/Summary data-sets/Net_Year_Developing.Rdata")
write.csv(Net_Year_Developing, file = "Results/Summary data-sets/Net_Year_Developing.csv",
          row.names = F)
save(Net_Year_LowHDI, file = "Results/Summary data-sets/Net_Year_LowHDI.Rdata")
write.csv(Net_Year_LowHDI, file = "Results/Summary data-sets/Net_Year_LowHDI.csv",
          row.names = F)

save(Net_Africa, file = "Results/Summary data-sets/Net_Africa.Rdata")
write.csv(Net_Africa, file = "Results/Summary data-sets/Net_Africa.csv",
          row.names = F)
save(Net_LMIC, file = "Results/Summary data-sets/Net_LMIC.Rdata")
write.csv(Net_LMIC, file = "Results/Summary data-sets/Net_LMIC.csv",
          row.names = F)
save(Net_Developing, file = "Results/Summary data-sets/Net_Developing.Rdata")
write.csv(Net_Developing, file = "Results/Summary data-sets/Net_Developing.csv",
          row.names = F)
save(Net_LowHDI, file = "Results/Summary data-sets/Net_LowHDI.Rdata")
write.csv(Net_LowHDI, file = "Results/Summary data-sets/Net_LowHDI.csv",
          row.names = F)



## ## ## ## ## ## ## ## ## ## ##
# AGGREGATE BY SECTOR       ####
## ## ## ## ## ## ## ## ## ## ##

load(paste0(data.disk, "Data/WDI/WDI.Rdata"))
load(paste0(data.disk, "Data/UN Stats/HS.Rdata"))
panel <- left_join(panel, HS %>% select(chapter, chapter.description),
                   by = c("commodity.code" = "chapter")) %>%
  rename(commodity = chapter.description)


# .. Aggregate results using Gross Excluding Reversals ####
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

GER_Exp_Sect <- panel %>%
  filter(pExp_IFF > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI,
           year, 
           section.code, section) %>%
  summarize(Exp_IFF = sum(pExp_IFF, na.rm = T)) %>%
  ungroup()

GER_Exp_Sect_disag <- panel %>%
  filter(pExp_IFF > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI,
           year, 
           commodity.code, commodity) %>%
  summarize(Exp_IFF = sum(pExp_IFF, na.rm = T)) %>%
  ungroup()

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
rm(GER_Imp_Sect, GER_Exp_Sect)

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
rm(GER_Imp_Sect_disag, GER_Exp_Sect_disag)

GER_Orig_Sect_Avg <- GER_Orig_Sect_Year %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI, section.code, section) %>%
  summarize(Imp_IFF = mean(Imp_IFF, na.rm = T),
            Exp_IFF = mean(Exp_IFF, na.rm = T)) %>%
  ungroup() %>%
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

GER_Orig_Sect_Avg_disag <- GER_Orig_Sect_Year_disag %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI, commodity.code, commodity) %>%
  summarize(Imp_IFF = mean(Imp_IFF, na.rm = T),
            Exp_IFF = mean(Exp_IFF, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF = Imp_IFF + Exp_IFF,
         Tot_IFF_bn = Tot_IFF / 10^9)

GER_Orig_Sect_Sum <- GER_Orig_Sect_Year %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI, section.code, section) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF = Imp_IFF + Exp_IFF,
         Tot_IFF_bn = Tot_IFF / 10^9)

GER_Sect_Year <- GER_Orig_Sect_Year %>%
  group_by(section.code, section, year) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF = Imp_IFF + Exp_IFF,
         Tot_IFF_bn = Tot_IFF / 10^9)

GER_Orig_Sect_Year_Africa <- GER_Orig_Sect_Year %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

GER_Orig_Sect_Avg_Africa <- GER_Orig_Sect_Avg %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

GER_Orig_Sect_Avg_Africa_disag <- GER_Orig_Sect_Avg_disag %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

GER_Orig_Sect_Avg_LMIC <- GER_Orig_Sect_Avg %>%
  filter(rIncome == "LIC" | rIncome == "LMC") %>%
  select(-rIncome)

GER_Orig_Sect_Avg_LMIC_disag <- GER_Orig_Sect_Avg_disag %>%
  filter(rIncome == "LIC" | rIncome == "LMC") %>%
  select(-rIncome)

GER_Orig_Sect_Avg_Developing <- GER_Orig_Sect_Avg %>%
  filter(rDev == "Developing") %>%
  select(-rDev)

GER_Orig_Sect_Avg_Developing_disag <- GER_Orig_Sect_Avg_disag %>%
  filter(rDev == "Developing") %>%
  select(-rDev)

GER_Orig_Sect_Avg_LowHDI <- GER_Orig_Sect_Avg %>%
  filter(rHDI == "Lo HDI" | rHDI == "Med HDI") %>%
  select(-rHDI)

GER_Orig_Sect_Avg_LowHDI_disag <- GER_Orig_Sect_Avg_disag %>%
  filter(rHDI == "Lo HDI" | rHDI == "Med HDI") %>%
  select(-rHDI)

GER_Orig_Sect_Sum_Africa <- GER_Orig_Sect_Sum %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

GER_Orig_Sect_Sum_LMIC <- GER_Orig_Sect_Sum %>%
  filter(rIncome == "LIC" | rIncome == "LMC") %>%
  select(-rIncome)

GER_Orig_Sect_Sum_Developing <- GER_Orig_Sect_Sum %>%
  filter(rDev == "Developing") %>%
  select(-rDev)

GER_Orig_Sect_Sum_LowHDI <- GER_Orig_Sect_Sum %>%
  filter(rHDI == "Lo HDI" | rHDI == "Med HDI") %>%
  select(-rHDI)

GER_Sect_Africa <- GER_Orig_Sect_Sum_Africa %>%
  group_by(section.code, section) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()

GER_Sect_LMIC <- GER_Orig_Sect_Sum_LMIC %>%
  group_by(section.code, section) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()

GER_Sect_Developing <- GER_Orig_Sect_Sum_Developing %>%
  group_by(section.code, section) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()

GER_Sect_LowHDI <- GER_Orig_Sect_Sum_LowHDI %>%
  group_by(section.code, section) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()

GER_Sect_Avg_Africa <- GER_Orig_Sect_Avg_Africa %>%
  group_by(section.code, section) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()

GER_Sect_Avg_Africa_disag <- GER_Orig_Sect_Avg_Africa_disag %>%
  group_by(commodity.code, commodity) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()

GER_Sect_Avg_LMIC <- GER_Orig_Sect_Avg_LMIC %>%
  group_by(section.code, section) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()

GER_Sect_Avg_LMIC_disag <- GER_Orig_Sect_Avg_LMIC_disag %>%
  group_by(commodity.code, commodity) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()

GER_Sect_Avg_Developing <- GER_Orig_Sect_Avg_Developing %>%
  group_by(section.code, section) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()

GER_Sect_Avg_Developing_disag <- GER_Orig_Sect_Avg_Developing_disag %>%
  group_by(commodity.code, commodity) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()

GER_Sect_Avg_LowHDI <- GER_Orig_Sect_Avg_LowHDI %>%
  group_by(section.code, section) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()

GER_Sect_Avg_LowHDI_disag <- GER_Orig_Sect_Avg_LowHDI_disag %>%
  group_by(commodity.code, commodity) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()

save(GER_Orig_Sect_Year_Africa, file = "Results/Summary data-sets/GER_Orig_Sect_Year_Africa.Rdata")
write.csv(GER_Orig_Sect_Year_Africa, file = "Results/Summary data-sets/GER_Orig_Sect_Year_Africa.csv",
          row.names = F)
save(GER_Orig_Sect_Avg, file = "Results/Summary data-sets/GER_Orig_Sect_Avg.Rdata")

write.csv(GER_Orig_Sect_Avg, file = "Results/Summary data-sets/GER_Orig_Sect_Avg.csv",
          row.names = F)
save(GER_Orig_Sect_Avg_Africa, file = "Results/Summary data-sets/GER_Orig_Sect_Avg_Africa.Rdata")
write.csv(GER_Orig_Sect_Avg_Africa, file = "Results/Summary data-sets/GER_Orig_Sect_Avg_Africa.csv",
          row.names = F)
save(GER_Orig_Sect_Sum_Africa, file = "Results/Summary data-sets/GER_Orig_Sect_Sum_Africa.Rdata")
write.csv(GER_Orig_Sect_Sum_Africa, file = "Results/Summary data-sets/GER_Orig_Sect_Sum_Africa.csv",
          row.names = F)

save(GER_Sect_Africa, file = "Results/Summary data-sets/GER_Sect_Africa.Rdata")
write.csv(GER_Sect_Africa, file = "Results/Summary data-sets/GER_Sect_Africa.csv",
          row.names = F)
save(GER_Sect_LMIC, file = "Results/Summary data-sets/GER_Sect_LMIC.Rdata")
write.csv(GER_Sect_LMIC, file = "Results/Summary data-sets/GER_Sect_LMIC.csv",
          row.names = F)
save(GER_Sect_Developing, file = "Results/Summary data-sets/GER_Sect_Developing.Rdata")
write.csv(GER_Sect_Developing, file = "Results/Summary data-sets/GER_Sect_Developing.csv",
          row.names = F)
save(GER_Sect_LowHDI, file = "Results/Summary data-sets/GER_Sect_LowHDI.Rdata")
write.csv(GER_Sect_LowHDI, file = "Results/Summary data-sets/GER_Sect_LowHDI.csv",
          row.names = F)

save(GER_Sect_Avg_Africa, file = "Results/Summary data-sets/GER_Sect_Avg_Africa.Rdata")
write.csv(GER_Sect_Avg_Africa, file = "Results/Summary data-sets/GER_Sect_Avg_Africa.csv",
          row.names = F)
save(GER_Sect_Avg_Africa_disag, file = "Results/Summary data-sets/GER_Sect_Avg_Africa_disag.Rdata")
write.csv(GER_Sect_Avg_Africa_disag, file = "Results/Summary data-sets/GER_Sect_Avg_Africa_disag.csv",
          row.names = F)
save(GER_Sect_Avg_LMIC, file = "Results/Summary data-sets/GER_Sect_Avg_LMIC.Rdata")
write.csv(GER_Sect_Avg_LMIC, file = "Results/Summary data-sets/GER_Sect_Avg_LMIC.csv",
          row.names = F)
save(GER_Sect_Avg_LMIC_disag, file = "Results/Summary data-sets/GER_Sect_Avg_LMIC_disag.Rdata")
write.csv(GER_Sect_Avg_LMIC_disag, file = "Results/Summary data-sets/GER_Sect_Avg_LMIC_disag.csv",
          row.names = F)
save(GER_Sect_Avg_Developing, file = "Results/Summary data-sets/GER_Sect_Avg_Developing.Rdata")
write.csv(GER_Sect_Avg_Developing, file = "Results/Summary data-sets/GER_Sect_Avg_Developing.csv",
          row.names = F)
save(GER_Sect_Avg_Developing_disag, file = "Results/Summary data-sets/GER_Sect_Avg_Developing_disag.Rdata")
write.csv(GER_Sect_Avg_Developing_disag, file = "Results/Summary data-sets/GER_Sect_Avg_Developing_disag.csv",
          row.names = F)
save(GER_Sect_Avg_LowHDI, file = "Results/Summary data-sets/GER_Sect_Avg_LowHDI.Rdata")
write.csv(GER_Sect_Avg_LowHDI, file = "Results/Summary data-sets/GER_Sect_Avg_LowHDI.csv",
          row.names = F)
save(GER_Sect_Avg_LowHDI_disag, file = "Results/Summary data-sets/GER_Sect_Avg_LowHDI_disag.Rdata")
write.csv(GER_Sect_Avg_LowHDI_disag, file = "Results/Summary data-sets/GER_Sect_Avg_LowHDI_disag.csv",
          row.names = F)


# .. Aggregate results using Net Aggregation ####
Net_Orig_Sect_Year <- panel %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rHDI,
           year, section.code, section) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(pExp_IFF, na.rm = T)) %>%
  ungroup()

Net_Orig_Sect_Avg <- Net_Orig_Sect_Year %>%
  group_by(reporter, reporter.ISO, rRegion, section.code, section) %>%
  summarize(Imp_IFF = mean(Imp_IFF, na.rm = T),
            Exp_IFF = mean(Exp_IFF, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF = Imp_IFF + Exp_IFF,
         Tot_IFF_bn = Tot_IFF / 10^9)

Net_Orig_Sect_Sum <- Net_Orig_Sect_Year %>%
  group_by(reporter, reporter.ISO, rRegion, section.code, section) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF = Imp_IFF + Exp_IFF,
         Tot_IFF_bn = Tot_IFF / 10^9)

Net_Orig_Sect_Year_Africa <- Net_Orig_Sect_Year %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

Net_Orig_Sect_Avg_Africa <- Net_Orig_Sect_Avg %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

Net_Orig_Sect_Sum_Africa <- Net_Orig_Sect_Sum %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

Net_Sect_Africa <- Net_Orig_Sect_Sum_Africa %>%
  group_by(section.code, section) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()

save(Net_Orig_Sect_Year_Africa, file = "Results/Summary data-sets/Net_Orig_Sect_Year_Africa.Rdata")
write.csv(GER_Orig_Sect_Year_Africa, file = "Results/Summary data-sets/Net_Orig_Sect_Year_Africa.csv",
          row.names = F)
save(Net_Orig_Sect_Avg_Africa, file = "Results/Summary data-sets/Net_Orig_Sect_Avg_Africa.Rdata")
write.csv(Net_Orig_Sect_Avg_Africa, file = "Results/Summary data-sets/Net_Orig_Sect_Avg_Africa.csv",
          row.names = F)
save(Net_Orig_Sect_Sum_Africa, file = "Results/Summary data-sets/Net_Orig_Sect_Sum_Africa.Rdata")
write.csv(Net_Orig_Sect_Sum_Africa, file = "Results/Summary data-sets/Net_Orig_Sect_Sum_Africa.csv",
          row.names = F)

save(Net_Sect_Africa, file = "Results/Summary data-sets/Net_Sect_Africa.Rdata")
write.csv(Net_Sect_Africa, file = "Results/Summary data-sets/Net_Sect_Africa.csv",
          row.names = F)



## ## ## ## ## ## ## ## ## ## ##
# HEADLINE RESULTS          ####
## ## ## ## ## ## ## ## ## ## ##


# .. For Africa ####
(Cumulative.gross <- sum(GER_Year_Africa$Tot_IFF_bn))
# 1493.782

(Cumulative.gross.GDP <- Cumulative.gross / (sum(GER_Year_Africa$GDP) / 10^9)) * 100
# 5.712601

(Cumulative.gross.trade <- Cumulative.gross / (sum(GER_Year_Africa$Total_value) / 10^9)) * 100
# 12.3659

(Cumulative.net <- sum(Net_Year_Africa$Tot_IFF_bn))
# 629.2157

(Cumulative.net.GDP <- Cumulative.net / (sum(Net_Year_Africa$GDP) / 10^9)) * 100
# 2.40628

(Gross.IFF.per.year <- sum(GER_Orig_Avg_Africa$Tot_IFF_bn))
# 94.79566

(Net.IFF.per.year <- sum(Net_Orig_Avg_Africa$Tot_IFF_bn))
# 31.91687


# .. For low and lower-middle income countries ####
(Cumulative.gross <- sum(GER_Year_LMIC$Tot_IFF_bn))
# 4002.906

(Cumulative.gross.GDP <- Cumulative.gross / (sum(GER_Year_LMIC$GDP) / 10^9)) * 100
# 6.353536

(Cumulative.gross.trade <- Cumulative.gross / (sum(GER_Year_LMIC$Total_value) / 10^9)) * 100
# 14.18342

(Cumulative.net <- sum(Net_Year_LMIC$Tot_IFF_bn))
# 1974.386

(Cumulative.net.GDP <- Cumulative.net / (sum(Net_Year_LMIC$GDP) / 10^9)) * 100
# 3.133774

(Gross.IFF.per.year <- sum(GER_Orig_Avg_LMIC$Tot_IFF_bn))
# 238.6362

(Net.IFF.per.year <- sum(Net_Orig_Avg_LMIC$Tot_IFF_bn))
# 109.3398


# .. For developing countries ####
(Cumulative.gross <- sum(GER_Year_Developing$Tot_IFF_bn))
# 22162.91

(Cumulative.gross.GDP <- Cumulative.gross / (sum(GER_Year_Developing$GDP) / 10^9)) * 100
# 6.349581

(Cumulative.gross.trade <- Cumulative.gross / (sum(GER_Year_Developing$Total_value) / 10^9)) * 100
# 11.23221

(Cumulative.net <- sum(Net_Year_Developing$Tot_IFF_bn))
# 11046.03

(Cumulative.net.GDP <- Cumulative.net / (sum(Net_Year_Developing$GDP) / 10^9)) * 100
# 3.163026

(Gross.IFF.per.year <- sum(GER_Orig_Avg_Developing$Tot_IFF_bn))
# 1211.909

(Net.IFF.per.year <- sum(Net_Orig_Avg_Developing$Tot_IFF_bn))
# 596.3079


# .. For low HDI countries ####
(Cumulative.gross <- sum(GER_Year_LowHDI$Tot_IFF_bn))
# 2807.377

(Cumulative.gross.GDP <- Cumulative.gross / (sum(GER_Year_LowHDI$GDP) / 10^9)) * 100
# 5.70254

(Cumulative.gross.trade <- Cumulative.gross / (sum(GER_Year_LowHDI$Total_value) / 10^9)) * 100
# 15.09529

(Cumulative.net <- sum(Net_Year_LowHDI$Tot_IFF_bn))
# 1336.962

(Cumulative.net.GDP <- Cumulative.net / (sum(Net_Year_LowHDI$GDP) / 10^9)) * 100
# 2.70594

(Gross.IFF.per.year <- sum(GER_Orig_Avg_LowHDI$Tot_IFF_bn))
# 171.9945

(Net.IFF.per.year <- sum(Net_Orig_Avg_LowHDI$Tot_IFF_bn))
# 73.87299



# ## ## ## ## ## ## ## ## ## ## ##
# # LMIC RESULTS              ####
# ## ## ## ## ## ## ## ## ## ## ##
# 
# # .. Proportion of LMIC outflows to LMICs ####
# load("Results/Summary data-sets/GER_Orig_Avg_LMIC.Rdata")
# load("Results/Summary data-sets/GER_Orig_Dest_Avg_LMIC.Rdata")
# 
# load("Results/Summary data-sets/GER_Orig_Dest_Year_LMIC.Rdata")
# 
# # GER_Orig_Dest_Year_LMIC_toLMICs <- GER_Orig_Dest_Year_LMIC %>%
# #   filter(pIncome == "LIC" | pIncome == "LMC") %>%
# #   group_by(reporter, reporter.ISO, rRegion, rDev) %>%
# #     summarize(Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
# #               Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T),
# #               Tot_IFF_hi = sum(Tot_IFF_hi, na.rm = T),
# #               Tot_IFF_hi_bn = sum(Tot_IFF_hi_bn, na.rm = T)) %>%
# #     ungroup()
# 
# GER_Orig_Dest_Avg_LMIC_toLMICs <- GER_Orig_Dest_Avg_LMIC %>%
#   filter(pIncome == "LIC" | pIncome == "LMC")
# 
# GER_Orig_Avg_LMIC_toLMICs <- GER_Orig_Dest_Avg_LMIC_toLMICs %>%
#   group_by(reporter, reporter.ISO, rRegion, rDev) %>%
#   summarize(Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
#             Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T),
#             Tot_IFF_hi = sum(Tot_IFF_hi, na.rm = T),
#             Tot_IFF_hi_bn = sum(Tot_IFF_hi_bn, na.rm = T)) %>%
#   ungroup()
# 
# sum(GER_Orig_Avg_LMIC_toLMICs$Tot_IFF_hi_bn) / sum(GER_Orig_Avg_LMIC$Tot_IFF_hi_bn)
# # 0.1454214
# 
# 
# # .. Compare net and gross ####
# load("Results/Summary data-sets/GER_Orig_Avg_LMIC.Rdata")
# load("Results/Summary data-sets/Net_Orig_Avg_LMIC.Rdata")
# 
# viz <- full_join(GER_Orig_Avg_LMIC %>% 
#                    select(reporter,
#                           GER_Tot_IFF_hi = Tot_IFF_hi),
#                  Net_Orig_Avg_LMIC %>%
#                    select(reporter,
#                           Net_Tot_IFF_hi = Tot_IFF_hi),
#                  by = c("reporter"))
# 
# ggplot(viz %>%  
#          melt(id.vars = "reporter"),
#        aes(x = reporter, y = value, fill = fct_rev(variable))) +
#   geom_bar(position = "dodge", stat = "identity") +
#   coord_flip() + 
#   scale_y_continuous(labels = dollar_format(scale = 1/10^9, accuracy = 1)) +
#   scale_fill_discrete(name = "Estimate", labels = c("Net", "Gross"))
