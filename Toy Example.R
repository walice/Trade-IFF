## ## ## ## ## ## ## ## ## ## ##
# CHECKING EXPORT IFF       ####
## ## ## ## ## ## ## ## ## ## ##

test <- panel_hi %>% 
  filter(id == "ETH_USA_07_2010" | id == "USA_ETH_07_2010") %>%
  select(-c(reporter, rRegion, rIncome, partner, pRegion, pIncome,
            Export_value, pExport_value, ReExport_value, pReExport_value))

test <- panel_hi %>% 
  filter(id == "ZAF_BRA_09_2015" | id == "BRA_ZAF_09_2015") %>%
  select(-c(reporter, rRegion, rIncome, partner, pRegion, pIncome,
            Export_value, pExport_value, ReExport_value, pReExport_value))


results <- panel
test <- results %>% filter(reporter.ISO == "ETH")
test <- test %>%
  filter(complete.cases(Imp_IFF_lo, pExp_IFF_lo)) %>%
  filter(complete.cases(Imp_IFF_hi, pExp_IFF_hi))
test <- test %>% filter(year == 2010 & partner.ISO == "USA")




## ## ## ## ## ## ## ## ## ## ##
# TOY EXAMPLE               ####
## ## ## ## ## ## ## ## ## ## ##
test <- panel %>% filter(id == "ARG_CHN_05_2000" | id == "CHN_ARG_05_2000")


###############
df <- data.frame(id = c("FRA_USA", "FRA_GBR", "FRA_ESP", "FRA_ETH", "FRA_AUS"),
                 Import = c(110, 325, 400, 1000, 500),
                 pNetExport = c(100, 300, 300, 500, 400),
                 Dist = c(10, 2, 1, 100, 500),
                 Tariff = c(1, 2, 3, 5, 3))
df

df$ratio_CIF <- df$Import / df$pNetExport

mod <- lm(log(ratio_CIF) ~ Dist + Tariff, data = df)
summary(mod)

coef <- coef(mod)
coef[3] <- 0
coef
df$fitted_nonIFF <- (model.matrix(mod) %*% coef)

coef <- coef(mod)
coef[1] <- 0
coef[2] <- 0
coef
df$fitted_IFF <- (model.matrix(mod) %*% coef)

df$fitted_all <- exp(df$fitted_nonIFF + df$fitted_IFF)
df$fitted <- exp(fitted(mod))
round(df$fitted_all, 5) == round(df$fitted, 5)
