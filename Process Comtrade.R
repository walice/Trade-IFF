# Process Comtrade
# Alice Lepissier
# alice.lepissier@gmail.com
# Prepared for UNECA

## ## ## ## ## ## ## ## ## ## ##
# INDEX                     ####
## ## ## ## ## ## ## ## ## ## ##
# Preamble
# Import Bulk Database
# Export Data



## ## ## ## ## ## ## ## ## ## ##
# PREAMBLE                  ####
## ## ## ## ## ## ## ## ## ## ##

#setwd("C:/boxsync/alepissier/PhD/Miscellany/Comtrade raw database/") # Alice work
setwd("/home/alice/IFFe/") # Virtual server
library(tidyverse)



## ## ## ## ## ## ## ## ## ## ##
# IMPORT BULK DATABASE      ####
## ## ## ## ## ## ## ## ## ## ##

yr1999 <- read.table("Data/Comtrade/Bulk downloads/Raw database/type-C_r-ALL_ps-1999_freq-A_px-HS_pub-20051228_fmt-csv_ex-20151115.csv",
                     sep = ",", header = TRUE, quote = "\"", fill = TRUE,
                     colClasses = c("NULL", NA, "NULL", "NULL", NA, "NULL", "NULL", NA, "NULL", NA, NA,
                                    "NULL", NA, NA, NA, NA, "NULL", "NULL", NA, "NULL", NA, "NULL"))
nrow1999 <- nrow(yr1999)
yr1999 <- yr1999 %>% filter(Aggregate.Level == 2)

yr2000 <- read.table("Data/Comtrade/Bulk downloads/Raw database/type-C_r-ALL_ps-2000_freq-A_px-HS_pub-20180115_fmt-csv_ex-20180225.csv",
                     sep = ",", header = TRUE, quote = "\"", fill = TRUE,
                     colClasses = c("NULL", NA, "NULL", "NULL", NA, "NULL", "NULL", NA, "NULL", NA, NA,
                                    "NULL", NA, NA, NA, NA, "NULL", "NULL", NA, "NULL", NA, "NULL"))
nrow2000 <- nrow(yr2000)
yr2000 <- yr2000 %>% filter(Aggregate.Level == 2)

yr2001 <- read.table("Data/Comtrade/Bulk downloads/Raw database/type-C_r-ALL_ps-2001_freq-A_px-HS_pub-20180115_fmt-csv_ex-20180225.csv",
                     sep = ",", header = TRUE, quote = "\"", fill = TRUE,
                     colClasses = c("NULL", NA, "NULL", "NULL", NA, "NULL", "NULL", NA, "NULL", NA, NA,
                                    "NULL", NA, NA, NA, NA, "NULL", "NULL", NA, "NULL", NA, "NULL"))
nrow2001 <- nrow(yr2001)
yr2001 <- yr2001 %>% filter(Aggregate.Level == 2)

yr2002 <- read.table("Data/Comtrade/Bulk downloads/Raw database/type-C_r-ALL_ps-2002_freq-A_px-HS_pub-20180115_fmt-csv_ex-20180225.csv",
                     sep = ",", header = TRUE, quote = "\"", fill = TRUE,
                     colClasses = c("NULL", NA, "NULL", "NULL", NA, "NULL", "NULL", NA, "NULL", NA, NA,
                                    "NULL", NA, NA, NA, NA, "NULL", "NULL", NA, "NULL", NA, "NULL"))
nrow2002 <- nrow(yr2002)
yr2002 <- yr2002 %>% filter(Aggregate.Level == 2)

yr2003 <- read.table("Data/Comtrade/Bulk downloads/Raw database/type-C_r-ALL_ps-2003_freq-A_px-HS_pub-20180113_fmt-csv_ex-20180225.csv",
                     sep = ",", header = TRUE, quote = "\"", fill = TRUE,
                     colClasses = c("NULL", NA, "NULL", "NULL", NA, "NULL", "NULL", NA, "NULL", NA, NA,
                                    "NULL", NA, NA, NA, NA, "NULL", "NULL", NA, "NULL", NA, "NULL"))
nrow2003 <- nrow(yr2003)
yr2003 <- yr2003 %>% filter(Aggregate.Level == 2)

yr2004 <- read.table("Data/Comtrade/Bulk downloads/Raw database/type-C_r-ALL_ps-2004_freq-A_px-HS_pub-20180112_fmt-csv_ex-20180225.csv",
                     sep = ",", header = TRUE, quote = "\"", fill = TRUE,
                     colClasses = c("NULL", NA, "NULL", "NULL", NA, "NULL", "NULL", NA, "NULL", NA, NA,
                                    "NULL", NA, NA, NA, NA, "NULL", "NULL", NA, "NULL", NA, "NULL"))
nrow2004 <- nrow(yr2004)
yr2004 <- yr2004 %>% filter(Aggregate.Level == 2)

yr2005 <- read.table("Data/Comtrade/Bulk downloads/Raw database/type-C_r-ALL_ps-2005_freq-A_px-HS_pub-20180109_fmt-csv_ex-20180225.csv",
                     sep = ",", header = TRUE, quote = "\"", fill = TRUE,
                     colClasses = c("NULL", NA, "NULL", "NULL", NA, "NULL", "NULL", NA, "NULL", NA, NA,
                                    "NULL", NA, NA, NA, NA, "NULL", "NULL", NA, "NULL", NA, "NULL"))
nrow2005 <- nrow(yr2005)
yr2005 <- yr2005 %>% filter(Aggregate.Level == 2)

yr2006 <- read.table("Data/Comtrade/Bulk downloads/Raw database/type-C_r-ALL_ps-2006_freq-A_px-HS_pub-20171212_fmt-csv_ex-20171229.csv",
                     sep = ",", header = TRUE, quote = "\"", fill = TRUE,
                     colClasses = c("NULL", NA, "NULL", "NULL", NA, "NULL", "NULL", NA, "NULL", NA, NA,
                                    "NULL", NA, NA, NA, NA, "NULL", "NULL", NA, "NULL", NA, "NULL"))
nrow2006 <- nrow(yr2006)
yr2006 <- yr2006 %>% filter(Aggregate.Level == 2)

yr2007 <- read.table("Data/Comtrade/Bulk downloads/Raw database/type-C_r-ALL_ps-2007_freq-A_px-HS_pub-20171212_fmt-csv_ex-20171229.csv",
                     sep = ",", header = TRUE, quote = "\"", fill = TRUE,
                     colClasses = c("NULL", NA, "NULL", "NULL", NA, "NULL", "NULL", NA, "NULL", NA, NA,
                                    "NULL", NA, NA, NA, NA, "NULL", "NULL", NA, "NULL", NA, "NULL"))
nrow2007 <- nrow(yr2007)
yr2007 <- yr2007 %>% filter(Aggregate.Level == 2)

yr2008 <- read.table("Data/Comtrade/Bulk downloads/Raw database/type-C_r-ALL_ps-2008_freq-A_px-HS_pub-20180726_fmt-csv_ex-20180828.csv",
                     sep = ",", header = TRUE, quote = "\"", fill = TRUE,
                     colClasses = c("NULL", NA, "NULL", "NULL", NA, "NULL", "NULL", NA, "NULL", NA, NA,
                                    "NULL", NA, NA, NA, NA, "NULL", "NULL", NA, "NULL", NA, "NULL"))
nrow2008 <- nrow(yr2008)
yr2008 <- yr2008 %>% filter(Aggregate.Level == 2)

yr2009 <- read.table("Data/Comtrade/Bulk downloads/Raw database/type-C_r-ALL_ps-2009_freq-A_px-HS_pub-20180726_fmt-csv_ex-20180828.csv",
                     sep = ",", header = TRUE, quote = "\"", fill = TRUE,
                     colClasses = c("NULL", NA, "NULL", "NULL", NA, "NULL", "NULL", NA, "NULL", NA, NA,
                                    "NULL", NA, NA, NA, NA, "NULL", "NULL", NA, "NULL", NA, "NULL"))
nrow2009 <- nrow(yr2009)
yr2009 <- yr2009 %>% filter(Aggregate.Level == 2)

yr2010 <- read.table("Data/Comtrade/Bulk downloads/Raw database/type-C_r-ALL_ps-2010_freq-A_px-HS_pub-20181226_fmt-csv_ex-20190116.csv",
                     sep = ",", header = TRUE, quote = "\"", fill = TRUE,
                     colClasses = c("NULL", NA, "NULL", "NULL", NA, "NULL", "NULL", NA, "NULL", NA, NA,
                                    "NULL", NA, NA, NA, NA, "NULL", "NULL", NA, "NULL", NA, "NULL"))
nrow2010 <- nrow(yr2010)
yr2010 <- yr2010 %>% filter(Aggregate.Level == 2)

yr2011 <- read.table("Data/Comtrade/Bulk downloads/Raw database/type-C_r-ALL_ps-2011_freq-A_px-HS_pub-20181115_fmt-csv_ex-20181127.csv",
                     sep = ",", header = TRUE, quote = "\"", fill = TRUE,
                     colClasses = c("NULL", NA, "NULL", "NULL", NA, "NULL", "NULL", NA, "NULL", NA, NA,
                                    "NULL", NA, NA, NA, NA, "NULL", "NULL", NA, "NULL", NA, "NULL"))
nrow2011 <- nrow(yr2011)
yr2011 <- yr2011 %>% filter(Aggregate.Level == 2)

yr2012 <- read.table("Data/Comtrade/Bulk downloads/Raw database/type-C_r-ALL_ps-2012_freq-A_px-HS_pub-20181220_fmt-csv_ex-20181221.csv",
                     sep = ",", header = TRUE, quote = "\"", fill = TRUE,
                     colClasses = c("NULL", NA, "NULL", "NULL", NA, "NULL", "NULL", NA, "NULL", NA, NA,
                                    "NULL", NA, NA, NA, NA, "NULL", "NULL", NA, "NULL", NA, "NULL"))
nrow2012 <- nrow(yr2012)
yr2012 <- yr2012 %>% filter(Aggregate.Level == 2)

yr2013 <- read.table("Data/Comtrade/Bulk downloads/Raw database/type-C_r-ALL_ps-2013_freq-A_px-HS_pub-20181231_fmt-csv_ex-20190116.csv",
                     sep = ",", header = TRUE, quote = "\"", fill = TRUE,
                     colClasses = c("NULL", NA, "NULL", "NULL", NA, "NULL", "NULL", NA, "NULL", NA, NA,
                                    "NULL", NA, NA, NA, NA, "NULL", "NULL", NA, "NULL", NA, "NULL"))
nrow2013 <- nrow(yr2013)
yr2013 <- yr2013 %>% filter(Aggregate.Level == 2)

yr2014 <- read.table("Data/Comtrade/Bulk downloads/Raw database/type-C_r-ALL_ps-2014_freq-A_px-HS_pub-20181226_fmt-csv_ex-20190117.csv",
                     sep = ",", header = TRUE, quote = "\"", fill = TRUE,
                     colClasses = c("NULL", NA, "NULL", "NULL", NA, "NULL", "NULL", NA, "NULL", NA, NA,
                                    "NULL", NA, NA, NA, NA, "NULL", "NULL", NA, "NULL", NA, "NULL"))
nrow2014 <- nrow(yr2014)
yr2014 <- yr2014 %>% filter(Aggregate.Level == 2)

yr2015 <- read.table("Data/Comtrade/Bulk downloads/Raw database/type-C_r-ALL_ps-2015_freq-A_px-HS_pub-20181116_fmt-csv_ex-20181127.csv",
                     sep = ",", header = TRUE, quote = "\"", fill = TRUE,
                     colClasses = c("NULL", NA, "NULL", "NULL", NA, "NULL", "NULL", NA, "NULL", NA, NA,
                                    "NULL", NA, NA, NA, NA, "NULL", "NULL", NA, "NULL", NA, "NULL"))
nrow2015 <- nrow(yr2015)
yr2015 <- yr2015 %>% filter(Aggregate.Level == 2)

yr2016 <- read.table("Data/Comtrade/Bulk downloads/Raw database/type-C_r-ALL_ps-2016_freq-A_px-HS_pub-20190109_fmt-csv_ex-20190116.csv",
                     sep = ",", header = TRUE, quote = "\"", fill = TRUE,
                     colClasses = c("NULL", NA, "NULL", "NULL", NA, "NULL", "NULL", NA, "NULL", NA, NA,
                                    "NULL", NA, NA, NA, NA, "NULL", "NULL", NA, "NULL", NA, "NULL"))
 nrow2016 <- nrow(yr2016)
yr2016 <- yr2016 %>% filter(Aggregate.Level == 2)

yr2017 <- read.table("Data/Comtrade/Bulk downloads/Raw database/type-C_r-ALL_ps-2017_freq-A_px-HS_pub-20181220_fmt-csv_ex-20181221.csv",
                     sep = ",", header = TRUE, quote = "\"", fill = TRUE,
                     colClasses = c("NULL", NA, "NULL", "NULL", NA, "NULL", "NULL", NA, "NULL", NA, NA,
                                    "NULL", NA, NA, NA, NA, "NULL", "NULL", NA, "NULL", NA, "NULL"))
nrow2017 <- nrow(yr2017)
yr2017 <- yr2017 %>% filter(Aggregate.Level == 2)



## ## ## ## ## ## ## ## ## ## ##
# EXPORT DATA               ####
## ## ## ## ## ## ## ## ## ## ##

summary <- list(year_1999 = paste(nrow1999, "observations in year 1999"),
                year_2000 = paste(nrow2000, "observations in year 2000"),
                year_2001 = paste(nrow2001, "observations in year 2001"),
                year_2002 = paste(nrow2002, "observations in year 2002"),
                year_2003 = paste(nrow2003, "observations in year 2003"),
                year_2004 = paste(nrow2004, "observations in year 2004"),
                year_2005 = paste(nrow2005, "observations in year 2005"),
                year_2006 = paste(nrow2006, "observations in year 2006"),
                year_2007 = paste(nrow2007, "observations in year 2007"),
                year_2008 = paste(nrow2008, "observations in year 2008"),
                year_2009 = paste(nrow2009, "observations in year 2009"),
                year_2010 = paste(nrow2010, "observations in year 2010"),
                year_2011 = paste(nrow2011, "observations in year 2011"),
                year_2012 = paste(nrow2012, "observations in year 2012"),
                year_2013 = paste(nrow2013, "observations in year 2013"),
                year_2014 = paste(nrow2014, "observations in year 2014"),
                year_2015 = paste(nrow2015, "observations in year 2015"),
                year_2016 = paste(nrow2016, "observations in year 2016"),
                year_2017 = paste(nrow2017, "observations in year 2017"),
                total_observations = paste(sum(nrow1999, nrow2000, nrow2001, nrow2002, 
                                               nrow2003, nrow2004, nrow2005, nrow2006, 
                                               nrow2007, nrow2008, nrow2009, nrow2010, 
                                               nrow2011, nrow2012, nrow2013, nrow2014, 
                                               nrow2015, nrow2016, nrow2017), "total observations"))
capture.output(summary, file = "Data/Comtrade/Size of database up to 6-digit level.txt")

write.csv(yr1999, "Data/Comtrade/2-digit level yearly data/yr_1999.csv", row.names = FALSE)
save(yr1999, file = "Data/Comtrade/2-digit level yearly data/yr_1999.Rdata")

write.csv(yr2000, "Data/Comtrade/2-digit level yearly data/yr_2000.csv", row.names = FALSE)
save(yr2000, file = "Data/Comtrade/2-digit level yearly data/yr_2000.Rdata")

write.csv(yr2001, "Data/Comtrade/2-digit level yearly data/yr_2001.csv", row.names = FALSE)
save(yr2001, file = "Data/Comtrade/2-digit level yearly data/yr_2001.Rdata")

write.csv(yr2002, "Data/Comtrade/2-digit level yearly data/yr_2002.csv", row.names = FALSE)
save(yr2002, file = "Data/Comtrade/2-digit level yearly data/yr_2002.Rdata")

write.csv(yr2003, "Data/Comtrade/2-digit level yearly data/yr_2003.csv", row.names = FALSE)
save(yr2003, file = "Data/Comtrade/2-digit level yearly data/yr_2003.Rdata")

write.csv(yr2004, "Data/Comtrade/2-digit level yearly data/yr_2004.csv", row.names = FALSE)
save(yr2004, file = "Data/Comtrade/2-digit level yearly data/yr_2004.Rdata")

write.csv(yr2005, "Data/Comtrade/2-digit level yearly data/yr_2005.csv", row.names = FALSE)
save(yr2005, file = "Data/Comtrade/2-digit level yearly data/yr_2005.Rdata")

write.csv(yr2006, "Data/Comtrade/2-digit level yearly data/yr_2006.csv", row.names = FALSE)
save(yr2006, file = "Data/Comtrade/2-digit level yearly data/yr_2006.Rdata")

write.csv(yr2007, "Data/Comtrade/2-digit level yearly data/yr_2007.csv", row.names = FALSE)
save(yr2007, file = "Data/Comtrade/2-digit level yearly data/yr_2007.Rdata")

write.csv(yr2008, "Data/Comtrade/2-digit level yearly data/yr_2008.csv", row.names = FALSE)
save(yr2008, file = "Data/Comtrade/2-digit level yearly data/yr_2008.Rdata")

write.csv(yr2009, "Data/Comtrade/2-digit level yearly data/yr_2009.csv", row.names = FALSE)
save(yr2009, file = "Data/Comtrade/2-digit level yearly data/yr_2009.Rdata")

write.csv(yr2010, "Data/Comtrade/2-digit level yearly data/yr_2010.csv", row.names = FALSE)
save(yr2010, file = "Data/Comtrade/2-digit level yearly data/yr_2010.Rdata")

write.csv(yr2011, "Data/Comtrade/2-digit level yearly data/yr_2011.csv", row.names = FALSE)
save(yr2011, file = "Data/Comtrade/2-digit level yearly data/yr_2011.Rdata")

write.csv(yr2012, "Data/Comtrade/2-digit level yearly data/yr_2012.csv", row.names = FALSE)
save(yr2012, file = "Data/Comtrade/2-digit level yearly data/yr_2012.Rdata")

write.csv(yr2013, "Data/Comtrade/2-digit level yearly data/yr_2013.csv", row.names = FALSE)
save(yr2013, file = "Data/Comtrade/2-digit level yearly data/yr_2013.Rdata")

write.csv(yr2014, "Data/Comtrade/2-digit level yearly data/yr_2014.csv", row.names = FALSE)
save(yr2014, file = "Data/Comtrade/2-digit level yearly data/yr_2014.Rdata")

write.csv(yr2015, "Data/Comtrade/2-digit level yearly data/yr_2015.csv", row.names = FALSE)
save(yr2015, file = "Data/Comtrade/2-digit level yearly data/yr_2015.Rdata")

write.csv(yr2016, "Data/Comtrade/2-digit level yearly data/yr_2016.csv", row.names = FALSE)
save(yr2016, file = "Data/Comtrade/2-digit level yearly data/yr_2016.Rdata")

write.csv(yr2017, "Data/Comtrade/2-digit level yearly data/yr_2017.csv", row.names = FALSE)
save(yr2017, file = "Data/Comtrade/2-digit level yearly data/yr_2017.Rdata")

comtrade <- rbind(yr1999, yr2000, yr2001, yr2002, yr2003, 
                  yr2004, yr2005, yr2006, yr2007, yr2008, 
                  yr2009, yr2010, yr2011, yr2012, yr2013, 
                  yr2014, yr2015, yr2016, yr2017)

rm(yr1999, yr2000, yr2001, yr2002, yr2003, 
   yr2004, yr2005, yr2006, yr2007, yr2008, 
   yr2009, yr2010, yr2011, yr2012, yr2013, 
   yr2014, yr2015, yr2016, yr2017, 
   summary,
   nrow1999, nrow2000, nrow2001, nrow2002, 
   nrow2003, nrow2004, nrow2005, nrow2006, 
   nrow2007, nrow2008, nrow2009, nrow2010, 
   nrow2011, nrow2012, nrow2013, nrow2014, 
   nrow2015, nrow2016, nrow2017)

save(comtrade, file = "Data/Comtrade/comtrade.Rdata")