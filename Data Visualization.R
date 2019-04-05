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
library(sf) # For vector data
library(raster) # For raster data
library(spData)
library(spDataLarge)
library(tidyverse)



## ## ## ## ## ## ## ## ## ## ##
# CHOROPOLETHS              ####
## ## ## ## ## ## ## ## ## ## ##

world <- world
africa <- world %>% filter(continent == "Africa")
load("Results/Current Version/GER_Orig_Africa.Rdata")
