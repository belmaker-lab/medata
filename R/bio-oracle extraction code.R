rm(list = ls())
.rs.restartR()
gc()

# Load package 
library(sdmpredictors)
library(tidyverse)

medata <- read_rds("data/medata.Rds")

# explore layers from Bio ORACLE
all_bio_layers <- sdmpredictors::list_layers(datasets = "Bio-ORACLE", marine = TRUE) %>%
  dplyr::select(layer_code, name) # to view layers names and code

# to search for a layer code by a specific string - change the 2nd arg in str_detect:
all_bio_layers %>% filter(stringr::str_detect(.$name, "temperature"))
all_bio_layers %>% filter(stringr::str_detect(.$name, "salinity"))
all_bio_layers %>% filter(stringr::str_detect(.$name, "Primary production"))

# Download environmental layers -------------------------------------------

my.sites <- medata %>% dplyr::select(site, lon, lat)

environment.layers <- sdmpredictors::load_layers(layercodes = c("BO_sstmean", # temperature
                                                 "BO2_salinitymean_ss",  # salinity
                                                 "BO22_ppmean_ss"), # productivity
                                                 datadir = "data/bio_oracle")
# # If there's an error with file downloading due to timeout limit, run the following code then attempt to download layers again:
# options(timeout = max(300, getOption("timeout")))

# Extract environmental values for sites in medata ------------------------

my.sites.environment <- bind_cols(my.sites, 
                                  raster::extract(environment.layers, my.sites[,2:3]))
my.sites.environment

# Check for NAs
my.sites.environment %>% 
  dplyr::filter(!complete.cases(.)) %>% 
  dplyr::distinct() %>% summarise(sites_with_NA = n())



