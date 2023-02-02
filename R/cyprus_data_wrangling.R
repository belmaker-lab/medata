library(tidyverse)
medata <- read_rds("data/medata.Rds")

cyp_data <- read_csv("data/Cyprus_Data_DecCoords.csv") %>% 
  mutate(lon = X, lat = Y) %>% 
  select(-X, -Y)
# Original data had coordinates in DMT format, I converted them to decimal using QGIS

# Fix NAs that are not recognised and remove rows with fundumental NAs:
cyp_data <- cyp_data %>% 
  mutate(across(.cols = everything(), .fns = ~na_if(., "#N/A"))) %>% 
  mutate(across(.cols = everything(), .fns = ~na_if(., "N/A")))

# Remove data that's not been gathered according to the protocol (not in rocky habitat)
cyp_data <- cyp_data %>% 
  filter(habitat %in% c("Rock", "Rocky"))
# 984 rows removed

# Correct seasons
cyp_data <- cyp_data %>% 
  mutate(date = lubridate::dmy(date)) %>% 
  mutate(season = case_when(lubridate::month(date) == 12 | 
                              lubridate::month(date) == 1 | 
                              lubridate::month(date) == 2 ~ "winter",
                            between(lubridate::month(date), left = 3, right = 5) ~ "spring",
                            between(lubridate::month(date), left = 6, right = 8) ~ "summer",
                            between(lubridate::month(date), left = 9, right = 11) ~ "autumn"))

# Correct reserves data
cyp_data <- cyp_data %>% 
  # Correct creation year
  rename(year_create = yr.creation) %>% 
  mutate(year = str_extract(year_create, "\\d\\d")) %>%
  mutate(yr.creation = as.integer(paste0("20", year))) %>% 
  # Correct reserve age column = survey_year - establish_year
  mutate(age.reserve.yr = if_else(yr.creation < lubridate::year(date), lubridate::year(date) - yr.creation, 0)) %>% 
  # Change enforcement: Okrotiri = 3 (no fishing regulations) &  Akamas = 2 (some fishing restrictions)
  mutate(enforcement = case_when(site == "Akrotiri" ~ 3,
                                 site == "Akamas" ~ 2,
                                 TRUE ~ as.numeric(enforcement)))

# Transect info corrections
cyp_data <- cyp_data %>%
  # Merge trans and trans2
  mutate(transect = coalesce(trans2, as.character(trans))) %>% 
  # Create Transect IDs without special characters, all lowercase + no spaces + distinction between depths
  mutate(trans_code = case_when(trans_code == "N4_R 15-30 m _T1" ~ "N4_R 15-30 m _T11",
                                trans_code == "N4_R 15-30 m _T2" ~ "N4_R 15-30 m _T21",
                                trans_code == "N4_R 15-30 m _T3" ~ "N4_R 15-30 m _T31",
                                trans_code == "CG6_R 0-5 m T3" & lat == 34.07540 ~ "CG6_R 0-5 m T300",
                                TRUE ~ as.character(trans_code))) %>% # specific changes
  mutate(unique_trans_id = snakecase::to_snake_case(trans_code)) %>% # case correction
  mutate(unique_trans_id = paste0(unique_trans_id, transect, depth, lubridate::year(date))) %>% # depth and year distinction
  mutate(unique_trans_id = str_remove_all(unique_trans_id, "[^[:alnum:]]"))
  

# Species corrections to Medata format
cyp_data <- cyp_data %>% 
  # Species names to Medata format
  mutate(species = snakecase::to_sentence_case(species)) %>% 
  mutate(species = str_replace_all(species, "sp", "spp")) %>%
  mutate(species = str_replace_all(species, "\\ ", "\\.")) %>% 
  # Correct specific species names
  mutate(species = case_when(species == "Sphyraena.chrysotaenia.flavicauda" ~ "Sphyraena.spp",
                             species == "Juveniles.unidentified" ~ NA_character_,
                             species == "Pteragogus.trisppilus" ~ "Pteragogus.trispilus",
                             TRUE ~ as.character(species))) %>% 
  filter(!is.na(species)) %>% # Removes the Juvenile.undentified species
  mutate(exotic = if_else(str_detect(species, "spp"), NA_character_, exotic)) %>%
  # Correct some families
  mutate(family = case_when(species == "Spicara.smaris" | 
                              species == "Spicara.maena" | 
                              species == "Spicara.spp" ~ "Sparidae",
                            species == "Tripterygion.melanurus" | 
                              species == "Tripterygion.delaisi" ~ "Tripterygiidae",
                            TRUE ~ as.character(family)))
# Removed 8 rows of "Juvenile.undentified" species

# Add environmental data (bio-oracle)
environment.layers <- sdmpredictors::load_layers(layercodes = c("BO_sstmean", # temperature
                                                                "BO2_salinitymean_ss",  # salinity
                                                                "BO22_ppmean_ss"), # productivity
                                                 datadir = "data/bio_oracle") 

cyp_data <- bind_cols(cyp_data, 
                      raster::extract(environment.layers, tibble(cyp_data[,9], cyp_data[,8]))) %>% 
  rename(tmean = BO_sstmean, sal_mean = BO2_salinitymean_ss, pp_mean = BO22_ppmean_ss)

# Add trophic data (fishbase)
species_list <- cyp_data %>% 
  distinct(species) %>% 
  mutate(species = str_replace(species, "\\.", "\\ ")) %>% 
  filter(!str_detect(species, "dae")) %>% 
  filter(!str_detect(species, "spp"))

species_diet <- rfishbase::ecology(species_list$species, 
                                   fields = c("Species", "FoodTroph", "FoodSeTroph"))

species_troph <- species_diet %>% 
  mutate(species = str_replace(Species, "\\ ", "\\.")) %>% 
  select(species, FoodTroph, FoodSeTroph)

cyp_data <- cyp_data %>% left_join(species_troph)

# Remove irrelevant columns and reorganise to the same format as medata
cyp_data <- cyp_data %>% 
  select(medata %>% colnames())

cyp_data %>% write_csv("data/cyp_data_clean.csv")

