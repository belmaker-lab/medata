library(magrittr)
library(tidyverse)

# The original data used here has been gathered and cleaned by Ori Frid and Shira Salingre (Balmaker lab).
# This script is updated each time the whole dataset requires a correction or addition.

medata <- read_csv("data/uvc_data_19022020.csv", col_types = cols(depth = "d"))
summary(medata)

# Correct seasons ---------------------------------------------------------

# Season exhibits 4 levels, 2 of which are the same (`Fall` and `Autumn`)

medata %>% distinct(season)

medata <- medata %>% mutate(season = if_else(season == "Fall", "Autumn", season))
as.factor(medata$season) %>% levels()

# Convert case ------------------------------------------------------------

medata <- medata %>% mutate(site = str_to_lower(site), season = str_to_lower(season))

# Remove whitespaces ------------------------------------------------------

# Species names
medata <- medata %>% mutate(species = str_trim(species, "both"))

# Sites (plus remove an unneeded `,` in one of the site names):
medata <- medata %>% mutate(site = str_replace_all(site, "[[:space:]]", "\\_"), site = str_remove_all(site, "\\,"))


# MPAs --------------------------------------------------------------------

# Change `protection` to logical
table(medata$protection) # just to make sure it works fine
medata <- medata %>% mutate(protection = if_else(protection == "NO", FALSE, TRUE))
table(medata$protection)

# Sites that are outside of MPAs (protection == FALSE) should have `total.mpa.ha` and `size.notake` set to NA
medata <- medata %>% mutate(total.mpa.ha = ifelse(protection == FALSE & total.mpa.ha == 0, NA, total.mpa.ha),
                              size.notake = ifelse(protection == FALSE & size.notake == 0, NA, size.notake))


# Depth -------------------------------------------------------------------
# Fix depth data from three transects in Crete, where there are 2 depth values:

# First find them:
medata %>%
  dplyr::select(site, lon, lat, trans, depth) %>%
  distinct() %>% 
  count(site,lon,lat,trans) %>%
  arrange(desc(n))

# ...Check them out...
medata %>%
  dplyr::select(site, lon, lat, trans, depth) %>%
  distinct() %>%
  filter(site == "assecret2210191mlsc_a" | site == "assecret2210191mlsc_b" | site == "assecret2210191mlsc_c") %>% 
  arrange(site)

# Then fix them
medata[which(medata$site == "assecret2210191mlsc_a"),]$depth %<>% mean()
medata[which(medata$site == "assecret2210191mlsc_b"),]$depth %<>% mean()
medata[which(medata$site == "assecret2210191mlsc_c"),]$depth %<>% mean()


# Add species information -------------------------------------------------
# data from FishBase (see 'R/fishbase_info_addition.R')


## Create a species information dataset ------------------------------------
species_list <- medata %>% 
  distinct(species) %>% 
  mutate(species = str_replace(species, "\\.", "\\ ")) %>% 
  filter(!str_detect(species, "dae")) %>% 
  filter(!str_detect(species, "spp"))

species_list %>% arrange(species) %>% print(n = Inf)


## Add trophic information -------------------------------------------------

species_diet <- rfishbase::ecology(species_list$species, 
                        fields = c("Species", "DietTroph", "DietSeTroph", "FoodTroph", "FoodSeTroph"))

# Species with missing FoodTroph also have missing DietTroph (except Parablennius zvonimiri)
# so we'll keep 'FoodTroph' as it has less NAs (more information)
species_troph <- species_diet %>% 
  mutate(species = str_replace(Species, "\\ ", "\\.")) %>% 
  select(species, FoodTroph, FoodSeTroph)

medata <- medata %>% left_join(species_troph)

## Add family --------------------------------------------------------------

species_family <- rfishbase::load_taxa() %>% 
  as_tibble() %>% 
  filter(Species %in% species_list$species) %>%
  select(Species, Family) %>% 
  mutate(species = str_replace(.$Species, pattern = "\\ ", "\\.")) %>% 
  select(species, family = Family)

medata <- medata %>% left_join(species_family)

# Manually add families where applicable:
medata %>% filter(is.na(family)) %>% distinct(species) %>% arrange(species)

medata <- medata %>% mutate(family = case_when(str_detect(species, "Symphodus") ~ "Labridae",
                                               species == "Atherina.spp" ~ "Atherinidae",
                                               species == "Belonidae" ~ "Belonidae",
                                               species == "Clupeidae" ~ "Clupeidae",
                                               species == "Labridae" ~ "Labridae",
                                               species == "Liza.aurata" ~ "Mugilidae",
                                               species == "Mullus.barbatus" ~ "Mullidae",
                                               str_detect(species, "Tripterygion") ~ "Tripterygiidae",
                                               TRUE ~ as.character(family)))

# Lessepsian migrants -----------------------------------------------------

indie_species <- read_csv("data/exotic_species.csv")

medata <- medata %>% left_join(indie_species)

# Add Length-Weight ratio constants ---------------------------------------
# relevant columns 'a' and 'b'
rfishbase::length_weight(species_list$species) %>% colnames()

missing_constants <- medata %>%
  filter(is.na(a)) %>%
  filter(is.na(b)) %>% 
  distinct(species) %>% 
  mutate(species = str_replace(.$species, pattern = "\\.", "\\ "))

species_LW <- rfishbase::length_weight(missing_constants$species, 
                            fields = c("Species", "a", "b", "Type", "Method")) %>% 
  filter(Method == "type I linear regression" & Type == "TL" | Method == "Type I linear regression" & Type == "TL") %>% 
  group_by(Species) %>% summarise(mean_a = mean(a), mean_b = mean(b)) %>% ungroup() %>% 
  mutate(species = str_replace(.$Species, pattern = "\\ ", "\\.")) %>% 
  select(-Species)

# Add to medata

medata <- medata %>% 
  left_join(species_LW) %>% 
  mutate(a = if_else(is.na(a), mean_a, a),
         b = if_else(is.na(b), mean_b, b)) %>% 
  select(-mean_a, -mean_b)


# Better site names for Crete expedition ----------------------------------
# The Crete 'sites' are actually transects here. Separate them

medata <- medata %>% 
  mutate(site = case_when(str_starts(site, "assecret") ~ 
                            str_extract(site, "assecret\\d\\d\\d\\d\\d\\d\\d"),
                          TRUE ~ as.character(site)))


# Add environmental data --------------------------------------------------

# Remove pre-existing environmental data (if needed)
medata %>% colnames()
medata <- medata %>% select(-c("tmean", "trange", "sal_mean", "pp_mean", "pp_range"))

gc() # clear the memory for handling large files

environment.layers <- sdmpredictors::load_layers(layercodes = c("BO_sstmean", # temperature
                                                                "BO2_salinitymean_ss",  # salinity
                                                                "BO22_ppmean_ss"), # productivity
                                                 datadir = "data/bio_oracle") 
raster::plot(environment.layers)

medata <- bind_cols(medata, 
                    raster::extract(environment.layers, medata[,4:5]))

# Add unique transect identifier for each observation ---------------------

medata <- medata %>% 
  mutate(unique_trans_id = str_remove_all(paste0(site, trans, lon, lat, depth), "\\."))

# Final checks and save ---------------------------------------------------

skimr::skim(medata)
summary(medata)

medata <- medata %>% 
  mutate(across(.cols = c("data.origin", "country", "season", "site", "enforcement", "yr.creation", 
                          "age.reserve.yr", "species", "family"), .fns = as.factor)) %>% 
  select(data.origin, country, season, lon, lat, site, trans, unique_trans_id, 
         protection, enforcement, total.mpa.ha, size.notake, yr.creation, age.reserve.yr,
         depth, tmean = BO_sstmean, sal_mean = BO2_salinitymean_ss, pp_mean = BO22_ppmean_ss, 
         species, sp.n, sp.length, a, b, family, exotic, FoodTroph, FoodSeTroph)


write_rds(medata, "data/medata.Rds")






