#libraries
library(tidyverse)
library(docopt)
library(data.table)

### Setting parameters for the HPC #############################################
doc <- "usage: gbif_insect_bias.R <output_dir>"

############################################
# Reading data file
#gbif_data <- read_delim(unzip("at.zip", "occurrence.txt"))

gbif_data <- fread("/gpfs1/work/chowdhus/gbif_insect/occurrence.txt")

NROW(gbif_data)

# Selecting relevant rows
gbif_data <- gbif_data %>% 
  select("class", "order", "family", "genus", "species", "decimalLongitude", 
         "decimalLatitude", "countryCode", "gbifID", "year", "month",
         "basisOfRecord", "institutionCode", "datasetName",
         "continent")

# Removing blank cells
gbif_data <- gbif_data[!(is.na(gbif_data$species) | gbif_data$species == ""),]
gbif_data <- gbif_data[!(is.na(gbif_data$decimalLongitude) | gbif_data$decimalLongitude == ""),]
gbif_data <- gbif_data[!(is.na(gbif_data$decimalLatitude) | gbif_data$decimalLatitude == ""),]

# Removing duplicated records
gbif_data <- gbif_data[!duplicated(gbif_data),]

###########################################
# Growth of citizen science and museum data
cit_sci_museum <- gbif_data %>% 
  filter(basisOfRecord %in% c("HUMAN_OBSERVATION",
                              "PRESERVED_SPECIMEN"))

gr_cit_sci_museum <- cit_sci_museum %>% 
  group_by(year, basisOfRecord) %>% 
  summarise(n = NROW(species)) %>% 
  ungroup()

# Exporting output
output_file <- write.csv(gr_cit_sci_museum, file = paste0("/work/chowdhus/gbif_insect/", 
                                                "growth_cit_sci_museum.csv.csv"))

###########################################
# Proportion of data by country
country_occ <- gbif_data %>% 
  select(countryCode) %>% 
  group_by(countryCode) %>% 
  summarise(occ_prop = n()/NROW(gbif_data))

n_species <- length(unique(gbif_data$species))

# Proportion of species by country
country_sp <- gbif_data %>% 
  select(species, countryCode) %>%
  unique() %>% 
  group_by(countryCode) %>% 
  summarise(sp_prop = n()/n_species)

# combining both datasets
country_sp_occ <- dplyr::left_join(country_occ, country_sp, by = "countryCode")

# Exporting output
output_file <- write.csv(country_sp_occ, file = paste0("/work/chowdhus/gbif_insect/", 
                                                          "country_sp_occ.csv"))
