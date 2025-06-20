# Loading libraries
library(tidyverse)

###########################################################
# Importing and combining data
###########################################################
# Importing revised data
complete_data <- read_csv("data/com_data.csv")
colnames(complete_data)[8] <- "site"

complete_data$lon <- as.numeric(complete_data$lon)
complete_data$lat <- as.numeric(complete_data$lat)
complete_data$month <- as.numeric(complete_data$month)
complete_data$year_group <- as.numeric(complete_data$year_group)
complete_data$day <- as.numeric(complete_data$day)

###########################################################
# Preparing data for model
###########################################################
# Add site information
# Add visit information [keep year here]
complete_data$date <- paste(complete_data$day, complete_data$month, complete_data$year, sep="-")

# Define a visit as site + date
complete_data$visit <- paste(complete_data$site, complete_data$date, sep="_")

# How many unique surveys (i.e., visits)?
surveys <- unique(complete_data[,c("visit","site","lon","lat","day","month","year_group")])

# How many grids were surveyed in at least two years?
# This is the threshold usually used in occupancy models
surveyYears <- surveys %>%
  group_by(site) %>%
  summarize(nuYears = length(unique(year_group))) 

# Do we have repeated surveys within years at same site?
# This is a prerequisite of occupancy-detection models
# at least some sites need to be visited at least twice within the year
surveySummary <- surveys %>%
  filter(site %in% surveyYears$site[surveyYears$nuYears>1]) %>%
  group_by(site, year_group) %>%
  count() %>%
  filter(n>1) 

# For sensitivity analysis, we are rerunning the model for species, which were
# observed at least five times.
# surveySummary <- surveys %>%
#   filter(site %in% surveyYears$site[surveyYears$nuYears>1]) %>%
#   group_by(site, year_group) %>%
#   count() %>%
#   filter(n>4) 

# Records per species
speciesSummary <- complete_data %>%
  filter(site %in% surveyYears$site[surveyYears$nuYears>1]) %>%
  group_by(species) %>%
  summarise(nuRecs = length(species),
            nuSites = length(unique(site)),
            nuYears = length(unique(year_group)))
summary(speciesSummary)

# Use sites visited in at least two years
complete_data <- complete_data %>% filter(site %in% surveyYears$site[surveyYears$nuYears>1]) 

# Remove very rare species
selectSpecies <- subset(speciesSummary, nuRecs > 49)

# Filtering species with > 49 records [need this list for HPC]
species_list <- selectSpecies[, 1]
write.csv(species_list, "data/species.csv")

# Exporting species list
write_csv(species_list, "data/species_list.csv")

# Combining dataframe
complete_data <- dplyr::left_join(selectSpecies, complete_data, by = "species")

# Exporting output
write_rds(complete_data, "data/complete_data_carabid_2yr.rds")

# Creating long list 
listlengthDF <- complete_data %>%
  group_by(visit, year_group, day, month, site) %>%
  summarize(nuSpecies = length(unique(species))) %>%
  ungroup()

# organize species matrix of detection-non detections in a given visit
occMatrix <- reshape2::acast(complete_data, visit ~ species, value.var="year_group", fun=length) #default to length
# occMatrix[1:10, 1:10]
occMatrix[occMatrix>1] <- 1 # make binary
occMatrix[is.na(occMatrix)] <- 0 

# check the detection - non detection data aligns with the visit data 
# [sometimes, it might show 'False' even if they align perfectly, but the model
# will work as long as they have the same number of rows]
all(row.names(occMatrix) == listlengthDF$visit)

# Exporting output
write_rds(occMatrix, "data/occ_matrix_carabid_2yr.rds")
