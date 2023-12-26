# Loading libraries
library(tidyverse)

###########################################################
# Importing and combining data
###########################################################
# Import data
complete_data <- read_csv("data/carabid_initial_data.csv")

# Subset by year
complete_data <- subset(complete_data, year>=1982 & year<2022)

# # Grouping data by years
complete_data <- complete_data %>%
  mutate(year_range=cut(year, breaks=c(1982, 1991, 2001, 2011, 2021),
                        labels=c("1","2","3", "4")))

complete_data$lon <- as.numeric(complete_data$lon)
complete_data$lat <- as.numeric(complete_data$lat)
complete_data$month <- as.numeric(complete_data$month)
complete_data$year_range <- as.numeric(complete_data$year_range)
complete_data$day <- as.numeric(complete_data$day)

###########################################################
# Cleaning data
###########################################################
# Remove rows with missing data
complete_data <- complete_data %>% 
  filter(!is.na(lon)) %>%
  filter(!is.na(lat))

###########################################################
# Preparing data for model
###########################################################
# Add site information
#these help us organise the data for the occupancy model
complete_data$site <- paste(complete_data$lon, complete_data$lat, sep="-") # short cut
complete_data$date <- paste(complete_data$day, complete_data$month, complete_data$year_range, sep="-")

# Define a visit as site + date
complete_data$visit <- paste(complete_data$site, complete_data$date, sep="_")

#lets look at how many unique surveys (i.e., visits) we have
surveys <- unique(complete_data[,c("visit","site","lon","lat","day","month","year_range")])

# How many grids were surveyed in at least two years?
# This is the threshold usually used in occupancy models
surveyYears <- surveys %>%
  group_by(site) %>%
  summarize(nuYears = length(unique(year_range))) 

# Do we have repeated surveys within years at same site?
# This is a prerequisite of occupancy-detection models
# at least some sites need to be visited at least twice within the year
surveySummary <- surveys %>%
  filter(site %in% surveyYears$site[surveyYears$nuYears>1]) %>%
  group_by(site, year_range) %>%
  count() %>%
  filter(n>1) 

# Records per species
speciesSummary <- complete_data %>%
  filter(site %in% surveyYears$site[surveyYears$nuYears>1]) %>%
  group_by(species) %>%
  summarise(nuRecs = length(source),
            nuSites = length(unique(site)),
            nuYears = length(unique(year_range)))
summary(speciesSummary)

# use sites visited in at least two years
complete_data <- complete_data %>% filter(site %in% surveyYears$site[surveyYears$nuYears>1]) 

sub_data <- complete_data %>% 
  filter(species %in% c("Carabus nitens",
                        "Abax parallelus",
                        "Carabus convexus",
                        "Carabus arcensis",
                        "Agonum ericeti",
                        "Amara famelica",
                        "Amara infima",
                        "Poecilus versicolor",
                        "Harpalus rufipes",
                        "Abax parallelepipedus",
                        "Carabus auronitens",
                        "Bembidion lampros",
                        "Bembidion properans",
                        "Trechus quadristriatus",
                        "Poecilus cupreus",
                        "Zabrus tenebrioides",
                        "Pterostichus melanarius"
  ))

# Exporting output
write_rds(sub_data, "data/complete_data_carabid.rds")

# #lets decide which species we want to analyse - we have to remove very rare species
# selectSpecies <- subset(speciesSummary, nuRecs > 0)

# Exporting output
# write_rds(sub_data, "data/sub_data.rds")

# Creating long list 
listlengthDF <- sub_data %>%
  group_by(visit, year_range, day, site) %>%
  summarize(nuSpecies = length(unique(species))) %>%
  ungroup()

# organize species matrix of detection-non detections in a given visit
occMatrix <- reshape2::acast(sub_data, visit ~ species, value.var="year_range", fun=length) #default to length
occMatrix[1:10, 1:10]
occMatrix[occMatrix>1] <- 1 # make binary
occMatrix[is.na(occMatrix)] <- 0 

# check the detection - non detection data aligns with the visit data
all(row.names(occMatrix) == listlengthDF$visit)

write_rds(occMatrix, "data/occ_matrix_carabid.rds")

#######################################
