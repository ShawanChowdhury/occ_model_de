# Loading libraries
library(tidyverse)

###########################################################
# Importing and combining data
###########################################################
# Earlier, using the intersect toolbar in ArcGIS, I grouped all the carabid distribution records
# into MTB quadrants. We can also do it in R, but ArcGIS is faster.
# Import data
complete_data <- read_delim("data/carabid_mtb.txt")

# Data organising
# Changing coloumn names
colnames(complete_data)
colnames(complete_data)[3] <- "site"

# Selecting required variables
complete_data <- complete_data %>% 
  select(site, species, lon, lat, day, month, year, source)

# Exporting revised data [to manually check if everything looks okay]
write_csv(complete_data, "data/carabid_initial_data_up.csv")

# Importing revised data
complete_data <- read_csv("data/carabid_initial_data_up.csv")

# The number of species occurrence records is quite low for every year, so I am
# grouping them in 14-year study period
# Subset by year
complete_data <- subset(complete_data, year>=1986 & year<2022)

# Grouping data by years
complete_data <- complete_data %>%
  mutate(year_group=cut(year, seq(from = 1985, to = 2022, by = 12),
                        labels=c("1","2","3")))

complete_data$lon <- as.numeric(complete_data$lon)
complete_data$lat <- as.numeric(complete_data$lat)
complete_data$month <- as.numeric(complete_data$month)
complete_data$year_group <- as.numeric(complete_data$year_group)
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
#I have already assigned the MTB grid cells as site, so I don't need to create site information

# Add visit information [keep year here]
complete_data$date <- paste(complete_data$day, complete_data$month, complete_data$year_group, sep="-")

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

# Records per species
speciesSummary <- complete_data %>%
  filter(site %in% surveyYears$site[surveyYears$nuYears>1]) %>%
  group_by(species) %>%
  summarise(nuRecs = length(source),
            nuSites = length(unique(site)),
            nuYears = length(unique(year_group)))
summary(speciesSummary)

# use sites visited in at least two years
complete_data <- complete_data %>% filter(site %in% surveyYears$site[surveyYears$nuYears>1]) 

# Exporting output
write_rds(complete_data, "data/complete_data_carabid_12yr.rds")

# Creating long list 
listlengthDF <- complete_data %>%
  group_by(visit, year_group, day, site) %>%
  summarize(nuSpecies = length(unique(species))) %>%
  ungroup()

# organize species matrix of detection-non detections in a given visit
occMatrix <- reshape2::acast(complete_data, visit ~ species, value.var="year_group", fun=length) #default to length
occMatrix[1:10, 1:10]
occMatrix[occMatrix>1] <- 1 # make binary
occMatrix[is.na(occMatrix)] <- 0 

# check the detection - non detection data aligns with the visit data 
# [sometimes, it might show 'False' even if they align perfectly, but the model
# will work as long as they have the same number of rows]
all(row.names(occMatrix) == listlengthDF$visit)

# Exporting output
write_rds(occMatrix, "data/occ_matrix_carabid_12yr.rds")
