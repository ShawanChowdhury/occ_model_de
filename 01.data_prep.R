# Loading libraries
library(tidyverse)

###########################################################
# Importing and combining data
###########################################################
# Import data
data <- read_csv("data/adultData.csv")
var <- read_csv("data/variable_data.csv")

# Explore data
head(data)
head(var)

# Converting variable data to wide format
var_wide <- tidyr::pivot_wider(var,
                               names_from = "land_use",
                               values_from = "percentages",
                               values_fill = 0)

# Renaming col name
colnames(var_wide)[1] <- "MTB"

# Combining data
complete_data <- dplyr::left_join(data, var_wide, by = c("MTB"))

# Cleaning memory
rm("data", "var")

###########################################################
# Cleaning data
###########################################################
# Remove rows with missing data
complete_data <- complete_data %>% 
  filter(!is.na(lon)) %>%
  filter(!is.na(lat))

# Subset by year
sub_data <- subset(complete_data, Year>=2008 & Year<2018)

###########################################################
# Preparing data for model
###########################################################
# # Adding visit ID
# sub_data <- sub_data %>% 
#   group_by(Species, MTB, Month, Year, lon, lat) %>% 
#   mutate(Replicate = seq_along(Species))
# 
# # Only using data from the first three replicates (visit) [for initial run]
# sub_data <- sub_data %>% 
#   filter(Replicate %in% c("1", "2", "3"))

# Add site information
sub_data$site <- sub_data$MTB

# Define a visit as site + date
sub_data$visit <- paste(sub_data$site, sub_data$Date, sep="_")

#lets look at how many unique surveys (i.e., visits) we have
surveys <- unique(sub_data[,c("visit","site","lon","lat","yday","Month","Year")])

# How many grids were surveyed in at least two years?
# This is the threshold usuauly used in occupancy models
surveyYears <- surveys %>%
  group_by(site) %>%
  summarize(nuYears = length(unique(Year))) 

# Do we have repeated surveys within years at same site?
# This is a prerequisite of occupancy-detection models
# at least some sites need to be visited at least twice within the year
surveySummary <- surveys %>%
  filter(site %in% surveyYears$site[surveyYears$nuYears>1]) %>%
  group_by(site, Year) %>%
  count() %>%
  filter(n>1) 

# Records per species
speciesSummary <- sub_data %>%
  filter(site %in% surveyYears$site[surveyYears$nuYears>1]) %>%
  group_by(Species) %>%
  summarise(nuRecs = length(source),
            nuSites = length(unique(site)),
            nuYears = length(unique(Year)))
summary(speciesSummary)

# use sites visited in at least two years
sub_data <- sub_data %>% filter(site %in% surveyYears$site[surveyYears$nuYears>1]) 

# #lets decide which species we want to analyse - we have to remove very rare species
# selectSpecies <- subset(speciesSummary, nuRecs > 0)

# sub_data <- sub_data %>% 
#   filter(Species == "Aeshna cyanea")

# Exporting output
write_rds(sub_data, "data/sub_data.rds")

# Creating long list 
listlengthDF <- sub_data %>%
  group_by(visit, Year, yday, site) %>%
  summarize(nuSpecies = length(unique(Species))) %>%
  ungroup()

# organize species matrix of detection-non detections in a given visit
occMatrix <- reshape2::acast(sub_data, visit ~ Species, value.var="Year", fun=length) #default to length
occMatrix[1:10, 1:10]
occMatrix[occMatrix>1] <- 1 # make binary
occMatrix[is.na(occMatrix)] <- 0 

# check the detection - non detection data aligns with the visit data
all(row.names(occMatrix) == listlengthDF$visit)

write_rds(occMatrix, "data/occ_matrix.rds")
