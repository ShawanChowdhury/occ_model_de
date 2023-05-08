# Loading libraries
library(tidyverse)
library(spOccupancy)
library(coda)
library(stars)
library(ggplot2)

# Import data
data <- read_csv("data/test_data.csv")

# Explore data
head(data)

# Remove rows with missing data
data <- data %>% 
  filter(!is.na(lon)) %>%
  filter(!is.na(lat)) 

# Subset by year
data <- subset(data, Year>=2011 & Year<2022)

# Adding PA data [ manipulation]
data$pa <- rnorm(NROW(data), mean = data$yday, sd = 1)
data$pa = as.numeric(as.factor(data$pa))

# Add site information
data$site <- data$MTB

# Define a visit as site + date
data$visit <- paste(data$site, data$Date, sep="_")

#lets look at how many unique surveys (i.e., visits) we have
surveys <- unique(data[,c("visit","site","lon","lat","yday","Month","Year")])

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
speciesSummary <- data %>%
  filter(site %in% surveyYears$site[surveyYears$nuYears>1]) %>%
  group_by(Species) %>%
  summarise(nuRecs = length(source),
            nuSites = length(unique(site)),
            nuYears = length(unique(Year)))
summary(speciesSummary)

# use sites visited in at least two years
data <- data %>% filter(site %in% surveyYears$site[surveyYears$nuYears>1]) 

listlengthDF <- data %>%
  group_by(visit, Year, yday, site) %>%
  summarize(nuSpecies = length(unique(Species))) %>%
  ungroup()

# organize species matrix of detection-non detections in a given visit
occMatrix <- reshape2::acast(data, visit ~ Species, value.var="Year", fun=length) #default to length
occMatrix[1:10, 1:10]
occMatrix[occMatrix>1] <- 1 # make binary
occMatrix[is.na(occMatrix)] <- 0 

# check the detection - non detection data aligns with the visit data
all(row.names(occMatrix) == listlengthDF$visit)

# bugs/nimble likes indicies that start at 1, rather than characters...
listlengthDF$siteIndex = as.numeric(as.factor(listlengthDF$site))
listlengthDF$yearIndex = as.numeric(as.factor(listlengthDF$Year))

# adding PA information [manipulation]
listlengthDF$pa <- rnorm(NROW(listlengthDF), mean = listlengthDF$nuSpecies, sd = 1)
listlengthDF$pa = as.numeric(as.factor(listlengthDF$pa))

# get other detection covariates
yday <- reshape2::acast(data, visit ~ Species, value.var="yday", fun=length)

#site covs
siteCovs <- data %>%
  dplyr::select(site, lon, lat, pa) %>%
  filter(!duplicated(site)) %>%
  arrange(site)

#Package up
data.list <- list(y = occ_data,
                  occ.covs = siteCovs,
                  det.covs = list(yday = yday),
                  coords = siteCovs[,c("lon","lat")])

#######################################
# Priors
prior.list <- list(beta.normal = list(mean = 0, var = 2.72),
                   alpha.normal = list(mean = 0, var = 2.72))

# Initial values
inits.list <- list(alpha = 0, beta = 0,
                   z = apply(occMatrix, 1, max, na.rm = TRUE))

# Settings
n.samples <- 10000
n.report <- 1000

# Run model - non spatial
out <- PGOcc(occ.formula = ~ site+lon+lat+pa,
             det.formula = ~ yday,
             data = data.list,
             inits = inits.list,
             n.samples = n.samples,
             priors = prior.list,
             n.omp.threads = 1,
             verbose = TRUE,
             n.report = n.report,
             n.burn = n.samples/2,
             n.thin = n.samples/10,
             n.chains = 3)

summary(out)