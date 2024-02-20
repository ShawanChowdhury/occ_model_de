# Loading required libraries
library(dplyr)
library(ggplot2)
library(countrycode)
library(CoordinateCleaner)
library(tidyverse)
library(rworldmap)
##############################
# Cleaning GBIF data
# Reading GBIF data
gbif_data <- read_delim("data/gbif/occurrence.txt")

# Selecting columns of interest
gbif_data <- gbif_data %>% 
  select("species", "decimalLongitude", "decimalLatitude", "year", "month", "day")

# Removing blank cells
gbif_data <- gbif_data[!(is.na(gbif_data$species) | gbif_data$species == ""),]
gbif_data <- gbif_data[!(is.na(gbif_data$decimalLongitude) | gbif_data$decimalLongitude == ""),]
gbif_data <- gbif_data[!(is.na(gbif_data$decimalLatitude) | gbif_data$decimalLatitude == ""),]
gbif_data <- gbif_data[!(is.na(gbif_data$year) | gbif_data$year == ""),]
gbif_data <- gbif_data[!(is.na(gbif_data$month) | gbif_data$month == ""),]

# Removing duplicated records
gbif_data <- gbif_data[!duplicated(gbif_data),]

# Renaming columns
colnames(gbif_data) <- c("species", "lon", "lat", "year", "month", "day")

# Exporting output
write_csv(gbif_data, "data/gbif_data_cleaned.csv")

##################################################
# Importing carabid data
gbif_data <- read_csv("data/gbif_data_cleaned.csv")

data <- read_csv("data/carabid_initial_data_up.csv")
data <- data[,2:7]

# Combining both data frames by species
com_data <- dplyr::left_join(data, gbif_data, by = c("species", "lon", "lat", "year", "month", "day"))

# Removing duplicated records
com_data <- com_data[!duplicated(com_data),]

# Filtering the last 30 years of data
# Grouping data by years
com_data <- subset(com_data, year>=1992 & year<2022)

com_data <- com_data %>%
  mutate(year_group=cut(year, seq(from = 1991, to = 2022, by = 2),
                        labels=c("1","2","3", "4",
                                 "5","6","7", "8",
                                 "9","10","11", "12",
                                 "13","14","15")))

# Exporting data
write_csv(com_data, "data/com_data.csv")

###############################################
# Median species occurrence records
sp_rec <- com_data %>% 
  group_by(species) %>% 
  summarise(n = NROW(species))
