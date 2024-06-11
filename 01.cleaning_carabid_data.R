# Loading required libraries
library(dplyr)
library(tidyverse)
library(sf)

##############################
# GBIF data
# We initially decided to include the GBIF data, but then decided not to.
 
# ##################################################
# # Importing carabid data
# Earlier, using the intersect toolbar in ArcGIS, I grouped all the carabid distribution records
# into MTB quadrants. We can also do it in R, but ArcGIS is faster.
data <- st_read("data/layer/carabid_int/carabid_int.shp")
data$geometry <- NULL
data$mtb <- data$NAME

data <- data[,8:15]

# Removing duplicated records
data <- data[!duplicated(data),]

# Filtering the last 30 years of data
# Grouping data by years
data <- subset(data, year>=1988 & year<2024)

data_yr <- data %>%
  mutate(year_group=cut(year, seq(from = 1987, to = 2024, by = 2),
                        labels=c("1","2","3", "4",
                                 "5","6","7", "8",
                                 "9","10","11", "12",
                                 "13","14","15", "16", "17", "18")))

# Exporting data
write_csv(data_yr, "data/com_data.csv")

###############################################
com_data <- read_csv("data/com_data.csv")

# Median species occurrence records
sp_rec <- com_data %>% 
  group_by(species) %>% 
  summarise(n = NROW(species))

median(sp_rec$n) # 346
