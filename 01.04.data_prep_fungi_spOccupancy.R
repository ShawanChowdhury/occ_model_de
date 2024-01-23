# Loading libraries
library(tidyverse)

###########################################################
# Importing and combining data
###########################################################
# Earlier, using the intersect toolbar in ArcGIS, I grouped all the carabid distribution records
# into MTB quadrants. We can also do it in R, but ArcGIS is faster.
# Import data
load("H:/DriverAnalysis/occ_model_de/data/fungi_all.RData")

index_var <- grepl(" var. ",fungi.all$Vollname, fixed = T)
index_subsp <- grepl(" subsp. ",fungi.all$Vollname, fixed = T)

fungi.all <- fungi.all %>% 
  mutate(Species = case_when(
    index_var ~ paste(tstrsplit(Vollname, " ", fixed=T)[[1]],
                      tstrsplit(Vollname, " ", fixed=T)[[2]],
                      tstrsplit(Vollname, " ", fixed=T)[[3]],
                      tstrsplit(Vollname, " ", fixed=T)[[4]],
                      sep=" "),
    index_subsp ~ paste(tstrsplit(Vollname, " ", fixed=T)[[1]],
                        tstrsplit(Vollname, " ", fixed=T)[[2]],
                        tstrsplit(Vollname, " ", fixed=T)[[3]],
                        tstrsplit(Vollname, " ", fixed=T)[[4]],
                        sep=" "),
    .default = paste(tstrsplit(Vollname, " ", fixed=T)[[1]],
                     tstrsplit(Vollname, " ", fixed=T)[[2]],
                     sep=" ")))
head(fungi.all$Species,100)
unique(fungi.all$Species[grepl(" var. ",fungi.all$Species, fixed = T)])
unique(fungi.all$Species[grepl(" subsp. ",fungi.all$Species, fixed = T)])

###########################################
# Filtering species of interests
species_256 <- read_csv("data/species_256.csv")

fungi.all <- dplyr::left_join(species_256, fungi.all, by = "Species")

fungi.all$Vollname <- NULL
colnames(fungi.all)[1] <- "Vollname"

###########################################
# Changing grid details
names(fungi.all)[names(fungi.all)=="MTB"] <- "MTBQ"
fungi.all$MTB <- as.numeric(tstrsplit(fungi.all$MTBQ, ",", fixed=T)[[1]])
MTB.list <- sort(unique(fungi.all$MTB))
table(MTB.list)
tail(MTB.list) 

# the last 4 MTBs do not make sense
any(is.na(fungi.all$MTB)) #T
length(fungi.all$Vollname[is.na(fungi.all$MTB)])
# 646
length(fungi.all$Vollname[is.na(fungi.all$MTBQ)])
# 646

MTB_coord <- fread("data/MTB.txt")
str(MTB_coord)
MTB.list <- sort(unique(fungi.all$MTB))
tail(MTB.list)
index1 <- match(MTB.list,MTB_coord$MTB)
any(is.na(index1)) #T
MTB.list[is.na(index1)]
# 99
index2 <- match(fungi.all$MTB,MTB_coord$MTB)
any(is.na(index2)) #T
length(fungi.all$Vollname[is.na(index2)]) #2167
# have to be checked

fungi.all <- fungi.all %>% left_join(MTB_coord %>% 
                                       dplyr::select(MTB,X,Y), by="MTB")
length(unique(fungi.all$Vollname))
# 14624
head(sort(unique(fungi.all$Vollname)), 100)

fungi.all.richness.MTB3 <- fungi.all %>% filter(Funddatum>1900 & Funddatum< 2023) %>% 
  mutate(five_yr_interval=cut(Funddatum, breaks=seq(1900,2020, by=5))) %>% 
  group_by(Vollname,MTB,five_yr_interval) %>%
  summarize(n=length(Vollname)) %>%
  mutate(present_in_interval=1) %>%  
  group_by(Vollname,MTB,present_in_interval) %>%
  summarize(n=length(Vollname)) %>% 
  filter(n>=5) %>% 
  mutate(present_in_interval=1) %>%  
  group_by(Vollname) %>%
  summarize(n=length(Vollname)) %>% 
  filter(n>=20)

fungi.all.richness.MTB3
# 170 species occur in at least 100 MTBs and in at least 5 intervals

fungi.all.richness.MTB4 <- fungi.all %>% filter(Funddatum>1900 & Funddatum< 2023) %>% 
  mutate(five_yr_interval=cut(Funddatum, breaks=seq(1900,2020, by=5))) %>% 
  filter(Vollname %in% fungi.all.richness.MTB3$Vollname)
str(fungi.all.richness.MTB4)
length(unique(fungi.all.richness.MTB4$Vollname))
# 170  

fungi.all.richness.MTB4 <- fungi.all %>% filter(Funddatum>1900 & Funddatum< 2023) %>% 
  mutate(five_yr_interval=cut(Funddatum, breaks=seq(1900,2020, by=5))) %>% 
  filter(Vollname %in% fungi.all.richness.MTB3$Vollname) %>% 
  group_by(Vollname,MTB,five_yr_interval) %>%
  summarize(n=length(Vollname)) %>%
  mutate(present_in_interval=1) %>%  
  group_by(Vollname,MTB,present_in_interval) %>%
  summarize(n=length(Vollname)) %>% 
  filter(n>=5)  

fungi.all.richness.MTB5 <- fungi.all %>% filter(Funddatum>1900 & Funddatum< 2023) %>% 
  mutate(five_yr_interval=cut(Funddatum, breaks=seq(1900,2020, by=5))) %>% 
  filter(MTB %in% fungi.all.richness.MTB4$MTB) 
str(fungi.all.richness.MTB5)
# 3396728      16

fungi.all.richness.MTB5 <- fungi.all %>% filter(Funddatum>1900 & Funddatum< 2023) %>% 
  mutate(five_yr_interval=cut(Funddatum, breaks=seq(1900,2020, by=5))) %>% 
  filter(MTB %in% fungi.all.richness.MTB4$MTB) %>% 
  filter(!is.na(five_yr_interval)) %>% 
  group_by(Vollname,MTB,Funddatum) %>%
  summarize(n=length(Vollname)) %>% 
  #mutate(mid_interval=as.numeric(substr(five_yr_interval,2,5))) %>% 
  mutate(Visit=paste(MTB, Funddatum, sep="_"))

fungi.all.richness.MTB5
head(fungi.all.richness.MTB5$Visit)
length(unique(fungi.all.richness.MTB5$Vollname))
# 13888

#model1 <- lmer(n~mid_interval + (1|Vollname) + (1|MTB), data=fungi.all.richness.MTB5)
#summary(model1)
'Fixed effects:
               Estimate Std. Error         df t value Pr(>|t|)    
(Intercept)  -2.715e+01  2.933e-01  1.482e+06  -92.54   <2e-16 ***
mid_interval  1.417e-02  1.467e-04  1.578e+06   96.62   <2e-16 ***
'
# now run occupancy models from here
table(fungi.all.richness.MTB5$Funddatum)

fungi.all.richness.MTB6 <- fungi.all.richness.MTB5 %>% 
  mutate(five_yr_interval=cut(Funddatum, breaks=seq(1900,2020, by=5))) %>% 
  mutate(mid_interval=as.numeric(substr(five_yr_interval,2,5))) %>% 
  filter(mid_interval>=1970)


complete_data <- fungi.all.richness.MTB6

# Data organising
# Changing coloumn names
colnames(complete_data)
colnames(complete_data)[1] <- "species"
colnames(complete_data)[2] <- "site"
colnames(complete_data)[3] <- "year"
colnames(complete_data)[5] <- "visit"

# Grouping data by years
complete_data <- complete_data %>%
  mutate(year_group=cut(year, seq(from = 1900, to = 2020, by = 5)))

# How many unique surveys (i.e., visits)?
surveys <- unique(complete_data[,c("visit","site","year_group")])

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
write_rds(complete_data, "data/complete_data_fungi.rds")

# Creating long list 
listlengthDF <- complete_data %>%
  group_by(visit, year_group, site) %>%
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
write_rds(occMatrix, "data/occ_matrix_fungi.rds")
