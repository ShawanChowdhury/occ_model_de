# Identifying if we have all the required R packages
list.of.packages <- c("RODBC", "tidyverse", "data.table", "lmerTest", 
                      "nimble", "ggmcmc", "MCMCvis")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Loading required packages
lapply(list.of.packages, require, character.only=TRUE)

# Cleaning memory
rm(list = ls())

# Importing data
load("H:/DriverAnalysis/occ_model_de/data/fungi_all.RData")

length(unique(fungi.all$Vollname))
# 14624
head(sort(unique(fungi.all$Vollname)), 100)

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
species_131 <- read_csv("data/species_131.csv")

fungi.all <- dplyr::left_join(species_131, fungi.all, by = "Species")

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
  filter(n>=50)

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


#records per species
speciesSummary <- fungi.all.richness.MTB6 %>%
  group_by(Vollname) %>%
  summarise(nuRecs = length(Vollname),
            nuSites = length(unique(MTB)),
            nuYears = length(unique(mid_interval)))
summary(speciesSummary)

#lets decide which species we want to analyse - we have to remove very rare species
selectSpecies <- speciesSummary %>%  filter(nuRecs > 50)

# Revising Fungi data
fungi.all.richness.MTB6 <- dplyr::right_join(fungi.all.richness.MTB6, selectSpecies,
                                             by = "Vollname")

# Exporting output
write_rds(fungi.all.richness.MTB6, "data/complete_data_fungi.rds")

listlengthDF <- fungi.all.richness.MTB6 %>%
  group_by(Visit, mid_interval, MTB) %>%
  summarize(nuSpecies = length(unique(Vollname))) %>%
  ungroup() %>% 
  #bugs/nimble likes indicies that start at 1, rather than characters...
  mutate(siteIndex = as.numeric(as.factor(MTB))) %>% 
  mutate(yearIndex = as.numeric(as.factor(mid_interval)))

occMatrix <- fungi.all.richness.MTB6 %>% 
  pivot_wider(id_cols = Visit, names_from = Vollname, names_sort = T, 
              values_from = n, values_fill = 0) %>% 
  arrange(Visit)
#all(row.names(occMatrix) == listlengthDF$Visit)
#head(row.names(occMatrix),10)
all(occMatrix$Visit == listlengthDF$Visit) #T
#head(occMatrix$Visit,20)

# Exporting output
write_rds(occMatrix, "data/occ_matrix_fungi.rds")
