# Loading required libraries
library(tidyverse)
library(data.table)
library(lmerTest)
library(nimble)  # occupancy models
library(ggmcmc)
library(MCMCvis)
library(docopt)

### Setting parameters for the HPC #############################################
doc <- "usage: 02.03.occ_model_fungi.R <species_fungi> <output_dir>"
opts <- docopt(doc)

## read parameter file
species_fungi <- read.csv(opts$species_fungi)

## try to get task id
task <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))

print(paste("task:", task))

species_name <- species_fungi$species_fungi[[task]]

print(paste("species_name:", species_name))
print(paste("class(species_name):", class(species_name)))

### Get data #############################################
fungi.all.richness.MTB6 <- read_rds("data/complete_data_fungi.rds")

listlengthDF <- fungi.all.richness.MTB6 %>%
  group_by(Visit, mid_interval, MTB) %>%
  summarize(nuSpecies = length(unique(Vollname))) %>%
  ungroup() %>% 
  #bugs/nimble likes indicies that start at 1, rather than characters...
  mutate(siteIndex = as.numeric(as.factor(MTB))) %>% 
  mutate(yearIndex = as.numeric(as.factor(mid_interval)))

# Metadata for each visit
# The species binary matric
occMatrix <- read_rds("data/occ_matrix_fungi.rds")
# Each row is a visit, while each column is data for a species

# Check the visit data fram and occ matrix align
all(occMatrix$Visit == listlengthDF$Visit)

# #lets start here with the first species
# # Species data for this task
# listlengthDF <- listlengthDF %>% 
#   filter(Vollname == species_name)
# 
# occMatrix <- occMatrix[,species_name]

#all the covariates, indicies and constants
nimble_const <- list(nsite = length(unique(listlengthDF$MTB)),
                     nyear = length(unique(listlengthDF$mid_interval)),
                     #day = lengthDF$day, #needs cleaning
                     #day2 = lengthDF$day^2, # needs cleaning
                     nvisit = nrow(listlengthDF),
                     site = listlengthDF$siteIndex,
                     year = listlengthDF$yearIndex)

#the detection-non detection data for our focal species
nimble_data <- list(y = occMatrix %>% dplyr::select(all_of(species_name)) %>% 
                      unlist()) 

# initial values ----------------------------

#for the z (latent occurrence)
#the best estimate is just whether our focal species was ever seen or not in each site/year
listlengthDF$zst <- occMatrix %>% dplyr::select(all_of(species_name)) %>% 
  unlist()
str(listlengthDF)
zst <- reshape2::acast(listlengthDF, MTB ~ mid_interval, value.var = "zst", fill=0)
zst[zst>1] <- 1
nimble_inits <- list(z = zst)

# model code -------------------------------

modelcode <- nimbleCode({
  
  # State model - model of true occurrence
  for (i in 1:nsite){ 
    for (t in 1:nyear){   
      z[i,t] ~ dbern(psi[i,t]) 
      logit(psi[i,t])<- a[t] + eta[i] # year + site - see priors below
    }
  }   
  
  # Observation Model - model of observed data
  for(j in 1:nvisit) {
    y[j] ~ dbern(Py[j])
    Py[j]<- z[site[j],year[j]]*p[j] # observation = occurrence * detection
    
    #simple model with just overall intercept, i.e., assumes constant detection probability
    logit(p[j]) <-  intercept.p 
    
    #model with quadrative day of year effect
    #logit(p[j]) <-  intercept.p + day[j] * dayEffect  + day2[j] * day2Effect
    #eventually we want to add here, method, purpose, maybe nu species etc..
    
  } 
  
  #We need to give priors for all the coefficients we specify in the above models
  
  # State model priors
  
  #random walk prior for the year effect
  #this can be removed, but it is typically used at CEH
  #i.e., if we have no data, our best guess is the occupancy in the year before
  a[1] ~ dnorm(0, 0.001)
  
  for(t in 2:nyear){
    a[t] ~ dnorm(a[t-1], tau.a)
  }
  sd.a ~ T(dt(0, 1, 1), 0, )
  tau.a <- pow(sd.a, -2)
  
  #random site effect - normally distributed random effect
  for(i in 1:nsite){
    eta[i] ~ dnorm(0, tau.eta)
  }
  sd.eta ~ T(dt(0, 1, 1), 0, )
  tau.eta <- pow(sd.eta, -2)
  
  # Observation model priors 
  
  mean.p ~ dunif(0,1)
  intercept.p <- logit(mean.p)
  
  #when we expand the model
  #dayEffect ~ dnorm(0,0.01)
  #day2Effect ~ dnorm(0,0.01)
  
  # Derived parameters - prop of occuppied sites in each year
  for (t in 1:nyear) {  
    psi.fs[t] <- sum(z[1:nsite, t])/nsite
  } 
  #this is calculated based on the predictions of the above models
  
})


# run model -------------------------------------

out <- nimble::nimbleMCMC(code = modelcode, 
                          constants = nimble_const, data = nimble_data, 
                          inits = nimble_inits, nchains = 3, 
                          niter = 50000, nburnin = 40000, thin = 30, 
                          samplesAsCodaMCMC = TRUE, 
                          monitors = c("mean.p","psi.fs"))

saveRDS(out, file = paste0("/work/chowdhus/ModelOutput/fungi/ModelOutput_", 
                                     species_name,".rds"))

#lets just save the summary for now
MCMCsummary <- MCMCsummary(object = out, round = 4)

#first row is the estimated detection probability
#remaining rows are estimates of the proportion of occupied sites in each year
saveRDS(MCMCsummary, file = paste0("/work/chowdhus/ModelOutput/fungi/MCMCsummary_", 
                                   species_name,".rds"))
