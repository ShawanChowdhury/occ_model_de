#libraries
library(tidyverse)
library(spOccupancy)
library(MCMCvis)
library(docopt)

### Setting parameters for the HPC #############################################
doc <- "usage: occ_model_fungi_spOccupancy.R <species> <output_dir>"
opts <- docopt(doc)

## read parameter file
species <- read.csv(opts$species)

## try to get task id
task <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))

print(paste("task:", task))

species_name <- species$species[[task]]

print(paste("species_name:", species_name))
print(paste("class(species_name):", class(species_name)))

### Get data #############################################
visit_data <- read_rds("data/complete_data_fungi.rds")

visit_data <- visit_data %>%
  group_by(visit, year_group, site) %>%
  summarize(nuSpecies = length(unique(species))) %>%
  ungroup()

# Metadata for each visit
# The species binary matric
occMatrix <- read_rds("data/occ_matrix_fungi.rds")
# Each row is a visit, while each column is data for a species

# Check the visit data fram and occ matrix align
all(visit_data$visit==row.names(occMatrix))
# This should be TRUE - if not, you need to rearrange the above objects.
# [sometimes, it might show 'False' even if they align perfectly, but the model
# will work as long as they have the same number of rows]

# Species data for this task
visit_data$Species <- occMatrix[,species_name]

# Checking presence and absence details
table(visit_data$Species)

### format ###########################################

#for spOccupancy we need to format the data into a matrix

# How many visits per site per year
visitSummary <- visit_data %>%
  group_by(site, year_group) %>%
  summarise(nuVisits = length(visit))

# Subsample at most 10 visits per year at any specific site
visit_data <- visit_data %>%
  group_by(site, year_group) %>%
  mutate(mySample = ifelse(length(visit)>10, 10, length(visit))) %>% 
  sample_n(.,size=unique(mySample)) %>%
  ungroup()

# Need to make visit be indexed from integer to numeric within each site, year, and month
visit_data <- visit_data %>%
  group_by(site, year_group) %>%
  mutate(visit = as.numeric(as.factor(visit)))%>%
  ungroup() 

visit_data$yearIndex <- as.numeric(visit_data$year_group)

# Make response into the matrix
y <- reshape2::acast(visit_data, site ~ yearIndex ~ visit,
                     value.var = "Species")
dim(y)

### Occupancy covariates #################################
siteDF <- data.frame(site = dimnames(y)[[1]])
yearMatrix <- expand.grid(site =siteDF$site,
                          yearIndex = unique(visit_data$yearIndex))

occ.covs <- list(site = as.numeric(as.factor(siteDF$site)),
                 year_group = reshape2::acast(yearMatrix, site ~ yearIndex, value.var = "yearIndex"))

### Detection covariates ##################################
visit_data$nuSpecies <- ifelse(visit_data$nuSpecies==1,"single",
                               ifelse(visit_data$nuSpecies %in% 2:3, "short", "long"))

det.covs <- list(year_group = reshape2::acast(visit_data, site ~ yearIndex ~ visit, value.var = "yearIndex"),
                 nuSpecies = reshape2::acast(visit_data, site ~ yearIndex ~ visit, value.var = "nuSpecies"))

data.list <- list(y = y, 
                  occ.covs = occ.covs, 
                  det.covs = det.covs)

print("data ready")

### Model setting ##########################################
# Setting priors
z.inits <- apply(y, c(1, 2), 
                 function(a){ max(a, na.rm = TRUE)}) 
z.inits[is.infinite(z.inits)] <- 0

all.inits <- list(beta = 0, # occurrence coefficients
                  alpha = 0, # detection coefficients 
                  sigma.sq.psi = 1, # occurrence random effect variances 
                  z = z.inits) # latent occurrence values

all.priors <- list(beta.normal = list(mean = 0, var = 2.72), 
                   alpha.normal = list(mean = 0, var = 2.72),
                   sigma.sq.psi.ig = list(a = 0.1, b = 0.1))

# Setting model syntax
n.chains <- 3
n.batch <- 1500
batch.length <- 100
(n.samples <- n.batch * batch.length) 
#n.samples <- 50000
n.burn <- n.samples*3/4
n.thin <- 30
ar1 <- FALSE
n.report <- 10000

#############################################
# Main model
det.formula <- ~ (1|year_group) + nuSpecies # Use the factor value of year
occ.formula <- ~ (1 - year_group) + (1|site) # Use the factor value of year

out <- tPGOcc(occ.formula = occ.formula,
              det.formula = det.formula,
              data = data.list,
              n.batch = n.batch,
              batch.length = batch.length,
              #n.samples = n.samples,
              inits = all.inits,
              priors = all.priors,
              ar1 = ar1,
              n.burn = n.burn,
              n.thin = n.thin,
              n.chains = n.chains,
              n.report = n.report)

# Exporting output
output_file <- write_rds(out, file = paste0("/work/chowdhus/ModelOutput/fungi/Modelsummary_", 
                                                species_name,".rds"))

# Extracting Rhat value
rhat <- out$rhat$beta

#############################################
# Model summary
#############################################
# waic
waicOcc <- waicOcc(out)

elpd <- waicOcc[1]
pd <- waicOcc[2]
waic <- waicOcc[3]

df <- as.data.frame(cbind(elpd, pd, waic))

#############################################
# Summary samples
psiCovs <- MCMCsummary(out$beta.samples)

rhat <- out$rhat$beta

# Merging model output
psiCovs <- psiCovs %>% 
  mutate(Rhat = rhat,
         elpd = df$elpd,
         pd = df$pD,
         waic = df$WAIC,
         species_name = species_name)

# Exporting output
output_file <- write.csv(psiCovs, file = paste0("/work/chowdhus/ModelOutput/fungi/MCMCsummary_", 
                                                species_name,".csv"))
