#libraries
library(tidyverse)
library(spOccupancy)
library(MCMCvis)
library(docopt)

doc <- "usage: 04.ModelSummary.R <species> <output_dir>"
opts <- docopt(doc)

## read parameter file
species <- read.csv(opts$species)

## try to get task id
task <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))

print(paste("task:", task))

species_name <- species$species[[task]]

print(paste("species_name:", species_name))
print(paste("class(species_name):", class(species_name)))

### get data #############################################
rds <- list.files(path = "/gpfs1/work/chowdhus/03.occ_model_up_carabid/outputs/", pattern = "\\.rds$", recursive = TRUE, full.names = TRUE)
sp <- rds[stringr::str_detect(rds, species_name)]

out <- read_rds(sp)

print(sp)


####################################
# waic
waicOcc <- waicOcc(out)

elpd <- waicOcc[1]
pd <- waicOcc[2]
waic <- waicOcc[3]

df <- as.data.frame(cbind(elpd, pd, waic))

####################################
#summary samples
psiCovs <- MCMCsummary(out$beta.samples)

rhat <- out$rhat$beta

# Merging model output
psiCovs <- psiCovs %>% 
  mutate(Rhat = rhat,
         elpd = df$elpd,
         pd = df$pD,
         waic = df$WAIC,
         species_name = species_name)

output_file <- write.csv(out, file = paste0("/work/chowdhus/ModelSummary_", 
                                          species_name,".csv"))
