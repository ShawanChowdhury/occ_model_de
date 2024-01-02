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
sp <- rds[stringr::str_detect(rds, species)]

out <- read_rds(sp)

####################################
# waic
waicOcc <- as.data.frame(waicOcc(out))
colnames(waicOcc) <- "val"

waicOcc <- waicOcc %>% 
  mutate(param = rownames(waicOcc))

waicOcc <- waicOcc %>% 
  pivot_wider(names_from = param, values_from = val)

####################################
#summary samples
psiCovs <- MCMCsummary(out$beta.samples)

rhat <- as.data.frame(out$rhat$beta)
colnames(rhat) <- "rhat"

# Merging model output
psiCovs <- psiCovs %>% 
  mutate(Rhat = rhat$rhat,
         elpd = waicOcc$elpd,
         pd = waicOcc$pD,
         waic = waicOcc$WAIC,
         species_name = species_name)

output_file <- write_csv(out, file = paste0("/work/chowdhus/ModelSummary_", 
                                          species_name,".csv"))
