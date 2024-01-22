# In addition to the main model, I also ran this script in the HPC. I used the same shell script;
# however, I changed the file name and memory usage to make it compatible.

# Libraries
library(tidyverse)
library(spOccupancy)
library(MCMCvis)
library(docopt)

### Setting parameters for the HPC #############################################
doc <- "usage: 04.model_summary.R <species> <output_dir>"
opts <- docopt(doc)

## Read parameter file
species <- read.csv(opts$species)

## Try to get task id
task <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))

print(paste("task:", task))

species_name <- species$species[[task]]

print(paste("species_name:", species_name))
print(paste("class(species_name):", class(species_name)))

### Get data #############################################
rds <- list.files(path = "/gpfs1/work/chowdhus/03.occ_model_up_carabid/outputs", pattern = "\\.rds$", recursive = TRUE, full.names = TRUE)
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
output_file <- write.csv(psiCovs, file = paste0("/work/chowdhus/ModelSummary_", 
                                          species_name,".csv"))

##########################################
# Model summary [Fungi]
sp <- read_csv("data/species.csv")

# Importing the species file
species <- unique(sp$species)

# Importing model outputs [rds files]
rds <- list.files(path = "output/fungi/", 
                  pattern = "\\.rds$", recursive = TRUE, full.names = TRUE)
rds <- rds[stringr::str_detect(rds, "MCMCsummary_")]

# Creating an empty dataframe to merge the outputs
df <- data.frame()

for (i in species) {
  print(i)
  
  sp_rds <- rds[stringr::str_detect(rds, i)]
  
  sp_rds <- read_rds(sp_rds)
  
  sp_rds_sum <- sp_rds %>% 
    mutate(species = i, Rhat = mean(Rhat, na.rm = TRUE))
  
  sp_rds_sum <- sp_rds_sum[1,]
  
  df <- rbind(df, sp_rds_sum)
  
}

# I don't need the rownames, so I am removing those.
rownames(df) <- NULL

# Export output
write_csv(df, "output/model_summary_fungi_mean.csv")

