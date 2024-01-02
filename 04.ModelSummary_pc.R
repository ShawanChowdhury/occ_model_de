#libraries
library(tidyverse)
library(spOccupancy)
library(MCMCvis)
library(docopt)

### get data #############################################
rds <- list.files(path = "outputs", pattern = "\\.rds$", recursive = TRUE, full.names = TRUE)
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
