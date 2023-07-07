#libraries
library(tidyverse)
library(sf)
library(tmap)
library(ggthemes)
library(spOccupancy)
library(data.table)
library(MCMCvis)

### get data #############################################

myfolder <- "/data/idiv_ess/carabids/" # HPC folder

# this is like your sub_data:
visit_data <- read_rds("data/sub_data.rds")

visit_data <- visit_data %>%
  group_by(visit, Year, yday, site, Month) %>%
  summarize(nuSpecies = length(unique(Species))) %>%
  ungroup()

#metadata for each visit

#the species binary matric
occMatrix <- read_rds("data/occ_matrix.rds")
#each row is a visit, while each column is data for a species

#check the visit data fram and occ matrix align
all(visit_data$visit==row.names(occMatrix))
#this should be TRUE - if not, you need to rearrange the above objects

### species #######################################

# #get all species
# allspecies <- colnames(occMatrix)
# 
# #decide focal species for this task
# task.id = as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID", "1"))
# myspecies = allspecies[task.id]
# print(myspecies)

myspecies <- "Aeshna cyanea"
  
#add on the focal species data for this task
visit_data$Species <- occMatrix[,myspecies]

# Checking presence and absence details
table(visit_data$Species)

### format ###########################################

#for spOccupancy we need to format the data into a matrix

#how many visits per site per year
visitSummary <- visit_data %>%
   group_by(site, Year) %>%
   summarise(nuVisits = length(visit))

sub <- visit_data %>% 
  filter(Year == 2013 & site == 916)

#some sampled up to 174 times!!
#subsample at most 10 visits per year at any specific site
visit_data <- visit_data %>%
   group_by(site, Year) %>%
   mutate(mySample = ifelse(length(visit)>10, 10, length(visit))) %>% 
   sample_n(.,size=unique(mySample)) %>%
   ungroup()

#need to make visit be indexed from i to n within each site, year, and month
visit_data <- visit_data %>%
  group_by(site, Year) %>%
  mutate(visit = as.numeric(as.factor(visit)))%>%
  ungroup() 

visit_data$yearIndex <- as.numeric(visit_data$Year)
visit_data$monthIndex <- as.numeric(visit_data$Month)

#make response into the matrix
y <- reshape2::acast(visit_data, site ~ yearIndex ~ visit,
                     value.var = "Species")
dim(y)

### occ covariates #################################

#occupancy  covariats
siteDF <- data.frame(site = dimnames(y)[[1]])
yearMatrix <- expand.grid(site =siteDF$site,
                          yearIndex = unique(visit_data$yearIndex))

occ.covs <- list(site = as.numeric(as.factor(siteDF$site)),
                 year = reshape2::acast(yearMatrix, site ~ yearIndex, value.var = "yearIndex"))

### det covariates ##################################

#detection covariates
# visit_data$monthIndex <- as.numeric(as.factor(visit_data$Month))
# visit_data$yearIndex <- as.numeric(as.factor(visit_data$Year))
visit_data$nuSpecies <- ifelse(visit_data$nuSpecies==1,"single",
                               ifelse(visit_data$nuSpecies %in% 2:3, "short", "long"))

det.covs <- list(month = reshape2::acast(visit_data, site ~ yearIndex ~ visit, value.var = "monthIndex"),
                 year = reshape2::acast(visit_data, site ~ yearIndex ~ visit, value.var = "yearIndex"),
                 nuSpecies = reshape2::acast(visit_data, site ~ yearIndex ~ visit, value.var = "nuSpecies"))

# coords <- as.matrix(unique(visit_data[,c("lon","lat")]))

data.list <- list(y = y, 
                  occ.covs = occ.covs, 
                  det.covs = det.covs)

print("data ready")

### model setting ##########################################

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

#n.chains <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "1")) 
n.chains <- 1
n.batch <- 10
batch.length <- 10
(n.samples <- n.batch * batch.length) 
#n.samples <- 50000
n.burn <- 10
n.thin <- 1
ar1 <- FALSE
n.report <- 100

###non sp trend run ##########################################

det.formula <- ~ (1|year) + (1|month) + nuSpecies
occ.formula <- ~ year + (1|site)

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

#print summary
summary(out)

#waic
waicOcc(out)

#summary samples
psiCovs <- MCMCsummary(out$beta.samples)
saveRDS(psiCovs, file=paste0("spocc_t_summary_",myspecies,".rds"))

#gof
ppc.out <- ppcOcc(out, fit.stat = 'freeman-tukey', group = 1)
summary(ppc.out)

print("finished model")