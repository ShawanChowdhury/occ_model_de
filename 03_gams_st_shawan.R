#libraries
library(tidyverse)
library(sf)
library(tmap)
library(ggthemes)
library(spOccupancy)
library(data.table)
library(MCMCvis)

### get data #############################################

myfolder <- "/data/idiv_ess/Amphis/" # HPC folder

# this is like your sub_data:
visit_data <- readRDS(paste(myfolder,"listlengthDF.rds",sep="/"))
#metadata for each visit

#the species binary matric
occMatrix <- readRDS(paste(myfolder,"occMatrix.rds",sep="/"))
#each row is a visit, while each column is data for a species

#check the visit data fram and occ matrix align
all(visit_data$visit==row.names(occMatrix))
#this should be TRUE - if not, you need to rearrange the above objects

### species #######################################

#get all species
allspecies <- colnames(occMatrix)

#decide focal species for this task
task.id = as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID", "1"))
myspecies = allspecies[task.id]
print(myspecies)

#add on the focal species data for this task
visit_data$Species <- occMatrix[,myspecies]

### format ###########################################

#for spOccupancy we need to format the data into a matrix

#how many visits per site per year
visitSummary <- visit_data %>%
  group_by(site, year) %>%
  summarise(nuVisits = length(visit))

#some sampled up to 174 times!!
#subsample at most 10 visits per year at any specific site
visit_data <- visit_data %>%
  group_by(site, year) %>%
  mutate(mySample = ifelse(length(visit)>10, 10, length(visit))) %>%
  sample_n(.,size=mySample) %>%
  ungroup()

#need to make visit be indexed from i to n within each site and year
visit_data <- visit_data %>%
  group_by(site, year) %>%
  mutate(visit = as.numeric(as.factor(visit)))%>%
  ungroup() 

#make response into the matrix
y <- reshape2::acast(visit_data, site ~ yearIndex ~ visit, value.var = "obs")
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
visit_data$monthIndex <- as.numeric(as.factor(visit_data$month))
visit_data$yearIndex <- as.numeric(as.factor(visit_data$year))
visit_data$nuSpecies <- ifelse(visit_data$nuSpecies==1,"single",
                               ifelse(visit_data$nuSpecies %in% 2:3, "short", "long"))

det.covs <- list(month = reshape2::acast(visit_data, site ~ yearIndex ~ visit, value.var = "monthIndex"),
                 year = reshape2::acast(visit_data, site ~ yearIndex ~ visit, value.var = "yearIndex"),
                 nuSpecies = reshape2::acast(visit_data, site ~ yearIndex ~ visit, value.var = "nuSpecies"))

coords <- as.matrix(unique(visit_data[,c("x","y")]))

data.list <- list(y = y, 
                  occ.covs = occ.covs, 
                  det.covs = det.covs, 
                  coords = coords)

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
n.chains <- 3
n.batch <- 1500
batch.length <- 100
(n.samples <- n.batch * batch.length) 
#n.samples <- 50000
n.burn <- n.samples*3/4
n.thin <- 30
ar1 <- FALSE
n.report <- 10000

###non sp trend run ##########################################

det.formula <- ~ (1|month) + (1|year) + nuSpecies
occ.formula <- ~ year + (1 | site) 

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