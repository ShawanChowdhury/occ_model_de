#libraries
library(tidyverse)
library(sf)
library(tmap)
library(ggthemes)
library(spOccupancy)
library(data.table)
library(MCMCvis)

### get data #############################################

#myfolder <- "HPC_inputs"
myfolder <- "/data/idiv_ess/Amphis/"

visit_data <- readRDS(paste(myfolder,"listlengthDF.rds",sep="/"))
occMatrix <- readRDS(paste(myfolder,"occMatrix.rds",sep="/"))
siteInfo <- readRDS(paste(myfolder,"siteInfo.rds",sep="/"))
coords <- readRDS(paste(myfolder,"df_sf.rds",sep="/")) %>%
            add_column(x = st_coordinates(.)[,1],
                       y = st_coordinates(.)[,2]) %>%
            as_tibble()

coords$naturraum[is.na(coords$naturraum)] <- "other"

#add on naturraum
visit_data$naturraum <- coords$naturraum[match(visit_data$SiteID,
                                               coords$SiteID)]

print("step 1")

### summary plots ########################################

# coords <- readRDS(paste(myfolder,"df_sf.rds",sep="/"))
# st_crs(coords) <- 25833
# 
# Saxony <- readRDS("gadm36_DEU_1_sp.rds") %>%
#   st_as_sf() %>%
#   dplyr::filter(NAME_1 %in% c("Sachsen")) %>%
#   st_transform(.,st_crs(coords))
# 
# t1 <- tm_shape(Saxony)+
#   tm_fill()+
# tm_shape(coords)+
#   tm_dots() +
#   tm_scale_bar()
# 
# Germany <- readRDS("gadm36_DEU_1_sp.rds") %>%
#   st_as_sf() %>%
#   st_transform(.,st_crs(coords))
# 
# t2 <- tm_shape(Germany)+
#   tm_borders() +
#   tm_shape(Saxony)+
#   tm_fill() +
#   tm_scale_bar()
# 
# m2 <- tmap_arrange(t2,t1, widths=c(0.4,.6))
# tmap_save(m2, "plots/map.png", width=1920, height=1080, asp=0)


# #number of visits per year
# visitSummary <- visit_data %>%
#                   group_by(Year) %>%
#                   summarise(nuVisits = length(unique(visit)))
# 
# g1 <- ggplot(visitSummary, aes(x=Year, y=nuVisits)) +
#   geom_point()+
#   geom_line()+
#   theme_few()+
#   xlim(1990,2020)+
#   xlab("Year") + ylab("Total number of site visits")+
#   theme(axis.text =element_text(size=15),
#         axis.title =element_text(size=17))
# 
# #detection methods
# 
# #group ones using script below
# 
# visitSummary <- visit_data %>%
#   group_by(Year,visitDetection) %>%
#   summarise(nuVisits = length(unique(visit))) %>%
#   mutate(visitDetection = tolower(visitDetection))
# 
# visitSummary$visitDetection <- factor(visitSummary$visitDetection)
# levels(visitSummary$visitDetection)
# levels(visitSummary$visitDetection) <- c("acoustic",
#                                          "drift fences along roads", 
#                                          "capture: various traps",
#                                          "capture: by hand", 
#                                          "capture: netting",
#                                          "sight",
#                                          "dead individuals",
#                                          "unknown")
# 
# visitSummary$visitDetection <- factor(visitSummary$visitDetection,
#                                       levels=c("acoustic",
#                                                "capture: various traps",
#                                                "capture: by hand", 
#                                                "capture: netting",
#                                                "dead individuals",
#                                                "drift fences along roads",
#                                                "sight","unknown"))
# 
# g2 <- ggplot(visitSummary, aes(x=Year, y=nuVisits)) +
#   #geom_point(aes(colour=visitDetection))+
#   geom_line(aes(colour=visitDetection), size=1.5)+
#   theme_few()+
#   xlim(1990,2020)+
#   scale_y_log10()+
#   scale_colour_brewer("Detection method",
#                       type="qual")+
#   xlab("Year") + ylab("Number of site visits") +
#   theme(legend.position = "bottom")+
#   theme(axis.text =element_text(size=15),
#         axis.title =element_text(size=17),
#         legend.title = element_text(size=14), #change legend title font size
#         legend.text = element_text(size=14))+
#   guides(color=guide_legend(nrow=3,byrow=TRUE))
# 
# cowplot::plot_grid(g1, g2, ncol=1,
#                    labels=c("a)","b)"))
# ggsave("plots/annualdetection.png", width=9,height=10)

### species #######################################

allspecies <- colnames(occMatrix)

#add on species data
task.id = as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID", "1"))
myspecies = allspecies[task.id]
print(myspecies)

visit_data$Species <- occMatrix[,myspecies]

print("step 2")

### add coordinates #####################################

sum(!visit_data$SiteID %in% coords$SiteID)

visit_data <- coords %>%
                dplyr::select(SiteID, x, y) %>%
                inner_join(visit_data,.)

utmProj <- "+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs"

print("step 3")

### detection covariates ################################

#get total records for each species per year
annual_Summary <- visit_data %>%
                    group_by(Year, naturraum) %>%
                    summarize(nuRecs = length(visit[Species==1]),
                              propRecs = mean(Species)) %>%
                    ungroup()

visit_data <- inner_join(visit_data, annual_Summary, by=c("Year","naturraum"))

#life stage - include as fixed effect
table(visit_data$visitEinheit)

#detection type - include as fixed effect
table(visit_data$visitDetection)

#plot time-series
# detectionSummary <- visit_data %>%
#                       group_by(visitDetection,Year) %>%
#                       summarise(nuVisits = length(unique(visit)))
# 
# ggplot(detectionSummary, aes(x=Year, y=nuVisits)) +
#         geom_point() +
#         geom_line() +
#         scale_y_log10() +
#         facet_wrap(~visitDetection)

#clean
visit_data$visitDetection[visit_data$visitDetection=="sonst"] <- "unknown"
visit_data$visitDetection[grepl("schutzanlage",visit_data$visitDetection)] <- "Amphibienschutzanlage"
visit_data$visitDetection[grepl("Fang: Bodenfalle",visit_data$visitDetection)] <- "Fang"
visit_data$visitDetection[grepl("Fang: Eimerfalle",visit_data$visitDetection)] <- "Fang"
visit_data$visitDetection[grepl("Fang: Flaschenfalle",visit_data$visitDetection)] <- "Fang"
visit_data$visitDetection[grepl("Wasserfang",visit_data$visitDetection)] <- "Fang"
visit_data$visitDetection[grepl("Fang: sonst. Fallen",visit_data$visitDetection)] <- "Fang"
visit_data$visitDetection[grepl("Fang: Elektrobefischung",visit_data$visitDetection)] <- "Fang"

#month- include as a quadratic effect
table(visit_data$monthIndex)
visit_data$monthIndex_2 <- visit_data$monthIndex^2

#include FFH monitoring indicator
visit_data$FFH <-sapply(tolower(visit_data$project), function(x)
                          ifelse(grepl("ffh",x),1,0))
table(visit_data$FFH)

#include a year index
table(visit_data$yearIndex)
#1992:2019
myyears <- sort(unique(visit_data$Year))

print("step 4")

### format ###############################################

#format for using this package

#correlation of site occupancy over space and time is induced by allowing occupancy probability 
#to be a smooth function of space and time

#need to make visit data with columns: site, occassion and obs
visit_data <- visit_data[,c("SiteID","Year","visit","Species","x","y",
                            "visitEinheit","visitDetection","nuSpecies","nuGenus",
                            "monthIndex","monthIndex_2","yearIndex","FFH",
                            "projectIndex","nuRecs","propRecs","naturraum")]

#col names needed for the package
names(visit_data)[1:4] <- c("site","occasion","visit","obs")

#group years to simplify the model
#visit_data$occasion <- plyr::round_any(visit_data$occasion,5)
#table(visit_data$occasion)
#visit_data$occasion[visit_data$occasion==0] <- 5
#visit_data$occasion[visit_data$occasion==30] <- 25

### subsample ###########################################

#how many visits per site per years
visitSummary <- visit_data %>%
  group_by(site,occasion) %>%
  summarise(nuVisits = length(obs))

#some sampled up to 174 times!!
#sample at most 4 visits per year

visit_data <- visit_data %>%
  group_by(site,occasion) %>%
  mutate(mySample = ifelse(length(obs)>5, 5, length(obs))) %>%
  sample_n(.,size=mySample) %>%
  ungroup()

#need to make visit be indexed from i to n within each site and occasion
visit_data <- visit_data %>%
  group_by(site, occasion) %>%
  mutate(visit = as.numeric(as.factor(visit)))%>%
  ungroup()

visit_data$occasion <- as.numeric(as.factor(visit_data$occasion))

table(visit_data$occasion)

### format for spOcc ########################################

visit_data <- visit_data %>%
                arrange(site,yearIndex)

#response
y <- reshape2::acast(visit_data, site ~ yearIndex ~ visit, value.var = "obs")
dim(y)

#occ covs
siteDF <- data.frame(site = dimnames(y)[[1]])
siteDF$naturraum <- visit_data$naturraum[match(siteDF$site, visit_data$site)]
siteDF$x <- visit_data$x[match(siteDF$site, visit_data$site)]/100000
siteDF$y <- visit_data$y[match(siteDF$site, visit_data$site)]/100000


yearMatrix <- expand.grid(site =siteDF$site,
                          yearIndex = unique(visit_data$yearIndex))

occ.covs <- list(site = as.numeric(as.factor(siteDF$site)),
                 naturraum = as.numeric(as.factor(siteDF$naturraum)),
                 x = siteDF$x,
                 y = siteDF$y,
                 year = reshape2::acast(yearMatrix, site ~ yearIndex, value.var = "yearIndex"),
                 year2 = reshape2::acast(yearMatrix, site ~ yearIndex, value.var = "yearIndex"))

#det covs
visit_data$projectIndex <- as.numeric(as.factor(visit_data$projectIndex))
visit_data$monthIndex <- as.numeric(as.factor(visit_data$monthIndex))
visit_data$yearIndex <- as.numeric(as.factor(visit_data$yearIndex))
visit_data$visitEinheit <- as.numeric(as.factor(visit_data$visitEinheit))
visit_data$visitDetection <- as.numeric(as.factor(visit_data$visitDetection))
visit_data$naturraum <- as.numeric(as.factor(visit_data$naturraum))
visit_data$nuSpecies <- ifelse(visit_data$nuSpecies==1,0,1)
table(visit_data$nuSpecies)

det.covs <- list(month = reshape2::acast(visit_data, site ~ yearIndex ~ visit, value.var = "monthIndex"),
                 year = reshape2::acast(visit_data, site ~ yearIndex ~ visit, value.var = "yearIndex"),
                 naturraum = reshape2::acast(visit_data, site ~ yearIndex ~ visit, value.var = "naturraum"),
                 nuRecs = reshape2::acast(visit_data, site ~ yearIndex ~ visit, value.var = "nuRecs"),
                 propRecs = reshape2::acast(visit_data, site ~ yearIndex ~ visit, value.var = "propRecs"),
                 nuSpecies = reshape2::acast(visit_data, site ~ yearIndex ~ visit, value.var = "nuSpecies"),
                 einheit = reshape2::acast(visit_data, site ~ yearIndex ~ visit, value.var = "visitEinheit"),
                 detmethod = reshape2::acast(visit_data, site ~ yearIndex ~ visit, value.var = "visitDetection"),
                 project = reshape2::acast(visit_data, site ~ yearIndex ~ visit, value.var = "projectIndex"),
                 FFH = reshape2::acast(visit_data, site ~ yearIndex ~ visit, value.var = "FFH"))

coords <- as.matrix(unique(visit_data[,c("x","y")]))

data.list <- list(y = y, 
                  occ.covs = occ.covs, 
                  det.covs = det.covs, 
                  coords = coords)

print("data ready")

### detection formula ####################################

det.formula <- ~ (1|month) + (1|year) + (1|project) + log(nuRecs+1)

###non sp setting ##########################################

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

# occ.formula <- ~ year + (1 | site) + (1|naturraum)
# 
# out <- tPGOcc(occ.formula = occ.formula,
#               det.formula = det.formula,
#               data = data.list,
#               n.batch = n.batch,
#               batch.length = batch.length,
#               #n.samples = n.samples,
#               inits = all.inits,
#               priors = all.priors,
#               ar1 = ar1,
#               n.burn = n.burn,
#               n.thin = n.thin,
#               n.chains = n.chains,
#               n.report = n.report)
# 
# #print summary
# summary(out)
# 
# #waic
# waicOcc(out)
# 
# #summary samples
# psiCovs <- MCMCsummary(out$beta.samples)
# saveRDS(psiCovs, file=paste0("spocc_t_summary_",myspecies,".rds"))
# 
# #gof
# ppc.out <- ppcOcc(out, fit.stat = 'freeman-tukey', group = 1)
# summary(ppc.out)
# 
# print("finished t")

###non sp annual run ##########################################

occ.formula <- ~ factor(year) - 1 + (1 | site) + (1|naturraum)

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
saveRDS(psiCovs, file=paste0("spocc_t_annualsummary_",myspecies,".rds"))

print("finished t annual")

### non sp lat lon ##################################################

# occ.formula <- ~ factor(year) - 1 + (1 | site) + x + y
# 
# out <- tPGOcc(occ.formula = occ.formula,
#               det.formula = det.formula,
#               data = data.list,
#               n.batch = n.batch,
#               batch.length = batch.length,
#               inits = all.inits,
#               priors = all.priors,
#               ar1 = ar1,
#               n.burn = n.burn,
#               n.thin = n.thin,
#               n.chains = n.chains,
#               n.report = 2000)
# 
# #print summary
# summary(out)
# 
# #summary samples
# psiCovs <- MCMCsummary(out$beta.samples)
# saveRDS(psiCovs, file=paste0("spocc_t_llannualsummary_",myspecies,".rds"))
# 
# print("finished t llannual")

###non sp random annual run ##########################################

# occ.formula <- ~ year + (1|year2) + (1 | site) + (1|naturraum)
# 
# out <- tPGOcc(occ.formula = occ.formula, 
#               det.formula = det.formula,
#               data = data.list,
#               n.batch = n.batch,
#               batch.length = batch.length,
#               inits = all.inits,
#               priors = all.priors,
#               ar1 = ar1,
#               n.burn = n.burn,
#               n.thin = n.thin,
#               n.chains = n.chains,
#               n.report = 2000)
# 
# #print summary
# summary(out)
# 
# #summary samples
# psiCovs <- MCMCsummary(out$beta.samples)
# saveRDS(psiCovs, file=paste0("spocc_t_annualsummary_",myspecies,".rds"))
# 
# #predicted occupancy for each year
# 
# #number of site predictions
# J.pred <- length(occ.covs$site)
# 
# # Number of prediction years.
# years <- sort(unique(visit_data$yearIndex))
# n.years.pred <- length(years)
# 
# # Number of predictors (including intercept)
# p.occ <- 5
# 
# # Create three-dimensional array
# # site, primary time period, and covariates
# X.0 <- array(1, dim = c(J.pred, n.years.pred, p.occ))
# 
# year.pred <- matrix(rep(years, J.pred), 
#                     J.pred, n.years.pred, byrow = TRUE)
# 
# #add year predictor to second dimension
# X.0[,,2] <- year.pred
# X.0[,,3] <- year.pred
# X.0[,,4] <- occ.covs$site
# X.0[,,5] <- occ.covs$naturraum
# 
# dimnames(X.0)[[3]] <- c("Intercept","year","year2","site","naturraum")
# 
# #check
# dim(X.0)
# X.0[1,,] #site predictions - one row for each year
# X.0[,1,] #all site predictions - one row for first year
# X.0[,,1] #intercepts for each site and year
# X.0[,,2] #year values for each site
# 
# #get predictions
# out.pred <- predict(out, 
#                     X.0, 
#                     t.cols = c(1:28), 
#                     ignore.RE = FALSE, 
#                     type = 'occupancy') 
# 
# #summarise across sites and year
# meanProp <- apply(out.pred$psi.0.samples,c(1,3),mean)
# annualMean <- apply(meanProp,2,mean)
# annualLower <- apply(meanProp,2,function(x) quantile(x,0.025))
# annualUpper <- apply(meanProp,2,function(x) quantile(x,0.975))
# 
# randomSummary <- data.frame(Year = years,
#                             mean = annualMean,
#                             x2_5_percent = annualLower, 
#                             x97_5_percent = annualUpper)
# 
# saveRDS(randomSummary, file=paste0("spocc_t_annualrandomsummary_",myspecies,".rds"))
# 
# #gof
# ppc.out <- ppcOcc(out, fit.stat = 'freeman-tukey', group = 1)
# summary(ppc.out)
# 
# print("finished t annual")

### sp settings ###########################################

# dist.hbef <- dist(coords)
# all.inits <- list(beta = 0,
#                   alpha = 0,
#                   z = z.inits,
#                   sigma.sq = 1,
#                   phi = 3 / mean(dist.hbef),
#                   sigma.sq.t = 1.5, rho = 0.2)
# 
# all.priors <- list(beta.normal = list(mean = 0, var = 2.72),
#                    alpha.normal = list(mean = 0, var = 2.72),
#                    sigma.sq.t.ig = c(2, 0.5),
#                    rho.unif = c(-1, 1),
#                    sigma.sq.ig = c(2, 1),
#                    phi.unif = c(3 / max(dist.hbef), 3 / min(dist.hbef)))
# 
# cov.model <- 'exponential'
# n.neighbors <- 5
# ar1 <- TRUE
# n.batch <- 500
# batch.length <- 100
# (n.samples <- n.batch * batch.length)
# # Total number of samples n.batch * batch.length
# n.burn <- 25000
# n.thin <- 20
# 
# ### sp run ####################################
# 
# occ.formula <- ~ year
# 
# out.st <- stPGOcc(occ.formula = occ.formula,
#                   det.formula = det.formula,
#                   data = data.list,
#                   inits = all.inits,
#                   priors = all.priors,
#                   cov.model = cov.model,
#                   n.neighbors = n.neighbors,
#                   n.batch = n.batch,
#                   batch.length = batch.length,
#                   verbose = TRUE,
#                   ar1 = ar1,
#                   n.report = 1000,
#                   n.burn = n.burn,
#                   n.thin = n.thin,
#                   n.chains = n.chains)
# 
# #print summary
# summary(out.st)
# 
# #get fit
# waicOcc(out.st)
# 
# #summary samples
# psiCovs.st <- MCMCsummary(out.st$beta.samples)
# saveRDS(psiCovs.st, file=paste0("spocc_st_summary_",myspecies,".rds"))
# 
# #gof
# ppc.out.st <- ppcOcc(out.st, fit.stat = 'freeman-tukey', group = 1)
# summary(ppc.out.st)
# 
# print("finished st")

# ### sp annual #########################################
# 
# occ.formula <- ~ factor(year)-1
# 
# out.annual <- stPGOcc(occ.formula = occ.formula,
#                   det.formula = det.formula,
#                   data = data.list,
#                   inits = all.inits,
#                   priors = all.priors,
#                   cov.model = cov.model,
#                   n.neighbors = n.neighbors,
#                   n.batch = n.batch,
#                   batch.length = batch.length,
#                   verbose = TRUE,
#                   ar1 = ar1,
#                   n.report = 1000,
#                   n.burn = n.burn,
#                   n.thin = n.thin,
#                   n.chains = n.chains)
# 
# #print summary
# summary(out.annual)
# 
# #get fit
# waicOcc(out.annual)
# 
# #summary samples
# psiCovs.annual <- MCMCsummary(out.annual$beta.samples)
# saveRDS(psiCovs.annual, file=paste0("spocc_st_annualsummary_",myspecies,".rds"))
# 
# #gof
# ppc.out.annual <- ppcOcc(out.annual, fit.stat = 'freeman-tukey', group = 1)
# summary(ppc.out.annual)
# 
# print("finished annual st")

### end ####################################################