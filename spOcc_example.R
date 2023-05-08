### libraries ####

library(spOccupancy)

### spOccupancy ####

#### PGOcc ####

# non-spatial model

#convert to occu matrix
y <- reshape2::acast(visit_data_capped,Site_ID ~ visitNu, value.var="Obs")

#get other detection covariates
LL <- reshape2::acast(visit_data_capped,Site_ID ~ visitNu, value.var="LL")
yday <- reshape2::acast(visit_data_capped,Site_ID ~ visitNu, value.var="yday")
yday2 <- reshape2::acast(visit_data_capped,Site_ID ~ visitNu, value.var="yday2")

#site covs
siteCovs <- visit_data_capped %>%
              dplyr::select(Site_ID, EASTING, NORTHING,
                            acid_grass, impr_grass, neutr_grass,
                            calc_grass, broad_wood, total_urban, wood, grass,
                            MeanTempColdQuarter,AnnualPrecip,
                            diffDays, maxday,max) %>%
              filter(!duplicated(Site_ID)) %>%
              arrange(Site_ID)

#Package up
data.list <- list(y = y,
                  occ.covs = siteCovs,
                  det.covs = list(LL = LL, yday = yday, yday2 = yday2),
                  coords = siteCovs[,c("EASTING","NORTHING")])
# Priors
prior.list <- list(beta.normal = list(mean = 0, var = 2.72),
                   alpha.normal = list(mean = 0, var = 2.72))

# Initial values
inits.list <- list(alpha = 0, beta = 0,
                   z = apply(y, 1, max, na.rm = TRUE))

# Settings
n.samples <- 10000
n.report <- 1000

# Run model - non spatial
out <- PGOcc(occ.formula = ~ total_urban+grass+wood+MeanTempColdQuarter+diffDays+maxday+max,
             det.formula = ~ LL + yday + yday2,
             data = data.list,
             inits = inits.list,
             n.samples = n.samples,
             priors = prior.list,
             n.omp.threads = 1,
             verbose = TRUE,
             n.report = n.report,
             n.burn = n.samples/2,
             n.thin = n.samples/10,
             n.chains = 3)

summary(out)

#GOF
ppc.out <- ppcOcc(out, fit.stat = 'freeman-tukey', group = 1)#group 1 = by row/site
summary(ppc.out)
#saveRDS(ppc.out, file=paste0(outDir,"/ppc_pgOcc_", myspecies, ".rds"))

#diff.fit <- ppc.out$fit.y.rep.group.quants[3, ] - ppc.out$fit.y.group.quants[3, ]
#plot(diff.fit, pch = 19, xlab = 'Site ID', ylab = 'Replicate - True Discrepancy')

# #plotting
# ppc.df <- data.frame(fit = ppc.out$fit.y, 
#                      fit.rep = ppc.out$fit.y.rep, 
#                      color = 'lightskyblue1')
# ppc.df$color[ppc.df$fit.rep > ppc.df$fit] <- 'lightsalmon'
# plot(ppc.df$fit, ppc.df$fit.rep, bg = ppc.df$color, pch = 21, 
#      ylab = 'Fit', xlab = 'True')
# lines(ppc.df$fit, ppc.df$fit, col = 'black')


#plot fitted data
stateModel <- summaryState(out)
detectionModel <- summaryDet(out)
#plotCoefs(stateModel)
#plotCoefs(detectionModel)

saveRDS(stateModel, file=paste0(outDir,"/stateCoefs_pgOcc_", myspecies, ".rds"))
saveRDS(detectionModel, file=paste0(outDir,"/detectionCoefs_pgOcc_", myspecies, ".rds"))

#extend predictions to whole area
Params <- stateModel$param 
Params[1] <- substr(Params[1],2,10) #strip brackets from intercept
predDF <- predict(object = out, 
                  X.0 = as.matrix(newdata_full[,Params]), 
                  type = "occupancy")

#summarise samples
psiSamples <- predDF$psi.0.samples
newdata_full$pgOcc_preds <- apply(psiSamples, 2, mean)
newdata_full$pgOcc_preds_sd <- apply(psiSamples, 2, sd)

# ggplot(newdata_full_testing) +
#   geom_tile(aes(x=EASTING, y=NORTHING, fill=pgOcc_preds),
#             size=rel(10))+
#   scale_fill_viridis_c()

message("pgocc done")

#### spPGOcc ####

# spatial model

# Priors 
prior.list <- list(beta.normal = list(mean = 0, var = 2.72), 
                   alpha.normal = list(mean = 0, var = 2.72),
                   sigma.sq.ig = c(2, 2), 
                   phi.unif = c(3/1, 3/.1)) 
# Initial values
dist.hbef <- dist(coords)
inits.list <- list(alpha = 0, beta = 0,
                   phi =  16, #spatial range parameter
                   sigma.sq = 2, # spatial variance parameter
                   w = rep(0, nrow(siteCovs)), #the latent spatial random effects at each site
                   z = apply(y, 1, max, na.rm = TRUE))
# Batches
n.batch <- 5000
batch.length <- 25
(n.iter <- n.batch * batch.length)

# Tuning
tuning.list <- list(phi = 0.5)

# Run model - spatial
outS <- spPGOcc(occ.formula = ~ total_urban+grass+wood+MeanTempColdQuarter+diffDays+maxday+max,
             det.formula = ~ LL + yday + yday2,
             data = data.list,
             inits = inits.list,
             n.batch = n.batch, 
             batch.length = batch.length,
             priors = prior.list,
             cov.model = "exponential",
             tuning = tuning.list, 
             NNGP = TRUE, 
             n.neighbors = 5, 
             search.type = 'cb', 
             n.report = 100, 
             n.burn = n.iter/2, 
             n.thin = 50,
             n.chains = 3)

#accept.rate = 0.43 by default
summary(outS)

#GOF
ppc.outS <- ppcOcc(outS, fit.stat = 'freeman-tukey', group = 1)
summary(ppc.outS)
#saveRDS(ppc.outS, file=paste0(outDir,"/ppc_spOcc_", myspecies, ".rds"))

#plot fitted data
stateModelS <- summaryState(outS)
detectionModelS <- summaryDet(outS)
#plotCoefs(stateModel)
#plotCoefs(detectionModel)

saveRDS(stateModelS, file=paste0(outDir,"/stateCoefs_spOcc_", myspecies, ".rds"))
saveRDS(detectionModelS, file=paste0(outDir,"/detectionCoefs_spOcc_", myspecies, ".rds"))

#extend predictions to whole area
Params <- stateModelS$param 
Params[1] <- substr(Params[1],2,10) #strip brackets from intercept
predDF <- predict(object = outS, 
                  X.0 = as.matrix(newdata_full[,Params]), 
                  coords.0 = newdata_full[,c("EASTING","NORTHING")],
                  type = "occupancy")

#summarise samples
psiSamples <- predDF$psi.0.samples
newdata_full$spOcc_preds <- apply(psiSamples,2,mean)
newdata_full$spOcc_preds_sd <- apply(psiSamples,2,sd)

saveRDS(newdata_full, file=paste0(outDir,"/Occ_predictions_",
                                  myspecies,".rds"))

# ggplot(newdata_full_testing) +
#   geom_points(aes(x=EASTING, y=NORTHING, colour=pgOcc_preds),
#             size=rel(50))+
#   scale_colour_viridis_c()

message("spocc done")

### end #####
