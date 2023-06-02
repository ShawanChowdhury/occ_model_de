# Loading libraries
library(tidyverse)
library(spOccupancy)
library(coda)
library(stars)
library(ggplot2)

# Import data
occMatrix <- read_rds("data/occ_matrix.rds")
data <- read_rds("data/sub_data.rds")

# detection covariates
yday <- reshape2::acast(data, visit ~ Species, value.var="yday", fun=length)

# Checking if they have the same number of rows
NROW(yday) == NROW(occMatrix)

#site covs
siteCovs <- data %>%
  dplyr::select(site, lon, lat, agriculture, protected_areas) %>%
  filter(!duplicated(site)) %>%
  arrange(site)

# siteCovs <- siteCovs[,3:8]

# Package up
data.list <- list(y = occMatrix[,1],
                  occ.covs = siteCovs,
                  det.covs = list(yday = yday),
                  coords = siteCovs[,c("lon","lat")])

#######################################
# Priors
prior.list <- list(beta.normal = list(mean = 0, var = 2.72),
                   alpha.normal = list(mean = 0, var = 2.72))

# Initial values
inits.list <- list(alpha = 0, beta = 0,
                   z = apply(occMatrix, 1, max, na.rm = TRUE))

# Settings
n.samples <- 10
n.report <- 10

# Run model - non spatial
out <- PGOcc(occ.formula = ~ protected_areas,
             det.formula = ~ yday,
             data = data.list,
             inits = inits.list,
             n.samples = n.samples,
             priors = prior.list,
             n.omp.threads = 1,
             verbose = TRUE,
             n.report = n.report,
             n.burn = 2,
             n.thin = 1,
             n.chains = 1)

# Model summary
summary(out)

ppc.out <- ppcOcc(out, fit.stat = 'freeman-tukey', group = 1)#group 1 = by row/site
summary(ppc.out)

diff.fit <- ppc.out$fit.y.rep.group.quants[3, ] - ppc.out$fit.y.group.quants[3, ]
plot(diff.fit, pch = 19, xlab = 'Site ID', ylab = 'Replicate - True Discrepancy')

#plotting
ppc.df <- data.frame(fit = ppc.out$fit.y,
                      fit.rep = ppc.out$fit.y.rep,
                      color = 'lightskyblue1')
ppc.df$color[ppc.df$fit.rep > ppc.df$fit] <- 'lightsalmon'
plot(ppc.df$fit, ppc.df$fit.rep, bg = ppc.df$color, pch = 21,
      ylab = 'Fit', xlab = 'True')
lines(ppc.df$fit, ppc.df$fit, col = 'black')
