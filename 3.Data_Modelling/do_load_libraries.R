library(tidyverse) # For data processing/coding utils
library(ggdist) # For certain distribution plots
library(sf) # For reading shape files
library(tmap) # For chloropleth maps
library(xtable) # For making latex files
library(rstan) # Prerequisite for brms
library(brms) # For fitting Bayesian models
library(posterior) # For extracting posterior draws
library(matrixStats) # For weightedMean/Var/Median
library(extraDistr) # Needed for computing LOOIC
options(mc.cores = parallel::detectCores()) # For Stan MCMC simulation

packageVersion("tidyverse") # For data processing/coding utils
packageVersion("ggdist") # For certain distribution plots
packageVersion("sf") # For reading shape files
packageVersion("tmap") # For chloropleth maps
packageVersion("xtable") # For making latex files
packageVersion("rstan") # Prerequisite for brms
packageVersion("brms") # For fitting Bayesian models
packageVersion("posterior") # For extracting posterior draws
packageVersion("matrixStats") # For weightedMean/Var/Median
packageVersion("extraDistr") # Needed for computing LOOIC
