# MAKEFILE AKA src/MAKE.R
library(ggtext) 
library(glmm)
library(glue)
library(ggpubr)
library(lme4) 
library(lmerTest)
library(lubridate)
library(MASS)
library(MCMCglmm)
library(oce)
library(pracma)
library(plyr)
library(R.matlab)
library(scales)
library(tidyverse)
library(rorqual.morpho)



# 1. Collect data using collect_lunges
if (!all(file.exists("data/output/deployments.RDS", "data/output/lunges.RDS"))) { 
  source("src/collect_lunges.R")
} else {
  deployments <- readRDS("data/output/deployments.RDS") 
  lunges <- readRDS("data/output/lunges.RDS")
}

# 2. Diel cycle calculations
if (!all(file.exists("data/output/diel_deployments.RDS", 
                     "data/output/diel_lunges.RDS"))) { 
  source("src/dielperiod.R")
} else {
  diel_deployments <- readRDS("data/output/diel_deployments.RDS")            
  diel_lunges <- readRDS("data/output/diel_lunges.RDS")
}


# 3. Feeding metric calculations
if (!all(file.exists("data/output/diel_deployments_pivot.RDS",
                     "data/output/lunge_counts.RDS",
                     "data/output/lunge_rates.RDS"))) { 
  source("src/depth_buckets.R")
} else {
  lunge_counts <- readRDS("data/output/lunge_counts.RDS")            
  diel_deployments_pivot <- readRDS("data/output/diel_deployments_pivot.RDS") 
  lunge_rates <- readRDS("data/output/lunge_rates.RDS") 
}

# 4. Collect morphology data
if (!file.exists("data/output/morphology.RDS")) {  
  source("src/collect_morphology.R")
} else {
  morphology <- readRDS("data/output/morphology.RDS") 
}

# 5. Collect prey data
if (!file.exists("data/output/prey.RDS")) {  
  source("src/collect_prey.R")
} else {
  prey <- readRDS("data/output/prey.RDS") 
}

# 6. Plastic from water (Choy et al 2019)
if (!all(file.exists("data/output/Choy_Kashi.RDS",  
                     "data/output/rplastic.RDS"))) {
  source("src/Choy_water.R")
} else {
  Choy_Kashi <- readRDS("data/output/Choy_Kashi.RDS")  
  rplastic <- readRDS("data/output/rplastic.RDS")
}


#7. Maximum effort from 24 hour plus deployments 
if (!all(file.exists("data/output/deploy_cycle.RDS",
                 "data/output/max_effort.RDS"))) {
  source("src/max_effort.R")
} else {
  deploy_cycle <- readRDS("data/output/deploy_cycle.RDS")
  max_effort <- readRDS("data/output/max_effort.RDS")
}

#8. Scenario preparation
if (!all(file.exists("data/output/scenarios.RDS"))) {
  source("src/scenes.R")
} else {
  scenarios <- readRDS("data/output/scenarios.RDS")
}

#9. Simulation 

#9. Run simulations 
# Go to that simulations.R



