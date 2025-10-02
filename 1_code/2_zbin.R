gc()
rm(list = ls())

library(tidyverse)
library(JAG)
library(abind)
library(sf)

## detection and occupancy models
load( "C:/Users/kukwh001/OneDrive - Texas A&M University - Kingsville/Desktop/Documents/Research Projects/Florida_mesocarn/2_tmp/weekly_detection_data.rda")
# remove columns with weeks that don't line up (from Heather's code): 2017-06-25_to_2017-07-01 and 2015-06-25_to_2015-07-01 ???

species_list<- c("raccoon", "panther", "bear", "coyote", "bobcat", "fox", "otter", "skunk")

detection_list <- lapply(species_list, function(sp) {
  df <- detections_matrix %>%
    distinct() %>%
    st_drop_geometry() %>%
    filter(species %in% c(sp, "any")) %>% # for when camera was operational, but nobody was detected
    select(camID, week, detected) %>%
    pivot_wider(id_cols = camID, names_from = week, values_from = detected) %>%
    arrange(camID)
  
  return(df)
})
names(detection_list) <- species_list

 # Combine into a 3D array (sites × sampling occasions × species)
# Keep only numeric detection columns (e.g., "1_dry_2015", etc.)
detection_list_trimmed <- lapply(detection_list, function(df) {
  df_only <- df[, sapply(df, is.numeric), drop = FALSE]
  mat <- as.matrix(df_only)
  rownames(mat) <- df$camID  # Set camera IDs as rownames
  return(mat)
})
# Bind 
y_array <- abind::abind(detection_list_trimmed, along = 3)
y_array_correct <- aperm(y_array, c(3, 1, 2))

# site covariates
site_covs <- detections_matrix %>%
  mutate(year = year(week)) %>%
  st_drop_geometry() %>%
  select(camID, year, season, Trail, human_activity_seasonal) %>%
  distinct(camID, .keep_all = TRUE) %>%
  arrange(camID) %>%
  mutate(human_activity_seasonal = ifelse(is.na(human_activity_seasonal), 0, human_activity_seasonal)) # just making them 0...

# observation covariates
weekly_covariates <- c(
  "year",
  "season",
  "weekly_tmax", 
  "weekly_swi", 
  "panther_activity_weekly",
  "coyote_activity_weekly",
  "human_activity_weekly",
)
detections_matrix<- detections_matrix %>%
  mutate(year = as.numeric(year),
         season = factor(season, levels = c("wet", "dry")))
# Pivot each covariate separately into site × occasion format
det.covs <- setNames(
  lapply(weekly_covariates, function(var) {
    detections_matrix %>%
      st_drop_geometry() %>%
      distinct(camID, week, !!sym(var)) %>%
      pivot_wider(names_from = week, values_from = !!sym(var)) %>%
      column_to_rownames("camID") %>%
      as.matrix()
  }),
  weekly_covariates
)

# build occ data
data.list <- list(y = y_array_correct, 
                  occ.covs = site_covs,
                  det.covs = det.covs,
                  coords = cam_locs %>% dplyr::select(UTME, UTMN) %>% mutate(UTME = as.numeric(UTME),
                                                                             UTMN = as.numeric(UTMN)) )

"model {
  # Community-level priors (hyperpriors)
  for (p in 1:P_occ) {
    mu_beta[p] ~ dnorm(0, 0.001)
    tau_beta[p] ~ dgamma(0.1, 0.1)
  }
  for (q in 1:P_det) {
    mu_alpha[q] ~ dnorm(0, 0.001)
    tau_alpha[q] ~ dgamma(0.1, 0.1)
  }
  
  # Species-level effects
  for (s in 1:n_species) {
    for (p in 1:P_occ) {
      beta[s, p] ~ dnorm(mu_beta[p], tau_beta[p])
    }
    for (q in 1:P_det) {
      alpha[s, q] ~ dnorm(mu_alpha[q], tau_alpha[q])
    }
  }
  
  # Model loop
  for (i in 1:n_obs) {
    # Zero-inflation (occupancy) submodel
    logit(psi[i]) <- inprod(beta[species[i], ], X_occ[i, ])
    z[i] ~ dbern(psi[i])
    
    # Detection submodel (counts conditional on use)
    log(lambda[i]) <- inprod(alpha[species[i], ], X_det[i, ])
    y[i] ~ dnegbin(p[i], r)
    p[i] <- r / (r + z[i] * lambda[i])
  }
  
  # Overdispersion parameter
  r ~ dgamma(0.1, 0.1)
}"