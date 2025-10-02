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
  "panther_presence_weekly",
  "coyote_activity_weekly",
  "coyote_presence_weekly",
  "human_activity_weekly",
  "human_presence_weekly"
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

# fit occupancy models
n.samples <- 5000     # Total post-burn-in samples per chain
n.burn <- 1000        # Burn-in iterations
n.thin <- 5           # Thinning rate
n.chains <- 3         # Number of MCMC chains
prior.list <- list(
  beta.normal = list(mean = 0, var = 2.72),   # Occurrence covariate priors
  alpha.normal = list(mean = 0, var = 2.72),  # Detection covariate priors
  sigma.sq.psi.unif = c(0.01, 10),            # Variance of species-level random effect
  sigma.sq.p.unif = c(0.01, 10)               # Variance of detection-level random effect
)
inits.list <- list()
for (i in 1:n.chains) {
  inits.list[[i]] <- list(
    beta = rnorm(5),           # Initial values for occurrence covariate effects
    alpha = rnorm(5),          # Initial values for detection covariate effects
    z = apply(y_array_correct, c(1, 2), function(x) as.numeric(any(x == 1, na.rm = TRUE))) # latent occupancy state
  )
}

fit <- spMsPGOcc(
  occ.formula = ~ Trail + human_activity_seasonal + season,
  det.formula = ~ human_activity_weekly + weekly_swi + weekly_tmax + season + (1 | year),
  data = data.list,
  cov.model = "exponential",  # still required even if spatial = FALSE
  n.batch = 1000,
  batch.length = 25,
  n.burn = 2000,
  n.thin = 10,
  n.chains = 3,
  verbose = TRUE
)

summary(fit)
cov_matrix <- fit$theta.samples  # posterior draws of species covariance matrix


# DETECT -----------------------------------------------------------------------
#
# FM1 # ------------------------------------------------------------------------

occFormulas <- c('~1','~1','~1')  # + hum_ar + rds_sc

detFormulas <- c('~trail + swi + humans','~trail + swi + humans')

fm1 <- occuMulti(detFormulas, occFormulas, dat)

# FM2 # ----------------------------------------------------------

occFormulas <- c('~1','~1','~1')  # + hum_ar + rds_sc

detFormulas <- c('~trail','~trail')

fm2 <- occuMulti(detFormulas, occFormulas, dat)

# FM3 # ----------------------------------------------------------

occFormulas <- c('~1','~1','~1')  # + hum_ar + rds_sc

detFormulas <- c('~swi','~swi')

fm3 <- occuMulti(detFormulas, occFormulas, dat)

# FM4 # ----------------------------------------------------------

occFormulas <- c('~1','~1','~1')  # + hum_ar + rds_sc

detFormulas <- c('~humans','~humans')

fm4 <- occuMulti(detFormulas, occFormulas, dat)

# FM5 # ----------------------------------------------------------

occFormulas <- c('~1','~1','~1')  # + hum_ar + rds_sc

detFormulas <- c('~trail + swi','~trail + swi')

fm5 <- occuMulti(detFormulas, occFormulas, dat)

# FM6 # ----------------------------------------------------------

occFormulas <- c('~1','~1','~swi_mean*hum_ar + year')  # + hum_ar + rds_sc

detFormulas <- c('~swi + humans','~swi + humans')

fm6 <- occuMulti(detFormulas, occFormulas, dat)

# FM7 # ----------------------------------------------------------

occFormulas <- c('~1','~1','~swi_mean + year')  # + hum_ar + rds_sc

detFormulas <- c('~trail + humans','~trail + humans')

fm7 <- occuMulti(detFormulas, occFormulas, dat)

# FM8 # ----------------------------------------------------------

occFormulas <- c('~1','~1','~1')  # + hum_ar + rds_sc

detFormulas <- c('~trail*swi','~trail*swi')

fm8 <- occuMulti(detFormulas, occFormulas, dat)

# FM9 # ----------------------------------------------------------

occFormulas <- c('~1','~1','~swi_mean*hum_ar + year')  # + hum_ar + rds_sc

detFormulas <- c('~swi*humans','~swi*humans')

fm9 <- occuMulti(detFormulas, occFormulas, dat)

# FM10 # ----------------------------------------------------------

occFormulas <- c('~1','~1','~swi_mean + year')  # + hum_ar + rds_sc

detFormulas <- c('~trail*humans','~trail*humans')

fm10 <- occuMulti(detFormulas, occFormulas, dat)

# Null # ---------------------------------------------------------

occFormulas <- c('~1','~1','~1') 

detFormulas <- c('~1','~ 1')

null <- occuMulti(detFormulas, occFormulas, dat)
# AIC MODEL AVG # ------------------------------------------------

require(AICcmodavg)

ml <- list(null, fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10) 

Model.names <- c("~1 ~1 ~1", 
                 "~trail + swi + humans ~trail + swi + humans", 
                 "~trail ~trail", 
                 "~swi ~swi", 
                 "~humans ~humans", 
                 "~trail + swi ~trail + swi",
                 "~swi + humans ~swi + humans", 
                 "~trail + humans ~trail + humans", 
                 "~trail*swi ~trail*swi", 
                 "~swi*humans ~swi*humans",
                 "~trail*humans ~trail*humans")

aictab(cand.set = ml, modnames = Model.names, second.ord=F)
