
# rut season, females - combine three years by stacking the year to be new 'sites' but year must be included as a fixed effect - 

# Occu data -----------------------------------------------------------------------------------------------------------------

# deer -------------------------------------------------------

setwd('D:/Dropbox/Graduate_School_PhD/Projects_Papers/Humans_Predator_Prey')

deer <- readRDS("./RDS/OccuMulti_data/deer/fdeer_all_seasons.RDS")

# create wet and dry seasons
# Feb 1 - Jun 30 = dry 
# July 1 - Jan 30 = wet

# Wet season pull -
deer <- deer[which(grepl('-01-', gsub('_to_', ' ', colnames(deer))) |
                     grepl('-07-', gsub('_to_', ' ', colnames(deer))) | 
                     grepl('-08-', gsub('_to_', ' ', colnames(deer))) |
                     grepl('-09-', gsub('_to_', ' ', colnames(deer))) |
                     grepl('-10-', gsub('_to_', ' ', colnames(deer))) |
                     grepl('-11-', gsub('_to_', ' ', colnames(deer))) |
                     grepl('-12-', gsub('_to_', ' ', colnames(deer))) )]

# 2015
deer15 <- deer[which(grepl('2015-', gsub('_to_', ' ', colnames(deer))))]
# it looks like the survey intervals are off by a week cause of 2016, thus remove
# colnames = "2015-06-25_to_2015-07-01" to align them more spatially 
deer15 <- deer15[-which(colnames(deer15) == "2015-06-25_to_2015-07-01" )]
colnames(deer15) <- paste0("V", seq(1:ncol(deer15)))

# 2016
deer16 <- deer[which(grepl('2016-', gsub('_to_', ' ', colnames(deer))))]
deer16[,31] <- deer16[,31] + deer16[,32]
deer16 <- deer16[,c(1:31)]
colnames(deer16) <- paste0("V", seq(1:ncol(deer16)))

# 2017
deer17 <- deer[which(grepl('2017-', gsub('_to_', ' ', colnames(deer))))]
# it looks like the survey intervals are off by a week cause of 2016, thus remove
# colnames = "2015-06-25_to_2015-07-01" to align them more spatially 
deer17 <- deer17[-which(colnames(deer17) == "2017-06-25_to_2017-07-01" )]
colnames(deer17) <- paste0("V", seq(1:ncol(deer17)))

# all years
deer.comb <- rbind.data.frame(deer15, deer16, deer17)

# anything above 0 gets a one, unless its an NA
deer.comb[deer.comb > 0 & !is.na(deer.comb)] <- 1 

# panthers -----------------------------------------------------

panther <- readRDS("./RDS/OccuMulti_data/panther/panther_all_seasons.RDS")

# Wet season pull -
panther <- panther[which(grepl('-01-', gsub('_to_', ' ', colnames(panther))) |
                           grepl('-07-', gsub('_to_', ' ', colnames(panther))) | 
                           grepl('-08-', gsub('_to_', ' ', colnames(panther))) |
                           grepl('-09-', gsub('_to_', ' ', colnames(panther))) |
                           grepl('-10-', gsub('_to_', ' ', colnames(panther))) |
                           grepl('-11-', gsub('_to_', ' ', colnames(panther))) |
                           grepl('-12-', gsub('_to_', ' ', colnames(panther))) )]


# 2015
panther15 <- panther[which(grepl('2015-', gsub('_to_', ' ', colnames(panther))))]
# it looks like the survey intervals are off by a week cause of 2016, thus remove
# colnames = "2015-06-25_to_2015-07-01" to align them more spatially 
panther15 <- panther15[-which(colnames(panther15) == "2015-06-25_to_2015-07-01" )]
colnames(panther15) <- paste0("V", seq(1:ncol(panther15)))

# 2016
panther16 <- panther[which(grepl('2016-', gsub('_to_', ' ', colnames(panther))))]
panther16[,31] <- panther16[,31] + panther16[,32]
panther16 <- panther16[,c(1:31)]
colnames(panther16) <- paste0("V", seq(1:ncol(panther16)))

# 2017
panther17 <- panther[which(grepl('2017-', gsub('_to_', ' ', colnames(panther))))]
# it looks like the survey intervals are off by a week cause of 2016, thus remove
# colnames = "2015-06-25_to_2015-07-01" to align them more spatially 
panther17 <- panther17[-which(colnames(panther17) == "2017-06-25_to_2017-07-01" )]
colnames(panther17) <- paste0("V", seq(1:ncol(panther17)))

# all years
panther.comb <- rbind.data.frame(panther15, panther16, panther17)

# anything above 0 gets a one, unless its an NA
panther.comb[panther.comb > 0 & !is.na(panther.comb)] <- 1 

# detection covars ----------------------------------------------------------------------------------------------------------

# human activity for detection rates  -----------------------------------------

# human <- readRDS("./RDS/OccuMulti_data/human/hum_vehicle_all_seasons.RDS")

# re-done on 11/23 to not filter for independent detections nor weekly >0 to 1
human <- readRDS("./RDS/OccuMulti_data/human/hum_veh_not_filtered_all_seasons.RDS")

# Wet season pull -
human <- human[which(grepl('-01-', gsub('_to_', ' ', colnames(human))) |
                       grepl('-07-', gsub('_to_', ' ', colnames(human))) | 
                       grepl('-08-', gsub('_to_', ' ', colnames(human))) |
                       grepl('-09-', gsub('_to_', ' ', colnames(human))) |
                       grepl('-10-', gsub('_to_', ' ', colnames(human))) |
                       grepl('-11-', gsub('_to_', ' ', colnames(human))) |
                       grepl('-12-', gsub('_to_', ' ', colnames(human))) )]

# 2015
human15 <- human[which(grepl('2015-', gsub('_to_', ' ', colnames(human))))]
# it looks like the survey intervals are off by a week cause of 2016, thus remove
# colnames = "2015-06-25_to_2015-07-01" to align them more spatially 
human15 <- human15[-which(colnames(human15) == "2015-06-25_to_2015-07-01" )]
colnames(human15) <- paste0("V", seq(1:ncol(human15)))

# 2016
human16 <- human[which(grepl('2016-', gsub('_to_', ' ', colnames(human))))]
human16[,31] <- human16[,31] + human16[,32]
human16 <- human16[,c(1:31)]
colnames(human16) <- paste0("V", seq(1:ncol(human16)))

# 2017
human17 <- human[which(grepl('2017-', gsub('_to_', ' ', colnames(human))))]
# it looks like the survey intervals are off by a week cause of 2016, thus remove
# colnames = "2015-06-25_to_2015-07-01" to align them more spatially 
human17 <- human17[-which(colnames(human17) == "2017-06-25_to_2017-07-01" )]
colnames(human17) <- paste0("V", seq(1:ncol(human17)))


# all years
human.comb <- rbind.data.frame(human15, human16, human17)

# anything above 0 gets a one, unless its an NA
human.comb[is.na(human.comb)] <- NA

human.comb <- scale.default(human.comb, center = T, scale=T)

# surface water ---------------------------------------------------------------

swi <- readRDS("./RDS/OccuMulti_data/SWI/full_study_weekly_SWI.RDS") 

# Wet season pull -
swi <- swi[which(grepl('-01-', gsub('_to_', ' ', colnames(swi))) |
                   grepl('-07-', gsub('_to_', ' ', colnames(swi))) | 
                   grepl('-08-', gsub('_to_', ' ', colnames(swi))) |
                   grepl('-09-', gsub('_to_', ' ', colnames(swi))) |
                   grepl('-10-', gsub('_to_', ' ', colnames(swi))) |
                   grepl('-11-', gsub('_to_', ' ', colnames(swi))) |
                   grepl('-12-', gsub('_to_', ' ', colnames(swi))) )]


# 2015
swi15 <- swi[which(grepl('2015-', gsub('_to_', ' ', colnames(swi))))]
# it looks like the survey intervals are off by a week cause of 2016, thus remove
# colnames = "2015-06-25_to_2015-07-01" to align them more spatially 
swi15 <- swi15[-which(colnames(swi15) == "2015-06-25_to_2015-07-01" )]
colnames(swi15) <- paste0("V", seq(1:ncol(swi15)))

# 2016
swi16 <- swi[which(grepl('2016-', gsub('_to_', ' ', colnames(swi))))]
swi16[,31] <- swi16[,31] + swi16[,32]
swi16 <- swi16[,c(1:31)]
colnames(swi16) <- paste0("V", seq(1:ncol(swi16)))

# 2017
swi17 <- swi[which(grepl('2017-', gsub('_to_', ' ', colnames(swi))))]
# it looks like the survey intervals are off by a week cause of 2016, thus remove
# colnames = "2015-06-25_to_2015-07-01" to align them more spatially 
swi17 <- swi17[-which(colnames(swi17) == "2017-06-25_to_2017-07-01" )]
colnames(swi17) <- paste0("V", seq(1:ncol(swi17)))


swi.comb <- rbind.data.frame(swi15, swi16, swi17)
#swi.comb[1:180,53] <- swi.comb[1:180,52]
#swi.comb[361:nrow(swi.comb),53] <- swi.comb[361:nrow(swi.comb),52] 

# trail cams ------------------------------------------------------------------

cam <- readRDS("./RDS/camera_loc_trail_info.RDS")

# on trail = 1, off = 0
cam$Trail <- ifelse(cam$Trail=='on', 1, 0)

# create a blank matrix of date columns and first column of named cameras - 
on.off.trail <- data.frame(matrix(NA, nrow = 180, ncol = ncol(swi15)))

#on.off.trail[,1] <- cam[,1]
on.off.trail[,1:ncol(on.off.trail)] <- cam[,3]

colnames(on.off.trail) <- paste0("V", seq(1:ncol(on.off.trail)))

# rep for all three years - 
trl <- rbind.data.frame(on.off.trail, on.off.trail, on.off.trail)

# combine them ---------------------------------------------------------------

oc <- list(trl, swi.comb, human.comb)
names(oc) <- c("trail","swi","humans")

# site covars --------------------------------------------------------------------------------------------------------------

# 2015
# distance to high/low cams 
require(raster)
hh  <- raster('./Spatial/E_dist/hh_ot15')
lh  <- raster('./Spatial/E_dist/lh_ot15')

dat <- readRDS("./RDS/camera_loc_trail_info.RDS")
dat.fin <- SpatialPointsDataFrame(coords=dat[,c('UTME','UTMN')], data = dat, proj4string = CRS("+proj=utm +zone=17 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

dat.fin@data$hh <- raster::extract(hh, dat.fin, method="simple")
dat.fin@data$lh <- raster::extract(lh, dat.fin, method="simple")

dat.fin@data$hh_sc <- (dat.fin@data$hh - mean(dat.fin@data$hh, na.rm=TRUE)) / (sd(dat.fin@data$hh, na.rm=TRUE))
dat.fin@data$lh_sc <- (dat.fin@data$lh - mean(dat.fin@data$lh, na.rm=TRUE)) / (sd(dat.fin@data$lh, na.rm=TRUE)) 

dat.fin15 <- data.frame(dat.fin@data)

# human activity rates 
HAR15 <- readRDS("./RDS/OccuMulti_data/human_activity/wet_2015_human_activity.RDS")

# habitat data  
hab15 <- readRDS("./RDS/OccuMulti_data/SiteCovars/cam_habitat_dat.RDS")
hab15 <- merge(hab15, dat.fin15, by="Camera")

hab15 <- hab15[,c(16:25,27,34:35)]

sc15 <- cbind.data.frame(data.frame(on.off.trail[,1]), data.frame(rowMeans(swi15, na.rm=T)), data.frame(HAR15[,2]), hab15)
colnames(sc15) <- c("trail_status","swi_mean","hum_ar", colnames(hab15))
sc15$year <- '2015'

# 2016
# distance to high/low cams  
hh  <- raster('./Spatial/E_dist/hh_ot16')
lh  <- raster('./Spatial/E_dist/lh_ot16')

dat <- readRDS("./RDS/camera_loc_trail_info.RDS")
dat.fin <- SpatialPointsDataFrame(coords=dat[,c('UTME','UTMN')], data = dat, proj4string = CRS("+proj=utm +zone=17 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

dat.fin@data$hh <- raster::extract(hh, dat.fin, method="simple")
dat.fin@data$lh <- raster::extract(lh, dat.fin, method="simple")

dat.fin@data$hh_sc <- (dat.fin@data$hh - mean(dat.fin@data$hh, na.rm=TRUE)) / (sd(dat.fin@data$hh, na.rm=TRUE))
dat.fin@data$lh_sc <- (dat.fin@data$lh - mean(dat.fin@data$lh, na.rm=TRUE)) / (sd(dat.fin@data$lh, na.rm=TRUE)) 

dat.fin16 <- data.frame(dat.fin@data)
#dat.fin16 <- dat.fin16[-61,]

HAR16 <- readRDS("./RDS/OccuMulti_data/human_activity/wet_2016_human_activity.RDS")
#HAR16 <- HAR16[-61,]

# habitat data and probability of death 
hab16 <- readRDS("./RDS/OccuMulti_data/SiteCovars/cam_habitat_dat.RDS")
hab16 <- merge(hab16, dat.fin16, by="Camera")

hab16 <- hab16[,c(16:25,27,34:35)]
# hab16 <- hab16[-61,] --> don't need to do this b/c the merge takes care of excluding the one with NAs

sc16 <- cbind.data.frame(data.frame(on.off.trail[,1]), data.frame(rowMeans(swi16, na.rm=T)), data.frame(HAR16[,2]), hab16)
colnames(sc16) <- c("trail_status","swi_mean","hum_ar", colnames(hab16))
sc16$year <- '2016'

# 2017
# distance to high/low cams 
hh  <- raster('./Spatial/E_dist/hh_ot17')
lh  <- raster('./Spatial/E_dist/lh_ot17')

dat <- readRDS("./RDS/camera_loc_trail_info.RDS")
dat.fin <- SpatialPointsDataFrame(coords=dat[,c('UTME','UTMN')], data = dat, proj4string = CRS("+proj=utm +zone=17 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

dat.fin@data$hh <- raster::extract(hh, dat.fin, method="simple")
dat.fin@data$lh <- raster::extract(lh, dat.fin, method="simple")

dat.fin@data$hh_sc <- (dat.fin@data$hh - mean(dat.fin@data$hh, na.rm=TRUE)) / (sd(dat.fin@data$hh, na.rm=TRUE))
dat.fin@data$lh_sc <- (dat.fin@data$lh - mean(dat.fin@data$lh, na.rm=TRUE)) / (sd(dat.fin@data$lh, na.rm=TRUE)) 

dat.fin17 <- data.frame(dat.fin@data)

HAR17 <- readRDS("./RDS/OccuMulti_data/human_activity/wet_2017_human_activity.RDS")

# habitat data and probability of death 
hab17 <- readRDS("./RDS/OccuMulti_data/SiteCovars/cam_habitat_dat.RDS")
hab17 <- merge(hab17, dat.fin17, by="Camera")

hab17 <- hab17[,c(16:25,27,34:35)]

sc17 <- cbind.data.frame(data.frame(on.off.trail[,1]), data.frame(rowMeans(swi17, na.rm=T)), data.frame(HAR17[,2]), hab17)
colnames(sc17) <- c("trail_status","swi_mean","hum_ar", colnames(hab17))
sc17$year <- '2017'

# combine the covars
sc.comb <- rbind.data.frame(sc15, sc16, sc17)

# ----------------------------------------------------------------------------------------------------------------------------------
# habitat variables correlated?
cor.d <- cor(sc.comb[,1:16], method=c("pearson"))
cor.d[cor.d<0.5] <- NA # none used in models below are related 
(cor.d)

# Final formatting and run some models - 

pan <- as.matrix(panther.comb)
md <- as.matrix(deer.comb)
#hum <- as.matrix(human2)

require(unmarked)

y <- list(md, pan) #, hum)

sc.comb$year <- as.factor(sc.comb$year)

dat <- unmarkedFrameOccuMulti(y=y, obsCovs=oc, siteCovs=sc.comb)

summary(dat) # deer = spp1, panther = spp2
plot(dat)

options(scipen = 99)

# saved 10/5/2022
# re-saved on 11/23/2022 - did not filter human detections -- not filtered for independent detections nor weekly >0 to 1
saveRDS(sc.comb, 'D:/Dropbox/Graduate_School_PhD/Projects_Papers/Humans_Predator_Prey/Results/revision_results/CoOccur_Datasets/femaleWS_CO_siteDataset.RDS')
saveRDS(oc, 'D:/Dropbox/Graduate_School_PhD/Projects_Papers/Humans_Predator_Prey/Results/revision_results/CoOccur_Datasets/femaleWS_CO_detDataset.RDS')


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

#
# OCCU -------------------------------------------------------------------------
#
# FM1 # ------------------------------------------------------------------------

occFormulas <- c('~year','~year','~swi_mean*hum_ar + year')  # + hum_ar + rds_sc

detFormulas <- c('~trail + swi + humans','~trail + swi + humans')

fm1 <- occuMulti(detFormulas, occFormulas, dat)

# FM2 # ----------------------------------------------------------

occFormulas <- c('~year','~year','~swi_mean + year')  # + hum_ar + rds_sc

detFormulas <- c('~trail + swi + humans','~trail + swi + humans')

fm2 <- occuMulti(detFormulas, occFormulas, dat)

# FM3 # ----------------------------------------------------------

occFormulas <- c('~year','~year','~hum_ar + year')  # + hum_ar + rds_sc

detFormulas <- c('~trail + swi + humans','~trail + swi + humans')

fm3 <- occuMulti(detFormulas, occFormulas, dat)

# FM4 # ----------------------------------------------------------

occFormulas <- c('~year','~year','~1')  # + hum_ar + rds_sc

detFormulas <- c('~trail + swi + humans','~trail + swi + humans')

fm4 <- occuMulti(detFormulas, occFormulas, dat)

# FM5 # ----------------------------------------------------------

occFormulas <- c('~year','~year','~swi_mean + hum_ar + year')  # + hum_ar + rds_sc

detFormulas <- c('~trail + swi + humans','~trail + swi + humans')

fm5 <- occuMulti(detFormulas, occFormulas, dat)

# FM6 # ----------------------------------------------------------

occFormulas <- c('~1','~1','~swi_mean*hum_ar + year')  # + hum_ar + rds_sc

detFormulas <- c('~trail + swi + humans','~trail + swi + humans')

fm6 <- occuMulti(detFormulas, occFormulas, dat)

# FM7 # ----------------------------------------------------------

occFormulas <- c('~1','~1','~swi_mean + year')  # + hum_ar + rds_sc

detFormulas <- c('~trail + swi + humans','~trail + swi + humans')

fm7 <- occuMulti(detFormulas, occFormulas, dat)

# FM8 # ----------------------------------------------------------

occFormulas <- c('~1','~1','~hum_ar + year')  # + hum_ar + rds_sc

detFormulas <- c('~trail + swi + humans','~trail + swi + humans')

fm8 <- occuMulti(detFormulas, occFormulas, dat)

# FM9 # ----------------------------------------------------------

occFormulas <- c('~1','~1','~swi_mean + hum_ar + year')  # + hum_ar + rds_sc

detFormulas <- c('~trail + swi + humans','~trail + swi + humans')

fm9 <- occuMulti(detFormulas, occFormulas, dat)

# Null # ---------------------------------------------------------

occFormulas <- c('~1','~1','~1') 

detFormulas <- c('~1','~ 1')

null <- occuMulti(detFormulas, occFormulas, dat)

# AIC MODEL AVG # ------------------------------------------------

require(AICcmodavg)

ml <- list(null, fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9) 

Model.names <- c("~1 ~1 ~1", "~Year ~Year ~Humans*SWI + Year", "~Year ~Year ~SWI + Year", "~Year ~Year ~Humans + Year", "~Year ~Year ~1", "~Year ~Year ~SWI + Humans + Year",
                 "~1 ~1 ~Humans*SWI + Year", "~1 ~1 ~SWI + Year", "~1 ~1 ~Humans + Year", "~1 ~1 ~SWI + Humans + Year")

aictab(cand.set = ml, modnames = Model.names, second.ord=F)

# -------------------------------------------------------------------------------
# combine the two to make a top model - 

occFormulas <- c('~1','~1','~swi_mean + hum_ar + year')  # + hum_ar + rds_sc

detFormulas <- c('~trail + swi + humans','~trail + swi + humans')

global <- occuMulti(detFormulas, occFormulas, dat)

# saved the top models for occu and detection - 12/4/2022
saveRDS(global, 'D:/Dropbox/Graduate_School_PhD/Projects_Papers/Humans_Predator_Prey/Results/revision_results/CoOccur_ModelOutputs/females_WS_top.RDS')

# format into a pretty table # --------------------------------------------------------------------

# occupancy table ---------------------------------------------------------------------------------
CI.o <- data.frame(confint(global, type=c("state"), level=0.95))
#confint(global, type=c("state"), level=0.95)

summaryOD(global)

df <- summary(global)[[1]]
df.occ <- cbind.data.frame(df, CI.o)
df.occ$covars <- rownames(df.occ)

df.occ$spp <- NA
df.occ$spp[1] <- 'Fdeer'
df.occ$spp[2] <- 'Panther'
df.occ$spp[3:nrow(df.occ)] <- 'Fdeer:Panther'

df.occ2 <- df.occ[,c(7:8, 1:2,4:6)]

df.occ2$covars <- gsub("\\[[^\\]+\\]\\s", '', df.occ2$covars)

colnames(df.occ2) <- c('Covars',  'Species', 'Estimate', 'SE', 'P-value', 'lowerCI', 'upperCI')
occ.tab <- df.occ2

# detection table ----------------------------------------------------------------------------------
CI.o <- data.frame(confint(global, type=c("det"), level=0.95))

df <- summary(global)[[2]]
df.occ <- cbind.data.frame(df, CI.o)
df.occ$covars <- rownames(df.occ)

df.occ$covars <- gsub("\\[[^\\]+\\]\\s", '', df.occ$covars)

df.occ$spp <- 'Fdeer'
df.occ$spp[5:8] <- 'panther'

df.occ2 <- df.occ[,c(7:8, 1:2,4:6)]

colnames(df.occ2) <- c('Covars', 'Species', 'Estimate', 'SE', 'P-value', 'lowerCI', 'upperCI')
det.tab <- df.occ2



