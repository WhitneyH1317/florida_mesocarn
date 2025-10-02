#Flooding rasters
hyd.dir <- "./SurWaterIndex_RastersUTM_updated_3_27_19"

hydroList <- list.files('./SurWaterIndex_RastersUTM_updated_3_27_19', pattern = '.tif', full.names = T)
#hydroList <- hydroList[-which(grepl('.ovr',hydroList)==T)]
file.name <- list.files('./SurWaterIndex_RastersUTM_updated_3_27_19', pattern = '.tif')
#file.name <- file.name[-which(grepl('.ovr',file.name)==T)]
water.df <- data.frame("file.path" = hydroList, "date" = as.Date(gsub('.tif','',file.name), 
                                                                 format = "%B-%d-%Y",
                                                                 tz='EST'))

sl <- split(allData, allData$ID)

fin2 <- NULL

##### Add date-specific hydrology data to steps #####
for(i in 1:length(sl)){
  i <- 90
  individual <- sl[[i]]
  individual$date <- as.Date(individual$DT, tz='EST')
  dayList <- split(individual, individual$date)
  dayList[[1]]
  
  fin <- NULL
  
  for(d in 1:length(dayList)){
    d <-632
    message('Deer # ',i,' of ',length(sl),' Day ',d,' of ',length(dayList))
    ras <- raster::raster(as.character(water.df[which(as.character(water.df$date)==names(dayList)[d]),'file.path']))
    names(ras) <- 'surface_water'
    sp.DL <- SpatialPointsDataFrame(coords=cbind(dayList[[d]]$x2_, dayList[[d]]$y2_), proj4string=CRS("+init=epsg:26917"), data=dayList[[d]])
    sp.DL <- spTransform(sp.DL, ras@crs)
    sur_wat <- raster::extract(ras, sp.DL)
    
    comb <- cbind(dayList[[d]], sur_wat)
    fin <- rbind(fin, comb)
    
    #rm(ras)
  }
  
  fin2 <- rbind(fin, fin2)
  rm(dayList, fin, ras)
  
}


