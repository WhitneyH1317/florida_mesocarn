# Author - Heather Abernathy
# Date - 9/24/2020
# Purpose - format camera data for occupancy models
# Updates - 9/30/2020, I redid the cleaned camera data and added to the deer data sex-specific detections. B/c I think the sexes will respond differently spatially to predation 
# risk and humans depending on the biological season 

require(stringr)
require(move)
require(dplyr)

# read in the corrected, must up-to-date camera data 
fin <- read.table("D:/Dropbox/Graduate_School_PhD/Projects_Papers/Camera_Data/Latest_camera_1_29_20/all_cams_final.txt", header = T)#, quote=F, row.names = F, col.names = c("keywords","tags"))

##########################################
# format all data 
##########################################

#fin <- fin[grep("_B2", fin$File_Name),]

n <- str_sub(fin[,1], start=-8)
# n <- str_sub(fin[,1], start=21)
n2 <- strsplit(n,'.JPG')
n3 <- unlist(n2) #strsplit automatically creates a list, you have to remove this list
n4 <- strsplit(n3,'.jpg')
n5 <- unlist(n4) #strsplit automatically creates a list, you have to remove this list
n6 <- strsplit(n5,'.AVI')
n7 <- unlist(n6) #strsplit automatically creates a list, you have to remove this list

table(n7)

# a7_FP22, 2_B21, and _FP31 need to be pulled out directly 

fin$camID <- n7
fin$camID <- gsub("_", "", fin$camID)

colnames(fin) <- c('File_Name','Keywords',"camID")

to.change <- fin

# remove those that don't contain A, B, or F 
fin <- fin[which(grepl("A", fin$camID) | grepl("B", fin$camID) | grepl("F", fin$camID)),]

fin$camID <- gsub("-17FP31", "FP31", fin$camID)
fin$camID <- gsub("-42FP22", "FP22", fin$camID)
fin$camID <- gsub("0-12B21", "B22", fin$camID)
fin$camID <- gsub("0B2", "B26", fin$camID)
fin$camID <- gsub("1B2", "B26", fin$camID)
fin$camID <- gsub("2B2", "B26", fin$camID)
fin$camID <- gsub("3B2", "B26", fin$camID)
fin$camID <- gsub("4B2", "B26", fin$camID)
fin$camID <- gsub("5B2", "B26", fin$camID)
fin$camID <- gsub("6B2", "B26", fin$camID)
fin$camID <- gsub("7B2", "B26", fin$camID)
fin$camID <- gsub("8B2", "B26", fin$camID)
fin$camID <- gsub("9B2", "B26", fin$camID)
fin$camID <- gsub("B560", "B60", fin$camID)

table(fin$camID)

camNam <- unique(fin$camID)

######################################################################
# separate each species 
######################################################################

fin$deer <- ifelse(grepl("deer", fin$Keywords, ",") ==T,1,0)
fin$deer_male <- ifelse(grepl("bucks=1", fin$Keywords, ",")==T, 1, 0)
fin$deer_female <- ifelse(grepl("does=1", fin$Keywords, ",")==T, 1, 0)
fin$deer_adults <- ifelse(grepl("deer", fin$Keywords, ",") ==T & grepl("spotfawns=1", fin$Keywords, ",")==F, 1, 0)

fin$panther <- ifelse(grepl("panther", fin$Keywords, ",") ==T,1,0)
fin$human <- ifelse(grepl("human", fin$Keywords, ",") ==T,1,0)
fin$vehicle <- ifelse(grepl("vehicle", fin$Keywords, ",") ==T,1,0)

# ---------------------------------------------------------------------------------------------------------------------
# dd a date column 
fin$DT <- as.POSIXct(str_sub(fin$File_Name, end=19), format="%Y-%m-%d-%H-%M-%S", tz = "EST")

fin0 <- fin[which(is.na(fin$DT)),]
fin <- fin[-which(is.na(fin$DT)),]

fin0$DT <- as.POSIXct(str_sub(fin0$File_Name, end=20), format="%Y-%m-%d--%H-%M-%S", tz = "EST")

fin <- rbind.data.frame(fin, fin0)

# filter to match the camera effort 
fin$date <- paste0(as.Date(fin$DT, tz='EST')) 
fin <- fin[which(fin$date >= "2015-01-01" & fin$date <= "2017-12-31"),]

# correct for daylight savings time: -- I think the cameras never subscribed to DST

#DST.S <- c("2015-03-08 02:00:00","2016-03-13 02:00:00","2017-03-12 02:00:00")
#DST.E <- c("2015-11-01 02:00:00","2016-11-06 02:00:00","2017-11-05 02:00:00")

#DST.df <- cbind.data.frame(DST.S, DST.E)

# during mar - nov is DST, so we need to remove an hour from those time slots 

#fin$DT2 <- NA

#for(i in 1:nrow(DST.df)){
  
  #i <- 1
  
 # temp <- DST.df[i,]
  #
  #DOI <- fin[which(fin$DT >= temp$DST.S[1] & fin$DT <= temp$DST.E[1]),][['DT']]
  
  #fin[which(fin$DT >= temp$DST.S[1] & fin$DT <= temp$DST.E[1]),][['DT2']] <- paste0(as.POSIXct((DOI - 3600), tz = 'EST'))
  
#}

#fin$DT2 <- ifelse(is.na(fin$DT2), paste0(as.POSIXct(fin$DT, tz = 'EST')),  paste0(as.POSIXct(fin$DT2, tz = 'EST')))
#fin$DT2 <- as.POSIXct(fin$DT2, tz = 'EST')
#fin$DT <- as.POSIXct(fin$DT, tz = 'EST')

#fin$test <- fin$DT - fin$DT2
#testR <- fin[which(fin$test == 3600),] # it worked great

#fin <- fin[,c(1:10,12:13)]
#colnames(fin)[12] <- 'DT'

#fin$date <- as.Date(fin$DT, tz = 'EST')

# filter to match the camera effort 
fin <- fin[order(fin$camID, fin$DT),]

# save your output - 
# 9/23/2020 - cleaned and truncated to match camera effort 
# 11/21/2022 - corrected for DL savings, but then realized I didn't need too, so data should be the same
saveRDS(fin, "D:/Dropbox/Graduate_School_PhD/Projects_Papers/Humans_Predator_Prey/RDS/cleaned_cam_data_11_21_2022.RDS") 

# update - saved a newer version on 9/30 because I neededc to add male and female deer, respectively
# update 2 - saved a newer version on 10/1 because I needed to include only adults and remove fawns bc
# fawns may be naive tp predators 
# update 3 - instead of humans, I pulled out rabbits, like an asshole, so I need to redo all my human stuff.... :( 
# ---------------------------------------------------------------------------------------------------------------------
# filter detections that are less than 30 minutes apart - 

# combine humans and vehicles for a special case 
fin$human_vehicle <- fin$human + fin$vehicle

# filter for deer 
spl <- fin[which(fin$human_vehicle==1),]
#spl <- spl[,c(3,13,11:12)]
head(spl)

spl$date <- as.Date(spl$date, tz='EST')

#split by camera 
splitdat <- split(spl, spl$camID)

add2 <- dat

for(i in 2:length(splitdat)){
  
  #i<-1
  dat <- as.data.frame(splitdat[[i]])
  dat <- dat[order(dat$DT),]
  dat$difft <- c(0,difftime(dat$DT[2:nrow(dat)], dat$DT[1:nrow(dat)-1], units='mins'))
  
  add2<-rbind(add2,dat)
  
}

#rename
#deer <- add2
#panther <- add2
#human <- add2
#vehicle <- add2
hum_veh <- add2
#deer_male <- add2
#deer_female <- add2
#deer_adults <- add2

saveRDS(deer, "./RDS/raw_data/deer_dat_camID_DT_diff_time_bw_det_in_mins.RDS")
saveRDS(panther, "D:/Dropbox/Graduate_School_PhD/Projects_Papers/Humans_Predator_Prey/RDS/raw_data/panther_dat_camID_DT_diff_time_bw_det_in_mins.RDS")
saveRDS(human, "./RDS/raw_data/human_dat_camID_DT_diff_time_bw_det_in_mins.RDS")
saveRDS(vehicle, "./RDS/raw_data/vehicle_dat_camID_DT_diff_time_bw_det_in_mins.RDS")
saveRDS(hum_veh, "D:/Dropbox/Graduate_School_PhD/Projects_Papers/Humans_Predator_Prey/RDS/raw_data/human_and_vehicle_dat_camID_DT_diff_time_bw_det_in_mins.RDS")
saveRDS(deer_male, "D:/Dropbox/Graduate_School_PhD/Projects_Papers/Humans_Predator_Prey/RDS/raw_data/deer_male_dat_camID_DT_diff_time_bw_det_in_mins.RDS")
saveRDS(deer_female, "D:/Dropbox/Graduate_School_PhD/Projects_Papers/Humans_Predator_Prey/RDS/raw_data/deer_female_dat_camID_DT_diff_time_bw_det_in_mins.RDS")
saveRDS(deer_adults, "./RDS/raw_data/deer_adults_dat_camID_DT_diff_time_bw_det_in_mins.RDS")

# force R to not use e+10 format 
options(scipen=999)

# keep the first row and remove everything else 
# deer 
deer2 <- deer[which(deer$difft > 30),]
deer <- rbind(deer[1,], deer2)

# panther 
panther2 <- panther[which(panther$difft > 30),]
panther <- rbind(panther[1,], panther2)

# human 
human2 <- human[which(human$difft > 30),]
human <- rbind(human[1,], human2)

# vehicle - did not filter this out - 11/22/22
#vehicle2 <- vehicle[which(vehicle$difft > 30),]
#vehicle <- rbind(vehicle[1,], vehicle2)

# human + vehicle
hum_veh2 <- hum_veh[which(hum_veh$difft > 30),]
hum_veh <- rbind(hum_veh[1,], hum_veh2)

# male deer
deer_male2 <- deer_male[which(deer_male$difft > 30),]
deer_male <- rbind(deer_male[1,], deer_male2)

# female deer
deer_female2 <- deer_female[which(deer_female$difft > 30),]
deer_female <- rbind(deer_female[1,], deer_female2)

# adult deer
deer_adults2 <- deer_adults[which(deer_adults$difft > 30),]
deer_adults <- rbind(deer_adults[1,], deer_adults2)


# save cleaned version

saveRDS(deer, "./RDS/raw_data/deer_dat_camID_DT_diff_time_bw_det_below_30min_rm.RDS")
saveRDS(panther, "D:/Dropbox/Graduate_School_PhD/Projects_Papers/Humans_Predator_Prey/RDS/raw_data/panther_dat_camID_DT_diff_time_bw_det_below_30min_rm.RDS")
saveRDS(human, "./RDS/raw_data/human_dat_camID_DT_diff_time_bw_det_below_30min_rm.RDS")
#saveRDS(vehicle, "./RDS/raw_data/vehicle_dat_camID_DT_diff_time_bw_det_below_30min_rm.RDS")
saveRDS(hum_veh, "./RDS/raw_data/human_and_vehicle_dat_camID_DT_diff_time_bw_det_below_30min_rm.RDS")
saveRDS(deer_male, "D:/Dropbox/Graduate_School_PhD/Projects_Papers/Humans_Predator_Prey/RDS/raw_data/deer_male_dat_camID_DT_diff_time_bw_det_below_30min_rm.RDS")
saveRDS(deer_female, "D:/Dropbox/Graduate_School_PhD/Projects_Papers/Humans_Predator_Prey/RDS/raw_data/deer_female_dat_camID_DT_diff_time_bw_det_below_30min_rm.RDS")
saveRDS(deer_adults, "./RDS/raw_data/deer_adults_dat_camID_DT_diff_time_bw_det_below_30min_rm.RDS")

# ------------------------------------------------------------------------------------------------------------------

# sum deer detections by camera and day bin the cameras by day to match our surface water data 
#deer2 <- data.frame(deer %>% group_by(camID)) 
deer2 <- data.frame(deer %>% group_by(camID, date) %>% summarize_at(vars(deer), sum)) #  summarise_each(funs(sum)))
panther2 <- data.frame(panther %>% group_by(camID, date) %>% summarize_at(vars(panther), sum)) #  summarise_each(funs(sum)))
human2 <- data.frame(human %>% group_by(camID, date) %>% summarize_at(vars(human), sum)) #  summarise_each(funs(sum)))
vehicle2 <- data.frame(vehicle %>% group_by(camID, date) %>% summarize_at(vars(vehicle), sum)) #  summarise_each(funs(sum)))
deer_male2 <- data.frame(deer_male %>% group_by(camID, date) %>% summarize_at(vars(deer_male), sum))
#deer_male$date <- as.Date(deer_male$DT, tz='EST')
deer_female2 <- data.frame(deer_female %>% group_by(camID, date) %>% summarize_at(vars(deer_female), sum))
deer_adults2 <- data.frame(deer_adults %>% group_by(camID, date) %>% summarize_at(vars(deer_adults), sum))

hum_veh2 <- data.frame(hum_veh %>% group_by(camID, date) %>% summarize_at(vars(human_vehicle), sum)) #  summarise_each(funs(sum)))


# save cleaned version

saveRDS(deer2, "./RDS/raw_data/deer_dat_camID_DT_diff_time_bw_det_below_30min_rm_summed_by_day.RDS")
saveRDS(panther2, "./RDS/raw_data/panther_dat_camID_DT_diff_time_bw_det_below_30min_rm_summed_by_day.RDS")
saveRDS(human2, "./RDS/raw_data/human_dat_camID_DT_diff_time_bw_det_below_30min_rm_summed_by_day.RDS")
saveRDS(vehicle2, "./RDS/raw_data/vehicle_dat_camID_DT_diff_time_bw_det_below_30min_rm_summed_by_day.RDS")
#saveRDS(hum_veh2, "./RDS/raw_data/human_and_vehicle_dat_camID_DT_diff_time_bw_det_below_30min_rm_summed_by_day.RDS")
saveRDS(deer_male2, "./RDS/raw_data/deer_male_dat_camID_DT_diff_time_bw_det_below_30min_rm_summed_by_day.RDS")
saveRDS(deer_female2, "./RDS/raw_data/deer_female_dat_camID_DT_diff_time_bw_det_below_30min_rm_summed_by_day.RDS")
saveRDS(deer_adults2, "./RDS/raw_data/deer_adults_dat_camID_DT_diff_time_bw_det_below_30min_rm_summed_by_day.RDS")

# ------------------------------------------------------------------------------------------------------------------
# for each I need to represent the day of the study and account for camera effort 

# bring in capture effort
ce <- read.csv("D:/Dropbox/Graduate_School_PhD/Projects_Papers/Camera_Data/camera_operational_matrix.csv")

ce2 <- data.frame(t(ce[,c(1,6:ncol(ce))]))
colnames(ce2) <- paste0(ce2[1,])

# bring in the unique camera names saved from ealier and rename the cols 
colnames(ce2) <- camNam[order(as.character(camNam))]

rownames(ce2)[2:nrow(ce2)] <- gsub("X","", rownames(ce2)[2:nrow(ce2)])
ce3 <- ce2[-1,]

ce3$date  <- strptime(rownames(ce2)[2:nrow(ce2)], format="%d.%b.%Y")
rownames(ce3) <- ce3$date

#ce3 <- ce3[,-c(ncol(ce3))]

# ------------------------------------------------------------------------------------------------------------------
# now I need to make a camera-specific day of study effort -
# if day is not in the data, add it. 
# then determine if camera was opperational (1) or not (0)

#deer2 <- readRDS("./RDS/raw_data/deer_dat_camID_DT_diff_time_bw_det_below_30min_rm_summed_by_day.RDS") # done 9/25/2020
# panther2 <- readRDS("./RDS/raw_data/panther_dat_camID_DT_diff_time_bw_det_below_30min_rm_summed_by_day.RDS") # done 9/25/2020
# human2 <- readRDS("./RDS/raw_data/human_dat_camID_DT_diff_time_bw_det_below_30min_rm_summed_by_day.RDS")
# vehicle2 <- readRDS("./RDS/raw_data/vehicle_dat_camID_DT_diff_time_bw_det_below_30min_rm_summed_by_day.RDS")
# hum_veh2 <- readRDS("./RDS/raw_data/human_and_vehicle_dat_camID_DT_diff_time_bw_det_below_30min_rm_summed_by_day.RDS")
# deer_male2 <- readRDS("./RDS/raw_data/deer_male_dat_camID_DT_diff_time_bw_det_below_30min_rm_summed_by_day.RDS")
# deer_female2 <- readRDS("./RDS/raw_data/deer_female_dat_camID_DT_diff_time_bw_det_below_30min_rm_summed_by_day.RDS")
# deer_adults2 <- readRDS("./RDS/raw_data/deer_adults_dat_camID_DT_diff_time_bw_det_below_30min_rm_summed_by_day.RDS")


AOI <- hum_veh2

finer <- NULL

# it looks like when cond 1 = T cameras that have detections seem to be duplicating 17xs

for(i in 1:nrow(ce3)){
  
  #i <- 45
  
  temp <- ce3[i,]
  
  daylist <- split(AOI, AOI$date)
  
  animal <- colnames(daylist[[1]][3])
  
    message(".. starting i = ", i, ", date: ", rownames(temp))
  
  if(rownames(temp) %in% names(daylist)){ # is the current day in the list for possible days that animal/human was observed? If yes, below 
    
    jemp <- daylist[[which(names(daylist)==rownames(temp))]] # extract the camera locations from that day 
    
    DIQ <- rownames(temp) # extract the date for indexing purposes 
    DIQ.frame <- ce3[which(ce3$date==DIQ),] # extract the camera effort for every camera on that day 
    
    output <- date.exisits(droplevels(DIQ.frame), jemp, animal) # custom function that assigns camera effort to that day (see below)
    
    output[order(output$camID),]
    
    finer <- rbind.data.frame(finer, output)
    rm(output)
    
  }else if(!rownames(temp) %in% names(daylist)){ # is the current day in the list for possible days that animal/human was observed? If no, below 
    
    DIQ <- rownames(temp) # extract the date for indexing purposes 
    DIQ.frame <- ce3[which(ce3$date==DIQ),] # extract the camera effort for every camera on that day 
    
    output <- date.does.not.exisits(droplevels(DIQ.frame), animal, colnames(daylist[[1]]))   # custom function that assigns camera effort to that day (see below)
    
    finer <- rbind.data.frame(finer, output)
    rm(output)
    
  }else{ # break for exceptions
    
    message(".. broke at i = ", i)
    break # broke at i = 
    
  }
  
  
}

# write it out to double-check 

# saveRDS(finer, "./RDS/occu_data/deer_occ_data.RDS") # created on 9/25/2020
# saveRDS(finer, "./RDS/occu_data/panther_occ_data.RDS") # created on 9/25/2020
# saveRDS(finer, "./RDS/occu_data/human_occ_data.RDS") # created on 9/25/2020
# saveRDS(finer, "./RDS/occu_data/vehicle_occ_data.RDS") # created on 9/25/2020
saveRDS(finer, "./RDS/occu_data/human_and_vehicle_occ_data.RDS") # created on 9/25/2020, updated on 11/16/2020
#saveRDS(finer, "./RDS/occu_data/deer_female_occ_data.RDS") 


testR <- finer[which(finer$human_vehicle > 300),]


#saved on 11/22/22 to update the data with unfiltered detections of humans to be used in the diel activity overlap analysis, wherein 
# I look at overlap ~ mean human detections per camera
saveRDS(finer, "D:/Dropbox/Graduate_School_PhD/Projects_Papers/Humans_Predator_Prey/RDS/occu_data/human_and_vehicle_occ_data_no30minfilter.RDS") # created on 9/25/2020, updated on 11/16/2020


# Debugging - 

#DIQ <- rownames(temp) # extract the date for indexing purposes 
#DIQ.frame <- ce3[which(ce3$date==DIQ),] # extract the camera effort for every camera on that day 

#output <- date.does.not.exisits(droplevels(DIQ.frame), animal, colnames(daylist[[1]]))   # custom function that assigns camera effort to that day (see below)

#x <- DIQ.frame
#y <- animal 
#animal <- colnames(daylist[[1]])

# something is wrong in this code, de-bug it 
date.exisits <- function(x, y, animal){
  
  x <- data.frame(droplevels(DIQ.frame))
  y <- jemp
  animal <- colnames(y)[3]
  
  adder <- NULL
  
  for(i in 1:180){ # for every column (all cameras (180); camera and its corresponding effort) in this date row, determine... (see the following logical statements) 
    
    #i <- 1
    
    tempx <- x[,i] # effort
    cam <- colnames(x)[i] # camera name 
    
    if(tempx == 0){ # if the camera was not opperational, add it but call it NA
      
      camID <- cam
      date <- rownames(x)
      animal.temp <- NA
      
      toadd <- cbind.data.frame(camID, date, animal.temp)
      colnames(toadd) <- colnames(y)
      
      toadd$date <- as.Date(toadd$date, tz="EST")
      
      adder <- rbind.data.frame(adder, toadd)
      rm(toadd)
      
    }else if((!cam %in% y$camID) & tempx == 1){ # if the col is not within the camIDs of that day But the camera was opperational, add it and call it 0
      
      camID <- cam
      date <- rownames(x)
      animal.temp <- 0
      
      toadd <- cbind.data.frame(camID, date, animal.temp)
      colnames(toadd) <- colnames(y)
      
      toadd$date <- as.Date(toadd$date, tz="EST")
      
      adder <- rbind.data.frame(adder, toadd)
      rm(toadd)
      
    }else if((cam %in% y$camID) & tempx == 1){ # of all possible cameras, do any match the current i? 
      
      # filter cam from avail data 
      y2 <- y[which(y$camID == cam),]
      
      toadd <- y2#y2[colnames(y2) == animal][[1]]
      
      adder <- rbind.data.frame(adder, toadd)
      rm(toadd)
      
    }else{
      
      adder <- message("...help! i = ", i, " in the function loop broke me")
      
    } 
    
  }
  
  return(adder)
  
  
  
}

date.does.not.exisits <- function(x, animal, CN){
  
  x <- data.frame(droplevels(DIQ.frame))
  CN <- colnames(daylist[[1]])
  animal <- animal
  
  adder <- NULL
  
  for(i in 1:180){ # for every column (all cameras (180); camera and its corresponding effort) in this date row, determine... (see the following logical statements) 
    
    #i <- 1
    
    tempx <- x[,i] # effort
    cam <- colnames(x)[i] # camera name 
    
    if(tempx == 0){ # if the camera was not opperational, add it but call it NA
      
      camID <- cam
      date <- rownames(x)
      animal.temp <- NA
      
      toadd <- cbind.data.frame(camID, date, animal.temp)
      colnames(toadd) <- CN
      
      toadd$date <- as.Date(toadd$date, tz="EST")
      
      adder <- rbind.data.frame(adder, toadd)
      rm(toadd)
      
    }else if(tempx == 1){ # if the camera was opperational, add it and call it 0
      
      camID <- cam
      date <- rownames(x)
      animal.temp <- 0
      
      toadd <- cbind.data.frame(camID, date, animal.temp)
      colnames(toadd) <- CN
      
      toadd$date <- as.Date(toadd$date, tz="EST")
      
      adder <- rbind.data.frame(adder, toadd)
      rm(toadd)
      
    }else{
      
      adder <- message("...help! i = ", i, " in the function loop broke me")
      
    } 
    
  }
  
  return(adder)
  
  
  
}

# data vetting - 

#dat <- readRDS("./RDS/occu_data/vehicle_occ_data.RDS") # dee, panther, vehicle 'look' okay


