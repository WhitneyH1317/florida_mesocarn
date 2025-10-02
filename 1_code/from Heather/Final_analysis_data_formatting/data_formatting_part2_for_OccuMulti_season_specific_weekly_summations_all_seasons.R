# function for the stuff below:
# -------------------------------------------------------------------

dealwithNAs_V2 <- function(x){
  
  #x <- temp2
  
  fin.out <- NULL
  
  for(i in 1:nrow(x)){
    
    #i <- 127
    
    ftemp <- x[i,]
    
    ftemp <- na.omit(ftemp)
    
    if(nrow(ftemp)==0){
      
      fin.out <- rbind.data.frame(fin.out, NA)
      
    }else if(nrow(ftemp)>0){
      
      fin.out <- rbind.data.frame(fin.out, sum(ftemp, na.rm=T))
      
    }
    
  }
  
  return(fin.out)
  
}

# -------------------------------------------------------------------

# weekly "visits" within biological seasons assign a biological season - 

# fawning = Jan - March 
# fawn rearing and antler growth = April - June 
# pre-rut and rut = July and Aug
# Post-rut = Sept and Oct 
# Gestation = Sept - Dec 
# full = all year

# deer
#deer <- readRDS("./RDS/OccuMulti_data/deer_female2.RDS")
deer <- readRDS("./RDS/OccuMulti_data/deer_male2.RDS")

require(stringr)

fawnin <- deer

# days in the month - Jan = 31, Feb = 29, 28, 29, Mar = 31

#yr.index <- seq(from=1, to=(ncol(fawnin)), by=62)# year subset 
yr.index <- c(1,366,732,1097)# year subset - had to do it manually bc of Feb

fawnin.weekly <- data.frame(deer[,1])

for(i in 1:(length(yr.index)-1)){
  
  #i <- 1
  
  temp <- fawnin[,c((yr.index[i]+1):(yr.index[i+1]))]
  
  wk.index <- seq(from=1, to=ncol(temp), by=7) 
  
  fawnin.temp <- data.frame(matrix(NA, nrow = 180, ncol = length(wk.index)))
  
  for(j in 1:length(wk.index)){
    
    #j <- 53
    
    if(j == max(length(wk.index))){
      
      temp2 <- temp[,c(wk.index[j]:ncol(temp))]
      
    }else if(j != max(length(wk.index))){
      
      temp2 <- temp[,c(wk.index[j]:(wk.index[j]+6))]
      
    }
    
    if(is.null(dim(temp2))){
      
      #fawnin.temp[,j] <- data.frame(sum(temp2, na.rm=T))
      
      #message("..stopp at ", i, " and ", j)
      #break
      
      next
      
    }else{
    
    fawnin.temp[,j] <- dealwithNAs_V2(temp2)
    
    }
    
    colnames(fawnin.temp)[j] <- paste0(colnames(temp2)[1], "_to_", colnames(temp2)[ncol(temp2)])
    
  }
  
  
  fawnin.weekly <- cbind.data.frame(fawnin.weekly, fawnin.temp)
  
}

# save the deer weekly 
colnames(fawnin.weekly)[1] <- "camID"

# the last day of the year 12/31, leaves only one day in a sampling occasion. It needs to be removed. I have updated the loop above to 
# reflect that and now I need to remove thme here 
#fawnin.weekly2 <- fawnin.weekly[-which(colnames(fawnin.weekly)=="X53")]

#saveRDS(fawnin.weekly, "D:/Dropbox/Graduate_School_PhD/Projects_Papers/Humans_Predator_Prey/RDS/OccuMulti_data/deer/fdeer_all_seasons.RDS")
saveRDS(fawnin.weekly2, "D:/Dropbox/Graduate_School_PhD/Projects_Papers/Humans_Predator_Prey/RDS/OccuMulti_data/deer/mdeer_all_seasons.RDS")


# panther ------------------------------------------------------------------------------------------------------------------

panther <- readRDS("./RDS/OccuMulti_data/panther.RDS")

fawnin <- panther

#yr.index <- seq(from=1, to=(ncol(fawnin)), by=62)# year subset 
yr.index <- c(1,366,732,1097)# year subset - had to do it manually bc of Feb

fawnin.weekly <- data.frame(panther[,1])

for(i in 1:(length(yr.index)-1)){
  
  #i <- 1
  
  temp <- fawnin[,c((yr.index[i]+1):(yr.index[i+1]))]
  
  wk.index <- seq(from=1, to=ncol(temp), by=7) 
  
  fawnin.temp <- data.frame(matrix(NA, nrow = 180, ncol = length(wk.index)))
  
  for(j in 1:length(wk.index)){
    
    #j <- 53
    
    if(j == max(length(wk.index))){
      
      temp2 <- temp[,c(wk.index[j]:ncol(temp))]
      
    }else if(j != max(length(wk.index))){
      
      temp2 <- temp[,c(wk.index[j]:(wk.index[j]+6))]
      
    }
    
    if(is.null(dim(temp2))){
      
      #fawnin.temp[,j] <- data.frame(sum(temp2, na.rm=T))
      
      #message("..stopp at ", i, " and ", j)
      #break
      
      next
      
    }else{
      
      fawnin.temp[,j] <- dealwithNAs_V2(temp2)
      
    }
    
    colnames(fawnin.temp)[j] <- paste0(colnames(temp2)[1], "_to_", colnames(temp2)[ncol(temp2)])
    
  }
  
  
  fawnin.weekly <- cbind.data.frame(fawnin.weekly, fawnin.temp)
  
}

# save the deer weekly 
colnames(fawnin.weekly)[1] <- "camID"

# the last day of the year 12/31, leaves only one day in a sampling occasion. It needs to be removed. I have updated the loop above to 
# reflect that and now I need to remove thme here 
#fawnin.weekly2 <- fawnin.weekly[-which(colnames(fawnin.weekly)=="X53")]

# save the deer weekly 
saveRDS(fawnin.weekly, "D:/Dropbox/Graduate_School_PhD/Projects_Papers/Humans_Predator_Prey/RDS/OccuMulti_data/panther/panther_all_seasons.RDS")

# human --------------------------------------------------------------------------------------------------------------------

human <- readRDS("./RDS/OccuMulti_data/hum_veh.RDS")

fawnin <- human

#yr.index <- seq(from=1, to=(ncol(fawnin)), by=62)# year subset 
yr.index <- c(1,366,732,1097)# year subset - had to do it manually bc of Feb

fawnin.weekly <- data.frame(human[,1])

for(i in 1:(length(yr.index)-1)){
  
  #i <- 1
  
  temp <- fawnin[,c((yr.index[i]+1):(yr.index[i+1]))]
  
  wk.index <- seq(from=1, to=ncol(temp), by=7) 
  
  fawnin.temp <- data.frame(matrix(NA, nrow = 180, ncol = length(wk.index)))
  
  for(j in 1:length(wk.index)){
    
    #j <- 53
    
    if(j == max(length(wk.index))){
      
      temp2 <- temp[,c(wk.index[j]:ncol(temp))]
      
    }else if(j != max(length(wk.index))){
      
      temp2 <- temp[,c(wk.index[j]:(wk.index[j]+6))]
      
    }
    
    if(is.null(dim(temp2))){
      
      #fawnin.temp[,j] <- data.frame(sum(temp2, na.rm=T))
      
      #message("..stopp at ", i, " and ", j)
      #break
      
      next
      
    }else{
      
      fawnin.temp[,j] <- dealwithNAs_V2(temp2)
      
    }
    
    colnames(fawnin.temp)[j] <- paste0(colnames(temp2)[1], "_to_", colnames(temp2)[ncol(temp2)])
    
  }
  
  
  fawnin.weekly <- cbind.data.frame(fawnin.weekly, fawnin.temp)
  
}

# save the deer weekly 
colnames(fawnin.weekly)[1] <- "camID"

# the last day of the year 12/31, leaves only one day in a sampling occasion. It needs to be removed. I have updated the loop above to 
# reflect that and now I need to remove thme here 
#fawnin.weekly2 <- fawnin.weekly[-which(colnames(fawnin.weekly)=="X53")]

# save the deer weekly 
saveRDS(fawnin.weekly, "D:/Dropbox/Graduate_School_PhD/Projects_Papers/Humans_Predator_Prey/RDS/OccuMulti_data/human/hum_veh_not_filtered_all_seasons.RDS") 
