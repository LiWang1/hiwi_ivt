# This script allows querying the Google Maps Directions API 
# and returns a table with the parameters given in namesModetable

# Libraries
library(gmapsdistance)
library(RCurl)
library(jsonlite)
library(plyr)
library(lubridate)
library(xtable)
library(texreg)
library(devtools)
library(remotes)
library(plyr)
library(dplyr)
library(data.table)
library(gtools)
library(ggmap)
library(maptools)
library(ggplot2)
library(sp)
library(foreign)
library(raster)
library(rgeos)
library(rgdal)
library(shapefiles)
library(MASS)
library(automap)
library(lmtest)
library(spdep)
library(reshape)
library(scales)
library(tmap)
library(naniar)
library(tidyr)


# Personal Google Maps API key
key <- 
register_google(key = key, account_type = "premium", day_limit = 100000)

namesModetable <- list(
  "Total_Time",                # Total time of the journey
  "Total_Time_WT_Traffic",     # Total time considering current traffic (only different for driving (and av))
  "Total_Distance",            # Total distance of the journey
  "Travel_Time_Mode",          # Travel time on transit vehicle (= Total_Time - (T_Walking + T_Waiting))
  "Distance_Mode",             # Distance on transit (Total_Distance - Distance_Walking)
  "Travel_Time_Feeder",        # Travel Time on AV
  "Distance_Feeder",           # Distance on AV
  "Travel_Time_Add_Walking",   # Time for walking (transit only)
  "Distance_Add_Walking",      # Distance walking (transit only)
  "Travel_Time_Waiting",       # Transit and AV: Waiting at train station or destination in case of early arrival
  "n_Vehicles",                # n vehicles used (transit and av) = n changes + 1 
  "hastrain"                   # binary, 1 if connection has train, 0 else
)

# Converts adress to URL in GMaps
makeurl <- function(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}

# Convert Adress to lat, lng, location ype, formatted address
geoCode <- function(address,verbose=FALSE) {
  print(address)
  #Sys.sleep(5) # Delay this function
  
  if(verbose) cat(address,"\n")
  u <- makeurl(address)
  doc <- getURL(u)
  x <- jsonlite::fromJSON(doc,simplifyVector = FALSE)
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    location_type <- x$results[[1]]$geometry$location_type
    formatted_address <- x$results[[1]]$formatted_address
    return(c(lat, lng, location_type, formatted_address))
  } else {
    return(c(NA,NA,NA, NA))
  }
}

#Use plyr to getgeocoding for a vector
addGeoCodeAddresses <- function(trips){
  locations <- ldply(trips$Startaddress, function(x) geoCode(x))
  names(locations) <- c("lat","lon","location_type", "forAddress")
  starttable<-locations
  
  locations <- ldply(trips$Endaddress, function(x) geoCode(x))
  names(locations) <- c("lat","lon","location_type", "forAddress")
  arrivaltable<-locations
  
  trips[,c("start_lat","start_lon")] <- starttable[,c(1,2)]
  trips[,c("end_lat","end_lon")] <- arrivaltable[,c(1,2)]
  return(trips)
}

# Query route
# Arrival-time queries are only possible for transit (and av).
# Bicycling, Walking and driving query departure times.
# If train == TRUE, Gmaps prefers train connections over other transit vehicles.
query <- function(mode, origin, destination, departuretime, train = F) {
  # Base URL
  url <- paste0("https://maps.googleapis.com/maps/api/directions/json?origin=", origin,
      "&destination=", destination,
      "&mode=", mode,
      "&units=metric",
      "&traffic_model=best_guess",
      "&key=", key
    )
  # Mode-specific URL
  #if (mode == "transit" || mode == "av")
   # url <- paste0(url, "&arrival_time=", arrivaltime)
    #else
    #url <- paste0(url, "&arrival_time=", arrivaltime)
  
  url <- paste0(url, "&departure_time=", departuretime)
  
  if (train){
    url <- paste0(url, "&transit_mode=train")
  }
  
  # Extract necessary data from response
  x <- getURL(url)
  x <- gsub(pattern = "\\\"polyline\\\".*?,", "", x)
  x <- gsub(pattern = "\\\"overview_polyline\\\".*?,", "", x)
  directionJSON <- fromJSON(x, simplifyVector = FALSE)
  
  if (directionJSON$status == "OK"){
    return(directionJSON$routes[[1]]$legs[[1]])
  } else {
    warning("No connection possible. NA produced.")
    return(NA)
  }
}

#GMaps only calculates travel times for dates in the future
#Based on the weekday provided by the respondents, we therefore need the next day that is ie a Monday
addDates<-function(trips){
  
  #workdays, weekend, specific day of the week, in english
  #weekdays --> takes monday
  
  actualweekday<-trips[,"DayOfWeek"]
  #if workday provided change to monday
  actualweekday[actualweekday=="Workday"]<-"Monday"
  weekdays<-matrix(c(1,"Sunday",2,"Monday",3,"Tuesday",4,"Wednesday",5,"Thursday",6,"Friday",7,"Saturday"),ncol=2,byrow=T)
  
  actualweekdaynr<-sapply(actualweekday,FUN=function(x)which(weekdays[,2]==x))
  
  neededdays<-(now(tzone="CET")+days(actualweekdaynr+7-wday(now(tzone="CET"))))
  
  hour(neededdays)= trips$Hour
  minute(neededdays)= trips$Minute
  
  trips[,"DateTime"]<-neededdays
  return(trips)
}

# Return data as specified in namesModetable for walking / cycling / driving
data.walking <- function(origin, destination, departuretime) {
  leg <- query(mode = "walking", origin = origin, destination = destination, departuretime = floor(as.numeric(departuretime)))
  
  if(!is.list(leg)){
    
    df <- data.frame(rep(NA,12))
    
    return(as.vector(unlist(df)))
    
  }else{
  total_time <- leg$duration$value
  total_time_WT_traffic <- total_time
  total_distance <- leg$distance$value
  #c(total_time, total_time_WT_traffic, total_distance, total_time, total_distance, 0,0,0,0,0,0,0)
  df <- data.frame(total_time, total_time_WT_traffic, total_distance, total_time, total_distance, 0,0,0,0,0,0,0)
  names(df) <- namesModetable
  df
  }
}
data.bicycling <- function(origin, destination, departuretime) {
  leg <- query(mode = "bicycling", origin, destination, departuretime = floor(as.numeric(departuretime)))
  
  if(!is.list(leg)){
    
    df <- data.frame(rep(NA,12))
    
    return(as.vector(unlist(df)))
    
  }else{
  
  total_time <- leg$duration$value
  total_distance <- leg$distance$value
  #c(total_time, total_time, total_distance, total_time, total_distance, 0,0,0,0,0,0,0)
  df <- data.frame(total_time, total_time, total_distance, total_time, total_distance, 0,0,0,0,0,0,0)
  names(df) <- namesModetable
  df
  }
}
data.driving <- function(origin, destination, departuretime) {
  leg <- query(mode = "driving",origin =  origin,destination =  destination, departuretime = floor(as.numeric(departuretime)))

    if(!is.list(leg)){
    
    names(df) <- namesModetable
    
    df<-as.vector(unlist(df))
    names(df) <- namesModetable
    return(df)
  }else{
  
  total_time <- leg$duration$value
  total_time_WT_traffic <- leg$duration_in_traffic$value
  total_distance <- leg$distance$value
  df <- data.frame(total_time, total_time_WT_traffic, total_distance, total_time, total_distance, 0,0,0,0,0,0,0)
  names(df) <- namesModetable
  df
  }
}

# Returns data for transit
# If train is set to TRUE, Google maps returns preferably train-connections
# Waiting_Time includes spare time in case of early arrival at the destination
data.transit <- function(origin, destination, departuretime, train = F) {
  
  leg <- query(mode = "transit", origin = origin, destination = destination, departuretime = departuretime,train =  train)
  
  if(!is.list(leg)){
    
    df <- data.frame(rep(NA,12))
    
    return(as.vector(unlist(df)))
    
  }else{
    df <- data.frame(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0)
    names(df) <- namesModetable
    
    df$Total_Distance <- leg$distance$value
    
    steps <- 1:length(leg$steps)
    legsum <- matrix(NA, nrow = length(steps), ncol = 2)
    rownames(legsum) <- rep("Error", length(steps))
    
    df$n_Vehicles <- 0
    
    
    for(a in steps){
      currentstep <- (leg$steps[[a]])
      rownames(legsum)[a] <- (currentstep$travel_mode)
      if(currentstep$travel_mode=="TRANSIT"){
        df$n_Vehicles <- df$n_Vehicles + 1
      }
      legsum[a,1] <- as.numeric(currentstep$duration$value)
      legsum[a,2] <- as.numeric(currentstep$distance$value)
      if (rownames(legsum)[a] == "TRANSIT") { # If a vehicle, save vehicle name
        #!is.null(currentstep$transit_details$line$vehicle$type) || 
        #if(currentstep$transit_details$line$vehicle$name == "Long distance train"){
          #Tram explicitly not mentioned!!!!!!!!
          if(currentstep$transit_details$line$vehicle$name == "Long distance train" ||  currentstep$transit_details$line$vehicle$type %in% c("COMMUTER_TRAIN","HIGH_SPEED_TRAIN","HEAVY_RAIL","RAIL")){
            rownames(legsum)[a] <- "train"
          }else{
            rownames(legsum)[a] <- "bus"
            
          }
        #}
        # if (!currentstep$transit_details$line$vehicle$name == "Long distance train" || 
        #     !is.null(currentstep$transit_details$line$vehicle$type)){
        #   
        #   if (!currentstep$transit_details$line$vehicle$type %in% c("COMMUTER_TRAIN","HIGH_SPEED_TRAIN","HEAVY_RAIL","RAIL")) {
        #     rownames(legsum)[a] <- currentstep$transit_details$line$vehicle$name
        # }
        

        }
      }
    
    
    
    resultleg <- aggregate(.~rownames(legsum),data = as.data.frame(legsum),sum)
    
    
    #if ("TRANSIT" %in% rownames(legsum)) # If transit is used
      #df$Total_Time <- leg$duration$value + (departuretime - leg$departure_time$value)
   # else { # If only walking involved (leg$arrival_time doesn't exist)
    df$Total_Time <- leg$duration$value
      #print("Transit only walking")
    #}
    
    df$Total_Time_WT_Traffic <- df$Total_Time
    df$Travel_Time_Waiting <- df$Total_Time - sum(resultleg[,2])
    
    
    
    if("train"%in%resultleg$`rownames(legsum)`){
      df$Travel_Time_Mode <- resultleg[resultleg$`rownames(legsum)`=="train",2]
      df$Distance_Mode <- resultleg[resultleg$`rownames(legsum)`=="train",3]
      if("bus"%in%resultleg$`rownames(legsum)`){
        df$Travel_Time_Feeder <- resultleg[resultleg$`rownames(legsum)`=="bus",2]
        df$Distance_Feeder <- resultleg[resultleg$`rownames(legsum)`=="bus",3]
      }
      df$hastrain <- 1
    }else if("bus"%in%resultleg$`rownames(legsum)`){
      df$Travel_Time_Mode <- resultleg[resultleg$`rownames(legsum)`=="bus",2]
      df$Distance_Mode <- resultleg[resultleg$`rownames(legsum)`=="bus",3]
      
    }
    

    if("WALKING"%in%resultleg$`rownames(legsum)`){
      df$Travel_Time_Add_Walking <- resultleg[resultleg$`rownames(legsum)`=="WALKING",2]
      df$Distance_Add_Walking <- resultleg[resultleg$`rownames(legsum)`=="WALKING",3]
    }
    as.vector(unlist(df))
  }
  
}


# Returns the data for av/train combination
# Walking time is neglected /assumed to be zero
data.av <- function(origin, destination, departuretime) {
  # Combined AV and transit (train)
  # 1. Query transit with preference train
  # 2. Extract first and last train station
  # 3. Query "driving" of start to start train station and from end train station to end
  # 4. Combine
  
  leg <- query(mode = "transit", origin, destination, departuretime, train = F)
  if (!is.list(leg)){
    return(rep(NA, length(namesModetable)))
  }
  
  steps <- 1:length(leg$steps)
  
  df <- data.frame(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0)
  names(df) <- namesModetable
  
  start_station <- NULL
  end_station <- NULL
  
  start_index <- NULL # Index of the start_station
  end_index <- NULL # Index of end_station

  for (i in steps) {
    if (leg$steps[[i]]$travel_mode == "TRANSIT") {
      # Check if transit first, otherwise "transit_details" doesn't exist
      if (leg$steps[[i]]$transit_details$line$vehicle$type %in% c("COMMUTER_TRAIN","HIGH_SPEED_TRAIN","HEAVY_RAIL","RAIL") ||
          leg$steps[[i]]$transit_details$line$vehicle$name == "Long distance train") {
        # Add data
        df$Total_Distance <- df$Total_Distance + leg$steps[[i]]$distance$value
        df$Travel_Time_Mode <- df$Travel_Time_Mode + leg$steps[[i]]$duration$value
        df$Distance_Mode <- df$Distance_Mode + leg$steps[[i]]$distance$value
        df$n_Vehicles <- df$n_Vehicles + 1
        # Set start and end railway station
        if (is.null(start_station)) {
          start_station <- paste0(leg$steps[[i]]$start_location$lat, ",", leg$steps[[i]]$start_location$lng)
          end_station <- paste0(leg$steps[[i]]$end_location$lat, ",", leg$steps[[i]]$end_location$lng)
          start_index <- i
          end_index <- i
        }
        else {
          end_station <- paste0(leg$steps[[i]]$end_location$lat, ",", leg$steps[[i]]$end_location$lng)
          end_index <- i
        }
      }
    }
  }

  if (!is.null(start_index)) {  # If there is a train
    # Query 'origin --> start_station' and 'end_station --> destination'
    df$hastrain <- 1
    #Train
    train_time_start <- leg$steps[[start_index]]$transit_details$departure_time$value
    train_time_end <- leg$steps[[end_index]]$transit_details$arrival_time$value
    
    df$Travel_Time_Waiting <- train_time_end - train_time_start - df$Travel_Time_Mode + 300 #5 minutes from first AV to train
    
    
    
    origin_start <- data.driving(origin, start_station, departuretime)
    end_destination <- data.driving(origin = end_station,destination =  destination, departuretime = departuretime)
    
    #ab 3 Minuten AV, sonst laufen
    
    
    #Im Kanton Z?rich Haltestelle durchschnittlich 195 Meter entfernt --> 300m zumutbar
    if(origin_start$Total_Distance<=300){
      origin_walk=T
      origin_av = F
      origin_start <- data.walking(origin, start_station, departuretime)
      
      df$Travel_Time_Add_Walking <- df$Travel_Time_Add_Walking + origin_start[,which(namesModetable=="Total_Time_WT_Traffic")]
      df$Distance_Add_Walking <- df$Distance_Add_Walking + origin_start[,which(namesModetable=="Distance_Add_Walking")]
      
    }else{
      origin_walk=F
      origin_av = T
      df$Travel_Time_Feeder <- df$Travel_Time_Feeder + origin_start$Total_Time_WT_Traffic
      df$Distance_Feeder <- df$Distance_Feeder + origin_start$Total_Distance
    }
    
    if(end_destination$Total_Distance<=300){
      dest_walk=T
      dest_av = F
      end_destination <- data.walking(origin, start_station, departuretime)
      df$Travel_Time_Add_Walking <- df$Travel_Time_Add_Walking + end_destination[,which(namesModetable=="Total_Time_WT_Traffic")]
      df$Distance_Add_Walking <- df$Distance_Add_Walking + end_destination[,which(namesModetable=="Distance_Add_Walking")]
      
    }else{
      dest_walk=F
      dest_av = T
      df$Travel_Time_Feeder <- df$Travel_Time_Feeder + end_destination$Total_Time_WT_Traffic
      df$Distance_Feeder <- df$Distance_Feeder + end_destination$Total_Distance
    }
    
    
    
    # Calculate total time / waiting time
    # Total_Time = 1. Time on train + 2. time in AV + 3. gap between actual and desired arrival time

    
    #waiting_at_arrival <- (departuretime - time_end) - end_destination$Total_Time
    
    df$Total_Time <- (train_time_end - train_time_start)  
    df$Total_Time <- df$Travel_Time_Mode + df$Travel_Time_Feeder + df$Travel_Time_Add_Walking + df$Travel_Time_Waiting
    #+ waiting_at_arrival
    df$Total_Time_WT_Traffic <- df$Total_Time 

    # n_Vehicles + 1 for each AV
    if (origin_av) # Check if origin != start_station
      df$n_Vehicles <- df$n_Vehicles + 1
    if (dest_av)
      df$n_Vehicles <- df$n_Vehicles + 1
    
    # Sum up
    df <- as.vector(unlist(df))
  } else {
    df$hastrain<-0
    tempdf <- data.driving(origin, destination, departuretime) # AV the whole way
    df$Total_Time <- tempdf$Total_Time
    df$Total_Time_WT_Traffic <- tempdf$Total_Time_WT_Traffic
    df$Travel_Time_Feeder <- tempdf$Total_Time_WT_Traffic
    df$Distance_Feeder <- tempdf$Total_Distance
    df$n_Vehicles <- 1 # n_Vehicles = 1
    df <- as.vector(unlist(df))
    }
    df
}

# Main method
createModeTable <- function(trips, key){
  
  modes <- c("driving","walking","bicycling","transit")
  
  modetable <- as.data.frame(matrix(nrow = 1, ncol = length(namesModetable) + 3))
  colnames(modetable) <- c("ID","Tripnr","Mode", namesModetable)
  
  # iterate through all trips
  for(i in 1:nrow(trips)){
    departuretime <- floor(as.numeric(as.POSIXct(trips$DateTime[i])))
    print(paste0("ID ", trips$ID[i]))
    print(paste0("Tripnr ", trips$Tripnr[i]))
    
    # Set trip parameters
    if(is.na(trips$start_lat[i])|is.na(trips$end_lat[i]) ){
      for (mode in modes){
        
        print(paste0("Mode ", mode))
        
        
        
        switch (mode,
                "walking" = modetable <- rbind(modetable, c(trips$ID[i],trips$Tripnr[i], mode, rep(NA,length(namesModetable)))),
                "bicycling" = modetable <- rbind(modetable, c(trips$ID[i],trips$Tripnr[i], mode, rep(NA,length(namesModetable)))),
                "driving" = modetable <- rbind(modetable, c(trips$ID[i],trips$Tripnr[i], mode, rep(NA,length(namesModetable)))),
                "transit" = modetable <- rbind(modetable, c(trips$ID[i],trips$Tripnr[i], mode, rep(NA,length(namesModetable)))),
                "av" = modetable <- rbind(modetable, c(trips$ID[i],trips$Tripnr[i], mode, rep(NA,length(namesModetable))))
                
                
        ) 
      }
    }else{
      origin = paste0(trips$start_lat[i],",",trips$start_lon[i])
      destination = paste0(trips$end_lat[i],",",trips$end_lon[i])

      # iterate through all modes
      for (mode in modes){
        
        print(paste0("Mode ", mode))
        
        switch (mode,
                "walking" = modetable <- rbind(modetable, c(trips$ID[i],trips$Tripnr[i], mode, as.vector(unlist(data.walking(origin = origin,destination =  destination, departuretime = departuretime))))),
                #"bicycling" = modetable <- rbind(modetable, c(trips$ID[i],trips$Tripnr[i], mode, as.vector(unlist(data.bicycling(origin = origin, destination = destination,departuretime =  departuretime))))),
                "driving" = modetable <- rbind(modetable, c(trips$ID[i],trips$Tripnr[i], mode, as.vector(unlist(data.driving(origin = origin,destination =  destination,departuretime =  departuretime))))),
                "transit" = modetable <- rbind(modetable, c(trips$ID[i],trips$Tripnr[i], mode, data.transit(origin = origin,destination =  destination,departuretime =  departuretime)))
                #"av" = modetable <- rbind(modetable, c(trips$ID[i],trips$Tripnr[i], mode, data.av(origin = origin,destination =  destination, departuretime = departuretime)))                
                
        ) 
      }
    }
    

  }
  modetable <- modetable[-1,]
  return(modetable)
}




# ------------------------------------------------------------------------------------------------------
# ----- Testing ----------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------
# trips<-as.data.frame(matrix(NA,nrow=2,ncol=3))
# trips[,1]<-c(1,2)
# 
# addresses<-c("Winterthurerstrasse 398, 8051 Zurich, Switzerland","Gloriastrasse 55, 8044 Zurich, Switzerland","Nueschelerstrasse 35, 8001 Zurich, Switzerland","Baerengasse 10, 8001 Zurich, Switzerland","Badenerstrasse 294, 8004 Zurich, Switzerland","Gloriastrasse 88, 8044 Zurich, Switzerland","Attenhoferstrasse 9, 8032 Zurich, Switzerland","Butzenstrasse 4, 8038 Zurich, Switzerland","Uetlibergstrasse 260, 8045 Zurich, Switzerland","Hotzestrasse 65, 8006 Zurich, Switzerland")
# 
# locations <- ldply(addresses, function(x) geoCode(x))
# names(locations) <- c("lat","lon","location_type", "forAddress")
# 
# 
# combinations<-combn(x = addresses,m = 2)
# trips<-aperm(combinations,c(2,1))
# 
# trips<-as.data.frame(cbind(as.numeric(1:nrow(trips)),trips))
# 
# trips[,4]<- sample(c("Monday","Tuesday","Saturday","Workday"),nrow(trips),replace=T)
# trips[,5]<- sample(x=6:22,nrow(trips),replace = T) # Transit only works at operating hours -> limit to daytime
# trips[,6]<- sample(x=0:60,nrow(trips),replace = T)
# colnames(trips)<-c("Tripnr","Startaddress","Endaddress","DayOfWeek","Hour","Minute")
# 
# 
# trips<-addDates(trips)
# trips<-addGeoCodeAddresses(trips)
# 
# modetable <- createModeTable(trips = trips[1:7,], key = key)
# 
# modetable$Tripnr <- as.numeric(modetable$Tripnr)
# modetable <- modetable[order(modetable$Tripnr),]
# modetable <- rbind(modetable,modetable[1:4,])
# modetable$Tripnr[c(181:184)] <- 46
# modetable$Tripnr <- modetable$Tripnr%%2+1
# modetable$ID <- sort(rep(c(1:(nrow(modetable)/8)),8))
# 
# 
# 
# # ----- For single queries ---------------------------------------------------------------
 #origin <- geoCode("Himmeristrasse 10, Zürich")
# origin <- paste0(origin[1], ",", origin[2])
 #destination <- geoCode("Sulgenauweg 2, Bern")
 #destination <- paste0(destination[1], ",", destination[2])
# 
# mode = "transit"
# 
 #arrivaltime <- floor(as.numeric(as.POSIXct(Sys.time())))
 
# train <- F
