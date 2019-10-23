
#wandelt DayOfWeek, Hour und Minute in DateTime mit nächstem Datum entsprechend Wochentag um
triptable <- addDates(triptable)

for(i in 1:nrow(triptable)){
  if(i==1){
    modetable <- createModeTable(trips = triptable[i,], key = key)
    
  }else{
    
    
    tryCatch(
      {modetable <-rbind(modetable,createModeTable(trips = triptable[i,], key = key))},
      error=function(e){
        a=1
      }
    )
    if(a==1){
      tryCatch(
        {modetable <-rbind(modetable,createModeTable(trips = triptable[i,], key = key))},
        error=function(e){
          a=1
        }
      )
    }
    a=0
  }
  
}


#Checks for modetable
for(i in 1:nrow(modetable)){
  if(sum(is.na(modetable[i,]))>2){
    print(i)
    id = modetable$ID[i]
    tripnr = modetable$Tripnr[i]
    
    modetable[modetable$ID==id & modetable$Tripnr==tripnr,] <- createModeTable(trips = trips_full[trips_full$ID==id & trips_full$Tripnr==tripnr,], key = key)
    
  }
}