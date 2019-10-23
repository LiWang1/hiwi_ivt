# To sort out the information to certain format 

# 1) person information processing
survery_person = read(...)  # read survey_person file 
home = read(...) # read survey_home file

person = subset(survey_person, select = c("sampno", "perno", "age", "gender","empl_status", "empl_status","transit_pass", "perwgt", "nrel_agebin","employment", "student","school_grade" ))
home = subset(survey_households, select = c("sampno", "home_county_id","home_tract_id", "vehicle_count", "bike_count", "income"))
home_tract_id = integer(nrow(person))
home_county_id = integer(nrow(person))
number_of_cars = integer(nrow(person))
number_of_bikes = integer(nrow(person))
household_income = integer(nrow(person))
person = cbind(person, home_tract_id,home_county_id, number_of_cars,number_of_bikes, household_income)

for(i in 1:nrow(person)){
  index = which(home$sampno == person$sampno[i])
  if(length(index)==1){
    person$home_tract_id[i] =home$home_tract_id[index]
    person$number_of_cars[i] = home$vehicle_count[index]
    person$number_of_bikes[i] = home$bike_count[index]
    person$household_income[i] = home$income[index]
    person$home_county_id[i] = home$home_county_id[index]
  }
}

write.csv(...)


# 2) for trip 
cal = read.csv("...", stringsAsFactors = FALSE)
cali = subset(cal, select =c("sampno","perno", "tripno", "travel_date","arr_time", "dep_time", 
                          "mode","prev_trip_duration_min","trip_distance_miles", "air_dist","tract_id", "county_id",
                             "perwgt", "tcfperwgt", "purpose", "purpose_cat",'travel_date'))
origin_zone = integer(nrow(cali))
origin_purpose = integer(nrow(cali))
destination_zone  = integer(nrow(cali))
destination_purpose = integer(nrow(cali))
departure_time  = integer(nrow(cali))
arrival_time = integer(nrow(cali))
cali = cbind(cali, origin_zone, origin_purpose, destination_zone, destination_purpose, departure_time, arrival_time)
cali$tract_id = 6*1000000000 + cali$county_id*1000000+cali$tract_id

# 
cali$origin_zone[2:length(cali$tract_id)] = cali$tract_id[1:length(cali$tract_id)-1]
cali$origin_purpose[2:length(cali$tract_id)] = cali$purpose_cat[1:length(cali$tract_id)-1]
cali$destination_zone = cali$tract_id
cali$destination_purpose = cali$purpose_cat
cali$departure_time[2:length(cali$tract_id)] = cali$dep_time[1:length(cali$tract_id)-1]
cali$arrival_time= cali$arr_time


# adjust the trip no.
for(i in 1:nrow(cali)){
  if(is.na(cali$mode[i])){
    cali$tripno[i] = 0
  }
  if(!is.na(cali$mode[i])){
    cali$tripno[i] = cali$tripno[i-1] + 1
  }
}

# delete 
index2delete= which(is.na(cali$mode))
cali2 = cali[-index2delete,]


# calculate the trip weight
trip_weight = cali2$tcfperwgt/cali2$perwgt
cali3 = cbind(cali2, trip_weight)
cali_final = subset(cali3, select = c("sampno", "perno", "tripno", "origin_zone", "origin_purpose", 
                                 "destination_zone","destination_purpose", "mode", "departure_time", 
                                 "arrival_time" , "prev_trip_duration_min", "perwgt","trip_weight", "trip_distance_miles","air_dist","county_id", 'weekday'))

# convert from km to miles 
cali_final$air_dist = cali_final$air_dist*1.60934
cali_final$trip_distance_miles = cali_final$trip_distance_miles*1.60934
names(cali_final)[names(cali_final)=="trip_distance_miles"] <- "trip_distance_km"
names(cali_final)[names(cali_final)=="air_dist"] <- "air_dist_km"


write.csv(cali_final, "cal_alldays_0815_withoutflight.csv")


