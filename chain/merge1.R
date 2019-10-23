#####  merge
mer_mode <- function(a, b){
  if(is.na(a) & is.na(b)){
    return(0)
  }
  else if(is.na(a)){
    return(b)
  }
  else if(is.na(b)){
    return(a)
  }
  
  else if((a>=b) & (a!=98) & (a!=99)){
    return(a)
  }
  else if((b!=98) & (b!=99)){
    return(b)
  }
  else{
    return(a)
  }
}

merge1 <- function(target){
  for (i in 1:nrow(target)){
    if(target$purpose[i] == 21 & !is.na(target$mode[i+1])){
      #merge mode
      target$mode[i+1] = mer_mode(target$mode[i], target$mode[i+1])
      #merge trip distance
      target$trip_distance_miles[i+1] = target$trip_distance_miles[i] + target$trip_distance_miles[i+1]
      #merge previous duration
      target$prev_trip_duration_min[i+1] = target$prev_trip_duration_min[i] + target$prev_trip_duration_min[i+1]
      #merge act_dur
      target$act_dur[i+1] = target$act_dur[i] + target$act_dur[i+1]
      #merge arr_time
      target$arr_time[i+1] = target$arr_time[i]
      #merge air distance 
      target$air_dist[i+1] = target$air_dist[i] + target$air_dist[i+1]
     }
    else if(target$purpose[i] == 21 & is.na(target$mode[i+1])){
      target$purpose[i] = 8
    }
    # merge 23
    else if(target$purpose[i] == 23 & !is.na(target$mode[i+1])){
      #merge mode
      target$mode[i+1] = mer_mode(target$mode[i], target$mode[i+1])
      #merge trip distance
      target$trip_distance_miles[i+1] = target$trip_distance_miles[i] + target$trip_distance_miles[i+1]
      #merge previous duration
      target$prev_trip_duration_min[i+1] = target$prev_trip_duration_min[i] + target$prev_trip_duration_min[i+1]
      #merge act_dur
      target$act_dur[i+1] = target$act_dur[i] + target$act_dur[i+1]
      #merge arr_time
      target$arr_time[i+1] = target$arr_time[i]
      #merge air distance 
      target$air_dist[i+1] = target$air_dist[i] + target$air_dist[i+1]
    }
    else if(target$purpose[i] == 23 & is.na(target$mode[i+1])){
      target$purpose[i] = 8
    }
    else if(target$purpose[i] == 24 & !is.na(target$mode[i+1])){
      #merge mode
      target$mode[i+1] = mer_mode(target$mode[i], target$mode[i+1])
      #merge trip distance
      target$trip_distance_miles[i+1] = target$trip_distance_miles[i] + target$trip_distance_miles[i+1]
      #merge previous duration
      target$prev_trip_duration_min[i+1] = target$prev_trip_duration_min[i] + target$prev_trip_duration_min[i+1]
      #merge act_dur
      target$act_dur[i+1] = target$act_dur[i] + target$act_dur[i+1]
      #merge arr_time
      target$arr_time[i+1] = target$arr_time[i]
      #merge air distance 
      target$air_dist[i+1] = target$air_dist[i] + target$air_dist[i+1]
      
    }
    else if(target$purpose[i] == 24 & is.na(target$mode[i+1])){
      target$purpose[i] = 8
    }
 }

  index_7_39 = which(target$purpose %in% c(39,7))
  index_index1 = which(target$purpose[index_7_39+1] %in% home)
  index_index2 = which(target$purpose[index_7_39-1] %in% home)
  index = intersect(index_index1, index_index2)
  index_home_39_7_home = index_7_39[index]

  target$act_dur[index_home_39_7_home+1] = target$act_dur[index_home_39_7_home] + target$act_dur[index_home_39_7_home+1]
  target$arr_time[index_home_39_7_home+1] = target$arr_time[index_home_39_7_home] 
  target$prev_trip_duration_min[index_home_39_7_home+1] = target$prev_trip_duration_min[index_home_39_7_home] + target$prev_trip_duration_min[index_home_39_7_home+1]

  # delete the redundant rows 
  index_21_23_24 = which(target$purpose %in% c(21, 23, 24))
  index_delete = union(index_21_23_24, index_home_39_7_home)
  target = target[-index_delete, ]
  return(target)
}
