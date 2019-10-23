### convert to detail time using the time in survey_activity 
# time converting 
library(chron)
# time to minutes 
conv2mins <- function(t){
  mins = 60 * 24 * as.numeric(times(t))
  return(mins)
}
# mins to time 
conv2time<-function(mins){
  mins[is.na(mins)] = 0
  hours<-round(mins%/%60)
  mins.left<-trunc(mins%%60)
  hours[which(hours>=24)] = hours[which(hours>=24)] - 24
  hours[which(hours<10)] = paste(0, hours[which(hours<10)], sep="")
  mins.left[which(mins.left<10)] = paste(0, mins.left[which(mins.left<10)], sep = "")
  end.time<-paste(hours,mins.left,"00", sep=":")
  end.time <- chron(times=end.time)
  return(end.time)
}

target = read(...)

chain_t_conv = subset(target, select = c("sampno", "perno", "tripno", "prev_trip_duration_min","air_dist","act_dur","arr_time", "dep_time"))
arr_time_mins = integer(nrow(chain_t_conv))
dep_time_mins = integer(nrow(chain_t_conv))
chain_t_conv = cbind(chain_t_conv, arr_time_mins, dep_time_mins)
# process the duration that's more than 1 day
chain_t_conv$act_dur[which(chain_t_conv$act_dur>1440)] = chain_t_conv$act_dur[which(chain_t_conv$act_dur>1440)]%%1440


# index to put the time extract from the activity file 
index_t = which(is.na(chain_t_conv$tripno) & is.na(chain_t_conv$prev_trip_duration_min))
survey_activity = read.csv("survey_activity.csv")
arr_t_act = integer(nrow(survey_activity))
survey_activity = cbind(survey_activity,arr_t_act )
survey_activity$arr_t_act = conv2mins(survey_activity$arr_time)
# fill in the extract time 
for(i in index_t){
  index_activity = which((survey_activity$sampno == chain_t_conv$sampno[i])
                         & (survey_activity$perno == chain_t_conv$perno[i])
                         & (survey_activity$tripno == 1) & (survey_activity$actno == 1)& (survey_activity$plano == 2))
  if(length(index_activity) == 1){
    chain_t_conv$dep_time_mins[i] = survey_activity$arr_t_act[index_activity]-chain_t_conv$prev_trip_duration_min[i+1]
  }
}

# fill in other time with the previous duration and activity duration
for(i in 1:nrow(chain_t_conv)){
  if(!is.na(chain_t_conv$tripno[i])){
    chain_t_conv$arr_time_mins[i] = chain_t_conv$dep_time_mins[i-1] + chain_t_conv$prev_trip_duration_min[i]
    chain_t_conv$dep_time_mins[i] = chain_t_conv$arr_time_mins[i] + chain_t_conv$act_dur[i]
  }
}


#
arr_t_final = integer(nrow(chain_t_conv))
dep_t_final = integer(nrow(chain_t_conv))
chain_t_final = chain_t_conv
chain_t_final = cbind(chain_t_final, arr_t_final, dep_t_final)


#change back to normal time 
chain_t_final$arr_t_final = conv2time(chain_t_final$arr_time_mins)
index_dep_na = which(is.na(chain_t_final$dep_t_final))
chain_t_final$dep_time_mins[index_dep_na] = conv2mins(chain_t_final$dep_time[index_dep_na])+30
chain_t_final$dep_t_final = conv2time(chain_t_final$dep_time_mins)


