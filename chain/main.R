# extract the purpose from activity file
chain1 = subset(survey_place, select = c('sampno', 'perno', 'plano', 'vehno', 'tripno', 'travel_date', 'arr_time', 'dep_time',
                                  'mode', 'trip_distance_miles', 'prev_trip_duration_min', 'act_dur', 'tract_id', 'county_id',
                                  'state_id', 'perwgt', 'tcfperwgt'))
purpose = integer(nrow(chain1))
chain1 = cbind(chain1, purpose)


# take the purpose of the activity to trip, I took the purpose of the first action in each trip.
index_na = which(is.na(survey_activity$tripno))
for (i in 1:nrow(chain1)){
  index1 = which((survey_activity$sampno == chain1$sampno[i]) & (survey_activity$perno == chain1$perno[i]) & (survey_activity$tripno == chain1$tripno[i]) & (survey_activity$actno == 1))
  index2_1 = which((survey_activity$sampno == chain1$sampno[i]) & (survey_activity$perno == chain1$perno[i]) & (survey_activity$actno == 1))
  index2 = intersect(index_na, index2_1)
  index = union(index1, index2)

  if(length(index)>0){
    chain1$purpose[i] = survey_activity$purpose[index[1]]
  }
}

# sort the chain 
sorted_chain = chain1[order(chain1$sampno, chain1$perno),]
sorted_chain = sorted_chain[,-1]

#poeple move and not move:
index=which(is.na(sorted_chain$tripno))
index_index = which(is.na(sorted_chain$tripno[index+1]))
index_stay = index[index_index]
chain_stay = sorted_chain[index_stay,]
chain_move = sorted_chain[-index_stay,]
write.csv(chain_stay, 'staytrip0814.csv')

### time correction:
write.csv(chain_move, 'chain_move_timecorrected.csv')

# merge the chain 
merge1_chain = merge1(chain_move)

# assign category
chain2 = categ(merge1_chain)


# 5: STUDY / SCHOOLWORK is not necesarily home 
survey_households <- read.csv("survey_households.csv")
index_5 = which(chain2$purpose == 5)
for(i in index_5){
  sample_no = chain2$sampno[i]
  if(chain2$tract_id[i] != (survey_households$home_tract_id[which(survey_households$sampno == sample_no)] %%1000000)){
    chain2$purpose_cat[i] = "Education"
  }
}
write.csv(chain2, 'after21_23_24merge0814.csv')


#LOOP TRIP: home-leisure...leisure-home
chain3 = read.csv("after21_23_24merge0814.csv")
chain3 = chain3[,-1]

index_rem_39 = which(chain3$purpose == 39)
index_39_home_1 = which(is.na(chain3$tcfperwgt[index_rem_39]))
index_39_home_2 = which(is.na(chain3$tcfperwgt[index_rem_39+1]))
index_39_home = index_rem_39[union(index_39_home_1, index_39_home_2)]
chain3$purpose_cat[index_39_home] = "Home"

#merge loop trip 
index_merloop = which(chain3$purpose == 39 & chain3$purpose_cat == "Leisure")
# find consecutive values to merge 
set = split(index_merloop, cumsum(c(1, diff(index_merloop) != 1)))

remained_index = c()
chain3$trip_distance_miles[is.na(chain3$trip_distance_miles)] = 0
chain3$air_dist[is.na(chain3$air_dist)] = 0

# deal with NAs
for(i in 1:length(set)){
  vector_tem = as.numeric(unlist(set[i]))

  #merge mode
  chain3$mode[vector_tem[1]] = chain3$mode[vector_tem[which.max(chain3$trip_distance_miles[vector_tem])]]
  #merge distance miles 
  chain3$trip_distance_miles[vector_tem[1]] = max(chain3$trip_distance_miles[vector_tem])
  #merge air distance miles 
  chain3$air_dist[vector_tem[1]] = max(chain3$air_dist[vector_tem])
  #merge tract id 
  chain3$tract_id[vector_tem[1]] = chain3$tract_id[vector_tem[which.max(chain3$trip_distance_miles[vector_tem])]]
  #merge dep time 
  chain3$dep_time[vector_tem[1]] = chain3$dep_time[vector_tem[length(vector_tem)]+1]
  #merge tripweight
  chain3$tcfperwgt[vector_tem[1]] = sum(chain3$tcfperwgt[vector_tem])/length(vector_tem)
  remained_index = c(remained_index, vector_tem[1])
  
}

# delete the redundant rows 
index_deleted = setdiff(index_merloop, remained_index)
chain4 = chain3[-index_deleted,]
write.csv(chain4, 'afterloopmerge0814.csv')
##### further refinement 

####home...home 
chain5 = read.csv('afterloopmerge0814.csv')
chain5 = chain5[, -1]
index_hh1 = which(chain5$purpose_cat =="Home")
index_hh2 = which((chain5$purpose_cat[index_hh1+1] == "Home")&(!is.na(chain5$tcfperwgt[index_hh1+1])))
index_hh = index_hh1[index_hh2]
set2 = split(index_hh, cumsum(c(1, diff(index_hh) != 1)))

index_deleted_h = c()
for(i in 1:length(set2)){
  vector_tem = as.numeric(unlist(set2[i]))
  #merge dep time 
  chain5$dep_time[vector_tem[1]] = chain5$dep_time[vector_tem[length(vector_tem)]+1]
  index_deleted_h = c(index_deleted_h, tail(vector_tem, n=length(vector_tem)-1), vector_tem[length(vector_tem)]+1)
}
# index to be deleted 
chain5 = chain5[-index_deleted_h,]

#weekdays(as.Date('16-08-2012','%d-%m-%Y'))
#chain5 = time_correct(chain5)
write.csv(chain5, "afterhhmerge0814.csv")


chain5$weekday = weekdays(as.Date(as.character(chain5$travel_date),'%Y-%m-%d'))
week = c("Tuesday", "Wednesday", "Thursday")
week_index = which(chain5$weekday %in% week)
chain_weekdays = chain5[week_index,]
write.csv(chain_weekdays,'cal_move_weekdays0814.csv')






