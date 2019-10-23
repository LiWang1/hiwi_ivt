### filter according to household address 
# filter for sf 
survey_households <- read.csv("/Users/wangli/Downloads/caltrans_full_survey/survey_households.csv")
bays = c(1, 13, 41, 55, 75, 81, 85, 95, 97)
houseid_bays = which(survey_households$home_county_id %in% bays)
houseid_bays_sampno = survey_households$sampno[houseid_bays]
index_bays = which(cali_final$sampno%in% houseid_bays_sampno)
sf_bays = cali_final[index_bays,]
write.csv(sf_bays, "sfbays0814_move_weekdays.csv")

#filter for la
la4county = c(37, 59, 65, 71, 111)
houseid_la = which(survey_households$home_county_id %in% la4county)
houseid_la_sampno = survey_households$sampno[houseid_la]
index_la = which(cal_people$sampno%in% houseid_la_sampno)
la4counties = cal_people[index_la,]
write.csv(la4counties, "la4counties_allpeople0816.csv")



# filter accoding to original and destination address 
# for trips 
remove_stay <- function(target){
  index_delete = which(is.na(target$tripno))
  target = target[-index_delete,]
  return(target)
}
la4county = c(37, 59, 65, 71, 111)
bays = c(1, 13, 41, 55, 75, 81, 85, 95, 97)
cal = read.csv('cal_allday_move_withoutflight_coordinates0819.csv')
filter <- function(target, area){
  sampno_id = target$sampno*10 + target$perno
  target_ori = floor((target$origin_zone %% 1000000000)/1000000)
  target_des = floor((target$destination_zone %% 1000000000)/1000000)
  ori_index = which(!(target_ori %in% area))
  des_index = which(!(target_des %in% area))
  target_index = union(ori_index, des_index)
  target_sampno_del = unique(sampno_id[target_index])
  index_stay = which(is.na(target$tripno))
  index_stay_id = unique(target[index_stay,]$sampno*10 +target[index_stay,]$perno)
  target_sampno_del = setdiff(target_sampno_del, index_stay_id)
  index_delete = which(sampno_id %in% target_sampno_del)
  target_remain = target[-index_delete,]
  return(target_remain)
}
la_trips = filter(cal, la4county)
la_trips = remove_stay(la_trips)
write.csv(la_trips, 'la5counties_weekdays_10009_withoutflight.csv', row.names = FALSE)


# for person 
la4county = c(37, 59, 65, 71, 111)
bays = c(1, 13, 41, 55, 75, 81, 85, 95, 97)
cal = read.csv('/Users/wangli/polybox2/processed\ data/chain/sorted\ data/trips/weekdays/cal_weekdays_0815_withoutflight.csv')
person = read.csv('cal_allpeople_withoutflight_0816.csv')
filter_person <- function(target, area, person){
  sampno_id = target$sampno*10 + target$perno
  target_ori = floor((target$origin_zone %% 1000000000)/1000000)
  target_des = floor((target$destination_zone %% 1000000000)/1000000)
  ori_index = which((target_ori %in% area))
  des_index = which((target_des %in% area))
  target_index = intersect(ori_index, des_index)
  target_sampno_del = unique(sampno_id[target_index])
  person_id = person$sampno*10 + person$perno
  index_delete = which(person_id %in% target_sampno_del)
  target_remain = person[index_delete,]
  return(target_remain)
}
la_person_week = filter_person(cal, la4county, person)
stay_people_index = which(is.na(cal$tripno))
stay_people = cal[stay_people_index,]
stay_people_id = stay_people$sampno*10 + stay_people$perno

person_la_index = which(person$home_county_id %in% la4county)
person_la = person[person_la_index,]
person_la_id = person_la$sampno*10 + person_la$perno
stay_la_index = which(person_la_id %in% stay_people_id)
stay_la_week = person_la[stay_la_index,]

la_combine = rbind(la_person_week, stay_la_week)
write.csv(la_combine, 'la5counties_allpeople_withoutflight_weekdays_0919.csv')






