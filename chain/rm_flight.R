# remove the flight 
chain1 = read.csv("/Users/wangli/Downloads/caltrans_full_survey/chainwithpurpose0718")
personID =  chain1$sampno*10 + chain1$perno
person_flight = personID[which(chain1$mode==13)]

cal_move = read.csv('/Users/wangli/polybox2/processed\ data/chain/sorted\ data/persons/sf_allpeople0816.csv')
person_id_cal = cal_move$sampno*10 + cal_move$perno
index_flight = which(person_id_cal %in% person_flight)
# cal_move_flight = cal_move[index_flight,]
# write.csv(cal_move_flight, 'cal_weekdays_0815_withflight.csv')

cal_move_without_flight = cal_move[-index_flight, ]
write.csv(cal_move_without_flight, 'sf_allpeople_withoutflight0816.csv')

## weekdays
chain_week= read.csv("/Users/wangli/polybox2/processed\ data/chain/sorted\ data/trips/weekdays/cal_weekdays_0815_withoutflight.csv")
week_index = chain_week$sampno*10 + chain_week$perno
person = read.csv("/Users/wangli/polybox2/processed\ data/chain/sorted\ data/persons/la4counties_allpeople_withoutflight_0816.csv")
person_id = person$sampno*10 + person$perno
person_index = which(person_id %in% week_index)
person_week = person[person_index, ]
write.csv(person_week, "la4counties_allpeople_withoutflight_weekdays_0816.csv")
