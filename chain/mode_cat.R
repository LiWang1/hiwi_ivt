trips = read.csv('cal_0814updated_weekday.csv')
car_alone = c(5, 10) 
carpooled = c(6,7)
public_transportation = c(12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29)
bicycle_walk = c(1, 2)
taxi_motorcycle = c(8, 9, 11)
other_mode = c(3, 4, 98, 99, 14)

mode_cat = nrow(trips)
mode_cat[which(trips$mode%in%car_alone)]='car_alone'
mode_cat[which(trips$mode%in%carpooled)]='carpooled'
mode_cat[which(trips$mode%in%public_transportation)]='pt'
mode_cat[which(trips$mode%in%bicycle_walk)]='bicycle_walk'
mode_cat[which(trips$mode%in%other_mode)]='others'
mode_cat[which(trips$mode%in%taxi_motorcycle)]='taxi_motor'

trips = cbind(trips, mode_cat)
#trips = trips[, -1]
#write.csv(trips, 'cal_move0814updated.csv')



# time correction 
trips$arrival_time = conv2time(conv2mins(trips$departure_time)+trips$prev_trip_duration_min)
#trips = trips[, -1]
write.csv(trips, 'cal_0814updated_weekday.csv')
