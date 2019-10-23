library(osmplotr)
library(osmdata)
library(sf)
process <- function(target){
  target$geometry = st_centroid(st_geometry(target))
  newTarget = subset(target, select = c("osm_id", "name", "geometry"))
  return(newTarget)
}

# bbbox 
bb <- get_bbox(getbb("sao paulo"))
# ## riverside county
# bb[1, 1] = -117.672060
# bb[1,2] = -116.024
# bb[2, 1] = 33.441462
# bb[2,2] = 34.03

# # san bernardino 
# bb[1, 1] = -117.729040
# bb[1,2] = -117.038386
# bb[2, 1] = 34.040388
# bb[2,2] = 34.898616
# 
# # part 1 of la county left bottom
# bb[1, 1] = -118.415108
# bb[1, 2] = -118.232254
# #bb[1,2] = -117.977921
# bb[2,1] = 33.742519
# bb[2,2] = 33.849466
# #bb[2,2] = 33.949466


# # part 1 of la county right bottom
# #bb[1, 1] = -118.415108
# bb[1,1] = -118.232254
# bb[1,2] = -117.977921
# bb[2,1] = 33.742519
# bb[2,2] = 33.849466
# #bb[2,2] = 33.949466

# # part 1 of la county right up
# #bb[1, 1] = -118.415108
# bb[1,1] = -118.232254
# bb[1,2] = -117.977921
# #bb[2,1] = 33.742519
# bb[2,1] = 33.849466
# bb[2,2] = 33.949466


# # part 1 of la county left up
# bb[1, 1] = -118.415108
# bb[1,2] = -118.232254
# #bb[1,2] = -117.977921
# #bb[2,1] = 33.742519
# bb[2,1] = 33.849466
# bb[2,2] = 33.949466

# # part 2 of la county 
# bb[1, 1] = -117.97721
# bb[1,2] = -117.681510
# bb[2,1] = 33.949466
# bb[2,2] = 34.163056


# # part 3 
# bb[1, 1] = -118.181515
# bb[1,2] = -118.002839
# bb[2,1] = 33.963748
# bb[2,2] = 34.201788

# 
# # part 4
# bb[1, 1] = -118.473287
# bb[1,2] = -118.183841
# bb[2,1] = 33.985507
# bb[2,2] = 34.153117

# # part 5
# bb[1, 1] = -118.662259
# bb[1,2] = -118.309478
# bb[2,1] = 34.162483
# bb[2,2] = 34.277880

# part 6
bb[1, 1] = -118.328313
bb[1,2] = -117.670997
bb[2,1] = 34.549967
bb[2,2] = 34.822534
##extract 
# residential office
data_office <- extract_osm_objects (key = 'office', bbox = bb, return_type = 'point')
data_appartment<-extract_osm_objects(key = "building", value='apartments', bbox = bb)
data_dormitory<-extract_osm_objects(key = "building", value='dormitory', bbox = bb)
data_house<-extract_osm_objects(key = "building", value='house', bbox = bb)
data_hotel<-extract_osm_objects(key = "building", value='hotel', bbox = bb)
data_residential<-extract_osm_objects(key = "building", value='residential', bbox = bb)

#education 
data_un1 <-extract_osm_objects(key = "building", value = "university", bbox = bb)
data_school <-extract_osm_objects(key = "building", value = "school", bbox = bb)
data_college <- extract_osm_objects(key = "building", value = "college", bbox = bb)
data_kid <- extract_osm_objects(key = "building", value = "kindergarten", bbox = bb)

#bigger shops
data_commerical <-extract_osm_objects(key = "building", value = "commercial", bbox = bb)
data_supermarket <-extract_osm_objects(key = "building", value = "supermarket", bbox = bb)

#small shops
data_kiosk <-extract_osm_objects(key = "building", value = "commercial", bbox = bb)
data_retail <-extract_osm_objects(key = "building", value = "retail", bbox = bb)

#Leisure
data_leisure <- extract_osm_objects(key = 'leisure', bbox = bb)

#restaurant bar caffe
data_restaurant <- extract_osm_objects(key="amenity", value="restaurant",  bbox = bb, return_type = 'point')
data_bar <- extract_osm_objects(key="amenity", value="bar",return_type = "points", bbox = bb)
data_cafe <- extract_osm_objects(key="amenity", value="cafe",return_type = "points", bbox = bb)
data_fastfood <- extract_osm_objects(key="amenity", value="fast_food",return_type = "points", bbox = bb)

## process and store the data
#education
data_school$name = "school"
data_bar$name='bar'
data_cafe$name='cafe'
data_commerical$name='commercial'
data_fastfood$name='fastfood'
data_kid$name='kindergarten'
data_kiosk$name='kiosk'
data_leisure$name='leisure'
data_restaurant$name='restaurant'
data_retail$name="retail"
data_un1$name="univeristy"
data_college$name='college'
data_office$name = 'office'
data_appartment$name = 'appartment'
data_dormitory$name = 'dormitory'
data_house$name = 'house'
data_hotel$name = 'hotel'
data_residential$name = 'residential'
data_college$osm_id = rownames(data_college)
data_residential$osm_id = rownames(data_residential)
data_hotel$osm_id = rownames(data_hotel)
data_house$osm_id = rownames(data_house)
data_un1$osm_id = rownames(data_un1)
data_appartment$osm_id = rownames(data_appartment)
data_kid$osm_id = rownames(data_kid)
# process the data 
office = process(data_office)
house = process(data_house)
app = process(data_appartment)
dorm = process(data_dormitory)
res = process(data_residential)
hotel = process(data_hotel)
school = process(data_school)
college = process(data_college)
univerisity = process(data_un1)
kindergarden = process(data_kid)
commericial = process(data_commerical)
supermarket = process(data_supermarket)
kiosk = process(data_kiosk)
retail = process(data_retail)
leisure = process(data_leisure)
restaurant = process(data_restaurant)
cafe = process(data_cafe)
bar = process(data_bar)
fastfood = process(data_fastfood)

#combine 
overall = house

if(exists("school")){
  overall = rbind(overall, school)
}
if(exists("college")){
  overall = rbind(overall, college)
}
if(exists("univerisity")){
  overall = rbind(overall, univerisity)
}
if(exists("kindergarden")){
  overall = rbind(overall, kindergarden)
}
if(exists("commericial")){
  overall = rbind(overall, commericial)
}
if(exists("supermarket")){
  overall = rbind(overall, supermarket)
}
if(exists("kiosk")){
  overall = rbind(overall, kiosk)
}
if(exists("retail")){
  overall = rbind(overall, retail)
}

if(exists("leisure")){
  overall = rbind(overall, leisure)
}
if(exists("cafe")){
  overall = rbind(overall, cafe)
}
if(exists("bar")){
  overall = rbind(overall, bar)
}
if(exists("fastfood")){
  overall = rbind(overall, fastfood)
}
if(exists("office")){
  overall = rbind(overall, office)
}
if(exists("restaurant")){
  overall = rbind(overall, restaurant)
}
if(exists("app")){
  overall = rbind(overall, app)
}
if(exists("dorm")){
  overall = rbind(overall, dorm)
}
if(exists("res")){
  overall = rbind(overall, res)
}
if(exists("hotel")){
  overall = rbind(overall, hotel)
}

write.csv(overall, 'sao_paulo.csv', row.names = FALSE)


## coordinates form processing 
library(stringr)
target = read.csv('sao_paulo.csv')
long = str_split(target$lon, '\\(', simplify = TRUE)
target$lon = long[,2]
lat = str_split(target$lat, '\\)', simplify = TRUE)
target$lat = lat[,1]
write.csv(target, 'sao_paulo.csv', row.names = FALSE)

## make tags 
total = read.csv('sao_paulo.csv')
total = total[, -1]
tag = nrow(total)
tag[which(total$name == 'residential')] = 'Home'
tag[which(total$name == 'appartment')] = 'Home'
tag[which(total$name == 'house')] = 'Home'
tag[which(total$name == 'dormitory')] = 'Home'
tag[which(total$name == 'hotel')] = 'Home Work'
tag[which(total$name == 'office')] = 'Work'
tag[which(total$name == "school")] = 'Education Work'
tag[which(total$name == "bar")] = 'Leisure Work'
tag[which(total$name == "cafe")] = 'Leisure Work'
tag[which(total$name == "commercial")] = 'Shop Leisure Work'
tag[which(total$name == "fastfood")] = 'Leisure Work'
tag[which(total$name == "kindergarten")] = 'Education Work'
tag[which(total$name == "kiosk")] = 'Shop Work'
tag[which(total$name == "leisure")] = 'Leisure'
tag[which(total$name == "restaurant")] = 'Leisure Work'
tag[which(total$name == "retail")] = 'Shop Work'
tag[which(total$name == "university")] = 'Education Work'
tag[which(total$name == "college")] = 'Education Work'
tag[which(is.na(tag))] = 'Education Work'

total = cbind(total, tag)
write.csv(total, 'sao_paulo.csv', row.names = FALSE)
