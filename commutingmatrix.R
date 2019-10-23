 # import the file 
target=read.csv("ventura.csv", stringsAsFactors = FALSE) 

# to fill the NAs with 0s
fill <- function(matr){
  matr[matr==""] = NA
  str = zoo::na.locf(matr$RESIDENCE)
  matr$RESIDENCE = str
  return(matr)
}
target = fill(target)
target[is.na(target)] = 0


#residence
library(stringr)
#split
residence = as.data.frame(str_split(target$RESIDENCE, ",", simplify = TRUE))
#state id 
residence$state_id = 6
#county id 
residence$county_id = NA
residence$county_id[which(residence$V2==" Alameda County")] = 1
residence$county_id[which(residence$V2==" Contra Costa County")] = 13
residence$county_id[which(residence$V2==" San Francisco County")] = 75
residence$county_id[which(residence$V2==" Marin County")] = 41
residence$county_id[which(residence$V2==" Napa County")] = 55
residence$county_id[which(residence$V2==" Santa Clara County")] = 85
residence$county_id[which(residence$V2==" San Mateo County")] = 81
residence$county_id[which(residence$V2==" Solano County")] = 95
residence$county_id[which(residence$V2==" Sonoma County")] = 97
residence$county_id[which(residence$V2==" Los Angeles County")] = 37
residence$county_id[which(residence$V2==" Riverside County")] = 65
residence$county_id[which(residence$V2==" Orange County")] = 59
residence$county_id[which(residence$V2==" San Bernardino County")] = 71
residence$county_id[which(residence$V2==" Ventura County")] = 111
tract = as.data.frame(str_split(residence$V1, " ", simplify = TRUE))
residence$tract_id = tract$V3
residence$tract_id = as.numeric(as.character(residence$tract_id))
residence$geoid = residence$state_id*1000000000 + residence$county_id*1000000 + residence$tract_id*100
target$res_geoid = residence$geoid


# workplace
#split
workplace = as.data.frame(str_split(target$WORKPLACE, ",", simplify = TRUE))
#state id 
workplace$state_id = 6
#county id 
workplace$county_id = NA
workplace$county_id[which(workplace$V2==" Alameda County")] = 1
workplace$county_id[which(workplace$V2==" Contra Costa County")] = 13
workplace$county_id[which(workplace$V2==" San Francisco County")] = 75
workplace$county_id[which(workplace$V2==" Marin County")] = 41
workplace$county_id[which(workplace$V2==" Napa County")] = 55
workplace$county_id[which(workplace$V2==" Santa Clara County")] = 85
workplace$county_id[which(workplace$V2==" San Mateo County")] = 81
workplace$county_id[which(workplace$V2==" Solano County")] = 95
workplace$county_id[which(workplace$V2==" Sonoma County")] = 97
workplace$county_id[which(workplace$V2==" Los Angeles County")] = 37
workplace$county_id[which(workplace$V2==" Riverside County")] = 65
workplace$county_id[which(workplace$V2==" Orange County")] = 59
workplace$county_id[which(workplace$V2==" San Bernardino County")] = 71
workplace$county_id[which(workplace$V2==" Ventura County")] = 111
tract = as.data.frame(str_split(workplace$V1, " ", simplify = TRUE))
workplace$tract_id = tract$V3
workplace$tract_id = as.numeric(as.character(workplace$tract_id))
workplace$geoid = workplace$state_id*1000000000 + workplace$county_id*1000000 + workplace$tract_id*100
target$work_geoid = workplace$geoid

write.csv(target, "ventura2othertracts_allworkers_0909.csv")

