library(tidyverse)
library(sf)

setwd('C:/Users/dapon/Dropbox/Harvard/dissertation/data')

places <- read_csv("uk_geography/uk_placenames_wiki.csv") %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  as_tibble() %>%
  select(-coordinates) %>% 
  mutate(index = seq(1, nrow(.), 1))

shp <- st_read("uk_geography/Westminster_Parliamentary_Constituencies__December_2017__Boundaries_UK.shp")
shp_crs <- st_crs(shp)



#transform coordinates of places into sf file
places_sf <- sf::st_as_sf(places, coords=c("longitude", "latitude"), crs = "WGS84")
#test <- test %>% st_set_crs(., shp_crs)
#test <- st_transform(test, "+proj=longlat +datum=WGS84")
places_sf <- st_transform(places_sf, crs = shp_crs) 



#the col.id column corresponds to constituencies by their row placement in shp
inter <- sf::st_intersects(places_sf, shp, sparse = TRUE) %>% as.data.frame() %>% as_tibble()
#note that some places don't have constituencies (entry 9, for instance)


#get vector of constituency names
constits <- cbind(shp$pcon17cd, shp$pcon17nm) %>% as.data.frame() %>%
  rename(code = V1, name = V2)

#loop through inter$col.id to match the numbers to constituency names 
const <- matrix(nrow = nrow(inter), ncol = 2)


for( i in 1:nrow(inter)) {
  print(i)
  const[i,1] <- constits[inter$col.id[i],1] # this is constituency code
  const[i,2] <- constits[inter$col.id[i],2] # constituency name
 # const[i,3] < inter$row.id[i] # index in places df, for merging
}
const <- const %>% as_tibble() %>%
  rename(code = V1, name = V2)

const$index <- (inter$row.id)

#places <- places %>% mutate(index = as.character(index))


full <- left_join(places, const, by = "index")


