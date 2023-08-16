library(ggplot2)
library(tidyverse)
library(countrycode)
library(data.table)
library(stringr)
library(xlsx)
library(ARTofR)
library(geojsonsf)
library(sf)
library(rgdal)
library(dplyr)
library(mapview)
library(terra)
library(tidyterra)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                               SCRIPT PURPOSE                             ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# In this script, we import the buffers of the filtered deals (data_muller) and out-of-sample deals (data_all)
# Then, we perform the final filtering of deals and buffer areas :
# 1. An overlap of T areas > threshold t -> both deals are removed
#2. If the treated area of a filtered deal includes a major urban agglomeration, it is removed

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                               IMPORTING DATA                             ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Base dataset with information, locations and areas of all deals is imported
data_all = fread('D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/data_roi.csv')
data_all_loc = st_read('D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/data_roi_loc.geojson') %>%
  st_transform("+proj=longlat +datum=WGS84 +units=m")
data_all_areas = st_read('D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/data_roi_areas.geojson') %>%
  st_transform("+proj=longlat +datum=WGS84 +units=m")
#Is also imported deals not in Muller but with accurate location and known size, with a buffer defined
data_no_muller_Tbuf = st_read('D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/data_no_muller_Tbuf.geojson') %>%
  st_transform("+proj=longlat +datum=WGS84 +units=m") %>%
  mutate(area_buf = expanse(vect(geometry), unit = "m", transform = TRUE)) %>%
  select(-c(r_max, nrow, size_max))

#Filtered deals are imported
data_muller = fread('D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/data_muller.csv')
data_muller_loc = st_read('D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/data_muller_loc.geojson') %>%
  st_transform("+proj=longlat +datum=WGS84 +units=m")
data_muller_areas = st_read('D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/data_muller_areas.geojson') %>%
  st_transform("+proj=longlat +datum=WGS84 +units=m")
data_muller_Tbuf = st_read('D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/data_muller_Tbuf.geojson') %>%
  st_transform("+proj=longlat +datum=WGS84 +units=m") %>%
  mutate(area_buf = expanse(vect(geometry), unit = "m", transform = TRUE)) %>%
  select(-c(r_max, r_max, nrow, size_max))
data_muller_C1buf = st_read('D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/data_muller_C1buf.geojson') %>%
  st_transform("+proj=longlat +datum=WGS84 +units=m") %>%
  mutate(area_buf = expanse(vect(geometry), unit = "m", transform = TRUE)) %>%
  select(-c(r_max, nrow))
data_muller_C2buf = st_read('D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/data_muller_C2buf.geojson') %>%
  st_transform("+proj=longlat +datum=WGS84 +units=m") %>%
  mutate(area_buf = expanse(vect(geometry), unit = "m", transform = TRUE)) %>%
  select(-c(r_max, nrow))
data_muller_C3buf = st_read('D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/data_muller_C3buf.geojson') %>%
  st_transform("+proj=longlat +datum=WGS84 +units=m") %>%
  mutate(area_buf = expanse(vect(geometry), unit = "m", transform = TRUE)) %>%
  select(-c(r_max, nrow))

#Data on cities and population across world

data_city = st_read('D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/cities/geonames-all-cities-with-a-population-1000.geojson') %>%
  st_transform("+proj=longlat +datum=WGS84 +units=m") 

#Muller et al. 2021 remove deals overlapping "major agglomeration" (not defined).
#Population threshold above which a city is a major agglomeration : th_pop
th_pop = 1e5
data_city_vect = vect(data_city) %>% select(c(label_en, name, population)) %>% 
  st_as_sf() %>% 
  subset(population > th_pop) %>%
  vect()


#visualization
mapview(data_all_Tbuf) +
  mapview(data_all_Tbuf) + 
  mapview(data_muller_Tbuf) + 
  mapview(data_muller_C1buf) + 
  mapview(data_muller_C2buf) + 
  mapview(data_muller_C3buf) +
  mapview(st_as_sf(data_city_vect)) + 
  mapview(test)




##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##--------------------- FILTERING THE DEALS FROM BUFFER-------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Overlap between T of parcels----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Define an overlapping threshold : 
#If the total area overlapped by other parcels is above th_over*100%, then the parcel is removed 
th_over = .5

#Bind the buffers of parcels in and out of our sample : every deal known with accurate location and known size
data_all_Tbuf = rbind(data_no_muller_Tbuf, data_muller_Tbuf)

#Then, compute the overlap between parcels in  our sample, and the bound one (in+out of sample)
data_overlap_muller = intersect(vect(data_muller_Tbuf), vect(data_all_Tbuf)) %>%
  st_as_sf() %>%
  #Remove self-intersection (deal overlap with themselves)
  subset(loc_id != loc_id.1) %>%
  #Compute area of the overlap and the ratio area/overlapping for each pair
  mutate(area_over = expanse(vect(geometry)),
         ratio_over = area_over/area_buf) %>%
  #For each parcel, compute the total overlap (area and ratio) and a dummy (1 if overlap > th_over)
  group_by(loc_id) %>%
  mutate(area_over_tot = sum(area_over),
         ratio_over_tot = area_over_tot/area_buf,
         to_rm = ratio_over_tot > th_over) %>%
  #One parcel can be present several time if overlap by multiple parcels
  #We keep only one row per parcel, with information on total overlap
  slice(1) %>%
  ungroup() %>%
  #Relevant variables finally selected
  select(c(deal_id, loc_id, area_buf, area_over_tot, ratio_over_tot, to_rm)) %>%
  st_drop_geometry()

list_loc_rm_over = subset(data_overlap_muller, to_rm == TRUE)$loc_id


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Overlap between parcel and major agglomeration  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#We want to remove any parcel whose T/C areas overlap a major agglomeration (defined above)

#Define a SpatVector object with T and C areas of each parcel
data_muller_buf = rbind(data_muller_Tbuf, data_muller_C1buf, data_muller_C2buf, data_muller_C3buf)
#Identify the cities locations intersected by one of the T/C areas
data_over_city_muller = intersect(vect(data_muller_buf), data_city_vect) %>% st_as_sf ()
#List of concerned parcels
list_loc_rm_city = unique(data_over_city_muller$loc_id)

rm(data_muller_buf)

# test = subset(data_over_city_muller, loc_id %in% list_loc_rm_city)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##          ~  Final filtering                  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data_muller_filt = data_muller %>%
  subset(!(loc_id %in% list_loc_rm_over)) %>%
  subset(!(loc_id %in% list_loc_rm_city))
data_muller_loc_filt = data_muller_loc %>%
  subset(!(loc_id %in% list_loc_rm_over)) %>%
  subset(!(loc_id %in% list_loc_rm_city))
data_muller_Tbuf_filt = data_muller_Tbuf %>%
  subset(!(loc_id %in% list_loc_rm_over)) %>%
  subset(!(loc_id %in% list_loc_rm_city))
data_muller_C1buf_filt = data_muller_C1buf %>%
  subset(!(loc_id %in% list_loc_rm_over)) %>%
  subset(!(loc_id %in% list_loc_rm_city))
data_muller_C2buf_filt = data_muller_C2buf %>%
  subset(!(loc_id %in% list_loc_rm_over)) %>%
  subset(!(loc_id %in% list_loc_rm_city))
data_muller_C3buf_filt = data_muller_C3buf %>%
  subset(!(loc_id %in% list_loc_rm_over)) %>%
  subset(!(loc_id %in% list_loc_rm_city))

# fwrite(data_muller_filt, 'D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/data_muller_filt.csv')
# 
# st_write(data_muller_loc_filt,
#          dsn = 'D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/data_muller_loc_filt.geojson',
#          delete_dsn = TRUE)
# st_write(data_muller_Tbuf_filt,
#          dsn = 'D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/data_muller_Tbuf_filt.geojson',
#          delete_dsn = TRUE)
# st_write(data_muller_C1buf_filt,
#          dsn = 'D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/data_muller_C1buf_filt.geojson',
#          delete_dsn = TRUE)
# st_write(data_muller_C2buf_filt,
#          dsn = 'D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/data_muller_C2buf_filt.geojson',
#          delete_dsn = TRUE)
# st_write(data_muller_C3buf_filt,
#          dsn = 'D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/data_muller_C3buf_filt.geojson',
#          delete_dsn = TRUE)

