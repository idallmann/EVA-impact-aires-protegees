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

tempdir()
dir.create(tempdir())

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                               SCRIPT PURPOSE                             ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Three scripts deal with buffers : definition, filtering of parcels regarding the buffers, and spatial adaptation of buffers
#This script performs the first step : defining buffers
# In this script, we import the filtered deals and perform the following operations :
#   1. Define a buffer for the average location of each deal (matters for deals associated with more than one location)
    # 2. Define a buffer for each parcel
    # 3. Define the surrounding, ring-shaped control buffers for the second type of buffer (the first being not used for the moment) 
#     4. Define a buffer for each deal with accurate location, known size, but not in our first dataset
#     5. All buffers are saved as geojson files

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

#Filtered deals are imported
data_muller = fread('D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/data_muller.csv')
data_muller_loc = st_read('D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/data_muller_loc.geojson') %>%
  st_transform("+proj=longlat +datum=WGS84 +units=m")
data_muller_areas = st_read('D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/data_muller_areas.geojson') %>%
  st_transform("+proj=longlat +datum=WGS84 +units=m")
 


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                               REFINING LOCATION OF DEALS                 ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# For each deal, more than one location can be reported
#Two approaches here :
##If n>1 locations are reported, we take the average location. Thus each deal has a unique location. This can be problematic if the parcels are far away
##If n>1 locations are reported, we keep them as "subdeals" and each location is associated to the size total_area/n. Solution of Muller et al 2021
#This will lead to two sets of buffers

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                   Several locations --> average location                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# /!\ OLD SOLUTION

# For deals with more than one location, we take the average coordinate. 

# data_muller_loc_avg = data_muller_loc %>%
#   #Extracting x and y coordinates of each location
#   mutate(x = as.data.frame(st_coordinates(geometry))$X,
#          y = as.data.frame(st_coordinates(geometry))$Y) %>%
#   st_drop_geometry() %>%
#   #Group by deal_id to take the average of x and y for each deal
#   group_by(deal_id) %>%
#   summarise(x_avg = mean(x),
#             y_avg = mean(y)) %>%
#   #Convert back coordinates to sf objet with projection 4326 (WGS84)
#   st_as_sf(coords = c("x_avg", "y_avg"), 
#            remove = TRUE,
#            crs = 4326) %>%
#   ungroup() 
# 
# mapview(data_muller_loc$geometry) + 
#   mapview(data_muller_loc_avg$geometry)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                               DEFINING BUFFERS                            ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Now we have relevant sizes and two location datasets (average location for each deal, location for each parcels/sub-deals), we can define the buffers :
##Each area given in ha is converted in m2
##Given each area, the radius of a corresponding disk is computed
##Disk-shape buffers are computed with the terra package (smoother borders than with sf::st_buffer)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                              Average location                            ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# /!\ OLD SOLUTION

#One deal = one location. If several locations for one deal, an average is taken.

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Extracting info to define buffers  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Create the dataset with average location and radius of the area for each deal
#Join dataset with deal info (data_muller) with the dataset of average locations
# data_muller_buf_avg_info = data_muller %>%
#   left_join(data_muller_loc_avg, by = "deal_id") %>%
#   #Compute areas in km2, m2 and the radius in m
#   mutate(size_max_km2 = size_max/100,
#          size_max_m2 = size_max * 1e4,
#          r_max = sqrt(size_max_m2/pi),
#          .after = size_max) %>%
#   select(c(deal_id, geometry, r_max))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Creating the buffers  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Treated area
#Note that the width corresponds to the radius of the disk-shaped buffer
#To the buffer is added the deal id as an attribute
# Tbuf_avg = buffer(x = vect(data_muller_buf_avg_info$geometry), width = data_muller_buf_avg_info$r_max)
# Tbuf_avg$deal_id = data_muller_buf_avg_info$deal_id
# Tbuf_avg$r_max = data_muller_buf_avg_info$r_max
# 
# mapview(st_as_sf(Tbuf_avg))

#Control areas
##Three donut-shaped areas are defined surrounding the treated area
##Each has thickness = radius of the treated area
##In practice, we build three buffer of radius r_max, 2*r_max, 3*r_max around the first buffer. Then, the previous buffers are removed

# C1buf_avg = buffer(x = Tbuf_avg, width = Tbuf_avg$r_max) %>% erase(Tbuf_avg)
# C2buf_avg = buffer(x = Tbuf_avg, width = 2*Tbuf_avg$r_max) %>% erase(C1buf_avg) %>% erase(Tbuf_avg)
# C3buf_avg = buffer(x = Tbuf_avg, width = 3*Tbuf_avg$r_max) %>% erase(C2buf_avg) %>% erase(C1buf_avg) %>% erase(Tbuf_avg)
# 
# mapview(st_as_sf(Tbuf_avg)) + mapview(st_as_sf(C1buf_avg)) + mapview(st_as_sf(C2buf_avg)) + mapview(st_as_sf(C3buf_avg))


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##            Several locations --> subdeals with a % of deal size          ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#If n>1 locations for a deal, we define sub-buffers centered at each location with area = total_area/n 

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Extracting info to define buffers  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data_muller_buf_info = data_muller %>%
  left_join(select(data_muller_loc, -c(deal_id)), by = "loc_id") %>%
  #For all parcels are computed the radius of a disk with same area
  mutate(size_max_m2 = size_max_par * 1e4,
         r_max = sqrt(size_max_m2/pi),
         nrow = as.integer(rownames(.))) %>%
  select(c(deal_id, loc_id, nrow, geometry, r_max))

# st_write(data_muller_buf_info,
#        "D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/data_muller_buf_info.geojson",
#        delete_dsn = TRUE)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Creating the buffers  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Treated area
##Note that the width corresponds to the radius of the disk-shaped buffer
##To the buffer is added the deal id as an attribute
Tbuf_muller = buffer(x = vect(data_muller_buf_info$geometry), width = data_muller_buf_info$r_max)
Tbuf_muller$deal_id = data_muller_buf_info$deal_id
Tbuf_muller$loc_id = data_muller_buf_info$loc_id
Tbuf_muller$r_max = data_muller_buf_info$r_max
Tbuf_muller$size_max = pi*data_muller_buf_info$r_max^2
Tbuf_muller$nrow = data_muller_buf_info$nrow

#Export it to geojson file
# st_write(st_as_sf(Tbuf_muller),
#          dsn = 'D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/data_muller_Tbuf.geojson',
#          delete_dsn = TRUE)


#Control areas
##Three donut-shaped areas are defined surrounding the treated area
##Each has thickness = radius of the treated area
##In practice, we build three buffer of radius r_max, 2*r_max, 3*r_max around the first buffer. 
##Then, the previous buffers are removed

C1temp = buffer(x = Tbuf_muller, width = Tbuf_muller$r_max) 
C2temp = buffer(x = Tbuf_muller, width = 2*Tbuf_muller$r_max) 
C3temp = buffer(x = Tbuf_muller, width = 3*Tbuf_muller$r_max) 

C1buf_muller = sapply(1:dim(Tbuf_muller)[1], function(X) {erase(C1temp[X,], Tbuf_muller[X,])}) %>% vect()
C2buf_muller = sapply(1:dim(Tbuf_muller)[1], function(X) {erase(C2temp[X,], C1temp[X,])}) %>% vect()
C3buf_muller = sapply(1:dim(Tbuf_muller)[1], function(X) {erase(C3temp[X,], C2temp[X,])}) %>% vect()
  
mapview(st_as_sf(Tbuf_muller)) + mapview(st_as_sf(C1buf_muller))  + mapview(st_as_sf(C2buf_muller)) + mapview(st_as_sf(C3buf_muller)) 


#Export it to geojson file

# st_write(select(st_as_sf(C1buf_muller), c(deal_id, loc_id, nrow, r_max, geometry)),
#          dsn = 'D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/data_muller_C1buf.geojson',
#          delete_dsn = TRUE)
# st_write(select(st_as_sf(C2buf_muller), c(deal_id, loc_id, nrow, r_max, geometry)),
#          dsn = 'D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/data_muller_C2buf.geojson',
#          delete_dsn = TRUE)
# st_write(select(st_as_sf(C3buf_muller), c(deal_id, loc_id, nrow, r_max, geometry)),
#          dsn = 'D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/data_muller_C3buf.geojson',
#          delete_dsn = TRUE)



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            Deals out of sample                           ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#From the dataset with all deals, remove deals from data_muller, 
#filtering deals with accurate locations and compute treated area

##Take the list of deals in data_muller
loc_id_muller = data_muller$loc_id

#Compute an informative dataset for out-of-sample deals
data_no_muller_buf_info = data_all %>%
  #Take out deals in data_muller (the one we work with) and take only parcels with accurate locations and known size
  subset(!(loc_id %in% loc_id_muller) & loc_accu == TRUE & is.na(size_max_par) == FALSE) %>%
  left_join(select(data_all_loc, -c(deal_id)), by = "loc_id") %>%
  #For all parcels are computed the radius of a disk with same area
  mutate(size_max_m2 = size_max_par * 1e4,
         r_max = sqrt(size_max_m2/pi),
         nrow = as.integer(rownames(.))) %>%
  select(c(deal_id, loc_id, nrow, geometry, r_max))

##Treated area
###Note that the width corresponds to the radius of the disk-shaped buffer
###To the buffer is added the deal id and other attributes 

Tbuf_no_muller = buffer(x = vect(data_no_muller_buf_info$geometry), width = data_no_muller_buf_info$r_max)
Tbuf_no_muller$deal_id = data_no_muller_buf_info$deal_id
Tbuf_no_muller$loc_id = data_no_muller_buf_info$loc_id
Tbuf_no_muller$r_max = data_no_muller_buf_info$r_max
Tbuf_no_muller$size_max = pi*data_no_muller_buf_info$r_max^2
Tbuf_no_muller$nrow = data_muller_buf_info$nrow

mapview(st_as_sf(Tbuf_no_muller))

# Export it to geojson file
# st_write(st_as_sf(Tbuf_no_muller),
#          dsn = 'D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/data_no_muller_Tbuf.geojson',
#          delete_dsn = TRUE)


##############END##################




