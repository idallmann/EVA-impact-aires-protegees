library(ggplot2)
library(tidyverse)
library(countrycode)
library(data.table)
library(stringr)
library(xlsx)
library(ARTofR)
library(geojsonsf)
library(dplyr)
library(mapview)
library(terra)
library(tidyterra)
library(sf)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                               SCRIPT PURPOSE                             ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#This script performs the final processing steps for buffers (follows ScriptMT_buffer_filt)
# In this script, we import the buffers of the filtered deals (data_muller_filt) and out-of-sample deals (data_all)
# Then, we perform spatial substraction on the T and C areas for each deal in our sample :
#1. The overlap between two T areas (for pairs whose overlap < t) is spatially substracted
#2.1 The overlap between C_i and C_j!=i, or between C_i and T (including deals out-of-sample) is subtracted
#2.2 (other option) The overlap between C_i and T only (including deals out-of-sample) is subtracted
#3. The overlap between T and sea, or C and sea, is removed


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

#Deals in the sample after the buffer filter are imported
data_muller_filt = fread('D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/data_muller_filt.csv')
data_muller_loc_filt = st_read('D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/data_muller_loc_filt.geojson') %>%
  st_transform("+proj=longlat +datum=WGS84 +units=m")
data_muller_Tbuf_filt = st_read('D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/data_muller_Tbuf_filt.geojson') %>%
  st_transform("+proj=longlat +datum=WGS84 +units=m")
data_muller_C1buf_filt = st_read('D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/data_muller_C1buf_filt.geojson') %>%
  st_transform("+proj=longlat +datum=WGS84 +units=m") 
data_muller_C2buf_filt = st_read('D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/data_muller_C2buf_filt.geojson') %>%
  st_transform("+proj=longlat +datum=WGS84 +units=m") 
data_muller_C3buf_filt = st_read('D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/data_muller_C3buf_filt.geojson') %>%
  st_transform("+proj=longlat +datum=WGS84 +units=m") 

#continent boundaries

data_conti_bnd = st_read('D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/others/continents/continent-poly/Continents.shp') %>%
  st_transform("+proj=longlat +datum=WGS84 +units=m") %>%
  dplyr::select(c(CONTINENT, geometry))
vect_conti_bnd = vect(data_conti_bnd)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                          DEFINE RELEVANT DATASETS                        ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                      Dataset of parcels out of sample                    ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#List of parcels in the sample after the buffer filter
list_loc_muller_filt = data_muller_filt$loc_id

#Building dataset of parcels out of the sample
##Select parcels not in the sample, with accurate location and known size
##Define radius for building buffers
data_no_muller_filt = data_all %>%
  subset(!(loc_id %in% list_loc_muller_filt)) %>%
  subset(loc_accu == TRUE & is.na(size_max_par) == FALSE & size_max_par > 0) %>%
  mutate(size_max_m2 = size_max_par * 1e4,
         r_max = sqrt(size_max_m2/pi)) %>%
  left_join(select(data_all_loc, -c(deal_id)), by = "loc_id") %>%
  select(c(deal_id, loc_id, r_max, geometry))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                      Buffers of parcels out of sample                    ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#T buffers

Tbuf_no_muller_filt = buffer(x = vect(data_no_muller_filt$geometry), width = data_no_muller_filt$r_max)
Tbuf_no_muller_filt$deal_id = data_no_muller_filt$deal_id
Tbuf_no_muller_filt$loc_id = data_no_muller_filt$loc_id
Tbuf_no_muller_filt$r_max = data_no_muller_filt$r_max
Tbuf_no_muller_filt$area_buf = expanse(Tbuf_no_muller_filt)

#C buffers
#disk-shaped...
C1temp = buffer(x = Tbuf_no_muller_filt, width = Tbuf_no_muller_filt$r_max) 
C2temp = buffer(x = Tbuf_no_muller_filt, width = 2*Tbuf_no_muller_filt$r_max) 
C3temp = buffer(x = Tbuf_no_muller_filt, width = 3*Tbuf_no_muller_filt$r_max) 
#... and ring-shaped
C1buf_no_muller_filt = sapply(1:dim(Tbuf_no_muller_filt)[1], function(X) {erase(C1temp[X,], Tbuf_no_muller_filt[X,])}) %>% 
  vect() %>%
  select(c(deal_id, loc_id, area_buf))
C2buf_no_muller_filt = sapply(1:dim(Tbuf_no_muller_filt)[1], function(X) {erase(C2temp[X,], C1temp[X,])}) %>% 
  vect() %>%
  select(c(deal_id, loc_id, area_buf))
C3buf_no_muller_filt = sapply(1:dim(Tbuf_no_muller_filt)[1], function(X) {erase(C3temp[X,], C2temp[X,])}) %>% 
  vect() %>%
  select(c(deal_id, loc_id, area_buf))

#rm(C1temp, C2temp, C3temp)

# mapview(data_muller_Tbuf_filt) +
#   mapview(data_no_muller_filt_Tbuf) +
#   mapview(st_as_sf(C1buf_no_muller_filt)) +
#   mapview(st_as_sf(C2buf_no_muller_filt)) +
#   mapview(st_as_sf(C3buf_no_muller_filt)) 

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                          Spatial subtraction                            ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Subtract overlap of T
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Vectorize the buffer dataset for deals in the sample
Tbuf_muller_filt = vect(data_muller_Tbuf_filt)

#Crop the treated areas of the parcels in the sample
##First, overlaps between parcels in the sample are removed ...
Tbuf_muller_filt_crop_T = sapply(1:dim(Tbuf_muller_filt)[1], 
                                 function(X) {erase(Tbuf_muller_filt[X,], Tbuf_muller_filt[-X,])}) %>% 
  vect() %>%
  ##...then, overlaps between parcels in and out the sample are removed
  erase(Tbuf_no_muller_filt) %>%
  st_as_sf() %>%
  ##We keep in this dataset the area of T and cropped T
  mutate(area_crop_T = expanse(vect(geometry)),
         ratio_crop_T = area_crop_T/area_buf) %>%
  rename("area_T" = "area_buf")


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Subtract overlap of C with T and potentially C
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#A dataset with area of cropped T
Tbuf_muller_filt_crop_T_area = Tbuf_muller_filt_crop_T %>%
  st_drop_geometry() %>%
  select(c(loc_id, area_crop_T, area_T))

#Vectorize the C buffer dataset for parcels in the sample
C1buf_muller_filt = vect(data_muller_C1buf_filt) %>%
  select(c(deal_id, loc_id, area_buf)) 
C2buf_muller_filt = vect(data_muller_C2buf_filt) %>%
  select(c(deal_id, loc_id, area_buf)) 
C3buf_muller_filt = vect(data_muller_C3buf_filt) %>%
  select(c(deal_id, loc_id, area_buf))

#As treated areas of parcels both in and out of the sample, we merge both
#Same for control areas
#This aims at facilitating computations (std::bad_alloc errors otherwise)
T_all = rbind(Tbuf_muller_filt, Tbuf_no_muller_filt) %>%
  select(c(deal_id, loc_id, area_buf))
C1_all = rbind(C1buf_muller_filt, C1buf_no_muller_filt) %>%
  select(c(deal_id, loc_id, area_buf))
C2_all = rbind(C2buf_muller_filt, C2buf_no_muller_filt) %>%
  select(c(deal_id, loc_id, area_buf))
C3_all = rbind(C3buf_muller_filt, C3buf_no_muller_filt) %>%
  select(c(deal_id, loc_id, area_buf))

#For the crop1, need Cn of parcel i in the sample cropped by Cn of parcel j!=i in the sample
C1buf_muller_filt_scrop = sapply(1:dim(C1buf_muller_filt)[1], 
                                 function(X) {erase(C1buf_muller_filt[X,], C1buf_muller_filt[-X,])}) %>% 
  vect() 
C2buf_muller_filt_scrop = sapply(1:dim(C2buf_muller_filt)[1], 
                                 function(X) {erase(C2buf_muller_filt[X,], C2buf_muller_filt[-X,])}) %>% 
  vect() 
C3buf_muller_filt_scrop = sapply(1:dim(C3buf_muller_filt)[1], 
                                 function(X) {erase(C3buf_muller_filt[X,], C3buf_muller_filt[-X,])}) %>% 
  vect() 

# These out-of-sample buffers are recorded for potential computations under QGIS
# st_write(st_as_sf(C1buf_muller_filt),
#             "D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/C1_muller_filt.geojson",
#          delete_dsn = TRUE)
# st_write(st_as_sf(C2buf_muller_filt),
#             "D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/C2_muller_filt.geojson",
#          delete_dsn = TRUE)
# st_write(st_as_sf(C3buf_muller_filt),
#             "D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/C3_muller_filt.geojson",
#          delete_dsn = TRUE)
# st_write(st_as_sf(C1buf_muller_filt_scrop),
#             "D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/C1_muller_filt_scrop.geojson",
#          delete_dsn = TRUE)
# st_write(st_as_sf(C2buf_muller_filt_scrop),
#             "D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/C2_muller_filt_scrop.geojson",
#          delete_dsn = TRUE)
# st_write(st_as_sf(C3buf_muller_filt_scrop),
#             "D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/C3_muller_filt_scrop.geojson",
#          delete_dsn = TRUE)
# st_write(st_as_sf(C1buf_no_muller_filt),
#             "D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/C1_no_muller_filt.geojson",
#          delete_dsn = TRUE)
# st_write(st_as_sf(C2buf_no_muller_filt),
#             "D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/C2_no_muller_filt.geojson",
#          delete_dsn = TRUE)
# st_write(st_as_sf(C3buf_no_muller_filt),
#             "D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/C3_no_muller_filt.geojson",
#          delete_dsn = TRUE)
# st_write(st_as_sf(T_all),
#          "D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/T_all.geojson",
#          delete_dsn = TRUE)
# st_write(st_as_sf(C1_all),
#          "D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/C1_all.geojson",
#          delete_dsn = TRUE)
# st_write(st_as_sf(C2_all),
#          "D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/C2_all.geojson",
#          delete_dsn = TRUE)
# st_write(st_as_sf(C3_all),
#          "D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/C3_all.geojson",
#          delete_dsn = TRUE)


# mapview(st_as_sf(T_all)) +
#   mapview(st_as_sf(C1_all)) +
#   mapview(st_as_sf(C2_all)) +
#   mapview(st_as_sf(C3_all))


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Option muller : subtract overlap of Ci with Cj!=i and T  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##C1
C1buf_muller_filt_cropm = C1buf_muller_filt %>%
  #Overlaps between C of the parcels and T of an other parcel in/out of the sample are removed ...
  erase(T_all) %>%
  #Finally, erase overlap with C_j!=i of deals in/out of the sample
  erase(C2_all) %>%
  erase(C3_all)
  st_as_sf() %>%
  tidyterra::left_join(Tbuf_muller_filt_crop_T_area, by = "loc_id") %>%
  mutate(area_buf_crop = expanse(vect(geometry)),
         is_rm_crop = area_buf_crop < area_crop_T)

list_C1_filt_cropm_keep = C1buf_muller_filt_cropm %>%
  subset(is_rm_crop == FALSE) %>%
  select(loc_id) %>%
  st_drop_geometry()
list_C1_filt_cropm_keep = list_C1_filt_cropm_keep$loc_id

C1buf_muller_filt_cropm_qgis = st_read("D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/C1_Tall_C2all_C3all.geojson") %>%
  tidyterra::left_join(Tbuf_muller_filt_crop_T_area, by = "loc_id") %>%
  mutate(area_buf_crop = expanse(vect(geometry)),
         is_rm_crop = area_buf_crop < area_crop_T 
         )
sum(C1buf_muller_filt_cropm_qgis$is_rm_crop)

##C2
C2buf_muller_filt_cropm = C2buf_muller_filt %>%
  #Overlaps between C of the parcels and T of an other parcel in/out of the sample are removed ...
  erase(T_all) %>% 
  ##Finally, erase overlap with C_j!=i of deals in/out of the sample
  erase(C3_all) %>%
  erase(C1_all) %>%
  st_as_sf() %>%
  tidyterra::left_join(Tbuf_muller_filt_crop_T_area, by = "loc_id") %>%
  mutate(area_buf_crop = expanse(vect(geometry)),
         is_rm_crop = area_buf_crop < area_crop_T)

list_C2_filt_cropm_keep = C2buf_muller_filt_cropm %>%
  subset(is_rm_crop == FALSE) %>%
  select(loc_id) %>%
  st_drop_geometry()
list_C2_filt_cropm_keep = list_C2_filt_crop1_keep$loc_id

C2buf_muller_filt_cropm_qgis = st_read("D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/C2_Tall_C3all_C1all.geojson") %>%
  tidyterra::left_join(Tbuf_muller_filt_crop_T_area, by = "loc_id") %>%
  mutate(area_buf_crop = expanse(vect(geometry)),
         is_rm_crop = area_buf_crop < area_crop_T)
sum(C2buf_muller_filt_cropm_qgis$is_rm_crop)

##C3
C3buf_muller_filt_cropm = C3buf_muller_filt %>%
  #Overlaps between C of the parcels and T of an other parcel in/out of the sample are removed ...
  erase(T_all) %>%
  #Finally, erase overlap with C_j of deals in/out of the sample
  erase(C2_all) %>%
  erase(C1_all) %>%
  st_as_sf() %>%
  tidyterra::left_join(Tbuf_muller_filt_crop_T_area, by = "loc_id") %>%
  mutate(area_buf_crop = expanse(vect(geometry)),
         is_rm_crop = area_buf_crop < area_crop_T)

list_C3_filt_cropm_keep = C3buf_muller_filt_cropm %>%
  subset(is_rm_crop == FALSE) %>%
  select(loc_id) %>%
  st_drop_geometry()
list_C3_filt_cropm_keep = list_C3_filt_cropm_keep$loc_id

C3buf_muller_filt_cropm_qgis = st_read("D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/C3_Tall_C2all_C1all.geojson") %>%
  tidyterra::left_join(Tbuf_muller_filt_crop_T_area, by = "loc_id") %>%
  mutate(area_buf_crop = expanse(vect(geometry)),
         is_rm_crop = area_buf_crop < area_crop_T)
sum(C3buf_muller_filt_cropm_qgis$is_rm_crop)

#Which deals would be kept eventually ? The one with the three buffers

C1inC2_cropm = sum(list_C1_filt_cropm_keep %in% list_C2_filt_cropm_keep)
C1inC3_cropm = sum(list_C1_filt_cropm_keep %in% list_C3_filt_cropm_keep)


#Visualize
mapview(C1buf_muller_filt_cropm) +
  mapview(C1buf_muller_filt_cropm_qgis) +
  mapview(C2buf_muller_filt_cropm) +
  mapview(C2buf_muller_filt_cropm_qgis) +
  mapview(C3buf_muller_filt_cropm) +
  mapview(C3buf_muller_filt_cropm_qgis)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Option 1 : subtract overlap of Ci with Cj and T  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#HAS TO BE DONE ON GQIS : FACE AN ISSUE AND LET THAT PART FOR LATER IF TIME

##C1
# C1buf_muller_filt_crop1_qgis = st_read("D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/C1m_C1nom_Tall_C2all_C3all.geojson") %>%
#   tidyterra::left_join(Tbuf_muller_filt_crop_T_area, by = "loc_id") %>%
#   mutate(area_buf_crop = expanse(vect(geometry)),
#          is_rm_crop = area_buf_crop < area_crop_T 
#   )
# 
# list_C1_filt_crop1_keep = C1buf_muller_filt_crop1_qgis %>%
#   subset(is_rm_crop == FALSE) %>%
#   select(loc_id) %>%
#   st_drop_geometry()
# list_C1_filt_crop1_keep = list_C1_filt_crop1_keep$loc_id
# 
# ##C2
# 
# C2buf_muller_filt_crop1_qgis = st_read("D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/C2m_C2nom_Tall_C2all_C3all.geojson") %>%
#   tidyterra::left_join(Tbuf_muller_filt_crop_T_area, by = "loc_id") %>%
#   mutate(area_buf_crop = expanse(vect(geometry)),
#          is_rm_crop = area_buf_crop < area_crop_T 
#   )
# 
# list_C2_filt_crop1_keep = C2buf_muller_filt_crop1_qgis %>%
#   subset(is_rm_crop == FALSE) %>%
#   select(loc_id) %>%
#   st_drop_geometry()
# list_C2_filt_crop1_keep = list_C2_filt_crop1_keep$loc_id
# 
# ##C3
# 
# C3buf_muller_filt_crop1_qgis = st_read("D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/C3m_C3nom_Tall_C2all_C3all.geojson") %>%
#   tidyterra::left_join(Tbuf_muller_filt_crop_T_area, by = "loc_id") %>%
#   mutate(area_buf_crop = expanse(vect(geometry)),
#          is_rm_crop = area_buf_crop < area_crop_T 
#   )
# 
# list_C3_filt_crop1_keep = C3buf_muller_filt_crop1_qgis %>%
#   subset(is_rm_crop == FALSE) %>%
#   select(loc_id) %>%
#   st_drop_geometry()
# list_C3_filt_crop1_keep = list_C3_filt_crop1_keep$loc_id
# 
# #Which deals would be kept eventually ? The one with the three buffers
# 
# C1inC2_crop1 = sum(list_C1_filt_crop1_keep %in% list_C2_filt_crop1_keep)
# C1inC3_crop1 = sum(list_C1_filt_crop1_keep %in% list_C3_filt_crop1_keep)
# 
# 
# #Visualize
# mapview(C1buf_muller_filt_crop1) +
#   mapview(C1buf_muller_filt_crop1_qgis) +
#   mapview(C2buf_muller_filt_crop1) +
#   mapview(C2buf_muller_filt_crop1_qgis) +
#   mapview(C3buf_muller_filt_crop1) +
#   mapview(C3buf_muller_filt_crop1_qgis)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Option 2 : subtract overlap of Ci with T only  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##C1
C1buf_muller_filt_crop2 = C1buf_muller_filt %>%
  #Overlaps between C of the parcels and T of an other parcel in/out of the sample are removed ...
  erase(T_all) %>%
  st_as_sf() %>%
  tidyterra::left_join(Tbuf_muller_filt_crop_T_area, by = "loc_id") %>%
  mutate(area_buf_crop = expanse(vect(geometry)),
         is_rm_crop = area_buf_crop < area_crop_T)

list_C1_filt_crop2_keep = C1buf_muller_filt_crop2 %>%
  subset(is_rm_crop == FALSE) %>%
  select(loc_id) %>%
  st_drop_geometry()
list_C1_filt_crop2_keep = list_C1_filt_crop2_keep$loc_id

##C2
C2buf_muller_filt_crop2 = C2buf_muller_filt %>%
  #Overlaps between C of the parcels and T of an other parcel in/out of the sample are removed ...
  erase(T_all) %>% 
  st_as_sf() %>%
  tidyterra::left_join(Tbuf_muller_filt_crop_T_area, by = "loc_id") %>%
  mutate(area_buf_crop = expanse(vect(geometry)),
         is_rm_crop = area_buf_crop < area_crop_T)

list_C2_filt_crop2_keep = C2buf_muller_filt_crop2 %>%
  subset(is_rm_crop == FALSE) %>%
  select(loc_id) %>%
  st_drop_geometry()
list_C2_filt_crop2_keep = list_C2_filt_crop2_keep$loc_id

##C3
C3buf_muller_filt_crop2 = C3buf_muller_filt %>%
  #Overlaps between C of the parcels and T of an other parcel in/out of the sample are removed ...
  erase(T_all) %>%
  st_as_sf() %>%
  tidyterra::left_join(Tbuf_muller_filt_crop_T_area, by = "loc_id") %>%
  mutate(area_buf_crop = expanse(vect(geometry)),
         is_rm_crop = area_buf_crop < area_crop_T)

list_C3_filt_crop2_keep = C3buf_muller_filt_crop2 %>%
  subset(is_rm_crop == FALSE) %>%
  select(loc_id) %>%
  st_drop_geometry()
list_C3_filt_crop2_keep = list_C3_filt_crop2_keep$loc_id

# mapview(C1buf_muller_filt_crop2) +
#   mapview(C2buf_muller_filt_crop2) + 
#   mapview(C3buf_muller_filt_crop2)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Substract overlap of seas with T/C  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##PAS FAIT DANS UN PREMIER TEMPS 
##OCEANS PEUVENT ETRE ENLEVES PLUS PROPREMENT DEPUIS LES RASTERS DIRECTEMENT

# mapview(data_conti_bnd$geometry)
# 
# #We perfom the following operation :
# ##Erase continent from T buffers : we get only part of the T buffer in the sea
# ##Substract this to the T buffers : we get the buffer with part overlapping the sea removed
# Tbuf_muller_filt_crop_T_sea = erase(vect(Tbuf_muller_filt_crop_T), erase(vect(Tbuf_muller_filt_crop_T), vect_conti_bnd)) %>%
#   st_as_sf() %>%
#   select(c(deal_id, loc_id, geometry))
# C1buf_muller_filt_crop_sea = erase(vect(C1buf_muller_filt_crop1), erase(vect(C1buf_muller_filt_crop1), vect_conti_bnd)) %>%
#   st_as_sf()
# C2buf_muller_filt_crop_sea = erase(vect(C2buf_muller_filt_crop1), erase(vect(C2buf_muller_filt_crop1), vect_conti_bnd)) %>%
#   st_as_sf()
# C3buf_muller_filt_crop_sea = erase(vect(C3buf_muller_filt_crop1), erase(vect(C3buf_muller_filt_crop1), vect_conti_bnd)) %>%
#   st_as_sf()
# 
# mapview(data_muller_Tbuf_filt) +
#   mapview(data_no_muller_filt_Tbuf) +
#   mapview(Tbuf_muller_filt_crop_T) +
#   mapview(Tbuf_muller_filt_crop_T_sea) +
#   mapview(C1buf_muller_filt_crop_sea) +
#   mapview(C2buf_muller_filt_crop_sea) +
#   mapview(C3buf_muller_filt_crop_sea) 

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                          Recording relevant files                        ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data_muller_filt_crop = Tbuf_muller_filt_crop_T %>%
  select(c(loc_id)) %>%
  left_join(data_all, by = "loc_id") %>%
  st_drop_geometry()
#   
# fwrite(data_muller_filt_crop, 'D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/filt_crop/data_muller_filt_crop.csv')
# #
# st_write(Tbuf_muller_filt_crop_T,
#          dsn = 'D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/filt_crop/Tbuf_muller_filt_crop.geojson',
#          delete_dsn = TRUE)
# 
# st_write(C1buf_muller_filt_crop1,
#          "D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/filt_crop/C1buf_muller_filt_crop1.geojson",
#          delete_dsn = TRUE)
# st_write(C2buf_muller_filt_crop1,
#          "D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/filt_crop/C2buf_muller_filt_crop1.geojson",
#          delete_dsn = TRUE)
# st_write(C3buf_muller_filt_crop1,
#          "D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/filt_crop/C3buf_muller_filt_crop1.geojson",
#          delete_dsn = TRUE)
# 
# 
# st_write(C1buf_muller_filt_crop2,
#          "D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/filt_crop/C1buf_muller_filt_crop2.geojson",
#          delete_dsn = TRUE)
# st_write(C2buf_muller_filt_crop2,
#          "D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/filt_crop/C2buf_muller_filt_crop2.geojson",
#          delete_dsn = TRUE)
# st_write(C3buf_muller_filt_crop2,
#          "D:/Documents/Cours/M2_ENS/Cours/MasterThesis/CommonsVulnerabilityCC/data/LMD/filt_crop/C3buf_muller_filt_crop2.geojson",
#          delete_dsn = TRUE)