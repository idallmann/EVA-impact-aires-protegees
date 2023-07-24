# Biodiversity in the protected areas

THIS SCRIPT IS UNFINISHED

In this document are computed and plotted descriptive statistics for the protected areas (PAs) using the mapme biodiversity package. This package provides an easy access to biodiversity-related data and allow to compute different indicators.

## Importing packages and functions

#```{r setup, include=FALSE, eval = FALSE}
#knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())
#```


```r
source("scripts/Statistics/fns_DesStat_biodiv.R", encoding = "utf-8")
```


```r
#The last version of mapme.biodiversity package is downloaded directly from github
remotes::install_github("mapme-initiative/mapme.biodiversity", upgrade="always")
library(mapme.biodiversity)
library(sf)
library(tidyverse)
library(mapview)
library(magrittr)
library(stargazer)
library(dplyr)
library(openxlsx)
library(writexl)
library(ggplot2)
library(questionr)
library(readxl)
library(data.table)
library(sp)
library(raster)
library(terra)
library(janitor)
library(ARTofR)
library(aws.s3)
```


```r
#A first look at mapme biodiversity indicators

resources <- names(available_resources())
indicators <- names(available_indicators())
cat(paste("Supported resources:\n-",
            paste(resources, collapse = "\n- "),
          "\n\nSupported indicators:\n-",
            paste(indicators, collapse = "\n- ")))
```

## Setting the environment

This might be necessary to access SSPCloud storage from mapme.biodiversity functions.


```r
# Sys.setenv("AWS_ACCESS_KEY_ID" = "GD8CI0UQQPTETOZS65J2",
#            "AWS_SECRET_ACCESS_KEY" = "e0bsXE55qz+xmNeVdwhTb39wKoQuIJFC0Y6eqPoG",
#            "AWS_DEFAULT_REGION" = "us-east-1",
#            "AWS_SESSION_TOKEN" = "eyJhbGciOiJIUzUxMiIsInR5cCI6IkpXVCJ9.eyJhY2Nlc3NLZXkiOiJHRDhDSTBVUVFQVEVUT1pTNjVKMiIsImFsbG93ZWQtb3JpZ2lucyI6WyIqIl0sImF1ZCI6WyJtaW5pby1kYXRhbm9kZSIsIm9ueXhpYSIsImFjY291bnQiXSwiYXV0aF90aW1lIjoxNjg4NjUxNjk5LCJhenAiOiJvbnl4aWEiLCJlbWFpbCI6InZ1aWxsb3RhQGFmZC5mciIsImVtYWlsX3ZlcmlmaWVkIjp0cnVlLCJleHAiOjE2ODg3Mzg4NzgsImZhbWlseV9uYW1lIjoiVnVpbGxvdCIsImdpdmVuX25hbWUiOiJBbnRvaW5lIiwiZ3JvdXBzIjpbImFmZC1ldmEtYXAiXSwiaWF0IjoxNjg4NjUxNzAwLCJpc3MiOiJodHRwczovL2F1dGgubGFiLnNzcGNsb3VkLmZyL2F1dGgvcmVhbG1zL3NzcGNsb3VkIiwianRpIjoiN2ZmMjJhNmEtM2IyYS00NWFiLWJmYTItNmVhZDZkNzY5YzBhIiwibmFtZSI6IkFudG9pbmUgVnVpbGxvdCIsIm5vbmNlIjoiYjQzM2E1N2MtMjZiNS00NmE0LTkzOGItOGMyM2FmOTVlYTI5IiwicG9saWN5Ijoic3Rzb25seSIsInByZWZlcnJlZF91c2VybmFtZSI6InZ1aWxsb3RhIiwicmVhbG1fYWNjZXNzIjp7InJvbGVzIjpbIm9mZmxpbmVfYWNjZXNzIiwidW1hX2F1dGhvcml6YXRpb24iLCJkZWZhdWx0LXJvbGVzLXNzcGNsb3VkIl19LCJyZXNvdXJjZV9hY2Nlc3MiOnsiYWNjb3VudCI6eyJyb2xlcyI6WyJtYW5hZ2UtYWNjb3VudCIsIm1hbmFnZS1hY2NvdW50LWxpbmtzIiwidmlldy1wcm9maWxlIl19fSwic2NvcGUiOiJvcGVuaWQgcHJvZmlsZSBncm91cHMgZW1haWwiLCJzZXNzaW9uUG9saWN5IjoiZXlKV1pYSnphVzl1SWpvaU1qQXhNaTB4TUMweE55SXNJbE4wWVhSbGJXVnVkQ0k2VzNzaVJXWm1aV04wSWpvaVFXeHNiM2NpTENKQlkzUnBiMjRpT2xzaWN6TTZLaUpkTENKU1pYTnZkWEpqWlNJNld5SmhjbTQ2WVhkek9uTXpPam82Y0hKdmFtVjBMV0ZtWkMxbGRtRXRZWEFpTENKaGNtNDZZWGR6T25Nek9qbzZjSEp2YW1WMExXRm1aQzFsZG1FdFlYQXZLaUpkZlN4N0lrVm1abVZqZENJNklrRnNiRzkzSWl3aVFXTjBhVzl1SWpwYkluTXpPa3hwYzNSQ2RXTnJaWFFpWFN3aVVtVnpiM1Z5WTJVaU9sc2lZWEp1T21GM2N6cHpNem82T2lvaVhTd2lRMjl1WkdsMGFXOXVJanA3SWxOMGNtbHVaMHhwYTJVaU9uc2ljek02Y0hKbFptbDRJam9pWkdsbVpuVnphVzl1THlvaWZYMTlMSHNpUldabVpXTjBJam9pUVd4c2IzY2lMQ0pCWTNScGIyNGlPbHNpY3pNNlIyVjBUMkpxWldOMElsMHNJbEpsYzI5MWNtTmxJanBiSW1GeWJqcGhkM002Y3pNNk9qb3FMMlJwWm1aMWMybHZiaThxSWwxOVhYMD0iLCJzZXNzaW9uX3N0YXRlIjoiYWYyMDdlY2UtOWEzNi00OTQyLTllODYtNjA1NTJmNGYxZTg1Iiwic2lkIjoiYWYyMDdlY2UtOWEzNi00OTQyLTllODYtNjA1NTJmNGYxZTg1Iiwic3ViIjoiOTI3N2Y3MzMtNTBmMy00MjM4LWI1YzctNjRjMmU4YTZiNDI0IiwidHlwIjoiQmVhcmVyIn0.40_wwJeo7HhTJjvPyob96nNEvbCmvohcN7Wnd02ETirLobdTmE7QZZbd9q5P1ZGANkkIkAf8who86ELp1crWfw",
#            "AWS_S3_ENDPOINT"= "minio.lab.sspcloud.fr",
#            "AWS_HTTPS" = "FALSE",
#            "AWS_VIRTUAL_HOSTING"="FALSE")
```

## Importing PAs datasets


```r
#Load spatial data and transform into polygons (requirement of the mapme package)

#If the function get_resources used later returns an error "...Loop 0 is not valid: Edge 94 crosses edge 96>...", then the following function might solve it. It works if sf was updated v1.0 and more, and brings back to old way of working. sf uses mostly a flat Earth model instead of s2 spherical geometry in the new versions. 
sf_use_s2(FALSE)

#/!\ : in pa_shp, variable sprfc is the area reported by AFD members, and is not equal to rep_a (area reported by WDPA) nor superficie in the BDD_joint. The latter is a combination of rep_a and sprfc, where superficie = rep_a except if rep_a = 0 or not reported.
pa_shp = 
  #read_sf("data_tidy/BDD_SHP_nodupl_pub.gpkg") %>%
  aws.s3::s3read_using(
  FUN = sf::read_sf,
  # Mettre les options de FUN ici
  object = "data_tidy/BDD_shp_pub.gpkg",
  bucket = "projet-afd-eva-ap",
  opts = list("region" = "")) %>%
  st_make_valid() %>%
  sf::st_cast(to = "POLYGON")

#From BDD_joint, that is the combination of the SIOP dataset and the AP dataset from ARB team in AFD. Imported to have the information on the PA area (combination of areas reported by WDPA and ARB)
pa_nodupl = 
  #fread("data_tidy/BDD_DesStat_nodupl.csv") %>%
  aws.s3::s3read_using(
  FUN = data.table::fread,
  encoding = "UTF-8",
  object = "data_tidy/BDD_DesStat_nofund_nodupl.csv",
  bucket = "projet-afd-eva-ap",
  opts = list("region" = "")) %>%
  dplyr::select(c(wdpaid, superficie))

#mapview(pa_shp)
```

## Downloading data of interests and compute relevant indicators

For the moment, mapme.biodiversity functions do not support reading/writing in a S3 server like SSPCloud. Waiting for the new release of the package, the following process is followed :

1.  Create a sub-folder in the temporary folder (RAM of the R session) to download and store the raw data

2.  Associate the portfolio to this sub-folder

3.  Download the raw data and compute the indicators of interest, unnest the data obtained

4.  Then, a dataframe can be saved.

Note that this solution is not time-optimal : raw data must be downloaded each time.

### Creating the portfolio


```r
#The raw data from mapme package are stored in a temporary folder
tmp = paste(tempdir(), "mapme", sep = "/")
#save_folder = get_bucket("projet-afd-eva-ap", region = "")

#Creating the portfolio
pa_pfolio = pa_shp %>%
  init_portfolio(2000:2020,
                 outdir = tmp,
                 #cores = 4,
                 add_resources = TRUE,
                 verbose = TRUE)
```

### TEST : importing data from SSPCloud

data are downloaded from an other script and stored in the SSPCloud. Here : copy these data to the temporary storage of the R session, and use them to compute indicators.


```r
df_files = aws.s3::get_bucket("projet-afd-eva-ap",
                   prefix = "data_raw/mapme_bio_data/gfw_treecover/",
                   region = "") %>%
  as.data.frame()
list_files = df_files$Key
i = list_files[1]
for (i in list_files) 
{
  temp_file = s3read_using(FUN = terra::rast,
               object = i,
               bucket = "projet-afd-eva-ap",
               opts = list("region"=""))
  terra::writeRaster(temp_file, file.path(tempdir(), "test.tif"), overwrite = TRUE)
}
plot(temp_file)
```

### Compute indicators from geospatial data


```r
##~~~~~~~~~~~~~~~~
##  ~ Forests ----
##~~~~~~~~~~~~~~~~

##Downloading data and computing indicators 
pa_pfolio_tcover =
  get_resources(pa_pfolio,
    resources = c("gfw_lossyear","gfw_treecover","gfw_emissions"),
    vers_treecover = "GFC-2020-v1.8",
    vers_lossyear = "GFC-2020-v1.8") %>%
  # FAO forest definition here: Minimum treecover = 10%, minimum size =1 hectare
  calc_indicators(indicators = "treecover_area",
                min_cover = 10,
                min_size = 1, overwrite=T)

#Unest the sf file into a classic data frame without geometry
data_pfolio_tcover = unnest(pa_pfolio_tcover, cols="treecover_area") %>%
  sf::st_drop_geometry() %>%
  dplyr::select(wdpaid, assetid, years,treecover) %>%
  mutate(treecover = case_when(treecover == 0 ~ NA, TRUE ~ treecover)) %>%
  filter(!is.na(treecover))

#Write the dataframe into the bucket
s3write_using(data_pfolio_tcover,
              data.table::fwrite,
              object = "data_tidy/mapme_bio_data/data_pfolio_tcover.csv",
              bucket = "projet-afd-eva-ap",
              opts = list("region" = "")
              )

#Copying files to SSPCloud
##emissions
files_emi <- list.files(paste(tmp, "gfw_emissions", sep = "/"), 
                        full.names = TRUE)
##Add each file in the bucket (same foler for every file in the temp)
for(f in files_emi) 
  {
  cat("Uploading file", paste0("'", f, "'"), "\n")
  aws.s3::put_object(file = f, 
                     bucket = "projet-afd-eva-ap/data_raw/mapme_bio_data/gfw_emissions", 
                     region = "", show_progress = TRUE)
}

##loss years
files_lossyear <- list.files(paste(tmp, "gfw_lossyear", sep = "/"), 
                        full.names = TRUE)
##Add each file in the bucket (same foler for every file in the temp)
for(f in files_emi) 
  {
  cat("Uploading file", paste0("'", f, "'"), "\n")
  aws.s3::put_object(file = f, 
                     bucket = "projet-afd-eva-ap/data_raw/mapme_bio_data/gfw_lossyear", 
                     region = "", show_progress = TRUE)
}

##Treecover loss
files_treecover <- list.files(paste(tmp, "gfw_treecover", sep = "/"), 
                        full.names = TRUE)
##Add each file in the bucket (same foler for every file in the temp)
for(f in files_emi) 
  {
  cat("Uploading file", paste0("'", f, "'"), "\n")
  aws.s3::put_object(file = f, 
                     bucket = "projet-afd-eva-ap/data_raw/mapme_bio_data/gfw_treecover", 
                     region = "", show_progress = TRUE)
}

#Removing files from temp
do.call(file.remove, list(list.files(paste(tmp, "gfw_emissions", sep = "/"), full.names = TRUE)))
do.call(file.remove, list(list.files(paste(tmp, "gfw_lossyear", sep = "/"), full.names = TRUE)))
do.call(file.remove, list(list.files(paste(tmp, "gfw_treecover", sep = "/"), full.names = TRUE)))
```


```r
##~~~~~~~~~~~~~~~~~~
##  ~ Mangroves ----
##~~~~~~~~~~~~~~~~~~

##Downloading data and computing indicators 
pa_pfolio_mang =
  get_resources(pa_pfolio,
    resources = c("gmw")) %>%
  # FAO forest definition here: Minimum treecover = 10%, minimum size =1      #hectare
  calc_indicators(indicators = "mangroves_area",
                overwrite=T)

##Unest the sf file into a classic data frame without geometry
data_pfolio_mang = unnest(pa_pfolio_mang, cols="mangroves_area") %>%
  sf::st_drop_geometry() %>%
  dplyr::select(wdpaid, assetid, year,mangrove_extent)
# s3write_using(data_pfolio_mang,
#               data.table::fwrite,
#               object = "data_tidy/mapme_bio_data/data_pfolio_mang.csv",
#               bucket = "projet-afd-eva-ap",
#               opts = list("region" = "")
#               )

# data_pfolio_mang = s3read_using(data.table::fread,
#               object = "data_tidy/mapme_bio_data/data_pfolio_mang.csv",
#               bucket = "projet-afd-eva-ap",
#               opts = list("region" = "")
#                                 )

#Removing files from temp
##emissions
files_mang <- list.files(paste(tmp, "gmw", sep = "/"), 
                        full.names = TRUE)
##Add each file in the bucket (same foler for every file in the temp)
for(f in files_mang) 
  {
  cat("Uploading file", paste0("'", f, "'"), "\n")
  aws.s3::put_object(file = f, 
                     bucket = "projet-afd-eva-ap/data_raw/mapme_bio_data/gmw", 
                     region = "", show_progress = TRUE)
}

do.call(file.remove, list(list.files(paste(tmp, "gmw", sep = "/"), full.names = TRUE)))
```


```r
##~~~~~~~~~~~~~~~~~~~~
##  ~ Land cover   --
##~~~~~~~~~~~~~~~~~~~~

##Downloading data and computing indicators 
pa_pfolio_land = 
  get_resources(pa_pfolio,
    resources = c("esalandcover")) %>%
  calc_indicators(indicators = "landcover",
                overwrite=T) 

##Unnest data
data_pfolio_land = unnest(pa_pfolio_land, cols = "landcover") %>%
  st_drop_geometry() %>%
  dplyr::select(wdpaid, assetid, year, area, classes)
s3write_using(data_pfolio_land,
              data.table::fwrite,
              object = "data_tidy/mapme_bio_data/data_pfolio_land.csv",
              bucket = "projet-afd-eva-ap",
              opts = list("region" = "")
              )

# data_pfolio_land = s3read_using(data.table::fread,
#               object = "data_tidy/mapme_bio_data/data_pfolio_land.csv",
#               bucket = "projet-afd-eva-ap",
#               opts = list("region" = "")
#                                 )
```


```r
##~~~~~~~~~~~~~~~~~~~
##  ~ Emissions  ----
##~~~~~~~~~~~~~~~~~~~

# pa_pfolio_emi = 
#     get_resources(pa_pfolio,
#     resources = c("gfw_lossyear","gfw_treecover","gfw_emissions"), 
#     vers_treecover = "GFC-2020-v1.8",
#     vers_lossyear = "GFC-2020-v1.8") %>%
#     calc_indicators(indicators = "treecoverloss_emissions",
#                   min_cover = 30,
#                   min_size = 1, overwrite=T)

# write_portfolio(pa_pfolio_emi,
#       dsn = "data_tidy/mapme_bio_data/pa_pfolio_emi.gpkg",
#       overwrite = FALSE)

pa_pfolio_emi = read_portfolio("data_tidy/mapme_bio_data/pa_pfolio_emi.gpkg")

# data_pfolio_emi = unnest(pa_pfolio_emi,
#                   cols="treecoverloss_emissions") %>%
#     sf::st_drop_geometry() %>%
#     dplyr::select(id_pr,wdpaid,years,emissions)

# fwrite(data_pfolio_emi,
#        "data_tidy/mapme_bio_data/data_pfolio_emi.csv")

data_pfolio_emi = fread("data_tidy/mapme_bio_data/data_pfolio_emi.csv")
```


```r
##~~~~~~~~~~~~~~~~~~~~
##  ~ Biome       ----
##~~~~~~~~~~~~~~~~~~~~

##Downloading data and computing indicators 
pa_pfolio_biome =
  get_resources(pa_pfolio,
    resources = c("teow")) %>%
  calc_indicators(indicators = "biome",
                overwrite=T)

# write_portfolio(pa_pfolio_biome, 
#       dsn = "01_data_tidy/mapme_bio_data/pa_pfolio_biome.gpkg",
#       overwrite = FALSE)

##Unest the sf file into a classic data frame without geometry
# data_biome = unnest(pa_pfolio_biome, cols="biome") %>%
#   sf::st_drop_geometry() %>%
#   dplyr::select(id_pr,wdpaid,year,mangrove_extent)
```


```r
##~~~~~~~~~~~~~~~~~~~~
##  ~ Soil features --
##~~~~~~~~~~~~~~~~~~~~

#/!\ voir quels indicateurs/mesures prendre !!

##Downloading data and computing indicators 

pa_pfolio_soil = 
  get_resources(pa_pfolio,
    resources = c("soilgrids")) %>%
  # FAO forest definition here: Minimum treecover = 10%, minimum size =1      #hectare
  calc_indicators(indicators = "soilproperties",
                overwrite=T) 

# write_portfolio(pa_pfolio_mang, 
#       dsn = "01_data_tidy/mapme_bio_data/pa_pfolio_mang.gpkg",
#       overwrite = FALSE)

##Unest the sf file into a classic data frame without geometry
# data_biome = unnest(pa_pfolio_biome, cols="biome") %>%
#   sf::st_drop_geometry() %>%
#   dplyr::select(id_pr,wdpaid,year,mangrove_extent)
```


```r
##~~~~~~~~~~~~~~~~~~~~
##  ~ Accessibility --
##~~~~~~~~~~~~~~~~~~~~

##Downloading data and computing indicators 
pa_pfolio_acc = 
  get_resources("nelson_et_al",
                range_traveltime = c("5k_10k", "100k_200k", 
                                     "500k_1mio", "1mio_5mio")) %>%
  calc_indicators("traveltime", 
                  stats_accessibility = c("min", "max"), 
                  engine = "extract")

# write_portfolio(pa_pfolio_mang, 
#       dsn = "01_data_tidy/mapme_bio_data/pa_pfolio_mang.gpkg",
#       overwrite = FALSE)

##Unest the sf file into a classic data frame without geometry
# data_biome = unnest(pa_pfolio_biome, cols="biome") %>%
#   sf::st_drop_geometry() %>%
#   dplyr::select(id_pr,wdpaid,year,mangrove_extent)
```

Faire une fonction pour l'extraction de tout jeu de données ? Répétitif en soit de faire forêts, mangroves, etc.

Warning and errors face and their meaning.

-   *although coordinates are longitude/latitude, st_intersects assumes that they are planar*.

    -   See <https://r-spatial.org/r/2020/06/17/s2.html> and <https://r-spatial.github.io/sf/articles/sf7.html#>. Basically I used sf_use_s2(FALSE) at the creation of the portfolio. Thus sf uses flat Earth model instead of s2 spherical geometry. The operation st_intersects assumes the data lie in a flat plane where one degree longitude equals one degree latitude, irrespective where the PA is on the world (equirectangular projection). Thus if the polygons are drawn from coordinates in a spherical geometry, the operation is performed on the wrong projection. Is it a problem in itself ? Well as a projection is a bijection between two coordinate systems, plygons that intersect (or not) in a projection will intersect in the second. However the polygons will be distorded (the more so as they are located closer to the poles), and the areas computed could be wrong. Anyway according to the documentation the WDPA polygons are provided in WGS84 geographic coordinate system, i.e a planar projection (Plate Carree) . No problem should arise from the warning then.

    -   So according to Florent Bedecarrats, this warning is related to an other issue. The sphere representing the Earth is transformed into a plane through a cut on a given line. If a polygon is located on this line, the flattened polygon is considered to go around the Earth. To test for this possibility, compare the area of the polygon to the true area of the PA.

-   *Avis : TIFFFillStrip:Read error at scanline 4294967295; got 0 bytes, expected 1387 (GDAL error 1)Avis : TIFFReadEncodedStrip() failed. (GDAL error 1).*

    This advice occurs during the computation of indicators (a few tenths for emissions). Fail to read some parts of the raster file ? Is it problematic for the whole process ?

    Best solution : remove the corresponding TIFF and download data again.

-   *Avis : D:/projet_AiresProtegees/00_data_raw/mapme_bio_data/gfw_lossyear/Hansen_GFC-2020-v1.8_lossyear_10N_010E.tif, band 1: IReadBlock failed at X offset 0, Y offset 29995: TIFFReadEncodedStrip() failed. (GDAL error 1)*

    Same as before. Best solution : remove the corresponding TIFF and download data again.

-   *Avis : Error : [crop] incorrect number of values (too many) for writing*

    Check the error in more details.
    
## Computing statistics

From the datasets obtained, statistics of interest can be computed and plotted.

### Forests

Variation of forest loss over time :

-   Building the figure dataset


```r
#Building the dataset
data_stat_treeloss = data_pfolio_forest %>%
  group_by(nm_ap) %>% 
  #Comoute variation over time in each PA
  mutate(lag_treecov = lag(treecover),
         loss = ((treecover - lag_treecov)/lag_treecov)*100) %>%
  ungroup() %>%
  #Regroup years for analysis in five years intervals
  mutate(years_regroup = case_when(
  years <= 2005 ~ "2000-2005",
  years <= 2010 ~ "2005-2010",
  years <= 2015 ~ "2010-2015",
  years <=2020 ~ "2015-2020",
  TRUE ~ NA),
  .after = years) %>%
  #Remove NA values for loss
  filter(!is.na(loss)) %>%
  #Compute mean loss for five years intervals in each PA
  group_by(nm_ap, years_regroup) %>%
  mutate(moy_5 = mean(loss)) %>%
  ungroup()
```

-   Statistics at PA level


```r
#Evolution for a given PA
##Buba
stat_forest_buba = stat_treeloss_id(df = data_stat_treeloss,
                                    id = "317051",
                                    name_pa = "AP Buba",
                                    treatment_yr = 2007)
fig_evo_buba = stat_forest_buba[[1]]
ggsave(plot = fig_evo_buba,
       filename = "fig_forest_loss_evo_ap_buba.png",
       path = "05_StatDes/biodiversity/forest/loss",
       width = 7, height = 5)
fig_evo5_buba = stat_forest_buba[[2]]
ggsave(plot = fig_evo5_buba,
       filename = "fig_forest_loss_evo5_ap_buba.png",
       path = "05_StatDes/biodiversity/forest/loss",
       width = 7, height = 5)

#Niumi
stat_forest_niumi = stat_treeloss_id(df = data_stat_treeloss,
                                    id = "109037",
                                    name_pa = "AP Niumi",
                                    treatment_yr = 2008)
fig_evo_niumi = stat_forest_niumi[[1]]
ggsave(plot = fig_evo_niumi,
       filename = "fig_forest_loss_evo_ap_niumi.png",
       path = "05_StatDes/biodiversity/forest/loss",
       width = 7, height = 5)
fig_evo5_niumi = stat_forest_niumi[[2]]
ggsave(plot = fig_evo5_niumi,
       filename = "fig_forest_loss_evo5_ap_niumi.png",
       path = "05_StatDes/biodiversity/forest/loss",
       width = 7, height = 5)

#Bamboung
stat_forest_bamboung = stat_treeloss_id(df = data_stat_treeloss,
                                    id = "555651496",
                                    name_pa = "AP Bamboung",
                                    treatment_yr = 2008)
fig_evo_bamboung = stat_forest_bamboung[[1]]
ggsave(plot = fig_evo_bamboung,
       filename = "fig_forest_loss_evo_ap_bamboung.png",
       path = "05_StatDes/biodiversity/forest/loss",
       width = 7, height = 5)
fig_evo5_bamboung = stat_forest_bamboung[[2]]
ggsave(plot = fig_evo5_bamboung,
       filename = "fig_forest_loss_evo5_ap_bamboung.png",
       path = "05_StatDes/biodiversity/forest/loss",
       width = 7, height = 5)
```

/!\ Modifier le code précédent pour inclure plusieurs aires ???

-   Statistics at region level


```r
#Evolution at region level : dataset
data_stat_treeloss_reg = data_stat_treeloss %>%
  group_by(drct, years) %>%
  #For each region and each period, compute average evolution
  mutate(evo_avg_reg = mean(treecover)) %>%
  ungroup() %>%
  group_by(drct) %>% 
  #Variation of the average evolution over time in each region
  mutate(lag_region = lag(evo_avg_reg),
         loss_region = ((evo_avg_reg - lag_region)/lag_region)*100) %>%
  #remove 2001 : either NA, or take value above when two regions are consecutive in the data frame (lag by region not applied) ???
  mutate(loss_region = case_when(
    !is.na(loss_region) & years == 2001 ~ NA,
    TRUE ~loss_region))

#Sahel
fig_evo_sahel = stat_treeloss_reg(df = data_stat_treeloss_reg,
                                  reg = "Dr Grand Sahel",
                                  name_reg = "Sahel")
ggsave(plot = fig_evo_sahel,
       filename = "fig_forest_loss_evo_reg_sahel.png",
       path = "05_StatDes/biodiversity/forest/loss",
       width = 7, height = 5)

#Guinea gulf
fig_evo_guinee = stat_treeloss_reg(df = data_stat_treeloss_reg,
                                  reg = "Dr Golfe De Guinee",
                                  name_reg = "Golfe de Guinée")

ggsave(plot = fig_evo_guinee,
       filename = "fig_forest_loss_evo_reg_guinee.png",
       path = "05_StatDes/biodiversity/forest/loss",
       width = 7, height = 5)

#Eastern Africa
fig_evo_eastAf = stat_treeloss_reg(df = data_stat_treeloss_reg,
                                  reg = "Dr Afrique De L'Est",
                                  name_reg = "Afrique de l'Est")
ggsave(plot = fig_evo_eastAf,
       filename = "fig_forest_loss_evo_reg_eastAf.png",
       path = "05_StatDes/biodiversity/forest/loss",
       width = 7, height = 5)

#Eastern Africa
fig_evo_austAf = stat_treeloss_reg(df = data_stat_treeloss_reg,
                                  reg = "Dr Afrique Australe",
                                  name_reg = "Afrique australe")

ggsave(plot = fig_evo_austAf,
       filename = "fig_forest_loss_evo_reg_austAf.png",
       path = "05_StatDes/biodiversity/forest/loss",
       width = 7, height = 5)
```

### Mangrove

Summary table of mangrove area in each PA :


```r
tbl_mang_summary = data_mang %>%
  dplyr::group_by(wdpaid) %>%
  summarize(area_sqkm = sum(value))
```

### Emissions

/!\ pas clair

Summary table of emissions in each PA :


```r
# create summary table 
tbl_emi_summary = data_emi %>%
  group_by(name) %>%
  summarize(area_sqkm = sum(value))
```

### Biome/TEOW
