---
title: "Exercise_1"
output: html_document
date: "2024-07-23"
editor_options: 
  markdown: 
    wrap: 72
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

The following exercises will help you to master the steps of the
analysis carried out in the chapter "Geospatial Impact Evaluation of
protected areas support on forest cover loss" and to understand the
methodology used. The exercise codnucts an evaluation for a single PA to
understand the code easier, but the scripts to analyze an entire
portfolio can be found here 'github link'.

The objective of this first exercise is to build a dataset with
outcomes, covariates and treatment variables for each unit of
observation (cells) that will be used in the matching process . This
notebook will also enable you to create your own shiny app to visualize
the data interactively. There are four parts to this notebook:

1.  Settings
2.  Classify observations
3.  Download the data
4.  Create an interactive map

## Exercise 1 : Build a matching sample

```{r}
# install packages if necessary
lop <- c("ggplot2", "tidyr", "dplyr", "stringr", "sf", "terra", "raster", "geodata", "exactextractr", "mapme.biodiversity", "future","progressr","wdpar","landscapemetrics","gridExtra","rstac","webdriver")
newp <- lop[!(lop %in% installed.packages()[,"Package"])]
if(length(newp)) install.packages(newp)
lapply(lop, require, character.only = TRUE)
```

```{r }
# Load Libraries
library(dplyr) # A grammar of data manipulation
library(tidyr) # Tools for creating tidy data
library(stringr) # String manipulation functions
library(ggplot2) # For plotting
library(sf) # For handling vector data
library(terra) # For handling raster data
library(raster) # For handling raster data
library(geodata) # For getting country files
remotes::install_github("prioritizr/wdpar@extra-wait") # to download protected areas shape
webdriver::install_phantomjs() # to fetch wdpa data 
library(exactextractr) # For zonal statistics
remotes::install_github("mapme-initiative/mapme.biodiversity", upgrade="always")
library(mapme.biodiversity) # Biodiversity data processing and analysis
library(future) # For parallel and distributed processing
library(progressr) # Progress bars for long-running operations
library(gridExtra) # Tools for arranging multiple grid-based visual objects
library(rstac) # Interface for querying and accessing SpatioTemporal Asset Catalog (STAC) compliant API service
```

## 1. Settings ⚙️

1.  Please enter a working directory and the name you wish to give to
    the output of this notebook, which is a dataset ready for matching :

```{r}
# Define the path to a working directory
wdir = 
# Define the file name of the output matching dataset
name_output = ""
```

2.  The next step is to decide the parameters of your analysis, in the
    next cell please enter:

\- the country code of El Salvador

\- the size of the buffer

\- the size of the grid

\- the WDPA ID of the Parque Nacional Montecristo

To lower the computation time we choose to analyze the Parque Nacional
Montecristo but you can try with another PA. It is also possible to vary
the following parameters according to the PA you wish to analyze.
Initially you can use the same parameters as in the case study.
Afterward you could run the notebook again with different parameters to
see the differences in the results.

```{r}
# Specify a country iso 3; to find the right country code, please refer to this page https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3
country = ""
# Specify buffer width in meter
buffer_m = 
# Specify the grid cell size in meter
gridSize = 
# Specify a WDPA IDs; to find the WDPA ID, please refer to this page 
# https://www.protectedplanet.net/en
paid = c()
```

## 2. Generating observation units for matching frame

The second step is to prepare a grid of observation units for the
spatial analysis, tailored specifically to the chosen country. After
downloading the country polygon, it is necessary to reproject the
country polygon to the appropriate UTM (Universal Transverse Mercator)
zone based on the country's centroid (find more information about this
projection
[here](https://gisgeography.com/utm-universal-transverse-mercator-projection/)).
The UTM projection minimizes distortion for specific regions, making
spatial calculations like distances and areas more accurate.

A bounding box is created, and a grid is generated within this box
covering the whole country. It is then intersected with the country's
boundary to retain only cells located within the country as observation
units.

1.  Add the right objects in the `ggplot()` to obtain a map with the
    box, the grid, and the shape of the country

```{r}
# Download country polygon to working directory and load it into workspace 
gadm <- gadm(country = country, resolution = 1, level = 0, path = wdir) %>%
  st_as_sf()

# Find UTM zone of the country centroid
centroid = st_coordinates(st_centroid(gadm))
lonlat2UTM = function(lonlat) {
  utm = (floor((lonlat[1] + 180) / 6) %% 60) + 1
  if (lonlat[2] > 0) {
    utm + 32600
  } else{
    utm + 32700
  }
}
utm_code = lonlat2UTM(centroid)
# Reproject GADM
gadm_prj = gadm %>% st_transform(crs = utm_code)
# Make bounding box around the reprojected country polygon because it is required in the next function to build the grid 

bbox = st_bbox(gadm_prj) %>% st_as_sfc() %>% st_as_sf()
# Make a Grid to the extent of the bounding box
grid.ini = st_make_grid(bbox, cellsize = c(gridSize,gridSize))
# Crop Grid to the extent of country boundary by
# subsetting to the grid cells that intersect with the country
grid.sub = grid.ini %>%
  st_intersects(gadm_prj, .) %>%
  unlist()
# Filter the grid to the subset
grid = grid.ini[sort(grid.sub)] %>%
  st_as_sf() %>%
  mutate(gridID = seq(1:nrow(.))) # Add id for grid cells
# Visualize
ggplot() +
 geom_sf(data = st_geometry()) +
 geom_sf(data = st_geometry()) +
 geom_sf(data = st_geometry(), alpha = 0)

```

To answer following questions you can refer to XXX chapter

2.  Obtain information about `gadm` and `gadm_prj` object. To do so you
    can just enter the name of the object in the console.

    a\. What is the main coordinate system used for each geospatial
    object?]

    [ANSWER HERE]

    b\. What are the units of measurement used in each projection?

    [ANSWER HERE]

    c\. Why do we use a projected system (UTM) in some geospatial
    applications instead of a geodetic system (WGS 84)?

    [ANSWER HERE]

## 3. Classifying observation units

The next step is to classify observation units i.e. cells of the grid
into one of these four groups:

-   Group 1 : PA analyzed

-   Group 2 : Other PA

-   Group 3 : Buffer zone

-   Group 0 : Others (meaning none of the above groups)

To this end this section first downloads protected area (PA) polygons
from WDPA and reprojects them to the same projection used previously.
After some filtering and cleaning of PA polygons, a buffer is created
around each one of them. Then, each polygon is assigned to a group . In
a second step, this dataset of PA polygons and buffers is rasterized,
and zonal statistics are used to define the treatment group of each
cell/observation unit.

1.Find the right *sf* function to create buffer around all PA using the
buffer size (`buffer_m`) chosen before and complete the code

```{r}
# Get the PA polygons/points of the specified country;
# They're downloaded to the working directory.
wdpa = wdpa_fetch(country, wait = TRUE, download_dir = wdir)

# PAs are projected, and column "geometry_type" is added
wdpa_prj = wdpa_clean(wdpa, geometry_precision = 1000,
                      # Don't erase overlapped polygons
                      erase_overlaps = FALSE) %>%
  # Remove the PAs that are only proposed, or have geometry type "point"
  filter(STATUS != "Proposed") %>%
  filter(GEOMETRY_TYPE != "POINT") %>%
  # Project PA polygons to the previously determined CRS
  st_transform(crs = utm_code)

# Separate the analyze PA from others PAs

wdpa_focus = wdpa_prj %>% filter(WDPAID == paid) %>%
  mutate(group=1) # Assign an ID "1" to the analyzed PA

wdpa_nofocus = wdpa_prj %>%
  filter(WDPAID!=paid) %>%
  st_buffer(., dist=0) # a hack to solve geometries issues with polygons in R, e.g. problem of self-intersection
#add a link 

# Determine the other PAs that intersect with the analyzed PAs,
# and delete the intersection part from others PAs to reduce noise when rasterizing WDPAIDs later.

intersection = st_intersection(wdpa_nofocus, wdpa_focus)
wdpa_nofocus <- wdpa_nofocus %>%
  { if (nrow(intersection) == 0) . else st_difference(., st_union(st_geometry(intersection))) } %>%
  mutate(group = 2)

# Make Buffers of 10km around all protected areas
# Complete this section to make buffers around wdpa_focus and wdpa_nofocus PAs and then bind them together 
wdpa_buffer <- st_buffer(wdpa_focus, dist = buffer_m) %>%
  rbind(st_buffer(wdpa_nofocus, dist = buffer_m)) %>%
  # Assign an ID "3" to the buffer group
  mutate(group=3)

# Merge the dataframes of funded PAs, non-funded PAs and buffers
wdpa_groups = rbind(wdpa_focus, wdpa_nofocus, wdpa_buffer)
# Subset to polygons that intersect with country boundary
wdpa.sub = wdpa_groups %>%
  st_intersects(gadm_prj, .) %>%
  unlist()
# Filter the PA+buffer to the subset
wdpa_groups = wdpa_groups[sort(wdpa.sub),] %>%
  st_as_sf()



# This map allow to visualize each PA in the country and its buffer zone around
colors <- c("1" = "darkgreen", "2" = "darkblue", "3" = "orange")

ggplot() +
   geom_sf(data = st_geometry(gadm_prj), fill = "lightgray", color = "lightgray", lwd = 0.2) + 
    geom_sf(data = subset(wdpa_groups, group == "3"), aes(fill = factor(group)), color = "black", lwd = 0.3, alpha = 0.6) +
    geom_sf(data = subset(wdpa_groups, group == "2"), aes(fill = factor(group)), color = "black", lwd = 0.3, alpha = 0.6) +
    geom_sf(data = subset(wdpa_groups, group == "1"), aes(fill = factor(group)), color = "black", lwd = 0.3, alpha = 0.6) +
    scale_fill_manual(values = colors, 
                      labels = c("1" = "analyzed PA", 
                               "2" = "Non Funded PA", 
                               "3" = "Buffer Zone")) +
    theme_bw() +
    theme(legend.position = "right")


```

The next step is to assign a group (analyzed PA, other PA, buffer, none)
to each cell of the grid. It may be the case that a cell is covered by
multiple groups, so it is necessary to decide how the group of the cell
will be attributed. To achieve this, we must first rasterize the data,
which involves transforming it from a vector representation to a raster
grid. Using raster data is a more time-efficient than vector, which is
especially beneficial in a portfolio analysis where a significant amount
of data must be processed.

raster

:   A **raster** is a type of spatial data representation that consists
    of a grid of cells, where each cell has a specific value
    representing information, such as color in an image or a particular
    data value in a geographic context

A raster layers is created over the country's extent, with cell size
equal to our grid cell size. The polygon layer (containing PAs and
buffer zones) is rasterized, and each cell in the newly created raster
is assigned to a group with cell values representing group IDs.If
multiple groups are covering a cell, one need to decide how to choose
the group of assignment. In this code the smallest group value is
chosen. For instance, if a cell is located both in a buffer polygon and
in the analyzed PA, the cell value will be set to group 1 i.e. the
analyzed PA zone, ensuring that PA group IDs take precedence over buffer
IDs. Another raster layer is created to assign a WDPA ID to each unit of
observation.

Finally, the raster values are aggregated into the grid cells with the
`mode` function. The mode function assign the group with the highest
frequency in the grid cell. Then values get merged into a single
dataset, and transformed to the WGS84 coordinate system to comply with
the mapme.biodiversity package requirements.

```{r}
# Initialize an empty raster to the spatial extent of the country
r.ini = raster()
extent(r.ini) = extent(gadm_prj)
# Specify the raster resolution as same as the pre-defined 'gridSize'
res(r.ini) = gridSize
# Assign the raster pixels with "Group" values,
# Take the minimal value if a pixel is covered by overlapping polygons, so that PA Group ID has higher priority than Buffer ID.
# Assign value "0" to the background pixels (control candidates group)
r.group = rasterize(wdpa_groups, r.ini, field="group", fun="min", background=0) %>%
  mask(., gadm_prj)
# Rename Layer
names(r.group) = "group"
# Rasterize wdpaid
r.wdpaid = rasterize(wdpa_prj, r.ini, field="WDPAID", fun="first", background=0) %>%
  mask(., gadm_prj)
names(r.wdpaid) = "wdpaid"

# Aggregate pixel values by taking the most frequent value 
grid.group = exact_extract(x=r.group, y=grid, fun='mode', append_cols="gridID") %>%
  rename(group = mode)
grid.wdpaid = exact_extract(x=r.wdpaid, y=grid, fun="mode", append_cols="gridID") %>%
  rename(wdpaid = mode)

# (LD :Why using exact extract if the pixels are the same size as the cells ? )
# Merge data frames
grid.param = grid.group %>%
  merge(., grid.wdpaid, by="gridID") %>%
  merge(., grid, by="gridID") %>%
  # drop rows having "NA" in column "group"
  drop_na(group) %>%
  # drop the column of "gridID"
  subset(., select=-c(gridID)) %>%
  st_as_sf() %>%
  # Grid is projected to WGS84 because mapme.biodiverty package merely works with this CRS
  st_transform(crs=4326)
```

You can now visualize the gridding of the country and the attribution of
a group to each cell.

```{r}
# Visualize the group of grid cells
grid.param %>%
  ggplot() +
  geom_sf(aes(fill = factor(group)), lwd=0) + # add the right dataframe in factor()
  scale_fill_manual(name="Group", # legend title
                    values = c("grey", "darkgreen", "darkblue", "orange"),#add colors
                    labels = c("control candidate", "treatment candidate", "non-funded PA", "buffer zone")) +
  theme_bw()

```

The function used to aggregate pixel values into the grid cells in the
exact_extract() function influences the final group attribution obtained
for each cells

Instead of assigning to a grid cell the most frequent value among the
pixels it overlaps we may want to retain the smallest pixel value, so
that if the grid cell overlaps with any pixels that intersects a PA, the
grid cell is assigned to the PA group.

1.   Try to change the function used in `exact_extract()` to retain the
    smallest pixel value.

2.  Compute group statistics of grid.param and `grid.param_2`, comment
    the difference

3.  Map `grid.param` and `grid.param_2` to visualize the difference.

```{r}
# Aggregate pixel values by taking the min
grid.group_2 = exact_extract(x=r.group, y=grid, fun=, append_cols="gridID") %>%
  rename(group = min)
grid.wdpaid_2 = exact_extract(x=r.wdpaid, y=grid, fun=, append_cols="gridID") %>%
  rename(wdpaid = min)
# Merge data frames
grid.param_2 = grid.group_2 %>%
  merge(., grid.wdpaid_2, by="gridID") %>%
  merge(., grid, by="gridID") %>%
  # drop rows having "NA" in column "group"
  drop_na(group) %>%
  # drop the column of "gridID"
  subset(., select=-c(gridID)) %>%
  st_as_sf() %>%
  # Grid is projected to WGS84 because mapme.biodiverty package merely works with this CRS
  st_transform(crs=4326)

```

```{r}
# Compute summary statistic (distribution) for group variable in grid.param and grid.param_2

```

```{r}
# Visualize maps with "mode" and "min" function
# create the graph of grid.param
map_1

# create the graphe of grid.param_2


grid.arrange(map_1, map_2, nrow = 2)
```

## 4. Download the data

Once each cell has been assigned to a group, all the covariates need to
be downloaded. To do so you can use the Mapme.biodiversity package that
enables you to download multiple resources and then compute the right
indicators. Please refer to this page
(<https://github.com/mapme-initiative/mapme.biodiversity>) to find the
function to complete the following code.

You will be ask to :

\- Download the soil data

\- Calculate elevation

\- Download travel time data and compute the indicators

This section use multisession to speed up the computing of covariates.

Multisession

:   **Multisession** refers to a type of parallel processing where
    multiple R sessions (or processes) are created to execute code
    concurrently. Each session operates independently and runs in its
    own R process, allowing for parallel execution of tasks.

    To learn more about multisession see this exercise/chapter (ADD A
    LINK)

```{r}
## 4. Calculating deforestation area and other covariates for all observation units
# Get input data ready for indicator calculation
      ## Version of Global Forest Cover data to consider
      list_version_gfc = mapme.biodiversity:::.available_gfw_versions() #all versions available
      version_gfc = list_version_gfc[length(list_version_gfc)] #last version considered
#years=2000:2021
mapme_options(outdir =wdir)
# aoi = init_portfolio(grid.param,
#                      years = 2000:2021,
#                      outdir = wdir,
#                      tmpdir = file.path(wdir, "tmp"),
#                      add_resources = FALSE)

years = 2000:2021
aoi=grid.param
```

### Covariate: Soil

1.  Enter the function to download soil data. Use the following
    parameters : "clay' for the layers , a depth between 0 and 5 cm, and
    the statistic "mean".

```{r}
start_time <- Sys.time()


get.soil = aoi %>% get_resources(get_soilgrids(
        layers = "clay", # resource specific argument
        depths = "0-5cm", # resource specific argument
        stats = "mean"))
                         
# set up parallel plan with 6 workers
plan(multisession, workers = 6)
# Calculate Indicator
with_progress({
  zonal.soil = get.soil %>% calc_indicators(
          calc_soilproperties(
            stats = "mean",
            engine = "zonal"
          )
        )
})
plan(sequential) # close child processes
# Transform the output dataframe into a pivot dataframe
pivot.soil = zonal.soil %>%
  unnest(soilproperties) %>%
  mutate(across(value, round, 3)) %>% # Round numeric columns
  pivot_wider(names_from = c("variable"), values_from = "value")%>%
  dplyr::select(-c(datetime,unit))
      


end_time <- Sys.time()
elapsed_time <- end_time - start_time

cat("Elapsed time:", elapsed_time, "\n")

```

2.  Try running the same code chunk without the multisession and compare
    execution times

```{r}
start_time <- Sys.time()
get.soil = aoi %>% get_resources(get_soilgrids(
        layers = "clay", # resource specific argument
        depths = "0-5cm", # resource specific argument
        stats = "mean"))
                         

# Calculate Indicator
with_progress({
  zonal.soil = get.soil %>% calc_indicators(
          calc_soilproperties(
            stats = "mean",
            engine = "zonal"
          )
        )
})

# Transform the output dataframe into a pivot dataframe
pivot.soil = zonal.soil %>%
  unnest(soilproperties) %>%
  mutate(across(value, round, 3)) %>% # Round numeric columns
  pivot_wider(names_from = c("variable"), values_from = "value")%>%
  dplyr::select(-c(datetime,unit))
      


end_time <- Sys.time()
elapsed_time <- end_time - start_time

cat("execution time:", elapsed_time, "\n")
```

### Covariate: Elevation

Insert the function to calculate elevation mean, you should use the mean
statistic and the exactextract engine :

```{r}
get.elevation = aoi %>% get_resources(get_nasa_srtm())
# set up parallel plan with 6 workers
plan(multisession, workers = 6)

# Calculate Indicator
with_progress({
  zonal.elevation = get.elevation %>% calc_indicators(calc_elevation(
          stats= "mean",
          engine = "exactextract"))
  
  
})
plan(sequential) # close child processes
# Transform the output dataframe into a pivot dataframe
pivot.elevation = zonal.elevation %>% unnest(elevation)%>%
  pivot_wider(names_from = c("variable"), values_from = "value")%>%
  dplyr::select(-c(datetime,unit))
      


```

### Covariate: TRI

```{r}
# set up parallel plan with 6 workers
plan(multisession, workers = 6)
# Calculate Indicator
with_progress({
  zonal.tri = get.elevation %>% calc_indicators(calc_tri(
          stats = "mean",
          engine = "exactextract"))
})
plan(sequential) # close child processes
# Transform the output dataframe into a pivot dataframe
pivot.tri = zonal.tri %>% unnest(tri)%>%
  pivot_wider(names_from = c("variable"), values_from = "value")%>%
  dplyr::select(-c(datetime,unit))


```

### Covariate: Travel Time

Download data related to travel time and calculate the median travel
time

```{r}
#Run this, to download correctly travel data 

Sys.setenv(
  "VSI_CACHE" = "TRUE",
  "CPL_VSIL_CURL_CHUNK_SIZE" = "10485760",
  "GDAL_HTTP_MAX_RETRY" = "5",
  "GDAL_HTTP_RETRY_DELAY" = "15"
)




get.travelT = aoi%>% get_resources(get_nelson_et_al(ranges = c("5k_110mio")))

# set up parallel plan with 6 concurrent threads
plan(multisession, workers = 20)
# Calculate Indicator
with_progress({
  zonal.travelT  <-get.travelT %>% calc_indicators(calc_traveltime(
          stats = "median",
          engine = "exactextract"))
  })
plan(sequential) # close child processes
# Transform the output dataframe into a pivot dataframe
pivot.travelT = zonal.travelT %>%
  unnest(traveltime) %>%
  pivot_wider(names_from = "variable", values_from = "value")%>%
  dplyr::select(-c(datetime,unit))

```

### Time Series of Tree Cover Area

```{r}
get.tree = aoi %>%get_resources(get_gfw_treecover(version =  version_gfc),
                                     get_gfw_lossyear(version = version_gfc))
# set up parallel plan with 6 workers
plan(multisession, workers = 6)
# Calculate time series
with_progress({
  zonal.tree = get.tree %>% calc_indicators(calc_treecover_area(years=years, min_size=0.5, 
                                                                    min_cover=10))
  
})


plan(sequential) # close child processes

# Transform the output dataframe into a pivot dataframe
pivot.tree = zonal.tree %>%
  unnest(treecover_area) %>%
  # Transfer treecover unit to percentage
  mutate(value = round((value*1e4)/(gridSize^2)*100, 2),datetime=format(datetime, "%Y")) %>%
  pivot_wider(names_from = "datetime", values_from = "value", names_prefix = "treecover_")%>%
  dplyr::select(-c(unit,variable))

```

```{r}
# The calculation of tree loss area is performed

# Get the column names of tree cover time series
colnames_tree = names(pivot.tree)[startsWith(names(pivot.tree), "treecover")]
# Drop the first year
dropFirst = tail(colnames_tree, -1)
# Drop the last year
dropLast = head(colnames_tree, -1)
# Set list of new column names for tree loss time series
colnames_loss = dropFirst %>% str_split(., "_")
# Add new columns: treeloss_tn = treecover_tn - treecover_t(n-1)
for (i in 1:length(dropFirst)) {
  new_colname <- paste0("treeloss_", colnames_loss[[i]][2])
  pivot.tree[[new_colname]] <- pivot.tree[[dropFirst[i]]] - pivot.tree[[dropLast[i]]]
}

# Export Matching Frame
# Remove "geometry" column from pivot dataframes
df.tree = pivot.tree %>% mutate(x = NULL) %>% as.data.frame()
df.travelT = pivot.travelT %>% mutate(x = NULL) %>% as.data.frame()
df.soil = pivot.soil %>% mutate(x = NULL) %>% as.data.frame()
df.elevation = pivot.elevation %>% mutate(x = NULL) %>% as.data.frame()
df.tri = pivot.tri %>% mutate(x=NULL) %>% as.data.frame()
# Make a dataframe containing only "assetid" and geometry
df.geom = pivot.tree[, c("assetid", "x")] %>% as.data.frame()
# Merge all output dataframes
pivot.all = Reduce(dplyr::full_join, list(df.travelT, df.soil, df.tree, df.elevation, df.tri, df.geom)) %>%
  st_as_sf()
# Make column Group ID and WDPA ID have data type "integer"
pivot.all$group = as.integer(pivot.all$group)
pivot.all$wdpaid = as.integer(pivot.all$wdpaid)

# Export the matching frame
st_write(pivot.all, dsn = file.path(wdir, name_output), delete_dsn = TRUE)

```

# 5. Create an interactive map (coming soon...)

Shiny apps are interactive web applications built using R. It is
particularly useful for analyzing geospatial data because it provides an
dynamic platform for visualizing them. Run the following code to see a
beta version of a shiny app. To improve this application try adding...

```{r}

```
