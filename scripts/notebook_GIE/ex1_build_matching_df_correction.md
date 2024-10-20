---
title: "Exercise_1_corr"
output: html_document
date: "2024-07-23"
editor_options: 
  markdown: 
    wrap: 72
---

***THIS IS THE VERSION WITH ANSWERS***

# **Geospatial impact evaluation of protected areas support on forest cover loss:Exercises**

The following two exercises are designed to help you master the
analytical steps outlined in the chapter "Geospatial Impact Evaluation
of Protected Areas on Forest Cover Loss" and gain a deeper understanding
of the methodology used. These exercises focus on evaluating a single
protected area (Parque Nacional Montecristo in El Salvador) to simplify
the code and facilitate learning. However, scripts for analyzing an
entire portfolio of protected areas can be found
[here](https://github.com/idallmann/EVA-impact-aires-protegees).

## **Exercise 1 : Build a matching sample**

Objectives :

-   Build a dataset with outcomes, covariates and treatment variables
    for each unit of observation (cells) necessary for the matching
    process

-   Create a interactive map to visualize the data on PAs.

There are four parts in this notebook:

1. [Settings](#-1.-Settings-⚙️).
2. [Classify observations](#-2.-Classify-observations)
3.[Download the data](#-3.-Download-the-data)
4.[Create an interactive map](#-4.-Create-an-interactive-map-🗺️)

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
# Install packages if necessary
lop = c(
  "dplyr",
  "exactextractr",
  "future",
  "geodata",
  "ggplot2",
  "gridExtra",
  "landscapemetrics",
  "mapme.biodiversity",
  "progressr",
  "raster",
  "rstac",
  "sf",
  "stringr",
  "terra",
  "tidyr",
  "wdpar",
  "webdriver"
)
newp = lop[!(lop %in% installed.packages()[, "Package"])]
if (length(newp))
  install.packages(newp)
lapply(lop, require, character.only = TRUE)
```

```{r }
# Load Libraries
library(dplyr) # A grammar of data manipulation
library(exactextractr) # For zonal statistics
library(future) # For parallel and distributed processing
library(geodata) # For getting country files
library(ggplot2) # For plotting
library(gridExtra) # Tools for arranging multiple grid-based visual objects
library(mapme.biodiversity) # Biodiversity data processing and analysis
library(progressr) # Progress bars for long-running operations
library(raster) # For handling raster data
library(rstac) # Interface for querying and accessing SpatioTemporal Asset Catalog (STAC) compliant API service
library(sf) # For handling vector data
library(stringr) # String manipulation functions
library(terra) # For handling raster data
library(tidyr) # Tools for creating tidy data

# Additional Installations
remotes::install_github("mapme-initiative/mapme.biodiversity", upgrade="always")
remotes::install_github("prioritizr/wdpar@extra-wait") # to download protected areas shape
webdriver::install_phantomjs() # to fetch wdpa data 

```

```{r }
# Set environment variables to optimize the download and handling of travel data.
Sys.setenv(
  "VSI_CACHE" = "TRUE",
  "CPL_VSIL_CURL_CHUNK_SIZE" = "10485760",
  "GDAL_HTTP_MAX_RETRY" = "5",
  "GDAL_HTTP_RETRY_DELAY" = "15"
)


```

### 1. Settings ⚙️

To reduce computation time of this exercise, we have specifically chosen
to analyze a PA in a small country. However once you complete this
exercise, you can apply the methodology to a different PA.

Additionally, you can re-run the same analysis multiple time with
different parameters to observe how different parameters affect the
results.

> 1.1 Please enter a working directory :

```{r}
# Define the path to a working directory
wdir = file.path(tempdir())

# Define the file name of the output matching dataset (matching frame in Salvador with 500ha-cells )
name_output = "mf_SLV_500ha.gpkg"

```

:   1.2 The next step is to decide the parameters of your analysis, in
    the next cell please enter:

    -   the country code of El Salvador

    -   the size of the buffer

    -   the size of the grid

    -   the WDPA ID of the Parque Nacional Montecristo

```{r}
# Specify a country iso 3; to find the right country code, please refer to this page https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3
country = "SLV"
# Specify buffer width in meter
buffer_m = 10000
# Specify the grid cell size in meter
gridSize = 2236.068  # The grid size in meters (2236.068 meters) corresponds to an area of approximately 500 hectares (ha).

# Specify a WDPA IDs; to find the WDPA ID, please refer to this page 
# https://www.protectedplanet.net/en ,you can find the ID introducing the PA’s name in the browser or directly in the map
paid = c(9638)
```

### 2. Generating observation units for matching frame

The second step is to prepare a grid of observation units for the
spatial analysis, tailored specifically to the chosen country. After
downloading the country polygon, it is necessary to reproject the
country polygon to the appropriate UTM (Universal Transverse Mercator)
zone based on the country's centroid . The UTM projection minimizes
distortion for specific regions, making spatial calculations like
distances and areas more accurate.

➔ You can find more information about this projection [here](#0) and a
notebook specifically on projection
[here](https://www.earthdatascience.org/courses/earth-analytics/spatial-data-r/reproject-vector-data/).

A bounding box is created, and a grid with cells of 500 ha is generated
within this box covering the whole country. It is then intersected with
the country's boundary to retain only cells located within the country
as observation units.

> 2.1 Add the right objects in the `ggplot()` to obtain a map with the
    box, the grid, and the shape of the country

```{r}
# Download the country polygon (administrative boundary) and load it into the workspace
# 
# The `gadm()` function fetches spatial data for the specified country at the given resolution and administrative level.
#We store the polygon in the `gadm` object and convert it to a simple features (sf) object for easier manipulation.
gadm = gadm(country = country, resolution = 1, level = 0, path = wdir) %>%
  st_as_sf()

# Calculate the UTM (Universal Transverse Mercator) zone based on the country's centroid
# 
# The UTM zone is determined by the longitude of the centroid. We use the `st_centroid()` function to calculate
# the centroid of the country polygon, then use the `lonlat2UTM()` function to convert the longitude and latitude
# coordinates to the appropriate UTM zone code (North or South, based on the latitude).
centroid = st_coordinates(st_centroid(gadm))

# Helper function to convert longitude and latitude to UTM zone code.
# 
# This function returns the correct EPSG code by calculating the UTM zone based on the longitude and adding 32600 (for Northern Hemisphere) or 32700 (for Southern Hemisphere).
lonlat2UTM = function(lonlat) {
  utm = (floor((lonlat[1] + 180) / 6) %% 60) + 1
  if (lonlat[2] > 0) {
    utm + 32600  # Northern Hemisphere UTM code
  } else {
    utm + 32700  # Southern Hemisphere UTM code
  }
}
utm_code = lonlat2UTM(centroid)

# Reproject the GADM polygon to the correct UTM zone.
# 
# Using the UTM code determined from the centroid, we reproject the GADM data to align it with the appropriate UTM coordinate system for more accurate spatial calculations.
gadm_prj = gadm %>% st_transform(crs = utm_code)

# Create a bounding box around the reprojected country polygon.
# 
# The bounding box defines the spatial extent of the country, which will be used to create a grid in the following steps.
bbox = st_bbox(gadm_prj) %>% st_as_sfc() %>% st_as_sf()

# Generate a grid covering the bounding box extent.
# 
# The `st_make_grid()` function creates a regular grid of cells over the bounding box. The grid size is defined by the `gridSize` parameter, which sets the dimensions of each cell.
grid.ini = st_make_grid(bbox, cellsize = c(gridSize, gridSize))

# Crop the grid to match the country boundary.
# 
# The grid is initially generated over the entire bounding box, which may include areas outside the country boundary.
# We use `st_intersects()` to keep only the grid cells that intersect with the country polygon, effectively cropping the grid to fit the country's boundaries.
grid.sub = grid.ini %>%
  st_intersects(gadm_prj, .) %>%
  unlist()

# Filter the grid based on the intersections.
# 
# Subset the grid to keep only the cells that intersect with the country, then convert it to an sf object and assign an ID to each grid cell for identification.
grid = grid.ini[sort(grid.sub)] %>%
  st_as_sf() %>%
  mutate(gridID = seq(1:nrow(.)))

# Visualize the results.
# 
# Use `ggplot2` to visualize the bounding box, the country polygon (reprojected), and the final cropped grid.
# The grid cells are visualized with transparent fills (alpha = 0) to clearly show their boundaries.
ggplot() +
  geom_sf(data = st_geometry(bbox)) +           # Plot the bounding box
  geom_sf(data = st_geometry(gadm_prj)) +       # Plot the reprojected country polygon
  geom_sf(data = st_geometry(grid), alpha = 0)  # Plot the grid (transparent fill with alpha=0)

```

>2.2 Display information about `gadm` and `gadm_prj` object. You can
    do this by simply typing the name of each object into the console.
      
> a\. What is the main coordinate system used for each geospatial
object?

[ANSWER HERE]

>b\. What are the units of measurement used in each projection?

[ANSWER HERE]

>c\. Why do we use a projected system (UTM) in some geospatial
applications instead of a geodetic system (WGS 84)?

[ANSWER HERE]


### 3. Classifying observation units

The next step is to classify observation units i.e. cells of the grid
into one of these four groups:

-   **Group 0-Background** : Control candidate cells, in neither group
    bellow

-   **Group 1-Analyzed PA** : Treatment candidate cells

-   **Group 2-Other PAs** : Cells of non-analyzed PA

-   **Group 3-Buffer zone** : Cells in the chosen perimeter around all
    PAs

In this section, we begin by downloading the PAs polygons from the WDPA
and reprojecting them to match the projection used earlier. After
filtering and cleaning the PA polygons, a buffer is created around each
one.Then, each polygon is assigned to a group (0,1,2,3). In the second
step, the dataset of PA polygons and their buffers is rasterized, and
zonal statistics are applied to assign each cell (observation unit) to a
treatment group.

>3.1 Find the right *sf* function to create buffer around all PA using
the buffer size (`buffer_m`) chosen before and complete the code

```{r}
# Fetch the protected area (PA) polygons or points for the specified country.
# The WDPA dataset is downloaded to the working directory.
wdpa = wdpa_fetch(country, wait = TRUE, download_dir = wdir)

# Clean and project the PA polygons:
#   - Adds a "geometry_type" column for polygon/point distinction.
#   - Keeps overlapping polygons (erase_overlaps = FALSE).
#   - Filters out proposed PAs and those with "point" geometry (we're only interested in polygons).
#   - Projects the PA polygons to the previously determined CRS (UTM zone).
wdpa_prj = wdpa_clean(wdpa, geometry_precision = 1000, 
                      erase_overlaps = FALSE) %>%
  filter(STATUS != "Proposed") %>%    # Remove proposed PAs
  filter(GEOMETRY_TYPE != "POINT") %>% # Remove point geometries
  st_transform(crs = utm_code)         # Reproject to the UTM coordinate system

# Isolate the PA of interest (i.e., the one being analyzed).
#   - Filter by the WDPAID of the target PA.
#   - Assign this PA to group 1 for identification.
wdpa_focus = wdpa_prj %>%
  filter(WDPAID == paid) %>%           # Select the PA to focus on
  mutate(group = 1)                    # Assign ID "1" for the focused PA

# Handle other PAs that are not the main PA:
#   - Filter out the focused PA.
#   - Use `st_buffer` with distance 0 to fix any geometry issues (e.g., self-intersections).
wdpa_nofocus = wdpa_prj %>%
  filter(WDPAID != paid) %>%           # Select all other PAs
  st_buffer(., dist = 0)               # Fix potential geometry issues

# Identify intersections between the other PAs and the focused PA:
#   - Compute the intersection between other PAs and the focused PA.
#   - Remove the intersecting parts from the other PAs to minimize overlap and noise during rasterization.
intersection = st_intersection(wdpa_nofocus, wdpa_focus)
wdpa_nofocus = wdpa_nofocus %>%
  { if (nrow(intersection) == 0) . else st_difference(., st_union(st_geometry(intersection))) } %>%
  mutate(group = 2)                    # Assign ID "2" for other PAs

# Create buffer zones around all PAs (both focused and non-focused):
#   - Buffers of size `buffer_m` are created around the PA polygons.
#   - Combine the buffer zones of focused and other PAs.
#   - Assign buffer zones to group 3 for identification.
wdpa_buffer = st_buffer(wdpa_focus, dist = buffer_m) %>%
  rbind(st_buffer(wdpa_nofocus, dist = buffer_m)) %>%  # Combine focused and other PA buffers
  mutate(group = 3)                    # Assign ID "3" for buffer zones

# Combine the focused PA, other PAs, and buffer zones into one dataset:
#   - This creates a unified dataset with all PA-related groups.
wdpa_groups = rbind(wdpa_focus, wdpa_nofocus, wdpa_buffer)

# Subset the combined dataset to include only polygons that intersect with the country's boundary:
#   - This ensures that only PAs and buffer zones within the country's extent are kept.
wdpa.sub = wdpa_groups %>%
  st_intersects(gadm_prj, .) %>%
  unlist()

# Filter the PA and buffer dataset to include only those intersecting with the country's boundary.
wdpa_groups = wdpa_groups[sort(wdpa.sub),] %>%
  st_as_sf()

# Visualization:
#   - Plot the country's boundary.
#   - Plot the analyzed PA, other PAs, and buffer zones, each in a different color for clarity.
colors = c("1" = "darkgreen", "2" = "darkblue", "3" = "orange")

ggplot() +
  # Plot the country's boundary
  geom_sf(data = st_geometry(gadm_prj), fill = "lightgray", color = "lightgray", lwd = 0.2) +
  
  # Plot the analyzed PA (group 1)
  geom_sf(data = subset(wdpa_groups, group == "1"), aes(fill = factor(group)), color = "black", lwd = 0.3, alpha = 0.6) +
  
  # Plot other PAs (group 2)
  geom_sf(data = subset(wdpa_groups, group == "2"), aes(fill = factor(group)), color = "black", lwd = 0.3, alpha = 0.6) +
  
  # Plot buffer zones (group 3)
  geom_sf(data = subset(wdpa_groups, group == "3"), aes(fill = factor(group)), color = "black", lwd = 0.3, alpha = 0.6) +

  # Define custom fill colors for each group and corresponding labels for the legend
  scale_fill_manual(values = colors, 
                    labels = c("1" = "Analyzed PA", 
                               "2" = "Other PAs", 
                               "3" = "Buffer Zone")) +
  
  # Set up theme and legend
  theme_bw() +
  theme(legend.position = "right")


```

The next step is to assign a group (Analyzed PA, Other PAs, Buffer, or
Others) to each cell in the grid. Since a cell can sometimes intersect
multiple groups, we need a method to determine which group the cell will
belong to. To do this, we first rasterize the WDPA data, converting it
from vector format into a raster grid. Working with raster data is more
computationally efficient than using vectors, which is especially
beneficial in a portfolio analysis where a significant amount of data
must be processed.

raster

:   A **raster** is a type of spatial data representation that consists
    of a grid of equal size pixels, where each cell has a specific value
    representing information, such as color in an image or a particular
    data value in a geographic context

A raster layers is created over the country's extent, with pixel size
equal to our grid cell size. The polygon layer (containing PAs and
buffer zones) is rasterized, and each pixel in the newly created raster
is assigned to a group with pixel values representing group IDs. In the
code if a pixel is intersecting multiple groups the smallest group value
is chosen. For instance, if a pixel is located both in a buffer polygon
and in the analyzed PA, the cell value will be set to group 1 i.e. the
analyzed PA zone, ensuring that PA group IDs (1or 2) take precedence
over buffer IDs. Then pixel with no group assigned take the value 0 for
Background. Another raster layer is created to assign a WDPA ID to each
unit of observation.

Finally, the raster values are aggregated into the grid cells with the
`mode` function. The mode function assign the group with the highest
frequency in the grid cell. Then values get merged into a single
dataset, and transformed to the WGS84 coordinate system to comply with
the mapme.biodiversity package requirements.

```{r}
# Initialize an empty raster with the spatial extent of the country.
r.ini = raster()
extent(r.ini) = extent(gadm_prj)  # Set the extent to match the country boundary (gadm_prj).

# Define the raster resolution based on the predefined grid size ('gridSize').
res(r.ini) = gridSize

# Rasterize the WDPA group data:
#   - Convert the 'wdpa_groups' polygons to raster format using the initialized raster (r.ini).
#   - The 'field' parameter specifies that we are rasterizing based on the "group" column.
#   - The 'min' function is used to prioritize smaller group values in cases of overlap:
#     PA groups (1 and 2) take priority over buffer zones (group 3).
#   - Pixels not covered by any polygons are assigned a background value of 0 (control candidates group).
r.group = rasterize(wdpa_groups, r.ini, field="group", fun="min", background=0) %>%
  mask(., gadm_prj)  # Mask to ensure the raster only covers the country extent.

# Rename the raster layer to "group" for clarity.
names(r.group) = "group"

# Rasterize the WDPAID values:
#   - This creates another raster based on the 'WDPAID' field, which identifies individual PAs.
#   - The 'first' function ensures that in overlapping areas, the first WDPAID is retained.
r.wdpaid = rasterize(wdpa_prj, r.ini, field="WDPAID", fun="first", background=0) %>%
  mask(., gadm_prj)  # Mask the raster to the country extent.
names(r.wdpaid) = "wdpaid"

# Aggregate the raster data to the grid cells:
#   - Extract group values from the raster (r.group) and assign each grid cell a value based on the most frequent (mode) pixel value within the cell.
#   - The 'gridID' column is appended to identify the cells.
grid.group = exact_extract(x=r.group, y=grid, fun='mode', append_cols="gridID") %>%
  rename(group = mode)  # Rename the column for clarity.

# Similarly, extract the most frequent WDPAID for each grid cell.
grid.wdpaid = exact_extract(x=r.wdpaid, y=grid, fun="mode", append_cols="gridID") %>%
  rename(wdpaid = mode)

# Merge the extracted group and WDPAID data into a single dataframe:
#   - First, merge the group data and WDPAID data by their common 'gridID'.
#   - Then, merge with the original grid to combine spatial information.
grid.param = grid.group %>%
  merge(., grid.wdpaid, by="gridID") %>%
  merge(., grid, by="gridID") %>%
  # Remove rows where the 'group' column contains NA values (i.e., empty or unassigned cells).
  drop_na(group) %>%
  # Drop the 'gridID' column since it’s no longer needed.
  subset(., select=-c(gridID)) %>%
  st_as_sf()  # Convert the result into an 'sf' (simple feature) object for spatial analysis.

# Reproject the grid to WGS84 (EPSG:4326), since the mapme.biodiversity package works with this CRS.
grid.param = st_transform(grid.param, crs=4326)

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
                    labels = c("Background", "Analysed PA", "other PA", "Buffer zone")) +
  theme_bw()

```

The function chosen as a parameter to aggregate pixel values into the
grid cells in the `exact_extract()` function influences the final group
attribution obtained for each cells.

Instead of assigning to a grid cell the most frequent value among the
pixels intersecting the cell we may want to retain the smallest pixel
value, so that if the grid cell overlaps with any pixels that intersects
a PA, the grid cell is assigned to the PA group.

>3.2 Try computing `grid.group_2` changing the function used in
`exact_extract()` to retain the smallest pixel value.

>3.3 Compute group statistics of `grid.param` and `grid.param_2`, comment
the difference

>3.4 Map `grid.param` and `grid.param_2` to visualize the difference.

```{r}
# Aggregate pixel values by taking the min
grid.group_2 = exact_extract(x=r.group, y=grid, fun='min', append_cols="gridID") %>%
  rename(group = min)
grid.wdpaid_2 = exact_extract(x=r.wdpaid, y=grid, fun="min", append_cols="gridID") %>%
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
stat_1=table(grid.param$group)
stat_2= table(grid.param_2$group)
c(stat_1,stat_2)
```

```{r}
# Visualize maps with "mode" and "min" function
# create the graph of grid.param
map_1=grid.param %>%
  ggplot() +
  geom_sf(aes(fill = factor(group)), lwd=0) + 
  scale_fill_manual(name="Group",
                    values = c("grey", "darkgreen", "darkblue", "orange"),
                    labels = c("Background", "Analysed PA", "other PA", "Buffer zone")) +
  theme_bw()

# create the graphe of grid.param_2
map_2=grid.param_2 %>%
  ggplot() +
  geom_sf(aes(fill = factor(group)), lwd=0) + 
  scale_fill_manual(name="Group",
                    values = c("grey", "darkgreen", "darkblue", "orange"),
                    labels = c("Background", "Analysed PA", "other PA", "Buffer zone")) +
  theme_bw()

grid.arrange(map_1, map_2, nrow = 2)
```

### 4. Downloading the data

Once each cell has been assigned to a group, all the covariates need to
be downloaded. To do so you can use the Mapme.biodiversity package that
enables you to download multiple resources and then compute the right
indicators.

➔ Please refer to this
[page](https://mapme-initiative.github.io/mapme.biodiversity/dev/) for
detailed information on the Mapme.biodiversity package, which will help
you complete the following section.

You will be ask to :

-   Download the soil data

-   Calculate elevation

-   Download travel time data and compute the indicators

This section use multisession to speed up the computing of covariates.

Multisession

:   **Multisession** refers to a type of parallel processing where
    multiple R sessions (or processes) are created to execute code
    concurrently. Each session operates independently and runs in its
    own R process, allowing for parallel execution of tasks.

➔ To learn more about multisession you can refer to this

```{r}

# Get input data ready for indicator calculation
      ## Version of Global Forest Cover data to consider
      list_version_gfc = mapme.biodiversity:::.available_gfw_versions() #all versions available
      version_gfc = list_version_gfc[length(list_version_gfc)] #last version considered
mapme_options(outdir =wdir)
years = 2000:2021
```

#### Covariate: Soil

>4.1 Enter the function to download soil data. Use the following
parameters : "clay' for the layers , a depth between 0 and 5 cm, and the
statistic "mean".

```{r}
start_time = Sys.time()


get.soil = grid.param %>% get_resources(# enter the right function and parameter here
         get_soilgrids( 
        layers = "clay",
        depths = "0-5cm", 
        stats = "mean"))
                         
# set up parallel plan with 6 workers
plan(multisession, workers = 6)
# Calculate Indicator
with_progress({
  # 'zonal.soil' stores the results of soil property calculations
zonal.soil = get.soil %>%
  calc_indicators(
     # Inside 'calc_indicators', use 'calc_soilproperties' to calculate specific soil properties
    calc_soilproperties(
      # 'stats = "mean"' specifies that you want to calculate the mean (average) of soil properties
      stats = "mean",
      # 'engine = "zonal"' means the calculations will be performed using a zonal approach
      # This method calculates the mean value over each pixel falling entirely or partially in the cell
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
      


end_time = Sys.time()
elapsed_time = end_time - start_time

cat("Elapsed time:", elapsed_time, "\n")
```

>4.2 Try running the same code chunk without the multisession and
compare execution times

```{r}
start_time = Sys.time()
get.soil = grid.param %>% get_resources(get_soilgrids(
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
      


end_time = Sys.time()
elapsed_time = end_time - start_time

cat("execution time:", elapsed_time, "\n")
```

#### Covariate: Elevation

>4.3 Insert the function to calculate elevation mean, you should use
the `'mean'` statistic and the `'zonal'` engine

```{r}
get.elevation = grid.param %>% get_resources(get_nasa_srtm())
# set up parallel plan with 6 workers
plan(multisession, workers = 6)

# Calculate Indicator
with_progress({
  zonal.elevation = get.elevation %>% calc_indicators(calc_elevation(
          stats= "mean",
          engine = "zonal"))
  
  
})
plan(sequential) # close child processes
# Transform the output dataframe into a pivot dataframe
pivot.elevation = zonal.elevation %>% unnest(elevation)%>%
  pivot_wider(names_from = c("variable"), values_from = "value")%>%
  dplyr::select(-c(datetime,unit))
      


```

#### Covariate: TRI

```{r}
# set up parallel plan with 6 workers
plan(multisession, workers = 6)
# Calculate Indicator
with_progress({
  zonal.tri = get.elevation %>% calc_indicators(calc_tri(
          stats = "mean",
          engine = "zonal"))
})
plan(sequential) # close child processes
# Transform the output dataframe into a pivot dataframe
pivot.tri = zonal.tri %>% unnest(tri)%>%
  pivot_wider(names_from = c("variable"), values_from = "value")%>%
  dplyr::select(-c(datetime,unit))


```

#### Covariate: Travel Time

>4.4 Download data related to travel time and calculate the median
travel time

```{r}

get.travelT = grid.param%>% get_resources(get_nelson_et_al(ranges = c("5k_110mio")))

# set up parallel plan with 6 concurrent threads
plan(multisession, workers = 20)
# Calculate Indicator
with_progress({
  zonal.travelT  =get.travelT %>% calc_indicators(calc_traveltime(
          stats = "median",
          engine = "zonal"))
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
get.tree = grid.param %>% get_resources(get_gfw_treecover(version =  version_gfc),
                                        get_gfw_lossyear(version = version_gfc))
# set up parallel plan with 6 workers
plan(multisession, workers = 6)
# Calculate time series
with_progress({
  zonal.tree = get.tree %>% calc_indicators(calc_treecover_area(
    years = years,
    min_size = 0.5,
    min_cover =
      10
  ))
  
})


plan(sequential) # close child processes

# Transform the output dataframe into a pivot dataframe
pivot.tree = zonal.tree %>%
  unnest(treecover_area) %>%
  # Transform treecover unit to percentage
  mutate(value = round((value * 1e4) / (gridSize ^ 2) * 100, 2),
         datetime = format(datetime, "%Y")) %>%
  pivot_wider(names_from = "datetime",
              values_from = "value",
              names_prefix = "treecover_") %>%
  dplyr::select(-c(unit, variable))

```

```{r}
# The calculation of tree loss area is performed

# Get the column names of tree cover time series
colnames_tree = names(pivot.tree)[startsWith(names(pivot.tree), "treecover")]

# Drop the first year
dropFirst = tail(colnames_tree, -1)
# Drop the last year
dropLast = head(colnames_tree, -1)

# This operation is done to compute the treeloss easier 

# Set list of new column names for tree loss time series
colnames_loss = dropFirst %>% str_split(., "_")
# Add new columns: treeloss_tn = treecover_tn - treecover_t(n-1)
for (i in 1:length(dropFirst)) {
  new_colname = paste0("treeloss_", colnames_loss[[i]][2])
  pivot.tree[[new_colname]] = pivot.tree[[dropFirst[i]]] - pivot.tree[[dropLast[i]]]
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
pivot.all = Reduce(dplyr::full_join,
                   list(df.travelT, df.soil, df.tree, df.elevation, df.tri, df.geom)) %>%
  st_as_sf()
# Make column Group ID and WDPA ID have data type "integer"
pivot.all$group = as.integer(pivot.all$group)
pivot.all$wdpaid = as.integer(pivot.all$wdpaid)

# Export the matching frame
st_write(pivot.all, dsn = file.path(wdir, name_output), delete_dsn = TRUE)

```

### 5. Creating an interactive map (coming soon...)

To visualize indicators on a map you can use the package tmap. It is
particularly useful for analyzing geospatial data because it provides an
dynamic map for visualizing them.

>5.1 Run the following code and comment on the mean distance to city in
El Salvador

```{r}
# Visualize PAs by mean forest loss
tmap_mode("view")
pivot.all %>%
  portfolio_long() %>%
  filter(variable == "elevation_mean") %>%
  tm_shape() +
  tm_polygons(col = "value", title = "Mean elevation (m)")
```

>5.2 Try to visualize another indicator of your choice
