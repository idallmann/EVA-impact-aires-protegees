#####
#Functions for matching
#####

lonlat2UTM = function(lonlat) 
{
  utm = (floor((lonlat[1] + 180) / 6) %% 60) + 1
  if (lonlat[2] > 0) {
    utm + 32600
  } else{
    utm + 32700
  }
}

fn_pre_grid = function(iso, path_tmp, path_save, gridSize)
{
  # Download country polygon to working directory and load it into workspace
  gadm = gadm(country = iso, resolution = 1, level = 0, path = path_tmp) %>% 
    st_as_sf() 
  
  # Find UTM zone of the country centroid
  centroid = st_coordinates(st_centroid(gadm))
  utm_code = lonlat2UTM(centroid)
  # Reproject GADM
  gadm_prj = gadm %>% st_transform(crs = utm_code)
  
  # Make bounding box of projected country polygon
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
  #Visualize and save the grid
  fig_grid = ggplot() +
    geom_sf(data = st_geometry(bbox)) +
    geom_sf(data = st_geometry(gadm_prj)) +
    geom_sf(data = st_geometry(grid), alpha = 0)
  fig_save = paste0(path_tmp, "/fig_grid_", iso, ".png")
  ggsave(fig_save,
         plot = fig_grid,
         device = "png",
         height = 6, width = 9)
  aws.s3::put_object(file = fig_save, 
                     bucket = paste("projet-afd-eva-ap/data_tidy/mapme_bio_data/matching", iso, sep = "/"), 
                     region = "", 
                     show_progress = FALSE)
  #Return outputs
  list_output = list("ctry_shp_prj" = gadm_prj, "grid" = grid, "utm_code" = utm_code)
  return(list_output)
}

fn_pre_group = function(iso, path_tmp, utm_code, buffer_m, data, gadm_prj, grid, gridSize)
{
  wdpa = wdpa_fetch(x = iso, wait = TRUE, download_dir = path_tmp)
  # PAs are projected, and column "geometry_type" is added
  wdpa_prj = wdpa_clean(wdpa, geometry_precision = 1000) %>%
    # Remove the PAs that are only proposed, or have geometry type "point"
    filter(STATUS != "Proposed") %>%
    filter(GEOMETRY_TYPE != "POINT") %>%
    # Project PA polygons to the previously determined UTM zone
    st_transform(crs = utm_code) 
  
  # Make Buffers around all protected areas
  buffer <- st_buffer(wdpa_prj, dist = buffer_m) %>% 
    # Assign an ID "3" to the buffer group
    mutate(group=3)
  
  # Separate funded and non-funded protected areas
  paid = data[data$iso3 == iso,]$wdpaid
  wdpaID_funded = paid
  wdpa_funded = wdpa_prj %>% filter(WDPAID %in% wdpaID_funded) %>%
    mutate(group=1) # Assign an ID "1" to the funded PA group
  wdpa_nofund = wdpa_prj %>% filter(!WDPAID %in% wdpaID_funded) %>% 
    mutate(group=2) # Assign an ID "2" to the non-funded PA group
  # Merge the dataframes of funded PAs, non-funded PAs and buffers
  wdpa_groups = rbind(wdpa_funded, wdpa_nofund, buffer)
  # Subset to polygons that intersect with country boundary
  wdpa.sub = wdpa_groups %>% 
    st_intersects(gadm_prj, .) %>% 
    unlist()
  # Filter the PA+buffer to the subset
  wdpa_groups = wdpa_groups[sort(wdpa.sub),] %>%
    st_as_sf()
  
  # Initialize an empty raster to the spatial extent of the country
  r.ini = raster()
  extent(r.ini) = extent(gadm_prj)
  # Specify the raster resolution as same as the pre-defined 'gridSize'
  res(r.ini) = gridSize
  # Assign the raster pixels with "Group" values, 
  # Take the minial value if a pixel is covered by overlapped polygons, so that PA Group ID has higher priority than Buffer ID.
  # Assign value "0" to the background pixels (control candidates group)
  r.group = rasterize(wdpa_groups, r.ini, field="group", fun="min", background=0) %>%
    mask(., gadm_prj)
  # Rename Layer
  names(r.group) = "group"
  
  # Rasterize wdpaid
  r.wdpaid = rasterize(wdpa_prj, r.ini, field="WDPAID", fun="first", background=0) %>%
    mask(., gadm_prj)
  names(r.wdpaid) = "wdpaid"
  
  # Aggregate pixel values by taking the majority
  grid.group = exact_extract(x=r.group, y=grid, fun='mode', append_cols="gridID") %>%
    rename(group = mode)
  grid.wdpaid = exact_extract(x=r.wdpaid, y=grid, fun="mode", append_cols="gridID") %>%
    rename(wdpaid = mode)
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
  
  # Visualize and save grouped grid cells
  fig_grid_group = 
    ggplot(grid.param) +
    geom_sf(aes(color=as.factor(group))) +
    scale_color_viridis_d(
      # legend title
      name="Group", 
      # legend label
      labels=c("control candidate", "treatment candidate", "non-funded PA", "buffer zone")) +
    theme_bw()
  fig_save = paste0(path_tmp, "/fig_grid_group_", iso, ".png")
  ggsave(fig_save,
         plot = fig_grid_group,
         device = "png",
         height = 6, width = 9)
  aws.s3::put_object(file = fig_save, 
                     bucket = paste("projet-afd-eva-ap/data_tidy/mapme_bio_data/matching", iso, sep = "/"), 
                     region = "", 
                     show_progress = FALSE)
  
  #Return outputs
  return(grid.param)
  
}

fn_pre_mf = function(grid.param, path_tmp, iso, name_output, ext_output) 
{
  print("----Initialize portfolio")
  # Get input data ready for indicator calculation
  aoi = init_portfolio(grid.param,
                       years = y_first:y_last,
                       outdir = path_tmp,
                       cores = 12,
                       add_resources = FALSE)
  print("----Download Rasters and Calculate Covariates")
  print("------Soil")
  # Covariate: Soil
  # Download Data
  get.soil = get_resources(aoi, 
                           resources = c("soilgrids"), 
                           layers = c("clay"), # resource specific argument
                           depths = c("0-5cm"), # resource specific argument
                           stats = c("mean")) %>%
    # Calculate Indicator
    calc_indicators(., 
                    indicators = "soilproperties",
                    stats_soil = c("mean"),
                    engine = "zonal") %>%
    # Transform the output dataframe into a pivot dataframe
    unnest(soilproperties) %>%
    mutate(across(mean, round, 3)) %>% # Round numeric columns
    pivot_wider(names_from = c("layer", "depth", "stat"), values_from = "mean")
  print("------Elevation")
  # Covariate: Elevation
  # get.elevation = get_resources(aoi, "nasa_srtm") %>%
  #   calc_indicators(.,
  #                   indicators = "elevation",
  #                   stats_elevation = c("mean")) %>%
  #   unnest(elevation)
  print("------TRI")
  # Covariate: TRI
  # get.tri = get_resources(aoi, "nasa_srtm") %>%
  #   calc_indicators(., indicators = "tri") %>%
  #   unnest(tri)
  print("------Travel time")
  # Covariate: Travel Time
  get.travelT = get_resources(aoi, resources = "nelson_et_al",
                              range_traveltime = c("5k_110mio")) %>% # resource specific argument
    calc_indicators(., 
                    indicators = "traveltime",
                    stats_accessibility = c("median")) %>%
    unnest(traveltime) %>%
    pivot_wider(names_from = "distance", values_from = "minutes_median", names_prefix = "minutes_median_")
  
  print("----Calculate Deforestation")
  # Time Series of Tree Cover Area
  get.tree = get_resources(aoi, resources = c("gfw_treecover", "gfw_lossyear")) %>%
    calc_indicators(.,
                    indicators = "treecover_area", 
                    min_size=1, # indicator-specific argument
                    min_cover=10) %>% # indicator-specific argument
    unnest(treecover_area) %>%
    mutate(across(treecover, round, 3)) %>% # Round numeric columns
    pivot_wider(names_from = "years", values_from = "treecover", names_prefix = "treecover_")
  
  # The calculation of tree loss area is performed at dataframe base
  # Get the column names of tree cover time series
  colnames_tree = names(get.tree)[startsWith(names(get.tree), "treecover")]
  # Drop the first year
  dropFirst = tail(colnames_tree, -1)
  # Drop the last year
  dropLast = head(colnames_tree, -1)
  # Set list of new column names for tree loss time series
  colnames_loss = dropFirst %>% str_split(., "_")
  
  get.tree = get.tree 
  # Add new columns: treeloss_tn = treecover_tn - treecover_t(n-1)  
  for (i in 1:length(dropFirst)) {
    new_colname <- paste0("treeloss_", colnames_loss[[i]][2]) 
    get.tree[[new_colname]] <- get.tree[[dropFirst[i]]] - get.tree[[dropLast[i]]]
  }
  
  print("----Export Matching Frame")
  # Remove "geometry" column from pivot dataframes
  df.tree = get.tree %>% mutate(x = NULL) %>% as.data.frame()
  df.travelT = get.travelT %>% mutate(x = NULL) %>% as.data.frame()
  df.soil = get.soil %>% mutate(x = NULL) %>% as.data.frame()
  # df.elevation = get.elevation %>% mutate(x = NULL) %>% as.data.frame()
  # df.tri = get.tri %>% mutate(x=NULL) %>% as.data.frame()
  
  # Make a dataframe containing only "assetid" and geometry
  df.geom = get.tree[, c("assetid", "x")] %>% as.data.frame()
  
  # Merge all output dataframes 
  # pivot.all = Reduce(dplyr::full_join, list(df.travelT, df.soil, df.tree, df.elevation, df.tri, df.geom)) %>%
  #   st_as_sf()
  pivot.all = Reduce(dplyr::full_join, list(df.travelT, df.soil, df.tree, df.geom)) %>%
    st_as_sf()
  # Make column Group ID and WDPA ID have data type "integer"
  pivot.all$group = as.integer(pivot.all$group)
  pivot.all$wdpaid = as.integer(pivot.all$wdpaid)
  
  name_save = paste0(name_output, "_", iso, ext_output)
  s3write_using(pivot.all,
                sf::st_write,
                object = paste0("data_tidy/mapme_bio_data/matching", "/", iso, "/", name_save),
                bucket = "projet-afd-eva-ap",
                opts = list("region" = "")
  )
  
  
}
