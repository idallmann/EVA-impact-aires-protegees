#####
#Functions for matching
#####

###
#Pre-processing
###

fn_pre_log = function(list_iso, name, notes)
{
  str_iso = paste(list_iso, collapse = ", ")
  log = paste(tempdir(), name, sep = "/")
  file.create(log)
  #Do not forget to end the writing with a \n to avoid warnings
  #cat(paste("#####\nCOUNTRY :", iso, "\nTIME :", print(Sys.time(), tz = "UTC-2"), "\n#####\n\n###\nPRE-PROCESSING\n###\n\n"), file = log, append = TRUE)
  cat(paste("STARTING TIME :", print(Sys.time(), tz = "UTC-2"), "\nCOUNTRIES :", str_iso, "\nNOTES :", notes, "\n\n##########\nPRE-PROCESSING\n##########\n\n"), file = log, append = TRUE)
  
  return(log)
}

#Find UTM code for a given set of coordinates
##INPUTS : coordinates (lonlat)
##OUTPUTS : UTM code
lonlat2UTM = function(lonlat) 
{
  utm = (floor((lonlat[1] + 180) / 6) %% 60) + 1
  if (lonlat[2] > 0) {
    utm + 32600
  } else{
    utm + 32700
  }
  
}

#Create a gridding of a given country
##INPUTS : 
### iso : ISO code 
### path_temp : temporary path for saving figures
### data_pa : dataset with information on protected areas, and especially their surfaces
### sampling : Number of pixels that subdivide the protected area with lowest area in the country considered
##OUTPUTS : 
### gadm_prj : country shapefile 
### grid : gridding of the country
### utm_code : UTM code of the country
### gridSize : the resolution of gridding, defined from the area of the PA with the lowest area

fn_pre_grid = function(iso, yr_min, path_tmp, data_pa, sampling, log, save_dir)
{
  
  output = withCallingHandlers(
    
    {
      
  # Download country polygon to working directory and load it into workspace
  gadm = gadm(country = iso, resolution = 1, level = 0, path = path_tmp) %>% 
    st_as_sf() %>%
    st_make_valid() #Necessary for some polygons : e.g BEN
  
  # Find UTM zone of the country centroid
  centroid = st_coordinates(st_centroid(gadm))
  utm_code = lonlat2UTM(centroid)
  # Reproject GADM
  gadm_prj = gadm %>% 
    st_transform(crs = utm_code)
  
  #Determine relevant grid size
  ##Select the PA in the country with minimum area. PAs with null areas, marine or treatment year before 2000 are discarded (not analyzed anyway)
  pa_min = data_pa %>%
    filter(iso3 == iso & is.na(wdpaid) == FALSE & status_yr >= yr_min & marine %in% c(0,1)) %>%
    arrange(area_km2) %>%
    slice(1)
  ##From this minimum area, define the grid size. 
  ##It depends on the sampling of the minimal area, i.e how many pixels we want to subdivide the PA with lowest area
  ## To avoid a resolution higher than the one of our data, grid size is set to be 30m at least (resolution of tree cover data, Hansen et al. 2013)
  area_min = pa_min$area_km2 #in kilometer
  gridSize = max(1e3, round(sqrt(area_min/sampling)*1000, 0)) #Side of the pixel is expressed in meter and rounded, if above 1km. 
  
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
  
  #Extract country name
  country.name = data_pa %>% 
    filter(iso3 == iso) %>% 
    slice(1)
  country.name = country.name$country_en
  
  #Visualize and save the grid
  fig_grid = ggplot() +
    geom_sf(data = st_geometry(bbox)) +
    geom_sf(data = st_geometry(gadm_prj)) +
    geom_sf(data = st_geometry(grid), alpha = 0) +
    labs(title = paste("Gridding of", country.name))
  fig_save = paste0(path_tmp, "/fig_grid_", iso, ".png")
  ggsave(fig_save,
         plot = fig_grid,
         device = "png",
         height = 6, width = 9)
  aws.s3::put_object(file = fig_save, 
                     bucket = paste("projet-afd-eva-ap", save_dir, iso, sep = "/"), 
                     region = "", 
                     show_progress = FALSE)
  
  #Append the log 
  cat("#Generating observation units\n-> OK\n", file = log, append = TRUE)
  
  #Return outputs
  list_output = list("ctry_shp_prj" = gadm_prj, 
                     "grid" = grid, 
                     "gridSize" = gridSize, 
                     "utm_code" = utm_code,
                     "is_ok" = TRUE)
  return(list_output)
  
    },
  
  error = function(e)
  {
    #Print the error and append the log
    print(e)
    #Append the log 
    cat(paste("#Generating observation units\n-> Error :\n", e, "\n"), file = log, append = TRUE)
    #Return string to inform user to skip
    return(list("is_ok" = FALSE))
  },
  
  warning = function(w)
  {
    #Print the warning and append the log
    print(w)
    #Append the log 
    cat(paste("#Generating observation units\n-> Warning :\n", w, "\n"), file = log, append = TRUE)
    #Return string to inform user to skip
    return(list("is_ok" = TRUE))
  }
  
  )
  
  return(output)
}

#Assign each pixel (observation unit) to a group : PA non-funded, funded and analyzed, funded and not analyzed, buffer, potential control
##INPUTS :
### iso : country ISO code
### path_tmp : temporary path to save figures
### utm_code : UTM of the country centroid
### buffer_m : buffer width, in meter
### data : a dataframe with the WDPAID of PAs funded by the AFD
### gadm_prj : country polygon, projected so that crs = UTM code
### grid : gridding of the country
### gridSize : resolution of the gridding
##OUTPUTS : 
### grid.param : a raster representing the gridding of the country with two layers. One for the group each pixel belongs to (funded PA, non-funded PA, potential control, buffer), the other for the WDPAID corresponding to each pixel (0 if not a PA)
##NOTES :
### Errors can arise from the wdpa_clean() function, during "formatting attribute data" step. Can be settled playing with geometry_precision parameter

fn_pre_group = function(iso, wdpa_raw, yr_min, path_tmp, utm_code, buffer_m, data_pa, gadm_prj, grid, gridSize, log, save_dir)
{
  
  output = withCallingHandlers(
    
    {

  # The polygons of PAs are taken from WDPA, cleaned
  wdpa_prj = wdpa_raw %>%
    filter(ISO3 == iso) %>%
    #st_make_valid() %>%
    #celanign of PAs from the wdap_clean function :
    #Status filtering is performed manually juste after. 
    #The geometry precision is set to default. Used to be 1000 in Kemmeng code
    # Overlaps are not erased because we rasterize polygons
    #UNESCO Biosphere Reserves are not excluded so that our analysis of AFD portfolio is the most extensive
    wdpa_clean(retain_status = NULL,
               erase_overlaps = FALSE,
               exclude_unesco = FALSE,
               verbose = TRUE) %>% 
    # Remove the PAs that are only proposed, or have geometry type "point"
    filter(STATUS != "Proposed") %>%  #24/08/2023 : "Proposed" status concerns only 6 PAs in the sample, including one implemented after 2000.
    filter(GEOMETRY_TYPE != "POINT") %>%
    # Project PA polygons to the previously determined UTM zone
    st_transform(crs = utm_code) 
  
  # Make Buffers around all protected areas
  buffer = st_buffer(wdpa_prj, dist = buffer_m) %>% 
    # Assign an ID "5" to the buffer group
    mutate(group=5,
           group_name = "Buffer")
  
  # Separate funded and non-funded protected areas
  ##PAs funded by AFD 
  ###... which can bu used in impact evaluation : in the country of interest, wdpaid known, area above 1km² (Wolf et al. 2021), implemented after yr_min defined by the user, non-marine (terrestrial or coastal, Wolf et al. 2021)
  pa_afd_ie = data_pa %>%
    filter(iso3 == iso & is.na(wdpaid) == FALSE & area_km2 > 1 & status_yr >= yr_min & marine %in% c(0,1))
  wdpaID_afd_ie = pa_afd_ie[pa_afd_ie$iso3 == iso,]$wdpaid
  wdpa_afd_ie = wdpa_prj %>% filter(WDPAID %in% wdpaID_afd_ie) %>%
    mutate(group=2,
           group_name = "Funded PA, analyzed") # Assign an ID "2" to the funded PA group
  ###...which cannot
  pa_afd_no_ie = data_pa %>%
    filter(iso3 == iso & (is.na(wdpaid) == TRUE | area_km2 <= 1 | is.na(area_km2) | status_yr < yr_min | marine == 2)) #PAs not in WDPA, of area less than 1km2 (Wolf et al 2020), not terrestrial/coastal or implemented after yr_min are not analyzed
  wdpaID_afd_no_ie = pa_afd_no_ie[pa_afd_no_ie$iso3 == iso,]$wdpaid 
  wdpa_afd_no_ie = wdpa_prj %>% filter(WDPAID %in% wdpaID_afd_no_ie) %>%
    mutate(group=3,
           group_name = "Funded PA, not analyzed") # Assign an ID "3" to the funded PA group which cannot be stuided in the impact evaluation
  ##PAs not funded by AFD
  wdpa_no_afd = wdpa_prj %>% filter(!WDPAID %in% c(wdpaID_afd_ie, wdpaID_afd_no_ie)) %>% 
    mutate(group=4,
           group_name = "Non-funded PA") # Assign an ID "4" to the non-funded PA group
  wdpaID_no_afd = wdpa_no_afd$WDPAID
  
  # Merge the dataframes of funded PAs, non-funded PAs and buffers
  # CAREFUL : the order of the arguments does matter. 
  ## During rasterization, in case a cell of the raster is on both funded analysed and non-funded, we want to cell to take the WDPAID of the funded analysed.
  ## Same funded, not analyzed. As the first layer is taken, wdpa_afd_ie needs to be first !
  wdpa_groups = rbind(wdpa_afd_ie, wdpa_afd_no_ie, wdpa_no_afd, buffer)
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
  # Take the minimal value if a pixel is covered by overlapped polygons, so that PA Group ID has higher priority than Buffer ID.
  # Assign value "0" to the background pixels (control candidates group)
  # fun = "min" can lead to bad group assignment. This issue is developed and tackled below
  r.group = rasterize(wdpa_groups, r.ini, field="group", fun="min", background=0) %>%
    mask(., gadm_prj)
  # Rename Layer
  names(r.group) = "group"
  
  # Rasterize wdpaid
  ## CAREFUL : as stated above, the wdpa_groups raster is ordered so that the first layer is the one of funded, analyzed PA. Thus one needs to have fun = "first"
  r.wdpaid = rasterize(wdpa_groups, r.ini, field="WDPAID", fun="first", background=0) %>%
    mask(., gadm_prj)
  names(r.wdpaid) = "wdpaid"
  
  # Aggregate pixel values by taking the majority
  grid.group.ini = exact_extract(x=r.group, y=grid, fun='mode', append_cols="gridID") %>%
    rename(group = mode)
  grid.wdpaid = exact_extract(x=r.wdpaid, y=grid, fun="mode", append_cols="gridID") %>%
    rename(wdpaid = mode)

  # Randomly select background pixels as potential control pixels
  ##Take the list of background pixels, the  number of background and treatment pixels
  list_back_ID = grid.group.ini[grid.group.ini$group == 0 & is.na(grid.group.ini$group) == FALSE,]$gridID
  n_back_ID = length(list_back_ID)
  n_treat = length(grid.group.ini[grid.group.ini$group == 2 & is.na(grid.group.ini$group) == FALSE,]$gridID)
  ##The number of potential control units is five times the number of treatment units
  n_control = min(n_back_ID, n_treat*5)
  ##Select randomly the list of background pixels selected as controls
  ### Note that we control for the case n_back_ID = 1, which causes weird behavior using sample()
  set.seed(0) #To ensure reproductibility of the random sampling
  if(n_back_ID <= 1) list_control_ID = list_back_ID else list_control_ID = sample(x = list_back_ID, size = n_control, replace = FALSE)
  ## Finally, assign the background pixel chosen to the control group, characterized by group = 1
  grid.group = grid.group.ini %>%
    mutate(group = case_when(gridID %in% list_control_ID ~ 1,
                             TRUE ~ group))
  

  # Merge data frames
  grid.param.ini = grid.group %>%
    merge(., grid.wdpaid, by="gridID") %>%
    merge(., grid, by="gridID") %>%
    # drop rows having "NA" in column "group"
    drop_na(group) %>%
    st_as_sf() %>%
    # Grid is projected to WGS84 because mapme.biodiverty package merely works with this CRS
    st_transform(crs=4326) %>%
    #Add treatment year variable
    left_join(dplyr::select(data_pa, c(region_afd, region, sub_region, country_en, iso3, wdpaid, status_yr, year_funding_first, year_funding_all)), by = "wdpaid")
  
  # If two PAs in different groups overlap, then the rasterization with fun = "min" (as in r.group definition) can lead to bad assignment of pixels.
  # For instance, if a PA non-funded (group = 4) overlaps with a funded, analyzed one (group = 2), then the pixel will be assigned to the group 2
  # Same for group 3 (funded, not analyzed). Then, the following correction is applied.
  # Finally, each group is given a name for later plotting
  # grid.param = grid.param.ini %>%
  #   mutate(group = case_when(wdpaid %in% wdpaID_no_afd & group == 2 ~ 4,
  #                            wdpaid %in% wdpaID_afd_no_ie & group == 2 ~3,
  #                            TRUE ~ group)) %>%
  
  #/!\ For the moment, a pixel both non-funded and funded is considered funded !
  #But if funded not analyzed AND funded analyzed, then funded not analyzed.
  #Idea : the pixel could be treated out of the period considered, so not comparable to toher treatment pixels considered in funded, analyzed.
  # -> Check with Léa, Ingrid and PY if that seems OK
  grid.param = grid.param.ini %>%
    mutate(group = case_when(wdpaid %in% wdpaID_afd_no_ie & group == 2 ~ 3,
                             TRUE ~ group)) %>%
    #Add name for the group
    mutate(group_name = case_when(group == 0 ~ "Background",
                                  group == 1 ~ "Potential control",
                                  group == 2 ~ "Funded PA, analyzed (potential treatment)",
                                  group == 3 ~ "Funded PA, not analyzed",
                                  group == 4 ~ "Non-funded PA",
                                  group == 5 ~ "Buffer")) %>%
  #Add spatial resolution in m2 : useful to compute share of forest area in a given pixel and extrapolate to the PA for instance
  mutate(res_m2 = gridSize)
  
  
  #Save the grid
  s3write_using(grid.param,
                sf::write_sf,
                overwrite = TRUE,
                object = paste0(save_dir, "/", iso, "/", paste0("grid_param_", iso, ".gpkg")),
                bucket = "projet-afd-eva-ap",
                opts = list("region" = ""))
  
  # Visualize and save grouped grid cells
  
  ## Extract country name
  country.name = grid.param %>% 
    filter(group == 2) %>% 
    slice(1)
  country.name = country.name$country_en
  
  fig_grid_group = 
    ggplot(grid.param) +
    geom_sf(aes(fill = as.factor(group_name)), color = NA) +
    labs(title = paste("Gridding of", country.name)) +
    scale_fill_brewer(name = "Group", type = "qual", palette = "YlGnBu", direction = -1) +
    # scale_color_viridis_d(
    #   # legend title
    #   name="Group", 
    #   # legend label
    #   labels=c("control candidate", "treatment candidate", "non-funded PA", "buffer zone")) +
    theme_bw()
  fig_save = paste0(path_tmp, "/fig_grid_group_", iso, ".png")
  ggsave(fig_save,
         plot = fig_grid_group,
         device = "png",
         height = 6, width = 9)
  aws.s3::put_object(file = fig_save,
                     bucket = paste("projet-afd-eva-ap", save_dir, iso, sep = "/"),
                     region = "", 
                     show_progress = FALSE)
  
  
  # Pie plots
  df_pie_wdpa = data_pa %>%
    filter(iso3 == iso) %>%
    dplyr::select(c(iso3, wdpaid, name_pa, status_yr, area_km2)) %>%
    mutate(group_wdpa = case_when(is.na(wdpaid) == FALSE ~ "WDPA",
                             is.na(wdpaid) == TRUE ~ "Not WDPA")) %>%
    group_by(iso3, group_wdpa) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    mutate(n_tot = sum(n),
           freq = round(n/n_tot*100, 1))
  
  df_pie_ie = wdpa_prj %>%
    st_drop_geometry() %>%
    dplyr::select(c(ISO3, WDPAID)) %>%
    mutate(group_ie = case_when(!WDPAID %in% c(wdpaID_afd_ie, wdpaID_afd_no_ie) ~ "Non-funded",
                                WDPAID %in% wdpaID_afd_ie ~ "Funded, analyzed",
                                WDPAID %in% wdpaID_afd_no_ie ~ "Funded, not analyzed")) %>%
    group_by(ISO3, group_ie) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    mutate(n_tot = sum(n),
           freq = round(n/n_tot*100, 1))
  
  ## PAs funded : reported in the WDPAID or not
  pie_wdpa = ggplot(df_pie_wdpa, 
                        aes(x="", y= freq, fill = group_wdpa)) %>%
    + geom_bar(width = 0.5, stat = "identity", color="white") %>%
    + coord_polar("y", start=0) %>%
    + geom_label_repel(aes(x=1.1, label = paste0(round(freq, 1), "% (", n, ")")), 
                       color = "black", 
                       position = position_stack(vjust = 0.55), 
                       size=4, show.legend = FALSE) %>%
    # + geom_label(aes(x=1.4, label = paste0(freq_iucn, "%")), 
    #              color = "white", 
    #              position = position_stack(vjust = 0.7), size=2.5, 
    #              show.legend = FALSE) %>%
    + labs(x = "", y = "",
           title = "Share of PAs funded and reported in the WDPA",
           subtitle = paste("Sample :", sum(df_pie_wdpa$n), "funded protected areas in", country.name)) %>%
    + scale_fill_brewer(name = "", palette = "Greens") %>%
    + theme_void()
  
  ## PAs in the WDPA : analyzed or not
  pie_ie = ggplot(df_pie_ie, 
                    aes(x="", y= freq, fill = group_ie)) %>%
    + geom_bar(width = 0.5, stat = "identity", color="white") %>%
    + coord_polar("y", start=0) %>%
    + geom_label_repel(aes(x=1.1, label = paste0(round(freq, 1), "% (", n, ")")), 
                       color = "black", 
                       position = position_stack(vjust = 0.55), 
                       size=4, show.legend = FALSE) %>%
    # + geom_label(aes(x=1.4, label = paste0(freq_iucn, "%")), 
    #              color = "white", 
    #              position = position_stack(vjust = 0.7), size=2.5, 
    #              show.legend = FALSE) %>%
    + labs(x = "", y = "",
           title = "Share of PAs reported in the WDPA and analyzed",
           subtitle = paste("Sample :", sum(df_pie_ie$n), "funded protected areas in", country.name)) %>%
    + scale_fill_brewer(name = "", palette = "Greens") %>%
    + theme_void()
  
  ##Saving plots
  tmp = paste(tempdir(), "fig", sep = "/")
  ggsave(paste(tmp, paste0("pie_funded_wdpa_", iso, ".png"), sep = "/"),
         plot = pie_wdpa,
         device = "png",
         height = 6, width = 9)
  ggsave(paste(tmp, paste0("pie_wdpa_ie_", iso, ".png"), sep = "/"),
         plot = pie_ie,
         device = "png",
         height = 6, width = 9)
  
  files <- list.files(tmp, full.names = TRUE)
  ##Add each file in the bucket (same foler for every file in the temp)
  for(f in files) 
  {
    cat("Uploading file", paste0("'", f, "'"), "\n")
    aws.s3::put_object(file = f,
                       bucket = paste("projet-afd-eva-ap", save_dir, iso, sep = "/"),
                       region = "", show_progress = TRUE)
  }
  do.call(file.remove, list(list.files(tmp, full.names = TRUE)))
  
  #Append the log 
  cat("#Determining Group IDs and WDPA IDs\n-> OK\n", file = log, append = TRUE)
  
  #Return the output
  list_output = list("grid.param" = grid.param, "is_ok" = TRUE)
  return(list_output)
  
    },
  
  error = function(e)
  {
    #Print the error and append the log
    print(e)
    #Append the log 
    cat(paste("#Determining Group IDs and WDPA IDs\n-> Error :\n", e, "\n"), file = log, append = TRUE)
    #Return string to inform user to skip
    return(list("is_ok" = FALSE))
  },
  
  warning = function(w)
  {
    #Print the warning and append the log
    print(w)
    #Append the log 
    cat(paste("#Determining Group IDs and WDPA IDs\n-> Warning :\n", w, "\n"), file = log, append = TRUE)
    #Return string to inform user to skip
    return(list("is_ok" = TRUE))
  }
  
  )
  
  #Return outputs
  return(output)
  
}





#Building a matching dataframe for a given country, and save it in the SSPCloud storage
##INPUTS :
### grid.param : a raster representing the gridding of the country with two layers. One for the group each pixel belongs to (funded PA, non-funded PA, potential control, buffer), the other for the WDPAID corresponding to each pixel (0 if not a PA)
### path_tmp : a temporary folder to store figures
### iso : ISO code of the country of interest
### name_output : the name of the matching frame to save
### ext_output : the file extention of the matching to save 
##OUTPUTS :
### None

fn_pre_mf_parallel = function(grid.param, path_tmp, iso, name_output, ext_output, yr_first, yr_last, log, save_dir) 
{
  output = tryCatch(
    
    {
  tic = tic()
  
  print("----Initialize portfolio")
  #Take only potential control (group = 1) and treatment (group = 2) in the country gridding
  grid.aoi = grid.param %>%
    filter(group %in% c(1,2))
  # Get input data ready for indicator calculation
  aoi = init_portfolio(grid.aoi,
                       years = yr_first:yr_last,
                       outdir = path_tmp,
                       add_resources = FALSE)
  
  #Extract a dataframe with pixels ID of grid and portfolio : useful for latter plotting of matched control and treated units
  df_gridID_assetID = aoi %>%
    st_drop_geometry() %>%
    as.data.frame() %>%
    dplyr::select(c(gridID, assetid))
  s3write_using(df_gridID_assetID,
                data.table::fwrite,
                object = paste0(save_dir, "/", iso, "/", "df_gridID_assetID_", iso, ".csv"),
                bucket = "projet-afd-eva-ap",
                opts = list("region" = ""))
  
  print("----Download Rasters")
  # Download Data

  
  dl.soil = get_resources(aoi, 
                          resources = c("soilgrids"), 
                          layers = c("clay"), # resource specific argument
                          depths = c("0-5cm"), # resource specific argument
                          stats = c("mean"))
  dl.travelT = get_resources(aoi, resources = "nelson_et_al",
                             range_traveltime = c("5k_110mio"))
  dl.tree = get_resources(aoi, resources = c("gfw_treecover", "gfw_lossyear"))
  
  # httr::set_config(httr::config(ssl_verifypeer = 0L))
  # options(download.file.method="curl", download.file.extra="--no-check-certificate")
  
  dl.elevation = get_resources(aoi, "nasa_srtm")
  
  dl.tri = get_resources(aoi, "nasa_srtm")
  
  print("----Compute indicators")
  #Compute indicators
  
  #Begin multisession
  # gc : optimize memory management for the background sessions.
  # Multisession with workers = 6 as in mapme.biodiversity tutorial : https://mapme-initiative.github.io/mapme.biodiversity/articles/quickstart.html?q=parall#enabling-parallel-computing
  
  plan(multisession, workers = 6, gc = TRUE)
  with_progress({
    get.soil %<-% {calc_indicators(dl.soil,
                                indicators = "soilproperties",
                                stats_soil = c("mean"),
                                engine = "zonal")}

    get.travelT  %<-% {calc_indicators(dl.travelT, 
                                  indicators = "traveltime",
                                  stats_accessibility = c("median"))} 
  
    get.tree  %<-% { calc_indicators(dl.tree,
                               indicators = "treecover_area",
                               min_size=1, # indicator-specific argument
                               min_cover=10)}
  
    get.elevation %<-% calc_indicators(dl.elevation,
                      indicators = "elevation",
                      stats_elevation = c("mean"))
    
    get.tri %<-% calc_indicators(dl.tri,
                      indicators = "tri")
    
    })
  
  print("----Build indicators' datasets")
  #Build indicators' datasets
  ## Transform the output dataframe into a pivot dataframe
  data.soil = unnest(get.soil, soilproperties) %>%
    mutate(across(c("mean"), \(x) round(x, 3))) %>% # Round numeric columns
    pivot_wider(names_from = c("layer", "depth", "stat"), values_from = "mean") %>%
    rename("clay_0_5cm_mean" = "clay_0-5cm_mean") %>%
    mutate(clay_0_5cm_mean = case_when(is.nan(clay_0_5cm_mean) ~ NA,
                                       TRUE ~ clay_0_5cm_mean))
  
  data.travelT = unnest(get.travelT, traveltime) %>%
    pivot_wider(names_from = "distance", values_from = "minutes_median", names_prefix = "minutes_median_") %>%
    mutate(minutes_median_5k_110mio = case_when(is.nan(minutes_median_5k_110mio) ~ NA,
                                       TRUE ~ minutes_median_5k_110mio))
  
  data.tree = unnest(get.tree, treecover_area) %>%
    drop_na(treecover) %>% #get rid of units with NA values 
    mutate(across(c("treecover"), \(x) round(x, 3))) %>% # Round numeric columns
    pivot_wider(names_from = "years", values_from = "treecover", names_prefix = "treecover_")
  
  data.tri = unnest(get.tri, tri) %>%
    mutate(tri_mean = case_when(is.nan(tri_mean) ~ NA,
                                TRUE ~ tri_mean))
  
  data.elevation = unnest(get.elevation, elevation) %>%
    mutate(elevation_mean = case_when(is.nan(elevation_mean) ~ NA,
                                TRUE ~ elevation_mean))
  
  ## End parallel plan : close parallel sessions, so must be done once indicators' datasets are built
  plan(sequential)
  
  # The calculation of tree loss area is performed at dataframe base
  # Get the column names of tree cover time series
  colnames_tree = names(data.tree)[startsWith(names(data.tree), "treecover")]
  # Drop the first year
  dropFirst = tail(colnames_tree, -1)
  # Drop the last year
  dropLast = head(colnames_tree, -1)
  # Set list of new column names for tree loss time series
  colnames_loss = dropFirst %>% str_split(., "_")
  
  # Add new columns: treeloss_tn = treecover_tn - treecover_t(n-1)  
  for (i in 1:length(dropFirst)) 
  {
    new_colname = paste0("treeloss_", colnames_loss[[i]][2]) 
    data.tree[[new_colname]] = data.tree[[dropFirst[i]]] - data.tree[[dropLast[i]]]
  }
  
  print("----Export Matching Frame")
  # Remove "geometry" column from pivot dataframes
  df.tree = data.tree %>% mutate(x = NULL) %>% as.data.frame()
  df.travelT = data.travelT %>% mutate(x = NULL) %>% as.data.frame()
  df.soil = data.soil %>% mutate(x = NULL) %>% as.data.frame()
  df.elevation = data.elevation %>% mutate(x = NULL) %>% as.data.frame()
  df.tri = data.tri %>% mutate(x=NULL) %>% as.data.frame()
  
  # Make a dataframe containing only "assetid" and geometry
  # Use data.soil instead of data.tree, as some pixels are removed in data.tree (NA values from get.tree)
  df.geom = data.soil[, c("assetid", "x")] %>% as.data.frame() 
  
  # Merge all output dataframes 
  pivot.all = Reduce(dplyr::full_join, list(df.travelT, df.soil, df.tree, df.elevation, df.tri, df.geom)) %>%
    st_as_sf()
  # pivot.all = Reduce(dplyr::full_join, list(df.travelT, df.soil, df.tree, df.geom)) %>%
  #   st_as_sf()
  # Make column Group ID and WDPA ID have data type "integer"
  pivot.all$group = as.integer(pivot.all$group)
  pivot.all$wdpaid = as.integer(pivot.all$wdpaid)
  #Add the spatial resolution of pixels
  pivot.all$res_m2 = grid.param[1, "res_m2"]
  
  name_save = paste0(name_output, "_", iso, ext_output)
  s3write_using(pivot.all,
                sf::st_write,
                object = paste0(save_dir, "/", iso, "/", name_save),
                bucket = "projet-afd-eva-ap",
                opts = list("region" = ""))
  
  #Removing files in the temporary folder
  do.call(file.remove, list(list.files(tmp_pre, include.dirs = F, full.names = T, recursive = T)))
  
  #End timer
  toc = toc()
  
  #Append the log
  cat(paste("#Calculating outcome and other covariates\n-> OK :", toc$callback_msg, "\n\n"), file = log, append = TRUE)

  #Return the output
  return(list("is_ok" = TRUE))
  
    },
  
  error = function(e)
  {
    #Print the error and append the log
    print(e)
    #Append the log 
    cat(paste("#Calculating outcome and other covariates\n-> Error :\n", e, "\n\n"), file = log, append = TRUE)
    #Return string to inform user to skip
    return(list("is_ok" = FALSE))
  }
  
  # warning = function(w)
  # {
  #   #Print the warning and append the log
  #   print(w)
  #   #Append the log 
  #   cat(paste("#Calculating outcome and other covariates\n-> Warning :\n", w, "\n"), file = log, append = TRUE)
  #   #Return string to inform user to skip
  #   return(list("is_ok" = TRUE))
  # }
  
  )
  
  return(output)
}


#####
###Post-processing
#####


#Load the matching dataframe obtained during pre-processing
##INPUTS :
### iso : the ISO code of the country considered
### name_input : name of the file to import
### ext_output : extension fo the file to import
### yr_min : the minimum for treatment year
##OUTPUTS :
### mf : matching dataframe. More precisely, it gives for each observation units in a country values of different covariates to perform matching.
fn_post_load_mf = function(iso, yr_min, name_input, ext_input, log, save_dir)
{
  output = tryCatch(
    
    {
      
  #Load the matching dataframe
  object = paste(save_dir, iso, paste0(name_input, "_", iso, ext_input), sep = "/")
  mf = s3read_using(sf::st_read,
                      bucket = "projet-afd-eva-ap",
                      object = object,
                      opts = list("region" = "")) 
  
  #Subset to control and treatment units with year of treatment >= yr_min
  mf = mf %>%
    #Remove PAs non-funded by AFD and buffers
    filter(group==1 | group==2) %>%
    #Remove observations with NA values only for covariates (except for status_yr, region_afd, year_funding_first which are NA for control units)
    drop_na(-c(status_yr, year_funding_first, year_funding_all, region_afd, region, sub_region, iso3, country_en, res_m2)) #%>%
     #filter(status_yr >= yr_min | is.na(status_yr))
  
  #Write the list of PAs matched
  list_pa = mf %>%
    st_drop_geometry() %>%
    as.data.frame() %>%
    dplyr::select(c(region_afd, region, sub_region, country_en, iso3, wdpaid, status_yr, year_funding_first, year_funding_all)) %>%
    mutate(iso3 = iso, .before = "wdpaid") %>%
    filter(wdpaid != 0) %>%
    group_by(wdpaid) %>%
    slice(1) %>%
    ungroup()
  
  s3write_using(list_pa,
                data.table::fwrite,
                bucket = "projet-afd-eva-ap",
                object = paste(save_dir, iso, paste0("list_pa_matched_", iso, ".csv"), sep = "/"),
                opts = list("region" = ""))
  
  #Append the log
  cat("Loading the matching frame -> OK\n", file = log, append = TRUE)
  
  #Return output
  return(list("mf" = mf, "is_ok" = TRUE))
  
    },
  
  error = function(e)
  {
    print(e)
    cat(paste("Error in loading the matching frame :\n", e, "\n"), file = log, append = TRUE)
    return(list("is_ok" = FALSE))
  }
  
  # warning = function(w)
  # {
  #   #Print the warning and append the log
  #   print(w)
  #   #Append the log 
  #   cat(paste("Warining while loading the matching frame :\n", w, "\n"), file = log, append = TRUE)
  #   #Return string to inform user to skip
  #   return(list("is_ok" = TRUE))
  # }
  
  
  )
  
  return(output)
}


#Compute average forest loss before funding, and add it to the matching frame as a covariate
##INPUTS : 
### mf : the matching dataframe
### yr_start : the first year of the period to compute average loss
### yr_end : the last year of the period to compute average loss
### colfl.prefix : name of the forest loss variable
### colname.flAvg : name of the average forest loss variable
## OUTPUTS :
### mf : matching frame with the new covariate

fn_post_avgLoss_prefund = function(mf, colfl.prefix, colname.flAvg, log)
{
  
  output = tryCatch(
    
    {
      
  #OLD
  # Columns treeloss, without geometry
  # start = yr_start - 2000
  # end = yr_end - 2000
  # df_fl = mf[grepl(colfl.prefix, names(mf))][start:end] %>% 
  #   st_drop_geometry()
  # # Add column: average treeloss before funding starts, 
  # mf$avgLoss_pre_fund = round(rowMeans(df_fl), 2)
      
  #Extract spatial resolution : can be useful to compute forest loss in percentage for instance
  #res_m2 = mf[1, "res_m2"]

  #Extract treatment year
  treatment.year = mf %>% 
    filter(group == 2) %>% 
    slice(1)
  treatment.year = treatment.year$status_yr
  
  #Extract first year treeloss is computed
  ##Select cols with "treeloss" in mf, drop geometry, replace "treeloss_" by "", convert to num and take min
  treeloss.ini.year = mf[grepl(colfl.prefix, names(mf))] %>%
    st_drop_geometry() %>%
    names() %>%
    gsub(paste0(colfl.prefix, "_"), "", .) %>%
    as.numeric() %>%
    min()
  
  #Define period to compute average loss
  ##If 5 pre-treatment periods are available at least, then average pre-treatment deforestation is computed on this 5 years range
  ## If less than 5 are available, compute on this restricted period
  ## Note that by construction, treatment.year >= treeloss.ini.year +1 (as yr_min = yr_first+2 in the parameters)
  if((treatment.year-treeloss.ini.year) >=5)
  {yr_start = (treatment.year)-5
  yr_end = (treatment.year)-1} else if((treatment.year-treeloss.ini.year <5) & (treatment.year-treeloss.ini.year >0))
  {yr_start = treeloss.ini.year
  yr_end = (treatment.year)-1} 
  #Transform it in variable suffix
  var_start = yr_start - 2000
  var_end = yr_end - 2000
  #Select only relevant variables
  df_fl = mf[grepl(colfl.prefix, names(mf))][var_start:var_end] %>% 
    st_drop_geometry()
  #Compute average loss for each pixel and store it in mf. Also add the start and end years of pre-treatment period where average loss is computed.
  mf$avgLoss_pre_fund = round(rowMeans(df_fl), 2)
  mf$start_pre_fund = yr_start
  mf$end_pre_fund = yr_end
  #Remove NA values
  mf = mf %>% drop_na(avgLoss_pre_fund)
  
  #Append the log
  cat("#Add average pre-treatment treecover loss\n-> OK\n", file = log, append = TRUE)
  
  #Return output
  return(list("mf" = mf, "is_ok" = TRUE))
  
    },
  
  error = function(e)
  {
    print(e)
    cat(paste("#Add average pre-treatment treecover loss\n-> Error :\n", e, "\n"), file = log, append = TRUE)
    return(list("is_ok" = FALSE))
  }
  
  # warning = function(w)
  # {
  #   #Print the warning and append the log
  #   print(w)
  #   #Append the log 
  #   cat(paste("#Add average pre-treatment treecover loss\n-> Warning :\n", w, "\n"), file = log, append = TRUE)
  #   #Return string to inform user to skip
  #   return(list("is_ok" = TRUE))
  # }
  
  )
  
  return(output)
}


  
fn_post_match_auto = function(mf,
                              iso,
                              dummy_int,
                              th_mean, 
                              th_var_min, th_var_max,
                              colname.travelTime, colname.clayContent, colname.elevation, colname.tri, colname.fcIni, colname.flAvg,
                              log)
{

  #Append the log file : CEM step
  cat("#Run Coarsened Exact Matching\n", 
      file = log, append = TRUE)
  
  ## Matching handling errors due to absence of matching
  output = 
    tryCatch(
    {
      # Formula
      formula = eval(bquote(group ~ .(as.name(colname.travelTime)) 
                            + .(as.name(colname.clayContent))  
                            +  .(as.name(colname.fcIni)) 
                            + .(as.name(colname.flAvg))
                            + .(as.name(colname.tri))
                            + .(as.name(colname.elevation))))
      
      #Try to perform matching
      out.cem = matchit(formula,
                        data = mf,
                        method = "cem",
                        cutpoints = "sturges",
                        k2k = TRUE,
                        k2k.method = "mahalanobis")
      
      ## Covariate balance : standardized mean difference and variance ratio
      ## For both tests and the joint one, a dummy variable is defined, with value TRUE is the test is passed
      df.cov.m = summary(out.cem, interactions = dummy_int)$sum.matched %>%
        as.data.frame() %>%
        clean_names() %>%
        mutate(is_var_ok = var_ratio < th_var_max & var_ratio > th_var_min, #Check variance ratio between treated and controls
               is_mean_ok = abs(std_mean_diff) < th_mean, #Check absolute standardized mean difference
               is_bal_ok = as.logical(is_var_ok*is_mean_ok), #Binary : TRUE if both variance and mean difference check pass, 0 if at least one does not
               .after = "std_mean_diff")
      
      #Add a warning if covariate balance tests are not passed
      if(sum(df.cov.m$is_bal_ok) < nrow(df.cov.m) | is.na(sum(df.cov.m$is_bal_ok)) == TRUE)
      {
        message("Matched control and treated units are not balanced enough. Increase sample size, turn to less restrictive tests or visually check balance.")
        cat("-> Careful : matched control and treated units are not balanced enough. Increase sample size, turn to less restrictive tests or visually check balance.\n", 
            file = log, append = TRUE)
      }
      
      #Append the log : note the step has already been appended at the beginning of the function
      cat("-> OK\n", file = log, append = TRUE)
      
      return(list("out.cem" = out.cem, "df.cov.m" = df.cov.m, "is_ok" = TRUE))
      
    },
    
    error=function(e)
    {
      print(e)
      cat(paste("-> Error :\n", e, "\n"), file = log, append = TRUE)
      return(list("is_ok" = FALSE))
    },
    
    warning = function(w)
    {
      #Print the warning and append the log
      #Append the log 
      cat(paste("-> Warning :\n", w, "\n"),
          file = log, append = TRUE)
      return(list("is_ok" = FALSE)) #Here warning comes from an absence of matching : thus must skip to next country
    }
    
  )
  
  return(output)
  
}
    


#Plot covariates balance (plots and summary table)
## INPUTS :
### out.cem : list of results from the CEM matching
### iso : ISO code of the country considered
### path_tmp : temporary folder to store figures
### colname.XXX : name of the XXX covariate in the matching frame
## OUTPUTS :
### None

fn_post_covbal = function(out.cem, mf, 
                          colname.travelTime, colname.clayContent, colname.fcIni, colname.flAvg, colname.tri, colname.elevation, 
                          iso, path_tmp, wdpaid, log,
                          save_dir)
{
  
  output = tryCatch(
    
    {
      
  #Save summary table from matching
  smry_cem = summary(out.cem)
  tbl_cem_nn = smry_cem$nn
  tbl_cem_m = smry_cem$sum.matched
  tbl_cem_all = smry_cem$sum.all
  
  #Extract country name
  country.name = mf %>% 
    filter(group == 2) %>% 
    slice(1)
  country.name = country.name$country_en
  
  #Extract start and end years of pre-treatment period where average loss is computed
  year.start.prefund = mf %>%
    filter(group == 2) %>% 
    slice(1)
  year.start.prefund = year.start.prefund$start_pre_fund
  
  year.end.prefund = mf %>%
    filter(group == 2) %>% 
    slice(1)
  year.end.prefund = year.end.prefund$end_pre_fund
  
  #Plot covariate balance
  colname.flAvg.new = paste0("Avg. Annual Forest \n Loss ",  year.start.prefund, "-", year.end.prefund)
  c_name = data.frame(old = c(colname.travelTime, colname.clayContent, colname.tri, colname.elevation,
                              colname.fcIni, colname.flAvg),
                      new = c("Accessibility", "Clay Content", "Terrain Ruggedness Index (TRI)", "Elevation (m)", "Forest Cover in 2000",
                              colname.flAvg.new))
  
  # Refer to cobalt::love.plot()
  # https://cloud.r-project.org/web/packages/cobalt/vignettes/cobalt.html#love.plot
  fig_covbal = love.plot(out.cem, 
                       binary = "std", 
                       abs = TRUE,
                       #thresholds = c(m = .1),
                       var.order = "unadjusted",
                       var.names = c_name,
                       title = paste0("Covariate balance for WDPA ID ", wdpaid, " in ", country.name),
                       sample.names = c("Discarded", "Selected"),
                       wrap = 25 # at how many characters does axis label break to new line
  )
  # Finetune Layouts using ggplot
  fig_covbal + 
    geom_vline(aes(xintercept=0.1, linetype="Acceptable \n Balance \n (x=0.1)"), color=c("#2ecc71"), linewidth=0.35) +
    theme_bw() +
    theme(
      plot.title = element_text(family="Arial Black", size=16, hjust=0.5),
      
      legend.title = element_blank(),
      legend.text=element_text(size=14),
      legend.spacing.x = unit(0.5, 'cm'),
      legend.spacing.y = unit(0.75, 'cm'),
      
      axis.text.x = element_text(angle = 20, hjust = 0.5, vjust = 0.5),
      axis.text=element_text(size=12),
      axis.title=element_text(size=14),
      axis.title.y = element_text(margin = margin(unit = 'cm', r = 0.5)),
      axis.title.x = element_text(margin = margin(unit = 'cm', t = 0.5)),
      
      panel.grid.major.x = element_line(color = 'grey', linewidth = 0.3, linetype = 1),
      panel.grid.minor.x = element_line(color = 'grey', linewidth = 0.3, linetype = 2)
    ) + guides(linetype = guide_legend(override.aes = list(color = "#2ecc71"))) # Add legend for geom_vline
  

  #Saving files
  
  ggsave(paste0(path_tmp, "/CovBal/fig_covbal", "_", iso, "_", wdpaid, ".png"),
         plot = fig_covbal,
         device = "png",
         height = 6, width = 9)
  print(xtable(tbl_cem_nn, type = "latex"),
        file = paste0(path_tmp, "/CovBal/tbl_cem_nn", "_", iso, "_", wdpaid, ".tex"))
  print(xtable(tbl_cem_m, type = "latex"),
        file = paste0(path_tmp, "/CovBal/tbl_cem_m", "_", iso, "_", wdpaid, ".tex"))
  print(xtable(tbl_cem_all, type = "latex"),
        file = paste0(path_tmp, "/CovBal/tbl_cem_all", "_", iso, "_", wdpaid, ".tex"))
  
  #Export to S3 storage
  ##List of files to save in the temp folder
  files <- list.files(paste(path_tmp, "CovBal", sep = "/"), full.names = TRUE)
  ##Add each file in the bucket (same foler for every file in the temp)
  for(f in files) 
  {
    cat("Uploading file", paste0("'", f, "'"), "\n")
    aws.s3::put_object(file = f, 
                       bucket = paste("projet-afd-eva-ap", save_dir, iso, wdpaid, sep = "/"),
                       region = "", show_progress = TRUE)
  }
  do.call(file.remove, list(list.files(paste(path_tmp, "CovBal", sep = "/"), full.names = TRUE)))
  
  #Append the log
  cat("#Plot covariates balance\n->OK\n", file = log, append = TRUE)
  
  return(list("is_ok" = TRUE))
  
    },
  
  error=function(e)
  {
    print(e)
    cat(paste("#Plot covariates balance\n-> Error :\n", e, "\n"), file = log, append = TRUE)
    return(list("is_ok" = FALSE))
  }
  
  # warning = function(w)
  # {
  #   #Print the warning and append the log
  #   print(w)
  #   #Append the log 
  #   cat(paste("#Plot covariates balance\n-> Warning :\n", w, "\n"), file = log, append = TRUE)
  #   #Return string to inform user to skip
  #   return(list("is_ok" = TRUE))
  # }
  
  )
  
  return(output)

}

#Plot the distribution of covariates for control and treatment units, before and after matching
## INPUTS :
### out.cem : list of results from the CEM matching
### iso : ISO code of the country considered
### path_tmp : temporary folder to store figures
### colname.XXX : name of the XXX covariate in the matching frame
## OUTPUTS :
### None

fn_post_plot_density = function(out.cem, mf, 
                                colname.travelTime, colname.clayContent, colname.fcIni, colname.flAvg, colname.tri, colname.elevation, 
                                iso, path_tmp, wdpaid, log, save_dir)
{
  output = tryCatch(
    
    {
      
  # Define Facet Labels
  fnl = c(`Unadjusted Sample` = "Before Matching",
          `Adjusted Sample` = "After Matching")
  
  #Extract country name
  country.name = mf %>% 
    filter(group == 2) %>% 
    slice(1)
  country.name = country.name$country_en
  
  #Define plots
  ## Density plot for Travel Time
  fig_travel = bal.plot(out.cem, 
                      var.name = colname.travelTime,
                      #sample.names = c("Control", "Treatment"),
                      which = "both") +
    facet_wrap(.~which, labeller = as_labeller(fnl)) +
    #scale_fill_viridis(discrete = T) +
    scale_fill_manual(labels = c("Control", "Treatment"), values = c("#f5b041","#5dade2")) +
    labs(title = "Distributional balance for accessibility",
         subtitle = paste0("Protected area in ", country.name, ", WDPAID ", wdpaid),
         x = "Accessibility (min)",
         fill = "Group") +
    theme_bw() +
    theme(
      plot.title = element_text(family="Arial Black", size=16, hjust = 0),
      
      legend.title = element_blank(),
      legend.text=element_text(size=14),
      legend.spacing.x = unit(0.5, 'cm'),
      legend.spacing.y = unit(0.75, 'cm'),
      
      axis.text=element_text(size=12),
      axis.title=element_text(size=14),
      axis.title.y = element_text(margin = margin(unit = 'cm', r = 0.5)),
      axis.title.x = element_text(margin = margin(unit = 'cm', t = 0.5)),
      
      strip.text.x = element_text(size = 12) # Facet Label
    )
  
  ## Density plot for Clay Content
  fig_clay = bal.plot(out.cem, 
                    var.name = colname.clayContent,
                    which = "both") +
    facet_wrap(.~which, labeller = as_labeller(fnl)) +
    #scale_fill_viridis(discrete = T) +
    scale_fill_manual(labels = c("Control", "Treatment"), values = c("#f5b041","#5dade2")) +
    labs(title = "Distributional balance for clay content",
         subtitle = paste0("Protected area in ", country.name, ", WDPAID ", wdpaid),
         x = "Clay content at 0~20cm soil depth (%)",
         fill = "Group") +
    theme_bw() +
    theme(
      plot.title = element_text(family="Arial Black", size=16, hjust=0),
      
      legend.title = element_blank(),
      legend.text=element_text(size=14),
      legend.spacing.x = unit(0.5, 'cm'),
      legend.spacing.y = unit(0.75, 'cm'),
      
      axis.text=element_text(size=12),
      axis.title=element_text(size=14),
      axis.title.y = element_text(margin = margin(unit = 'cm', r = 0.5)),
      axis.title.x = element_text(margin = margin(unit = 'cm', t = 0.5)),
      
      strip.text.x = element_text(size = 12) # Facet Label
    )
  
  ## Density plot for Elevation
  fig_elevation = bal.plot(out.cem,
                      var.name = colname.elevation,
                      which = "both") +
      facet_wrap(.~which, labeller = as_labeller(fnl)) +
      #scale_fill_viridis(discrete = T) +
      scale_fill_manual(labels = c("Control", "Treatment"), values = c("#f5b041","#5dade2")) +
      labs(title = "Distributional balance for elevation",
           subtitle = paste0("Protected area in ", country.name, ", WDPAID ", wdpaid),
           x = "Elevation (m)",
           fill = "Group") +
      theme_bw() +
      theme(
          plot.title = element_text(family="Arial Black", size=16, hjust=0),
          legend.title = element_blank(),
          legend.text=element_text(size=14),
          legend.spacing.x = unit(0.5, 'cm'),
          legend.spacing.y = unit(0.75, 'cm'),

          axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          axis.title.y = element_text(margin = margin(unit = 'cm', r = 0.5)),
          axis.title.x = element_text(margin = margin(unit = 'cm', t = 0.5)),

          strip.text.x = element_text(size = 12) # Facet Label
      )
  
  ## Density plot for TRI
  fig_tri = bal.plot(out.cem,
                      var.name = colname.tri,
                      which = "both") +
      facet_wrap(.~which, labeller = as_labeller(fnl)) +
      #scale_fill_viridis(discrete = T) +
      scale_fill_manual(labels = c("Control", "Treatment"), values = c("#f5b041","#5dade2")) +
      labs(title = "Distributional balance for Terrain Ruggedness Index (TRI)",
          subtitle = paste0("Protected area in ", country.name, ", WDPAID ", wdpaid),
           x = "TRI",
           fill = "Group") +
      theme_bw() +
      theme(
          plot.title = element_text(family="Arial Black", size=16, hjust=0),

          legend.title = element_blank(),
          legend.text=element_text(size=14),
          legend.spacing.x = unit(0.5, 'cm'),
          legend.spacing.y = unit(0.75, 'cm'),

          axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          axis.title.y = element_text(margin = margin(unit = 'cm', r = 0.5)),
          axis.title.x = element_text(margin = margin(unit = 'cm', t = 0.5)),

          strip.text.x = element_text(size = 12) # Facet Label
      )
  
  ## Density plot for covariate "forest cover 2000"
  fig_fc = bal.plot(out.cem, 
                  var.name = colname.fcIni,
                  which = "both") +
    facet_wrap(.~which, labeller = as_labeller(fnl)) +
    scale_fill_manual(labels = c("Control", "Treatment"), values = c("#f5b041","#5dade2")) +
    # scale_x_continuous(trans = "log10") +
    labs(title = "Distributional balance for forest cover in 2000",
         subtitle = paste0("Protected area in ", country.name, ", WDPAID ", wdpaid),
         x = "Forest cover (ha)",
         fill = "Group") +
    theme_bw() +
    theme(
      plot.title = element_text(family="Arial Black", size=16, hjust=0),
      
      legend.title = element_blank(),
      legend.text=element_text(size=14),
      legend.spacing.x = unit(0.5, 'cm'),
      legend.spacing.y = unit(0.75, 'cm'),
      
      axis.text=element_text(size=12),
      axis.title=element_text(size=14),
      axis.title.y = element_text(margin = margin(unit = 'cm', r = 0.5)),
      axis.title.x = element_text(margin = margin(unit = 'cm', t = 0.5)),
      
      strip.text.x = element_text(size = 12) # Facet Label
    )
  
  ## Density plot for covariate "avg. annual forest loss prior funding"
  fig_fl = bal.plot(out.cem, 
                  var.name = colname.flAvg,
                  which = "both") +
    facet_wrap(.~which, labeller = as_labeller(fnl)) +
    #scale_fill_viridis(discrete = T) +
    scale_fill_manual(labels = c("Control", "Treatment"), values = c("#f5b041","#5dade2")) +
    labs(title = "Distributional balance for former average forest loss",
         subtitle = paste0("Protected area in ", country.name, ", WDPAID ", wdpaid),
         x = "Forest loss (%)",
         fill = "Group") +
    theme_bw() +
    theme(
      plot.title = element_text(family="Arial Black", size=16, hjust=0.5),
      legend.title = element_blank(),
      legend.text=element_text(size=14),
      legend.spacing.x = unit(0.5, 'cm'),
      legend.spacing.y = unit(0.75, 'cm'),
      
      axis.text=element_text(size=12),
      axis.title=element_text(size=14),
      axis.title.y = element_text(margin = margin(unit = 'cm', r = 0.5)),
      axis.title.x = element_text(margin = margin(unit = 'cm', t = 0.5)),
      
      strip.text.x = element_text(size = 12) # Facet Label
    )
  
  #Saving plots
  
  tmp = paste(tempdir(), "fig", sep = "/")
  ggsave(paste(tmp, paste0("fig_travel_dplot_", iso, "_", wdpaid, ".png"), sep = "/"),
         plot = fig_travel,
         device = "png",
         height = 6, width = 9)
  ggsave(paste(tmp, paste0("fig_clay_dplot_", iso, "_", wdpaid, ".png"), sep = "/"),
         plot = fig_clay,
         device = "png",
         height = 6, width = 9)
  ggsave(paste(tmp, paste0("fig_elevation_dplot_", iso, "_", wdpaid, ".png"), sep = "/"),
         plot = fig_elevation,
         device = "png",
         height = 6, width = 9)
  ggsave(paste(tmp, paste0("fig_tri_dplot_", iso, "_", wdpaid, ".png"), sep = "/"),
         plot = fig_tri,
         device = "png",
         height = 6, width = 9)
  ggsave(paste(tmp, paste0("fig_fc_dplot_", iso, "_", wdpaid, ".png"), sep = "/"),
         plot = fig_fc,
         device = "png",
         height = 6, width = 9)
  ggsave(paste(tmp, paste0("fig_fl_dplot_", iso, "_", wdpaid, ".png"), sep = "/"),
         plot = fig_fl,
         device = "png",
         height = 6, width = 9)
  
  files <- list.files(tmp, full.names = TRUE)
  ##Add each file in the bucket (same foler for every file in the temp)
  for(f in files) 
  {
    cat("Uploading file", paste0("'", f, "'"), "\n")
    aws.s3::put_object(file = f, 
                       bucket = paste("projet-afd-eva-ap", save_dir, iso, wdpaid, sep = "/"),
                       region = "", show_progress = TRUE)
  }
  do.call(file.remove, list(list.files(tmp, full.names = TRUE)))
  
  #Append the log
  cat("#Plot covariates density\n->OK\n", file = log, append = TRUE)
  
  return(list("is_ok" = TRUE))
  
    },
  
  error=function(e)
  {
    print(e)
    cat(paste("#Plot covariates density\n-> Error :\n", e, "\n"), file = log, append = TRUE)
    return(list("is_ok" = FALSE))
  }
  
  # warning = function(w)
  # {
  #   #Print the warning and append the log
  #   print(w)
  #   #Append the log 
  #   cat(paste("#Plot covariates density\n-> Warning :\n", w, "\n"), file = log, append = TRUE)
  #   #Return string to inform user to skip
  #   return(list("is_ok" = TRUE))
  # }
  
  )
  
  return(output)
  
}

#Define panel datasets (long, wide) for control and treatment observation units, before and after matching.
## INPUTS :
### out.cem : list of results from the CEM matching
### mf : the matching dataframe
### colfc.prefix : prefix of columns for forest cover
### colfc.bind : separation between prefix and year
## OUTPUTS :
### list_output : a list of dataframes : (un)matched.wide/long. They contain covariates and outcomes for treatment and control units, before and after matching, in a wide or long format

fn_post_panel = function(out.cem, mf, colfc.prefix, colfc.bind, ext_output, wdpaid, iso, log, save_dir)
{
  
  output = tryCatch(
    
    {
      
  # Convert dataframe of matched objects to pivot wide form
  matched.wide = match.data(object=out.cem, data=mf)
  
  # Pivot Wide ==> Pivot Long
  matched.long = matched.wide %>%
    dplyr::select(c(region_afd, region, sub_region, iso3, group, wdpaid, status_yr, year_funding_first, year_funding_all, assetid, weights, starts_with(colfc.prefix), res_m2)) %>%
    pivot_longer(cols = c(starts_with(colfc.prefix)),
                 names_to = c("var", "year"),
                 names_sep = colfc.bind,
                 values_to = "fc_ha")
  
  # Pivot wide Dataframe of un-matched objects
  unmatched.wide = mf
  
  # Pivot Wide ==> Pivot Long
  unmatched.long = unmatched.wide %>%
    dplyr::select(c(region_afd, region, sub_region, iso3, group, wdpaid, status_yr, year_funding_first, year_funding_all, assetid, starts_with(colfc.prefix), res_m2)) %>%
    pivot_longer(cols = c(starts_with(colfc.prefix)),
                 names_to = c("var", "year"),
                 names_sep = colfc.bind,
                 values_to = "fc_ha")
  
  #Save the dataframes
  s3write_using(matched.wide,
                sf::st_write,
                object = paste0(save_dir, "/", iso, "/", wdpaid, "/", paste0("matched_wide", "_", iso, "_", wdpaid, ext_output)),
                bucket = "projet-afd-eva-ap",
                opts = list("region" = ""))
  s3write_using(unmatched.wide,
                sf::st_write,
                object = paste0(save_dir, "/", iso, "/", wdpaid, "/", paste0("unmatched_wide", "_", iso, "_", wdpaid, ext_output)),
                bucket = "projet-afd-eva-ap",
                opts = list("region" = ""))
  s3write_using(matched.long,
                sf::st_write,
                object = paste0(save_dir, "/", iso, "/", wdpaid, "/", paste0("matched_long", "_", iso, "_", wdpaid, ext_output)),
                bucket = "projet-afd-eva-ap",
                opts = list("region" = ""))
  s3write_using(unmatched.long,
                sf::st_write,
                object = paste0(save_dir, "/", iso, "/", wdpaid, "/", paste0("unmatched_long", "_", iso, "_", wdpaid, ext_output)),
                bucket = "projet-afd-eva-ap",
                opts = list("region" = ""))
  
  #Append the log
  cat("#Panelize dataframe\n-> OK\n", file = log, append = TRUE)
  
  #Return outputs
  list_output = list("matched.wide" = matched.wide, "matched.long" = matched.long,
                     "unmatched.wide" = unmatched.wide, "unmatched.long" = unmatched.long,
                     "is_ok" = TRUE)
  return(list_output)
  
    },
  
  error=function(e)
{
  print(e)
  cat(paste("#Panelize dataframe\n-> Error :\n", e, "\n"), file = log, append = TRUE)
  return(list("is_ok" = FALSE))
}

# warning = function(w)
# {
#   #Print the warning and append the log
#   print(w)
#   #Append the log 
#   cat(paste("#Panelize dataframe\n-> Warning :\n", w, "\n"), file = log, append = TRUE)
#   #Return string to inform user to skip
#   return(list("is_ok" = TRUE))
# }

  )
  
  return(output)
  
}

#Plot the average trend of control and treated units in a given country, before and after the matching
## INPUTS :
### (un)matched.long : dataframe with covariates and outcomes for each treatment and control unit, before and after matching, in a long format (one row : pixel+year)
## OUTPUTS : 
### None

fn_post_plot_trend = function(matched.long, unmatched.long, mf, iso, wdpaid, log, save_dir) 
{
  
  output = tryCatch(
    
    {
  # Make dataframe for plotting Trend
  df.matched.trend = matched.long %>%
    group_by(group, year) %>%
    summarise(avgFC = mean(fc_ha, na.rm=TRUE), n = n(), matched = TRUE)
  
  df.unmatched.trend = unmatched.long %>%
    group_by(group, year) %>%
    summarise(avgFC = mean(fc_ha, na.rm=TRUE), n = n(), matched = FALSE)
  
  df.trend = rbind(df.matched.trend, df.unmatched.trend)
  
  #Extract spatial resolution of pixels
  res_m2 = mf[1, "res_m2"]
  
  #Extract treatment year
  treatment.year = mf %>% 
    filter(group == 2) %>% 
    slice(1)
  treatment.year = treatment.year$status_yr
  
  #Extract funding years
  funding.years = mf %>% 
    filter(group == 2) %>% 
    slice(1)
  funding.years = funding.years$year_funding_first
  #funding.years = as.numeric(unlist(strsplit(funding.years$year_funding_all, split = ",")))
  
  #Extract country name
  country.name = mf %>% 
    filter(group == 2) %>% 
    slice(1)
  country.name = country.name$country_en
  
  #Plot
  ## Change Facet Labels
  fct.labs <- c("Before Matching", "After Matching")
  names(fct.labs) <- c(FALSE, TRUE)
  
  ## Trend Plot for unmatched data
  fig_trend_unm = ggplot(data = df.trend, aes(x = year, y = avgFC)) +
    geom_line(aes(group = group, color = as.character(group))) +
    geom_point(aes(color = as.character(group))) +
    geom_vline(aes(xintercept=as.character(treatment.year), size="Treatment year"), linetype=1, linewidth=0.5, color="orange") +
    geom_vline(aes(xintercept=as.character(funding.years), size="Funding year"), linetype=2, linewidth=0.5, color="grey30") +
    scale_x_discrete(breaks=seq(2000,2020,5), labels=paste(seq(2000,2020,5))) +
    scale_color_hue(labels = c("Control", "Treatment")) +
    facet_wrap(matched~., ncol = 2, #scales = 'free_x',
               labeller = labeller(matched = fct.labs)) +
    labs(title = "Evolution of forest cover",
         subtitle = paste0("Protected area in ", country.name, ", WDPAID ", wdpaid),
         x = "Year", y = "Average forest cover (ha)", color = "Group") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = -20, hjust = 0.5, vjust = 0.5),
      axis.text=element_text(size=11),
      axis.title=element_text(size=14),
      
      #legend.position = "bottom",
      legend.title = element_blank(),
      legend.text=element_text(size=14),
      #legend.spacing.x = unit(1.0, 'cm'),
      legend.spacing.y = unit(0.75, 'cm'),
      legend.key.size = unit(2, 'line'),
      
      
      panel.grid.major.x = element_line(color = 'grey', linewidth = 0.3, linetype = 1),
      panel.grid.minor.x = element_line(color = 'grey', linewidth = 0.2, linetype = 2),
      panel.grid.major.y = element_line(color = 'grey', linewidth = 0.3, linetype = 1),
      panel.grid.minor.y = element_line(color = 'grey', linewidth = 0.2, linetype = 2),
      
      strip.text.x = element_text(size = 12) # Facet Label
      ) +
    guides(size = guide_legend(override.aes = list(color = c("grey30", "orange")))) # Add legend for geom_vline

  fig_trend_unm
  # Trend Plot for matched data
  fig_trend_m = ggplot(df.matched.trend, aes(x = year, y = avgFC)) +
    geom_line(aes(group = group, color = as.character(group))) +
    geom_point(aes(color = as.character(group))) +
    geom_vline(aes(xintercept=as.character(treatment.year), size="Treatment year"), linetype=1, linewidth=0.5, color="orange") +
    geom_vline(aes(xintercept=as.character(funding.years), size="Funding year(s)"), linetype=2, linewidth=0.5, color="grey30") +
    #scale_y_continuous(breaks=seq(0,100,10), labels=paste(seq(0,100,10)),
    #                   expand=c(0,0), limits=c(0,100)) +
    scale_x_discrete(breaks=seq(2000,2020,5), labels=paste(seq(2000,2020,5))) +
    scale_color_hue(labels = c("Control", "Treatment")) +
    
    labs(title = "Evolution of forest cover",
         subtitle = paste0("Protected area in ", country.name, ", WDPA ID ", wdpaid),
         x = "Year", y = "Average forest cover (ha)", color = "Group") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = -20, hjust = 0.5, vjust = 0.5),
      axis.text=element_text(size=11),
      axis.title=element_text(size=14),
      
      #legend.position = "bottom",
      legend.title = element_blank(),
      legend.text=element_text(size=14),
      #legend.spacing.x = unit(1.0, 'cm'),
      legend.spacing.y = unit(0.75, 'cm'),
      legend.key.size = unit(2, 'line'),
      
      panel.grid.major.x = element_line(color = 'grey', linewidth = 0.3, linetype = 1),
      panel.grid.minor.x = element_line(color = 'grey', linewidth = 0.2, linetype = 2),
      panel.grid.major.y = element_line(color = 'grey', linewidth = 0.3, linetype = 1),
      panel.grid.minor.y = element_line(color = 'grey', linewidth = 0.2, linetype = 2),
      
    ) + guides(size = guide_legend(override.aes = list(color = c("grey30", "orange")))) # Add legend for geom_vline
  
  ##Saving plots
  tmp = paste(tempdir(), "fig", sep = "/")
  
  ggsave(paste(tmp, paste0("fig_trend_unmatched_", iso, "_", wdpaid, ".png"), sep = "/"),
         plot = fig_trend_unm,
         device = "png",
         height = 6, width = 9)
  ggsave(paste(tmp, paste0("fig_trend_matched_", iso, "_", wdpaid, ".png"), sep = "/"),
         plot = fig_trend_m,
         device = "png",
         height = 6, width = 9)
  
  files <- list.files(tmp, full.names = TRUE)
  ##Add each file in the bucket (same foler for every file in the temp)
  for(f in files) 
  {
    cat("Uploading file", paste0("'", f, "'"), "\n")
    aws.s3::put_object(file = f, 
                       bucket = paste("projet-afd-eva-ap", save_dir, iso, wdpaid, sep = "/"),
                       region = "", show_progress = TRUE)
  }
  do.call(file.remove, list(list.files(tmp, full.names = TRUE)))
  
  #Append the log
  cat("#Plot matched and unmatched trends\n-> OK\n\n", file = log, append = TRUE)
  
  return(list("is_ok" = TRUE))
  
    },
  
  error=function(e)
  {
    print(e)
    cat(paste("#Plot matched and unmatched trends\n-> Error :\n", e, "\n\n"), file = log, append = TRUE)
    return(list("is_ok" = FALSE))
  }
  
  # warning = function(w)
  # {
  #   #Print the warning and append the log
  #   print(w)
  #   #Append the log 
  #   cat(paste("#Plot matched and unmatched trends\n-> Warning :\n", w, "\n"), file = log, append = TRUE)
  #   #Return string to inform user to skip
  #   return(list("is_ok" = TRUE))
  # }
  
  )
  return(output)
  
}

# Plot the country grid with matched control and treated
##INPUTS
### iso : the ISO3 code of the country considered
### wdpaid : the WDPA ID of the PA considered
### is_pa : logical, whether the plotted grid is for a unique PA or all the PAs in the country considered
### df_pix_matched : dataframe with ID of matched pixels (ID from mapme.biodiversity portfolio)
##OUTPUTS
### None (plots)

fn_post_plot_grid = function(iso, wdpaid, is_pa, df_pix_matched, path_tmp, log, save_dir)
{
  
  output = tryCatch(
    
    {
      
  #Import dataframe where each pixel in the grid has both its grid ID and asset ID from the portfolio creation
  df_gridID_assetID = s3read_using(data.table::fread,
                                   object = paste0(save_dir, "/", iso, "/", paste0("df_gridID_assetID_", iso, ".csv")),
                                   bucket = "projet-afd-eva-ap",
                                   opts = list("region" = ""))
  
  #Importing the gridding of the country (funded and analyzed PAs, funded not analyzed PAs, non-funded PAs, buffer, control)
  #Merge with a dataframe so that each pixel in the grid has both its grid ID and asset ID from the portfolio creation
  #Merge with matched pixels dataframe
  grid =  s3read_using(sf::read_sf,
                       object = paste0(save_dir, "/", iso, "/", paste0("grid_param_", iso, ".gpkg")),
                       bucket = "projet-afd-eva-ap",
                       opts = list("region" = "")) %>%
    left_join(df_gridID_assetID, by = "gridID") %>%
    left_join(df_pix_matched, by = "assetid") %>%
    mutate(group_plot = case_when(group_matched == 1 ~ "Control (matched)",
                                  group_matched == 2 ~ "Treatment (matched)",
                                  TRUE ~ group_name))
  
  #Extract country name
  country.name = grid %>% 
    filter(group == 2) %>% 
    slice(1)
  country.name = country.name$country_en
  
  # Visualize and save grouped grid cells
  fig_grid = 
    ggplot(grid) +
    #The original gridding as a first layer
    geom_sf(aes(fill = as.factor(group_plot)), color = NA) +
    scale_fill_brewer(name = "Group", type = "qual", palette = "BrBG", direction = 1) +
    labs(title = paste("Gridding of", country.name, ": matched units"),
         subtitle = ifelse(is_pa == TRUE,
                            yes = paste("Focus on WDPAID", wdpaid),
                            no = "All protected areas analyzed")) +
    theme_bw()
  
  fig_save = ifelse(is_pa == TRUE,
                    yes = paste0(path_tmp, "/fig_grid_group_", iso, "_matched_", wdpaid, ".png"),
                    no = paste0(path_tmp, "/fig_grid_group_", iso, "_matched_all", ".png"))
  ggsave(fig_save,
         plot = fig_grid,
         device = "png",
         height = 6, width = 9)
  aws.s3::put_object(file = fig_save, 
                     bucket = ifelse(is_pa == TRUE,
                                     yes = paste("projet-afd-eva-ap", save_dir, iso, wdpaid, sep = "/"),
                                     no = paste("projet-afd-eva-ap", save_dir, iso, sep = "/")),
                     region = "", 
                     show_progress = FALSE)
  
  #Append the log
  if(is_pa == TRUE)
  {
    cat("#Plot the grid with matched control and treated for the PA \n-> OK\n", file = log, append = TRUE)
  } else cat("#Plot the grid with matched control and treated for all PAs in the country \n-> OK\n", file = log, append = TRUE)

  
  return(list("is_ok" = TRUE))
  
    },
  
  error = function(e)
  {
    print(e)
    if(is_pa == TRUE)
    {
      cat(paste("#Plot the grid with matched control and treated for the PA \n-> Error :\n", e, "\n"), file = log, append = TRUE)
    } else cat(paste("#Plot the grid with matched control and treated for all the PAs in the country \n-> Error :\n", e, "\n"), file = log, append = TRUE)
    return(list("is_ok" = FALSE))
  }
  
  # warning = function(w)
  # {
  #   #Print the warning and append the log
  #   print(w)
  #   if(is_pa == TRUE)
  #   {
  #     cat(paste("#Plot the grid with matched control and treated for the PA \n-> Warning :\n", w, "\n"), file = log, append = TRUE)
  #   } else cat(paste("#Plot the grid with matched control and treated for all the PAs in the country \n-> Warning :\n", w, "\n"), file = log, append = TRUE)
  #   return(list("is_ok" = TRUE))
  # }
  
  )
  
  return(output)
}
