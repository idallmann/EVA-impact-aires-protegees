#####
#Functions for matching process
#####

#For each function, the aim of the function, inputs, outputs, data saved and notes are detailed. This takes the following form :
#Aim of the function
##INPUTS : the arguments needed in the function
###INPUT 1 to N
##OUTPUTS : the information returned by the function (data frames, numeric, characters, etc.) and necessary to pursue to processing
### OUTPUT 1 to N
##DATA SAVED : information put in the storage but not necessarily need to pursue the processing (figures, tables, data frames, etc.)
### ...
##NOTES : any useful remark
### ...

#Remarks :
##most functions are adapted for errors handling using base::withCallingHandlers(). Basically, the computation steps are declared in a block of withCallingHandlers function, while two other blocks specify what to do in case the first block face a warning or error. In our case, errors led to return a boolean indicating an error has occured and append the log with the error message. Warnings return a boolean but do not block the iteration. They also edit the log with the warning message.
##PA is used for "protected area(s)".
##To save plots and tables : save on temporary folder in the R session then put the saved object in the storage. Indeed print() and ggplot::ggsave() cannot write directly on s3 storage
###


#Pre-processing
###

#Create a log to track progress of the processing (warnings, errors, parameters, country and PAs analyzed, etc.)
##INPUTS :
###list_iso : the list of ISO3 code corresponding to the countries analyzed
### buffer : the buffer width in meter
### gridSize : the resolution of gridding, defined by the user
### yr_first : the first year of the period where the analysis takes place
### yr_last : the last year of the period where the analysis takes place
### yr_min : the minimum treatment year to be considered in the analysis. As some matching covariates are defined with pre-treatment data (e.g average tree cover loss before treatment), this minimal year is greater than yr_first
### name : specify the name of the log file to save
### notes : any notes on the analysis performed
##OUTPUTS :
###log : a text file in the R session memory that will be edited through the data processing
fn_pre_log = function(list_iso, buffer, gridSize, yr_first, yr_last, yr_min, list_cov, name, notes)
{
  str_iso = paste(list_iso, collapse = ", ")
  log = paste(tempdir(), name, sep = "/")
  file.create(log)
  #Do not forget to end the writing with a \n to avoid warnings
  #cat(paste("#####\nCOUNTRY :", iso, "\nTIME :", print(Sys.time(), tz = "UTC-2"), "\n#####\n\n###\nPRE-PROCESSING\n###\n\n"), file = log, append = TRUE)
  cat(paste("STARTING TIME :", print(Sys.time(), tz = "UTC-2"), "\nPARAMETERS : \nBuffer =", buffer, "m \nPixel resolution", gridSize, "m\nPeriod of analysis", yr_first, "to", yr_last, "\nMinimum treatment year is", yr_min, "\nMatching covariates :", list_cov, "\nCOUNTRIES :", str_iso, "\nNOTES :", notes, "\n\n##########\nPRE-PROCESSING\n##########\n\n"), file = log, append = TRUE)
  
  return(log)
}


#Find the UTM code for a given set of coordinates
##INPUTS : 
### lonlat : coordinates
##OUTPUTS : 
### UTM code
fn_lonlat2UTM = function(lonlat) 
{
  utm = (floor((lonlat[1] + 180) / 6) %% 60) + 1
  if (lonlat[2] > 0) {
    utm + 32600
  } else{
    utm + 32700
  }
  
}


#Create the gridding of a given country. 
##INPUTS : 
### iso : ISO code 
### yr_min : the minimum treatment year to be considered in the analysis. As some matching covariates are defined with pre-treatment data (e.g average tree cover loss before treatment), this minimal year is greater than the first year in the period considered
### path_tmp : temporary path for saving figures
### data_pa : dataset with information on protected areas, and especially their surfaces
### gridSize : the resolution of gridding, defined by the user
### log : a log file to track progress of the processing
### save_dir : saving directory
##OUTPUTS (depending on potential errors)
### gadm_prj : country shapefile 
### grid : gridding of the country
### utm_code : UTM code of the country
### is_ok : a boolean indicating whether or not an error occured inside the function
##DATA SAVED :
### None
fn_pre_grid = function(iso, yr_min, path_tmp, data_pa, gridSize, log, save_dir)
{
  
  output = withCallingHandlers(
    
    {
      
  # Download country polygon
  gadm = gadm(country = iso, resolution = 1, level = 0, path = path_tmp) %>% 
    st_as_sf() %>%
    st_make_valid() #Necessary for some polygons : e.g BEN
  
  # Find UTM zone of the country centroid
  centroid = st_coordinates(st_centroid(gadm))
  utm_code = fn_lonlat2UTM(centroid)
  # Reproject GADM
  gadm_prj = gadm %>% 
    st_transform(crs = utm_code)
  
  #Determine relevant grid size
  ## From the smallest PA in the country considered and the desired sampling size (OLD)
  # ##Select the PA in the country with minimum area. PAs with null areas, marine or treatment year before 2000 are discarded (not analyzed anyway)
  # pa_min = data_pa %>%
  #   filter(iso3 == iso & is.na(wdpaid) == FALSE & status_yr >= yr_min & marine %in% c(0,1)) %>%
  #   arrange(area_km2) %>%
  #   slice(1)
  # ##From this minimum area, define the grid size. 
  # ##It depends on the sampling of the minimal area, i.e how many pixels we want to subdivide the PA with lowest area
  # ## To avoid a resolution higher than the one of our data, grid size is set to be 30m at least (resolution of tree cover data, Hansen et al. 2013)
  # area_min = pa_min$area_km2 #in kilometer
  # gridSize = max(1e3, round(sqrt(area_min/sampling)*1000, 0)) #Side of the pixel is expressed in meter and rounded, if above 1km. 
  
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
  
  #Append the log 
  cat("#Generating observation units\n-> OK\n", file = log, append = TRUE)
  
  #Return outputs
  list_output = list("ctry_shp_prj" = gadm_prj, 
                     "grid" = grid, 
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

#Assign each pixel (observation unit) to a group : PA non-funded, funded and analyzed, funded and not analyzed, buffer, potential control. 
##INPUTS :
### iso : country ISO code
### path_tmp : temporary path to save figures
### utm_code : UTM of the country centroid
### buffer_m : buffer width, in meter
### data : a dataframe with the WDPAID of PAs funded by the AFD
### gadm_prj : country polygon, projected so that crs = UTM code
### grid : gridding of the country
### gridSize : resolution of the gridding
##OUTPUTS (depending on potential errors): 
### grid.param : a raster representing the gridding of the country with two layers. One for the group each pixel belongs to (funded PA, non-funded PA, potential control, buffer), the other for the WDPAID corresponding to each pixel (0 if not a PA)
### is_ok : a boolean indicating whether or not an error occured inside the function
##DATA SAVED :
### grid.param
### A plot of the country gridding with group of each pixel
### The share of PAs in the portfolio considered that are reported in the WDPA
### In the country considered, the share of PAs in the portfolio and analyzed, not analyzed or not in the portfolio
### Share of PAs reported in the WDPA and analyzed in the country considered
##NOTES :
### Errors can arise from the wdpa_clean() function, during "formatting attribute data" step. Can be settled playing with geometry_precision parameter
fn_pre_group = function(iso, wdpa_raw, status, yr_min, path_tmp, utm_code, buffer_m, data_pa, gadm_prj, grid, gridSize, log, save_dir)
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
    wdpa_clean(retain_status = status, #NULL to remove proposed
               erase_overlaps = FALSE,
               exclude_unesco = FALSE,
               verbose = TRUE) %>% 
    # Remove the PAs that are only proposed, or have geometry type "point"
    #filter(STATUS != "Proposed") %>%  #24/08/2023 : "Proposed" status concerns only 6 PAs in the sample, including one implemented after 2000.
    filter(GEOMETRY_TYPE != "POINT") %>%
    # Project PA polygons to the previously determined UTM zone
    st_transform(crs = utm_code) 
  
  # Numerous polygons reported in the WDPA overlap. This can lead be an issue : a pixel can be treated more than once, and the treatment effect of the PA of interest cannot be isolated a priori 
  # Compute a layer of polygons without overlaps, and with overlaps only. The analysis will be performed on polygons with no overlap and whose non-overlapped area is not too small (10% at least of initial area). Also, the overlapped areas are assigned to a specific group so that they are not used as control.
  ## Layer without overlap
  wdpa_prj_vect = wdpa_prj %>% terra::vect()
  wdpa_prj_noverlap = sapply(1:dim(wdpa_prj_vect)[1], 
                         function(X) {erase(wdpa_prj_vect[X,], wdpa_prj_vect[-X,])}) %>% 
    terra::vect() %>%
    st_as_sf() %>%
    mutate(area_overlap = expanse(terra::vect(geometry), unit = "km"),
           rm = area_overlap < 0.1*REP_AREA)
  ## Layer of overlaps
  wdpa_prj_overlap = sapply(1:dim(wdpa_prj_vect)[1], 
                        function(X) {intersect(wdpa_prj_vect[X,], wdpa_prj_vect[-X,])}) %>% 
    terra::vect() %>%
    st_as_sf() 
  
  
  # Make Buffers around all protected areas : done on the polygons with overlaps, as it is the buffer to consider in practice
  buffer = st_buffer(wdpa_prj, dist = buffer_m) %>% 
    # Assign an ID "6" to the buffer group
    mutate(group=6,
           group_name = "Buffer")
  
  # Assign polygon overlap to a specific group, so that the corresponding pixerls won't be used as potential control
  overlap = wdpa_prj_overlap %>%
    dplyr::select(contains("_1")) %>%
    rename_with(.fn = ~gsub("_1", "", .x),
                .cols = everything()) %>%
    #Assign it the ID 1
    mutate(group = 1,
           group_name = "PA overlap")
  
  # Separate PA in sample or not
  ##PAs in the sample
  ###... which can bu used in impact evaluation : in the country of interest, wdpaid known, area above 1km² (Wolf et al. 2021), implemented after yr_min defined by the user, non-marine (terrestrial or coastal, Wolf et al. 2021)
  pa_sample_ie = data_pa %>%
    filter(iso3 == iso & is.na(wdpaid) == FALSE & area_km2 > 1 & status_yr >= yr_min & marine %in% c(0,1))
  wdpaID_sample_ie = pa_sample_ie$wdpaid
  wdpa_sample_ie = wdpa_prj_noverlap %>% 
    filter(WDPAID %in% wdpaID_sample_ie & rm == F) %>% #Remove PA with too much overlap
    mutate(group=3,
           group_name = "PA in sample, analyzed") %>% # Assign an ID "3" to the PA in sample, analysed
    dplyr::select(-c(area_overlap, rm))
  ###...which cannot
  pa_sample_no_ie = data_pa %>%
    filter(iso3 == iso & (is.na(wdpaid) == TRUE | area_km2 <= 1 | is.na(area_km2) | status_yr < yr_min | marine == 2)) #PAs not in WDPA, of area less than 1km2 (Wolf et al 2020), not terrestrial/coastal or implemented after yr_min are not analyzed
  wdpaID_sample_no_ie = pa_sample_no_ie$wdpaid 
  wdpa_sample_no_ie = wdpa_prj_noverlap %>% filter(WDPAID %in% wdpaID_sample_no_ie) %>%
    mutate(group=4,
           group_name = "PA in sample, not analyzed") %>% # Assign an ID "4" to the PA in sample which cannot be studied in the impact evaluation
    dplyr::select(-c(area_overlap, rm))
  ##PAs not in the sample
  wdpa_no_sample = wdpa_prj_noverlap %>% 
    filter(!(WDPAID %in% c(wdpa_sample_ie$WDPAID, wdpa_sample_no_ie$WDPAID))) %>% 
    mutate(group=5,
           group_name = "PA not in sample") %>% # Assign an ID "5" to the AP not in sample
    dplyr::select(-c(area_overlap, rm))
  wdpaID_no_sample = wdpa_no_sample$WDPAID
  
  # Merge the dataframes of funded PAs, non-funded PAs and buffers
  # CAREFUL : the order of the arguments does matter. 
  ## During rasterization, in case a cell of the raster is on both funded analysed and non-funded, we want to cell to take the WDPAID of the funded analysed.
  ## Same funded, not analyzed. As the first layer is taken, wdpa_afd_ie needs to be first !
  wdpa_groups = rbind(overlap, wdpa_sample_ie, wdpa_sample_no_ie, wdpa_no_sample, buffer)
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

  # Control pixels
  ## Take all background pixels as potential control : assign to group 2
  grid.group = grid.group.ini %>%
    mutate(group = case_when(group == 0 ~ 2,
                             TRUE ~ group))
  
  ## Randomly select background pixels as potential control pixels
  # ##Take the list of background pixels, the  number of background and treatment pixels
  # list_back_ID = grid.group.ini[grid.group.ini$group == 0 & is.na(grid.group.ini$group) == FALSE,]$gridID
  # n_back_ID = length(list_back_ID)
  # n_treat = length(grid.group.ini[grid.group.ini$group == 2 & is.na(grid.group.ini$group) == FALSE,]$gridID)
  # ##The number of potential control units is five times the number of treatment units
  # n_control = min(n_back_ID, n_treat*5)
  # ##Select randomly the list of background pixels selected as controls
  # ### Note that we control for the case n_back_ID = 1, which causes weird behavior using sample()
  # set.seed(0) #To ensure reproductibility of the random sampling
  # if(n_back_ID <= 1) list_control_ID = list_back_ID else list_control_ID = sample(x = list_back_ID, size = n_control, replace = FALSE)
  # ## Finally, assign the background pixel chosen to the control group, characterized by group = 1
  # grid.group = grid.group.ini %>%
  #   mutate(group = case_when(gridID %in% list_control_ID ~ 1,
  #                            TRUE ~ group))
  

  # Merge data frames
  grid.param.ini = grid.group %>%
    merge(., grid.wdpaid, by="gridID") %>%
    merge(., grid, by="gridID") %>%
    # drop rows having "NA" in column "group"
    drop_na(group) %>%
    st_as_sf() %>%
    # Grid is projected to WGS84 because mapme.biodiverty package merely works with this CRS
    st_transform(crs=4326) %>%
    #Add relevant variables
    left_join(dplyr::select(data_pa, c(region_afd, region, sub_region, country_en, iso3, wdpaid, status_yr, year_funding_first, year_funding_all, focus)), by = "wdpaid")
  
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
    mutate(group = case_when(wdpaid %in% wdpa_sample_no_ie$WDPAID & group == 3 ~ 4,
                             TRUE ~ group)) %>%
    #Add name for the group
    mutate(group_name = case_when(group == 0 ~ "Background",
                                  group == 1 ~ "PA overlap",
                                  group == 2 ~ "Potential control",
                                  group == 3 ~ "PA in sample, analyzed (potential treatment)",
                                  group == 4 ~ "PA in sample, not analyzed",
                                  group == 5 ~ "PA not in sample",
                                  group == 6 ~ "Buffer")) %>%
  #Add spatial resolution in m : useful to compute share of forest area in a given pixel and extrapolate to the PA for instance
  mutate(res_m = gridSize)
  
  
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
    filter(group == 3) %>% 
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
    #   labels=c("control candidate", "treatment candidate", "PA not in sample", "buffer zone")) +
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
    mutate(group_ie = case_when(!WDPAID %in% c(wdpaID_sample_ie, wdpaID_sample_no_ie) ~ "Not in sample",
                                WDPAID %in% wdpaID_sample_ie ~ "In sample, analyzed",
                                WDPAID %in% wdpaID_sample_no_ie ~ "In sample, not analyzed")) %>%
    group_by(ISO3, group_ie) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    mutate(n_tot = sum(n),
           freq = round(n/n_tot*100, 1))
  
  ## PAs in the sample : reported in the WDPAID or not
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
           title = "Share of PAs in sample and reported in the WDPA",
           subtitle = paste("Sample :", sum(df_pie_wdpa$n), "protected areas in the sample, in", country.name)) %>%
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
           subtitle = paste("Sample :", sum(df_pie_ie$n), "protected areas in sample, in", country.name)) %>%
    + scale_fill_brewer(name = "", palette = "Greens") %>%
    + theme_void()
  
  ##Saving plots
  tmp = paste(tempdir(), "fig", sep = "/")
  ggsave(paste(tmp, paste0("pie_sample_wdpa_", iso, ".png"), sep = "/"),
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


# Presently (19/102023) there is an issue with mapme.biodiversity::calc_indicators() function to compute the biome indicator from TEOW data. The following function takes mapme.biodiversity functions and dd slight modifications so that it works.
# https://github.com/mapme-initiative/mapme.biodiversity/issues/196 for a detailed explanation
##INPUTS
### x : the portfolio where TEOW data have been downloaded through mapme.biodiversity::get_resources() function
### indicator : the indicator we want to compute from TEOW data ("biome" here).
##OUTPTUS
### data.biome : a dataset with biome and corresponding area for each asset in the portfolio x
fn_calc_biome_temp = function(x, indicator)
{
  i <- NULL
  args <- list() #No additionnal arguments
  atts <- attributes(x) #Attributes of the portfolio
  selected_indicator <- available_indicators(indicator) #The indicator available, processing mode and function to extract it
  processing_mode <- selected_indicator[[indicator]]$processing_mode #Get processing mode
  params <- mapme.biodiversity:::.check_resource_arguments(selected_indicator, 
                                                           args) #Check we do have the resource to compute the indicator
  params$verbose <- atts$verbose #Get verbose
  fun <- selected_indicator[[indicator]]$fun #Get the function
  available_resources <- atts$resources #The resources available in the temporary folder of mapme
  required_resources <- selected_indicator[[indicator]]$resources #The resource required to compute the indicator specified
  if (processing_mode == "asset") {
    p <- with_progress(progressr::progressor(steps = nrow(x)))
    results <- furrr::future_map(1:nrow(x), function(i) {
      p()
      resources <- mapme.biodiversity:::.prep(x[i, ], atts$resources, required_resources)
      mapme.biodiversity:::.compute(x[i, ], resources, fun, params, i)
    }, .options = furrr::furrr_options(seed = TRUE))
  }
  else {
    resources <- .prep(x, atts$resources, required_resources)
    results <- .compute(x, resources, fun, params, 1)
  }
  biome <- mapme.biodiversity:::.bind_assets(results) %>%
    mutate(.id = as.numeric(.id)) %>%
    dplyr::select(-c(area))
  # results <- nest(results, `:=`(!!indicator, !.id))
  # x[indicator] <- results[indicator]
  data.biome = left_join(x, biome, by = c("assetid" = ".id"))
  # x <- relocate(x, !!attributes(x)[["sf_column"]], .after = last_col())
  # x
  
  return(data.biome)
  
}


#Building a matching dataframe for the country considered : for each pixel in treated and control groups, the data needed for the analysis are downloaded and the indicators computed. Eventually a dataset is obtained that is ready to enter a matching algorithm
##INPUTS :
### grid.param : a raster representing the gridding of the country with two layers. One for the group each pixel belongs to (funded PA, non-funded PA, potential control, buffer), the other for the WDPAID corresponding to each pixel (0 if not a PA)
### path_tmp : a temporary folder to store figures
### iso : ISO code of the country of interest
### name_output : the name of the matching frame to save
### ext_output : the file extension of the matching to save 
### yr_first : the first year of the period where the analysis takes place
### yr_last : the last year of the period where the analysis takes place
### log : a log file to track progress of the processing
### save_dir : saving directory
##OUTPUTS :
### is_ok : a boolean indicating whether or not an error occured inside the function
##DATA SAVED
### pivot.all : a dataframe with variables of interest (outcome, matching covariates) for all treated and potential control pixels

fn_pre_mf_parallel = function(grid.param, path_tmp, iso, yr_first, yr_last, log, save_dir) 
{
  output = tryCatch(
    
    {
  tic = tic()
  
  print("----Initialize portfolio")
  # Take only potential control (group = 1) and treatment (group = 2) in the country gridding to lower the number of computations to perform
  grid.aoi = grid.param %>%
    filter(group %in% c(2,3))
  # Create a mapme.biodiversity portfolio for the area of interest (aoi). This specifies the period considered and the geospatial units where data are downloaded and indicators computed (here, the treated and control pixels in the country gridding)
  aoi = init_portfolio(grid.aoi,
                       years = yr_first:yr_last,
                       outdir = path_tmp,
                       add_resources = FALSE)
  
  #Extract a dataframe with pixels ID in the grid and the portfolio : useful for latter plotting of matched control and treated units. 
  df_gridID_assetID = aoi %>%
    st_drop_geometry() %>%
    as.data.frame() %>%
    dplyr::select(c(gridID, assetid))
  s3write_using(df_gridID_assetID,
                data.table::fwrite,
                object = paste0(save_dir, "/", iso, "/", "df_gridID_assetID_", iso, ".csv"),
                bucket = "projet-afd-eva-ap",
                opts = list("region" = ""))
  
  print("----Download data")
  # Download Data
  ## Version of Global Forest Cover data to consider
  list_version_gfc = mapme.biodiversity:::.available_gfw_versions() #all versions available
  version_gfc = list_version_gfc[length(list_version_gfc)] #last version considered
  ## Soil characteristics
  dl.soil = get_resources(aoi, 
                          resources = c("soilgrids"), 
                          layers = c("clay"), # resource specific argument
                          depths = c("0-5cm"), # resource specific argument
                          stats = c("mean"))
  ## Accessibility
  dl.travelT = get_resources(aoi, resources = "nelson_et_al",
                             range_traveltime = c("5k_110mio"))
  ## Tree cover evolution on the period
  dl.tree = get_resources(aoi, 
                          resources = c("gfw_treecover", "gfw_lossyear"),
                          vers_treecover = version_gfc,
                          vers_lossyear = version_gfc)
  ## Elevation
  dl.elevation = get_resources(aoi, "nasa_srtm")
  ## Terrain Ruggedness Index
  dl.tri = get_resources(aoi, "nasa_srtm")
  ## Biome
  dl.bio = get_resources(aoi, resources = "teow")
  
  print("----Compute indicators")
  #Compute indicators
  
  #Begin multisession : use of parallel computing (computations performed in separate R sessions in background) to speed up the computations of indicators
  # gc : optimize memory management for the background sessions.
  # Multisession with workers = 6 as in mapme.biodiversity tutorial : https://mapme-initiative.github.io/mapme.biodiversity/articles/quickstart.html?q=parall#enabling-parallel-computing
  # Careful to the format of command to call parallel computations here : VALUE TO COMPUTE %<-% {EXPRESSION}.
  # Note after each calc_indicators is specified a seed manually (for reproducibility). This is to ensure the Random Number Generating processes in the parellel sessons are independant, and avoid any bug (https://www.r-bloggers.com/2020/09/future-1-19-1-making-sure-proper-random-numbers-are-produced-in-parallel-processing/; https://stackoverflow.com/questions/69365728/how-do-i-suppress-a-random-number-generation-warning-with-future-callr)
  library(future)
  library(progressr)
  plan(multisession, workers = 6, gc = TRUE)
  with_progress({
    get.soil <- calc_indicators(dl.soil,
                                indicators = "soilproperties",
                                stats_soil = c("mean"),
                                engine = "exactextract") %seed% 1 # the "exactextract" engine is chosen as it is the faster one for large rasters (https://tmieno2.github.io/R-as-GIS-for-Economists/extraction-speed-comparison.html)

    get.travelT  <- calc_indicators(dl.travelT,
                                  indicators = "traveltime",
                                  stats_accessibility = c("mean"),  #Note KfW use "median" here, but for no specific reason a priori (mail to Kemmeng Liu, 28/09/2023). Mean is chosen coherently with the other covariates, though we could test in a second time whether this changes anything to the results.
                                  engine = "exactextract") %seed% 2

    get.tree <- calc_indicators(dl.tree,
                               indicators = "treecover_area",
                               min_size=0.5, # FAO definition of forest :  Minimum treecover = 10%, minimum size =0.5 hectare (FAO 2020 Global Forest Resources Assessment, https://www.fao.org/3/I8661EN/i8661en.pdf)
                               min_cover=10) %seed% 3

    get.elevation <- calc_indicators(dl.elevation,
                      indicators = "elevation",
                      stats_elevation = c("mean"),
                      engine = "exactextract") %seed% 4

    get.tri <- calc_indicators(dl.tri,
                      indicators = "tri",
                      stats_tri = c("mean"),
                      engine = "exactextract") %seed% 5
    
    # get.bio %<-% {calc_indicators(dl.bio,
    #                               indicators = "biome")} %seed% 6 
    
    })
  
  print("----Build indicators' datasets")
  #Build indicators' datasets
  ## Transform the output dataframe into a -ore convenient format
  data.soil = unnest(get.soil, soilproperties) %>%
    #mutate(across(c("mean"), \(x) round(x, 3))) %>% # Round numeric columns --> rounding before the matching algorithm is irrelevant to me
    pivot_wider(names_from = c("layer", "depth", "stat"), values_from = "mean") %>%
    rename("clay_0_5cm_mean" = "clay_0-5cm_mean") %>%
    mutate(clay_0_5cm_mean = case_when(is.nan(clay_0_5cm_mean) ~ NA,
                                       TRUE ~ clay_0_5cm_mean))
  
  data.travelT = unnest(get.travelT, traveltime) %>%
    pivot_wider(names_from = "distance", values_from = "minutes_mean", names_prefix = "minutes_mean_") %>%
    mutate(minutes_mean_5k_110mio = case_when(is.nan(minutes_mean_5k_110mio) ~ NA,
                                       TRUE ~ minutes_mean_5k_110mio))
  
  data.tree = unnest(get.tree, treecover_area) %>%
    drop_na(treecover) %>% #get rid of units with NA values 
    #mutate(across(c("treecover"), \(x) round(x, 3))) %>% # Round numeric columns
    pivot_wider(names_from = "years", values_from = "treecover", names_prefix = "treecover_")
  
  data.tri = unnest(get.tri, tri) %>%
    mutate(tri_mean = case_when(is.nan(tri_mean) ~ NA,
                                TRUE ~ tri_mean))
  
  data.elevation = unnest(get.elevation, elevation) %>%
    mutate(elevation_mean = case_when(is.nan(elevation_mean) ~ NA,
                                TRUE ~ elevation_mean))
  # mutate(elevation_mean = case_when(is.nan(elevation_mean) ~ NA,
  #                                   TRUE ~ elevation_mean))
  
  #data.bio = unnest(get.bio, biome) 
  data.bio = fn_calc_biome_temp(x = dl.bio, indicator = "biome")

  
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
  # Remove "geometry" column from dataframes
  df.tree = data.tree %>% mutate(x = NULL) %>% as.data.frame()
  df.travelT = data.travelT %>% mutate(x = NULL) %>% as.data.frame()
  df.soil = data.soil %>% mutate(x = NULL) %>% as.data.frame()
  df.elevation = data.elevation %>% mutate(x = NULL) %>% as.data.frame()
  df.tri = data.tri %>% mutate(x=NULL) %>% as.data.frame()
  #df.bio = data.bio %>% mutate(x=NULL) %>% as.data.frame()
  df.bio = data.bio %>% mutate(x = NULL) %>% as.data.frame()
  
  # Make a dataframe containing only "assetid" and geometry
  # Use data.soil instead of data.tree, as some pixels are removed in data.tree (NA values from get.tree)
  df.geom = data.soil[, c("assetid", "x")] %>% as.data.frame() 
  
  # Merge all output dataframes 
  pivot.all = Reduce(dplyr::full_join, list(df.travelT, df.soil, df.tree, df.elevation, df.tri, df.bio, df.geom)) %>%
    st_as_sf()

  # Make column Group ID and WDPA ID have data type "integer"
  pivot.all$group = as.integer(pivot.all$group)
  pivot.all$wdpaid = as.integer(pivot.all$wdpaid)

  # Save this matching dataframe
  name_save = paste0("matching_frame_spling", "_", iso, ".gpkg")
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
### yr_min : the minimum treatment year to be considered in the analysis. As some matching covariates are defined with pre-treatment data (e.g average tree cover loss before treatment), this minimal year is greater than the first year in the period considered
### log : a log file to track progress of the processing
### save_dir : saving directory
##OUTPUTS :
### mf : matching dataframe. More precisely, it gives for each observation units in a country values of different covariates to perform matching.
### is_ok : a boolean indicating whether or not an error occured inside the function
##DATA SAVED
### The list of PAs in the matching frame, characterized by their WDPAID. Useful to loop over each PAs we want to analyze in a given country
fn_post_load_mf = function(iso, yr_min, name_input, ext_input, log, save_dir)
{
  output = tryCatch(
    
    {
      
  #Load the matching dataframe
  object = paste(save_dir, iso, paste0("matching_frame_spling_", iso, ".gpkg"), sep = "/")
  mf = s3read_using(sf::st_read,
                      bucket = "projet-afd-eva-ap",
                      object = object,
                      opts = list("region" = "")) 
  
  #Subset to control and treatment units with year of treatment >= yr_min
  mf = mf %>%
    filter(group ==2 | group==3) %>%
    #Remove observations with NA values only for covariates :
    ## except for creation year, funding years, geographical location, country ISO and name, pixel resolution which are NA for control units
    drop_na(-c(status_yr, year_funding_first, year_funding_all, region_afd, region, sub_region, iso3, country_en, res_m, focus)) #%>%
     #filter(status_yr >= yr_min | is.na(status_yr))
  
  #Write the list of PAs to match
  list_pa = mf %>%
    st_drop_geometry() %>%
    as.data.frame() %>%
    dplyr::select(c(region_afd, region, sub_region, country_en, iso3, wdpaid, status_yr, year_funding_first, year_funding_all, focus)) %>%
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


#Compute average forest loss and cover before protected area creation, and add them to the matching frame as covariates
##INPUTS : 
### mf : the matching dataframe
### colname.flAvg : name of the average forest loss variable
### log : a log file to track progress of the processing
## OUTPUTS :
### mf : matching frame with the new covariate
### is_ok : a boolean indicating whether or not an error occured inside the function

fn_post_fl_fc_pre_treat = function(mf, colname.flAvg, log)
{
  
  output = tryCatch(
    
    {
      
  #Extract treatment year
  treatment.year = mf %>% 
    filter(group == 3) %>% 
    slice(1)
  treatment.year = treatment.year$status_yr
  
  #Extract first year treeloss is computed
  ##Select cols with "treeloss" in mf, drop geometry, replace "treeloss_" by "", convert to num and take min
  treeloss.ini.year = mf[grepl("treeloss", names(mf))] %>%
    st_drop_geometry() %>%
    names() %>%
    gsub(paste0("treeloss", "_"), "", .) %>%
    as.numeric() %>%
    min()
  
  #Extract first year treecover is computed
  ##Select cols with "treecover" in mf, drop geometry, replace "treecover_" by "", convert to num and take min
  treecover.ini.year = mf[grepl("treecover", names(mf))] %>%
    st_drop_geometry() %>%
    names() %>%
    gsub(paste0("treecover", "_"), "", .) %>%
    as.numeric() %>%
    min()
  
  #Define period to compute average loss and cover
  ## Global Forest Watch recommends to smooth forest cover on three years average : "Use 3-year moving averages to smooth any inconsistencies between years caused by the availability of Landsat images and better account for delays in detection of late-year fires and other losses." (https://www.globalforestwatch.org/blog/data-and-research/tree-cover-loss-satellite-data-trend-analysis/) 
  ##If 3 pre-treatment periods are available at least, then average pre-treatment deforestation/forest cover is computed on this 3 years range
  ## If less than 3 are available, compute on this restricted period
  ## Note that by construction, treatment.year >= treeloss.ini.year +1 (as yr_min = yr_first+2 in the parameters), so that at least one period of forest cover loss is known for building this variable. 
  ## treecover data starts in 2000 but 2001 for treeloss. Thus if at least one period is accessible for treeloss by construction (2001 if treatment year = 2002), two are for treecover (2000 and 2001). Thus we need to take that into account.
  ### Period for forest loss
  if((treatment.year-treeloss.ini.year) >=3)
  {yr_start_loss = (treatment.year)-3
  yr_end_loss = (treatment.year)-1} else if((treatment.year-treeloss.ini.year <3) & (treatment.year-treeloss.ini.year >0))
  {yr_start_loss = treeloss.ini.year
  yr_end_loss = (treatment.year)-1} 
  ### Period for forest cover
  if((treatment.year-treecover.ini.year) >=3)
  {yr_start_cover = (treatment.year)-3
  yr_end_cover = (treatment.year)-1} else if((treatment.year-treecover.ini.year <3) & (treatment.year-treecover.ini.year >0))
  {yr_start_cover = treecover.ini.year #here is the real change compared to treeloss : the starting year is 2000 and not 2001
  yr_end_cover = (treatment.year)-1} 
  #Transform it in variable suffix
  var_start_loss = yr_start_loss - 2000
  var_end_loss = yr_end_loss - 2000
  var_start_cover = yr_start_cover - 2000
  var_end_cover = yr_end_cover - 2000
  #Select only relevant variables
  df_fl = mf[grepl("treeloss", names(mf))][var_start_loss:var_end_loss] %>% 
    st_drop_geometry()
  df_fc = mf[grepl("treecover", names(mf))][var_start_cover:var_end_cover] %>% 
    st_drop_geometry()
  #Compute average loss for each pixel and store it in mf. Also add the start and end years of pre-treatment period where average loss is computed.
  mf$avgLoss_pre_treat = round(rowMeans(df_fl), 2)
  mf$avgCover_pre_treat = round(rowMeans(df_fc), 2)
  mf$start_pre_treat_fl = yr_start_loss
  mf$end_pre_treat_fl = yr_end_loss
  mf$start_pre_treat_fc = yr_start_cover
  mf$end_pre_treat_fc = yr_end_cover
  
  #Remove NA values
  mf = mf %>% drop_na(c(avgLoss_pre_treat, avgCover_pre_treat))
  
  #Append the log
  cat("#Add average pre-treatment forest loss and cover\n-> OK\n", file = log, append = TRUE)
  
  #Return output
  return(list("mf" = mf, "is_ok" = TRUE))
  
    },
  
  error = function(e)
  {
    print(e)
    cat(paste("#Add average pre-treatment forest loss and cover\n-> Error :\n", e, "\n"), file = log, append = TRUE)
    return(list("is_ok" = FALSE))
  }
  
  # warning = function(w)
  # {
  #   #Print the warning and append the log
  #   print(w)
  #   #Append the log 
  #   cat(paste("#Add average pre-treatment forest loss and cover\n-> Warning :\n", w, "\n"), file = log, append = TRUE)
  #   #Return string to inform user to skip
  #   return(list("is_ok" = TRUE))
  # }
  
  )
  
  return(output)
}


#Perform matching of treated and potential control units. 
##INPUTS :
### mf : the matching dataframe
### iso : the ISO code of the country considered
### dummy_int : should we consider the interaction of variables for matching ? Is recommended generally speaking (https://cran.r-project.org/web/packages/MatchIt/vignettes/assessing-balance.html). When using CEM matching, variables are binned then exact matching is performed on binned values. As a first approximation we can argue that if two units have the same binned values for two variables, then they likely have the same binned interaction value. It is not necessarily true though, as binned(A)*binned(B) can be different from binned(A*B).  
### match_method : the matching method to use. See https://cran.r-project.org/web/packages/MatchIt/vignettes/matching-methods.html for a list of matching methods we can use with the MatchIT package 
### cutoff_method : the method to use for automatic histogram binning of the variables. See Iacus, King and Porro 2011 (https://gking.harvard.edu/files/political_analysis-2011-iacus-pan_mpr013.pdf), 5.5.1, or MathIT documentation. "Sturges" tend to have the best outcomes (number of matched units) in our case (Antoine Vuillot, 28/09/2023)
### is_k2k : boolean. Should we use k2k matching ? If yes, each treated unit is eventually matched with a single control. For CEM matching, a treated unit is potentially associated with more than one control unit (exact matching on binned variables), and then the "closest' one is chosen with a metric defined in k2k_method
### k2k_method : metric to use to choose the closest control among the control units matched with a treated unit in CEM matching.
### th_mean :the maximum acceptable value for absolute standardized mean difference of covariates between matched treated and control units. Typically 0.1 (https://cran.r-project.org/web/packages/MatchIt/vignettes/assessing-balance.html) or 0.25 in conservation literature (e.g https://conbio.onlinelibrary.wiley.com/doi/abs/10.1111/cobi.13728) 
### th_var_min, th_var_max : the range of acceptable value for covariate variance ratio between matched treated and control units. Typicall 0.5 and 2, respectively (https://cran.r-project.org/web/packages/MatchIt/vignettes/assessing-balance.html)
### colname.travelTime, colname.clayContent, colname.elevation, colname.tri, colname.fcAvg, colname.flAvg : name of the matching covariates
### log : a log file to track progress of the processing
##OUTPUTS : 
### out.cem : an object with all information on matching (parameters, results, etc.)
### df.cov.m : for each matching covariate, statistics to assess the quality of the match
### is_ok : a boolean indicating whether or not an error occured inside the function
##NOTES
### The matching method chosen is CEM though other exists. For a presentation of the different matching algorithms, see https://cran.r-project.org/web/packages/MatchIt/vignettes/matching-methods.html
fn_post_match_auto = function(mf,
                              iso,
                              dummy_int,
                              match_method,
                              cutoff_method,
                              is_k2k,
                              k2k_method,
                              th_mean, 
                              th_var_min, th_var_max,
                              colname.travelTime, colname.clayContent, colname.elevation, colname.tri, colname.fcAvg, colname.flAvg, colname.biome,
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
      ## Two cases : if only one value of biome across control and treated units considered for the matching, then we do not consider biome (de facto, matched units will have the same biome). If we have at least two biomes, then we need to take it into account in the formula.
      ## Note that no grouping is needed for the categorical variable "biomes" : according to the documentation "Note that if a categorical variable does not appear in grouping, it will not be coarsened, so exact matching will take place on it". 
      if(length(unique(mf$biomes)) == 1) 
        {
        formula = eval(bquote(group ~ .(as.name(colname.travelTime)) 
                      + .(as.name(colname.clayContent))  
                      +  .(as.name(colname.fcAvg)) 
                      + .(as.name(colname.flAvg))
                      + .(as.name(colname.tri))
                      + .(as.name(colname.elevation))))
      } else formula = eval(bquote(group ~ .(as.name(colname.travelTime)) 
                          + .(as.name(colname.clayContent))  
                          +  .(as.name(colname.fcAvg)) 
                          + .(as.name(colname.flAvg))
                          + .(as.name(colname.tri))
                          + .(as.name(colname.elevation))
                          + .(as.name(colname.biome))))
      
      #Try to perform matching
      out.cem = matchit(formula,
                        data = mf,
                        method = match_method,
                        cutpoints = cutoff_method,
                        k2k = is_k2k,
                        k2k.method = k2k_method)
      
      # Then the performance of the matching is assessed, based on https://cran.r-project.org/web/packages/MatchIt/vignettes/assessing-balance.html
      ## Covariate balance : standardized mean difference and variance ratio
      ## For both tests and the joint one, a dummy variable is defined, with value TRUE is the test is passed
      df.cov.m = summary(out.cem, interactions = dummy_int)$sum.matched %>%
        as.data.frame() %>%
        clean_names() %>%
        mutate(th_var_min = th_var_min,
               th_var_max = th_var_max,
               th_mean = th_mean,
               is_var_ok = var_ratio < th_var_max & var_ratio > th_var_min, #Check variance ratio between treated and controls
               is_mean_ok = abs(std_mean_diff) < th_mean, #Check absolute standardized mean difference
               is_bal_ok = as.logical(is_var_ok*is_mean_ok), #Binary : TRUE if both variance and mean difference check pass, 0 if at least one does not
               .after = "var_ratio")
      
      # Build a table to report match quality
      ## Get relevant info on the PA
      ### Country name and iso
      df_info = mf %>%
        st_drop_geometry() %>%
        filter(group == 3) %>%
        slice(1)
      region = df_info$region
      sub_region = df_info$sub_region
      country_en = df_info$country_en
      wdpaid = df_info$wdpaid
      ## Build the table from previous matchit.summary
      covariate = row.names(df.cov.m)
      tbl.quality = cbind(covariate, as.data.frame(df.cov.m)) %>%
        mutate(region = region,
               sub_region = sub_region,
               iso3 = iso,
               country_en = country_en,
               wdpaid = wdpaid,
               .before = "covariate")
      row.names(tbl.quality) = NULL
      
      #Add a warning if covariate balance tests are not passed
      if(sum(df.cov.m$is_bal_ok) < nrow(df.cov.m) | is.na(sum(df.cov.m$is_bal_ok)) == TRUE)
      {
        message("Matched control and treated units are not balanced enough. Increase sample size, turn to less restrictive tests or visually check balance.")
        cat("-> Careful : matched control and treated units are not balanced enough. Increase sample size, turn to less restrictive tests or visually check balance.\n", 
            file = log, append = TRUE)
      }
      
      #Append the log : note the step has already been appended at the beginning of the function
      cat("-> OK\n", file = log, append = TRUE)
      
      return(list("out.cem" = out.cem, "df.cov.m" = df.cov.m, "tbl.quality" = tbl.quality, "is_ok" = TRUE))
      
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
### mf : the matching dataframe
### colname.travelTime, colname.clayContent, colname.elevation, colname.tri, colname.fcAvg, colname.flAvg : name of the matching covariates
### iso : ISO code of the country considered
### path_tmp : temporary folder to store figures
### wdpaid : the WDPA ID of the protected area considered
### log : a log file to track progress of the processing
### save_dir : saving directory
##OUTPUTS :
### is_ok : a boolean indicating whether or not an error occured inside the function
## DATA SAVED :
### A covariate love plot
### A table with number of treated and control units, before and after matching
### A table with statistics on matched control and treated units 
### A table with statistics on unmatched control and treated units,
fn_post_covbal = function(out.cem, tbl.quality, mf, 
                          colname.travelTime, colname.clayContent, colname.fcAvg, colname.flAvg, colname.tri, colname.elevation, colname.biome,
                          th_mean,
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
    filter(group == 3) %>% 
    slice(1)
  country.name = country.name$country_en
  
  #Extract start and end years of pre-treatment period where average loss is computed
  df_year = mf %>%
    filter(group == 3) %>% 
    slice(1)
  year.start.fl = df_year$start_pre_treat_fl
  year.end.fl = df_year$end_pre_treat_fl
  year.start.fc = df_year$start_pre_treat_fc
  year.end.fc = df_year$end_pre_treat_fc
  
  #Plot covariate balance
  colname.flAvg.new = paste0("Pre-treatment forest loss\n",  year.start.fl, "-", year.end.fl)
  colname.fcAvg.new = paste0("Pre-treatment forest cover\n",  year.start.fc, "-", year.end.fc)
  c_name = data.frame(old = c(colname.travelTime, colname.clayContent, colname.tri, colname.elevation,
                              colname.fcAvg, colname.flAvg, colname.biome),
                      new = c("Accessibility", "Clay Content", "Terrain Ruggedness Index (TRI)", "Elevation", colname.fcAvg.new,
                              colname.flAvg.new, "Biomes"))
  
  # Refer to cobalt::love.plot()
  # https://cloud.r-project.org/web/packages/cobalt/vignettes/cobalt.html#love.plot
  fig_covbal = love.plot(out.cem, 
                       binary = "std", 
                       abs = T,
                       #thresholds = c(m = th_mean),
                       var.order = "unadjusted",
                       var.names = c_name,
                       title = paste0("Covariate balance"),
                       sample.names = c("Discarded", "Selected"),
                       alpha = .7,
                       wrap = 40 # at how many characters does axis label break to new line
  ) + 
    geom_vline(aes(xintercept=th_mean, linetype=paste0("Threshold\n(x=", th_mean, ")")), color=c("grey40"), linewidth=0.5) +
    labs(subtitle = paste("Protected area in", country.name, "WDPAID", wdpaid)) +
    theme_bw() +
    theme(
      plot.title = element_text(family="Arial Black", size=14, hjust=0),
      plot.subtitle = element_text(size=12, hjust=0),
      legend.title = element_blank(),
      legend.text=element_text(size=12),
      legend.spacing.x = unit(0.5, 'cm'),
      legend.spacing.y = unit(0.75, 'cm'),
      
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
      axis.text=element_text(size=10),
      axis.title=element_text(size=12),
      axis.title.y = element_text(margin = margin(unit = 'cm', r = 0.5)),
      axis.title.x = element_text(margin = margin(unit = 'cm', t = 0.5)),
      
      
      panel.grid.major.x = element_line(color = 'grey', linewidth = 0.3, linetype = 1),
      panel.grid.minor.x = element_line(color = 'grey', linewidth = 0.3, linetype = 2)
    ) + guides(linetype = guide_legend(override.aes = list(color = "grey40"))) # Add legend for geom_vline
  
  #Saving files
  
  ggsave(paste0(path_tmp, "/CovBal/fig_covbal", "_", iso, "_", wdpaid, ".png"),
         plot = fig_covbal,
         device = "png",
         height = 8, width = 12)
  print(xtable(tbl_cem_nn, type = "latex"),
        file = paste0(path_tmp, "/CovBal/tbl_cem_nn", "_", iso, "_", wdpaid, ".tex"))
  print(xtable(tbl_cem_m, type = "latex"),
        file = paste0(path_tmp, "/CovBal/tbl_cem_m", "_", iso, "_", wdpaid, ".tex"))
  print(xtable(tbl_cem_all, type = "latex"),
        file = paste0(path_tmp, "/CovBal/tbl_cem_all", "_", iso, "_", wdpaid, ".tex"))
  print(xtable(tbl.quality, type = "latex", auto = T),
        file =paste0(path_tmp, "/CovBal/tbl_quality_ct_", iso, "_", wdpaid, ".tex"))
  
  #Export to S3 storage
  ##List of files to save in the temp folder
  files <- list.files(paste(path_tmp, "CovBal", sep = "/"), full.names = TRUE)
  ##Add each file in the bucket (same foler for every file in the temp)
  for(f in files) 
  {
    cat("Uploading file", paste0("'", f, "'"), "\n")
    aws.s3::put_object(file = f, 
                       bucket = paste("projet-afd-eva-ap", save_dir, iso, wdpaid, "match_quality", sep = "/"),
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


#Density plots of covariates for control and treatment units, before and after matching
## INPUTS :
### out.cem : list of results from the CEM matching
### mf : the matching dataframe
### colname.travelTime, colname.clayContent, colname.elevation, colname.tri, colname.fcAvg, colname.flAvg : name of the matching covariates
### iso : ISO code of the country considered
### path_tmp : temporary folder to store figures
### wdpaid : the WDPA ID of the protected area considered
### log : a log file to track progress of the processing
### save_dir : saving directory
## OUTPUTS :
### is_ok : a boolean indicating whether or not an error occured inside the function
## DATA SAVED :
### Density plots of the matching covariates considered, for matched treated and control units
fn_post_plot_density = function(out.cem, mf, 
                                colname.travelTime, colname.clayContent, colname.fcAvg, colname.flAvg, colname.tri, colname.elevation, colname.biome,
                                iso, path_tmp, wdpaid, log, save_dir)
{
  output = tryCatch(
    
    {
      
  # Define Facet Labels
  fnl = c(`Unadjusted Sample` = "Before Matching",
          `Adjusted Sample` = "After Matching")
  
  #Extract country name
  country.name = mf %>% 
    filter(group == 3) %>% 
    slice(1)
  country.name = country.name$country_en
  
  #Define plots
  ## Density plot for Travel Time
  fig_travel = bal.plot(out.cem, 
                      var.name = colname.travelTime,
                      #sample.names = c("Control", "Treatment"),
                      which = "both",
                      type = "density",
                      disp.means = T,
                      mirror = F,
                      alpha.weight = T) +
    facet_wrap(.~which, labeller = as_labeller(fnl)) +
    #scale_fill_viridis(discrete = T) +
    scale_fill_discrete(labels = c("Control", "Treatment")) +
    #scale_fill_manual(labels = c("Control", "Treatment"), values = c("#f5b041","#5dade2")) +
    labs(title = "Distributional balance for accessibility",
         subtitle = paste0("Protected area in ", country.name, ", WDPAID ", wdpaid),
         x = "Travel time to nearest city (min)",
         caption = "Vertical lines represent the mean of their respective distribution",
         fill = "Group") +
    theme_bw() +
    theme(
      plot.title = element_text(family="Arial Black", size=16, hjust = 0),
      plot.caption = element_text(size = 10 ,hjust = 0),
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
                    #sample.names = c("Control", "Treatment"),
                    which = "both",
                    type = "density",
                    disp.means = T,
                    mirror = F,
                    alpha.weight = T) +
    facet_wrap(.~which, labeller = as_labeller(fnl)) +
    #scale_fill_viridis(discrete = T) +
    scale_fill_discrete(labels = c("Control", "Treatment")) +
    #scale_fill_manual(labels = c("Control", "Treatment"), values = c("#f5b041","#5dade2")) +
    labs(title = "Distributional balance for clay content",
         subtitle = paste0("Protected area in ", country.name, ", WDPAID ", wdpaid),
         x = "Clay content at 0-5cm soil depth (g/100g)",
         caption = "Vertical lines represent the mean of their respective distribution",
         fill = "Group") +
    theme_bw() +
    theme(
      plot.title = element_text(family="Arial Black", size=16, hjust = 0),
      plot.caption = element_text(size = 10 ,hjust = 0),
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
                      #sample.names = c("Control", "Treatment"),
                      which = "both",
                      type = "density",
                      disp.means = T,
                      mirror = F,
                      alpha.weight = T) +
      facet_wrap(.~which, labeller = as_labeller(fnl)) +
    #scale_fill_viridis(discrete = T) +
    scale_fill_discrete(labels = c("Control", "Treatment")) +
    #scale_fill_manual(labels = c("Control", "Treatment"), values = c("#f5b041","#5dade2")) +
      labs(title = "Distributional balance for elevation",
           subtitle = paste0("Protected area in ", country.name, ", WDPAID ", wdpaid),
           x = "Elevation (m)",
           caption = "Vertical lines represent the mean of their respective distribution",
           fill = "Group") +
    theme_bw() +
    theme(
      plot.title = element_text(family="Arial Black", size=16, hjust = 0),
      plot.caption = element_text(size = 10 ,hjust = 0),
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
                     #sample.names = c("Control", "Treatment"),
                     which = "both",
                     type = "density",
                     disp.means = T,
                     mirror = F,
                     alpha.weight = T) +
      facet_wrap(.~which, labeller = as_labeller(fnl)) +
    #scale_fill_viridis(discrete = T) +
    scale_fill_discrete(labels = c("Control", "Treatment")) +
    #scale_fill_manual(labels = c("Control", "Treatment"), values = c("#f5b041","#5dade2")) +
      labs(title = "Distributional balance for Terrain Ruggedness Index",
          subtitle = paste0("Protected area in ", country.name, ", WDPAID ", wdpaid),
           x = "Terrain Ruggedness Index (m)",
          caption = "Vertical lines represent the mean of their respective distribution",
          fill = "Group") +
    theme_bw() +
    theme(
      plot.title = element_text(family="Arial Black", size=16, hjust = 0),
      plot.caption = element_text(size = 10 ,hjust = 0),
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
                  var.name = colname.fcAvg,
                  #sample.names = c("Control", "Treatment"),
                  which = "both",
                  type = "density",
                  disp.means = T,
                  mirror = F,
                  alpha.weight = T) +
    facet_wrap(.~which, labeller = as_labeller(fnl)) +
    #scale_fill_viridis(discrete = T) +
    scale_fill_discrete(labels = c("Control", "Treatment")) +
    #scale_fill_manual(labels = c("Control", "Treatment"), values = c("#f5b041","#5dade2")) +
    labs(title = "Distributional balance for pre-treatment forest cover",
         subtitle = paste0("Protected area in ", country.name, ", WDPAID ", wdpaid),
         x = "Forest cover (ha)",
         caption = "Vertical lines represent the mean of their respective distribution.\nForest cover here is the average forest cover, up to three years before treatment.",
         fill = "Group") +
    theme_bw() +
    theme(
      plot.title = element_text(family="Arial Black", size=16, hjust = 0),
      plot.caption = element_text(size = 10 ,hjust = 0),
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
                  #sample.names = c("Control", "Treatment"),
                  which = "both",
                  type = "density",
                  disp.means = T,
                  mirror = F,
                  alpha.weight = T) +
    facet_wrap(.~which, labeller = as_labeller(fnl)) +
    #scale_fill_viridis(discrete = T) +
    scale_fill_discrete(labels = c("Control", "Treatment")) +
    #scale_fill_manual(labels = c("Control", "Treatment"), values = c("#f5b041","#5dade2")) +
    labs(title = "Distributional balance for average pre-treatment forest loss",
         subtitle = paste0("Protected area in ", country.name, ", WDPAID ", wdpaid),
         x = "Forest loss (ha)",
         caption = "Vertical lines represent the mean of their respective distribution.\nForest cover loss is the average annual forest cover loss, up to three years before treatment.",
         fill = "Group") +
    theme_bw() +
    theme(
      plot.title = element_text(family="Arial Black", size=16, hjust = 0),
      plot.caption = element_text(size = 10 ,hjust = 0),
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
  
  #Proportion of biomes : plotted only if biome is taken in the formula, not relevant otherwise
  formula = as.character(out.cem$formula)[3]
  if(grepl("biome", formula)) {
    
    
  fig_biome = bal.plot(out.cem, 
                    var.name = colname.biome,
                    #sample.names = c("Control", "Treatment"),
                    which = "both",
                    mirror = F,
                    alpha.weight = T) +
    coord_flip() +
    facet_wrap(.~which, labeller = as_labeller(fnl)) +
    #scale_fill_viridis(discrete = T) +
    scale_fill_discrete(labels = c("Control", "Treatment")) +
    #scale_fill_manual(labels = c("Control", "Treatment"), values = c("#f5b041","#5dade2")) +
    labs(title = "Distributional balance for biomes",
         subtitle = paste0("Protected area in ", country.name, ", WDPAID ", wdpaid),
         x = "",
         fill = "Group") +
    theme_bw() +
    theme(
      plot.title = element_text(family="Arial Black", size=16, hjust = 0),
      plot.caption = element_text(size = 10 ,hjust = 0),
      legend.title = element_blank(),
      legend.text=element_text(size=14),
      legend.spacing.x = unit(0.5, 'cm'),
      legend.spacing.y = unit(0.75, 'cm'),
      
      axis.text=element_text(size=10),
      axis.title=element_text(size=14),
      axis.title.y = element_text(margin = margin(unit = 'cm', r = 0.5)),
      axis.title.x = element_text(margin = margin(unit = 'cm', t = 0.5)),
      
      strip.text.x = element_text(size = 12), # Facet Label
      panel.spacing = unit(1, "lines")
    )
  
  tmp = paste(tempdir(), "fig", sep = "/")
  ggsave(paste(tmp, paste0("fig_biome_dplot_", iso, "_", wdpaid, ".png"), sep = "/"),
         plot = fig_biome,
         device = "png",
         height = 4, width = 10)
  
  } else print("Biome is not taken as a matching covariate : no density plot")
  
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
                       bucket = paste("projet-afd-eva-ap", save_dir, iso, wdpaid, "match_quality", sep = "/"),
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

#Histograms of covariates for control and treatment units, before and after matching
## INPUTS :
### out.cem : list of results from the CEM matching
### mf : the matching dataframe
### colname.travelTime, colname.clayContent, colname.elevation, colname.tri, colname.fcAvg, colname.flAvg : name of the matching covariates
### iso : ISO code of the country considered
### path_tmp : temporary folder to store figures
### wdpaid : the WDPA ID of the protected area considered
### log : a log file to track progress of the processing
### save_dir : saving directory
## OUTPUTS :
### is_ok : a boolean indicating whether or not an error occured inside the function
## DATA SAVED :
### Histograms of the matching covariates considered, for matched treated and control units
fn_post_plot_hist = function(out.cem, mf, 
                                colname.travelTime, colname.clayContent, colname.fcAvg, colname.flAvg, colname.tri, colname.elevation, colname.biome,
                                iso, path_tmp, wdpaid, log, save_dir)
{
  output = tryCatch(
    
    {
      
      # Define Facet Labels
      fnl = c(`Unadjusted Sample` = "Before Matching",
              `Adjusted Sample` = "After Matching")
      
      #Extract country name
      country.name = mf %>% 
        filter(group == 3) %>% 
        slice(1)
      country.name = country.name$country_en
      
      #Define plots
      ## Density plot for Travel Time
      fig_travel = bal.plot(out.cem, 
                            var.name = colname.travelTime,
                            #sample.names = c("Control", "Treatment"),
                            which = "both",
                            type = "histogram",
                            disp.means = T,
                            mirror = F,
                            alpha.weight = T) +
        facet_wrap(.~which, labeller = as_labeller(fnl)) +
        #scale_fill_viridis(discrete = T) +
        scale_fill_discrete(labels = c("Control", "Treatment")) +
        #scale_fill_manual(labels = c("Control", "Treatment"), values = c("#f5b041","#5dade2")) +
        labs(title = "Distributional balance for accessibility",
             subtitle = paste0("Protected area in ", country.name, ", WDPAID ", wdpaid),
             x = "Travel time to nearest city (min)",
             caption = "Vertical lines represent the mean of their respective distribution",
             fill = "Group") +
        theme_bw() +
        theme(
          plot.title = element_text(family="Arial Black", size=16, hjust = 0),
          plot.caption = element_text(size = 10 ,hjust = 0),
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
                          #sample.names = c("Control", "Treatment"),
                          which = "both",
                          type = "histogram",
                          disp.means = T,
                          mirror = F,
                          alpha.weight = T) +
        facet_wrap(.~which, labeller = as_labeller(fnl)) +
        #scale_fill_viridis(discrete = T) +
        scale_fill_discrete(labels = c("Control", "Treatment")) +
        #scale_fill_manual(labels = c("Control", "Treatment"), values = c("#f5b041","#5dade2")) +
        labs(title = "Distributional balance for clay content",
             subtitle = paste0("Protected area in ", country.name, ", WDPAID ", wdpaid),
             x = "Clay content at 0-5cm soil depth (g/100g)",
             caption = "Vertical lines represent the mean of their respective distribution",
             fill = "Group") +
        theme_bw() +
        theme(
          plot.title = element_text(family="Arial Black", size=16, hjust = 0),
          plot.caption = element_text(size = 10 ,hjust = 0),
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
                               #sample.names = c("Control", "Treatment"),
                               which = "both",
                               type = "histogram",
                               disp.means = T,
                               mirror = F,
                               alpha.weight = T) +
        facet_wrap(.~which, labeller = as_labeller(fnl)) +
        #scale_fill_viridis(discrete = T) +
        scale_fill_discrete(labels = c("Control", "Treatment")) +
        #scale_fill_manual(labels = c("Control", "Treatment"), values = c("#f5b041","#5dade2")) +
        labs(title = "Distributional balance for elevation",
             subtitle = paste0("Protected area in ", country.name, ", WDPAID ", wdpaid),
             x = "Elevation (m)",
             caption = "Vertical lines represent the mean of their respective distribution",
             fill = "Group") +
        theme_bw() +
        theme(
          plot.title = element_text(family="Arial Black", size=16, hjust = 0),
          plot.caption = element_text(size = 10 ,hjust = 0),
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
                         #sample.names = c("Control", "Treatment"),
                         which = "both",
                         type = "histogram",
                         disp.means = T,
                         mirror = F,
                         alpha.weight = T) +
        facet_wrap(.~which, labeller = as_labeller(fnl)) +
        #scale_fill_viridis(discrete = T) +
        scale_fill_discrete(labels = c("Control", "Treatment")) +
        #scale_fill_manual(labels = c("Control", "Treatment"), values = c("#f5b041","#5dade2")) +
        labs(title = "Distributional balance for Terrain Ruggedness Index",
             subtitle = paste0("Protected area in ", country.name, ", WDPAID ", wdpaid),
             x = "Terrain Ruggedness Index (m)",
             caption = "Vertical lines represent the mean of their respective distribution",
             fill = "Group") +
        theme_bw() +
        theme(
          plot.title = element_text(family="Arial Black", size=16, hjust = 0),
          plot.caption = element_text(size = 10 ,hjust = 0),
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
                        var.name = colname.fcAvg,
                        #sample.names = c("Control", "Treatment"),
                        which = "both",
                        type = "histogram",
                        disp.means = T,
                        mirror = F,
                        alpha.weight = T) +
        facet_wrap(.~which, labeller = as_labeller(fnl)) +
        #scale_fill_viridis(discrete = T) +
        scale_fill_discrete(labels = c("Control", "Treatment")) +
        #scale_fill_manual(labels = c("Control", "Treatment"), values = c("#f5b041","#5dade2")) +
        labs(title = "Distributional balance for pre-treatment forest cover",
             subtitle = paste0("Protected area in ", country.name, ", WDPAID ", wdpaid),
             x = "Forest cover (ha)",
             caption = "Vertical lines represent the mean of their respective distribution.\nForest cover here is the average forest cover, up to three years before treatment.",
             fill = "Group") +
        theme_bw() +
        theme(
          plot.title = element_text(family="Arial Black", size=16, hjust = 0),
          plot.caption = element_text(size = 10 ,hjust = 0),
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
                        #sample.names = c("Control", "Treatment"),
                        which = "both",
                        type = "histogram",
                        disp.means = T,
                        mirror = F,
                        alpha.weight = T) +
        facet_wrap(.~which, labeller = as_labeller(fnl)) +
        #scale_fill_viridis(discrete = T) +
        scale_fill_discrete(labels = c("Control", "Treatment")) +
        #scale_fill_manual(labels = c("Control", "Treatment"), values = c("#f5b041","#5dade2")) +
        labs(title = "Distributional balance for average pre-treatment forest loss",
             subtitle = paste0("Protected area in ", country.name, ", WDPAID ", wdpaid),
             x = "Forest loss (ha)",
             caption = "Vertical lines represent the mean of their respective distribution.\nForest loss is the average annual forest cover loss, up to three years before treatment.",
             fill = "Group") +
        theme_bw() +
        theme(
          plot.title = element_text(family="Arial Black", size=16, hjust = 0),
          plot.caption = element_text(size = 10 ,hjust = 0),
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
      
      #/!\ Biome already plotted with fn_post_plot_density : no need to plot it again
      useless_var = colname.biome #To avoid error if colname.biome not used in the function
      # #Proportion of biomes : plotted only if biome is taken in the formula, not relevant otherwise
      # formula = as.character(out.cem$formula)[3]
      # if(grepl("biome", formula)) {
      #   
      #   
      #   fig_biome = bal.plot(out.cem, 
      #                        var.name = colname.biome,
      #                        #sample.names = c("Control", "Treatment"),
      #                        which = "both",
      #                        mirror = F,
      #                        alpha.weight = T) +
      #     facet_wrap(.~which, labeller = as_labeller(fnl)) +
      #     #scale_fill_viridis(discrete = T) +
      #     scale_fill_discrete(labels = c("Control", "Treatment")) +
      #     #scale_fill_manual(labels = c("Control", "Treatment"), values = c("#f5b041","#5dade2")) +
      #     labs(title = "Distributional balance for biomes",
      #          subtitle = paste0("Protected area in ", country.name, ", WDPAID ", wdpaid),
      #          x = "",
      #          fill = "Group") +
      #     theme_bw() +
      #     theme(
      #       plot.title = element_text(family="Arial Black", size=16, hjust=0),
      #       legend.title = element_blank(),
      #       legend.text=element_text(size=14),
      #       legend.spacing.x = unit(0.5, 'cm'),
      #       legend.spacing.y = unit(0.75, 'cm'),
      #       
      #       axis.text=element_text(size=12),
      #       axis.text.x = element_text(angle = 90),
      #       axis.title=element_text(size=14),
      #       axis.title.y = element_text(margin = margin(unit = 'cm', r = 0.5)),
      #       axis.title.x = element_text(margin = margin(unit = 'cm', t = 0.5)),
      #       
      #       strip.text.x = element_text(size = 12) # Facet Label
      #     )
      #   
      #   tmp = paste(tempdir(), "fig", sep = "/")
      #   ggsave(paste(tmp, paste0("fig_biome_dplot_", iso, "_", wdpaid, ".png"), sep = "/"),
      #          plot = fig_biome,
      #          device = "png",
      #          height = 6, width = 9)
      #   
      # } else print("Biome is not taken as a matching covariate : no histogram")
      
      #Saving plots
      
      tmp = paste(tempdir(), "fig", sep = "/")
      ggsave(paste(tmp, paste0("fig_travel_hist_", iso, "_", wdpaid, ".png"), sep = "/"),
             plot = fig_travel,
             device = "png",
             height = 6, width = 9)
      ggsave(paste(tmp, paste0("fig_clay_hist_", iso, "_", wdpaid, ".png"), sep = "/"),
             plot = fig_clay,
             device = "png",
             height = 6, width = 9)
      ggsave(paste(tmp, paste0("fig_elevation_hist_", iso, "_", wdpaid, ".png"), sep = "/"),
             plot = fig_elevation,
             device = "png",
             height = 6, width = 9)
      ggsave(paste(tmp, paste0("fig_tri_hist_", iso, "_", wdpaid, ".png"), sep = "/"),
             plot = fig_tri,
             device = "png",
             height = 6, width = 9)
      ggsave(paste(tmp, paste0("fig_fc_hist_", iso, "_", wdpaid, ".png"), sep = "/"),
             plot = fig_fc,
             device = "png",
             height = 6, width = 9)
      ggsave(paste(tmp, paste0("fig_fl_hist_", iso, "_", wdpaid, ".png"), sep = "/"),
             plot = fig_fl,
             device = "png",
             height = 6, width = 9)
      
      files <- list.files(tmp, full.names = TRUE)
      ##Add each file in the bucket (same foler for every file in the temp)
      for(f in files) 
      {
        cat("Uploading file", paste0("'", f, "'"), "\n")
        aws.s3::put_object(file = f, 
                           bucket = paste("projet-afd-eva-ap", save_dir, iso, wdpaid, "match_quality", sep = "/"),
                           region = "", show_progress = TRUE)
      }
      do.call(file.remove, list(list.files(tmp, full.names = TRUE)))
      
      #Append the log
      cat("#Plot covariates histograms\n->OK\n", file = log, append = TRUE)
      
      return(list("is_ok" = TRUE))
      
    },
    
    error=function(e)
    {
      print(e)
      cat(paste("#Plot covariates histograms\n-> Error :\n", e, "\n"), file = log, append = TRUE)
      return(list("is_ok" = FALSE))
    }
    
    # warning = function(w)
    # {
    #   #Print the warning and append the log
    #   print(w)
    #   #Append the log 
    #   cat(paste("#Plot covariates histograms\n-> Warning :\n", w, "\n"), file = log, append = TRUE)
    #   #Return string to inform user to skip
    #   return(list("is_ok" = TRUE))
    # }
    
  )
  
  return(output)
  
}


#Define panel datasets (long, wide format) for control and treatment observation units, before and after matching.
## INPUTS :
### out.cem : list of results from the CEM matching
### mf : the matching dataframe
### iso : ISO code of the country considered
### wdpaid : the WDPA ID of the protected area considered
### log : a log file to track progress of the processing
### save_dir : saving directory
## OUTPUTS :
### a list of dataframes : (un)matched.wide/long. They contain covariates and outcomes for treatment and control units, before and after matching, in a wide or long format
### is_ok : a boolean indicating whether or not an error occured inside the function
## DATA SAVED
### (un)matched.wide/long dataframes. They contain covariates and outcomes for treatment and control units, before and after matching, in a wide or long format

fn_post_panel = function(out.cem, mf, wdpaid, iso, log, save_dir)
{
  
  output = tryCatch(
    
    {
      
  # Convert dataframe of matched objects to pivot wide form
  matched.wide = match.data(object=out.cem, data=mf) %>%
    st_drop_geometry()
  
  
  # Pivot Wide ==> Pivot Long
  matched.long = matched.wide %>%
    dplyr::select(c(region_afd, region, sub_region, country_en, iso3, group, focus, wdpaid, status_yr, year_funding_first, year_funding_all, assetid, res_m, minutes_mean_5k_110mio, clay_0_5cm_mean, elevation_mean, tri_mean, biomes, starts_with("treecover"), avgLoss_pre_treat, avgCover_pre_treat, start_pre_treat_fl, end_pre_treat_fl , start_pre_treat_fc, end_pre_treat_fc)) %>%
    pivot_longer(cols = c(starts_with("treecover")),
                 names_to = c("var", "year"),
                 names_sep = "_",
                 values_to = "fc_ha") %>%
    st_drop_geometry()
  
  # Pivot wide Dataframe of un-matched objects
  unmatched.wide = mf %>%
    st_drop_geometry()
  
  # Pivot Wide ==> Pivot Long
  unmatched.long = unmatched.wide %>%
    dplyr::select(c(region_afd, region, sub_region, country_en, iso3, group, focus, wdpaid, status_yr, year_funding_first, year_funding_all, assetid, res_m, minutes_mean_5k_110mio, clay_0_5cm_mean, elevation_mean, tri_mean, biomes, starts_with("treecover"), avgLoss_pre_treat, avgCover_pre_treat, start_pre_treat_fl, end_pre_treat_fl , start_pre_treat_fc, end_pre_treat_fc)) %>%    pivot_longer(cols = c(starts_with("treecover")),
                 names_to = c("var", "year"),
                 names_sep = "_",
                 values_to = "fc_ha") %>%
    st_drop_geometry()
  
  #Save the dataframes
  s3write_using(matched.wide,
                sf::st_write,
                object = paste0(save_dir, "/", iso, "/", wdpaid, "/", paste0("matched_wide", "_", iso, "_", wdpaid, ".csv")),
                bucket = "projet-afd-eva-ap",
                opts = list("region" = ""))
  s3write_using(unmatched.wide,
                sf::st_write,
                object = paste0(save_dir, "/", iso, "/", wdpaid, "/", paste0("unmatched_wide", "_", iso, "_", wdpaid, ".csv")),
                bucket = "projet-afd-eva-ap",
                opts = list("region" = ""))
  s3write_using(matched.long,
                sf::st_write,
                object = paste0(save_dir, "/", iso, "/", wdpaid, "/", paste0("matched_long", "_", iso, "_", wdpaid, ".csv")),
                bucket = "projet-afd-eva-ap",
                opts = list("region" = ""))
  s3write_using(unmatched.long,
                sf::st_write,
                object = paste0(save_dir, "/", iso, "/", wdpaid, "/", paste0("unmatched_long", "_", iso, "_", wdpaid, ".csv")),
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


#Assess the difference between matched and unmatched treated units
## INPUTS
### df_m : dataframe (df) of matched units, in a wide format (one row = one observation unit)
### df_unm : same as df_m, for unmatched units
### iso : ISO code of the country considered
### wdpaid : the WDPA ID of the protected area considered
### th_mean :the maximum acceptable value for absolute standardized mean difference of covariates between matched treated and control units. Typically 0.1 (https://cran.r-project.org/web/packages/MatchIt/vignettes/assessing-balance.html) or 0.25 in conservation literature (e.g https://conbio.onlinelibrary.wiley.com/doi/abs/10.1111/cobi.13728) 
### th_var_min, th_var_max : the range of acceptable value for covariate variance ratio between matched treated and control units. Typicall 0.5 and 2, respectively (https://cran.r-project.org/web/packages/MatchIt/vignettes/assessing-balance.html)
### log : a log file to track progress of the processing
### save_dir : saving directory
## OUTPUTS
### is_ok : a boolean indicating whether or not an error occured inside the function
## DATA SAVED :
### Histograms + density plots of the matching covariates considered, for matched treated and unmatched treated units
fn_post_m_unm_treated = function(df_m, df_unm, iso, wdpaid, th_mean, th_var_min, th_var_max, save_dir, log)
{
  output = tryCatch(
    
    {
      #A dataframe with matched and unmatched units
      df = rbind(mutate(df_m, is_m = "Matched"), 
                 mutate(df_unm, is_m = "Unmatched", weights = 1, subclass = 1)) %>%
        filter(group == 3 & wdpaid == wdpaid) %>%
        st_drop_geometry() %>%
        as.data.frame() %>%
        #Add average of each covariate for matched and unmatched treated units, for latter plotting
        group_by(is_m) %>%
        mutate(avg_clay = mean(clay_0_5cm_mean, na.rm = TRUE),
               avg_fl = mean(avgLoss_pre_treat, na.rm = TRUE),
               avg_fc = mean(avgCover_pre_treat, na.rm = TRUE),
               avg_travel =  mean(minutes_mean_5k_110mio, na.rm = TRUE),
               avg_tri =  mean(tri_mean, na.rm = TRUE),
               avg_elevation = mean(elevation_mean, na.rm = TRUE)) %>%
        ungroup()
      
      #Matched units
      ##Average values of covariates
      df_mean_m = df_m %>%
        st_drop_geometry() %>%
        filter(group == 3 & wdpaid == wdpaid) %>% #Select treated units only
        summarize(minutes_mean_5k_110mio = mean(minutes_mean_5k_110mio, na.rm = TRUE), #Compute relevant measures
                  avgLoss_pre_treat = mean(avgLoss_pre_treat, na.rm = TRUE), 
                  clay_0_5cm_mean = mean(clay_0_5cm_mean, na.rm = TRUE),
                  elevation_mean = mean(elevation_mean, na.rm = TRUE),
                  tri_mean = mean(tri_mean, na.rm = TRUE),
                  avgCover_pre_treat = mean(avgCover_pre_treat, na.rm = TRUE)) %>%
        t() %>% #Move to long dataframe
        as.data.frame() %>%
        rename("mean_m" = "V1")#Change name of the column
      ##Standard deviations of covariates
      df_sd_m = df_m %>%
        st_drop_geometry() %>%
        filter(group == 3 & wdpaid == wdpaid) %>% #Select treated units only
        summarize(minutes_mean_5k_110mio = sd(minutes_mean_5k_110mio, na.rm = TRUE),
                  avgLoss_pre_treat = sd(avgLoss_pre_treat, na.rm = TRUE),
                  clay_0_5cm_mean = sd(clay_0_5cm_mean, na.rm = TRUE),
                  elevation_mean = sd(elevation_mean, na.rm = TRUE),
                  tri_mean = sd(tri_mean, na.rm = TRUE),
                  avgCover_pre_treat = sd(avgCover_pre_treat, na.rm = TRUE)) %>%
        t() %>%
        as.data.frame() %>%
        rename("sd_m" = "V1") 
      
      #Unmatched units
      ##Average of covariates
      df_mean_unm = df_unm %>%
        st_drop_geometry() %>%
        filter(group == 3 & wdpaid == wdpaid) %>% #Select treated units only
        summarize(minutes_mean_5k_110mio = mean(minutes_mean_5k_110mio, na.rm = TRUE),
                  avgLoss_pre_treat = mean(avgLoss_pre_treat, na.rm = TRUE),
                  clay_0_5cm_mean = mean(clay_0_5cm_mean, na.rm = TRUE),
                  elevation_mean = mean(elevation_mean, na.rm = TRUE),
                  tri_mean = mean(tri_mean, na.rm = TRUE),
                  avgCover_pre_treat = mean(avgCover_pre_treat, na.rm = TRUE)) %>%
        t() %>%
        as.data.frame() %>%
        rename("mean_unm" = "V1") 
      ##Standard deviation of covariates
      df_sd_unm = df_unm %>%
        st_drop_geometry() %>%
        filter(group == 3 & wdpaid == wdpaid) %>% #Select treated units only
        summarize(minutes_mean_5k_110mio = sd(minutes_mean_5k_110mio, na.rm = TRUE),
                  avgLoss_pre_treat = sd(avgLoss_pre_treat, na.rm = TRUE),
                  clay_0_5cm_mean = sd(clay_0_5cm_mean, na.rm = TRUE),
                  elevation_mean = sd(elevation_mean, na.rm = TRUE),
                  tri_mean = sd(tri_mean, na.rm = TRUE),
                  tavgCover_pre_treat = sd(avgCover_pre_treat, na.rm = TRUE)) %>%
        t() %>%
        as.data.frame() %>%
        rename("sd_unm" = "V1") 
      
      #Get row names 
      covariate = row.names(df_mean_m)
      #Get relevant PA info to add in the quality table
      df_info = df %>%
        filter(group == 3 & wdpaid == wdpaid) %>%
        slice(1)
      region = df_info$region
      sub_region = df_info$sub_region
      country_en = df_info$country_en
      wdpaid = df_info$wdpaid
      #Gathering in one table and computing variance ratio, absolute standardized mean difference (SDM), and comparing to pre-defined thresholds
      tbl.quality = cbind(covariate, df_mean_m, df_mean_unm, df_sd_m, df_sd_unm) %>%
        rowwise() %>%
        mutate(std_mean_diff = abs((mean_m - mean_unm)/(sqrt((sd_m^2+sd_unm^2)/2)))) %>% #SDM from https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3144483/#s11title or 10.1080/00273171.2011.568786
        mutate(var_ratio = (sd_m/sd_unm)^2) %>% #Variance ratio
        ungroup() %>%
        mutate(th_var_min = th_var_min,
               th_var_max = th_var_max,
               th_mean = th_mean,
               is_var_ok = var_ratio < th_var_max & var_ratio > th_var_min, #Check variance ratio between treated and controls
               is_mean_ok = abs(std_mean_diff) < th_mean, #Check absolute standardized mean difference
               is_bal_ok = as.logical(is_var_ok*is_mean_ok), #Binary : TRUE if both variance and mean difference check pass, 0 if at least one does not
               .after = "var_ratio") %>%
        mutate(region = region,
               sub_region = sub_region,
               iso3 = iso,
               country_en = country_en,
               wdpaid = wdpaid,
               .before = "covariate")
      
      fig_clay = ggplot(data = df, aes(x = clay_0_5cm_mean, fill = is_m)) %>%
        + geom_histogram(aes(y = ..density..), 
                       #bins = 30, 
                       color = "black") %>%
        + geom_density(alpha = 0.5) %>%
        + geom_vline(aes(xintercept = avg_clay, color = is_m), linetype = 1, linewidth = 1, alpha = .6) %>%
        + labs(title = "Distribution of clay content among treated units",
               subtitle = paste0("Protected area in ", country_en, ", WDPAID ", wdpaid),
               caption = "Vertical lines represent the mean of their respective distribution",
               y = "Density",
               x = "Mean clay content (0-5 cm)") %>%
        + scale_color_brewer(name = "", palette = "Paired") %>%
        + scale_fill_brewer(name = "", palette = "Paired") %>%
        + theme_bw() %>%
        + theme(
          plot.title = element_text(family="Arial Black", size=16, hjust = 0),
          plot.caption = element_text(size = 10 ,hjust = 0),
          
          #legend.title = element_blank(),
          #legend.text=element_text(size=14),
          #legend.spacing.x = unit(0.5, 'cm'),
          #legend.spacing.y = unit(0.75, 'cm'),
          
          axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          axis.title.y = element_text(margin = margin(unit = 'cm', r = 0.5)),
          axis.title.x = element_text(margin = margin(unit = 'cm', t = 0.5)),
          
          strip.text.x = element_text(size = 12) # Facet Label
        ) %>%
        + guides(col = "none")
        
      fig_travel = ggplot(data = df,
                          aes(x = minutes_mean_5k_110mio, fill = is_m)) %>%
        + geom_histogram(aes(y = ..density..), 
                         #bins = 30, 
                         color = "black") %>%
        + geom_density(alpha = 0.5) %>%
        + geom_vline(aes(xintercept = avg_clay, color = is_m), linetype = 1, linewidth = 1, alpha = .6) %>%
        + labs(title = "Distribution of travel time to nearest city among treated units",
               subtitle = paste0("Protected area in ", country_en, ", WDPAID ", wdpaid),
               caption = "Vertical lines represent the mean of their respective distribution",
               y = "Density",
               x = "Travel time to nearest city (min)") %>%
        + scale_color_brewer(name = "", palette = "Paired") %>%
        + scale_fill_brewer(name = "", palette = "Paired") %>%
        + theme_bw() %>%
        + theme(
          plot.title = element_text(family="Arial Black", size=16, hjust = 0),
          plot.caption = element_text(size = 10 ,hjust = 0),
          
          #legend.title = element_blank(),
          #legend.text=element_text(size=14),
          #legend.spacing.x = unit(0.5, 'cm'),
          #legend.spacing.y = unit(0.75, 'cm'),
          
          axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          axis.title.y = element_text(margin = margin(unit = 'cm', r = 0.5)),
          axis.title.x = element_text(margin = margin(unit = 'cm', t = 0.5)),
          
          strip.text.x = element_text(size = 12) # Facet Label
        ) %>%
        + guides(col = "none")
      
      fig_elevation = ggplot(data = df,
                             aes(x = elevation_mean, fill = is_m)) %>%
        + geom_histogram(aes(y = ..density..), 
                         #bins = 30, 
                         color = "black") %>%
        + geom_density(alpha = 0.5) %>%
        + geom_vline(aes(xintercept = avg_elevation, color = is_m), linetype = 1, linewidth = 1, alpha = .6) %>%
        + labs(title = "Distribution of elevation among treated units",
               subtitle = paste0("Protected area in ", country_en, ", WDPAID ", wdpaid),
               caption = "Vertical lines represent the mean of their respective distribution",
               y = "Density",
               x = "Elevation (m)") %>%
        + scale_color_brewer(name = "", palette = "Paired") %>%
        + scale_fill_brewer(name = "", palette = "Paired") %>%
        + theme_bw() %>%
        + theme(
          plot.title = element_text(family="Arial Black", size=16, hjust = 0),
          plot.caption = element_text(size = 10 ,hjust = 0),
          
          #legend.title = element_blank(),
          #legend.text=element_text(size=14),
          #legend.spacing.x = unit(0.5, 'cm'),
          #legend.spacing.y = unit(0.75, 'cm'),
          
          axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          axis.title.y = element_text(margin = margin(unit = 'cm', r = 0.5)),
          axis.title.x = element_text(margin = margin(unit = 'cm', t = 0.5)),
          
          strip.text.x = element_text(size = 12) # Facet Label
        ) %>%
        + guides(col = "none")
      
      fig_loss = ggplot(data = df,
                        aes(x = avgLoss_pre_treat, fill = is_m)) %>%
        + geom_histogram(aes(y = ..density..), 
                         #bins = 30, 
                         color = "black") %>%
        + geom_density(alpha = 0.5) %>%
        + geom_vline(aes(xintercept = avg_fl, color = is_m), linetype = 1, linewidth = 1, alpha = .6) %>%
        + labs(title = "Distribution of pre-treatment forest cover loss, among treated units",
               subtitle = paste0("Protected area in ", country_en, ", WDPAID ", wdpaid),
               y = "Density",
               caption = "Vertical lines represent the mean of their respective distribution.\nForest cover loss is the average annual forest cover loss, up to three years before treatment.",
               x = "Forest cover loss (ha)") %>%
        + scale_color_brewer(name = "", palette = "Paired") %>%
        + scale_fill_brewer(name = "", palette = "Paired") %>%
        + theme_bw() %>%
        + theme(
          plot.title = element_text(family="Arial Black", size=16, hjust = 0),
          plot.caption = element_text(size = 10 ,hjust = 0),
          
          #legend.title = element_blank(),
          #legend.text=element_text(size=14),
          #legend.spacing.x = unit(0.5, 'cm'),
          #legend.spacing.y = unit(0.75, 'cm'),
          
          axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          axis.title.y = element_text(margin = margin(unit = 'cm', r = 0.5)),
          axis.title.x = element_text(margin = margin(unit = 'cm', t = 0.5)),
          
          strip.text.x = element_text(size = 12) # Facet Label
        ) %>%
        + guides(col = "none")
      
      fig_fc = ggplot(data = df,
                          aes(x = avgCover_pre_treat, fill = is_m)) %>%
        + geom_histogram(aes(y = ..density..), 
                         #bins = 30, 
                         color = "black") %>%
        + geom_density(alpha = 0.5) %>%
        + geom_vline(aes(xintercept = avg_fc, color = is_m), linetype = 1, linewidth = 1, alpha = .6) %>%
        + labs(title = "Distribution of pre-treatment forest cover, among treated units",
               subtitle = paste0("Protected area in ", country_en, ", WDPAID ", wdpaid),
               caption = "Vertical lines represent the mean of their respective distribution.\nForest cover here is the average forest cover, up to three years before treatment.",
               y = "Density",
               x = "Forest cover (ha)") %>%
        + scale_color_brewer(name = "", palette = "Paired") %>%
        + scale_fill_brewer(name = "", palette = "Paired") %>%
        + theme_bw() %>%
        + theme(
          plot.title = element_text(family="Arial Black", size=16, hjust = 0),
          plot.caption = element_text(size = 10 ,hjust = 0),
          
          #legend.title = element_blank(),
          #legend.text=element_text(size=14),
          #legend.spacing.x = unit(0.5, 'cm'),
          #legend.spacing.y = unit(0.75, 'cm'),
          
          axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          axis.title.y = element_text(margin = margin(unit = 'cm', r = 0.5)),
          axis.title.x = element_text(margin = margin(unit = 'cm', t = 0.5)),
          
          strip.text.x = element_text(size = 12) # Facet Label
        ) %>%
        + guides(col = "none")
      
      fig_tri = ggplot(data = df,
                       aes(x = tri_mean, fill = is_m)) %>%
        + geom_histogram(aes(y = ..density..), 
                         #bins = 30, 
                         color = "black") %>%
        + geom_density(alpha = 0.5) %>%
        + geom_vline(aes(xintercept = avg_tri, color = is_m), linetype = 1, linewidth = 1, alpha = .6) %>%
        + labs(title = "Distribution of soil ruggedness among treated units",
               subtitle = paste0("Protected area in ", country_en, ", WDPAID ", wdpaid),
               caption = "Vertical lines represent the mean of their respective distribution",
               y = "Density",
               x = "Terrain Ruggedness Index (m)") %>%
        + scale_color_brewer(name = "", palette = "Paired") %>%
        + scale_fill_brewer(name = "", palette = "Paired") %>%
        + theme_bw() %>%
        + theme(
          plot.title = element_text(family="Arial Black", size=16, hjust = 0),
          plot.caption = element_text(size = 10 ,hjust = 0),
          
          #legend.title = element_blank(),
          #legend.text=element_text(size=14),
          #legend.spacing.x = unit(0.5, 'cm'),
          #legend.spacing.y = unit(0.75, 'cm'),
          
          axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          axis.title.y = element_text(margin = margin(unit = 'cm', r = 0.5)),
          axis.title.x = element_text(margin = margin(unit = 'cm', t = 0.5)),
          
          strip.text.x = element_text(size = 12) # Facet Label
        ) %>%
        + guides(col = "none")
      
      # df_biome = df %>%
      #   group_by(is_m, biomes) %>%
      #   summarize(n_biome = n()) %>%
      #   ungroup() %>%
      #   group_by(is_m) %>%
      #   mutate(freq = n_biome/sum(n_biome))

      fig_biome = ggplot(data = df,
                         aes(y = biomes, fill = is_m)) %>%
        + geom_bar(aes(x = ..prop..), stat = "count", position = "dodge") %>%
        + labs(title = "Distribution of biomes among treated units",
               subtitle = paste0("Protected area in ", country_en, ", WDPAID ", wdpaid),
               x = "Proportion",
               y = "") %>%
        + scale_color_brewer(name = "", palette = "Paired") %>%
        + scale_fill_brewer(name = "", palette = "Paired") %>%
        + theme_bw() %>%
        + theme(
          plot.title = element_text(family="Arial Black", size=16, hjust = 0),
          plot.caption = element_text(size = 10 ,hjust = 0),
          #legend.title = element_blank(),
          #legend.text=element_text(size=14),
          #legend.spacing.x = unit(0.5, 'cm'),
          #legend.spacing.y = unit(0.75, 'cm'),

          axis.text=element_text(size=10),
          axis.title=element_text(size=14),
          axis.title.y = element_text(margin = margin(unit = 'cm', r = 0.5)),
          axis.title.x = element_text(margin = margin(unit = 'cm', t = 0.5)),

          strip.text.x = element_text(size = 12) # Facet Label
        )
      
      #Saving plots
      tmp = paste(tempdir(), "fig", sep = "/")
      ggsave(paste(tmp, paste0("fig_travel_dplot_treated_", iso, "_", wdpaid, ".png"), sep = "/"),
             plot = fig_travel,
             device = "png",
             height = 6, width = 9)
      ggsave(paste(tmp, paste0("fig_clay_dplot_treated_", iso, "_", wdpaid, ".png"), sep = "/"),
             plot = fig_clay,
             device = "png",
             height = 6, width = 9)
      ggsave(paste(tmp, paste0("fig_elevation_dplot_treated_", iso, "_", wdpaid, ".png"), sep = "/"),
             plot = fig_elevation,
             device = "png",
             height = 6, width = 9)
      ggsave(paste(tmp, paste0("fig_tri_dplot_treated_", iso, "_", wdpaid, ".png"), sep = "/"),
             plot = fig_tri,
             device = "png",
             height = 6, width = 9)
      ggsave(paste(tmp, paste0("fig_fc_dplot_treated_", iso, "_", wdpaid, ".png"), sep = "/"),
             plot = fig_fc,
             device = "png",
             height = 6, width = 9)
      ggsave(paste(tmp, paste0("fig_fl_dplot_treated_", iso, "_", wdpaid, ".png"), sep = "/"),
             plot = fig_loss,
             device = "png",
             height = 6, width = 9)
      ggsave(paste(tmp, paste0("fig_biome_dplot_treated_", iso, "_", wdpaid, ".png"), sep = "/"),
             plot = fig_biome,
             device = "png",
             height = 4, width = 10)
      
      print(xtable(tbl.quality, type = "latex", auto = T),
            file = paste(tmp, paste0("tbl_quality_tt_", iso, "_", wdpaid, ".tex"), sep = "/"))
      
      files <- list.files(tmp, full.names = TRUE)
      ##Add each file in the bucket (same foler for every file in the temp)
      for(f in files) 
      {
        cat("Uploading file", paste0("'", f, "'"), "\n")
        aws.s3::put_object(file = f, 
                           bucket = paste("projet-afd-eva-ap", save_dir, iso, wdpaid, "match_quality", sep = "/"),
                           region = "", show_progress = TRUE)
      }
      do.call(file.remove, list(list.files(tmp, full.names = TRUE)))

      
      #Append the log
      cat("#Assess difference of matched and unmatched units\n-> OK\n", file = log, append = TRUE)
      
      #Return outputs
      list_output = list("tbl.quality" = tbl.quality, "is_ok" = TRUE)
      return(list_output)
      
    },
    error=function(e)
    {
      print(e)
      cat(paste("#Assess difference of matched and unmatched units\n-> Error :\n", e, "\n"), file = log, append = TRUE)
      return(list("is_ok" = FALSE))
    }
  )
  
  return(output)
    
}


#Plot the average trend of control and treated units in a given country, before and after the matching
## INPUTS :
### (un)matched.long : dataframe with covariates and outcomes for each treatment and control unit, before and after matching, in a long format (one row : pixel+year)
### mf : the matching dataframe
### data_pa : dataframe with information on each PA considered in the analysis
### iso : ISO code of the country considered
### wdpaid : the WDPA ID of the protected area considered
### log : a log file to track progress of the processing
### save_dir : saving directory
## OUTPUTS : 
### is_ok : a boolean indicating whether or not an error occured inside the function
## DATA SAVED :
### Evolution of forest cover in a treated and control pixel on average, before and after matching
### Same for total forest cover (pixel*# of pixels in the PA)
### Cumulated deforestation relative to 2000 forest cover, in treated and control pixels, before and after matching
fn_post_plot_trend = function(matched.long, unmatched.long, mf, data_pa, iso, wdpaid, log, save_dir) 
{
    
  output = tryCatch(
    
    {
      #First extract some relevant information
      #Extract spatial resolution of pixels res_m and define pixel area in ha
      res_m = unique(mf$res_m)
      res_ha = res_m^2*1e-4
        
      #Extract treatment year
      treatment.year = mf %>% 
        filter(group == 3) %>% 
        slice(1)
      treatment.year = treatment.year$status_yr
      
      #Extract funding years
      funding.years = mf %>% 
        filter(group == 3) %>% 
        slice(1)
      funding.years = funding.years$year_funding_first
      #funding.years = as.numeric(unlist(strsplit(funding.years$year_funding_all, split = ",")))
      
      #Extract period where forest cover 
      
      #Extract country name
      country.name = mf %>% 
        filter(group == 3) %>% 
        slice(1)
      country.name = country.name$country_en
      
      ##Area of the PA
      ###As reported in the WDPA
      wdpa_id = wdpaid #Need to give a name to wdpaid (function argument) different from the varaible in the dataset (wdpaid)
      area_ha = data_pa[data_pa$wdpaid == wdpa_id,]$area_km2*100
      
      ## Name of the PA
      pa.name = data_pa %>% 
        filter(wdpaid == wdpa_id) %>% 
        slice(1)
      pa.name = pa.name$name_pa
      
      ###Without overlap
      ### Note that the polygon reported by the WDAP can have a surface different from the reported area : thus the area without overalp can be bigger than the reported area ! 
      ### Example below ("test") with a Kenyan PA.
      area_noverlap_ha = mf %>%
        filter(group == 3) 
      area_noverlap_ha = nrow(area_noverlap_ha)*res_ha
      

      
      # test = wdpa_wld_raw %>% 
      #   filter(WDPAID == 555555482) %>%         
      #   mutate(area_poly_m2 = st_area(geom),
      #          area_pply_ha = area_poly_m2*1e-4)
      
      ##Total forest cover before treatment, in ha. Note this forest cover is in the PA without the overlap, as it is computed from the treated pixels. This is more rigorous to extrapolate on these : it corresponds to the area we can arguably isolate the effect of the PA we consider
      ## Global Forest Watch recommends to smooth forest cover on three years average : "Use 3-year moving averages to smooth any inconsistencies between years caused by the availability of Landsat images and better account for delays in detection of late-year fires and other losses." (https://www.globalforestwatch.org/blog/data-and-research/tree-cover-loss-satellite-data-trend-analysis/) 
      
      ###Extract period where forest cover can be averaged : same as the period where average pre-treatment forest cover is computed
      yr_cover = mf %>% 
        filter(group == 3) %>% 
        slice(1)
      yr_start_cover = yr_cover$start_pre_treat_fc
      yr_end_cover = yr_cover$end_pre_treat_fc
      ### Compute pre-treatment forest area
      fc_tot_pre_treat = unmatched.long %>%
        filter(group == 3 & year >= yr_start_cover & year <= yr_end_cover) %>%
        group_by(assetid) %>%
        summarize(avg_fc_ha = mean(fc_ha, na.rm = TRUE))
      fc_tot_pre_treat = sum(fc_tot_pre_treat$avg_fc_ha, na.rm = TRUE)
      n_pix_fc_pre_treat = fc_tot_pre_treat/res_ha 
      
      ##Average forest cover in a pixel before treatment. Note this forest cover is in the PA without the overlap, as it is computed from the treated pixels.
      # fc_avg_pre_treat = unmatched.long %>%
      #   filter(group == 3 & year == treatment.year-1)
      # fc_avg_pre_treat = mean(fc_avg_pre_treat$fc_ha, na.rm = TRUE)
      
      #Extract number of pixels in the PA
      #n_pix_pa = area_ha/res_ha #This measure is imperfect for extrapolation of total deforestation avoided, as part of a PA can be coastal. Indeed, this extrapolation assumes implicitly that all the PA is covered by forest potentially deforested in absence of the conservation 

      
     #Open a multisession for dataframe computations
      #Note the computations on unmatched units are the slowest here due to the number of observations relatively higher than for matched units
      library(future)
      library(progressr)
      plan(multisession, gc = TRUE, workers = 6)
      with_progress({
 
  # Make dataframe for plotting trend
  ## Matched units
  df.matched.trend  <- matched.long %>%
    #First, compute deforestation relative to 2000 for each pixel (deforestation as computed in Wolf et al. 2021), and forest cover relative to pre-treatment
    group_by(assetid) %>%
    mutate(FL_2000_cum = (fc_ha-fc_ha[year == 2000])/fc_ha[year == 2000]*100,
           FC_rel_pre_treat = fc_ha/mean(fc_ha[year >= yr_start_cover & year <= yr_end_cover], na.rm = TRUE)) %>%
    ungroup() %>%
    #Then compute the average forest cover and deforestation in each year, for treated and control groups
    #Standard deviation and 95% confidence interval is also computed for each variable
    group_by(group, year) %>%
    summarise(n = n(),
              avgFC = mean(fc_ha, na.rm=TRUE), #Compute average forest cover in a pixel, its sd and ci
              sdFC = sd(fc_ha, na.rm = TRUE),
              ciFC_low = avgFC - qt(0.975,df=n-1)*sdFC/sqrt(n),
              ciFC_up = avgFC + qt(0.975,df=n-1)*sdFC/sqrt(n),
              avgFC_tot = n_pix_fc_pre_treat*mean(fc_ha, na.rm=TRUE), #Compute total average forest cover, sd and CI
              sdFC_tot = n_pix_fc_pre_treat*sdFC,
              ciFC_tot_low = avgFC_tot - qt(0.975,df=n-1)*sdFC_tot/sqrt(n),
              ciFC_tot_up = avgFC_tot + qt(0.975,df=n-1)*sdFC_tot/sqrt(n),
              avgFC_rel = mean(FC_rel_pre_treat, na.rm=TRUE), #Compute relative average forest cover, sd and CI
              sdFC_rel = sd(FC_rel_pre_treat, na.rm = TRUE),
              ciFC_rel_low = avgFC_rel - qt(0.975,df=n-1)*sdFC_rel/sqrt(n),
              ciFC_rel_up = avgFC_rel + qt(0.975,df=n-1)*sdFC_rel/sqrt(n),
              avgFL_2000_cum = mean(FL_2000_cum, na.rm = TRUE), #Compute average forest loss relative to 2000 (Wolf et al 2021), sd and CI
              sdFL_2000_cum = sd(FL_2000_cum, na.rm = TRUE),
              ciFL_low = avgFL_2000_cum - qt(0.975,df=n-1)*sdFL_2000_cum/sqrt(n),
              ciFL_up = avgFL_2000_cum + qt(0.975,df=n-1)*sdFL_2000_cum/sqrt(n),
              matched = TRUE) %>%
    ungroup() %>%
    st_drop_geometry() %seed% 1
  
  #df.dumb = unmatched.long #Just use the unmacthed.long dataframe to avoid error in the code
  ##Unmatched
  df.unmatched.trend  <- unmatched.long %>%
      #First, compute deforestation relative to 2000 for each pixel (deforestation as computed in Wolf et al. 2021); compute percentage of forest cover in the pixel in 2000
      group_by(assetid) %>%
  mutate(FL_2000_cum = (fc_ha-fc_ha[year == 2000])/fc_ha[year == 2000]*100,
         FC_rel_pre_treat = fc_ha/mean(fc_ha[year >= yr_start_cover & year <= yr_end_cover], na.rm = TRUE)) %>%
      ungroup() %>% #Compute average percentage of FC in a pixel in 2000, for each group. Compute also standard deviation
    #Then compute the average forest cover, average forest cover percentage, and deforestation in each year, for treated and control groups
    #Standard deviation and 95% confidence interval is also computed for each variable
    group_by(group, year) %>%
    summarise(n = n(),
              avgFC = mean(fc_ha, na.rm=TRUE), #Compute average forest cover in a pixel, its sd and ci
              sdFC = sd(fc_ha, na.rm = TRUE),
              ciFC_low = avgFC - qt(0.975,df=n-1)*sdFC/sqrt(n),
              ciFC_up = avgFC + qt(0.975,df=n-1)*sdFC/sqrt(n),
              avgFC_tot = n_pix_fc_pre_treat*mean(fc_ha, na.rm=TRUE), #Compute total average forest cover, sd and CI
              sdFC_tot = n_pix_fc_pre_treat*sdFC,
              ciFC_tot_low = avgFC_tot - qt(0.975,df=n-1)*sdFC_tot/sqrt(n),
              ciFC_tot_up = avgFC_tot + qt(0.975,df=n-1)*sdFC_tot/sqrt(n),
              avgFC_rel = mean(FC_rel_pre_treat, na.rm=TRUE), #Compute average relative forest cover, sd and CI
              sdFC_rel = sd(FC_rel_pre_treat, na.rm = TRUE),
              ciFC_rel_low = avgFC_rel - qt(0.975,df=n-1)*sdFC_rel/sqrt(n),
              ciFC_rel_up = avgFC_rel + qt(0.975,df=n-1)*sdFC_rel/sqrt(n),
              avgFL_2000_cum = mean(FL_2000_cum, na.rm = TRUE), #Compute average forest loss relative to 2000 (Wolf et al 2021), sd and CI
              sdFL_2000_cum = sd(FL_2000_cum, na.rm = TRUE),
              ciFL_low = avgFL_2000_cum - qt(0.975,df=n-1)*sdFL_2000_cum/sqrt(n),
              ciFL_up = avgFL_2000_cum + qt(0.975,df=n-1)*sdFL_2000_cum/sqrt(n),
              matched = FALSE) %>%
      #Compute total forest cover loss, knowing area of the PA and average forest cover in 2000 in treated pixels
    ungroup() %>%
    st_drop_geometry() %seed% 2
  
      })
    
  df.trend = rbind(df.matched.trend, df.unmatched.trend)
  
  #Close multisession
  plan(sequential)
  
  #Plot
  ## Change Facet Labels
  fct.labs <- c("Before Matching", "After Matching")
  names(fct.labs) <- c(FALSE, TRUE)
  
  ## Trend Plot for unmatched data
  ### Average forest cover in a pixel
  fig_trend_unm_fc_pix = ggplot(data = df.trend, aes(x = year, y = avgFC)) +
    geom_line(aes(group = group, color = as.character(group))) +
    geom_point(aes(color = as.character(group))) +
    geom_ribbon(aes(ymin = ciFC_low, ymax = ciFC_up, group = group, fill = as.character(group)), alpha = .1, show.legend = FALSE) +
    geom_vline(aes(xintercept=as.character(treatment.year), size="Treatment year"), linetype=1, linewidth=0.5, color="orange") +
    geom_vline(aes(xintercept=as.character(funding.years), size="Funding year"), linetype=2, linewidth=0.5, color="grey30") +
    scale_x_discrete(breaks=seq(2000,2020,5), labels=paste(seq(2000,2020,5))) +
    scale_color_hue(labels = c("Control", "Treatment")) +
    facet_wrap(matched~., ncol = 2, #scales = 'free_x',
               labeller = labeller(matched = fct.labs)) +
    labs(title = "Evolution of forest cover in a pixel on average (unmatched units)",
         subtitle = paste0(pa.name, ", ", country.name, ", implemented in ", treatment.year),
  caption = paste("Ribbons represent 95% confidence intervals | WDPA reported surface :", format(area_ha, big.mark  = ",", scientific = F, digits = 1), "ha | Surface without overlap :", format(area_noverlap_ha, big.mark  = ",", scientific = F, digits = ), "ha\nTotal forest cover before treatment :", format(fc_tot_pre_treat, big.mark  = ",", scientific = F, digits = ), "ha | Pixel resolution :", res_ha, "ha.\nNote the area without overlap and total forest cover are computed using the polygon reported by the WDPA.\nsome areas have a polygon size different from the reported one, explaining some inconsistencies with computed areas."),
         x = "Year", y = "Forest cover (ha)", color = "Group") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = -20, hjust = 0.5, vjust = 0.5),
      axis.text=element_text(size=11),
      axis.title=element_text(size=14),

      plot.caption = element_text(hjust = 0),

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

  ### Average forest cover in a pixel, relative to average pre-treatment forest cover
  fig_trend_unm_fc_rel = ggplot(data = df.trend, aes(x = year, y = avgFC_rel*100)) +
    geom_line(aes(group = group, color = as.character(group))) +
    geom_point(aes(color = as.character(group))) +
    geom_ribbon(aes(ymin = ciFC_rel_low*100, ymax = ciFC_rel_up*100, group = group, fill = as.character(group)), alpha = .1, show.legend = FALSE) +
    geom_vline(aes(xintercept=as.character(treatment.year), size="Treatment year"), linetype=1, linewidth=0.5, color="orange") +
    geom_vline(aes(xintercept=as.character(funding.years), size="Funding year"), linetype=2, linewidth=0.5, color="grey30") +
    scale_x_discrete(breaks=seq(2000,2020,5), labels=paste(seq(2000,2020,5))) +
    scale_color_hue(labels = c("Control", "Treatment")) +
    facet_wrap(matched~., ncol = 2, #scales = 'free_x',
               labeller = labeller(matched = fct.labs)) +
    labs(title = "Evolution of forest cover in a pixel on average, relative to pre-treatment (unmatched units)",
         subtitle = paste0(pa.name, ", ", country.name, ", implemented in ", treatment.year),
  caption = paste("Forest cover expressed as a share of pre-treatment cover in the pixel, on average.\nThis can be interpreted as the evolution of forest cover in whole the protected area (without overlap).\nRibbons represent 95% confidence intervals | WDPA reported surface :", format(area_ha, big.mark  = ",", scientific = F, digits = ), "ha | Surface without overlap :", format(area_noverlap_ha, big.mark  = ",", scientific = F, digits = ), "ha\nTotal forest cover before treatment :", format(fc_tot_pre_treat, big.mark  = ",", scientific = F, digits = ), "ha | Pixel resolution :", res_ha, "ha.\nNote the area without overlap and total forest cover are computed using the polygon reported by the WDPA.\nsome areas have a polygon size different from the reported one, explaining some inconsistencies with computed areas."),
         x = "Year", y = "Forest cover relative to pre-treatment (%)", color = "Group") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = -20, hjust = 0.5, vjust = 0.5),
      axis.text=element_text(size=11),
      axis.title=element_text(size=14),

      plot.caption = element_text(hjust = 0),

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

  ### Total forest cover
  fig_trend_unm_fc_tot = ggplot(data = df.trend, aes(x = year, y = avgFC_tot)) +
    geom_line(aes(group = group, color = as.character(group))) +
    geom_point(aes(color = as.character(group))) +
    geom_ribbon(aes(ymin = ciFC_tot_low, ymax = ciFC_tot_up, group = group, fill = as.character(group)), alpha = .1, show.legend = FALSE) +
    geom_vline(aes(xintercept=as.character(treatment.year), size="Treatment year"), linetype=1, linewidth=0.5, color="orange") +
    geom_vline(aes(xintercept=as.character(funding.years), size="Funding year"), linetype=2, linewidth=0.5, color="grey30") +
    scale_x_discrete(breaks=seq(2000,2020,5), labels=paste(seq(2000,2020,5))) +
    scale_color_hue(labels = c("Control", "Treatment")) +
    facet_wrap(matched~., ncol = 2, #scales = 'free_x',
               labeller = labeller(matched = fct.labs)) +
    labs(title = "Evolution of total forest cover (unmatched units)",
         subtitle = paste0(pa.name, ", ", country.name, ", implemented in ", treatment.year),
  caption = paste("The average forest cover evolution in a pixel is extrapolated to the total forest cover before treatment, without PA overlap.\nRibbons represent 95% confidence intervals | WDPA reported surface :", format(area_ha, big.mark  = ",", scientific = F, digits = ), "ha | Surface without overlap :", format(area_noverlap_ha, big.mark  = ",", scientific = F, digits = ), "ha\nTotal forest cover before treatment :", format(fc_tot_pre_treat, big.mark  = ",", scientific = F, digits = ), "ha | Pixel resolution :", res_ha, "ha.\nNote the area without overlap and total forest cover are computed using the polygon reported by the WDPA.\nsome areas have a polygon size different from the reported one, explaining some inconsistencies with computed areas."),
         x = "Year", y = "Forest cover (ha)", color = "Group") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = -20, hjust = 0.5, vjust = 0.5),
      axis.text=element_text(size=11),
      axis.title=element_text(size=14),

      plot.caption = element_text(hjust = 0),

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

  ### Cumulative deforestation relative to 2000
  fig_trend_unm_defo = ggplot(data = df.trend, aes(x = year, y = avgFL_2000_cum)) +
    geom_line(aes(group = group, color = as.character(group))) +
    geom_point(aes(color = as.character(group))) +
    geom_ribbon(aes(ymin = ciFL_low, ymax = ciFL_up, group = group, fill = as.character(group)), alpha = .1, show.legend = FALSE) +
    geom_vline(aes(xintercept=as.character(treatment.year), size="Treatment year"), linetype=1, linewidth=0.5, color="orange") +
    geom_vline(aes(xintercept=as.character(funding.years), size="Funding year"), linetype=2, linewidth=0.5, color="grey30") +
    scale_x_discrete(breaks=seq(2000,2020,5), labels=paste(seq(2000,2020,5))) +
    scale_color_hue(labels = c("Control", "Treatment")) +
    facet_wrap(matched~., ncol = 2, #scales = 'free_x',
               labeller = labeller(matched = fct.labs)) +
    labs(title = "Cumulated deforestation relative to 2000 (unmatched units)",
         subtitle = paste0(pa.name, ", ", country.name, ", implemented in ", treatment.year),
  caption = paste("Ribbons represent 95% confidence intervals | WDPA reported surface :", format(area_ha, big.mark  = ",", scientific = F, digits = ), "ha | Surface without overlap :", format(area_noverlap_ha, big.mark  = ",", scientific = F, digits = ), "ha\nTotal forest cover before treatment :", format(fc_tot_pre_treat, big.mark  = ",", scientific = F, digits = 1), "ha | Pixel resolution :", res_ha, "ha.\nNote the area without overlap and total forest cover are computed using the polygon reported by the WDPA.\nsome areas have a polygon size different from the reported one, explaining some inconsistencies with computed areas."),
  x = "Year", y = "Forest loss relative to 2000 (%)", color = "Group") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = -20, hjust = 0.5, vjust = 0.5),
      axis.text=element_text(size=11),
      axis.title=element_text(size=14),

      plot.caption = element_text(hjust = 0),

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

  
  # Trend Plot for matched data
  ### Average forest cover in a pixel
  fig_trend_m_fc_pix = ggplot(data = df.matched.trend, aes(x = year, y = avgFC)) +
    geom_line(aes(group = group, color = as.character(group))) +
    geom_point(aes(color = as.character(group))) +
    geom_ribbon(aes(ymin = ciFC_low, ymax = ciFC_up, group = group, fill = as.character(group)), alpha = .1, show.legend = FALSE) +
    geom_vline(aes(xintercept=as.character(treatment.year), size="Treatment year"), linetype=1, linewidth=0.5, color="orange") +
    geom_vline(aes(xintercept=as.character(funding.years), size="Funding year"), linetype=2, linewidth=0.5, color="grey30") +
    scale_x_discrete(breaks=seq(2000,2020,5), labels=paste(seq(2000,2020,5))) + 
    scale_color_hue(labels = c("Control", "Treatment")) +
    labs(title = "Evolution of forest cover in a pixel on average (matched units)",
         subtitle = paste0(pa.name, ", ", country.name, ", implemented in ", treatment.year),
         caption = paste("Ribbons represent 95% confidence intervals | WDPA reported surface :", format(area_ha, big.mark  = ",", scientific = F, digits = ), "ha | Surface without overlap :", format(area_noverlap_ha, big.mark  = ",", scientific = F, digits = ), "ha\nTotal forest cover before treatment :", format(fc_tot_pre_treat, big.mark  = ",", scientific = F, digits = ), "ha | Pixel resolution :", res_ha, "ha.\nNote the area without overlap and total forest cover are computed using the polygon reported by the WDPA.\nsome areas have a polygon size different from the reported one, explaining some inconsistencies with computed areas."),
         x = "Year", y = "Forest cover (ha)", color = "Group") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = -20, hjust = 0.5, vjust = 0.5),
      axis.text=element_text(size=11),
      axis.title=element_text(size=14),
      
      plot.caption = element_text(hjust = 0),
      
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
  
  
  ### Average forest cover in a pixel, relative to average pre-treatment forest cover 
  fig_trend_m_fc_rel = ggplot(data = df.matched.trend, aes(x = year, y = avgFC_rel*100)) +
    geom_line(aes(group = group, color = as.character(group))) +
    geom_point(aes(color = as.character(group))) +
    geom_ribbon(aes(ymin = ciFC_rel_low*100, ymax = ciFC_rel_up*100, group = group, fill = as.character(group)), alpha = .1, show.legend = FALSE) +
    geom_vline(aes(xintercept=as.character(treatment.year), size="Treatment year"), linetype=1, linewidth=0.5, color="orange") +
    geom_vline(aes(xintercept=as.character(funding.years), size="Funding year"), linetype=2, linewidth=0.5, color="grey30") +
    scale_x_discrete(breaks=seq(2000,2020,5), labels=paste(seq(2000,2020,5))) + 
    scale_color_hue(labels = c("Control", "Treatment")) +
    labs(title = "Evolution of forest cover in a pixel, relative to pre-treatment (matched units)",
         subtitle = paste0(pa.name, ", ", country.name, ", implemented in ", treatment.year),
         caption = paste("Forest cover expressed as a share of pre-treatment cover in the pixel, on average.\nThis can be interpreted as the evolution of forest cover in whole the protected area (without overlap).\nRibbons represent 95% confidence intervals | WDPA reported surface :", format(area_ha, big.mark  = ",", scientific = F, digits = ), "ha | Surface without overlap :", format(area_noverlap_ha, big.mark  = ",", scientific = F, digits = ), "ha\nTotal forest cover before treatment :", format(fc_tot_pre_treat, big.mark  = ",", scientific = F, digits = ), "ha | Pixel resolution :", res_ha, "ha.\nNote the area without overlap and total forest cover are computed using the polygon reported by the WDPA.\nsome areas have a polygon size different from the reported one, explaining some inconsistencies with computed areas."),
         x = "Year", y = "Forest cover relative to pre-treatment (%)", color = "Group") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = -20, hjust = 0.5, vjust = 0.5),
      axis.text=element_text(size=11),
      axis.title=element_text(size=14),
      
      plot.caption = element_text(hjust = 0),
      
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
  
  ### Total forest cover
  fig_trend_m_fc_tot = ggplot(data = df.matched.trend, aes(x = year, y = avgFC_tot)) +
    geom_line(aes(group = group, color = as.character(group))) +
    geom_point(aes(color = as.character(group))) +
    geom_ribbon(aes(ymin = ciFC_tot_low, ymax = ciFC_tot_up, group = group, fill = as.character(group)), alpha = .1, show.legend = FALSE) +
    geom_vline(aes(xintercept=as.character(treatment.year), size="Treatment year"), linetype=1, linewidth=0.5, color="orange") +
    geom_vline(aes(xintercept=as.character(funding.years), size="Funding year"), linetype=2, linewidth=0.5, color="grey30") +
    scale_x_discrete(breaks=seq(2000,2020,5), labels=paste(seq(2000,2020,5))) +
    scale_color_hue(labels = c("Control", "Treatment")) +
    labs(title = "Evolution of total forest cover (matched units)",
         subtitle = paste0(pa.name, ", ", country.name, ", implemented in ", treatment.year),
         caption = paste("The average forest cover evolution in a pixel is extrapolated to the total forest cover before treatment, without PA overlap.\nRibbons represent 95% confidence intervals | WDPA reported surface :", format(area_ha, big.mark  = ",", scientific = F, digits = ), "ha | Surface without overlap :", format(area_noverlap_ha, big.mark  = ",", scientific = F, digits = ), "ha\nTotal forest cover before treatment :", format(fc_tot_pre_treat, big.mark  = ",", scientific = F, digits = ), "ha | Pixel resolution :", res_ha, "ha.\nNote the area without overlap and total forest cover are computed using the polygon reported by the WDPA.\nsome areas have a polygon size different from the reported one, explaining some inconsistencies with computed areas."),
         x = "Year", y = "Total forest cover (ha)", color = "Group") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = -20, hjust = 0.5, vjust = 0.5),
      axis.text=element_text(size=11),
      axis.title=element_text(size=14),
      
      plot.caption = element_text(hjust = 0),
      
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
  
  ### Cumulative deforestation relative to 2000
  fig_trend_m_defo = ggplot(data = df.matched.trend, aes(x = year, y = avgFL_2000_cum)) +
    geom_line(aes(group = group, color = as.character(group))) +
    geom_point(aes(color = as.character(group))) +
    geom_ribbon(aes(ymin = ciFL_low, ymax = ciFL_up, group = group, fill = as.character(group)), alpha = .1, show.legend = FALSE) +
    geom_vline(aes(xintercept=as.character(treatment.year), size="Treatment year"), linetype=1, linewidth=0.5, color="orange") +
    geom_vline(aes(xintercept=as.character(funding.years), size="Funding year"), linetype=2, linewidth=0.5, color="grey30") +
    scale_x_discrete(breaks=seq(2000,2020,5), labels=paste(seq(2000,2020,5))) +
    scale_color_hue(labels = c("Control", "Treatment")) +
    labs(title = "Cumulated deforestation relative to 2000 (matched units)",
         subtitle = paste0(pa.name, ", ", country.name, ", implemented in ", treatment.year),
         caption = paste("Ribbons represent 95% confidence intervals | WDPA reported surface :", format(area_ha, big.mark  = ",", scientific = F, digits = ), "ha | Surface without overlap :", format(area_noverlap_ha, big.mark  = ",", scientific = F, digits = ), "ha\nTotal forest cover before treatment :", format(fc_tot_pre_treat, big.mark  = ",", scientific = F, digits = ), "ha | Pixel resolution :", res_ha, "ha.\nNote the area without overlap and total forest cover are computed using the polygon reported by the WDPA.\nsome areas have a polygon size different from the reported one, explaining some inconsistencies with computed areas."),
         x = "Year", y = "Forest loss relative to 2000 (%)", color = "Group") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = -20, hjust = 0.5, vjust = 0.5),
      axis.text=element_text(size=11),
      axis.title=element_text(size=14),
      
      plot.caption = element_text(hjust = 0),
      
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
  
  ##Saving plots
  tmp = paste(tempdir(), "fig", sep = "/")
  
  ggsave(paste(tmp, paste0("fig_trend_unmatched_avgFC_", iso, "_", wdpaid, ".png"), sep = "/"),
         plot = fig_trend_unm_fc_pix,
         device = "png",
         height = 6, width = 9)
  ggsave(paste(tmp, paste0("fig_trend_matched_avgFC_", iso, "_", wdpaid, ".png"), sep = "/"),
         plot = fig_trend_m_fc_pix,
         device = "png",
         height = 6, width = 9)
  
  ggsave(paste(tmp, paste0("fig_trend_unmatched_avgFC_rel_", iso, "_", wdpaid, ".png"), sep = "/"),
         plot = fig_trend_unm_fc_rel,
         device = "png",
         height = 6, width = 9)
  ggsave(paste(tmp, paste0("fig_trend_matched_avgFC_rel_", iso, "_", wdpaid, ".png"), sep = "/"),
         plot = fig_trend_m_fc_rel,
         device = "png",
         height = 6, width = 9)
  
  ggsave(paste(tmp, paste0("fig_trend_unmatched_avgFC_tot_", iso, "_", wdpaid, ".png"), sep = "/"),
         plot = fig_trend_unm_fc_tot,
         device = "png",
         height = 6, width = 9)
  ggsave(paste(tmp, paste0("fig_trend_matched_avgFC_tot_", iso, "_", wdpaid, ".png"), sep = "/"),
         plot = fig_trend_m_fc_tot,
         device = "png",
         height = 6, width = 9)
  
  ggsave(paste(tmp, paste0("fig_trend_unmatched_avgFL_cum_2000_", iso, "_", wdpaid, ".png"), sep = "/"),
         plot = fig_trend_unm_defo,
         device = "png",
         height = 6, width = 9)
  ggsave(paste(tmp, paste0("fig_trend_matched_avgFL_cum_2000_", iso, "_", wdpaid, ".png"), sep = "/"),
         plot = fig_trend_m_defo,
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


# Plot the country grid with matched control and treated, for a given protected area (PA) or all protected areas in a country
##INPUTS
### iso : the ISO3 code of the country considered
### wdpaid : the WDPA ID of the PA considered
### is_pa : logical, whether the plotted grid is for a unique PA or all the PAs in the country considered
### df_pix_matched : dataframe with ID of matched pixels (ID from mapme.biodiversity portfolio)
### path_tmp : temporary folder to store figures
### log : a log file to track progress of the processing
### save_dir : saving directory
##OUTPUTS
### is_ok : a boolean indicating whether or not an error occured inside the function
##DATA SAVED
### Country grid with matched control and treated, for a given protected area (PA) or all protected areas in a country
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
    mutate(group_plot = case_when(group_matched == 2 ~ "Control (matched)",
                                  group_matched == 3 ~ "Treatment (matched)",
                                  TRUE ~ group_name))
  
  #Extract country name
  country.name = grid %>% 
    filter(group == 3) %>% 
    slice(1)
  country.name = country.name$country_en
  
  # Visualize and save grouped grid cells
  fig_grid = 
    ggplot(grid) +
    #The original gridding as a first layer
    geom_sf(aes(fill = as.factor(group_plot)), color = NA) +
    scale_fill_brewer(name = "Pixel group", type = "qual", palette = "BrBG", direction = 1) +
    labs(title = paste("Gridding of", country.name, "after matching"),
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
