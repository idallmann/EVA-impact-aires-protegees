#####
#Functions for matching
#####

###
#Pre-processing
###

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
  
  return(utm)
}

#Create a gridding of a given country
##INPUTS : 
### iso : ISO code 
### path_temp : temporary path for saving figures
### gridSize : grid resolution
##OUTPUTS : 
### gadm_prj : country shapefile 
### grid : gridding of the country
### utm_code : UTM code of the country

fn_pre_grid = function(iso, path_tmp, gridSize)
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

#Assing each pixel (observation unit) to a group
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

#Building a matching dataframe for a given country, and save it in the SSPCloud storage
##INPUTS :
### grid.param : a raster representing the gridding of the country with two layers. One for the group each pixel belongs to (funded PA, non-funded PA, potential control, buffer), the other for the WDPAID corresponding to each pixel (0 if not a PA)
### path_tmp : a temporary folder to store figures
### iso : ISO code of the country of interest
### name_output : the name of the matching frame to save
### ext_output : the file extention of the matching to save 
##OUTPUTS :
### None


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

#Load the matching dataframe obtained during pre-processing
##INPUTS :
### iso : the ISO code of the country considered
##OUTPUTS :
### mf : matching dataframe. More precisely, it gives for each observation units in a country values of different covariates to perform matching.
fn_post_load_mf = function(iso)
{
  object = paste("data_tidy/mapme_bio_data/matching", iso, paste0(name_input, "_", iso, ext_input), sep = "/")
  mf = s3read_using(sf::st_read,
                      bucket = "projet-afd-eva-ap",
                      object = object,
                      opts = list("region" = "")) %>%
    filter(group==0 | group==1) %>%
    drop_na() #%>%
  #st_drop_geometry()
  return(mf)
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

fn_post_avgLoss_prefund = function(mf, yr_start, yr_end, colfl.prefix, colname.flAvg)
{
  # Columns treeloss, without geometry
  start = yr_start - 2000
  end = yr_end - 2000
  df_fl = mf[grepl(colfl.prefix, names(mf))][start:end] %>% 
    st_drop_geometry()
  # Add column: average treeloss before funding starts, 
  mf$avgLoss_pre_fund = round(rowMeans(df_fl), 2)
  
  return(mf)
}

#Define the cutoffs of the covariates histogram, to perform Coarsened Exact Matching (CEM)
## INPUTS :
### mf : the matching dataframe
### colname.XXX : name of the XXX covariate in the matching frame
## OUTPUTS :
### lst_cutoffs : list of the cutoffs for each covariates

fn_post_cutoff = function(mf, colname.travelTime, colname.clayContent, colname.elevation, colname.tri, colname.fcIni, colname.flAvg)
{
  # Make cut-off list ####
  lst_cutoffs = c()
  
  # Quantile in 8 parts
  lst_cutoffs[[colname.travelTime]] = as.integer(quantile(mf[[colname.travelTime]], probs = seq(0, 1, 0.125), na.rm=TRUE))
  
  #lst_cutoffs[[colname.clayContent]] = as.integer(quantile(mf[[colname.clayContent]], probs = seq(0, 1, 0.05), na.rm=TRUE))
  lst_cutoffs[[colname.clayContent]] = as.integer(c(0,10,20,30, 32,34,36,38,40, 50,60,70,80,90,100))
  
  # lst_cutoffs[[colname.elevation]] = as.integer(quantile(mf[[colname.elevation]], probs = seq(0, 1, 0.125), na.rm=TRUE))
  # 
  # lst_cutoffs[[colname.tri]] = as.integer(quantile(mf[[colname.tri]], probs = seq(0, 1, 0.125), na.rm=TRUE))
  
  lst_cutoffs[[colname.fcIni]] = as.integer(quantile(mf[[colname.fcIni]], probs = seq(0, 1, 0.025), na.rm=TRUE))
  
  lst_cutoffs[[colname.flAvg]] = as.integer(quantile(mf[[colname.flAvg]], probs = seq(0, 1, 0.025), na.rm=TRUE))
  
  return(lst_cutoffs)
}

#Perform CEM
## INPUTS :
### mf : the matching dataframe
### lst_cutoffs : list of the cutoffs for each covariates
### iso : ISO code of the country considered
### path_tmp : temporary folder to store figures
### colname.XXX : name of the XXX covariate in the matching frame
## OUTPUTS :
### out.cem : list of results from the CEM matching

fn_post_cem = function(mf, lst_cutoffs, iso, path_tmp,
                       colname.travelTime, colname.clayContent, 
                       colname.elevation, colname.tri, 
                       colname.fcIni, colname.flAvg)
{
  formula = eval(bquote(group ~ .(as.name(colname.travelTime)) 
                        + .(as.name(colname.clayContent))  
                        +  .(as.name(colname.fcIni)) 
                        + .(as.name(colname.flAvg))
  ))
  
  # CEM Match
  out.cem = matchit(formula,
                    data = mf, 
                    method = "cem", 
                    cutpoints = lst_cutoffs)
  
  # fig_save = paste0(path_tmp, "/fig_cov_imb_", iso, ".png")
  # png(filename = fig_save,  width = 480, height = 480, units = "px", pointsize = 12)
  # plot(summary(out.cem))
  # dev.off()
  # aws.s3::put_object(file = fig_save, 
  #                    bucket = paste("projet-afd-eva-ap/data_tidy/mapme_bio_data/matching", iso, sep = "/"), 
  #                    region = "", 
  #                    show_progress = FALSE)
  
  return(out.cem)
}

#Plot covariates balance
## INPUTS :
### out.cem : list of results from the CEM matching
### iso : ISO code of the country considered
### path_tmp : temporary folder to store figures
### colname.XXX : name of the XXX covariate in the matching frame
## OUTPUTS :
### None

fn_post_plot_covbal = function(out.cem, colname.travelTime, colname.clayContent, colname.fcIni, colname.flAvg, iso, path_tmp)
{
  c_name = data.frame(old = c(colname.travelTime, colname.clayContent,
                              colname.fcIni, colname.flAvg),
                      new = c("Accessibility", "Clay Content", "Forest Cover in 2000",
                              "Avg. Annual Forest \n Loss 2001 ~ 2006"))
  
  # Refer to cobalt::love.plot()
  # https://cloud.r-project.org/web/packages/cobalt/vignettes/cobalt.html#love.plot
  fig_covbal = love.plot(out.cem, 
                       binary = "std", 
                       abs = TRUE,
                       #thresholds = c(m = .1),
                       var.order = "unadjusted",
                       var.names = c_name,
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
  
  fig_save = paste0(path_tmp, "/fig_covbal_", iso, ".png")
  ggsave(fig_save,
         plot = fig_covbal,
         device = "png",
         height = 6, width = 9)
  aws.s3::put_object(file = fig_save, 
                     bucket = paste("projet-afd-eva-ap/data_tidy/mapme_bio_data/matching", iso, sep = "/"), 
                     region = "", 
                     show_progress = FALSE)
}

#Plot the distribution of covariates for control and treatment units, before and after matching
## INPUTS :
### out.cem : list of results from the CEM matching
### iso : ISO code of the country considered
### path_tmp : temporary folder to store figures
### colname.XXX : name of the XXX covariate in the matching frame
## OUTPUTS :
### None

fn_post_plot_density = function(out.cem, colname.travelTime, colname.clayContent, colname.fcIni, colname.flAvg, iso, path_tmp)
{
  # Define Facet Labels
  fnl = c(`Unadjusted Sample` = "Before Matching",
          `Adjusted Sample` = "After Matching")
  
  #Define plots
  ## Density plot for Travel Time
  fig_travel = bal.plot(out.cem, 
                      var.name = colname.travelTime,
                      #sample.names = c("Control", "Treatment"),
                      which = "both") +
    facet_wrap(.~which, labeller = as_labeller(fnl)) +
    #scale_fill_viridis(discrete = T) +
    scale_fill_manual(labels = c("Control", "Treatment"), values = c("#f5b041","#5dade2")) +
    labs(title = "Distributional Balance for Accessibility",
         x = "Accessibility (min)",
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
  
  ## Density plot for Clay Content
  fig_clay = bal.plot(out.cem, 
                    var.name = colname.clayContent,
                    which = "both") +
    facet_wrap(.~which, labeller = as_labeller(fnl)) +
    #scale_fill_viridis(discrete = T) +
    scale_fill_manual(labels = c("Control", "Treatment"), values = c("#f5b041","#5dade2")) +
    labs(title = "Distributional Balance for Clay Content",
         x = "Clay Content at 0~20cm soil depth (%)",
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
  
  ## Density plot for Elevation
  # fig_elevation = bal.plot(out.cem, 
  #                     var.name = colname.elevation,
  #                     which = "both") +
  #     facet_wrap(.~which, labeller = as_labeller(fnl)) +
  #     #scale_fill_viridis(discrete = T) +
  #     scale_fill_manual(labels = c("Control", "Treatment"), values = c("#f5b041","#5dade2")) +
  #     labs(title = "Distributional Balance for Elevation",
  #          x = "Elevation (m)",
  #          fill = "Group") +
  #     theme_bw() +
  #     theme(
  #         plot.title = element_text(family="Arial Black", size=16, hjust=0.5),
  #         
  #         legend.title = element_blank(),
  #         legend.text=element_text(size=14),
  #         legend.spacing.x = unit(0.5, 'cm'),
  #         legend.spacing.y = unit(0.75, 'cm'),
  #         
  #         axis.text=element_text(size=12),
  #         axis.title=element_text(size=14),
  #         axis.title.y = element_text(margin = margin(unit = 'cm', r = 0.5)),
  #         axis.title.x = element_text(margin = margin(unit = 'cm', t = 0.5)),
  #         
  #         strip.text.x = element_text(size = 12) # Facet Label
  #     )
  
  ## Density plot for TRI
  # fig_tri = bal.plot(out.cem, 
  #                     var.name = colname.tri,
  #                     which = "both") +
  #     facet_wrap(.~which, labeller = as_labeller(fnl)) +
  #     #scale_fill_viridis(discrete = T) +
  #     scale_fill_manual(labels = c("Control", "Treatment"), values = c("#f5b041","#5dade2")) +
  #     labs(title = "Distributional Balance for Terrain Ruggedness Index (TRI)",
  #          x = "TRI",
  #          fill = "Group") +
  #     theme_bw() +
  #     theme(
  #         plot.title = element_text(family="Arial Black", size=16, hjust=0.5),
  #         
  #         legend.title = element_blank(),
  #         legend.text=element_text(size=14),
  #         legend.spacing.x = unit(0.5, 'cm'),
  #         legend.spacing.y = unit(0.75, 'cm'),
  #         
  #         axis.text=element_text(size=12),
  #         axis.title=element_text(size=14),
  #         axis.title.y = element_text(margin = margin(unit = 'cm', r = 0.5)),
  #         axis.title.x = element_text(margin = margin(unit = 'cm', t = 0.5)),
  #         
  #         strip.text.x = element_text(size = 12) # Facet Label
  #     )
  
  ## Density plot for covariate "forest cover 2000"
  fig_fc = bal.plot(out.cem, 
                  var.name = colname.fcIni,
                  which = "both") +
    facet_wrap(.~which, labeller = as_labeller(fnl)) +
    #scale_fill_viridis(discrete = T) +
    scale_fill_manual(labels = c("Control", "Treatment"), values = c("#f5b041","#5dade2")) +
    labs(title = "Distributional Balance for Forest Cover in 2000",
         x = "Forest Cover (%)",
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
  
  ## Density plot for covariate "avg. annual forest loss prior funding"
  fig_fl = bal.plot(out.cem, 
                  var.name = colname.flAvg,
                  which = "both") +
    facet_wrap(.~which, labeller = as_labeller(fnl)) +
    #scale_fill_viridis(discrete = T) +
    scale_fill_manual(labels = c("Control", "Treatment"), values = c("#f5b041","#5dade2")) +
    labs(title = "Distributional Balance for avgerage Forest Loss 2001~2006",
         x = "Forest Loss (%)",
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
  paste0(path_tmp, "/fig_covbal_", iso, ".png")
  ggsave(paste(tmp, paste0("fig_travel_dplot_", iso, ".png"), sep = "/"),
         plot = fig_travel,
         device = "png",
         height = 6, width = 9)
  ggsave(paste(tmp, paste0("fig_clay_dplot_", iso, ".png"), sep = "/"),
         plot = fig_clay,
         device = "png",
         height = 6, width = 9)
  # ggsave(paste(tmp, paste0("fig_elevation_dplot_", iso, ".png"), sep = "/"),
  #        plot = fig_elevation,
  #        device = "png",
  #        height = 6, width = 9)
  # ggsave(paste(tmp, paste0("fig_tri_dplot_", iso, ".png"), sep = "/"),
  #        plot = fig_tri,
  #        device = "png",
  #        height = 6, width = 9)
  ggsave(paste(tmp, paste0("fig_fc_dplot_", iso, ".png"), sep = "/"),
         plot = fig_fc,
         device = "png",
         height = 6, width = 9)
  ggsave(paste(tmp, paste0("fig_fl_dplot_", iso, ".png"), sep = "/"),
         plot = fig_fl,
         device = "png",
         height = 6, width = 9)
  
  files <- list.files(tmp, full.names = TRUE)
  ##Add each file in the bucket (same foler for every file in the temp)
  for(f in files) 
  {
    cat("Uploading file", paste0("'", f, "'"), "\n")
    aws.s3::put_object(file = f, 
                       bucket = paste("projet-afd-eva-ap/data_tidy/mapme_bio_data/matching", iso, sep = "/"), 
                       region = "", show_progress = TRUE)
  }
  do.call(file.remove, list(list.files(tmp, full.names = TRUE)))
  
}

#Define panel datasets (long, wide) for control and treatment observation units, before and after matching.
## INPUTS :
### out.cem : list of results from the CEM matching
### mf : the matching dataframe
### colfc.prefix : prefix of columns for forest cover
### colfc.bind : separation between prefix and year
## OUTPUTS :
### list_output : a list of dataframes : (un)matched.wide/long. They contain covariates and outcomes for treatment and control units, before and after matching, in a wide or long format

fn_post_panel = function(out.cem, mf, colfc.prefix, colfc.bind)
{
  # Convert dataframe of matched objects to pivot wide form
  matched.wide = match.data(object=out.cem, data=mf)
  
  # Pivot Wide ==> Pivot Long
  matched.long = matched.wide %>%
    dplyr::select(c(group, wdpaid, assetid, weights, starts_with(colfc.prefix))) %>%
    pivot_longer(cols = c(starts_with(colfc.prefix)),
                 names_to = c("var", "year"),
                 names_sep = colfc.bind,
                 values_to = "fc_pct")
  
  # Pivot wide Dataframe of un-matched objects
  unmatched.wide = mf
  
  # Pivot Wide ==> Pivot Long
  unmatched.long = unmatched.wide %>%
    dplyr::select(c(group, wdpaid, assetid, starts_with(colfc.prefix))) %>%
    pivot_longer(cols = c(starts_with(colfc.prefix)),
                 names_to = c("var", "year"),
                 names_sep = colfc.bind,
                 values_to = "fc_pct")
  
  #Return outputs
  list_output = list("matched.wide" = matched.wide, "matched.long" = matched.long,
                     "unmatched.wide" = unmatched.wide, "unmatched.long" = unmatched.long)
  return(list_output)
  
}

#Plot the average trend of control and treated units in a given country, before and after the matching
## INPUTS :
### (un)matched.long : dataframe with covariates and outcomes for each treatment and control unit, before and after matching, in a long format (one row : pixel+year)
## OUTPUTS : 
### None

fn_post_plot_trend = function(matched.long, unmatched.long) 
{
  # Make dataframe for plotting Trend
  df.matched.trend = matched.long %>%
    group_by(group, year) %>%
    summarise(avgFC = mean(fc_pct, na.rm=TRUE), n = n(), matched = TRUE)
  
  df.unmatched.trend = unmatched.long %>%
    group_by(group, year) %>%
    summarise(avgFC = mean(fc_pct, na.rm=TRUE), n = n(), matched = FALSE)
  
  df.trend = rbind(df.matched.trend, df.unmatched.trend)
  
  #Plot
  ## Change Facet Labels
  fct.labs <- c("Before Matching", "After Matching")
  names(fct.labs) <- c(FALSE, TRUE)
  
  ## Trend Plot for unmatched data
  fig_trend_unm = ggplot(df.trend, aes(x = year, y = avgFC)) +
    geom_line(aes(group = group, color = as.character(group))) +
    geom_point(aes(color = as.character(group))) +
    geom_vline(aes(xintercept=as.character(funding.start), size="Funding Start"), linetype=2, linewidth=0.5, color="orange") +
    
    scale_x_discrete(breaks=seq(2000,2020,5), labels=paste(seq(2000,2020,5))) +
    scale_color_hue(labels = c("Control", "Treatment")) +
    
    facet_wrap(matched~., ncol = 2, #scales = 'free_x',
               labeller = labeller(matched = fct.labs)) +
    
    labs(x = "Year", y = "Average Tree Cover (%) per KM2", color = "Group") +
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
    ) + guides(size = guide_legend(override.aes = list(color = "orange"))) # Add legend for geom_vline
  
  # Trend Plot for matched data
  fig_trend_m = ggplot(df.matched.trend, aes(x = year, y = avgFC)) +
    geom_line(aes(group = group, color = as.character(group))) +
    geom_point(aes(color = as.character(group))) +
    geom_vline(aes(xintercept=as.character(funding.start), size="Funding Start"), linetype=2, linewidth=0.5, color="orange") +
    
    #scale_y_continuous(breaks=seq(0,100,10), labels=paste(seq(0,100,10)),
    #                   expand=c(0,0), limits=c(0,100)) +
    scale_x_discrete(breaks=seq(2000,2020,5), labels=paste(seq(2000,2020,5))) +
    scale_color_hue(labels = c("Control", "Treatment")) +
    
    labs(x = "Year", y = "Average Tree Cover (%) per KM2", color = "Group") +
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
      
    ) + guides(size = guide_legend(override.aes = list(color = "orange"))) # Add legend for geom_vline
  
  ##Saving plots
  tmp = paste(tempdir(), "fig", sep = "/")
  
  ggsave(paste(tmp, paste0("fig_trend_unmatched_", iso, ".png"), sep = "/"),
         plot = fig_trend_unm,
         device = "png",
         height = 6, width = 9)
  ggsave(paste(tmp, paste0("fig_trend_matched_", iso, ".png"), sep = "/"),
         plot = fig_trend_m,
         device = "png",
         height = 6, width = 9)
  
  files <- list.files(tmp, full.names = TRUE)
  ##Add each file in the bucket (same foler for every file in the temp)
  for(f in files) 
  {
    cat("Uploading file", paste0("'", f, "'"), "\n")
    aws.s3::put_object(file = f, 
                       bucket = paste("projet-afd-eva-ap/data_tidy/mapme_bio_data/matching", iso, sep = "/"), 
                       region = "", show_progress = TRUE)
  }
  do.call(file.remove, list(list.files(tmp, full.names = TRUE)))
  
}
