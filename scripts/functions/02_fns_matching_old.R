#Building a matching dataframe for a given country, and save it in the SSPCloud storage
##INPUTS :
### grid.param : a raster representing the gridding of the country with two layers. One for the group each pixel belongs to (funded PA, non-funded PA, potential control, buffer), the other for the WDPAID corresponding to each pixel (0 if not a PA)
### path_tmp : a temporary folder to store figures
### iso : ISO code of the country of interest
### name_output : the name of the matching frame to save
### ext_output : the file extention of the matching to save 
##OUTPUTS :
### None

fn_pre_mf = function(grid.param, path_tmp, iso, name_output, ext_output, yr_first, yr_last, save_dir)
{
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
  
  # print("------Elevation")
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
  
  # Add new columns: treeloss_tn = treecover_tn - treecover_t(n-1)
  for (i in 1:length(dropFirst))
  {
    new_colname = paste0("treeloss_", colnames_loss[[i]][2])
    get.tree[[new_colname]] = get.tree[[dropFirst[i]]] - get.tree[[dropLast[i]]]
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
  
  ## Save files
  name_save = paste0(name_output, "_", iso, ext_output)
  s3write_using(pivot.all,
                sf::st_write,
                object = paste0(save_dir, "/", iso, "/", name_save),
                bucket = "projet-afd-eva-ap",
                opts = list("region" = ""))
  
  #Removing files in the temporary folder
  do.call(file.remove, list(list.files(tmp_pre, include.dirs = F, full.names = T, recursive = T)))
  
  
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
  lst_cutoffs[[colname.travelTime]] = as.integer(quantile(mf[[colname.travelTime]], probs = seq(0, 1, 0.1), na.rm=TRUE))
  
  lst_cutoffs[[colname.clayContent]] = as.integer(quantile(mf[[colname.clayContent]], probs = seq(0, 1, 0.1), na.rm=TRUE))
  #lst_cutoffs[[colname.clayContent]] = as.integer(c(0,10,20,30, 32,34,36,38,40, 50,60,70,80,90,100))
  
  # lst_cutoffs[[colname.elevation]] = as.integer(quantile(mf[[colname.elevation]], probs = seq(0, 1, 0.125), na.rm=TRUE))
  # 
  # lst_cutoffs[[colname.tri]] = as.integer(quantile(mf[[colname.tri]], probs = seq(0, 1, 0.125), na.rm=TRUE))
  
  lst_cutoffs[[colname.fcIni]] = as.integer(quantile(mf[[colname.fcIni]], probs = seq(0, 1, 0.1), na.rm=TRUE))
  
  lst_cutoffs[[colname.flAvg]] = as.integer(quantile(mf[[colname.flAvg]], probs = seq(0, 1, 0.1), na.rm=TRUE))
  
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
                       colname.fcIni, colname.flAvg,
                       save_dir)
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
  #                   bucket = paste("projet-afd-eva-ap", save_dir, iso, sep = "/")
  #                    region = "", 
  #                    show_progress = FALSE)
  
  return(out.cem)
  
}

fn_post_match_manual = function(mf, iso, wdpaid, path_tmp,
                                th_mean, th_var_min, th_var_max,
                                colname.travelTime, colname.clayContent, 
                                colname.elevation, colname.tri, 
                                colname.fcIni, colname.flAvg)
{
  # Formula
  formula = eval(bquote(group ~ .(as.name(colname.travelTime)) 
                        + .(as.name(colname.clayContent))  
                        +  .(as.name(colname.fcIni)) 
                        + .(as.name(colname.flAvg))))
  
  
  is_match_ok = FALSE
  count = 1
  list_sep = c(1, 5, 10, 15, 20, 25, 40, 50, 100) #The quantile thresholds to try, in percentage points
  
  while (is_match_ok == FALSE & count <= length(list_sep))
  {
    # Define cutoffs for CEM matching
    lst_cutoffs = c()
    sep = list_sep[count] #step is increased by 10% each iteration
    
    
    ## Quantile in 8 parts
    lst_cutoffs[[colname.travelTime]] = as.integer(quantile(mf[[colname.travelTime]], probs = seq(0, 1, length.out = (1/sep)*100+1), na.rm=TRUE))
    
    lst_cutoffs[[colname.clayContent]] = as.integer(quantile(mf[[colname.clayContent]], probs = seq(0, 1, length.out = (1/sep)*100+1), na.rm=TRUE))
    #lst_cutoffs[[colname.clayContent]] = as.integer(c(0,10,20,30, 32,34,36,38,40, 50,60,70,80,90,100))
    
    # lst_cutoffs[[colname.elevation]] = as.integer(quantile(mf[[colname.elevation]], probs = seq(0, 1, 0.125), na.rm=TRUE))
    # 
    # lst_cutoffs[[colname.tri]] = as.integer(quantile(mf[[colname.tri]], probs = seq(0, 1, 0.125), na.rm=TRUE))
    
    lst_cutoffs[[colname.fcIni]] = as.integer(quantile(mf[[colname.fcIni]], probs = seq(0, 1, length.out = (1/sep)*100+1), na.rm=TRUE))
    
    lst_cutoffs[[colname.flAvg]] = as.integer(quantile(mf[[colname.flAvg]], probs = seq(0, 1, length.out = (1/sep)*100+1), na.rm=TRUE))
    
    
    ## Matching handling errors due to absence of matching
    withCallingHandlers(
      {
        #Try to perform matching
        out.cem = matchit(formula,
                          data = mf,
                          method = "cem",
                          cutpoints = lst_cutoffs)
        
        #Compute matching feature IF the matching has been performed, otherwise it runs the error function
        ##Percentage of units matched
        df.nn = summary(out.cem)$nn %>% as.data.frame()
        n.matched = df.nn["Matched", "Treated"]
        n.all = df.nn["All", "Treated"]
        per.matched = n.matched/n.all*100
        ## Covariate balance : standardized mean difference and variance ratio
        df.cov.m = summary(out.cem, interactions = FALSE)$sum.matched %>%
          as.data.frame() %>%
          clean_names() %>%
          mutate(is_var_ok = var_ratio < th_var_max & var_ratio > th_var_min, #Check variance ratio between treated and controls
                 is_mean_ok = abs(std_mean_diff) < th_mean, #Check absolute standardized mean difference
                 is_bal_ok = as.logical(is_var_ok*is_mean_ok), #Binary : TRUE if both variance and mean difference check pass, 0 if at least one does not
                 .after = "std_mean_diff")
        
        # Depending on covariate balance, different possibilities
        ## If all covariates are balanced, then the matching with the cutoffs in this iteration is kept
        if(sum(df.cov.m$is_bal_ok) == nrow(df.cov.m) & is.na(sum(df.cov.m$is_bal_ok)) == FALSE) 
        {
          print(paste("It's a good match ! Variance ratio is between", th_var_min, "and", th_var_max, ", absolute standardized mean difference is below", th_mean, ".", round(per.matched, 1), "% of treated units matched."))
          is_match_ok <<- TRUE
          return(list("out.cem" = out.cem, "df.cov.m" = df.cov.m))
        } else if (sum(df.cov.m$is_bal_ok) > 0 & sum(df.cov.m$is_bal_ok) < nrow(df.cov.m) & is.na(sum(df.cov.m$is_bal_ok)) == FALSE)
          ## If at least one covariate is not balanced
        {
          print(paste("Matching not satisfactory regarding variance ratio or standardized mean difference."))
          count = count+1
        }
        
      },
      
      error=function(e)
      {
        if(is_match_ok == FALSE) 
        {
          message(paste('Error : cutoffs of', sep, 'pp not enough.'))
          count <<- count + 1
        }
      }
    )
    
    
  }
  
  
  
  
  # ## Extract covariate balance after matching
  # smry.out.cem = summary(out.cem)
  # df.cov.m = smry.out.cem$sum.matched %>%
  #   as.data.frame() %>%
  #   clean_names() %>%
  #   mutate(abs_std_mean_diff = abs(std_mean_diff),
  #          sum_abs_std_mean_diff = sum(abs_std_mean_diff),
  #          is_bal_ok = abs_std_mean_diff < 0.25,
  #          .after = "std_mean_diff")
  # 
  # ## If all covariates have an absolute standardized mean difference below 0.25 standard deviation, then the matching is considered good
  # is_match_ok = sum(df.cov.m$is_bal_ok) == nrow(df.cov.m)
  
  ###TO DO
  #Implement a while loop so that cutoffs are modified if : no matching OR matching balance is not satisfying.
  # First idea : 
  ## No matching : coarsen each covariates
  ## Matching not satisfactory : get the covariates, coarsen them. Appropriate to do this ? Or risk than changing the cutoffs for a variable induce problem for an other
  
}



fn_post_match_auto_old = function(mf, iso,
                                  dummy_int,
                                  th_mean, 
                                  th_var_min, th_var_max)
{
  # Formula
  formula = eval(bquote(group ~ .(as.name(colname.travelTime)) 
                        + .(as.name(colname.clayContent))  
                        +  .(as.name(colname.fcIni)) 
                        + .(as.name(colname.flAvg))))
  
  list_cut = c("sturges", "scott", "fd") #Three methods to define bins size of a distribution automatically (Iacus et al. 2011)
  list_results = c()
  
  # For the three methods offered by matchit, perform matching and run some tests on covariate balance
  for (cut_method in list_cut)
  {
    
    ## Matching handling errors due to absence of matching
    withCallingHandlers(
      {
        #Try to perform matching
        out.cem = matchit(formula,
                          data = mf,
                          method = "cem",
                          cutpoints = "fd",
                          k2k = TRUE,
                          k2k.method = "mahalanobis")
        
        #Compute matching feature IF the matching has been performed, otherwise it runs the error function
        ##Percentage of units matched
        # df.nn = summary(out.cem)$nn %>% as.data.frame()
        # n.matched = df.nn["Matched", "Treated"]
        # n.all = df.nn["All", "Treated"]
        # per.matched = n.matched/n.all*100
        ## Covariate balance : standardized mean difference and variance ratio
        ## For both tests and the joint one, a dummy variable is defined, with value TRUE is the test is passed
        df.cov.m = summary(out.cem, interactions = dummy_int)$sum.matched %>%
          as.data.frame() %>%
          clean_names() %>%
          mutate(is_var_ok = var_ratio < th_var_max & var_ratio > th_var_min, #Check variance ratio between treated and controls
                 is_mean_ok = abs(std_mean_diff) < th_mean, #Check absolute standardized mean difference
                 is_bal_ok = as.logical(is_var_ok*is_mean_ok), #Binary : TRUE if both variance and mean difference check pass, 0 if at least one does not
                 .after = "std_mean_diff")
        
        list_results[[cut_method]] = list("out.cem" = out.cem, "df.cov.m" = df.cov.m)
        
      },
      
      error=function(e)
      {
        message(paste('Error : the method', cut_method, "for cutpoints is not able to match units. Increase the sampling or try an other method."))
        list_results[[cut_method]] = list("df.nn" = paste('Error : the method', cut_method, "for cutpoints is not able to match units. Increase the sampling or try an other method."), 
                                          "df.cov.m" = paste('Error : the method', cut_method, "for cutpoints is not able to match units. Increase the sampling or try an other method."))
      }
      
    )
    
  }
  
  # From the covariate balance check, choosing which method whose results are returned : arbitrarily, we prefer sturges over scott, and scott over fd.
  is_sturges_ok = sum(list_results[["sturges"]]$df.cov.m$is_bal_ok) == nrow(list_results[["sturges"]]$df.cov.m)
  is_scott_ok = sum(list_results[["scott"]]$df.cov.m$is_bal_ok) == nrow(list_results[["scott"]]$df.cov.m)
  is_fd_ok = sum(list_results[["fd"]]$df.cov.m$is_bal_ok) == nrow(list_results[["fd"]]$df.cov.m)
  if(is_sturges_ok == TRUE)
  {
    return(list_results[["sturges"]])
  } else if (is_scott_ok == TRUE) 
  {
    return(list_results[["scott"]])
  } else if (is_fd_ok == TRUE)
  {
    return(list_results[["fd"]])
  } else next 
  
}
