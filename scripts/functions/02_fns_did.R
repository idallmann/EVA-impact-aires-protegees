#####
#Functions to perform difference-in-difference and plot results
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


#Load the list of PA matched during the matchign process
##INPUTS :
### iso : the ISO code of the country considered
##OUTPUTS :
### list_pa : a dataframe with the PA matched
### is_ok : a boolean indicating whether or not an error occured inside the function
fn_did_list_pa = function(iso, load_dir)
{
  output = tryCatch(
    
    {
      
  list_pa = s3read_using(data.table::fread,
                         bucket = "projet-afd-eva-ap",
                         object = paste(load_dir, iso, paste0("list_pa_matched_", iso, ".csv"), sep = "/"),
                         opts = list("region" = ""))
  list_pa = unique(list_pa$wdpaid)
  
  return(list("list_pa" = list_pa, "is_ok" = TRUE))
    },
  
  error = function(e)
  {
    print(e)
    #cat(paste("Error in loading the list of protected areas :\n", e, "\n"), file = log, append = TRUE)
    print(paste("Error in loading the list of protected areas :\n", e, "\n"))
    return(list("is_ok" = FALSE))
  }
  
  )
  
  return(output)
}


#For a protected area, compute annual deforestation rates à la Wolf et al. 2021, before and after treatment
## INPUTS 
### iso : the iso3 code for the country considered
### wdpaid : the WDPAID of the PA considered
### alpha : the margin of error to define confidence interval
### load_dir : a path to load matching frame
## OUTPUTS
### df_fl_annual_wolf : a dataframe with statistics on annual deforestation in matched treated and control units, computed à la Wolf et al. 2021
### is_ok : a boolean indicating whether or not an error occured inside the function 
fn_fl_wolf = function(iso, wdpaid, alpha, load_dir)
{
  output = tryCatch(
    
    {
      
  #Import matched units
  df_long = s3read_using(data.table::fread,
                           object = paste0(load_dir, "/", iso, "/", wdpaid, "/", paste0("matched_long", "_", iso, "_", wdpaid, ".csv")),
                           bucket = "projet-afd-eva-ap",
                           opts = list("region" = "")) %>%
    dplyr::select(c(region, iso3, wdpaid, focus, group, assetid, status_yr, year_funding_first, year_funding_all, res_m, year, var, fc_ha))
  #select(c(region, country_en, iso3, wdpaid, group, status_yr, year_funding_first, year_funding_all, year, var, fc_ha))
  
  ##Extract country info
  df_info = df_long %>% 
    filter(group == 3) %>% 
    slice(1)
  #iso
  country.iso = df_info$iso3
  
  ##Extract region name
region.name = df_info$region
  
  ##Extract focus dummy
  is_focus = as.logical(df_info$focus)
  
  #Compute annual deforestation rates à la Wolf et al. 2021 before and after treatment for treated, and for all the period for controls. This is averaged across pixels.
  
  ###########
  #/!\ Careful : error in the first mutate that mus tbe handle
  ###########
  
  df_fl_annual_wolf = df_long %>%
    mutate(treatment_year = case_when(group == 2 ~ 0,
                                      group == 3 ~ status_yr), #Set treatment year to 0 for control units (required by did::att_gt)
           time = case_when(group == 3 ~ year-treatment_year,
                            group == 2 ~ NA),
           .after = status_yr) %>%
    group_by(assetid) %>%
    mutate(FL_annual_wolf_pre = case_when(group == 3 & fc_ha[year == 2000] > 0 ~ ((fc_ha[time == -1]/fc_ha[year == 2000])^(1/(year[time == -1] - 2000))-1)*100,
                                          group == 3 & fc_ha[year == 2000] == 0 ~ 0,
                                          group == 2 ~ NA),
           FL_annual_wolf_post = case_when(group == 3 & fc_ha[time == 0] > 0 ~ ((fc_ha[time == max(time)]/fc_ha[time == 0])^(1/max(time))-1)*100,
                                           group == 3 & fc_ha[time == 0] == 0 ~ 0,
                                           group == 2 ~ NA),
           FL_annual_wolf_tot = case_when(fc_ha[year == 2000] > 0 ~ ((fc_ha[year == 2021]/fc_ha[year == 2000])^(1/(2021-2000))-1)*100,
                                          fc_ha[year == 2000] == 0 ~ 0)) %>%
    slice(1) %>%
    ungroup() %>%
    group_by(group) %>%
    summarize(avgFL_annual_wolf_pre = mean(FL_annual_wolf_pre, na.rm = TRUE),
              avgFL_annual_wolf_post = mean(FL_annual_wolf_post, na.rm = TRUE),
              avgFL_annual_wolf_tot = mean(FL_annual_wolf_tot, na.rm = TRUE),
              medFL_annual_wolf_pre = median(FL_annual_wolf_pre, na.rm = TRUE),
              medFL_annual_wolf_post = median(FL_annual_wolf_post, na.rm = TRUE),
              medFL_annual_wolf_tot = median(FL_annual_wolf_tot, na.rm = TRUE),
              sdFL_annual_wolf_pre = sd(FL_annual_wolf_pre, na.rm = TRUE),
              sdFL_annual_wolf_post = sd(FL_annual_wolf_post, na.rm = TRUE),
              sdFL_annual_wolf_tot = sd(FL_annual_wolf_tot, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(region = region.name, iso3 = country.iso, wdpaid = wdpaid, focus = is_focus, .before = "group") %>%
    mutate(group = case_when(group == 2 ~ "Control",
                             group == 3 ~ "Treated")) %>%
    ungroup() %>%
    as.data.frame()
  
  return(list("df_fl_annual_wolf" = df_fl_annual_wolf, "is_ok" = TRUE))
    },
  
  error = function(e)
  {
    print(e)
    #cat(paste("Error while computing annual deforestation à la Wolf et al. 2021 :\n", e, "\n"), file = log, append = TRUE)
    print(paste("Error while annual deforestation à la Wolf et al. 2021 :\n", e, "\n"))
    return(list("is_ok" = FALSE))
  }
  
  )
}

#Compute the treatment effect for a given protected area that is supported by the AFD. This function specifically includes information related to funding we obtain from AFD internal services.
## INPUTS : 
### iso : the iso3 code for the country considered
### wdpaid : the WDPAID of the PA considered
### data_pa : dataset with information on protected areas, and especially their surfaces
### data_fund : information on funding from AFD internal datasets, on AFD funded projects related to protected areas.
### data_report : list of projects related to protected areas in AFD, reported by technical departments
### alpha : the threshold for confidence interval
### is_m : boolean stating whether we compute treatment effects from matched (TRUE) or unmatched treated and control units (FALSE)
### save_dir : the saving directory in the remote storage
### load_dir : the loading directory in the remote storage
## OUTPUTS :
### df_fc_attgt : treatment effect computed for the protected area considered, expressed in avoided deforestation (hectare)
### df_fl_attgt : treatment effect computed for the protected area considered, expressed in change of deforestation rate
### is_ok : a boolean indicating whether or not an error occured inside the function 
### df_did : a pre-did dataframe used by the did::att_gt function to compute treatment effect
### df_pre_test : a dataframe with pre-treatment p-value to test for parallel trend assumption
## DATA SAVED :
### Dynamic treatment effects : avoided deforestation in an average pixel (in ha), avoided deforestation relative to 2000 forest cover, avoided deforestation extrapolated to the entire protected area (in ha), change in deforestation rate (in percentage points)
fn_did_att_afd = function(iso, wdpaid, data_pa, data_fund, data_report, alpha, is_m, load_dir, save_dir)
{
  
  output = tryCatch(
    
    {
      
  #Loading matched and unmatched datasets
  df_long_m = s3read_using(data.table::fread,
                           object = paste0(load_dir, "/", iso, "/", wdpaid, "/", paste0("matched_long", "_", iso, "_", wdpaid, ".csv")),
                           bucket = "projet-afd-eva-ap",
                           opts = list("region" = "")) %>%
    dplyr::select(c(region, iso3, country_en, wdpaid, focus, group, assetid, status_yr, year_funding_first, year_funding_all, res_m, year, var, fc_ha))
  #dplyr::select(c(region, country_en, iso3, wdpaid, group, status_yr, year_funding_first, year_funding_all, year, var, fc_ha))
  
  df_long_unm = s3read_using(data.table::fread,
                             object = paste0(load_dir, "/", iso, "/", wdpaid, "/", paste0("unmatched_long", "_", iso, "_", wdpaid, ".csv")),
                             bucket = "projet-afd-eva-ap",
                             opts = list("region" = "")) %>%
    dplyr::select(c(region, iso3, country_en,  wdpaid, focus, group, assetid, status_yr, year_funding_first, year_funding_all, year, res_m, var, fc_ha))
  #dplyr::select(c(region, country_en, iso3, wdpaid, group, status_yr, year_funding_first, year_funding_all, year, var, fc_ha))
  
  # Define the working datasets depending on the is_m value
  if(is_m == TRUE)
  {
    df_long = df_long_m
  } else{df_long = df_long_unm 
  }
  
  #Extract some relevant variables for later plots and treatment effect computations
  df_info = df_long %>% 
    filter(group == 3) %>% 
    slice(1)
  ##Extract spatial resolution of pixels res_m and define pixel area in ha
  res_m = unique(df_long$res_m)
  res_ha = res_m^2*1e-4
  
  ##Extract treatment year
treatment.year = df_info$status_yr
  
  ##Extract funding years
funding.years = df_info$year_funding_first
  list.funding.years = df_info$year_funding_all
  
  ##Extract country name
  #country.name = df_info$country_en
  
  ##Extract country iso
 country.iso = df_info$iso3
  
  ##Extract region name
 region.name = df_info$region
  
  ##Area without overlap
  ### Note that the polygon reported by the WDAP can have a surface different from the reported area : thus the area without overalp can be bigger than the reported area ! 
  ### Example below ("test") with a Kenyan PA.
  area_noverlap_ha = df_long_unm %>%
    filter(group == 3) %>%
    group_by(assetid) %>%
    slice(1) %>%
    ungroup()
  area_noverlap_ha = nrow(area_noverlap_ha)*res_ha
  
  ##Total forest cover before treatment, in ha. Note this forest cover is in the PA without the overlap, as it is computed from the treated pixels. This is more rigorous to extrapolate on these : it corresponds to the area we can arguably isolate the effect of the PA we consider
  ## Global Forest Watch recommends to smooth forest cover on three years average : "Use 3-year moving averages to smooth any inconsistencies between years caused by the availability of Landsat images and better account for delays in detection of late-year fires and other losses." (https://www.globalforestwatch.org/blog/data-and-research/tree-cover-loss-satellite-data-trend-analysis/) 
  ###Extract period where forest cover can be averaged : same as the period where average pre-treatment forest cover is computed
  yr_cover = df_long_unm %>% 
    filter(group == 3) %>% 
    slice(1)
  yr_start_cover = yr_cover$start_pre_treat_fc
  yr_end_cover = yr_cover$end_pre_treat_fc
  ### Compute pre-treatment forest area
  fc_tot_pre_treat = df_long_unm %>%
    filter(group == 3 & year >= yr_start_cover & year <= yr_end_cover) %>%
    group_by(assetid) %>%
    summarize(avg_fc_ha = mean(fc_ha, na.rm = TRUE))
  fc_tot_pre_treat = sum(fc_tot_pre_treat$avg_fc_ha, na.rm = TRUE)
  n_pix_fc_pre_treat = fc_tot_pre_treat/res_ha 
  
  ##Extract more information not in the matched dataframe
  wdpa_id = wdpaid #Need to give a name to wdpaid (function argument) different from the variable in the dataset (wdpaid)
  df_info_pa = data_pa %>% 
    filter(wdpaid == wdpa_id) %>% 
    slice(1)
  ### Area reported by the WDPA
  area_ha = data_pa[data_pa$wdpaid == wdpa_id,]$area_km2*100
  ### Name of the PA
  pa.name = data_pa %>% 
    filter(wdpaid == wdpa_id) %>% 
    slice(1)
  pa.name = pa.name$name_pa
  ### Country name
country.name = df_info_pa$country_en
  ### AFD project ID
  # id.project = data_pa %>% 
  #   filter(wdpaid == wdpa_id) %>% 
  #   slice(1)
  # id.project = id.project$id_projet
  ### WDPA status
status.wdpa = df_info_pa$status
  ### IUCN category and description
iucn.cat = df_info_pa$iucn_cat
  iucn.des = df_info_pa$iucn_des_en
  ### Ecosystem
 eco.wdpa = df_info_pa$marine
  ### Governance
  gov.wdpa = df_info_pa$gov_type
  ### Owner
own.wdpa = df_info_pa$own_type
  
  ## Extract information on funding
  ### Type of funding
  fund.type = data_fund %>%
    filter(id_projet == id.project) %>%
    slice(1)
  fund.type = fund.type$libelle_produit
  ### Cofunders
  cofund = data_fund %>%
    filter(id_projet == id.project) %>%
    slice(1)
  cofund = cofund$cofinanciers
  ### KfW ?
  kfw = data_fund %>%
    filter(id_projet == id.project) %>%
    slice(1)
  kfw = kfw$kfw
  ### FFEM ?
  ffem = data_fund %>%
    filter(id_projet == id.project) %>%
    slice(1)
  ffem = ffem$ffem

  ## Extract reporting department
  reporter = data_report %>%
    filter(wdpaid == wdpa_id & id_projet == id.project & nom_ap == pa.name) %>%
    slice(1)
  reporter = reporter$auteur_entree
  
  #Extract number of pixels in the PA
  #n_pix_pa = length(unique(filter(df_long_unm, group == 2)$assetid))
  #n_pix_pa = area_ha/res_ha
  
  #Average forest cover in a treated pixel before treatment
  ## For matched 
  avgFC_pre_treat_m = df_long_m %>% 
    filter(group == 3 & year >= yr_start_cover & year <= yr_end_cover) 
  avgFC_pre_treat_m = mean(avgFC_pre_treat_m$fc_ha, na.rm = TRUE)
  ## For unmatched
  avgFC_pre_treat_unm = df_long_unm %>% 
    filter(group == 3 & year >= yr_start_cover & year <= yr_end_cover) 
  avgFC_pre_treat_unm = mean(avgFC_pre_treat_unm$fc_ha, na.rm = TRUE)
  
  #Then modify the dataframe before difference-in-difference computations
  ## Set treatment year = 0 for controls (necessary for did package to consider "never treated" units)
  ## Compute cumulative deforestation relative to 2000 forest cover (outcome where TE is computed)
  ## Add different areas, important to aggregate and interpret the results at country level
  df_did = df_long %>%
    mutate(treatment_year = case_when(group == 2 ~ 0,
                                      group == 3 ~ status_yr), #Set treatment year to 0 for control units (required by did::att_gt)
           time = ifelse(group == 3, yes = year-treatment_year, no = NA),
           .after = status_yr) %>%
    #Remove any time, assetid duplicate (case of WDPAID 555555493 in Kenya, asset id 161654 162057 162457 162856)
    group_by(assetid, time) %>%
    slice(1) %>%
    ungroup() %>%
    group_by(assetid) %>%
    group_by(assetid) %>%
    # mutate(FL_2000_cum = (fc_ha-fc_ha[year == 2000])/fc_ha[year == 2000]*100,
    #        fc_2000 = fc_ha[year == 2000]) %>%
    mutate(FL_2000_cum = case_when(fc_ha[year == 2000] > 0 ~ (fc_ha-fc_ha[year == 2000])/fc_ha[year == 2000]*100, 
                                   TRUE ~ 0)) %>%
    ungroup()
  

  #Compute dynamic treatment effect with did package. 
  ## Control are "never treated" units, no covariate is added in the regression estimated with doubly-robust method
  ## standard errors are computed with bootstrap, and confidence intervals computed from it.
  ## No clustering is performed as it does not seem relevant in our case (https://blogs.worldbank.org/impactevaluations/when-should-you-cluster-standard-errors-new-wisdom-econometrics-oracle)
  ## Pseudo treatment effects are computed for each pre-treatment year (varying base period)
  
  ##For forest cover (ha and %)
  ### treatment effect computation
  fc_attgt = did::att_gt(yname = "fc_ha",
                         gname = "treatment_year",
                         idname = "assetid",
                         tname = "year",
                         control_group = "nevertreated", #Thsi corresponds to control pixels as defined in the matching , with treatment year set to 0
                         xformla = ~1,
                         alp = alpha, #For 95% confidence interval
                         allow_unbalanced_panel = TRUE, #Ensure no unit is dropped, though every pixel should have data for all years in the period
                         bstrap=TRUE, #Compute bootstrap CI
                         biters = 1000, #The number of bootstrap iteration, 1000 is default
                         cband = TRUE, #Compute CI
                         clustervars = NULL, #No clustering seems relevant to me 
                         base_period = "varying",
                         data = df_did,
                         print_details = F)
  
  pval_pretest_fc = ifelse(is.numeric(fc_attgt$Wpval[1]) == TRUE,
                           yes = fc_attgt$Wpval[1],
                           no = NA)
  
  ##For change in deforestation rate (percentage points)
  ### treatment effect computation
  fl_attgt = did::att_gt(yname = "FL_2000_cum",
                         gname = "treatment_year",
                         idname = "assetid",
                         tname = "year",
                         control_group = "nevertreated", #Thsi corresponds to control pixels as defined in the matching , with treatment year set to 0
                         xformla = ~1,
                         alp = alpha, #For 95% confidence interval
                         allow_unbalanced_panel = TRUE, #Ensure no unit is dropped, though every pixel should have data for all years in the period
                         bstrap=TRUE, #Compute bootstrap CI
                         biters = 1000, #The number of bootstrap iteration, 1000 is default
                         cband = TRUE, #Compute CI
                         clustervars = NULL, #No clustering seems relevant to me
                         base_period = "varying",
                         data = df_did,
                         print_details = F)
  
  pval_pretest_fl = ifelse(is.numeric(fl_attgt$Wpval[1]) == TRUE,
                           yes = fl_attgt$Wpval[1],
                           no = NA)
  
  ### Store the P-value for pre-test of parallel trends assumption : in a dataframe
  df_pre_test = data.frame("region" = region.name,
                           "country_en" = country.name,
                           "iso3" = country.iso,
                           "name_pa" = pa.name,
                           "wdpaid" = wdpaid, 
                           "pval_fc" = pval_pretest_fc, 
                           "pval_fl" = pval_pretest_fl)  
  ### Report results in a dataframe
  ### The computed is at pixel level
  ### This treatment effect is aggregated to protected area by multiplying treatment effect by the number of pixel in the PA. It is also expressed in percentage of pixel area (avoided deforestation in share of pixel area)
  ### confidence intervals (at pixel level) are computed from bootstrap standard errors after a coefficient is applied.
  ### This computation takes the one from did:::summary.MP function, line 15 and 16. 
  ### They are multiplied by the number of pixels to compute confidence intervals for treatment effect at protected area level 
  ### They are divided by the pixel area to compute CI for treatment effect in percentage of pixel area
  df_fc_attgt = data.frame("treatment_year" = fc_attgt$group,
                           "year" = fc_attgt$t,
                           "att_pix" = fc_attgt$att,
                           "c" = fc_attgt$c,
                           "se" = fc_attgt$se,  
                           "n" = fc_attgt$n) %>%
    #Compute treatment effect at PA level and in share of pixel area
    ## att_pa : the total avoided deforestation is the avoided deforestation in ha in a given pixel, multiplied by the number of pixel in the PA.
    ## att_per : avoided deforestation in a pixel, as a share of average forest cover in 2000 in matched treated. Can be extrapolated to full PA in principle (avoided deforestation in share of 2000 forest cover)
    mutate(att_pa = att_pix*n_pix_fc_pre_treat,
           att_per = att_pix/avgFC_pre_treat_m*100) %>% 
    #Compute time relative to treatment year
    mutate(time = year - treatment_year,
           .before = year) %>%
    #Compute confidence intervals
    mutate(cband_lower_pix = round(att_pix-c*se, 4),
           cband_upper_pix = round(att_pix+c*se, 4),
           cband_lower_pa = cband_lower_pix*n_pix_fc_pre_treat,
           cband_upper_pa = cband_upper_pix*n_pix_fc_pre_treat,
           cband_lower_per = case_when(is.na(avgFC_pre_treat_m) == T ~ NA,
                                       TRUE ~ cband_lower_pix/avgFC_pre_treat_m*100),
           cband_upper_per = case_when(is.na(avgFC_pre_treat_m) == T ~ NA,
                                       TRUE ~ cband_upper_pix/avgFC_pre_treat_m*100),
           sig = sign(cband_lower_pix) == sign(cband_upper_pix),
           sig_5 = ifelse(max(time) >=5, yes = sig[time == 5] == TRUE, no = NA),
           sig_10 = ifelse(max(time) >= 10, yes = sig[time == 10] == TRUE, no = NA),
           sig_end = sig[time == max(time)] == TRUE,
           alpha = alpha) %>%
    #Add relevant information
    mutate(region = region.name,
           country_en = country.name,
           iso3 = country.iso,
           name_pa = pa.name,
           wdpaid = wdpaid,
           area_ha = area_ha,
           area_noverlap_ha = area_noverlap_ha,
           fc_tot_pre_treat = fc_tot_pre_treat,
           res_ha = res_ha,
           status_wdpa = status.wdpa,
           iucn_cat = iucn.cat,
           iucn_des_en = iucn.des,
           gov_type = gov.wdpa,
           own_type = own.wdpa,
           marine = eco.wdpa,
           funding_year = funding.years,
           funding_year_list = list.funding.years,
           .before = "treatment_year")
  
  # Same for change in deforestation rate
  df_fl_attgt = data.frame("treatment_year" = fl_attgt$group,
                           "year" = fl_attgt$t,
                           "att" = case_when(fl_attgt$att <= .Machine$double.eps ~ 0,
                                             TRUE ~ fl_attgt$att),
                           "c" = case_when(fl_attgt$c == -Inf ~ NA, TRUE ~ fl_attgt$c),
                           "se" = fl_attgt$se,
                           "n" = fl_attgt$n) %>%
    #Compute time relative to treatment year
    mutate(time = year - treatment_year,
           .before = year) %>%
    mutate(cband_lower = round(att-c*se, 4),
           cband_upper = round(att+c*se, 4),
           sig = sign(cband_lower) == sign(cband_upper),
           sig_5 = ifelse(max(time) >=5, yes = sig[time == 5] == TRUE, no = NA),
           sig_10 = ifelse(max(time) >= 10, yes = sig[time == 10] == TRUE, no = NA),
           sig_end = sig[time == max(time)] == TRUE,
           alpha = alpha) %>%
    #Compute time relative to treatment year
    mutate(time = year - treatment_year,
           .before = year) %>%
    #Add relevant information
    mutate(region = region.name,
           country_en = country.name,
           iso3 = country.iso,
           name_pa = pa.name,
           wdpaid = wdpaid,
           area_ha = area_ha,
           area_noverlap_ha = area_noverlap_ha,
           fc_tot_pre_treat = fc_tot_pre_treat,
           res_ha = res_ha,
           status_wdpa = status.wdpa,
           iucn_cat = iucn.cat,
           iucn_des_en = iucn.des,
           gov_type = gov.wdpa,
           own_type = own.wdpa,
           marine = eco.wdpa,
           funding_year = funding.years,
           funding_year_list = list.funding.years,
           .before = "treatment_year")
  
  ###Plot results
  ## treatment effect : avoided deforestation at pixel level (in ha)
  fig_att_pix = ggplot(data = df_fc_attgt,
                       aes(x = time, y = att_pix)) %>%
    + geom_line(color = "#08519C") %>%
    + geom_point(color = "#08519C") %>%
    + geom_ribbon(aes(ymin = cband_lower_pix, ymax = cband_upper_pix),
                  alpha=0.1, fill = "#FB6A4A", color = "black", linetype = "dotted") %>%
    + labs(title = ifelse(is_m == TRUE, 
                          yes = "Deforestation avoided in a pixel,on average (matched units)",
                          no = "Deforestation avoided in a pixel,on average (unmatched units)"),
           subtitle = paste0(pa.name, ", ", country.name, ", implemented in ", treatment.year),
           caption = paste("Ribbon represents", (1-alpha)*100, "% confidence interval | P-value for pre-test of parallel trends assumption :", pval_pretest_fc, "\nWDPA ID :", wdpa_id, "| WDPA reported surface :", format(area_ha, big.mark  = ",", scientific = F, digits = 1), "ha | Surface without overlap :", format(area_noverlap_ha, big.mark  = ",", scientific = F, digits = ), "ha\nTotal forest cover before treatment :", format(fc_tot_pre_treat, big.mark  = ",", scientific = F, digits = ), "ha | Pixel resolution :", res_ha, "ha.\nTreatment effect is interpreted as the deforestation avoided at pixel level in hectare, due to the conservation program.\nA negative effect means the conservation program has caused higher deforestation."),
           y = "Area (ha)",
           x = "Year relative to treatment (t = 0)") %>%
    + scale_x_continuous(breaks=seq(min(df_fc_attgt$time),max(df_fc_attgt$time),by=1)) %>%
    + theme_minimal() %>%
    + theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
      axis.text=element_text(size=11, color = "black"),
      axis.title=element_text(size=14, color = "black", face = "plain"),
      
      plot.caption = element_text(hjust = 0),
      plot.title = element_text(size=16, color = "black", face = "plain", hjust = 0),
      plot.subtitle = element_text(size=12, color = "black", face = "plain", hjust = 0),
      
      strip.text = element_blank(),
      
      #legend.position = "bottom",
      legend.title = element_blank(),
      legend.text=element_text(size=10),
      #legend.spacing.x = unit(1.0, 'cm'),
      legend.spacing.y = unit(0.75, 'cm'),
      legend.key.size = unit(2, 'line'),
      
      panel.grid.major.x = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.x = element_line(color = 'grey80', linewidth = 0.2, linetype = 2),
      panel.grid.major.y = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.y = element_line(color = 'grey80', linewidth = 0.2, linetype = 2)
    )
  
  # treatment effect : avoided deforestation in terms of 2000 forest cover
  fig_att_per = ggplot(data = df_fc_attgt,
                       aes(x = time, y = att_per)) %>%
    + geom_line(color = "#08519C") %>%
    + geom_point(color = "#08519C") %>%
    + geom_ribbon(aes(ymin = cband_lower_per, ymax = cband_upper_per),
                  alpha=0.1, fill = "#FB6A4A", color = "black", linetype = "dotted") %>%
    + labs(title = ifelse(is_m == TRUE, 
                          yes = "Average deforestation avoided relative to pre-treatment forest cover (matched units)",
                          no = "Average deforestation avoided relative to pre-treatment forest cover (unmatched units)"),
           subtitle = paste0(pa.name, ", ", country.name, ", implemented in ", treatment.year),
           caption = paste("Ribbon represents", (1-alpha)*100, "% confidence interval | P-value for pre-test of parallel trends assumption :", pval_pretest_fc, "\nWDPA ID :", wdpa_id, "| WDPA reported surface :", format(area_ha, big.mark  = ",", scientific = F, digits = 1), "ha | Surface without overlap :", format(area_noverlap_ha, big.mark  = ",", scientific = F, digits = ), "ha\nTotal forest cover before treatment :", format(fc_tot_pre_treat, big.mark  = ",", scientific = F, digits = ), "ha | Pixel resolution :", res_ha, "ha.\nTreatment effect is interpreted as the deforestation avoided in percentage of pre-treatment forest cover.\nA negative effect means the conservation program has caused higher deforestation."),
           y = "%",
           x = "Year relative to treatment (t = 0)") %>%
    + scale_x_continuous(breaks=seq(min(df_fc_attgt$time),max(df_fc_attgt$time),by=1)) %>%
    + theme_minimal() %>%
    + theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
      axis.text=element_text(size=11, color = "black"),
      axis.title=element_text(size=14, color = "black", face = "plain"),
      
      plot.caption = element_text(hjust = 0),
      plot.title = element_text(size=16, color = "black", face = "plain", hjust = 0),
      plot.subtitle = element_text(size=12, color = "black", face = "plain", hjust = 0),
      
      strip.text = element_blank(),
      
      #legend.position = "bottom",
      legend.title = element_blank(),
      legend.text=element_text(size=10),
      #legend.spacing.x = unit(1.0, 'cm'),
      legend.spacing.y = unit(0.75, 'cm'),
      legend.key.size = unit(2, 'line'),
      
      panel.grid.major.x = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.x = element_line(color = 'grey80', linewidth = 0.2, linetype = 2),
      panel.grid.major.y = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.y = element_line(color = 'grey80', linewidth = 0.2, linetype = 2)
    )
  
  # treatment effect : avoided deforestation in the PA
  fig_att_pa = ggplot(data = df_fc_attgt,
                      aes(x = time, y = att_pa)) %>%
    + geom_line(color = "#08519C") %>%
    + geom_point(color = "#08519C") %>%
    + geom_ribbon(aes(ymin = cband_lower_pa, ymax = cband_upper_pa),
                  alpha=0.1, fill = "#FB6A4A", color = "black", linetype = "dotted") %>%
    + labs(title = ifelse(is_m == TRUE, 
                          yes = "Total deforestation avoided (matched units)",
                          no = "Total deforestation avoided (unmatched units)"),
           subtitle = paste0(pa.name, ", ", country.name, ", implemented in ", treatment.year),
           caption = paste("Ribbon represents", (1-alpha)*100, "% confidence interval | P-value for pre-test of parallel trends assumption :", pval_pretest_fc, "\nWDPA ID :", wdpa_id, "| WDPA reported surface :", format(area_ha, big.mark  = ",", scientific = F, digits = 1), "ha | Surface without overlap :", format(area_noverlap_ha, big.mark  = ",", scientific = F, digits = ), "ha\nTotal forest cover before treatment :", format(fc_tot_pre_treat, big.mark  = ",", scientific = F, digits = ), "ha | Pixel resolution :", res_ha, "ha.\nTreatment effect is interpreted as the total deforestation avoided in the protected areas, in hectare (ha).\nThis measure is an extrapolation to the full protected area of average avoided deforestation at pixel level.\nA negative effect means the conservation program has caused higher deforestation."),
           y = "Forest area (ha)",
           x = "Year relative to treatment (t = 0)") %>%
    + scale_x_continuous(breaks=seq(min(df_fc_attgt$time),max(df_fc_attgt$time),by=1)) %>%
    + theme_minimal() %>%
    + theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
      axis.text=element_text(size=11, color = "black"),
      axis.title=element_text(size=14, color = "black", face = "plain"),
      
      plot.caption = element_text(hjust = 0),
      plot.title = element_text(size=16, color = "black", face = "plain", hjust = 0),
      plot.subtitle = element_text(size=12, color = "black", face = "plain", hjust = 0),
      
      strip.text = element_blank(),
      
      #legend.position = "bottom",
      legend.title = element_blank(),
      legend.text=element_text(size=10),
      #legend.spacing.x = unit(1.0, 'cm'),
      legend.spacing.y = unit(0.75, 'cm'),
      legend.key.size = unit(2, 'line'),
      
      panel.grid.major.x = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.x = element_line(color = 'grey80', linewidth = 0.2, linetype = 2),
      panel.grid.major.y = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.y = element_line(color = 'grey80', linewidth = 0.2, linetype = 2)
    )
  

  # treatment effect : change in deforestation rate
  fig_fl_att = ggplot(data = df_fl_attgt,
                      aes(x = time, y = att)) %>%
    + geom_line(color = "#08519C") %>%
    + geom_point(color = "#08519C") %>%
    + geom_ribbon(aes(ymin = cband_lower, ymax = cband_upper),
                  alpha=0.1, fill = "#FB6A4A", color = "black", linetype = "dotted") %>%
    + labs(title = ifelse(is_m == TRUE, 
                          yes = "Effect of the conservation on the deforestation rate, relative to 2000 (matched units)",
                          no = "Effect of the conservation on the deforestation rate, relative to 2000 (unmatched units)"),
           subtitle = paste0(pa.name, ", ", country.name, ", implemented in ", treatment.year),
           caption = paste("Ribbon represents", (1-alpha)*100, "% confidence interval | P-value for pre-test of parallel trends assumption :", pval_pretest_fl, "\nWDPA ID :", wdpa_id, "| WDPA reported surface :", format(area_ha, big.mark  = ",", scientific = F, digits = 1), "ha | Surface without overlap :", format(area_noverlap_ha, big.mark  = ",", scientific = F, digits = ), "ha\nTotal forest cover before treatment :", format(fc_tot_pre_treat, big.mark  = ",", scientific = F, digits = ), "ha | Pixel resolution :", res_ha, "ha.\nTreatment effect is interpreted as the reduction of cumulated deforestation rate (relative to 2000 forest cover) in percentage points (pp).\nA negative effect means the conservation program has caused higher deforestation."),
           y = "Reduction of deforestation (p.p)",
           x = "Year relative to treatment (t = 0)") %>%
    + scale_x_continuous(breaks=seq(min(df_fc_attgt$time),max(df_fc_attgt$time),by=1)) %>%
    + theme_minimal() %>%
    + theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
      axis.text=element_text(size=11, color = "black"),
      axis.title=element_text(size=14, color = "black", face = "plain"),
      
      plot.caption = element_text(hjust = 0),
      plot.title = element_text(size=16, color = "black", face = "plain", hjust = 0),
      plot.subtitle = element_text(size=12, color = "black", face = "plain", hjust = 0),
      
      strip.text = element_blank(),
      
      #legend.position = "bottom",
      legend.title = element_blank(),
      legend.text=element_text(size=10),
      #legend.spacing.x = unit(1.0, 'cm'),
      legend.spacing.y = unit(0.75, 'cm'),
      legend.key.size = unit(2, 'line'),
      
      panel.grid.major.x = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.x = element_line(color = 'grey80', linewidth = 0.2, linetype = 2),
      panel.grid.major.y = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.y = element_line(color = 'grey80', linewidth = 0.2, linetype = 2)
    )
  
  
  
  ##Saving plots
  tmp = paste(tempdir(), "fig", sep = "/")
  
  ggsave(ifelse(is_m == TRUE,
                yes = paste(tmp, paste0("fig_att_pix_", iso, "_", wdpaid, "_m", ".png"), sep = "/"),
                no = paste(tmp, paste0("fig_att_pix_", iso, "_", wdpaid, "_unm", ".png"), sep = "/")),
         plot = fig_att_pix,
         device = "png",
         height = 6, width = 9)
  ggsave(ifelse(is_m == TRUE,
                yes = paste(tmp, paste0("fig_att_pa_", iso, "_", wdpaid, "_m", ".png"), sep = "/"),
                no = paste(tmp, paste0("fig_att_pa_", iso, "_", wdpaid, "_unm", ".png"), sep = "/")),
         plot = fig_att_pa,
         device = "png",
         height = 6, width = 9)
  ggsave(ifelse(is_m == TRUE,
                yes = paste(tmp, paste0("fig_att_per_", iso, "_", wdpaid, "_m", ".png"), sep = "/"),
                no = paste(tmp, paste0("fig_att_per_", iso, "_", wdpaid, "_unm", ".png"), sep = "/")),
         plot = fig_att_per,
         device = "png",
         height = 6, width = 9)
  ggsave(paste(tmp, paste0("fig_fl_att_", iso, "_", wdpaid, ".png"), sep = "/"),
         plot = fig_fl_att,
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
  
  
  
  #Return outputs
  return(list("df_fc_att" = df_fc_attgt, "df_fl_att" = df_fl_attgt, "df_pre_test" = df_pre_test, "is_ok" = TRUE))
  
    },
  
  error = function(e)
  {
    print(e)
    #cat(paste("Error while computing/plotting DiD :\n", e, "\n"), file = log, append = TRUE)
    print(paste("Error while computing/plotting DiD :\n", e, "\n"))
    return(list("is_ok" = FALSE))
  }
  
  )
  
  return(output)
  
  #TEST : is treatment effect computed by the did package coherent with manual computations ? 
  # --> YES :D
  # test = df_did %>% 
  #   group_by(group, year) %>%
  #   summarize(avgFL_2000_cum = mean(FL_2000_cum, na.rm = TRUE),
  #             avgFC_ha = mean(fc_ha, na.rm = TRUE)) %>%
  #   ungroup() %>%
  #   mutate(fc_te2 = (avgFC_ha[year == 2009 & group == 2] - avgFC_ha[year == 2006 & group == 2]) - (avgFC_ha[year == 2009 & group == 1] - avgFC_ha[year == 2006 & group == 1]),
  #          fl_te2 = (avgFL_2000_cum[year == 2009 & group == 2] - avgFL_2000_cum[year == 2006 & group == 2]) - (avgFL_2000_cum[year == 2009 & group == 1] - avgFL_2000_cum[year == 2006 & group == 1]))
  # 
  
  
}


#Compute the treatment effect for a given protected area. This function can be used on any protected area for which, though no funding information will be displayed contrary to fn_did_att_afd.
## INPUTS 
### iso : the iso3 code for the country considered
### wdpaid : the WDPAID of the PA considered
### data_pa : dataset with information on protected areas, and especially their surfaces
### alpha : the threshold for confidence interval
### is_m : boolean stating whether we compute treatment effects from matched (TRUE) or unmatched treated and control units (FALSE)
### save_dir : the saving directory in the remote storage
### load_dir : the loading directory in the remote storage
### ext_input : the extension of input dataframe
## OUTPUTS
### df_fc_attgt : treatment effect computed for the protected area considered, expressed in avoided deforestation (hectare)
### df_fl_attgt : treatment effect computed for the protected area considered, expressed in change of deforestation rate
### is_ok : a boolean indicating whether or not an error occured inside the function 
### df_did : a pre-did dataframe used by the did::att_gt function to compute treatment effect
### df_pre_test : a dataframe with pre-treatment p-value to test for parallel trend assumption
## DATA SAVED :
### Dynamic treatment effects : avoided deforestation in an average pixel (in ha), avoided deforestation relative to 2000 forest cover, avoided deforestation extrapolated to the entire protected area (in ha), change in deforestation rate (in percentage points)
fn_did_att_general = function(iso, wdpaid, data_pa, alpha, is_m, load_dir, save_dir)
{
  
  output = tryCatch(
    
    {
      
      #Loading matched and unmatched datasets
      df_long_m = s3read_using(data.table::fread,
                               object = paste0(load_dir, "/", iso, "/", wdpaid, "/", paste0("matched_long", "_", iso, "_", wdpaid, ".csv")),
                               bucket = "projet-afd-eva-ap",
                               opts = list("region" = "")) %>%
        dplyr::select(c(region, iso3, country_en, wdpaid, focus, group, assetid, status_yr, year_funding_first, year_funding_all, start_pre_treat_fc, end_pre_treat_fc, start_pre_treat_fl, end_pre_treat_fl, res_m, year, var, fc_ha))
      #dplyr::select(c(region, country_en, iso3, wdpaid, group, status_yr, year_funding_first, year_funding_all, year, var, fc_ha))
      
      df_long_unm = s3read_using(data.table::fread,
                                 object = paste0(load_dir, "/", iso, "/", wdpaid, "/", paste0("unmatched_long", "_", iso, "_", wdpaid, ".csv")),
                                 bucket = "projet-afd-eva-ap",
                                 opts = list("region" = "")) %>%
        dplyr::select(c(region, iso3, country_en, wdpaid, focus, group, assetid, status_yr, year_funding_first, year_funding_all,start_pre_treat_fc, end_pre_treat_fc, start_pre_treat_fl, end_pre_treat_fl, year, res_m, var, fc_ha))
      #dplyr::select(c(region, country_en, iso3, wdpaid, group, status_yr, year_funding_first, year_funding_all, year, var, fc_ha))
      
      # Define the working datasets depending on the is_m value
      if(is_m == TRUE)
      {
        df_long = df_long_m
      } else{df_long = df_long_unm}
      
      #Extract some relevant variables for later plots and treatment effect computations
      df_info = df_long %>% 
        filter(group == 3) %>% 
        slice(1)
      ##Extract spatial resolution of pixels res_m and define pixel area in ha
      res_m = unique(df_long$res_m)
      res_ha = res_m^2*1e-4
      
      ##Extract treatment year
      treatment.year = df_info$status_yr
      
      ##Extract funding years
      funding.years = df_info$year_funding_first
      list.funding.years = df_info$year_funding_all
      
      ##Extract country name
      country.name = df_info$country_en
      
      ##Extract country iso
      country.iso = df_info$iso3
      
      ##Extract region name
      region.name = df_info$region
      
      #Is it a PA of interest ?
      is_focus = as.logical(df_info$focus)
      
      ##Area without overlap
      ### Note that the polygon reported by the WDAP can have a surface different from the reported area : thus the area without overalp can be bigger than the reported area ! 
      ### Example below ("test") with a Kenyan PA.
      area_noverlap_ha = df_long_unm %>%
        filter(group == 3) %>%
        group_by(assetid) %>%
        slice(1) %>%
        ungroup()
      area_noverlap_ha = nrow(area_noverlap_ha)*res_ha
      
      ##Total forest cover before treatment, in ha. Note this forest cover is in the PA without the overlap, as it is computed from the treated pixels. This is more rigorous to extrapolate on these : it corresponds to the area we can arguably isolate the effect of the PA we consider
      ## Global Forest Watch recommends to smooth forest cover on three years average : "Use 3-year moving averages to smooth any inconsistencies between years caused by the availability of Landsat images and better account for delays in detection of late-year fires and other losses." (https://www.globalforestwatch.org/blog/data-and-research/tree-cover-loss-satellite-data-trend-analysis/) 
      ###Extract period where forest cover can be averaged : same as the period where average pre-treatment forest cover is computed
      yr_cover = df_long_unm %>% 
        filter(group == 3) %>% 
        slice(1)
      yr_start_cover = yr_cover$start_pre_treat_fc
      yr_end_cover = yr_cover$end_pre_treat_fc
      ### Compute pre-treatment forest area
      fc_tot_pre_treat = df_long_unm %>%
        filter(group == 3 & year >= yr_start_cover & year <= yr_end_cover) %>%
        group_by(assetid) %>%
        summarize(avg_fc_ha = mean(fc_ha, na.rm = TRUE)) %>%
        ungroup()
      fc_tot_pre_treat = sum(fc_tot_pre_treat$avg_fc_ha, na.rm = TRUE)
      n_pix_fc_pre_treat = fc_tot_pre_treat/res_ha 
      
      ##Extract more information not in the matched dataframe
      wdpa_id = wdpaid #Need to give a name to wdpaid (function argument) different from the variable in the dataset (wdpaid)
      df_info_pa = data_pa %>% 
        filter(wdpaid == wdpa_id) %>% 
        slice(1)
      ### Area reported by the WDPA
      area_ha = data_pa[data_pa$wdpaid == wdpa_id,]$area_km2*100
      ### Name of the PA
      pa.name = data_pa %>% 
        filter(wdpaid == wdpa_id) %>% 
        slice(1)
      pa.name = pa.name$name_pa

      ### AFD project ID
      # id.project = data_pa %>% 
      #   filter(wdpaid == wdpa_id) %>% 
      #   slice(1)
      # id.project = id.project$id_projet
      ### WDPA status
      status.wdpa = df_info_pa$status
      ### IUCN category and description
      iucn.cat = df_info_pa$iucn_cat
      iucn.des = df_info_pa$iucn_des_en
      ### Ecosystem
      eco.wdpa = df_info_pa$marine
      ### Governance
      gov.wdpa = df_info_pa$gov_type
      ### Owner
      own.wdpa = df_info_pa$own_type

      #Extract number of pixels in the PA
      #n_pix_pa = length(unique(filter(df_long_unm, group == 2)$assetid))
      #n_pix_pa = area_ha/res_ha
      
      #Average forest cover in a treated pixel before treatment
      ## For matched 
      avgFC_pre_treat_m = df_long_m %>% 
        filter(group == 3 & year >= yr_start_cover & year <= yr_end_cover) 
      avgFC_pre_treat_m = mean(avgFC_pre_treat_m$fc_ha, na.rm = TRUE)
      ## For unmatched
      avgFC_pre_treat_unm = df_long_unm %>% 
        filter(group == 3 & year >= yr_start_cover & year <= yr_end_cover) 
      avgFC_pre_treat_unm = mean(avgFC_pre_treat_unm$fc_ha, na.rm = TRUE)
      
      #Then modify the dataframe before difference-in-difference computations
      ## Set treatment year = 0 for controls (necessary for did package to consider "never treated" units)
      ## Compute cumulative deforestation relative to 2000 forest cover (outcome where TE is computed)
      ## Add different areas, important to aggregate and interpret the results at country level
      df_did = df_long %>%
        mutate(treatment_year = case_when(group == 2 ~ 0,
                                          group == 3 ~ status_yr), #Set treatment year to 0 for control units (required by did::att_gt)
               time = ifelse(group == 3, yes = year-treatment_year, no = NA),
               .after = status_yr) %>%
        #Remove any year, assetid duplicate (case of WDPAID 555555493 in Kenya, asset id 161654 162057 162457 162856)
        group_by(assetid, year) %>%
        slice(1) %>%
        ungroup() %>%
        group_by(assetid) %>%
        mutate(FL_2000_cum = case_when(fc_ha[year == 2000] > 0 ~ (fc_ha-fc_ha[year == 2000])/fc_ha[year == 2000]*100, 
                                       TRUE ~ 0)) %>% #If forest cover is null in 2000, then it is null in the following years (reforestation not taken inot account here), and deforestation rate is null
        ungroup()
      
      
      #TEST : is treatment effect computed by the did package coherent with manual computations ? 
      # --> YES :D
      # test = df_did %>%
      #   group_by(group, year) %>%
      #   summarize(avgFL_2000_cum = mean(FL_2000_cum, na.rm = TRUE),
      #             avgFC_ha = mean(fc_ha, na.rm = TRUE)) %>%
      #   ungroup() %>%
      #   mutate(fc_te2 = (avgFC_ha[year == 2005 & group == 3] - avgFC_ha[year == 2001 & group == 3]) - (avgFC_ha[year == 2005 & group == 2] - avgFC_ha[year == 2001 & group == 2]),
      #          fl_te2 = (avgFL_2000_cum[year == 2005 & group == 3] - avgFL_2000_cum[year == 2001 & group == 3]) - (avgFL_2000_cum[year == 2005 & group == 3] - avgFL_2000_cum[year == 2001 & group == 2]))


      #Compute dynamic treatment effect with did package. 
      ## Control are "never treated" units, no covariate is added in the regression estimated with doubly-robust method
      ## standard errors are computed with bootstrap, and confidence intervals computed from it.
      ## No clustering is performed as it does not seem relevant in our case (https://blogs.worldbank.org/impactevaluations/when-should-you-cluster-standard-errors-new-wisdom-econometrics-oracle)
      ## Pseudo treatment effects are computed for each pre-treatment year (varying base period)
      
      ##For forest cover (ha and %)
      ### treatment effect computation
      fc_attgt = did::att_gt(yname = "fc_ha",
                             gname = "treatment_year",
                             idname = "assetid",
                             tname = "year",
                             control_group = "nevertreated", #This corresponds to control pixels as defined in the matching , with treatment year set to 0
                             xformla = ~1,
                             alp = alpha, #For 95% confidence interval
                             allow_unbalanced_panel = TRUE, #Ensure no unit is dropped, though every pixel should have data for all years in the period
                             bstrap=TRUE, #Compute bootstrap CI
                             biters = 1000, #The number of bootstrap iteration, 1000 is default
                             cband = TRUE, #Compute CI
                             clustervars = NULL, #No clustering seems relevant to me 
                             base_period = "varying",
                             data = df_did,
                             print_details = F)
      
      pval_pretest_fc = ifelse(is.numeric(fc_attgt$Wpval[1]) == TRUE,
                                yes = fc_attgt$Wpval[1],
                                no = NA)
      
      ##For change in deforestation rate (percentage points)
      ### treatment effect computation
      fl_attgt = did::att_gt(yname = "FL_2000_cum",
                             gname = "treatment_year",
                             idname = "assetid",
                             tname = "year",
                             control_group = "nevertreated", #Thsi corresponds to control pixels as defined in the matching , with treatment year set to 0
                             xformla = ~1,
                             alp = alpha, #For 95% confidence interval
                             allow_unbalanced_panel = TRUE, #Ensure no unit is dropped, though every pixel should have data for all years in the period
                             bstrap=TRUE, #Compute bootstrap CI
                             biters = 1000, #The number of bootstrap iteration, 1000 is default
                             cband = TRUE, #Compute CI
                             clustervars = NULL, #No clustering seems relevant to me
                             base_period = "varying",
                             data = df_did,
                             print_details = F)
      pval_pretest_fl = ifelse(is.numeric(fl_attgt$Wpval[1]) == TRUE,
                               yes = fl_attgt$Wpval[1],
                               no = NA)
      
      ### Store the P-value for pre-test of parallel trends assumption : in a dataframe
      df_pre_test = data.frame("region" = region.name,
                               "country_en" = country.name,
                               "iso3" = country.iso,
                               "name_pa" = pa.name,
                               "wdpaid" = wdpaid, 
                               "pval_fc" = as.numeric(pval_pretest_fc), 
                               "pval_fl" = as.numeric(pval_pretest_fl))

      ### Report results in a dataframe
      ### The computed is at pixel level
      ### This treatment effect is aggregated to protected area by multiplying treatment effect by the number of pixel in the PA. It is also expressed in percentage of pixel area (avoided deforestation in share of pixel area)
      ### confidence intervals (at pixel level) are computed from bootstrap standard errors after a coefficient is applied.
      ### This computation takes the one from did:::summary.MP function, line 15 and 16. 
      ### They are multiplied by the number of pixels to compute confidence intervals for treatment effect at protected area level 
      ### They are divided by the pixel area to compute CI for treatment effect in percentage of pixel area
      df_fc_attgt = data.frame("treatment_year" = fc_attgt$group,
                               "year" = fc_attgt$t,
                               "att_pix" = fc_attgt$att,
                               "c" = fc_attgt$c,
                               "se" = fc_attgt$se,  
                               "n" = fc_attgt$n) %>%
        #Compute treatment effect at PA level and in share of pixel area
        ## att_pa : the total avoided deforestation is the avoided deforestation in ha in a given pixel, multiplied by the number of pixel in the PA.
        ## att_per : avoided deforestation in a pixel, as a share of average forest cover in 2000 in matched treated. Can be extrapolated to full PA in principle (avoided deforestation in share of 2000 forest cover)
        mutate(att_pa = att_pix*n_pix_fc_pre_treat,
               att_per = att_pix/avgFC_pre_treat_m*100) %>% 
        #Compute time relative to treatment year
        mutate(time = year - treatment_year,
               .before = year) %>%
        #Compute confidence intervals
        mutate(cband_lower_pix = round(att_pix-c*se, 4),
               cband_upper_pix = round(att_pix+c*se, 4),
               cband_lower_pa = cband_lower_pix*n_pix_fc_pre_treat,
               cband_upper_pa = cband_upper_pix*n_pix_fc_pre_treat,
               cband_lower_per = case_when(is.na(avgFC_pre_treat_m) == T ~ NA,
                                           TRUE ~ cband_lower_pix/avgFC_pre_treat_m*100),
               cband_upper_per = case_when(is.na(avgFC_pre_treat_m) == T ~ NA,
                                           TRUE ~ cband_upper_pix/avgFC_pre_treat_m*100),
               sig = sign(cband_lower_pix) == sign(cband_upper_pix),
               sig_5 = ifelse(max(time) >=5, yes = sig[time == 5] == TRUE, no = NA),
               sig_10 = ifelse(max(time) >= 10, yes = sig[time == 10] == TRUE, no = NA),
               sig_end = sig[time == max(time)] == TRUE,
               alpha = alpha) %>%
        #Add relevant information
        mutate(region = region.name,
               country_en = country.name,
               iso3 = country.iso,
               name_pa = pa.name,
               wdpaid = wdpaid,
               focus = is_focus,
               area_ha = area_ha,
               area_noverlap_ha = area_noverlap_ha,
               fc_tot_pre_treat = fc_tot_pre_treat,
               res_ha = res_ha,
               status_wdpa = status.wdpa,
               iucn_cat = iucn.cat,
               iucn_des_en = iucn.des,
               gov_type = gov.wdpa,
               own_type = own.wdpa,
               marine = eco.wdpa,
               funding_year = funding.years,
               funding_year_list = list.funding.years,
               .before = "treatment_year")

      # Same for change in deforestation rate
      df_fl_attgt = data.frame("treatment_year" = fl_attgt$group,
                               "year" = fl_attgt$t,
                               "att" = fl_attgt$att,
                               "c" = fl_attgt$c,
                               "se" = fl_attgt$se,
                               "n" = fl_attgt$n) %>%
        #Compute time relative to treatment year
        mutate(time = year - treatment_year,
               .before = year) %>%
        mutate(cband_lower = round(att-c*se, 4),
               cband_upper = round(att+c*se, 4),
               sig = sign(cband_lower) == sign(cband_upper),
               sig_5 = ifelse(max(time) >=5, yes = sig[time == 5] == TRUE, no = NA),
               sig_10 = ifelse(max(time) >= 10, yes = sig[time == 10] == TRUE, no = NA),
               sig_end = sig[time == max(time)] == TRUE,
               alpha = alpha) %>%
        #Compute time relative to treatment year
        mutate(time = year - treatment_year,
               .before = year) %>%
        #Add relevant information
        mutate(region = region.name,
               country_en = country.name,
               iso3 = country.iso,
               name_pa = pa.name,
               wdpaid = wdpaid,
               focus = is_focus,
               area_ha = area_ha,
               area_noverlap_ha = area_noverlap_ha,
               fc_tot_pre_treat = fc_tot_pre_treat,
               res_ha = res_ha,
               status_wdpa = status.wdpa,
               iucn_cat = iucn.cat,
               iucn_des_en = iucn.des,
               gov_type = gov.wdpa,
               own_type = own.wdpa,
               marine = eco.wdpa,
               funding_year = funding.years,
               funding_year_list = list.funding.years,
               .before = "treatment_year")
      
      ###Plot results
      ## treatment effect : avoided deforestation at pixel level (in ha)
      fig_att_pix = ggplot(data = df_fc_attgt,
                           aes(x = time, y = att_pix)) %>%
        + geom_line(color = "#08519C") %>%
        + geom_point(color = "#08519C") %>%
        + geom_ribbon(aes(ymin = cband_lower_pix, ymax = cband_upper_pix),
                      alpha=0.1, fill = "#FB6A4A", color = "black", linetype = "dotted") %>%
        + labs(title = ifelse(is_m == TRUE, 
                              yes = "Deforestation avoided in a pixel, on average (matched units)",
                              no = "Deforestation avoided in a pixel, on average (unmatched units)"),
               subtitle = paste0(pa.name, ", ", country.name, ", implemented in ", treatment.year),
               caption = paste("Ribbon represents", (1-alpha)*100, "% confidence interval | P-value for pre-test of parallel trends assumption :", pval_pretest_fc, "\nWDPA ID :", wdpa_id, "| WDPA reported surface :", format(area_ha, big.mark  = ",", scientific = F, digits = 1), "ha | Surface without overlap :", format(area_noverlap_ha, big.mark  = ",", scientific = F, digits = ), "ha\nTotal forest cover before treatment :", format(fc_tot_pre_treat, big.mark  = ",", scientific = F, digits = ), "ha | Pixel resolution :", res_ha, "ha.\nTreatment effect is interpreted as the deforestation avoided at pixel level in hectare, due to the conservation program.\nA negative effect means the conservation program has caused higher deforestation."),
               y = "Area (ha)",
               x = "Year relative to treatment (t = 0)") %>%
        + scale_x_continuous(breaks=seq(min(df_fc_attgt$time),max(df_fc_attgt$time),by=1)) %>%
        + theme_minimal() %>%
        + theme(
          axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
          axis.text=element_text(size=11, color = "black"),
          axis.title=element_text(size=14, color = "black", face = "plain"),
          
          plot.caption = element_text(hjust = 0),
          plot.title = element_text(size=16, color = "black", face = "plain", hjust = 0),
          plot.subtitle = element_text(size=12, color = "black", face = "plain", hjust = 0),
          
          strip.text = element_blank(),
          
          #legend.position = "bottom",
          legend.title = element_blank(),
          legend.text=element_text(size=10),
          #legend.spacing.x = unit(1.0, 'cm'),
          legend.spacing.y = unit(0.75, 'cm'),
          legend.key.size = unit(2, 'line'),
          
          panel.grid.major.x = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
          panel.grid.minor.x = element_line(color = 'grey80', linewidth = 0.2, linetype = 2),
          panel.grid.major.y = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
          panel.grid.minor.y = element_line(color = 'grey80', linewidth = 0.2, linetype = 2)
        )
      
      # treatment effect : avoided deforestation in terms of 2000 forest cover
      fig_att_per = ggplot(data = df_fc_attgt,
                           aes(x = time, y = att_per)) %>%
        + geom_line(color = "#08519C") %>%
        + geom_point(color = "#08519C") %>%
        + geom_ribbon(aes(ymin = cband_lower_per, ymax = cband_upper_per),
                      alpha=0.1, fill = "#FB6A4A", color = "black", linetype = "dotted") %>%
        + labs(title = ifelse(is_m == TRUE, 
                              yes = "Average deforestation avoided relative to pre-treatment forest cover (matched units)",
                              no = "Average deforestation avoided relative to pre-treatment forest cover (unmatched units)"),
               subtitle = paste0(pa.name, ", ", country.name, ", implemented in ", treatment.year),
               caption = paste("Ribbon represents", (1-alpha)*100, "% confidence interval | P-value for pre-test of parallel trends assumption :", pval_pretest_fc, "\nWDPA ID :", wdpa_id, "| WDPA reported surface :", format(area_ha, big.mark  = ",", scientific = F, digits = 1), "ha | Surface without overlap :", format(area_noverlap_ha, big.mark  = ",", scientific = F, digits = ), "ha\nTotal forest cover before treatment :", format(fc_tot_pre_treat, big.mark  = ",", scientific = F, digits = ), "ha | Pixel resolution :", res_ha, "ha.\nTreatment effect is interpreted as the deforestation avoided in percentage of pre-treatment forest cover.\nA negative effect means the conservation program has caused higher deforestation."),
               y = "%",
               x = "Year relative to treatment (t = 0)") %>%
        + scale_x_continuous(breaks=seq(min(df_fc_attgt$time),max(df_fc_attgt$time),by=1)) %>%
        + theme_minimal() %>%
        + theme(
          axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
          axis.text=element_text(size=11, color = "black"),
          axis.title=element_text(size=14, color = "black", face = "plain"),
          
          plot.caption = element_text(hjust = 0),
          plot.title = element_text(size=16, color = "black", face = "plain", hjust = 0),
          plot.subtitle = element_text(size=12, color = "black", face = "plain", hjust = 0),
          
          strip.text = element_blank(),
          
          #legend.position = "bottom",
          legend.title = element_blank(),
          legend.text=element_text(size=10),
          #legend.spacing.x = unit(1.0, 'cm'),
          legend.spacing.y = unit(0.75, 'cm'),
          legend.key.size = unit(2, 'line'),
          
          panel.grid.major.x = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
          panel.grid.minor.x = element_line(color = 'grey80', linewidth = 0.2, linetype = 2),
          panel.grid.major.y = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
          panel.grid.minor.y = element_line(color = 'grey80', linewidth = 0.2, linetype = 2)
        )
      
      # treatment effect : avoided deforestation in the PA
      fig_att_pa = ggplot(data = df_fc_attgt,
                          aes(x = time, y = att_pa)) %>%
        + geom_line(color = "#08519C") %>%
        + geom_point(color = "#08519C") %>%
        + geom_ribbon(aes(ymin = cband_lower_pa, ymax = cband_upper_pa),
                      alpha=0.1, fill = "#FB6A4A", color = "black", linetype = "dotted") %>%
        + labs(title = ifelse(is_m == TRUE, 
                              yes = "Total deforestation avoided (matched units)",
                              no = "Total deforestation avoided (unmatched units)"),
               subtitle = paste0(pa.name, ", ", country.name, ", implemented in ", treatment.year),
               caption = paste("Ribbon represents", (1-alpha)*100, "% confidence interval | P-value for pre-test of parallel trends assumption :", pval_pretest_fc, "\nWDPA ID :", wdpa_id, "| WDPA reported surface :", format(area_ha, big.mark  = ",", scientific = F, digits = 1), "ha | Surface without overlap :", format(area_noverlap_ha, big.mark  = ",", scientific = F, digits = ), "ha\nTotal forest cover before treatment :", format(fc_tot_pre_treat, big.mark  = ",", scientific = F, digits = ), "ha | Pixel resolution :", res_ha, "ha.\nTreatment effect is interpreted as the total deforestation avoided in the protected areas, in hectare (ha).\nThis measure is an extrapolation to the full protected area of average avoided deforestation at pixel level.\nA negative effect means the conservation program has caused higher deforestation."),
               y = "Forest area (ha)",
               x = "Year relative to treatment (t = 0)") %>%
        + scale_x_continuous(breaks=seq(min(df_fc_attgt$time),max(df_fc_attgt$time),by=1)) %>%
        + theme_minimal() %>%
        + theme(
          axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
          axis.text=element_text(size=11, color = "black"),
          axis.title=element_text(size=14, color = "black", face = "plain"),
          
          plot.caption = element_text(hjust = 0),
          plot.title = element_text(size=16, color = "black", face = "plain", hjust = 0),
          plot.subtitle = element_text(size=12, color = "black", face = "plain", hjust = 0),
          
          strip.text = element_blank(),
          
          #legend.position = "bottom",
          legend.title = element_blank(),
          legend.text=element_text(size=10),
          #legend.spacing.x = unit(1.0, 'cm'),
          legend.spacing.y = unit(0.75, 'cm'),
          legend.key.size = unit(2, 'line'),
          
          panel.grid.major.x = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
          panel.grid.minor.x = element_line(color = 'grey80', linewidth = 0.2, linetype = 2),
          panel.grid.major.y = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
          panel.grid.minor.y = element_line(color = 'grey80', linewidth = 0.2, linetype = 2)
        )
      
      
      # treatment effect : change in deforestation rate
      fig_fl_att = ggplot(data = df_fl_attgt,
                          aes(x = time, y = att)) %>%
        + geom_line(color = "#08519C") %>%
        + geom_point(color = "#08519C") %>%
        + geom_ribbon(aes(ymin = cband_lower, ymax = cband_upper),
                      alpha=0.1, fill = "#FB6A4A", color = "black", linetype = "dotted") %>%
        + labs(title = ifelse(is_m == TRUE, 
                              yes = "Effect of the conservation on the deforestation rate, relative to 2000 (matched units)",
                              no = "Effect of the conservation on the deforestation rate, relative to 2000 (unmatched units)"),
               subtitle = paste0(pa.name, ", ", country.name, ", implemented in ", treatment.year),
               caption = paste("Ribbon represents", (1-alpha)*100, "% confidence interval | P-value for pre-test of parallel trends assumption :", pval_pretest_fl, "\nWDPA ID :", wdpa_id, "| WDPA reported surface :", format(area_ha, big.mark  = ",", scientific = F, digits = 1), "ha | Surface without overlap :", format(area_noverlap_ha, big.mark  = ",", scientific = F, digits = ), "ha\nTotal forest cover before treatment :", format(fc_tot_pre_treat, big.mark  = ",", scientific = F, digits = ), "ha | Pixel resolution :", res_ha, "ha.\nTreatment effect is interpreted as the reduction of cumulated deforestation rate (relative to 2000 forest cover) in percentage points (pp).\nA negative effect means the conservation program has caused higher deforestation."),
               y = "Reduction of deforestation (p.p)",
               x = "Year relative to treatment (t = 0)") %>%
        + scale_x_continuous(breaks=seq(min(df_fc_attgt$time),max(df_fc_attgt$time),by=1)) %>%
        + theme_minimal() %>%
        + theme(
          axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
          axis.text=element_text(size=11, color = "black"),
          axis.title=element_text(size=14, color = "black", face = "plain"),
          
          plot.caption = element_text(hjust = 0),
          plot.title = element_text(size=16, color = "black", face = "plain", hjust = 0),
          plot.subtitle = element_text(size=12, color = "black", face = "plain", hjust = 0),
          
          strip.text = element_blank(),
          
          #legend.position = "bottom",
          legend.title = element_blank(),
          legend.text=element_text(size=10),
          #legend.spacing.x = unit(1.0, 'cm'),
          legend.spacing.y = unit(0.75, 'cm'),
          legend.key.size = unit(2, 'line'),
          
          panel.grid.major.x = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
          panel.grid.minor.x = element_line(color = 'grey80', linewidth = 0.2, linetype = 2),
          panel.grid.major.y = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
          panel.grid.minor.y = element_line(color = 'grey80', linewidth = 0.2, linetype = 2)
        )
      
      
      
      ##Saving plots
      tmp = paste(tempdir(), "fig", sep = "/")
      
      ggsave(ifelse(is_m == TRUE,
                    yes = paste(tmp, paste0("fig_att_pix_", iso, "_", wdpaid, "_m", ".png"), sep = "/"),
                    no = paste(tmp, paste0("fig_att_pix_", iso, "_", wdpaid, "_unm", ".png"), sep = "/")),
             plot = fig_att_pix,
             device = "png",
             height = 6, width = 9)
      ggsave(ifelse(is_m == TRUE,
                    yes = paste(tmp, paste0("fig_att_pa_", iso, "_", wdpaid, "_m", ".png"), sep = "/"),
                    no = paste(tmp, paste0("fig_att_pa_", iso, "_", wdpaid, "_unm", ".png"), sep = "/")),
             plot = fig_att_pa,
             device = "png",
             height = 6, width = 9)
      ggsave(ifelse(is_m == TRUE,
                    yes = paste(tmp, paste0("fig_att_per_", iso, "_", wdpaid, "_m", ".png"), sep = "/"),
                    no = paste(tmp, paste0("fig_att_per_", iso, "_", wdpaid, "_unm", ".png"), sep = "/")),
             plot = fig_att_per,
             device = "png",
             height = 6, width = 9)
      ggsave(paste(tmp, paste0("fig_fl_att_", iso, "_", wdpaid, ".png"), sep = "/"),
             plot = fig_fl_att,
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
      
      
      
      #Return outputs
      return(list("df_fc_att" = df_fc_attgt, "df_fl_att" = df_fl_attgt, "df_pre_test" = df_pre_test, "is_ok" = TRUE))
      
    },
    
    error = function(e)
    {
      print(e)
      #cat(paste("Error while computing/plotting DiD :\n", e, "\n"), file = log, append = TRUE)
      print(paste("Error while computing/plotting DiD :\n", e, "\n"))
      return(list("is_ok" = FALSE))
    }
    
  )
  
  return(output)
  
  #TEST : is treatment effect computed by the did package coherent with manual computations ? 
  # --> YES :D
  # test = df_did %>% 
  #   group_by(group, year) %>%
  #   summarize(avgFL_2000_cum = mean(FL_2000_cum, na.rm = TRUE),
  #             avgFC_ha = mean(fc_ha, na.rm = TRUE)) %>%
  #   ungroup() %>%
  #   mutate(fc_te2 = (avgFC_ha[year == 2009 & group == 2] - avgFC_ha[year == 2006 & group == 2]) - (avgFC_ha[year == 2009 & group == 1] - avgFC_ha[year == 2006 & group == 1]),
  #          fl_te2 = (avgFL_2000_cum[year == 2009 & group == 2] - avgFL_2000_cum[year == 2006 & group == 2]) - (avgFL_2000_cum[year == 2009 & group == 1] - avgFL_2000_cum[year == 2006 & group == 1]))
  # 
  
  
}

#Plot the forest cover loss with 2000 as a base year in treated and control units, before and after matching
##INPUTS
### iso : the iso3 code for the country considered
### wdpaid : the WDPAID of the PA considered
### data_pa : dataset with information on protected areas, and especially their surfaces
### alpha : the threshold for confidence interval
### save_dir : the saving directory in the remote storage
### load_dir : the loading directory in the remote storage
## DATA SAVED
### Plot of forest cover loss with 2000 as a base year in treated and control units, before and after matching
fn_plot_forest_loss = function(iso, wdpaid, data_pa, alpha, load_dir, save_dir)
{
  
  #Loading matched and unmatched data frames
  df_long_m_raw = s3read_using(data.table::fread,
                               object = paste0(load_dir, "/", iso, "/", wdpaid, "/", paste0("matched_long", "_", iso, "_", wdpaid, ".csv")),
                               bucket = "projet-afd-eva-ap",
                               opts = list("region" = "")) %>%
    dplyr::select(c(region, iso3, wdpaid, focus, group, assetid, status_yr, year_funding_first, year_funding_all, start_pre_treat_fc, end_pre_treat_fc, res_m, year, var, fc_ha))
  
  df_long_unm_raw = s3read_using(data.table::fread,
                                 object = paste0(load_dir, "/", iso, "/", wdpaid, "/", paste0("unmatched_long", "_", iso, "_", wdpaid, ".csv")),
                                 bucket = "projet-afd-eva-ap",
                                 opts = list("region" = "")) %>%
    dplyr::select(c(region, iso3, wdpaid, focus, group, assetid, status_yr, year_funding_first, year_funding_all,start_pre_treat_fc, end_pre_treat_fc, year, res_m, var, fc_ha))
  
  
  #Extract relevant information
  df_info = df_long_m_raw %>% 
    filter(group == 3) %>% 
    slice(1)
  ##Spatial resolution of pixels res_m and define pixel area in ha
  res_m = unique(df_long_m_raw$res_m)
  res_ha = res_m^2*1e-4
  
  ##treatment year
 treatment.year = df_info$status_yr
  
  ##funding years
  funding.years = df_info$year_funding_first
  #funding.years = as.numeric(unlist(strsplit(funding.years$year_funding_all, split = ",")))
  
  ##country iso
  country.iso = df_info$iso3
  
  ##region name
 region.name = df_info$region
  
  ##Area of the PA and PA/country name
 wdpa_id = wdpaid
  area_ha = data_pa[data_pa$wdpaid == wdpa_id,]$area_km2*100
  
  country.name = data_pa %>% 
    filter(iso3 == iso) %>% 
    slice(1)
  country.name = country.name$country_en
  pa.name = data_pa %>% 
    filter(wdpaid == wdpa_id) %>% 
    slice(1)
  pa.name = pa.name$name_pa
  
  #Is it a PA of interest ?
  is_focus = as.logical(df_info$focus)
  
  ##Area without overlap
  ### Note that the polygon reported by the WDAP can have a surface different from the reported area : thus the area without overalp can be bigger than the reported area ! 
  ### Example below ("test") with a Kenyan PA.
  area_noverlap_ha = df_long_unm_raw %>%
    filter(group == 3) %>%
    group_by(assetid) %>%
    slice(1) %>%
    ungroup()
  area_noverlap_ha = nrow(area_noverlap_ha)*res_ha
  
  ### Compute forest cover in 2000 in the area without overlap
  fc_tot_2000 = df_long_unm_raw %>%
    filter(group == 3 & year == 2000) 
  fc_tot_2000 = sum(fc_tot_2000$fc_ha, na.rm = TRUE)
  n_pix_fc_2000 = fc_tot_pre_treat/res_ha 
  
  #Forest cover loss in final period year is computed for each pixel relative to 2000, then average forest loss is computed for treated and controls
  year.min = 2000
  year.max = 2021
  
  df_long_m = df_long_m_raw %>%
    #Keep only min and max year to lower computation burden
    filter(year %in% c(year.min, year.max)) %>%
    group_by(assetid) %>%
    mutate(fc_rel00_ha = fc_ha - fc_ha[year == 2000],
           .after = "fc_ha") %>%
    ungroup() %>%
    #Compute average forest cover and forest cover loss relative to 2000 for each group
    group_by(group, year) %>%
    summarise(n= n(),
              avgfc_ha = mean(fc_ha, na.rm = TRUE),
              sdfc_ha = sd(fc_ha, na.rm = TRUE),
              avgfc_rel00_ha = mean(fc_rel00_ha, na.rm = TRUE),
              sdfc_rel00_ha = sd(fc_rel00_ha, na.rm = TRUE),
              fc_ha_ci_upper = avgfc_ha + qt((1-alpha)/2,df=n-1)*sdfc_ha/sqrt(n),
              fc_ha_ci_lower = avgfc_ha - qt((1-alpha)/2,df=n-1)*sdfc_ha/sqrt(n),
              fc_rel00_ha_ci_upper = avgfc_rel00_ha + qt((1-alpha)/2,df=n-1)*sdfc_rel00_ha/sqrt(n),
              fc_rel00_ha_ci_lower = avgfc_rel00_ha - qt((1-alpha)/2,df=n-1)*sdfc_rel00_ha/sqrt(n)
              ) %>%
    #Compute total forest cover and forest loss relative to 2000, knowing area of the PA and average forest share in a pixel in 2000
    #CI are computed at 95% confidence level
    ungroup() %>%
    mutate(#per_fc_2000_avg = min(fc_ha[year == 2000]/res_ha, 1),
      #fc_tot_ha = fc_ha*(area_ha*per_fc_2000_avg/res_ha),
      fc_tot_ha = avgfc_ha*n_pix_fc_2000,
      #fc_tot_rel00_ha = avgfc_rel00_ha*(area_ha*per_fc_2000_avg/res_ha),
      fc_tot_rel00_ha = avgfc_rel00_ha*n_pix_fc_2000,
      fc_tot_ha_ci_upper = fc_ha_ci_upper*n_pix_fc_2000,
      fc_tot_ha_ci_upper = fc_ha_ci_lower*n_pix_fc_2000,
      fc_tot_rel00_ha_ci_upper = fc_rel00_ha_ci_upper*n_pix_fc_2000,
      fc_tot_rel00_ha_ci_lower = fc_rel00_ha_ci_lower*n_pix_fc_2000,
      alpha = alpha,
      matched = T,
      focus = is_focus) %>%
    as.data.frame()
  
  df_long_unm = df_long_unm_raw %>%
    #Keep only min and max year to lower computation burden
    filter(year %in% c(year.min, year.max)) %>%
    #Compute forest loss relative to 2000 in ha for each pixel
    group_by(assetid) %>%
    mutate(fc_rel00_ha = fc_ha - fc_ha[year == 2000],
           .after = "fc_ha") %>%
    ungroup() %>%
    #Compute average forest cover and forest cover loss relative to 2000 for each group, year
    group_by(group, year) %>%
    summarise(n= n(),
              avgfc_ha = mean(fc_ha, na.rm = TRUE),
              sdfc_ha = sd(fc_ha, na.rm = TRUE),
              avgfc_rel00_ha = mean(fc_rel00_ha, na.rm = TRUE),
              sdfc_rel00_ha = sd(fc_rel00_ha, na.rm = TRUE),
              fc_ha_ci_upper = avgfc_ha + qt((1-alpha)/2,df=n-1)*sdfc_ha/sqrt(n),
              fc_ha_ci_lower = avgfc_ha - qt((1-alpha)/2,df=n-1)*sdfc_ha/sqrt(n),
              fc_rel00_ha_ci_upper = avgfc_rel00_ha + qt((1-alpha)/2,df=n-1)*sdfc_rel00_ha/sqrt(n),
              fc_rel00_ha_ci_lower = avgfc_rel00_ha - qt((1-alpha)/2,df=n-1)*sdfc_rel00_ha/sqrt(n)) %>%
    #Compute total forest cover and forest loss relative to 2000, extrapolating knowing forest cover in 2000
    #CI are computed at 95% confidence level
    ungroup() %>%
    mutate(#per_fc_2000_avg = min(fc_ha[year == 2000]/res_ha, 1),
      #fc_tot_ha = fc_ha*(area_ha*per_fc_2000_avg/res_ha),
      fc_tot_ha = avgfc_ha*n_pix_fc_2000,
      #fc_tot_rel00_ha = avgfc_rel00_ha*(area_ha*per_fc_2000_avg/res_ha),
      fc_tot_rel00_ha = avgfc_rel00_ha*n_pix_fc_2000,
      fc_tot_ha_ci_upper = fc_ha_ci_upper*n_pix_fc_2000,
      fc_tot_ha_ci_upper = fc_ha_ci_lower*n_pix_fc_2000,
      fc_tot_rel00_ha_ci_upper = fc_rel00_ha_ci_upper*n_pix_fc_2000,
      fc_tot_rel00_ha_ci_lower = fc_rel00_ha_ci_lower*n_pix_fc_2000,
      alpha = alpha,
      matched = F,
      focus = is_focus) %>%
    as.data.frame()
  
  
  #Define plotting dataset
  df_plot = rbind(df_long_m, df_long_unm) %>%
    mutate(group = case_when(group == 2 ~"Control",
                             group == 3 ~"Treated"),
           region = region.name,
           country_en = country.name,
           iso3 = country.iso,
           wdpaid = wdpaid, 
           name_pa = pa.name,
           area_ha = area_ha,
           area_noverlap_ha = area_noverlap_ha,
           fc_2000_ha = fc_tot_2000)
  
  #Plot
  fct.labs <- c("Before Matching", "After Matching")
  names(fct.labs) <- c(FALSE, TRUE)
  
  fig = ggplot(data = filter(df_plot, year == year.max),
               aes(y = abs(fc_tot_rel00_ha), fill = as.factor(group), x = group)) %>%
    + geom_bar(position =  position_dodge(width = 0.8), stat = "identity", show.legend = FALSE) %>% 
    + geom_errorbar(aes(ymax=abs(fc_tot_rel00_ha_ci_upper), ymin=abs(fc_tot_rel00_ha_ci_lower)), width=0.4, colour="grey60", alpha=0.9, size=1.3) %>%
    + geom_label(aes(label = format(round(abs(fc_tot_rel00_ha), 1), big.mark = ","), y = abs(fc_tot_rel00_ha)), 
                 color = "black",
                 show.legend = FALSE) %>%
    + scale_fill_brewer(name = "Group", palette = "Blues") %>%
    + labs(x = "",
           y = "Forest cover loss (ha)",
           title = paste("Average area deforested between 2000 and", year.max),
           subtitle = paste0(pa.name, " in ", country.name, ", implemented in ", treatment.year, " with a 2000 forest cover of ", format(fc_tot_2000, big.mark = ",", digits = 1, scientific = F), "ha"),
           caption = paste((1-alpha)*100, "% confidence intervals | WDPA reported area :", format(area_ha, big.mark = ",", digits = 1, scientific = F), "ha | Area without overlap :", format(area_noverlap_ha, big.mark = ",", digits = 1, scientific = F), "ha\nWDPA reported area does not necessarily correspond to the area of the reported polygon, explaining discrepancies in the previous areas.")) %>%
    + facet_wrap(~matched,
                 labeller = labeller(matched = fct.labs))  %>%
    + theme_minimal() %>%
    + theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
      axis.text=element_text(size=11, color = "black"),
      axis.title=element_text(size=14, color = "black", face = "plain"),
      
      plot.caption = element_text(hjust = 0),
      plot.title = element_text(size=16, color = "black", face = "plain", hjust = 0),
      plot.subtitle = element_text(size=12, color = "black", face = "plain", hjust = 0),
      
      strip.text = element_text(color = "black", size = 12),
      
      #legend.position = "bottom",
      #legend.title = element_blank(),
      legend.text=element_text(size=10),
      #legend.spacing.x = unit(1.0, 'cm'),
      legend.spacing.y = unit(0.75, 'cm'),
      legend.key.size = unit(2, 'line'),
      
      panel.grid.major.x = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.x = element_line(color = 'grey80', linewidth = 0.2, linetype = 2),
      panel.grid.major.y = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.y = element_line(color = 'grey80', linewidth = 0.2, linetype = 2)
    )
  
  ##Saving plots
  tmp = paste(tempdir(), "fig", sep = "/")
  
  ggsave(paste(tmp, paste0("fig_fl_2000_", year.max, "_m_unm_", iso, "_", wdpaid, ".png"), sep = "/"),
         plot = fig,
         device = "jpg",
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
  
  return(df_plot)
}

#Plot the forest cover loss with 2000 as a base year in treated and control units, before and after matching
##INPUTS
### df_plot_forest_loss : a dataframe with forest loss for each PA in the sample, on the 2000-2021 period
### alpha : the threshold for confidence interval
### save_dir : the saving directory in the remote storage
## DATA SAVED
### Plot of aggregated forest cover loss with 2000 as a base year in treated and control units, before and after matching
fn_plot_forest_loss_agg = function(df_plot_forest_loss, save_dir, alpha)
{
  year.max = max(df_plot_forest_loss$year)
  
  #Compute average and total deforestation ... 
  ## across all protected areas in the sample
  df_plot_forest_loss_agg_all = df_plot_forest_loss %>%
    group_by(matched, group, year) %>%
    summarize(tot_fc_rel00_ha = sum(fc_tot_rel00_ha, na.rm = TRUE),
              tot_fc_rel00_ha_ci_lower = sum(fc_tot_rel00_ha_ci_lower, na.rm = FALSE),
              tot_fc_rel00_ha_ci_upper = sum(fc_tot_rel00_ha_ci_upper, na.rm = FALSE),
              avg_fc_rel00_ha = mean(fc_tot_rel00_ha, na.rm = TRUE),
              avg_fc_rel00_ha_ci_lower = mean(fc_tot_rel00_ha_ci_lower, na.rm = TRUE),
              avg_fc_rel00_ha_ci_upper = mean(fc_tot_rel00_ha_ci_upper, na.rm = TRUE)) %>%
    ungroup() 
  
  ## For a specific subset of protected areas
  ### PA of interest
  df_plot_forest_loss_agg_focus = df_plot_forest_loss %>%
    filter(focus == T) %>%
    group_by(matched, group, year) %>%
    summarize(tot_fc_rel00_ha = sum(fc_tot_rel00_ha, na.rm = TRUE),
              tot_fc_rel00_ha_ci_lower = sum(fc_tot_rel00_ha_ci_lower, na.rm = TRUE),
              tot_fc_rel00_ha_ci_upper = sum(fc_tot_rel00_ha_ci_upper, na.rm = TRUE),
              avg_fc_rel00_ha = mean(fc_tot_rel00_ha, na.rm = TRUE),
              avg_fc_rel00_ha_ci_lower = mean(fc_tot_rel00_ha_ci_lower, na.rm = TRUE),
              avg_fc_rel00_ha_ci_upper = mean(fc_tot_rel00_ha_ci_upper, na.rm = TRUE)) %>%
    ungroup()
  
  ### Other PA
  df_plot_forest_loss_agg_nofocus = df_plot_forest_loss %>%
    filter(focus == F) %>%
    group_by(matched, group, year) %>%
    summarize(tot_fc_rel00_ha = sum(fc_tot_rel00_ha, na.rm = TRUE),
              tot_fc_rel00_ha_ci_lower = sum(fc_tot_rel00_ha_ci_lower, na.rm = TRUE),
              tot_fc_rel00_ha_ci_upper = sum(fc_tot_rel00_ha_ci_upper, na.rm = TRUE),
              avg_fc_rel00_ha = mean(fc_tot_rel00_ha, na.rm = TRUE),
              avg_fc_rel00_ha_ci_lower = mean(fc_tot_rel00_ha_ci_lower, na.rm = TRUE),
              avg_fc_rel00_ha_ci_upper = mean(fc_tot_rel00_ha_ci_upper, na.rm = TRUE)) %>%
    ungroup()
  
  ## For each country
  ### Considering all PA
  # df_plot_forest_loss_agg_ctry_all = df_plot_forest_loss %>%
  #   group_by(region, iso3, country_en, matched, group, year) %>%
  #   summarize(tot_fc_rel00_ha = sum(fc_tot_rel00_ha, na.rm = TRUE),
  #             tot_fc_rel00_ha_ci_lower = sum(fc_tot_rel00_ha_ci_lower, na.rm = FALSE),
  #             tot_fc_rel00_ha_ci_upper = sum(fc_tot_rel00_ha_ci_upper, na.rm = FALSE),
  #             avg_fc_rel00_ha = mean(fc_tot_rel00_ha, na.rm = TRUE),
  #             avg_fc_rel00_ha_ci_lower = mean(fc_tot_rel00_ha_ci_lower, na.rm = TRUE),
  #             avg_fc_rel00_ha_ci_upper = mean(fc_tot_rel00_ha_ci_upper, na.rm = TRUE)) %>%
  #   ungroup() 
  # ### Considering PA of interest
  # df_plot_forest_loss_agg_ctry_focus = df_plot_forest_loss %>%
  #   filter(focus == T) %>%
  #   group_by(region, iso3, country_en, matched, group, year) %>%
  #   summarize(tot_fc_rel00_ha = sum(fc_tot_rel00_ha, na.rm = TRUE),
  #             tot_fc_rel00_ha_ci_lower = sum(fc_tot_rel00_ha_ci_lower, na.rm = FALSE),
  #             tot_fc_rel00_ha_ci_upper = sum(fc_tot_rel00_ha_ci_upper, na.rm = FALSE),
  #             avg_fc_rel00_ha = mean(fc_tot_rel00_ha, na.rm = TRUE),
  #             avg_fc_rel00_ha_ci_lower = mean(fc_tot_rel00_ha_ci_lower, na.rm = TRUE),
  #             avg_fc_rel00_ha_ci_upper = mean(fc_tot_rel00_ha_ci_upper, na.rm = TRUE)) %>%
  #   ungroup() 
  # ### Considering other PA
  # df_plot_forest_loss_agg_ctry_nofocus = df_plot_forest_loss %>%
  #   filter(focus == F) %>%
  #   group_by(region, iso3, country_en, matched, group, year) %>%
  #   summarize(tot_fc_rel00_ha = sum(fc_tot_rel00_ha, na.rm = TRUE),
  #             tot_fc_rel00_ha_ci_lower = sum(fc_tot_rel00_ha_ci_lower, na.rm = FALSE),
  #             tot_fc_rel00_ha_ci_upper = sum(fc_tot_rel00_ha_ci_upper, na.rm = FALSE),
  #             avg_fc_rel00_ha = mean(fc_tot_rel00_ha, na.rm = TRUE),
  #             avg_fc_rel00_ha_ci_lower = mean(fc_tot_rel00_ha_ci_lower, na.rm = TRUE),
  #             avg_fc_rel00_ha_ci_upper = mean(fc_tot_rel00_ha_ci_upper, na.rm = TRUE)) %>%
  #   ungroup() 

  
  ## The number of protected areas (all and in a given subsample, for instance)
  n_pa  = length(unique(df_plot_forest_loss$wdpaid))
  n_focus = length(unique(filter(df_plot_forest_loss, focus == T)$wdpaid))
  n_nofocus = length(unique(filter(df_plot_forest_loss, focus == F)$wdpaid))
  
  ## The total surfaces of protected areas (all and in a given subsample, for instance)
  ### For all PA
  df_area_all = df_plot_forest_loss %>%
    group_by(wdpaid) %>%
    slice(1) %>%
    ungroup()
  area_ha_tot_all = sum(df_area_all$area_ha)
  area_noverlap_ha_tot_all = sum(df_area_all$area_noverlap_ha)
  fc_2000_ha_tot_all = sum(df_area_all$fc_2000_ha)
  fc_2000_ha_avg_all = mean(df_area_all$fc_2000_ha)
  ### For PA of interest
  df_area_focus = df_plot_forest_loss %>%
    filter(focus == T) %>%
    group_by(wdpaid) %>%
    slice(1) %>%
    ungroup()
  area_ha_tot_focus = sum(df_area_focus$area_ha)
  area_noverlap_ha_tot_focus = sum(df_area_focus$area_noverlap_ha)
  fc_2000_ha_tot_focus = sum(df_area_focus$fc_2000_ha)
  fc_2000_ha_avg_focus = mean(df_area_focus$fc_2000_ha)
  ### For other PA
  df_area_nofocus = df_plot_forest_loss %>%
    filter(focus == F) %>%
  group_by(wdpaid) %>%
    slice(1) %>%
    ungroup()
  area_ha_tot_nofocus = sum(df_area_nofocus$area_ha)
  area_noverlap_ha_tot_nofocus = sum(df_area_nofocus$area_noverlap_ha)
  fc_2000_ha_tot_nofocus = sum(df_area_nofocus$fc_2000_ha)
  fc_2000_ha_avg_nofocus = mean(df_area_nofocus$fc_2000_ha)
  
  
  # Define the figures
  fct.labs <- c("Before Matching", "After Matching")
  names(fct.labs) <- c(FALSE, TRUE)
  ## Total deforestation
  ### PA of interest
  fig_forest_loss_agg_tot_focus = ggplot(data = filter(df_plot_forest_loss_agg_focus, year == year.max),
                                         aes(y = abs(tot_fc_rel00_ha), fill = as.factor(group), x = group)) %>%
    + geom_bar(position =  position_dodge(width = 0.8), stat = "identity", show.legend = FALSE) %>%
    + geom_errorbar(aes(ymax=abs(tot_fc_rel00_ha_ci_upper), ymin=abs(tot_fc_rel00_ha_ci_lower)), width=0.3, colour="grey70", alpha=0.9, size=1) %>%
    + geom_label(aes(label = paste("~",format(round(abs(tot_fc_rel00_ha), 1), big.mark = ",")), y = 0),
                 vjust = -0.5,
                 color = "black",
                 show.legend = FALSE) %>%
    + scale_fill_brewer(name = "Group", palette = "Blues") %>%
    + labs(x = "",
           y = "Forest cover loss (ha)",
           title = paste("Total area deforested between 2000 and", year.max),
           subtitle = paste0("Sample : protected areas of interest (" , n_focus, " areas with ", format(fc_2000_ha_tot_focus, big.mark = ","), " ha of total forest cover in 2000)"),
           caption = paste(((1-alpha)*100), "% confidence intervals")) %>%
    + facet_wrap(~matched,
                 labeller = labeller(matched = fct.labs))  %>%
    + theme_minimal() %>%
    + theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
      axis.text=element_text(size=11, color = "black"),
      axis.title=element_text(size=14, color = "black", face = "plain"),
      
      plot.caption = element_text(hjust = 0),
      plot.title = element_text(size=16, color = "black", face = "plain", hjust = 0),
      plot.subtitle = element_text(size=12, color = "black", face = "plain", hjust = 0),
      
      strip.text = element_text(color = "black", size = 12),
      
      #legend.position = "bottom",
      #legend.title = element_blank(),
      legend.text=element_text(size=10),
      #legend.spacing.x = unit(1.0, 'cm'),
      legend.spacing.y = unit(0.75, 'cm'),
      legend.key.size = unit(2, 'line'),
      
      panel.grid.major.x = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.x = element_line(color = 'grey80', linewidth = 0.2, linetype = 2),
      panel.grid.major.y = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.y = element_line(color = 'grey80', linewidth = 0.2, linetype = 2)
    )
  
  fig_forest_loss_agg_tot_nofocus = ggplot(data = filter(df_plot_forest_loss_agg_nofocus, year == year.max),
                                         aes(y = abs(tot_fc_rel00_ha), fill = as.factor(group), x = group)) %>%
    + geom_bar(position =  position_dodge(width = 0.8), stat = "identity", show.legend = FALSE) %>%
    + geom_errorbar(aes(ymax=abs(tot_fc_rel00_ha_ci_upper), ymin=abs(tot_fc_rel00_ha_ci_lower)), width=0.3, colour="grey70", alpha=0.9, size=1) %>%
    + geom_label(aes(label = paste("~",format(round(abs(tot_fc_rel00_ha), 1), big.mark = ",")), y = 0),
                 vjust = -0.5,
                 color = "black",
                 show.legend = FALSE) %>%
    + scale_fill_brewer(name = "Group", palette = "Blues") %>%
    + labs(x = "",
           y = "Forest cover loss (ha)",
           title = paste("Total area deforested between 2000 and", year.max),
           subtitle = paste0("Sample : other protected areas (" , n_nofocus, " areas with ", format(fc_2000_ha_tot_nofocus, big.mark = ","), " ha of total forest cover in 2000)"),
           caption = paste(((1-alpha)*100), "% confidence intervals")) %>%
    + facet_wrap(~matched,
                 labeller = labeller(matched = fct.labs))  %>%
    + theme_minimal() %>%
    + theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
      axis.text=element_text(size=11, color = "black"),
      axis.title=element_text(size=14, color = "black", face = "plain"),
      
      plot.caption = element_text(hjust = 0),
      plot.title = element_text(size=16, color = "black", face = "plain", hjust = 0),
      plot.subtitle = element_text(size=12, color = "black", face = "plain", hjust = 0),
      
      strip.text = element_text(color = "black", size = 12),
      
      #legend.position = "bottom",
      #legend.title = element_blank(),
      legend.text=element_text(size=10),
      #legend.spacing.x = unit(1.0, 'cm'),
      legend.spacing.y = unit(0.75, 'cm'),
      legend.key.size = unit(2, 'line'),
      
      panel.grid.major.x = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.x = element_line(color = 'grey80', linewidth = 0.2, linetype = 2),
      panel.grid.major.y = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.y = element_line(color = 'grey80', linewidth = 0.2, linetype = 2)
    )
  
  ## Average deforestation
  ### For PA of interest
  fig_forest_loss_agg_avg_focus = ggplot(data = filter(df_plot_forest_loss_agg_focus, year == year.max),
                                           aes(y = abs(avg_fc_rel00_ha), fill = as.factor(group), x = group)) %>%
    + geom_bar(position =  position_dodge(width = 0.8), stat = "identity", show.legend = FALSE) %>%
    + geom_errorbar(aes(ymax=abs(avg_fc_rel00_ha_ci_upper), ymin=abs(avg_fc_rel00_ha_ci_lower)), width=0.3, colour="grey70", alpha=0.9, size=1) %>%
    + geom_label(aes(label = paste("~",format(round(abs(avg_fc_rel00_ha), 1), big.mark = ",")), y = 0),
                 vjust = -0.5,
                 color = "black",
                 show.legend = FALSE) %>%
    + scale_fill_brewer(name = "Group", palette = "Blues") %>%
    + labs(x = "",
           y = "Forest cover loss (ha)",
           title = paste("Area deforested in protected areas on average, between 2000 and", year.max),
           subtitle = paste0("Sample : protected areas of interest (" , n_focus, " areas with ", format(fc_2000_ha_avg_focus, big.mark = ","), " ha of forest cover in 2000 on average)"),
           caption = paste(((1-alpha)*100), "% confidence intervals")) %>%
    + facet_wrap(~matched,
                 labeller = labeller(matched = fct.labs))  %>%
    + theme_minimal() %>%
    + theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
      axis.text=element_text(size=11, color = "black"),
      axis.title=element_text(size=14, color = "black", face = "plain"),
      
      plot.caption = element_text(hjust = 0),
      plot.title = element_text(size=16, color = "black", face = "plain", hjust = 0),
      plot.subtitle = element_text(size=12, color = "black", face = "plain", hjust = 0),
      
      strip.text = element_text(color = "black", size = 12),
      
      #legend.position = "bottom",
      #legend.title = element_blank(),
      legend.text=element_text(size=10),
      #legend.spacing.x = unit(1.0, 'cm'),
      legend.spacing.y = unit(0.75, 'cm'),
      legend.key.size = unit(2, 'line'),
      
      panel.grid.major.x = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.x = element_line(color = 'grey80', linewidth = 0.2, linetype = 2),
      panel.grid.major.y = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.y = element_line(color = 'grey80', linewidth = 0.2, linetype = 2)
    )
  
  ## For other PA
  fig_forest_loss_agg_avg_nofocus = ggplot(data = filter(df_plot_forest_loss_agg_nofocus, year == year.max),
                                           aes(y = abs(avg_fc_rel00_ha), fill = as.factor(group), x = group)) %>%
    + geom_bar(position =  position_dodge(width = 0.8), stat = "identity", show.legend = FALSE) %>%
    + geom_errorbar(aes(ymax=abs(avg_fc_rel00_ha_ci_upper), ymin=abs(avg_fc_rel00_ha_ci_lower)), width=0.3, colour="grey70", alpha=0.9, size=1) %>%
    + geom_label(aes(label = paste("~",format(round(abs(avg_fc_rel00_ha), 1), big.mark = ",")), y = 0),
                 vjust = -0.5,
                 color = "black",
                 show.legend = FALSE) %>%
    + scale_fill_brewer(name = "Group", palette = "Blues") %>%
    + labs(x = "",
           y = "Forest cover loss (ha)",
           title = paste("Area deforested in protected areas on average, between 2000 and", year.max),
           subtitle = paste0("Sample : other protected areas (" , n_nofocus, " areas with ", format(fc_2000_ha_avg_nofocus, big.mark = ","), " ha of forest cover in 2000 on average)"),
           caption = paste(((1-alpha)*100), "% confidence intervals")) %>%
    + facet_wrap(~matched,
                 labeller = labeller(matched = fct.labs))  %>%
    + theme_minimal() %>%
    + theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
      axis.text=element_text(size=11, color = "black"),
      axis.title=element_text(size=14, color = "black", face = "plain"),
      
      plot.caption = element_text(hjust = 0),
      plot.title = element_text(size=16, color = "black", face = "plain", hjust = 0),
      plot.subtitle = element_text(size=12, color = "black", face = "plain", hjust = 0),
      
      strip.text = element_text(color = "black", size = 12),
      
      #legend.position = "bottom",
      #legend.title = element_blank(),
      legend.text=element_text(size=10),
      #legend.spacing.x = unit(1.0, 'cm'),
      legend.spacing.y = unit(0.75, 'cm'),
      legend.key.size = unit(2, 'line'),
      
      panel.grid.major.x = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.x = element_line(color = 'grey80', linewidth = 0.2, linetype = 2),
      panel.grid.major.y = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.y = element_line(color = 'grey80', linewidth = 0.2, linetype = 2)
    )
  
  ##Saving plots
  tmp = paste(tempdir(), "fig", sep = "/")
  
  ggsave(paste(tmp, paste0("fig_fl_2000_", year.max, "_agg_tot_focus.png"), sep = "/"),
         plot = fig_forest_loss_agg_tot_focus,
         device = "png",
         height = 6, width = 9)
  ggsave(paste(tmp, paste0("fig_fl_2000_", year.max, "_agg_tot_nofocus.png"), sep = "/"),
         plot = fig_forest_loss_agg_tot_nofocus,
         device = "png",
         height = 6, width = 9)
  ggsave(paste(tmp, paste0("fig_fl_2000_", year.max, "_agg_avg_focus.png"), sep = "/"),
         plot = fig_forest_loss_agg_avg_focus,
         device = "png",
         height = 6, width = 9)
  ggsave(paste(tmp, paste0("fig_fl_2000_", year.max, "_agg_avg_nofocus.png"), sep = "/"),
         plot = fig_forest_loss_agg_avg_nofocus,
         device = "png",
         height = 6, width = 9)
  
  files <- list.files(tmp, full.names = TRUE)
  ##Add each file in the bucket (same foler for every file in the temp)
  for(f in files) 
  {
    cat("Uploading file", paste0("'", f, "'"), "\n")
    aws.s3::put_object(file = f, 
                       bucket = paste("projet-afd-eva-ap", save_dir, sep = "/"),
                       region = "", show_progress = TRUE)
  }
  
  do.call(file.remove, list(list.files(tmp, full.names = TRUE)))

  }


# Plotting the treatment effect of each protected area analyzed in the same graph. This function suits for AFD supported protected areas : it includes funding information to the table and figures
## INPUTS
### df_fc_att : a dataset with treatment effects for each protected area in the sample, expressed as avoided deforestation (hectare)
### df_fl_att : a dataset with treatment effects for each protected area in the sample, expressed as change in deforestation rate
### alpha : the threshold for confidence interval
### save_dir : the saving directory in the remote storage
## DATA SAVED
### Tables and figures : treatment effects computed for each protected area in the sample, expressed as avoided deforestaion (hectare and percentage of 2000 forest cover) and change in deforestation rate.
fn_plot_att_afd = function(df_fc_att, df_fl_att, alpha = alpha, save_dir)
{
  
  #list of PAs and two time periods
  list_ctry_plot = df_fc_att %>%
    dplyr::select(iso3, country_en, wdpaid, iucn_cat) %>%
    unique() %>%
    group_by(iso3, country_en, wdpaid) %>%
    summarize(time = c(5, 10),
              iucn_wolf = case_when(iucn_cat %in% c("I", "II", "III", "IV") ~ "Strict",
                                    iucn_cat %in% c("V", "VI") ~ "Non strict",
                                    grepl("not", iucn_cat, ignore.case = TRUE) ~ "Unknown")) %>%
    ungroup()
  
  #treatment effect for each wdpa (some have not on the two time periods)
  temp_fc = df_fc_att %>%
    dplyr::select(c(region, iso3, country_en, wdpaid, name_pa, iucn_cat, treatment_year, time, year, att_per, cband_lower_per, cband_upper_per, att_pa, cband_lower_pa, cband_upper_pa)) %>%
    mutate(sig_pa = sign(cband_lower_pa) == sign(cband_upper_pa),
           sig_per = sign(cband_lower_per) == sign(cband_upper_per)) %>%
    filter(time %in% c(5, 10)) 
  temp_fl = df_fl_att %>%
    dplyr::select(c(region, iso3, country_en, wdpaid, name_pa, iucn_cat, treatment_year, time, year, att, cband_lower, cband_upper)) %>%
    mutate(sig = sign(cband_lower) == sign(cband_upper)) %>%
    filter(time %in% c(5, 10)) 
  
  #Att for each WDPAID, for each period (NA if no value)
  df_plot_fc_att = left_join(list_ctry_plot, temp_fc, by = c("iso3", "country_en", "wdpaid", "time")) %>%
    group_by(time, country_en) %>%
    arrange(country_en) %>%
    mutate(country_en = paste0(country_en, " (", LETTERS[row_number()], ")")) %>%
    ungroup() 
  df_plot_fl_att = left_join(list_ctry_plot, temp_fl, by = c("iso3", "country_en", "wdpaid", "time"))%>%
    group_by(time, country_en) %>%
    arrange(country_en) %>%
    mutate(country_en = paste0(country_en, " (", LETTERS[row_number()], ")")) %>%
    ungroup()
  
  #Plots
  names = c(`5` = "5 years after treatment",
            `10` = "10 years after treatment",
            `Strict` = "Strict\nIUCN cat. I-IV",
            `Non strict` = "Non strict\nIUCN V-VI",
            `Unknown` = "Unknown")
  
  ## Att in share of pre-treatment forest cover
  fig_att_per = ggplot(df_plot_fc_att, 
                       aes(x = att_per, 
                           y = factor(country_en, levels = unique(rev(sort(country_en)))),
                           xmin = cband_lower_per, xmax = cband_upper_per)) %>%
    + geom_point(aes(color = sig_per)) %>%
    + geom_vline(xintercept = 0) %>%
    + geom_errorbarh(aes(color = sig_per)) %>% 
    + scale_color_discrete(name = paste0("Significance\n(", (1-alpha)*100, "% level)"),
                           na.translate = F) %>%
    # + scale_x_continuous(breaks=seq(min(df_plot_fc_att$att_per, na.rm = TRUE),max(df_plot_fc_att$att_per, na.rm = TRUE),by=1)) %>%
    + facet_grid(~time,scales="free", space="free",  labeller= as_labeller(names)) %>%
    + labs(title = "Deforestation avoided relative to pre-treatment forest cover",
           x = "%",
           y = "") %>%
    + theme_minimal() %>%
    + theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
      axis.text=element_text(size=11, color = "black"),
      axis.title=element_text(size=14, color = "black", face = "plain"),
      
      plot.caption = element_text(hjust = 0),
      plot.title = element_text(size=16, color = "black", face = "plain", hjust = 0),
      plot.subtitle = element_text(size=12, color = "black", face = "plain", hjust = 0),
      
      strip.text = element_text(color = "black", size = 12),
      strip.clip = "off",
      panel.spacing = unit(2, "lines"),
      
      #legend.position = "bottom",
      legend.text=element_text(size=10),
      #legend.spacing.x = unit(1.0, 'cm'),
      #legend.spacing.y = unit(0.75, 'cm'),
      legend.key.size = unit(2, 'line'),
      
      panel.grid.major.x = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.x = element_line(color = 'grey80', linewidth = 0.2, linetype = 2),
      panel.grid.major.y = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.y = element_line(color = 'grey80', linewidth = 0.2, linetype = 2)
    )
  
  fig_att_per_iucn = ggplot(df_plot_fc_att, 
                       aes(x = att_per, 
                           y = factor(country_en, levels = unique(rev(sort(country_en)))),
                           xmin = cband_lower_per, xmax = cband_upper_per)) %>%
    + geom_point(aes(color = sig_per)) %>%
    + geom_vline(xintercept = 0) %>%
    + geom_errorbarh(aes(color = sig_per)) %>% 
    + scale_color_discrete(name = paste0("Significance\n(", (1-alpha)*100, "% level)"),
                           na.translate = F) %>%
    # + scale_x_continuous(breaks=seq(min(df_plot_fc_att$att_per, na.rm = TRUE),max(df_plot_fc_att$att_per, na.rm = TRUE),by=1)) %>%
    + facet_grid(iucn_wolf~time,scales="free", space="free",  labeller= as_labeller(names)) %>%
    + labs(title = "Deforestation avoided relative to pre-treatment forest cover",
           x = "%",
           y = "") %>%
    + theme_minimal() %>%
    + theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
      axis.text=element_text(size=11, color = "black"),
      axis.title=element_text(size=14, color = "black", face = "plain"),
      
      plot.caption = element_text(hjust = 0),
      plot.title = element_text(size=16, color = "black", face = "plain", hjust = 0),
      plot.subtitle = element_text(size=12, color = "black", face = "plain", hjust = 0),
      
      strip.text = element_text(color = "black", size = 12),
      strip.clip = "off",
      panel.spacing = unit(2, "lines"),
      
      #legend.position = "bottom",
      legend.text=element_text(size=10),
      #legend.spacing.x = unit(1.0, 'cm'),
      #legend.spacing.y = unit(0.75, 'cm'),
      legend.key.size = unit(2, 'line'),
      
      panel.grid.major.x = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.x = element_line(color = 'grey80', linewidth = 0.2, linetype = 2),
      panel.grid.major.y = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.y = element_line(color = 'grey80', linewidth = 0.2, linetype = 2)
    )

    
  ##treatment effect : total deforestation avoided
  fig_att_pa = ggplot(df_plot_fc_att, 
                      aes(x = att_pa, 
                          y = factor(country_en, levels = unique(rev(sort(country_en)))),
                          xmin = cband_lower_pa, xmax = cband_upper_pa)) %>%
    + geom_point(aes(color = sig_pa)) %>%
    + geom_vline(xintercept = 0) %>%
    + geom_errorbarh(aes(color = sig_pa)) %>% 
    + scale_color_discrete(name = paste0("Significance\n(", (1-alpha)*100, "% level)"),
                           na.translate = F) %>%
    + facet_grid(~time,scales="free", space="free",  labeller= as_labeller(names)) %>%
    + labs(title = "Total deforestation avoided",
           x = "ha",
           y = "") %>%
    + theme_minimal() %>%
    + theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
      axis.text=element_text(size=11, color = "black"),
      axis.title=element_text(size=14, color = "black", face = "plain"),
      
      plot.caption = element_text(hjust = 0),
      plot.title = element_text(size=16, color = "black", face = "plain", hjust = 0),
      plot.subtitle = element_text(size=12, color = "black", face = "plain", hjust = 0),
      
      strip.text = element_text(color = "black", size = 12),
      strip.clip = "off",
      panel.spacing = unit(2, "lines"),
      
      #legend.position = "bottom",
      legend.text=element_text(size=10),
      #legend.spacing.x = unit(1.0, 'cm'),
      #legend.spacing.y = unit(0.75, 'cm'),
      legend.key.size = unit(2, 'line'),
      
      panel.grid.major.x = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.x = element_line(color = 'grey80', linewidth = 0.2, linetype = 2),
      panel.grid.major.y = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.y = element_line(color = 'grey80', linewidth = 0.2, linetype = 2)
    )
  
  fig_att_pa_iucn = ggplot(df_plot_fc_att, 
                      aes(x = att_pa, 
                          y = factor(country_en, levels = unique(rev(sort(country_en)))),
                          xmin = cband_lower_pa, xmax = cband_upper_pa)) %>%
    + geom_point(aes(color = sig_pa)) %>%
    + geom_vline(xintercept = 0) %>%
    + geom_errorbarh(aes(color = sig_pa)) %>% 
    + scale_color_discrete(name = paste0("Significance\n(", (1-alpha)*100, "% level)"),
                           na.translate = F) %>%
    + facet_grid(iucn_wolf~time,scales="free", space="free",  labeller= as_labeller(names)) %>%
    + labs(title = "Total deforestation avoided",
           x = "ha",
           y = "") %>%
    + theme_minimal() %>%
    + theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
      axis.text=element_text(size=11, color = "black"),
      axis.title=element_text(size=14, color = "black", face = "plain"),
      
      plot.caption = element_text(hjust = 0),
      plot.title = element_text(size=16, color = "black", face = "plain", hjust = 0),
      plot.subtitle = element_text(size=12, color = "black", face = "plain", hjust = 0),
      
      strip.text = element_text(color = "black", size = 12),
      strip.clip = "off",
      panel.spacing = unit(2, "lines"),
      
      #legend.position = "bottom",
      legend.text=element_text(size=10),
      #legend.spacing.x = unit(1.0, 'cm'),
      #legend.spacing.y = unit(0.75, 'cm'),
      legend.key.size = unit(2, 'line'),
      
      panel.grid.major.x = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.x = element_line(color = 'grey80', linewidth = 0.2, linetype = 2),
      panel.grid.major.y = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.y = element_line(color = 'grey80', linewidth = 0.2, linetype = 2)
    )
  
  ##treatment effect : avoided deforestation in percentage points
  fig_att_fl = ggplot(df_plot_fl_att, 
                      aes(x = att, 
                          y = factor(country_en, levels = unique(rev(sort(country_en)))),
                          xmin = cband_lower, xmax = cband_upper)) %>%
    + geom_point(aes(color = sig)) %>%
    + geom_vline(xintercept = 0) %>%
    + geom_errorbarh(aes(color = sig)) %>% 
    + scale_color_discrete(name = paste0("Significance\n(", (1-alpha)*100, "% level)"),
                           na.translate = F) %>%
    + facet_grid(~time,scales="free", space="free",  labeller= as_labeller(names)) %>%
    + labs(title = "Reduction of deforestation due to the conservation",
           x = "p.p.",
           y = "") %>%
    + theme_minimal() %>%
    + theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
      axis.text=element_text(size=11, color = "black"),
      axis.title=element_text(size=14, color = "black", face = "plain"),
      
      plot.caption = element_text(hjust = 0),
      plot.title = element_text(size=16, color = "black", face = "plain", hjust = 0),
      plot.subtitle = element_text(size=12, color = "black", face = "plain", hjust = 0),
      
      strip.text = element_text(color = "black", size = 12),
      strip.clip = "off",
      panel.spacing = unit(2, "lines"),
      
      #legend.position = "bottom",
      legend.text=element_text(size=10),
      #legend.spacing.x = unit(1.0, 'cm'),
      #legend.spacing.y = unit(0.75, 'cm'),
      legend.key.size = unit(2, 'line'),
      
      panel.grid.major.x = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.x = element_line(color = 'grey80', linewidth = 0.2, linetype = 2),
      panel.grid.major.y = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.y = element_line(color = 'grey80', linewidth = 0.2, linetype = 2)
    )
  
  fig_att_fl_iucn = ggplot(df_plot_fl_att, 
                      aes(x = att, 
                          y = factor(country_en, levels = unique(rev(sort(country_en)))),
                          xmin = cband_lower, xmax = cband_upper)) %>%
    + geom_point(aes(color = sig)) %>%
    + geom_vline(xintercept = 0) %>%
    + geom_errorbarh(aes(color = sig)) %>% 
    + scale_color_discrete(name = paste0("Significance\n(", (1-alpha)*100, "% level)"),
                           na.translate = F) %>%
    + facet_grid(iucn_wolf~time, scales="free", space="free",  labeller= as_labeller(names)) %>%
    + labs(title = "Reduction of deforestation due to the conservation",
           x = "p.p.",
           y = "") %>%
    + theme_minimal() %>%
    + theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
      axis.text=element_text(size=11, color = "black"),
      axis.title=element_text(size=14, color = "black", face = "plain"),
      
      plot.caption = element_text(hjust = 0),
      plot.title = element_text(size=16, color = "black", face = "plain", hjust = 0),
      plot.subtitle = element_text(size=12, color = "black", face = "plain", hjust = 0),
      
      strip.text = element_text(color = "black", size = 12),
      strip.clip = "off",
      panel.spacing = unit(2, "lines"),
      
      #legend.position = "bottom",
      legend.text=element_text(size=10),
      #legend.spacing.x = unit(1.0, 'cm'),
      #legend.spacing.y = unit(0.75, 'cm'),
      legend.key.size = unit(2, 'line'),
      
      panel.grid.major.x = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.x = element_line(color = 'grey80', linewidth = 0.2, linetype = 2),
      panel.grid.major.y = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.y = element_line(color = 'grey80', linewidth = 0.2, linetype = 2)
    )
  
  
  #Tables 
  ## treatment effect : percentage of deforestation avoided
  tbl_fc_att_per = df_fc_att %>%
    mutate(sig_per = case_when(sign(cband_lower_per) == sign(cband_upper_per) ~ "Yes",
                               sign(cband_lower_per) != sign(cband_upper_per) ~ "No"),
           iucn_wolf = case_when(iucn_cat %in% c("I", "II", "III", "IV") ~ "Strict",
                                 iucn_cat %in% c("V", "VI") ~ "Non strict",
                                 grepl("not", iucn_cat, ignore.case = TRUE) ~ "Unknown"),
           dept_report = case_when(dept_report == "Léa Poulin,Pierre-Yves Durand,Ingrid Dallmann" ~ "Unknown",
                                   TRUE ~ dept_report),
           kfw = case_when(kfw == TRUE ~ "Yes", kfw == FALSE ~ "No"),
           ffem = case_when(ffem == TRUE ~ "Yes", ffem == FALSE ~ "No"),
           funding_year_list = case_when(is.na(funding_year_list) == TRUE ~ "Unknown",
                                         TRUE ~ funding_year_list),
           name_pa = case_when(nchar(name_pa) <= 25 ~ stri_trans_general(name_pa, id = "Latin-ASCII"),
                               nchar(name_pa) > 25 ~ stri_trans_general(paste0(substr(name_pa, 1, 25), "..."),  id = "Latin-ASCII"))
    ) %>%
    dplyr::select(c(name_pa, id_projet, dept_report, country_en, treatment_year, funding_year_list, fund_type, kfw, ffem, iucn_wolf, gov_type, time, att_per, sig_per)) %>%
    filter(time %in% c(5, 10)) %>%
    pivot_wider(values_from = c("att_per", "sig_per"), names_from = c("time", "time")) %>%
    dplyr::select(c(name_pa, id_projet, dept_report, country_en, treatment_year, funding_year_list, kfw, ffem, iucn_wolf, att_per_5, sig_per_5, att_per_10, sig_per_10)) %>%
    #dplyr::select(c(name_pa, id_projet, dept_report, country_en, treatment_year, funding_year_list, fund_type, kfw, ffem, iucn_wolf, gov_type, att_per_5, sig_per_5, att_per_10, sig_per_10)) %>%
    mutate(across(.cols = starts_with(c("att", "sig")),
                  .fns = \(x) case_when(is.na(x) == TRUE ~ "/", TRUE ~ as.character(format(x, digit = 1))))) %>%
    rename("Effect (5 y., %)" = "att_per_5",
           "Signi. (5 y.)" = "sig_per_5",
           "Effect (10 y., %)" = "att_per_10",
           "Signi. (10 y.)" = "sig_per_10") 
  # names(tbl_fc_att_per) = c("Name", "Project ID", "Tech. div.", "Country", "Creation", "Funding year", "Type of funding", "KfW", "FFEM", "Protection", 
  #                           "Governance", "Effect (5 y., %)", "Significance (5 y.)","Effect (10 y., %)", "Significance (10 y.)")
  names(tbl_fc_att_per) = c("Name", "Project ID", "Tech. div.", "Country", "Creation", "Funding year", "KfW", "FFEM", "Protection", 
                            "Effect (5 y., %)", "Signi. (5 y.)","Effect (10 y., %)", "Signi. (10 y.)")
  
  # treatment effect : total deforestation avoided 
  tbl_fc_att_pa = df_fc_att %>%
    mutate(sig_pa = case_when(sign(cband_lower_pa) == sign(cband_upper_pa) ~ "Yes",
                              sign(cband_lower_pa) != sign(cband_upper_pa) ~ "No"),
           iucn_wolf = case_when(iucn_cat %in% c("I", "II", "III", "IV") ~ "Strict",
                                 iucn_cat %in% c("V", "VI") ~ "Non strict",
                                 grepl("not", iucn_cat, ignore.case = TRUE) ~ "Unknown"),
           dept_report = case_when(dept_report == "Léa Poulin,Pierre-Yves Durand,Ingrid Dallmann" ~ "Unknown",
                                   TRUE ~ dept_report),
           kfw = case_when(kfw == TRUE ~ "Yes", kfw == FALSE ~ "No"),
           ffem = case_when(ffem == TRUE ~ "Yes", ffem == FALSE ~ "No"),
           funding_year_list = case_when(is.na(funding_year_list) == TRUE ~ "Unknown",
                                         TRUE ~ funding_year_list),
           name_pa = case_when(nchar(name_pa) <= 25 ~ stri_trans_general(name_pa, id = "Latin-ASCII"),
                               nchar(name_pa) > 25 ~ stri_trans_general(paste0(substr(name_pa, 1, 25), "..."),  id = "Latin-ASCII"))
    ) %>%
    dplyr::select(c(name_pa, id_projet, dept_report, country_en, treatment_year, funding_year_list, fund_type, kfw, ffem, iucn_wolf, gov_type, time, att_pa, sig_pa)) %>%
    filter(time %in% c(5, 10)) %>%
    pivot_wider(values_from = c("att_pa", "sig_pa"), names_from = c("time", "time")) %>%
    # dplyr::select(c(name_pa, id_projet, dept_report, country_en, treatment_year, funding_year_list, fund_type, kfw, ffem, iucn_wolf, gov_type, att_pa_5, sig_pa_5, att_pa_10, sig_pa_10)) %>%
    dplyr::select(c(name_pa, id_projet, dept_report, country_en, treatment_year, funding_year_list, kfw, ffem, iucn_wolf, att_pa_5, sig_pa_5, att_pa_10, sig_pa_10)) %>%
    mutate(across(.cols = starts_with(c("att", "sig")),
                  .fns = \(x) case_when(is.na(x) == TRUE ~ "/", TRUE ~ as.character(format(x, digit = 1))))) %>%
    rename("Effect (5 y., %)" = "att_pa_5",
           "Signi. (5 y.)" = "sig_pa_5",
           "Effect (10 y., %)" = "att_pa_10",
           "Signi. (10 y.)" = "sig_pa_10") 
  # names(tbl_fc_att_pa) = c("Name", "Project ID", "Tech. div.", "Country", "Creation", "Funding year", "Type of funding", "KfW", "FFEM", "Protection", 
  #                           "Governance", "Effect (5 y., ha)", "Significance (5 y.)","Effect (10 y., ha)", "Significance (10 y.)")
  names(tbl_fc_att_pa) = c("Name", "Project ID", "Tech. div.", "Country", "Creation", "Funding year", "KfW", "FFEM", "Protection", 
                           "Effect (5 y., ha)", "Signi. (5 y.)","Effect (10 y., ha)", "Signi. (10 y.)")
  
  
  
  #Saving plots
  
  ##Saving plots
  tmp = paste(tempdir(), "fig", sep = "/")
  
  ggsave(paste(tmp, "fig_att_per.png", sep = "/"),
         plot = fig_att_per,
         device = "png",
         height = 6, width = 9)
  
  ggsave(paste(tmp, "fig_att_per_iucn.png", sep = "/"),
         plot = fig_att_per_iucn,
         device = "png",
         height = 6, width = 9)
  
  ggsave(paste(tmp, "fig_att_pa.png", sep = "/"),
         plot = fig_att_pa,
         device = "png",
         height = 6, width = 9)
  
  ggsave(paste(tmp, "fig_att_pa_iucn.png", sep = "/"),
         plot = fig_att_pa_iucn,
         device = "png",
         height = 6, width = 9)
  
  ggsave(paste(tmp, "fig_att_fl.png", sep = "/"),
         plot = fig_att_fl,
         device = "png",
         height = 6, width = 9)
  
  ggsave(paste(tmp, "fig_att_fl_iucn.png", sep = "/"),
         plot = fig_att_fl_iucn,
         device = "png",
         height = 6, width = 9)
  
  print(xtable(tbl_fc_att_pa, 
               type = "latex"),
        file = paste(tmp, "tbl_fc_att_pa.tex", sep = "/"))
  
  print(xtable(tbl_fc_att_per, type = "latex"),
        file = paste(tmp, "tbl_fc_att_per.tex", sep = "/"))
  
  files <- list.files(tmp, full.names = TRUE)
  ##Add each file in the bucket (same foler for every file in the temp)
  for(f in files) 
  {
    cat("Uploading file", paste0("'", f, "'"), "\n")
    aws.s3::put_object(file = f, 
                       bucket = paste("projet-afd-eva-ap", save_dir, sep = "/"),
                       region = "", show_progress = TRUE)
  }
  do.call(file.remove, list(list.files(tmp, full.names = TRUE)))
}


# Plotting the treatment effect of each protected area analyzed in the same graph. This function suits for all protected areas in general, and does not include any information on funding.
## INPUTS
### df_fc_att : a dataset with treatment effects for each protected area in the sample, expressed as avoided deforestation (hectare)
### df_fl_att : a dataset with treatment effects for each protected area in the sample, expressed as change in deforestation rate
### alpha : the threshold for confidence interval
### save_dir : the saving directory in the remote storage
## DATA SAVED
### Tables and figures : treatment effects computed for each protected area in the sample, expressed as avoided deforestaion (hectare and percentage of 2000 forest cover) and change in deforestation rate.
fn_plot_att_general = function(df_fc_att, df_fl_att, list_focus, alpha = alpha, save_dir)
{
  
  #list of PAs and two time periods
  list_ctry_plot = df_fc_att %>%
    dplyr::select(iso3, country_en, wdpaid, name_pa, iucn_cat, gov_type, own_type, treatment_year, status_wdpa) %>%
    unique() %>%
    group_by(iso3, country_en, wdpaid, name_pa) %>%
    summarize(time = c(5, 10),
              iucn_cat = iucn_cat,
              iucn_wolf = case_when(iucn_cat %in% c("I", "II", "III", "IV") ~ "Strict",
                                    iucn_cat %in% c("V", "VI") ~ "Non strict",
                                    grepl("not", iucn_cat, ignore.case = TRUE) ~ "Unknown"),
              treatment_year = treatment_year,
              gov_type = gov_type,
              own_type = own_type,
              status_wdpa = status_wdpa) %>%
    ungroup()
  
  #treatment effect for each wdpa (some have not on the two time periods)
  temp_fc = df_fc_att %>%
    dplyr::select(c(region, iso3, country_en, wdpaid, name_pa, time, year, att_per, cband_lower_per, cband_upper_per, att_pa, cband_lower_pa, cband_upper_pa)) %>%
    mutate(sig_pa = sign(cband_lower_pa) == sign(cband_upper_pa),
           sig_per = sign(cband_lower_per) == sign(cband_upper_per)) %>%
    filter(time %in% c(5, 10)) 
  temp_fl = df_fl_att %>%
    dplyr::select(c(region, iso3, country_en, wdpaid, name_pa, time, year, att, cband_lower, cband_upper)) %>%
    mutate(sig = sign(cband_lower) == sign(cband_upper)) %>%
    filter(time %in% c(5, 10)) 
  
  #Att for each WDPAID, for each period (NA if no value)
  ## For figures
  df_plot_fc_att = left_join(list_ctry_plot, temp_fc, by = c("iso3", "country_en", "wdpaid", "name_pa", "time")) %>%
    mutate(focus = case_when(wdpaid %in% list_focus ~ "focus",
                             !(wdpaid %in% list_focus) ~ "not focus")) %>%
    group_by(time, country_en) %>%
    arrange(country_en, focus) %>%
    mutate(country_en = paste0(country_en, " (", row_number(), ")"),
           n = row_number()) %>%
    ungroup()
  
  df_plot_fl_att = left_join(list_ctry_plot, temp_fl, by = c("iso3", "country_en", "wdpaid", "name_pa", "time"))%>%
    mutate(focus = case_when(wdpaid %in% list_focus ~ "focus",
                             !(wdpaid %in% list_focus) ~ "not focus")) %>%
    group_by(time, country_en) %>%
    arrange(country_en, focus) %>%
    mutate(country_en = paste0(country_en, " (", row_number(), ")"),
           n = row_number()) %>%
    ungroup()
  
  
  #Plots
  names = c(`5` = "5 years after treatment",
            `10` = "10 years after treatment",
            `Strict` = "Strict\nIUCN cat. I-IV",
            `Non strict` = "Non strict\nIUCN V-VI",
            `Unknown` = "Unknown",
            `focus` = "PA of interest",
            `not focus` = "Others")
  # df_colors = df_plot_fc_att %>% group_by(n) %>% slice(1)
  # colors = ifelse(df_colors$wdpaid %in% list_focus,"#3182BD","black")
  
  ## Att in share of pre-treatment forest cover
  fig_att_per = ggplot(df_plot_fc_att, 
                       aes(x = att_per, 
                           y = factor(name_pa, levels = unique(rev(sort(name_pa)))),
                           xmin = cband_lower_per, xmax = cband_upper_per)) %>%
    + geom_point(aes(color = sig_per)) %>%
    + geom_vline(xintercept = 0) %>%
    + geom_errorbarh(aes(color = sig_per)) %>% 
    + scale_color_discrete(name = paste0("Significance\n(", (1-alpha)*100, "% level)"),
                           na.translate = F) %>%
    # + scale_x_continuous(breaks=seq(min(df_plot_fc_att$att_per, na.rm = TRUE),max(df_plot_fc_att$att_per, na.rm = TRUE),by=1)) %>%
    + facet_grid(~time,scales="free", space="free",  labeller= as_labeller(names)) %>%
    + labs(title = "Deforestation avoided relative to pre-treatment forest cover",
           #caption = "Protected areas of interest are in blue, others are in black.",
           x = "%",
           y = "") %>%
    + theme_minimal() %>%
    + theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
      #axis.text.y = element_text(color = rev(colors)),
      axis.text=element_text(size=11, color = "black"),
      axis.title=element_text(size=14, color = "black", face = "plain"),
      
      plot.caption = element_text(hjust = 0),
      plot.title = element_text(size=16, color = "black", face = "plain", hjust = 0),
      plot.subtitle = element_text(size=12, color = "black", face = "plain", hjust = 0),
      
      strip.text = element_text(color = "black", size = 12),
      strip.clip = "off",
      panel.spacing = unit(2, "lines"),
      
      #legend.position = "bottom",
      legend.text=element_text(size=10),
      #legend.spacing.x = unit(1.0, 'cm'),
      #legend.spacing.y = unit(0.75, 'cm'),
      legend.key.size = unit(2, 'line'),
      
      panel.grid.major.x = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.x = element_line(color = 'grey80', linewidth = 0.2, linetype = 2),
      panel.grid.major.y = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.y = element_line(color = 'grey80', linewidth = 0.2, linetype = 2)
    )

  fig_att_per_focus_others = ggplot(df_plot_fc_att, 
                       aes(x = att_per, 
                           y = factor(name_pa, levels = unique(rev(sort(name_pa)))),
                           xmin = cband_lower_per, xmax = cband_upper_per)) %>%
    + geom_point(aes(color = sig_per)) %>%
    + geom_vline(xintercept = 0) %>%
    + geom_errorbarh(aes(color = sig_per)) %>% 
    + scale_color_discrete(name = paste0("Significance\n(", (1-alpha)*100, "% level)"),
                           na.translate = F) %>%
    # + scale_x_continuous(breaks=seq(min(df_plot_fc_att$att_per, na.rm = TRUE),max(df_plot_fc_att$att_per, na.rm = TRUE),by=1)) %>%
    + facet_grid(focus~time,scales="free", space="free",  labeller= as_labeller(names)) %>%
    + labs(title = "Deforestation avoided relative to pre-treatment forest cover",
           x = "%",
           y = "") %>%
    + theme_minimal() %>%
    + theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
      axis.text=element_text(size=11, color = "black"),
      axis.title=element_text(size=14, color = "black", face = "plain"),
      
      plot.caption = element_text(hjust = 0),
      plot.title = element_text(size=16, color = "black", face = "plain", hjust = 0),
      plot.subtitle = element_text(size=12, color = "black", face = "plain", hjust = 0),
      
      strip.text = element_text(color = "black", size = 12),
      strip.clip = "off",
      panel.spacing = unit(2, "lines"),
      
      #legend.position = "bottom",
      legend.text=element_text(size=10),
      #legend.spacing.x = unit(1.0, 'cm'),
      #legend.spacing.y = unit(0.75, 'cm'),
      legend.key.size = unit(2, 'line'),
      
      panel.grid.major.x = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.x = element_line(color = 'grey80', linewidth = 0.2, linetype = 2),
      panel.grid.major.y = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.y = element_line(color = 'grey80', linewidth = 0.2, linetype = 2)
    )
  
  fig_att_per_focus = ggplot(filter(df_plot_fc_att, focus == "focus"),
                                    aes(x = att_per, 
                                        y = factor(name_pa, levels = unique(rev(sort(name_pa)))),
                                        xmin = cband_lower_per, xmax = cband_upper_per)) %>%
    + geom_point(aes(color = sig_per)) %>%
    + geom_vline(xintercept = 0) %>%
    + geom_errorbarh(aes(color = sig_per)) %>% 
    + scale_color_discrete(name = paste0("Significance\n(", (1-alpha)*100, "% level)"),
                           na.translate = F) %>%
    # + scale_x_continuous(breaks=seq(min(df_plot_fc_att$att_per, na.rm = TRUE),max(df_plot_fc_att$att_per, na.rm = TRUE),by=1)) %>%
    + facet_grid(~time,scales="free", space="free",  labeller= as_labeller(names)) %>%
    + labs(title = "Deforestation avoided relative to pre-treatment forest cover",
           subtitle = "Protected areas of interest only",
           x = "%",
           y = "") %>%
    + theme_minimal() %>%
    + theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
      axis.text=element_text(size=11, color = "black"),
      axis.title=element_text(size=14, color = "black", face = "plain"),
      
      plot.caption = element_text(hjust = 0),
      plot.title = element_text(size=16, color = "black", face = "plain", hjust = 0),
      plot.subtitle = element_text(size=12, color = "black", face = "plain", hjust = 0),
      
      strip.text = element_text(color = "black", size = 12),
      strip.clip = "off",
      panel.spacing = unit(2, "lines"),
      
      #legend.position = "bottom",
      legend.text=element_text(size=10),
      #legend.spacing.x = unit(1.0, 'cm'),
      #legend.spacing.y = unit(0.75, 'cm'),
      legend.key.size = unit(2, 'line'),
      
      panel.grid.major.x = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.x = element_line(color = 'grey80', linewidth = 0.2, linetype = 2),
      panel.grid.major.y = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.y = element_line(color = 'grey80', linewidth = 0.2, linetype = 2)
    )
  
  
  fig_att_per_iucn = ggplot(df_plot_fc_att, 
                            aes(x = att_per, 
                                y = factor(name_pa, levels = unique(rev(sort(name_pa)))),
                                xmin = cband_lower_per, xmax = cband_upper_per)) %>%
    + geom_point(aes(color = sig_per)) %>%
    + geom_vline(xintercept = 0) %>%
    + geom_errorbarh(aes(color = sig_per)) %>% 
    + scale_color_discrete(name = paste0("Significance\n(", (1-alpha)*100, "% level)"),
                           na.translate = F) %>%
    # + scale_x_continuous(breaks=seq(min(df_plot_fc_att$att_per, na.rm = TRUE),max(df_plot_fc_att$att_per, na.rm = TRUE),by=1)) %>%
    + facet_grid(iucn_wolf~time,scales="free", space="free",  labeller= as_labeller(names)) %>%
    + labs(title = "Deforestation avoided relative to 2000 forest cover",
           x = "%",
           y = "") %>%
    + theme_minimal() %>%
    + theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
      #axis.text.y = element_text(color = rev(colors)),
      axis.text=element_text(size=11, color = "black"),
      axis.title=element_text(size=14, color = "black", face = "plain"),
      
      plot.caption = element_text(hjust = 0),
      plot.title = element_text(size=16, color = "black", face = "plain", hjust = 0),
      plot.subtitle = element_text(size=12, color = "black", face = "plain", hjust = 0),
      
      strip.text = element_text(color = "black", size = 12),
      strip.clip = "off",
      panel.spacing = unit(2, "lines"),
      
      #legend.position = "bottom",
      legend.text=element_text(size=10),
      #legend.spacing.x = unit(1.0, 'cm'),
      #legend.spacing.y = unit(0.75, 'cm'),
      legend.key.size = unit(2, 'line'),
      
      panel.grid.major.x = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.x = element_line(color = 'grey80', linewidth = 0.2, linetype = 2),
      panel.grid.major.y = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.y = element_line(color = 'grey80', linewidth = 0.2, linetype = 2)
    )
  
  
  ##treatment effect : total deforestation avoided
  fig_att_pa = ggplot(df_plot_fc_att, 
                      aes(x = att_pa, 
                          y = factor(name_pa, levels = unique(rev(sort(name_pa)))),
                          xmin = cband_lower_pa, xmax = cband_upper_pa)) %>%
    + geom_point(aes(color = sig_pa)) %>%
    + geom_vline(xintercept = 0) %>%
    + geom_errorbarh(aes(color = sig_pa)) %>% 
    + scale_color_discrete(name = paste0("Significance\n(", (1-alpha)*100, "% level)"),
                           na.translate = F) %>%
    + facet_grid(~time,scales="free", space="free",  labeller= as_labeller(names)) %>%
    + labs(title = "Total deforestation avoided",
           x = "ha",
           y = "") %>%
    + theme_minimal() %>%
    + theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
      axis.text=element_text(size=11, color = "black"),
      axis.title=element_text(size=14, color = "black", face = "plain"),
      
      plot.caption = element_text(hjust = 0),
      plot.title = element_text(size=16, color = "black", face = "plain", hjust = 0),
      plot.subtitle = element_text(size=12, color = "black", face = "plain", hjust = 0),
      
      strip.text = element_text(color = "black", size = 12),
      strip.clip = "off",
      panel.spacing = unit(2, "lines"),
      
      #legend.position = "bottom",
      legend.text=element_text(size=10),
      #legend.spacing.x = unit(1.0, 'cm'),
      #legend.spacing.y = unit(0.75, 'cm'),
      legend.key.size = unit(2, 'line'),
      
      panel.grid.major.x = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.x = element_line(color = 'grey80', linewidth = 0.2, linetype = 2),
      panel.grid.major.y = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.y = element_line(color = 'grey80', linewidth = 0.2, linetype = 2)
    )
  
  fig_att_pa_focus_others = ggplot(df_plot_fc_att, 
                      aes(x = att_pa, 
                          y = factor(name_pa, levels = unique(rev(sort(name_pa)))),
                          xmin = cband_lower_pa, xmax = cband_upper_pa)) %>%
    + geom_point(aes(color = sig_pa)) %>%
    + geom_vline(xintercept = 0) %>%
    + geom_errorbarh(aes(color = sig_pa)) %>% 
    + scale_color_discrete(name = paste0("Significance\n(", (1-alpha)*100, "% level)"),
                           na.translate = F) %>%
    + facet_grid(focus~time,scales="free", space="free",  labeller= as_labeller(names)) %>%
    + labs(title = "Total deforestation avoided",
           x = "ha",
           y = "") %>%
    + theme_minimal() %>%
    + theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
      axis.text=element_text(size=11, color = "black"),
      axis.title=element_text(size=14, color = "black", face = "plain"),
      
      plot.caption = element_text(hjust = 0),
      plot.title = element_text(size=16, color = "black", face = "plain", hjust = 0),
      plot.subtitle = element_text(size=12, color = "black", face = "plain", hjust = 0),
      
      strip.text = element_text(color = "black", size = 12),
      strip.clip = "off",
      panel.spacing = unit(2, "lines"),
      
      #legend.position = "bottom",
      legend.text=element_text(size=10),
      #legend.spacing.x = unit(1.0, 'cm'),
      #legend.spacing.y = unit(0.75, 'cm'),
      legend.key.size = unit(2, 'line'),
      
      panel.grid.major.x = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.x = element_line(color = 'grey80', linewidth = 0.2, linetype = 2),
      panel.grid.major.y = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.y = element_line(color = 'grey80', linewidth = 0.2, linetype = 2)
    )
  
  fig_att_pa_focus = ggplot(filter(df_plot_fc_att, focus == "focus"), 
                                   aes(x = att_pa, 
                                       y = factor(name_pa, levels = unique(rev(sort(name_pa)))),
                                       xmin = cband_lower_pa, xmax = cband_upper_pa)) %>%
    + geom_point(aes(color = sig_pa)) %>%
    + geom_vline(xintercept = 0) %>%
    + geom_errorbarh(aes(color = sig_pa)) %>% 
    + scale_color_discrete(name = paste0("Significance\n(", (1-alpha)*100, "% level)"),
                           na.translate = F) %>%
    + facet_grid(focus~time,scales="free", space="free",  labeller= as_labeller(names)) %>%
    + labs(title = "Total deforestation avoided",
           subtitle = "Protected areas of interest only",
           x = "ha",
           y = "") %>%
    + theme_minimal() %>%
    + theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
      axis.text=element_text(size=11, color = "black"),
      axis.title=element_text(size=14, color = "black", face = "plain"),
      
      plot.caption = element_text(hjust = 0),
      plot.title = element_text(size=16, color = "black", face = "plain", hjust = 0),
      plot.subtitle = element_text(size=12, color = "black", face = "plain", hjust = 0),
      
      strip.text = element_text(color = "black", size = 12),
      strip.clip = "off",
      panel.spacing = unit(2, "lines"),
      
      #legend.position = "bottom",
      legend.text=element_text(size=10),
      #legend.spacing.x = unit(1.0, 'cm'),
      #legend.spacing.y = unit(0.75, 'cm'),
      legend.key.size = unit(2, 'line'),
      
      panel.grid.major.x = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.x = element_line(color = 'grey80', linewidth = 0.2, linetype = 2),
      panel.grid.major.y = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.y = element_line(color = 'grey80', linewidth = 0.2, linetype = 2)
    )
  
  fig_att_pa_iucn = ggplot(df_plot_fc_att, 
                           aes(x = att_pa, 
                               y = factor(name_pa, levels = unique(rev(sort(name_pa)))),
                               xmin = cband_lower_pa, xmax = cband_upper_pa)) %>%
    + geom_point(aes(color = sig_pa)) %>%
    + geom_vline(xintercept = 0) %>%
    + geom_errorbarh(aes(color = sig_pa)) %>% 
    + scale_color_discrete(name = paste0("Significance\n(", (1-alpha)*100, "% level)"),
                           na.translate = F) %>%
    + facet_grid(iucn_wolf~time,scales="free", space="free",  labeller= as_labeller(names)) %>%
    + labs(title = "Total deforestation avoided",
           x = "ha",
           y = "") %>%
    + theme_minimal() %>%
    + theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
      axis.text=element_text(size=11, color = "black"),
      axis.title=element_text(size=14, color = "black", face = "plain"),
      
      plot.caption = element_text(hjust = 0),
      plot.title = element_text(size=16, color = "black", face = "plain", hjust = 0),
      plot.subtitle = element_text(size=12, color = "black", face = "plain", hjust = 0),
      
      strip.text = element_text(color = "black", size = 12),
      strip.clip = "off",
      panel.spacing = unit(2, "lines"),
      
      #legend.position = "bottom",
      legend.text=element_text(size=10),
      #legend.spacing.x = unit(1.0, 'cm'),
      #legend.spacing.y = unit(0.75, 'cm'),
      legend.key.size = unit(2, 'line'),
      
      panel.grid.major.x = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.x = element_line(color = 'grey80', linewidth = 0.2, linetype = 2),
      panel.grid.major.y = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.y = element_line(color = 'grey80', linewidth = 0.2, linetype = 2)
    )
  
  ##treatment effect : avoided deforestation in percentage points
  fig_att_fl = ggplot(df_plot_fl_att, 
                      aes(x = att, 
                          y = factor(name_pa, levels = unique(rev(sort(name_pa)))),
                          xmin = cband_lower, xmax = cband_upper)) %>%
    + geom_point(aes(color = sig)) %>%
    + geom_vline(xintercept = 0) %>%
    + geom_errorbarh(aes(color = sig)) %>% 
    + scale_color_discrete(name = paste0("Significance\n(", (1-alpha)*100, "% level)"),
                           na.translate = F) %>%
    + facet_grid(~time,scales="free", space="free",  labeller= as_labeller(names)) %>%
    + labs(title = "Reduction of deforestation due to the conservation",
           x = "p.p.",
           y = "") %>%
    + theme_minimal() %>%
    + theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
      axis.text=element_text(size=11, color = "black"),
      axis.title=element_text(size=14, color = "black", face = "plain"),
      
      plot.caption = element_text(hjust = 0),
      plot.title = element_text(size=16, color = "black", face = "plain", hjust = 0),
      plot.subtitle = element_text(size=12, color = "black", face = "plain", hjust = 0),
      
      strip.text = element_text(color = "black", size = 12),
      strip.clip = "off",
      panel.spacing = unit(2, "lines"),
      
      #legend.position = "bottom",
      legend.text=element_text(size=10),
      #legend.spacing.x = unit(1.0, 'cm'),
      #legend.spacing.y = unit(0.75, 'cm'),
      legend.key.size = unit(2, 'line'),
      
      panel.grid.major.x = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.x = element_line(color = 'grey80', linewidth = 0.2, linetype = 2),
      panel.grid.major.y = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.y = element_line(color = 'grey80', linewidth = 0.2, linetype = 2)
    )
  
  fig_att_fl_focus_others = ggplot(df_plot_fl_att, 
                      aes(x = att, 
                          y = factor(name_pa, levels = unique(rev(sort(name_pa)))),
                          xmin = cband_lower, xmax = cband_upper)) %>%
    + geom_point(aes(color = sig)) %>%
    + geom_vline(xintercept = 0) %>%
    + geom_errorbarh(aes(color = sig)) %>% 
    + scale_color_discrete(name = paste0("Significance\n(", (1-alpha)*100, "% level)"),
                           na.translate = F) %>%
    + facet_grid(focus~time,scales="free", space="free",  labeller= as_labeller(names)) %>%
    + labs(title = "Reduction of deforestation due to the conservation",
           x = "p.p.",
           y = "") %>%
    + theme_minimal() %>%
    + theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
      axis.text=element_text(size=11, color = "black"),
      axis.title=element_text(size=14, color = "black", face = "plain"),
      
      plot.caption = element_text(hjust = 0),
      plot.title = element_text(size=16, color = "black", face = "plain", hjust = 0),
      plot.subtitle = element_text(size=12, color = "black", face = "plain", hjust = 0),
      
      strip.text = element_text(color = "black", size = 12),
      strip.clip = "off",
      panel.spacing = unit(2, "lines"),
      
      #legend.position = "bottom",
      legend.text=element_text(size=10),
      #legend.spacing.x = unit(1.0, 'cm'),
      #legend.spacing.y = unit(0.75, 'cm'),
      legend.key.size = unit(2, 'line'),
      
      panel.grid.major.x = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.x = element_line(color = 'grey80', linewidth = 0.2, linetype = 2),
      panel.grid.major.y = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.y = element_line(color = 'grey80', linewidth = 0.2, linetype = 2)
    )
  
  fig_att_fl_focus = ggplot(filter(df_plot_fl_att, focus == "focus"),
                                   aes(x = att, 
                                       y = factor(name_pa, levels = unique(rev(sort(name_pa)))),
                                       xmin = cband_lower, xmax = cband_upper)) %>%
    + geom_point(aes(color = sig)) %>%
    + geom_vline(xintercept = 0) %>%
    + geom_errorbarh(aes(color = sig)) %>% 
    + scale_color_discrete(name = paste0("Significance\n(", (1-alpha)*100, "% level)"),
                           na.translate = F) %>%
    + facet_grid(focus~time,scales="free", space="free",  labeller= as_labeller(names)) %>%
    + labs(title = "Reduction of deforestation due to the conservation",
           subtitle = "Protected areas of interest only",
           x = "p.p.",
           y = "") %>%
    + theme_minimal() %>%
    + theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
      axis.text=element_text(size=11, color = "black"),
      axis.title=element_text(size=14, color = "black", face = "plain"),
      
      plot.caption = element_text(hjust = 0),
      plot.title = element_text(size=16, color = "black", face = "plain", hjust = 0),
      plot.subtitle = element_text(size=12, color = "black", face = "plain", hjust = 0),
      
      strip.text = element_text(color = "black", size = 12),
      strip.clip = "off",
      panel.spacing = unit(2, "lines"),
      
      #legend.position = "bottom",
      legend.text=element_text(size=10),
      #legend.spacing.x = unit(1.0, 'cm'),
      #legend.spacing.y = unit(0.75, 'cm'),
      legend.key.size = unit(2, 'line'),
      
      panel.grid.major.x = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.x = element_line(color = 'grey80', linewidth = 0.2, linetype = 2),
      panel.grid.major.y = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.y = element_line(color = 'grey80', linewidth = 0.2, linetype = 2)
    )
  
  fig_att_fl_iucn = ggplot(df_plot_fl_att, 
                           aes(x = att, 
                               y = factor(name_pa, levels = unique(rev(sort(name_pa)))),
                               xmin = cband_lower, xmax = cband_upper)) %>%
    + geom_point(aes(color = sig)) %>%
    + geom_vline(xintercept = 0) %>%
    + geom_errorbarh(aes(color = sig)) %>% 
    + scale_color_discrete(name = paste0("Significance\n(", (1-alpha)*100, "% level)"),
                           na.translate = F) %>%
    + facet_grid(iucn_wolf~time, scales="free", space="free",  labeller= as_labeller(names)) %>%
    + labs(title = "Reduction of deforestation due to the conservation",
           x = "p.p.",
           y = "") %>%
    + theme_minimal() %>%
    + theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
      axis.text=element_text(size=11, color = "black"),
      axis.title=element_text(size=14, color = "black", face = "plain"),
      
      plot.caption = element_text(hjust = 0),
      plot.title = element_text(size=16, color = "black", face = "plain", hjust = 0),
      plot.subtitle = element_text(size=12, color = "black", face = "plain", hjust = 0),
      
      strip.text = element_text(color = "black", size = 12),
      strip.clip = "off",
      panel.spacing = unit(2, "lines"),
      
      #legend.position = "bottom",
      legend.text=element_text(size=10),
      #legend.spacing.x = unit(1.0, 'cm'),
      #legend.spacing.y = unit(0.75, 'cm'),
      legend.key.size = unit(2, 'line'),
      
      panel.grid.major.x = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.x = element_line(color = 'grey80', linewidth = 0.2, linetype = 2),
      panel.grid.major.y = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.y = element_line(color = 'grey80', linewidth = 0.2, linetype = 2)
    )
  
  
  #Tables 
  ## treatment effect : percentage of deforestation avoided
  tbl_fc_att_per = df_plot_fc_att  %>%
    mutate(focus = case_when(wdpaid %in% list_focus ~ "Yes",
                             !(wdpaid %in% list_focus) ~ "No"),
           sig_per = case_when(sign(cband_lower_per) == sign(cband_upper_per) ~ "Yes",
                               sign(cband_lower_per) != sign(cband_upper_per) ~ "No"),
           iucn_wolf = case_when(iucn_cat %in% c("I", "II", "III", "IV") ~ "Strict",
                                 iucn_cat %in% c("V", "VI") ~ "Non strict",
                                 grepl("not", iucn_cat, ignore.case = TRUE) ~ "Unknown"),
           name_pa = case_when(nchar(name_pa) <= 25 ~ stri_trans_general(name_pa, id = "Latin-ASCII"),
                               nchar(name_pa) > 25 ~ stri_trans_general(paste0(substr(name_pa, 1, 25), "..."),  id = "Latin-ASCII"))
    ) %>%
    dplyr::select(c(name_pa, focus, treatment_year, iucn_wolf, gov_type, time, att_per, sig_per)) %>%
    pivot_wider(values_from = c("att_per", "sig_per"), names_from = c("time", "time")) %>%
    dplyr::select(c(name_pa, focus, treatment_year, iucn_wolf, att_per_5, sig_per_5, att_per_10, sig_per_10)) %>%
    #dplyr::select(c(name_pa, country_en, treatment_year, iucn_wolf, gov_type, att_per_5, sig_per_5, att_per_10, sig_per_10)) %>%
    mutate(across(.cols = starts_with(c("att")),
                  .fns = \(x) case_when(is.na(x) == TRUE ~ "/", TRUE ~ as.character(format(round(x, 2), scientific = FALSE))))) %>%
    mutate(across(.cols = starts_with(c("sig")),
                  .fns = \(x) case_when(is.na(x) == TRUE ~ "/", TRUE ~ x))) %>%
    rename("Effect (5 y., %)" = "att_per_5",
           "Signi. (5 y.)" = "sig_per_5",
           "Effect (10 y., %)" = "att_per_10",
           "Signi. (10 y.)" = "sig_per_10") %>%
    arrange(focus, name_pa)
  # names(tbl_fc_att_per) = c("Name", "PA of interest", "Creation",  "Protection", 
  #                           "Governance", "Effect (5 y., %)", "Significance (5 y.)","Effect (10 y., %)", "Significance (10 y.)")
  names(tbl_fc_att_per) = c("Name", "PA of interest", "Creation", "Protection", 
                            "Effect (5 y., %)", "Signi. (5 y.)","Effect (10 y., %)", "Signi. (10 y.)")
  
  # treatment effect : total deforestation avoided 
  tbl_fc_att_pa = df_plot_fc_att %>%
    mutate(focus = case_when(wdpaid %in% list_focus ~ "Yes",
                             !(wdpaid %in% list_focus) ~ "No"),
           sig_pa = case_when(sign(cband_lower_pa) == sign(cband_upper_pa) ~ "Yes",
                              sign(cband_lower_pa) != sign(cband_upper_pa) ~ "No"),
           iucn_wolf = case_when(iucn_cat %in% c("I", "II", "III", "IV") ~ "Strict",
                                 iucn_cat %in% c("V", "VI") ~ "Non strict",
                                 grepl("not", iucn_cat, ignore.case = TRUE) ~ "Unknown"),
           name_pa = case_when(nchar(name_pa) <= 25 ~ stri_trans_general(name_pa, id = "Latin-ASCII"),
                               nchar(name_pa) > 25 ~ stri_trans_general(paste0(substr(name_pa, 1, 25), "..."),  id = "Latin-ASCII"))
    ) %>%
    dplyr::select(c(name_pa, focus, country_en, treatment_year, iucn_wolf, gov_type, time, att_pa, sig_pa)) %>%
    pivot_wider(values_from = c("att_pa", "sig_pa"), names_from = c("time", "time")) %>%
    # dplyr::select(c(name_pa, country_en, treatment_year, iucn_wolf, gov_type, att_pa_5, sig_pa_5, att_pa_10, sig_pa_10)) %>%
    dplyr::select(c(name_pa, focus, treatment_year, iucn_wolf, att_pa_5, sig_pa_5, att_pa_10, sig_pa_10)) %>%
    mutate(across(.cols = starts_with(c("att")),
                  .fns = \(x) case_when(is.na(x) == TRUE ~ "/", TRUE ~ as.character(format(round(x, 2), scientific = FALSE))))) %>%
    mutate(across(.cols = starts_with(c("sig")),
                  .fns = \(x) case_when(is.na(x) == TRUE ~ "/", TRUE ~ x))) %>%
    rename("Effect (5 y., %)" = "att_pa_5",
           "Signi. (5 y.)" = "sig_pa_5",
           "Effect (10 y., %)" = "att_pa_10",
           "Signi. (10 y.)" = "sig_pa_10") %>%
  arrange(focus, name_pa)
  # names(tbl_fc_att_pa) = c("Name", "PA of interest", "Creation",  "Protection", 
  #                           "Governance", "Effect (5 y., ha)", "Significance (5 y.)","Effect (10 y., ha)", "Significance (10 y.)")
  names(tbl_fc_att_pa) = c("Name", "PA of interest", "Creation", "Protection", 
                            "Effect (5 y., ha)", "Signi. (5 y.)","Effect (10 y., ha)", "Signi. (10 y.)")
  
  # treatment effect : avoided deforestation, in terms of difference in cumultaed deforestation rate 
  tbl_fl_att = df_plot_fl_att %>%
    mutate(focus = case_when(wdpaid %in% list_focus ~ "Yes",
                             !(wdpaid %in% list_focus) ~ "No"),
           sig = case_when(sign(cband_lower) == sign(cband_upper) ~ "Yes",
                              sign(cband_lower) != sign(cband_upper) ~ "No"),
           iucn_wolf = case_when(iucn_cat %in% c("I", "II", "III", "IV") ~ "Strict",
                                 iucn_cat %in% c("V", "VI") ~ "Non strict",
                                 grepl("not", iucn_cat, ignore.case = TRUE) ~ "Unknown"),
           name_pa = case_when(nchar(name_pa) <= 25 ~ stri_trans_general(name_pa, id = "Latin-ASCII"),
                               nchar(name_pa) > 25 ~ stri_trans_general(paste0(substr(name_pa, 1, 25), "..."),  id = "Latin-ASCII"))
    ) %>%
    dplyr::select(c(name_pa, focus, country_en, treatment_year, iucn_wolf, gov_type, time, att, sig)) %>%
    pivot_wider(values_from = c("att", "sig"), names_from = c("time", "time")) %>%
    # dplyr::select(c(name_pa, country_en, treatment_year, iucn_wolf, gov_type, att_5, sig_5, att_10, sig_10)) %>%
    dplyr::select(c(name_pa, focus, treatment_year, iucn_wolf, att_5, sig_5, att_10, sig_10)) %>%
    mutate(across(.cols = starts_with(c("att")),
                  .fns = \(x) case_when(is.na(x) == TRUE ~ "/", TRUE ~ as.character(format(round(x, 2), scientific = FALSE))))) %>%
    mutate(across(.cols = starts_with(c("sig")),
                  .fns = \(x) case_when(is.na(x) == TRUE ~ "/", TRUE ~ x))) %>%
    rename("Effect (5 y., %)" = "att_5",
           "Signi. (5 y.)" = "sig_5",
           "Effect (10 y., %)" = "att_10",
           "Signi. (10 y.)" = "sig_10") %>%
    arrange(focus, name_pa)
  # names(tbl_fl_att) = c("Name", "PA of interest", "Creation",  "Protection", 
  #                           "Governance", "Effect (5 y., pp)", "Significance (5 y.)","Effect (10 y., pp)", "Significance (10 y.)")
  names(tbl_fl_att) = c("Name", "PA of interest", "Creation", "Protection", 
                           "Effect (5 y., pp)", "Signi. (5 y.)","Effect (10 y., pp)", "Signi. (10 y.)")
  
  #Saving plots
  
  ##Saving plots
  tmp = paste(tempdir(), "fig", sep = "/")
  
  ggsave(paste(tmp, "fig_att_per.png", sep = "/"),
         plot = fig_att_per,
         device = "png",
         height = 8, width = 12)

  ggsave(paste(tmp, "fig_att_per_focus.png", sep = "/"),
         plot = fig_att_per_focus,
         device = "png",
         height = 6, width =9)
  
  ggsave(paste(tmp, "fig_att_per_focus_others.png", sep = "/"),
         plot = fig_att_per_focus_others,
         device = "png",
         height = 8, width = 12)
  
  ggsave(paste(tmp, "fig_att_per_iucn.png", sep = "/"),
         plot = fig_att_per_iucn,
         device = "png",
         height = 8, width = 12)
  
  ggsave(paste(tmp, "fig_att_pa.png", sep = "/"),
         plot = fig_att_pa,
         device = "png",
         height = 8, width = 12)
  
  ggsave(paste(tmp, "fig_att_pa_focus.png", sep = "/"),
         plot = fig_att_pa_focus,
         device = "png",
         height = 6, width = 9)
  
  ggsave(paste(tmp, "fig_att_pa_focus_others.png", sep = "/"),
         plot = fig_att_pa_focus_others,
         device = "png",
         height = 8, width = 12)
  
  ggsave(paste(tmp, "fig_att_pa_iucn.png", sep = "/"),
         plot = fig_att_pa_iucn,
         device = "png",
         height = 8, width = 12)
  
  ggsave(paste(tmp, "fig_att_fl.png", sep = "/"),
         plot = fig_att_fl,
         device = "png",
         height = 8, width = 12)
  
  ggsave(paste(tmp, "fig_att_fl_focus.png", sep = "/"),
         plot = fig_att_fl_focus,
         device = "png",
         height = 6, width = 9)
  
  ggsave(paste(tmp, "fig_att_fl_focus_others.png", sep = "/"),
         plot = fig_att_fl_focus_others,
         device = "png",
         height = 8, width = 12)
  
  ggsave(paste(tmp, "fig_att_fl_iucn.png", sep = "/"),
         plot = fig_att_fl_iucn,
         device = "png",
         height = 8, width = 12)
  
  print(xtable(tbl_fc_att_pa, type = "latex", auto = T),
        file = paste(tmp, "tbl_fc_att_pa.tex", sep = "/"))
  
  print(xtable(tbl_fc_att_per, type = "latex", auto = T),
        file = paste(tmp, "tbl_fc_att_per.tex", sep = "/"))
  
  print(xtable(tbl_fl_att, type = "latex", auto = T),
        file = paste(tmp, "tbl_fl_att.tex", sep = "/"))
  
  files <- list.files(tmp, full.names = TRUE)
  ##Add each file in the bucket (same foler for every file in the temp)
  for(f in files) 
  {
    cat("Uploading file", paste0("'", f, "'"), "\n")
    aws.s3::put_object(file = f, 
                       bucket = paste("projet-afd-eva-ap", save_dir, sep = "/"),
                       region = "", show_progress = TRUE)
  }
  do.call(file.remove, list(list.files(tmp, full.names = TRUE)))
}


# Plotting the treatment effects aggregated at country level
## INPUTS
### df_fc_att : a dataset with treatment effects for each protected area in the sample, expressed as avoided deforestation (hectare)
### df_fl_att : a dataset with treatment effects for each protected area in the sample, expressed as change in deforestation rate
### alpha : the threshold for confidence interval
### save_dir : the saving directory in the remote storage
## DATA SAVED
### Tables and figures : treatment effects computed aggregated at country level, expressed as avoided deforestaion (hectare and percentage of 2000 forest cover) and change in deforestation rate.

fn_plot_att_agg = function(df_fc_att, df_fl_att, is_focus, alpha = alpha, save_dir)
{
  
  if(is_focus == T)
  {
    df_fc_att = df_fc_att %>% filter(focus == T)
    df_fl_att = df_fl_att %>% filter(focus == T)
  } 
  if(is_focus == F)
  {
    df_fc_att = df_fc_att %>% filter(focus == F)
    df_fl_att = df_fl_att %>% filter(focus == F)
  } 
  
  #list of PAs and two time periods
  list_ctry_plot = df_fc_att %>%
    dplyr::select(iso3, country_en, wdpaid) %>%
    unique() %>%
    group_by(iso3, country_en) %>%
    summarize(time = c(5, 10),
              n_pa = n()) %>%
    ungroup()
  
  #Treatment effects are aggregated by country
  #For avoided deforestation in % of pre-treatment forest cover, treatment effects are averaged and so are CI. CI being NA is less a problem here as we use a mean, not a sum
  
  df_fc_att_agg = df_fc_att %>%
    group_by(country_en, iso3, time) %>%
    summarize(n_obs = n(),
              att_per = mean(att_per, na.rm = TRUE),
              cband_lower_per = mean(cband_lower_per, na.rm = TRUE),
              cband_upper_per = mean(cband_upper_per, na.rm = TRUE),
              att_pa = mean(att_pa, na.rm = TRUE),
              cband_lower_pa = mean(cband_lower_pa, na.rm = TRUE),
              cband_upper_pa = mean(cband_upper_pa, na.rm = TRUE)) %>%
    ungroup()
  
  df_fl_att_agg = df_fl_att %>%
    group_by(country_en, iso3, time) %>%
    summarize(n_obs = n(),
              att = mean(att, na.rm = TRUE),
              cband_lower = mean(cband_lower, na.rm = TRUE),
              cband_upper = mean(cband_upper, na.rm = TRUE)) %>%
    ungroup()
  
  #treatment effect for each country (some have not on the two time periods)
  temp_fc = df_fc_att_agg %>%
    dplyr::select(c(iso3, country_en, time, att_per, cband_lower_per, cband_upper_per, att_pa, cband_lower_pa, cband_upper_pa)) %>%
    mutate(sig_pa = sign(cband_lower_pa) == sign(cband_upper_pa),
           sig_per = sign(cband_lower_per) == sign(cband_upper_per)) %>%
    filter(time %in% c(5, 10)) 
  temp_fl = df_fl_att_agg %>%
    dplyr::select(c(iso3, country_en, time, att, cband_lower, cband_upper)) %>%
    mutate(sig = sign(cband_lower) == sign(cband_upper)) %>%
    filter(time %in% c(5, 10)) 
  
  
  #Att for each country, for each period (NA if no value)
  ## For figures
  df_plot_fc_att_agg = left_join(list_ctry_plot, temp_fc, by = c("iso3", "country_en", "time")) %>%
    group_by(time, country_en) %>%
    arrange(country_en) %>%
    mutate(n = row_number(),
           country_en_n = paste0(country_en, " (", n_pa, ")")) %>%
    ungroup()
  
  df_plot_fl_att_agg = left_join(list_ctry_plot, temp_fl, by = c("iso3", "country_en", "time"))%>%
    group_by(time, country_en) %>%
    arrange(country_en) %>%
    mutate(n = row_number(),
           country_en_n = paste0(country_en, " (", n_pa, ")")) %>%
    ungroup()
  
  
  #Plots
  names = c(`5` = "5 years after treatment",
            `10` = "10 years after treatment")
  # df_colors = df_plot_fc_att %>% group_by(n) %>% slice(1)
  # colors = ifelse(df_colors$wdpaid %in% list_focus,"#3182BD","black")
  
  ## Att in share of pre-treatment forest cover
  fig_att_per_agg = ggplot(df_plot_fc_att_agg, 
                       aes(x = att_per, 
                           y = factor(country_en_n, levels = unique(rev(sort(country_en_n)))),
                           xmin = cband_lower_per, xmax = cband_upper_per)) %>%
    + geom_point(aes(color = sig_per)) %>%
    + geom_vline(xintercept = 0) %>%
    + geom_errorbarh(aes(color = sig_per)) %>% 
    + scale_color_discrete(name = paste0("Significance\n(", (1-alpha)*100, "% level)"),
                           na.translate = F) %>%
    # + scale_x_continuous(breaks=seq(min(df_plot_fc_att$att_per, na.rm = TRUE),max(df_plot_fc_att$att_per, na.rm = TRUE),by=1)) %>%
    + facet_grid(~time,scales="free", space="free",  labeller= as_labeller(names)) %>%
    + labs(title = "Deforestation avoided relative to pre-treatment forest cover",
           subtitle = ifelse(is_focus == T,
                             yes = "Sample : protected areas of interest",
                             no = "Sample : other protected areas"),
           caption = "95% confidence intervals are represented | The number of PA in each country is indicated in parentheses",
           x = "%",
           y = "") %>%
    + theme_minimal() %>%
    + theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
      #axis.text.y = element_text(color = rev(colors)),
      axis.text=element_text(size=11, color = "black"),
      axis.title=element_text(size=14, color = "black", face = "plain"),
      
      plot.caption = element_text(hjust = 0),
      plot.title = element_text(size=16, color = "black", face = "plain", hjust = 0),
      plot.subtitle = element_text(size=12, color = "black", face = "plain", hjust = 0),
      
      strip.text = element_text(color = "black", size = 12),
      strip.clip = "off",
      panel.spacing = unit(2, "lines"),
      
      #legend.position = "bottom",
      legend.text=element_text(size=10),
      #legend.spacing.x = unit(1.0, 'cm'),
      #legend.spacing.y = unit(0.75, 'cm'),
      legend.key.size = unit(2, 'line'),
      
      panel.grid.major.x = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.x = element_line(color = 'grey80', linewidth = 0.2, linetype = 2),
      panel.grid.major.y = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.y = element_line(color = 'grey80', linewidth = 0.2, linetype = 2)
    )
  
  ##treatment effect : total deforestation avoided
  fig_att_pa_agg = ggplot(df_plot_fc_att_agg, 
                      aes(x = att_pa, 
                          y = factor(country_en_n, levels = unique(rev(sort(country_en_n)))),
                          xmin = cband_lower_pa, xmax = cband_upper_pa)) %>%
    + geom_point(aes(color = sig_pa)) %>%
    + geom_vline(xintercept = 0) %>%
    + geom_errorbarh(aes(color = sig_pa)) %>% 
    + scale_color_discrete(name = paste0("Significance\n(", (1-alpha)*100, "% level)"),
                           na.translate = F) %>%
    + facet_grid(~time,scales="free", space="free",  labeller= as_labeller(names)) %>%
    + labs(title = "Total deforestation avoided",
           subtitle = ifelse(is_focus == T,
                             yes = "Sample : protected areas of interest",
                             no = "Sample : other protected areas"),
           caption = "95% confidence intervals are represented | The number of PA in each country is indicated in parentheses",
           x = "ha",
           y = "") %>%
    + theme_minimal() %>%
    + theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
      axis.text=element_text(size=11, color = "black"),
      axis.title=element_text(size=14, color = "black", face = "plain"),
      
      plot.caption = element_text(hjust = 0),
      plot.title = element_text(size=16, color = "black", face = "plain", hjust = 0),
      plot.subtitle = element_text(size=12, color = "black", face = "plain", hjust = 0),
      
      strip.text = element_text(color = "black", size = 12),
      strip.clip = "off",
      panel.spacing = unit(2, "lines"),
      
      #legend.position = "bottom",
      legend.text=element_text(size=10),
      #legend.spacing.x = unit(1.0, 'cm'),
      #legend.spacing.y = unit(0.75, 'cm'),
      legend.key.size = unit(2, 'line'),
      
      panel.grid.major.x = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.x = element_line(color = 'grey80', linewidth = 0.2, linetype = 2),
      panel.grid.major.y = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.y = element_line(color = 'grey80', linewidth = 0.2, linetype = 2)
    )
  
  ##treatment effect : avoided deforestation in percentage points
  fig_att_fl_agg = ggplot(df_plot_fl_att_agg, 
                      aes(x = att, 
                          y = factor(country_en_n, levels = unique(rev(sort(country_en_n)))),
                          xmin = cband_lower, xmax = cband_upper)) %>%
    + geom_point(aes(color = sig)) %>%
    + geom_vline(xintercept = 0) %>%
    + geom_errorbarh(aes(color = sig)) %>% 
    + scale_color_discrete(name = paste0("Significance\n(", (1-alpha)*100, "% level)"),
                           na.translate = F) %>%
    + facet_grid(~time,scales="free", space="free",  labeller= as_labeller(names)) %>%
    + labs(title = "Reduction of deforestation due to the conservation",
           subtitle = ifelse(is_focus == T,
                             yes = "Sample : protected areas of interest",
                             no = "Sample : other protected areas"),
           caption = "95% confidence intervals are represented | The number of PA in each country is indicated in parentheses",
           x = "p.p.",
           y = "") %>%
    + theme_minimal() %>%
    + theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
      axis.text=element_text(size=11, color = "black"),
      axis.title=element_text(size=14, color = "black", face = "plain"),
      
      plot.caption = element_text(hjust = 0),
      plot.title = element_text(size=16, color = "black", face = "plain", hjust = 0),
      plot.subtitle = element_text(size=12, color = "black", face = "plain", hjust = 0),
      
      strip.text = element_text(color = "black", size = 12),
      strip.clip = "off",
      panel.spacing = unit(2, "lines"),
      
      #legend.position = "bottom",
      legend.text=element_text(size=10),
      #legend.spacing.x = unit(1.0, 'cm'),
      #legend.spacing.y = unit(0.75, 'cm'),
      legend.key.size = unit(2, 'line'),
      
      panel.grid.major.x = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.x = element_line(color = 'grey80', linewidth = 0.2, linetype = 2),
      panel.grid.major.y = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
      panel.grid.minor.y = element_line(color = 'grey80', linewidth = 0.2, linetype = 2)
    )
  
  
  #Tables 
  ## treatment effect : percentage of deforestation avoided
  tbl_fc_att_per_agg = df_plot_fc_att_agg  %>%
    mutate(sig_per = case_when(sign(cband_lower_per) == sign(cband_upper_per) ~ "Yes",
                               sign(cband_lower_per) != sign(cband_upper_per) ~ "No")
    ) %>%
    dplyr::select(c(country_en, n_pa, time, att_per, sig_per)) %>%
    pivot_wider(values_from = c("att_per", "sig_per"), names_from = c("time", "time")) %>%
    dplyr::select(c(country_en, n_pa, att_per_5, sig_per_5, att_per_10, sig_per_10)) %>%
    #dplyr::select(c(country_en, att_per_5, sig_per_5, att_per_10, sig_per_10)) %>%
    mutate(across(.cols = starts_with(c("att")),
                  .fns = \(x) case_when(is.na(x) == TRUE ~ "/", TRUE ~ as.character(format(round(x, 2), scientific = FALSE))))) %>%
    mutate(across(.cols = starts_with(c("sig")),
                  .fns = \(x) case_when(is.na(x) == TRUE ~ "/", TRUE ~ x))) %>%
    rename("# PA" = "n_pa",
           "Effect (5 y., %)" = "att_per_5",
           "Signi. (5 y.)" = "sig_per_5",
           "Effect (10 y., %)" = "att_per_10",
           "Signi. (10 y.)" = "sig_per_10") %>%
    arrange(country_en)
  # names(tbl_fc_att_per) = c("Name", "PA of interest", "Creation",  "Protection", 
  #                           "Governance", "Effect (5 y., %)", "Significance (5 y.)","Effect (10 y., %)", "Significance (10 y.)")
  names(tbl_fc_att_per_agg) = c("Country", "# PA", "Effect (5 y., %)", "Signi. (5 y.)","Effect (10 y., %)", "Signi. (10 y.)")
  
  # treatment effect : total deforestation avoided 
  tbl_fc_att_pa_agg = df_plot_fc_att_agg %>%
    mutate(sig_pa = case_when(sign(cband_lower_pa) == sign(cband_upper_pa) ~ "Yes",
                              sign(cband_lower_pa) != sign(cband_upper_pa) ~ "No")) %>%
    dplyr::select(c(country_en, n_pa, time, att_pa, sig_pa)) %>%
    pivot_wider(values_from = c("att_pa", "sig_pa"), names_from = c("time", "time")) %>%
    # dplyr::select(c(name_pa, country_en, treatment_year, iucn_wolf, gov_type, att_pa_5, sig_pa_5, att_pa_10, sig_pa_10)) %>%
    dplyr::select(c(country_en, n_pa, att_pa_5, sig_pa_5, att_pa_10, sig_pa_10)) %>%
    mutate(across(.cols = starts_with(c("att")),
                  .fns = \(x) case_when(is.na(x) == TRUE ~ "/", TRUE ~ as.character(format(round(x, 2), scientific = FALSE))))) %>%
    mutate(across(.cols = starts_with(c("sig")),
                  .fns = \(x) case_when(is.na(x) == TRUE ~ "/", TRUE ~ x))) %>%
    rename("# PA" = "n_pa",
           "Effect (5 y., %)" = "att_pa_5",
           "Signi. (5 y.)" = "sig_pa_5",
           "Effect (10 y., %)" = "att_pa_10",
           "Signi. (10 y.)" = "sig_pa_10") %>%
    arrange(country_en)
  # names(tbl_fc_att_pa) = c("Name", "PA of interest", "Creation",  "Protection", 
  #                           "Governance", "Effect (5 y., ha)", "Significance (5 y.)","Effect (10 y., ha)", "Significance (10 y.)")
  names(tbl_fc_att_pa_agg) = c("Country", "# PA", "Effect (5 y., ha)", "Signi. (5 y.)","Effect (10 y., ha)", "Signi. (10 y.)")
  
  # treatment effect : avoided deforestation, in terms of difference in cumultaed deforestation rate 
  tbl_fl_att_agg = df_plot_fl_att_agg %>%
    mutate(sig = case_when(sign(cband_lower) == sign(cband_upper) ~ "Yes",
                           sign(cband_lower) != sign(cband_upper) ~ "No")) %>%
    dplyr::select(c(country_en, n_pa, time, att, sig)) %>%
    pivot_wider(values_from = c("att", "sig"), names_from = c("time", "time")) %>%
    # dplyr::select(c(name_pa, country_en, treatment_year, iucn_wolf, gov_type, att_5, sig_5, att_10, sig_10)) %>%
    dplyr::select(c(country_en, n_pa, att_5, sig_5, att_10, sig_10)) %>%
    mutate(across(.cols = starts_with(c("att")),
                  .fns = \(x) case_when(is.na(x) == TRUE ~ "/", TRUE ~ as.character(format(round(x, 2), scientific = FALSE))))) %>%
    mutate(across(.cols = starts_with(c("sig")),
                  .fns = \(x) case_when(is.na(x) == TRUE ~ "/", TRUE ~ x))) %>%
    rename("# PA" = "n_pa",
           "Effect (5 y., %)" = "att_5",
           "Signi. (5 y.)" = "sig_5",
           "Effect (10 y., %)" = "att_10",
           "Signi. (10 y.)" = "sig_10") %>%
    arrange(country_en)
  # names(tbl_fl_att) = c("Name", "PA of interest", "Creation",  "Protection", 
  #                           "Governance", "Effect (5 y., pp)", "Significance (5 y.)","Effect (10 y., pp)", "Significance (10 y.)")
  names(tbl_fl_att_agg) = c("Country", "# PA" , "Effect (5 y., pp)", "Signi. (5 y.)","Effect (10 y., pp)", "Signi. (10 y.)")
  
  
  ##Saving plots
  tmp = paste(tempdir(), "fig", sep = "/")
  
  ggsave(ifelse(is_focus == T, yes = paste(tmp, "fig_att_per_agg_focus.png", sep = "/"), no = paste(tmp, "fig_att_per_agg_nofocus.png", sep = "/")),
         plot = fig_att_per_agg,
         device = "png",
         height = 8, width = 12)
  
  ggsave(ifelse(is_focus == T, yes = paste(tmp, "fig_att_pa_agg_focus.png", sep = "/"), no = paste(tmp, "fig_att_pa_agg_nofocus.png", sep = "/")),
         plot = fig_att_pa_agg,
         device = "png",
         height = 8, width = 12)
  
  ggsave(ifelse(is_focus == T, yes = paste(tmp, "fig_att_fl_agg_focus.png", sep = "/"), no = paste(tmp, "fig_att_fl_agg_nofocus.png", sep = "/")),
         plot = fig_att_fl_agg,
         device = "png",
         height = 8, width = 12)
  
  
  print(xtable(tbl_fc_att_pa_agg, type = "latex", auto = T),
        row.names = FALSE,
        file = ifelse(is_focus == T, yes = paste(tmp, "tbl_fc_att_pa_agg_focus.tex", sep = "/"), no = paste(tmp, "tbl_fc_att_pa_agg_nofocus.tex", sep = "/")))
  
  print(xtable(tbl_fc_att_per_agg, type = "latex", auto = T),
        row.names = FALSE,
        file = ifelse(is_focus == T, yes = paste(tmp, "tbl_fc_att_per_agg_focus.tex", sep = "/"), no = paste(tmp, "tbl_fc_att_per_agg_nofocus.tex", sep = "/")))
  
  print(xtable(tbl_fl_att_agg, type = "latex", auto = T),
        row.names = FALSE,
        file = ifelse(is_focus == T, yes = paste(tmp, "tbl_fl_att_agg_focus.tex", sep = "/"), no = paste(tmp, "tbl_fl_att_agg_nofocus.tex", sep = "/")))
  
  files <- list.files(tmp, full.names = TRUE)
  ##Add each file in the bucket (same foler for every file in the temp)
  for(f in files) 
  {
    cat("Uploading file", paste0("'", f, "'"), "\n")
    aws.s3::put_object(file = f, 
                       bucket = paste("projet-afd-eva-ap", save_dir, sep = "/"),
                       region = "", show_progress = TRUE)
  }
  do.call(file.remove, list(list.files(tmp, full.names = TRUE)))
  
}
