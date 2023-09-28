
#Load the list of PA matched during the matchign process
##INPUTS :
### iso : the ISO code of the country considered
### yr_min : the minimum for treatment year
##OUTPUTS :
### list_pa : a dataframe with the PA matched
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


#Compute average annual deforestation rate à la Wolf et al. 2021
## INPUTS 
### iso : the iso3 code for the country considered
### wdpaid : the WDPAID of the PA considered
### alpha : the threshold for confidence interval
### load_dir : a path to load matching frame
### ext_output : the output extension
## OUTPUTS
### a dataframe with statistics on annual deforestation in matched treated and control units, computed à la Wolf et al. 2021

fn_fl_wolf = function(iso, wdpaid, alpha, load_dir, ext_input)
{
  output = tryCatch(
    
    {
      
  #Import matched units
  df_long = s3read_using(data.table::fread,
                           object = paste0(load_dir, "/", iso, "/", wdpaid, "/", paste0("matched_long", "_", iso, "_", wdpaid, ext_input)),
                           bucket = "projet-afd-eva-ap",
                           opts = list("region" = "")) %>%
    dplyr::select(c(region, iso3, wdpaid, group, assetid, status_yr, year_funding_first, year_funding_all, res_m, year, var, fc_ha))
  #select(c(region, country_en, iso3, wdpaid, group, status_yr, year_funding_first, year_funding_all, year, var, fc_ha))
  
  ##Extract country iso
  country.iso = df_long %>% 
    filter(group == 2) %>% 
    slice(1)
  country.iso = country.iso$iso3
  
  ##Extract region name
  region.name = df_long %>% 
    filter(group == 2) %>% 
    slice(1)
  region.name = region.name$region
  
  #Compute annual deforestation rates à la Wolf et al. 2021 before and after treatment for treated, and for all the period for controls. This is averaged across pixels.
  df_fl_annual_wolf = df_long %>%
    mutate(treatment_year = case_when(group == 1 ~0,
                                      group == 2 ~status_yr), #Set treatment year to 0 for control units (required by did::att_gt)
           time = ifelse(group == 2, yes = year-treatment_year, no = NA),
           .after = status_yr) %>%
    group_by(assetid) %>%
    # mutate(FL_2000_cum = (fc_ha-fc_ha[year == 2000])/fc_ha[year == 2000]*100,
    #        fc_2000 = fc_ha[year == 2000]) %>%
    mutate(FL_annual_wolf_pre = ifelse(group == 2, yes = ((fc_ha[time == -1]/fc_ha[year == 2000])^(1/(year[time == -1] - 2000))-1)*100, no = NA),
           FL_annual_wolf_post = ifelse(group == 2, yes = ((fc_ha[time == max(time)]/fc_ha[time == 0])^(1/max(time))-1)*100, no = NA),
           FL_annual_wolf_tot = ((fc_ha[year == 2021]/fc_ha[year == 2000])^(1/(2021-2000))-1)*100) %>%
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
    mutate(region = region.name, iso3 = country.iso, wdpaid = wdpaid, .before = "group") %>%
    mutate(group = case_when(group == 1 ~ "Control",
                             group == 2 ~ "Treated"))
  
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

#Compute the treatment effect for a given PA
## INPUTS 
### iso : the iso3 code for the country considered
### wdpaid : the WDPAID of the PA considered
### df_long : matched dataframe 
### alpha : the threshold for confidence interval
### save_dir : the saving directory in SSP Cloud
### ext_output : the output extension
## OUTPUTS
### None

fn_did_att_afd = function(iso, wdpaid, data_pa, data_fund, data_report, alpha, is_m, load_dir, ext_input, save_dir)
{
  
  output = tryCatch(
    
    {
      
  df_long_m = s3read_using(data.table::fread,
                           object = paste0(load_dir, "/", iso, "/", wdpaid, "/", paste0("matched_long", "_", iso, "_", wdpaid, ext_input)),
                           bucket = "projet-afd-eva-ap",
                           opts = list("region" = "")) %>%
    dplyr::select(c(region, iso3, wdpaid, group, assetid, status_yr, year_funding_first, year_funding_all, res_m, year, var, fc_ha))
  #dplyr::select(c(region, country_en, iso3, wdpaid, group, status_yr, year_funding_first, year_funding_all, year, var, fc_ha))
  
  df_long_unm = s3read_using(data.table::fread,
                             object = paste0(load_dir, "/", iso, "/", wdpaid, "/", paste0("unmatched_long", "_", iso, "_", wdpaid, ext_input)),
                             bucket = "projet-afd-eva-ap",
                             opts = list("region" = "")) %>%
    dplyr::select(c(region, iso3, wdpaid, group, assetid, status_yr, year_funding_first, year_funding_all, year, res_m, var, fc_ha))
  #dplyr::select(c(region, country_en, iso3, wdpaid, group, status_yr, year_funding_first, year_funding_all, year, var, fc_ha))
  
  if(is_m == TRUE)
  {
    df_long = df_long_m
  } else{df_long = df_long_unm 
  }
  
  #First extract some relevant variables
  ##Extract spatial resolution of pixels res_m and define pixel area in ha
  res_m = unique(df_long$res_m)
  res_ha = res_m^2*1e-4
  
  ##Extract treatment year
  treatment.year = df_long %>% 
    filter(group == 2) %>% 
    slice(1)
  treatment.year = treatment.year$status_yr
  
  ##Extract funding years
  df_fund_yr = df_long %>% 
    filter(group == 2) %>% 
    slice(1)
  funding.years = df_fund_yr$year_funding_first
  list.funding.years = df_fund_yr$year_funding_all
  
  ##Extract country name
  # country.name = df_long %>% 
  #   filter(group == 2) %>% 
  #   slice(1)
  # country.name = country.name$country_en
  
  ##Extract country iso
  country.iso = df_long %>% 
    filter(group == 2) %>% 
    slice(1)
  country.iso = country.iso$iso3
  
  ##Extract region name
  region.name = df_long %>% 
    filter(group == 2) %>% 
    slice(1)
  region.name = region.name$region
  
  ##Extract more information not in the matched dataframe
  ### Area
  wdpa_id = wdpaid #Need to give a name to wdpaid (function argument) different from the varaible in the dataset (wdpaid)
  area_ha = data_pa[data_pa$wdpaid == wdpa_id,]$area_km2*100
  ### Name of the PA
  pa.name = data_pa %>% 
    filter(wdpaid == wdpa_id) %>% 
    slice(1)
  pa.name = pa.name$name_pa
  ### Country name
  country.name = data_pa %>% 
    filter(wdpaid == wdpa_id) %>% 
    slice(1)
  country.name = country.name$country_en
  ### AFD project ID
  # id.project = data_pa %>% 
  #   filter(wdpaid == wdpa_id) %>% 
  #   slice(1)
  # id.project = id.project$id_projet
  ### WDPA status
  status.wdpa = data_pa %>% 
    filter(wdpaid == wdpa_id) %>% 
    slice(1)
  status.wdpa = status.wdpa$status
  ### IUCN category and description
  iucn.wdpa = data_pa %>% 
    filter(wdpaid == wdpa_id) %>% 
    slice(1)
  iucn.cat = iucn.wdpa$iucn_cat
  iucn.des = iucn.wdpa$iucn_des_en
  ### Ecosystem
  eco.wdpa = data_pa %>% 
    filter(wdpaid == wdpa_id) %>% 
    slice(1)
  eco.wdpa = eco.wdpa$marine
  ### Governance
  gov.wdpa = data_pa %>% 
    filter(wdpaid == wdpa_id) %>% 
    slice(1)
  gov.wdpa = gov.wdpa$gov_type
  ### Owner
  own.wdpa = data_pa %>% 
    filter(wdpaid == wdpa_id) %>% 
    slice(1)
  own.wdpa = own.wdpa$own_type
  
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
  n_pix_pa = area_ha/res_ha
  
  #Average forest cover in a treated pixel in 2000
  ## For matched 
  avgFC_2000_m = df_long_m %>% 
    filter(group == 2 & year == 2000) 
  avgFC_2000_m = mean(avgFC_2000_m$fc_ha, na.rm = TRUE)
  ## For unmatched
  avgFC_2000_unm = df_long_unm %>% 
    filter(group == 2 & year == 2000) 
  avgFC_2000_unm = mean(avgFC_2000_unm$fc_ha, na.rm = TRUE)
  
  #Then modify the dataframe before DiD computations
  ## Set treatment year = 0 for controls (necessary for did package to consider "never treated" units)
  ## Compute cumulative deforestation relative to 2000 forest cover (outcome where TE is computed)
  df_did = df_long %>%
    mutate(treatment_year = case_when(group == 1 ~0,
                                      group == 2 ~status_yr), #Set treatment year to 0 for control units (required by did::att_gt)
           time = ifelse(group == 2, yes = year-treatment_year, no = NA),
           .after = status_yr) %>%
    group_by(assetid) %>%
    # mutate(FL_2000_cum = (fc_ha-fc_ha[year == 2000])/fc_ha[year == 2000]*100,
    #        fc_2000 = fc_ha[year == 2000]) %>%
    mutate(FL_2000_cum = case_when(fc_ha[year == 2000] > 0 ~ (fc_ha-fc_ha[year == 2000])/fc_ha[year == 2000]*100, 
                                   TRUE ~ NA)) %>%
    ungroup()
  

  ##Average forest cover in 2000 in a pixel, and average share of forest cover in a pixel
  # fc_2000_avg = mean(df_did[df_did$group == 2,]$fc_2000, na.rm = TRUE)
  # per_fc_2000_avg = min(fc_2000_avg/res_ha, 1) #Take the min as in some cases, reported forest cover is higher than pixel area
  
  #Compute dynamic TE with did package. 
  ## Control are "never treated" units, no covariate is added in the regression estimated with doubly-robust method
  ## standard errors are computed with bootstrap, and confidence intervals computed from it.
  ## No clustering is performed as it does not seem relevant in our case (https://blogs.worldbank.org/impactevaluations/when-should-you-cluster-standard-errors-new-wisdom-econometrics-oracle)
  ## Pseudo ATT are computed for each pre-treatment year (varying base period)
  
  ##For forest cover (ha and %)
  ### ATT computation
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
  
  
  ### Report results in a dataframe
  ### The ATT computed is at pixel level (avoided deforestation in a pixel, in ha)
  ### This ATT is aggregated to PA by multiplying ATT by the number of pixel in the PA. It is also expressed in percentage of pixel area (avoided deforestation in share of pixel area)
  ### confidence intervals (at pixel level) are computed from bootstrap standard errors after a coefficient is applied.
  ### This computation takes the one from did:::summary.MP function, line 15 and 16. 
  ### They are multiplied by the number of pixels to compute CI for ATT at PA level 
  ### They are divided by the pixel area to compute CI for ATT in percentage of pixel area
  df_fc_attgt = data.frame("treatment_year" = fc_attgt$group,
                           "year" = fc_attgt$t,
                           "att_pix" = fc_attgt$att,
                           "c" = fc_attgt$c,
                           "se" = fc_attgt$se,  
                           "n" = fc_attgt$n) %>%
    #Compute ATT at PA level and in share of pixel area
    ## att_pa : the total avoided deforestation is the avoided deforestation in ha in a given pixel, multiplied by the number of pixel in the PA.
    ## att_per : avoided deforestation in a pixel, as a share of average forest cover in 2000 in matched treated. Can be extrapolated to full PA in principle (avoided deforestation in share of 2000 forest cover)
    mutate(att_pa = att_pix*n_pix_pa,
           att_per = att_pix/avgFC_2000_m*100) %>% 
    #Compute time relative to treatment year
    mutate(time = year - treatment_year,
           .before = year) %>%
    #Compute confidence intervals
    mutate(cband_lower_pix = round(att_pix-c*se, 4),
           cband_upper_pix = round(att_pix+c*se, 4),
           cband_lower_pa = cband_lower_pix*n_pix_pa,
           cband_upper_pa = cband_upper_pix*n_pix_pa,
           cband_lower_per = cband_lower_pix/avgFC_2000_m*100,
           cband_upper_per = cband_upper_pix/avgFC_2000_m*100,
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
           res_ha = res_ha,
           id_projet = id.project,
           status_wdpa = status.wdpa,
           iucn_cat = iucn.cat,
           iucn_des_en = iucn.des,
           gov_type = gov.wdpa,
           own_type = own.wdpa,
           marine = eco.wdpa,
           cofund = cofund,
           kfw = kfw,
           ffem = ffem,
           fund_type = fund.type,
           dept_report = reporter,
           funding_year = funding.years,
           funding_year_list = list.funding.years,
           .before = "treatment_year")
  
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
           res_ha = res_ha,
           id_projet = id.project,
           status_wdpa = status.wdpa,
           iucn_cat = iucn.cat,
           iucn_des_en = iucn.des,
           gov_type = gov.wdpa,
           own_type = own.wdpa,
           marine = eco.wdpa,
           cofund = cofund,
           kfw = kfw,
           ffem = ffem,
           fund_type = fund.type,
           dept_report = reporter,
           funding_year = funding.years,
           funding_year_list = list.funding.years,
           .before = "treatment_year")
  
  ###Plot results
  ## ATT : avoided deforestation at pixel level (in ha)
  fig_att_pix = ggplot(data = df_fc_attgt,
                       aes(x = time, y = att_pix)) %>%
    + geom_line(color = "#08519C") %>%
    + geom_point(color = "#08519C") %>%
    + geom_ribbon(aes(ymin = cband_lower_pix, ymax = cband_upper_pix),
                  alpha=0.1, fill = "#FB6A4A", color = "black", linetype = "dotted") %>%
    + labs(title = ifelse(is_m == TRUE, 
                          yes = "Deforestation avoided in a pixel,on average (matched)",
                          no = "Deforestation avoided in a pixel,on average (unmatched)"),
           subtitle = paste0(pa.name, ", ", country.name, ", implemented in ", treatment.year),
           caption = paste("WDPA ID :", wdpa_id, "|", format(area_ha, big.mark = ","), "ha |", "Pixel resolution :", res_ha, "ha", "\nRibbon represents", (1-alpha)*100, "% confidence interval.\nTreatment effect is interpreted as the deforestation avoided at pixel level in hectare, due to the conservation program.\nA negative effect means the conservation program has caused higher deforestation."),
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
  
  # ATT : avoided deforestation in terms of 2000 forest cover
  fig_att_per = ggplot(data = df_fc_attgt,
                       aes(x = time, y = att_per)) %>%
    + geom_line(color = "#08519C") %>%
    + geom_point(color = "#08519C") %>%
    + geom_ribbon(aes(ymin = cband_lower_per, ymax = cband_upper_per),
                  alpha=0.1, fill = "#FB6A4A", color = "black", linetype = "dotted") %>%
    + labs(title = ifelse(is_m == TRUE, 
                          yes = "Average deforestation avoided relative to 2000 forest cover (matched)",
                          no = "Average deforestation avoided relative to 2000 forest cover (unmatched)"),
           subtitle = paste0(pa.name, ", ", country.name, ", implemented in ", treatment.year),
           caption = paste("WDPA ID :", wdpa_id, "|", format(area_ha, big.mark = ","), "ha |", "Pixel resolution :", res_ha, "ha", "\nRibbon represents", (1-alpha)*100, "% confidence interval.\nTreatment effect is interpreted as the deforestation avoided in percentage of 2000 forest cover.\nA negative effect means the conservation program has caused higher deforestation."),
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
  
  # ATT : avoided deforestation in the PA
  fig_att_pa = ggplot(data = df_fc_attgt,
                      aes(x = time, y = att_pa)) %>%
    + geom_line(color = "#08519C") %>%
    + geom_point(color = "#08519C") %>%
    + geom_ribbon(aes(ymin = cband_lower_pa, ymax = cband_upper_pa),
                  alpha=0.1, fill = "#FB6A4A", color = "black", linetype = "dotted") %>%
    + labs(title = ifelse(is_m == TRUE, 
                          yes = "Total deforestation avoided (matched)",
                          no = "Total deforestation avoided (unmatched)"),
           subtitle = paste0(pa.name, ", ", country.name, ", implemented in ", treatment.year),
           caption = paste("WDPA ID :", wdpa_id, "|", format(area_ha, big.mark = ","), "ha |", "Pixel resolution :", res_ha, "ha",  "\nRibbon represents", (1-alpha)*100, "% confidence interval.\nTreatment effect is interpreted as the total deforestation avoided in the protected areas, in hectare (ha).\nThis measure is an extrapolation to the full protected area of average avoided deforestation at pixel level.\nA negative effect means the conservation program has caused higher deforestation."),
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
  

    
  fig_fl_att = ggplot(data = df_fl_attgt,
                      aes(x = time, y = att)) %>%
    + geom_line(color = "#08519C") %>%
    + geom_point(color = "#08519C") %>%
    + geom_ribbon(aes(ymin = cband_lower, ymax = cband_upper),
                  alpha=0.1, fill = "#FB6A4A", color = "black", linetype = "dotted") %>%
    + labs(title = "Effect of the conservation on the deforestation rate, relative to 2000",
           subtitle = paste0(pa.name, ", ", country.name, ", implemented in ", treatment.year),
           caption = paste("WDPA ID :", wdpa_id, "|", format(area_ha, big.mark = ","), "ha |", "Pixel resolution :", res_ha, "ha", "\nRibbon represents ", (1-alpha)*100, " % confidence interval.\nTreatment effect is interpreted as the reduction of cumulated deforestation rate (relative to 2000 forest cover) in percentage points (pp).\nA negative effect means the conservation program has caused higher deforestation."),
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
  return(list("df_fc_att" = df_fc_attgt, "df_fl_att"  = df_fl_attgt, "is_ok" = TRUE))
  
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
  
  #TEST : is ATT computed by the did package coherent with manual computations ? 
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

fn_did_att_general = function(iso, wdpaid, data_pa, alpha, is_m, load_dir, ext_input, save_dir)
{
  
  output = tryCatch(
    
    {
      
      df_long_m = s3read_using(data.table::fread,
                               object = paste0(load_dir, "/", iso, "/", wdpaid, "/", paste0("matched_long", "_", iso, "_", wdpaid, ext_input)),
                               bucket = "projet-afd-eva-ap",
                               opts = list("region" = "")) %>%
        dplyr::select(c(region, iso3, wdpaid, group, assetid, status_yr, year_funding_first, year_funding_all, res_m, year, var, fc_ha))
      #dplyr::select(c(region, country_en, iso3, wdpaid, group, status_yr, year_funding_first, year_funding_all, year, var, fc_ha))
      
      df_long_unm = s3read_using(data.table::fread,
                                 object = paste0(load_dir, "/", iso, "/", wdpaid, "/", paste0("unmatched_long", "_", iso, "_", wdpaid, ext_input)),
                                 bucket = "projet-afd-eva-ap",
                                 opts = list("region" = "")) %>%
        dplyr::select(c(region, iso3, wdpaid, group, assetid, status_yr, year_funding_first, year_funding_all, year, res_m, var, fc_ha))
      #dplyr::select(c(region, country_en, iso3, wdpaid, group, status_yr, year_funding_first, year_funding_all, year, var, fc_ha))
      
      if(is_m == TRUE)
      {
        df_long = df_long_m
      } else{df_long = df_long_unm 
      }
      
      #First extract some relevant variables
      ##Extract spatial resolution of pixels res_m and define pixel area in ha
      res_m = unique(df_long$res_m)
      res_ha = res_m^2*1e-4
      
      ##Extract treatment year
      treatment.year = df_long %>% 
        filter(group == 2) %>% 
        slice(1)
      treatment.year = treatment.year$status_yr
      
      ##Extract funding years
      df_fund_yr = df_long %>% 
        filter(group == 2) %>% 
        slice(1)
      funding.years = df_fund_yr$year_funding_first
      list.funding.years = df_fund_yr$year_funding_all
      
      ##Extract country name
      # country.name = df_long %>% 
      #   filter(group == 2) %>% 
      #   slice(1)
      # country.name = country.name$country_en
      
      ##Extract country iso
      country.iso = df_long %>% 
        filter(group == 2) %>% 
        slice(1)
      country.iso = country.iso$iso3
      
      ##Extract region name
      region.name = df_long %>% 
        filter(group == 2) %>% 
        slice(1)
      region.name = region.name$region
      
      ##Extract more information not in the matched dataframe
      ### Area
      wdpa_id = wdpaid #Need to give a name to wdpaid (function argument) different from the varaible in the dataset (wdpaid)
      area_ha = data_pa[data_pa$wdpaid == wdpa_id,]$area_km2*100
      ### Name of the PA
      pa.name = data_pa %>% 
        filter(wdpaid == wdpa_id) %>% 
        slice(1)
      pa.name = pa.name$name_pa
      ### Country name
      country.name = data_pa %>% 
        filter(wdpaid == wdpa_id) %>% 
        slice(1)
      country.name = country.name$country_en
      ### WDPA status
      status.wdpa = data_pa %>% 
        filter(wdpaid == wdpa_id) %>% 
        slice(1)
      status.wdpa = status.wdpa$status
      ### IUCN category and description
      iucn.wdpa = data_pa %>% 
        filter(wdpaid == wdpa_id) %>% 
        slice(1)
      iucn.cat = iucn.wdpa$iucn_cat
      iucn.des = iucn.wdpa$iucn_des_en
      ### Ecosystem
      eco.wdpa = data_pa %>% 
        filter(wdpaid == wdpa_id) %>% 
        slice(1)
      eco.wdpa = eco.wdpa$marine
      ### Governance
      gov.wdpa = data_pa %>% 
        filter(wdpaid == wdpa_id) %>% 
        slice(1)
      gov.wdpa = gov.wdpa$gov_type
      ### Owner
      own.wdpa = data_pa %>% 
        filter(wdpaid == wdpa_id) %>% 
        slice(1)
      own.wdpa = own.wdpa$own_type
      
      #Extract number of pixels in the PA
      #n_pix_pa = length(unique(filter(df_long_unm, group == 2)$assetid))
      n_pix_pa = area_ha/res_ha
      
      #Average forest cover in a treated pixel in 2000
      ## For matched 
      avgFC_2000_m = df_long_m %>% 
        filter(group == 2 & year == 2000) 
      avgFC_2000_m = mean(avgFC_2000_m$fc_ha, na.rm = TRUE)
      ## For unmatched
      avgFC_2000_unm = df_long_unm %>% 
        filter(group == 2 & year == 2000) 
      avgFC_2000_unm = mean(avgFC_2000_unm$fc_ha, na.rm = TRUE)
      
      #Then modify the dataframe before DiD computations
      ## Set treatment year = 0 for controls (necessary for did package to consider "never treated" units)
      ## Compute cumulative deforestation relative to 2000 forest cover (outcome where TE is computed)
      df_did = df_long %>%
        mutate(treatment_year = case_when(group == 1 ~0,
                                          group == 2 ~status_yr), #Set treatment year to 0 for control units (required by did::att_gt)
               time = ifelse(group == 2, yes = year-treatment_year, no = NA),
               .after = status_yr) %>%
        group_by(assetid) %>%
        # mutate(FL_2000_cum = (fc_ha-fc_ha[year == 2000])/fc_ha[year == 2000]*100,
        #        fc_2000 = fc_ha[year == 2000]) %>%
        mutate(FL_2000_cum = case_when(fc_ha[year == 2000] > 0 ~ (fc_ha-fc_ha[year == 2000])/fc_ha[year == 2000]*100, 
                                       TRUE ~ NA)) %>%
        ungroup()
      
      
      ##Average forest cover in 2000 in a pixel, and average share of forest cover in a pixel
      # fc_2000_avg = mean(df_did[df_did$group == 2,]$fc_2000, na.rm = TRUE)
      # per_fc_2000_avg = min(fc_2000_avg/res_ha, 1) #Take the min as in some cases, reported forest cover is higher than pixel area
      
      #Compute dynamic TE with did package. 
      ## Control are "never treated" units, no covariate is added in the regression estimated with doubly-robust method
      ## standard errors are computed with bootstrap, and confidence intervals computed from it.
      ## No clustering is performed as it does not seem relevant in our case (https://blogs.worldbank.org/impactevaluations/when-should-you-cluster-standard-errors-new-wisdom-econometrics-oracle)
      ## Pseudo ATT are computed for each pre-treatment year (varying base period)
      
      ##For forest cover (ha and %)
      ### ATT computation
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
      
      
      ### Report results in a dataframe
      ### The ATT computed is at pixel level (avoided deforestation in a pixel, in ha)
      ### This ATT is aggregated to PA by multiplying ATT by the number of pixel in the PA. It is also expressed in percentage of pixel area (avoided deforestation in share of pixel area)
      ### confidence intervals (at pixel level) are computed from bootstrap standard errors after a coefficient is applied.
      ### This computation takes the one from did:::summary.MP function, line 15 and 16. 
      ### They are multiplied by the number of pixels to compute CI for ATT at PA level 
      ### They are divided by the pixel area to compute CI for ATT in percentage of pixel area
      df_fc_attgt = data.frame("treatment_year" = fc_attgt$group,
                               "year" = fc_attgt$t,
                               "att_pix" = fc_attgt$att,
                               "c" = fc_attgt$c,
                               "se" = fc_attgt$se,  
                               "n" = fc_attgt$n) %>%
        #Compute ATT at PA level and in share of pixel area
        ## att_pa : the total avoided deforestation is the avoided deforestation in ha in a given pixel, multiplied by the number of pixel in the PA.
        ## att_per : avoided deforestation in a pixel, as a share of average forest cover in 2000 in matched treated. Can be extrapolated to full PA in principle (avoided deforestation in share of 2000 forest cover)
        mutate(att_pa = att_pix*n_pix_pa,
               att_per = att_pix/avgFC_2000_m*100) %>% 
        #Compute time relative to treatment year
        mutate(time = year - treatment_year,
               .before = year) %>%
        #Compute confidence intervals
        mutate(cband_lower_pix = round(att_pix-c*se, 4),
               cband_upper_pix = round(att_pix+c*se, 4),
               cband_lower_pa = cband_lower_pix*n_pix_pa,
               cband_upper_pa = cband_upper_pix*n_pix_pa,
               cband_lower_per = cband_lower_pix/avgFC_2000_m*100,
               cband_upper_per = cband_upper_pix/avgFC_2000_m*100,
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
      ## ATT : avoided deforestation at pixel level (in ha)
      fig_att_pix = ggplot(data = df_fc_attgt,
                           aes(x = time, y = att_pix)) %>%
        + geom_line(color = "#08519C") %>%
        + geom_point(color = "#08519C") %>%
        + geom_ribbon(aes(ymin = cband_lower_pix, ymax = cband_upper_pix),
                      alpha=0.1, fill = "#FB6A4A", color = "black", linetype = "dotted") %>%
        + labs(title = ifelse(is_m == TRUE, 
                              yes = "Deforestation avoided in a pixel,on average (matched)",
                              no = "Deforestation avoided in a pixel,on average (unmatched)"),
               subtitle = paste0(pa.name, ", ", country.name, ", implemented in ", treatment.year),
               caption = paste("WDPA ID :", wdpa_id, "|", format(area_ha, big.mark = ","), "ha |", "Pixel resolution :", res_ha, "ha", "\nRibbon represents", (1-alpha)*100, "% confidence interval.\nTreatment effect is interpreted as the deforestation avoided at pixel level in hectare, due to the conservation program.\nA negative effect means the conservation program has caused higher deforestation."),
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
      
      # ATT : avoided deforestation in terms of 2000 forest cover
      fig_att_per = ggplot(data = df_fc_attgt,
                           aes(x = time, y = att_per)) %>%
        + geom_line(color = "#08519C") %>%
        + geom_point(color = "#08519C") %>%
        + geom_ribbon(aes(ymin = cband_lower_per, ymax = cband_upper_per),
                      alpha=0.1, fill = "#FB6A4A", color = "black", linetype = "dotted") %>%
        + labs(title = ifelse(is_m == TRUE, 
                              yes = "Average deforestation avoided relative to 2000 forest cover (matched)",
                              no = "Average deforestation avoided relative to 2000 forest cover (unmatched)"),
               subtitle = paste0(pa.name, ", ", country.name, ", implemented in ", treatment.year),
               caption = paste("WDPA ID :", wdpa_id, "|", format(area_ha, big.mark = ","), "ha |", "Pixel resolution :", res_ha, "ha", "\nRibbon represents", (1-alpha)*100, "% confidence interval.\nTreatment effect is interpreted as the deforestation avoided in percentage of 2000 forest cover.\nA negative effect means the conservation program has caused higher deforestation."),
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
      
      # ATT : avoided deforestation in the PA
      fig_att_pa = ggplot(data = df_fc_attgt,
                          aes(x = time, y = att_pa)) %>%
        + geom_line(color = "#08519C") %>%
        + geom_point(color = "#08519C") %>%
        + geom_ribbon(aes(ymin = cband_lower_pa, ymax = cband_upper_pa),
                      alpha=0.1, fill = "#FB6A4A", color = "black", linetype = "dotted") %>%
        + labs(title = ifelse(is_m == TRUE, 
                              yes = "Total deforestation avoided (matched)",
                              no = "Total deforestation avoided (unmatched)"),
               subtitle = paste0(pa.name, ", ", country.name, ", implemented in ", treatment.year),
               caption = paste("WDPA ID :", wdpa_id, "|", format(area_ha, big.mark = ","), "ha |", "Pixel resolution :", res_ha, "ha",  "\nRibbon represents", (1-alpha)*100, "% confidence interval.\nTreatment effect is interpreted as the total deforestation avoided in the protected areas, in hectare (ha).\nThis measure is an extrapolation to the full protected area of average avoided deforestation at pixel level.\nA negative effect means the conservation program has caused higher deforestation."),
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
      
      
      
      fig_fl_att = ggplot(data = df_fl_attgt,
                          aes(x = time, y = att)) %>%
        + geom_line(color = "#08519C") %>%
        + geom_point(color = "#08519C") %>%
        + geom_ribbon(aes(ymin = cband_lower, ymax = cband_upper),
                      alpha=0.1, fill = "#FB6A4A", color = "black", linetype = "dotted") %>%
        + labs(title = "Effect of the conservation on the deforestation rate, relative to 2000",
               subtitle = paste0(pa.name, ", ", country.name, ", implemented in ", treatment.year),
               caption = paste("WDPA ID :", wdpa_id, "|", format(area_ha, big.mark = ","), "ha |", "Pixel resolution :", res_ha, "ha", "\nRibbon represents ", (1-alpha)*100, " % confidence interval.\nTreatment effect is interpreted as the reduction of cumulated deforestation rate (relative to 2000 forest cover) in percentage points (pp).\nA negative effect means the conservation program has caused higher deforestation."),
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
      return(list("df_fc_att" = df_fc_attgt, "df_fl_att"  = df_fl_attgt, "is_ok" = TRUE))
      
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
  
  #TEST : is ATT computed by the did package coherent with manual computations ? 
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

#Plot the forest cover loss relative to 2000 in treated and control, before and after matching
##INPUTS
###
##OUTPUTS
### 

fn_plot_forest_loss = function(iso, wdpaid, data_pa, alpha, load_dir, ext_input, save_dir)
{
  
  #Loading matched and unmatched data frames
  df_long_m_raw = s3read_using(data.table::fread,
                               object = paste0(load_dir, "/", iso, "/", wdpaid, "/", paste0("matched_long", "_", iso, "_", wdpaid, ext_input)),
                               bucket = "projet-afd-eva-ap",
                               opts = list("region" = "")) %>%
    dplyr::select(c(region, iso3, wdpaid, group, assetid, status_yr, year_funding_first, year_funding_all, res_m, year, var, fc_ha))
  
  df_long_unm_raw = s3read_using(data.table::fread,
                                 object = paste0(load_dir, "/", iso, "/", wdpaid, "/", paste0("unmatched_long", "_", iso, "_", wdpaid, ext_input)),
                                 bucket = "projet-afd-eva-ap",
                                 opts = list("region" = "")) %>%
    dplyr::select(c(region, iso3, wdpaid, group, assetid, status_yr, year_funding_first, year_funding_all, year, res_m, var, fc_ha))
  
  wdpa_id = wdpaid
  #Extract relevant information
  ##Spatial resolution of pixels res_m and define pixel area in ha
  res_m = unique(df_long_m_raw$res_m)
  res_ha = res_m^2*1e-4
  
  ##treatment year
  treatment.year = df_long_m_raw %>% 
    filter(group == 2) %>% 
    slice(1)
  treatment.year = treatment.year$status_yr
  
  ##funding years
  funding.years = df_long_m_raw %>% 
    filter(group == 2) %>% 
    slice(1)
  funding.years = funding.years$year_funding_first
  #funding.years = as.numeric(unlist(strsplit(funding.years$year_funding_all, split = ",")))
  
  ##country iso
  country.iso = df_long_m_raw %>% 
    filter(group == 2) %>% 
    slice(1)
  country.iso = country.iso$iso3
  
  ##region name
  region.name = df_long_m_raw %>% 
    filter(group == 2) %>% 
    slice(1)
  region.name = region.name$region
  
  ##Area of the PA and PA/country name
  area_ha = data_pa[data_pa$wdpaid == wdpa_id,]$area_km2*100
  country.name = data_pa %>% 
    filter(iso3 == iso) %>% 
    slice(1)
  country.name = country.name$country_en
  pa.name = data_pa %>% 
    filter(wdpaid == wdpa_id) %>% 
    slice(1)
  pa.name = pa.name$name_pa
  
  
  #Forest cover loss is computed for each pixel relative to 2000, then average forest cover evolution and loss is computed for treated and controls
  df_long_m = df_long_m_raw %>%
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
              fc_rel00_ha_ci_lower = avgfc_rel00_ha - qt((1-alpha)/2,df=n-1)*sdfc_rel00_ha/sqrt(n),
              matched = T) %>%
    #Compute total forest cover and forest loss relative to 2000, knowing area of the PA and average forest share in a pixel in 2000
    #CI are computed at 95% confidence level
    ungroup() %>%
    mutate(#per_fc_2000_avg = min(fc_ha[year == 2000]/res_ha, 1),
      #fc_tot_ha = fc_ha*(area_ha*per_fc_2000_avg/res_ha),
      fc_tot_ha = avgfc_ha*(area_ha/res_ha),
      #fc_tot_rel00_ha = avgfc_rel00_ha*(area_ha*per_fc_2000_avg/res_ha),
      fc_tot_rel00_ha = avgfc_rel00_ha*(area_ha/res_ha),
      fc_tot_ha_ci_upper = fc_ha_ci_upper*(area_ha/res_ha),
      fc_tot_ha_ci_upper = fc_ha_ci_lower*(area_ha/res_ha),
      fc_tot_rel00_ha_ci_upper = fc_rel00_ha_ci_upper*(area_ha/res_ha),
      fc_tot_rel00_ha_ci_lower = fc_rel00_ha_ci_lower*(area_ha/res_ha),
      alpha = alpha)
  
  df_long_unm = df_long_unm_raw %>%
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
              fc_rel00_ha_ci_lower = avgfc_rel00_ha - qt((1-alpha)/2,df=n-1)*sdfc_rel00_ha/sqrt(n),
              matched = F) %>%
    #Compute total forest cover and forest loss relative to 2000, knowing area of the PA and average forest share in a pixel in 2000
    #CI are computed at 95% confidence level
    ungroup() %>%
    mutate(#per_fc_2000_avg = min(fc_ha[year == 2000]/res_ha, 1),
      #fc_tot_ha = fc_ha*(area_ha*per_fc_2000_avg/res_ha),
      fc_tot_ha = avgfc_ha*(area_ha/res_ha),
      #fc_tot_rel00_ha = avgfc_rel00_ha*(area_ha*per_fc_2000_avg/res_ha),
      fc_tot_rel00_ha = avgfc_rel00_ha*(area_ha/res_ha),
      fc_tot_ha_ci_upper = fc_ha_ci_upper*(area_ha/res_ha),
      fc_tot_ha_ci_upper = fc_ha_ci_lower*(area_ha/res_ha),
      fc_tot_rel00_ha_ci_upper = fc_rel00_ha_ci_upper*(area_ha/res_ha),
      fc_tot_rel00_ha_ci_lower = fc_rel00_ha_ci_lower*(area_ha/res_ha),
      alpha = alpha)
  
  
  #Define plotting dataset
  df_plot = rbind(df_long_m, df_long_unm) %>%
    mutate(group = case_when(group == 1 ~"Control",
                             group == 2 ~"Treated"),
           region = region.name,
           country_en = country.name,
           iso3 = country.iso,
           wdpaid = wdpaid, 
           name_pa = pa.name,
           area_ha = area_ha)
  
  #The period where deforestation is plotted
  year.max = max(df_long_m$year)
  
  #Plot
  fct.labs <- c("Before Matching", "After Matching")
  names(fct.labs) <- c(FALSE, TRUE)
  
  fig = ggplot(data = filter(df_plot, year == year.max),
               aes(y = abs(fc_tot_rel00_ha), fill = as.factor(group), x = group)) %>%
    + geom_bar(position =  position_dodge(width = 0.8), stat = "identity", show.legend = FALSE) %>% 
    + geom_errorbar(aes(ymax=abs(fc_tot_rel00_ha_ci_upper), ymin=abs(fc_tot_rel00_ha_ci_lower)), width=0.4, colour="grey60", alpha=0.9, size=1.3) %>%
    + geom_label(aes(label = format(round(abs(fc_tot_rel00_ha), 0), big.mark = ","), y = abs(fc_tot_rel00_ha)), 
                 color = "black",
                 show.legend = FALSE) %>%
    + scale_fill_brewer(name = "Group", palette = "Blues") %>%
    + labs(x = "",
           y = "Forest cover loss (ha)",
           title = paste("Average area deforested between 2000 and", year.max),
           subtitle = paste("WDPA ID", wdpaid, "in", country.iso, ",implemented in", treatment.year, "and covering", format(area_ha, big.mark = ","), "ha"),
           caption = paste((1-alpha)*100, "% confidence intervals.")) %>%
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
  
  return(df_plot)
}


# Plotting the ATT of each PA analyzed in the same graph
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
  
  #ATT for each wdpa (some have not on the two time periods)
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
  
  ## Att in share of 2000 forest cover
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
    + labs(title = "Deforestation avoided relative to 2000 forest cover",
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
    + labs(title = "Deforestation avoided relative to 2000 forest cover",
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

    
  ##ATT : total deforestation avoided
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
  
  ##ATT : avoided deforestation in percentage points
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
  ## ATT : percentage of deforestation avoided
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
  
  # ATT : total deforestation avoided 
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
  
  #ATT for each wdpa (some have not on the two time periods)
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
            `focus` = "FAPBM funded",
            `not focus` = "Others")
  # df_colors = df_plot_fc_att %>% group_by(n) %>% slice(1)
  # colors = ifelse(df_colors$wdpaid %in% list_focus,"#3182BD","black")
  
  ## Att in share of 2000 forest cover
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
    + labs(title = "Deforestation avoided relative to 2000 forest cover",
           #caption = "FAPBM funded protected areas are in blue, others are in black.",
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
    + labs(title = "Deforestation avoided relative to 2000 forest cover",
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
    + labs(title = "Deforestation avoided relative to 2000 forest cover",
           subtitle = "Protected areas funded by the FAPBM only",
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
  
  
  ##ATT : total deforestation avoided
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
           subtitle = "Protected areas funded by the FAPBM only",
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
  
  ##ATT : avoided deforestation in percentage points
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
           subtitle = "Protected areas funded by the FAPBM only",
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
  ## ATT : percentage of deforestation avoided
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
  # names(tbl_fc_att_per) = c("Name", "FAPBM", "Creation",  "Protection", 
  #                           "Governance", "Effect (5 y., %)", "Significance (5 y.)","Effect (10 y., %)", "Significance (10 y.)")
  names(tbl_fc_att_per) = c("Name", "FAPBM", "Creation", "Protection", 
                            "Effect (5 y., %)", "Signi. (5 y.)","Effect (10 y., %)", "Signi. (10 y.)")
  
  # ATT : total deforestation avoided 
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
  # names(tbl_fc_att_pa) = c("Name", "FAPBM", "Creation",  "Protection", 
  #                           "Governance", "Effect (5 y., ha)", "Significance (5 y.)","Effect (10 y., ha)", "Significance (10 y.)")
  names(tbl_fc_att_pa) = c("Name", "FAPBM", "Creation", "Protection", 
                            "Effect (5 y., ha)", "Signi. (5 y.)","Effect (10 y., ha)", "Signi. (10 y.)")
  
  # ATT : avoided deforestation, in terms of difference in cumultaed deforestation rate 
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
  # names(tbl_fl_att) = c("Name", "FAPBM", "Creation",  "Protection", 
  #                           "Governance", "Effect (5 y., pp)", "Significance (5 y.)","Effect (10 y., pp)", "Significance (10 y.)")
  names(tbl_fl_att) = c("Name", "FAPBM", "Creation", "Protection", 
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
