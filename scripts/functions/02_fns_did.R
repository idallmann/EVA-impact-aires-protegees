
#Load the list of PA matched during the matchign process
##INPUTS :
### iso : the ISO code of the country considered
### yr_min : the minimum for treatment year
##OUTPUTS :
### list_pa : a dataframe with the PA matched
fn_did_list_pa = function(iso, save_dir)
{
  list_pa = s3read_using(data.table::fread,
                    bucket = "projet-afd-eva-ap",
                    object = paste(save_dir, iso, paste0("list_pa_matched_", iso, ".csv"), sep = "/"),
                    opts = list("region" = "")) 

  return(list_pa)
}


fn_did_load_df = function(iso, wdpaid, save_dir, ext_output)
{
  #Load matched datasets
  df_matched_wide = s3read_using(data.table::fread,
                                 object = paste0(save_dir, "/", iso, "/", wdpaid, "/", paste0("matched_wide", "_", iso, "_", wdpaid, ext_output)),
                                 bucket = "projet-afd-eva-ap",
                                 opts = list("region" = "")) %>%
    select(c(region, country_en, iso3, wdpaid, group, assetid, status_yr, year_funding_first, year_funding_all, res_m, starts_with("treecover"), starts_with("treeloss")))
  
  df_unmatched_wide = s3read_using(data.table::fread,
                                 object = paste0(save_dir, "/", iso, "/", wdpaid, "/", paste0("unmatched_wide", "_", iso, "_", wdpaid, ext_output)),
                                 bucket = "projet-afd-eva-ap",
                                 opts = list("region" = "")) %>%
    select(c(region, country_en, iso3, wdpaid, group, assetid, status_yr, year_funding_first, year_funding_all, res_m, starts_with("treecover"), starts_with("treeloss")))
  
  df_matched_long = s3read_using(data.table::fread,
                                 object = paste0(save_dir, "/", iso, "/", wdpaid, "/", paste0("matched_long", "_", iso, "_", wdpaid, ext_output)),
                                 bucket = "projet-afd-eva-ap",
                                 opts = list("region" = "")) %>%
    select(c(region, iso3, wdpaid, group, assetid, status_yr, year_funding_first, year_funding_all, res_m, year, var, fc_ha))
    #select(c(region, country_en, iso3, wdpaid, group, status_yr, year_funding_first, year_funding_all, year, var, fc_ha))
  
  df_unmatched_long = s3read_using(data.table::fread,
                                 object = paste0(save_dir, "/", iso, "/", wdpaid, "/", paste0("unmatched_long", "_", iso, "_", wdpaid, ext_output)),
                                 bucket = "projet-afd-eva-ap",
                                 opts = list("region" = "")) %>%
    select(c(region, iso3, wdpaid, group, assetid, status_yr, year_funding_first, year_funding_all, year, res_m, var, fc_ha))
    #select(c(region, country_en, iso3, wdpaid, group, status_yr, year_funding_first, year_funding_all, year, var, fc_ha))
  
  return(list("df_matched_wide" = df_matched_wide, "df_unmatched_wide" = df_unmatched_wide,
              "df_matched_long" = df_matched_long, "df_unmatched_long" = df_unmatched_long))

}


#Compute the treatment effect for a given PA
fn_did_te = function(iso, wdpaid, df_long_m, dt_start, dt_end, yr_T_min, yr_T_max, alpha, save_dir, ext_output)
{
  
  ####
  #1ere option : faire manuellement le calcul du TE à la Master Thesis
  ####
  
  
  #First extract some relevant variables
  #Extract spatial resolution of pixels res_m and define pixel area in ha
  res_m = unique(df_long_m$res_m)
  res_ha = res_m^2*1e-4
  
  #Extract treatment year
  treatment.year = df_long_m %>% 
    filter(group == 2) %>% 
    slice(1)
  treatment.year = treatment.year$status_yr
  
  #Extract funding years
  funding.years = df_long_m %>% 
    filter(group == 2) %>% 
    slice(1)
  funding.years = funding.years$year_funding_first
  #funding.years = as.numeric(unlist(strsplit(funding.years$year_funding_all, split = ",")))
  
  #Extract country name
  # country.name = df_long_m %>% 
  #   filter(group == 2) %>% 
  #   slice(1)
  # country.name = country.name$country_en
  
  #Extract country iso
  country.iso = df_long_m %>% 
    filter(group == 2) %>% 
    slice(1)
  country.iso = country.iso$iso3
  
  #Extract region name
  region.name = df_long_m %>% 
    filter(group == 2) %>% 
    slice(1)
  region.name = region.name$region
  
  # Define the number of periods we can extract before and after treatment regarding 
  #time period covered by the dataset [dt_start, dt_end]
  nper_before = treatment.year - dt_start
  nper_after = dt_end - treatment.year
  #Creating relevant name for the variables (outcome before and after treatment in T and C)
  ##For T
  Tname_before = paste0("Tyear_", c((yr_T_max-dt_start):1))
  Tname_inst = "Tyear+0"
  Tname_after = paste0("Tyear+", c(1:(dt_end-yr_T_min)))
  Tname = c(Tname_before, Tname_inst, Tname_after)
  ##For C
  Cname_before = paste0("Cyear_", c((yr_T_max-dt_start):1))
  Cname_inst = "Cyear+0"
  Cname_after = paste0("Cyear+", c(1:(dt_end-yr_T_min)))
  Cname = c(Cname_before, Cname_inst, Cname_after)
  
  #Average evolution of forest cover in control and treated groups
  df_fc = df_long_m %>%
  #First, compute deforestation relative to 2000 for each pixel (deforestation as computed in Wolf et al. 2021)
  group_by(assetid) %>%
    # mutate(FL_2000_cum = (fc_ha-fc_ha[year == 2000])/fc_ha[year == 2000]*100,
    #        fper = fc_ha/res_ha*100) %>%
    ungroup() %>%
    #Then compute the average forest cover and deforestation in each year, for treated and control groups
    #Standard deviation and 95% confidence interval is also computed for each variable
    group_by(group, year) %>%
    summarise(n = n(),
              avgFC = mean(fc_ha, na.rm=TRUE),
              sdFC = sd(fc_ha, na.rm = TRUE),
              # ciFC_low = avgFC - qt(0.975,df=n-1)*sdFC/sqrt(n),
              # ciFC_up = avgFC + qt(0.975,df=n-1)*sdFC/sqrt(n),
              # avgFCper = mean(fper, na.rm=TRUE),
              # sdFCper = sd(fper, na.rm = TRUE),
              # ciFCper_low = avgFCper - qt(0.975,df=n-1)*sdFCper/sqrt(n),
              # ciFCper_up = avgFCper + qt(0.975,df=n-1)*sdFCper/sqrt(n),
              # avgFL_2000_cum = mean(FL_2000_cum, na.rm = TRUE),
              # sdFL_2000_cum = sd(FL_2000_cum, na.rm = TRUE),
              # ciFL_low = avgFL_2000_cum - qt(0.975,df=n-1)*sdFL_2000_cum/sqrt(n),
              # ciFL_up = avgFL_2000_cum + qt(0.975,df=n-1)*sdFL_2000_cum/sqrt(n),
              matched = TRUE) %>%
    mutate(year_treatment = treatment.year,
           year_funding = funding.years,
           .after = year) %>%
    mutate(region = region.name,
           iso3 = iso,
           country_en = country.name,
           .before = group) %>%
    ungroup()
  
    test = df_fc %>%
      mutate(group = case_when(group == 1 ~ "C",
                               group == 2 ~ "T")) %>%
      pivot_wider(names_from = "group",
                  values_from = c("avgFC", "sdFC")) %>%
      pivot_wider(names_from = "year",
                values_from = c(starts_with("avgFC"), starts_with("sdFC")))
    
    df = test
    
    #Get the outcome for the years before and after the treatment year
    #The sooner treatment period is yr_T_min, the later is yr_T_max. Then there are at most 
    #yr_T_max-dt_start pre-treatment period, or yr_T_max-dt_start-nper_before for a given deal. 
    #The same way, there at most dt_end-yr_T_min post treatment periods, and
    #dt_end-yr_T_min-nper_after for a given deal. 
    #So that all rows have the same number of columns, pre- and post-treatment period with no data
    #are filled with NA
    ##T
    if(treatment.year != dt_start)
    {
      T_before = cbind(data.frame(matrix(NA, nrow = 1, ncol = yr_T_max-dt_start-nper_before)), df[1, paste0("avgFC_T_", (dt_start):(treatment.year - 1))])
      C_before = cbind(data.frame(matrix(NA, nrow = 1, ncol = yr_T_max-dt_start-nper_before)), df[1, paste0("avgFC_C_", (treatment.year - nper_before):(treatment.year - 1))])
      
    }
    if(treatment.year == dt_start)
    {
      T_before = data.frame(matrix(NA, nrow = 1, ncol = yr_T_max-dt_start-nper_before))
      C_before = data.frame(matrix(NA, nrow = 1, ncol = yr_T_max-dt_start-nper_before))
    }
    if(treatment.year != dt_end)
    {
      T_after = cbind(df[1, paste0("avgFC_T_", (treatment.year + 1):(dt_end))], data.frame(matrix(NA, nrow = 1, ncol = dt_end-yr_T_min-nper_after)))
      C_after = cbind(df[1, paste0("avgFC_C_", (treatment.year + 1):(treatment.year + nper_after))], data.frame(matrix(NA, nrow = 1, ncol = dt_end-yr_T_min-nper_after)))
    }
    if(treatment.year == dt_end)
    {
      T_after = data.frame(matrix(NA, nrow = 1, ncol = dt_end-yr_T_min-nper_after))
      C_after = data.frame(matrix(NA, nrow = 1, ncol = dt_end-yr_T_min-nper_after))
    }
    
    # T_before = ifelse(treatment.year == dt_start,
    #                      yes = data.frame(matrix(NA, nrow = 1, ncol = yr_T_max-dt_start-nper_before)),
    #                      no = cbind(data.frame(matrix(NA, nrow = 1, ncol = yr_T_max-dt_start-nper_before)), df[i, paste0("T_", (dt_start):(treatment.year - 1))]))
    T_inst =  df[1, paste0("avgFC_T_", treatment.year)]
    # T_after = ifelse(treatment.year == dt_end,
    #                     yes = data.frame(matrix(NA, nrow = 1, ncol = dt_end-yr_T_min-nper_after)),
    #                     no = cbind(df[i, paste0("T_", (treatment.year + 1):(dt_end))], data.frame(matrix(NA, nrow = 1, ncol = dt_end-yr_T_min-nper_after))))
    ##C
    # C_before = ifelse(treatment.year == dt_start,
    #                       yes = data.frame(matrix(NA, nrow = 1, ncol = yr_T_max-dt_start-nper_before)),
    #                       no = cbind(data.frame(matrix(NA, nrow = 1, ncol = yr_T_max-dt_start-nper_before)), df[i, paste0("C_", (treatment.year - nper_before):(treatment.year - 1))]))
    C_inst =  df[1, paste0("avgFC_C_", treatment.year)]
    # C_after = ifelse(treatment.year == dt_end,
    #                     yes = data.frame(matrix(NA, nrow = 1, ncol = dt_end-yr_T_min-nper_after)),
    #                     no = cbind(df[i, paste0("C_", (treatment.year + 1):(treatment.year + nper_after))], data.frame(matrix(NA, nrow = 1, ncol = dt_end-yr_T_min-nper_after))))
    # Combine the outcome for T and C before and after treatment
    df_did = cbind(T_before, T_inst, T_after, 
                         C_before, C_inst, C_after) 
    #Modify the names with pre- and post-treatment periods info
    names(df_did) = c(Tname, Cname)
    
    df_did = df_did %>%
      mutate(region = region.name, 
             #country_en = country.name,
             iso3 = country.iso,
             .before = Tname[1])
    
    
    
    
    ####
    #Option 2 : utiliser le package did
    ####
    
    #First extract some relevant variables
    #Extract spatial resolution of pixels res_m and define pixel area in ha
    res_m = unique(df_long_m$res_m)
    res_ha = res_m^2*1e-4
    
    df_did = df_long_m %>%
      mutate(treatment_year = case_when(group == 1 ~0,
                                        group == 2 ~status_yr), #Set treatment year to 0 for control units (required by did::att_gt)
             .after = status_yr) %>%
      group_by(assetid) %>%
      mutate(FL_2000_cum = (fc_ha-fc_ha[year == 2000])/fc_ha[year == 2000]*100,
             fper = fc_ha/res_ha*100) %>%
      ungroup()
    
    
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
                      cband = TRUE, #Compute classic CI
                      clustervars = NULL, #No clustering seems relevant to me (https://blogs.worldbank.org/impactevaluations/when-should-you-cluster-standard-errors-new-wisdom-econometrics-oracle)
                      base_period = "varying",
                      data = df_did,
                      print_details = F)
    
    ##Après : 
    ## Extraire les variables pertinentes
    
    df_fc_attgt = data.frame("treatment_year" = fc_attgt$group,
                             "year" = fc_attgt$t,
                             "att" = fc_attgt$att,
                             "se" = fc_attgt$se,
                             "n" = fc_attgt$n)
    
    ##Calculer bootstrap et CI pour les ajouter ! En effet, data::tidy() calcle autrement les CI
    inffunc = fc_attgt$inffunc
    n = fc_attgt$n
    V <- Matrix::t(inffunc) %*% inffunc/n
    se = sqrt(Matrix::diag(V)/n)
    se[se <= sqrt(.Machine$double.eps) * 10] <- NA
    zero_na_sd_entry <- unique(which(is.na(se)))
    bout <- did::mboot(inffunc, DIDparams = fc_attgt$DIDparams)
    bres <- bout$bres
    if (length(zero_na_sd_entry) > 0) {
      se[-zero_na_sd_entry] <- bout$se[-zero_na_sd_entry]
    }else {
      se <- bout$se
    }
    se[se <= sqrt(.Machine$double.eps) * 10] <- NA
    cval <- qnorm(1 - alpha/2)
    bSigma <- apply(bres, 2, function(b) (quantile(b, 
                                                   0.75, type = 1, na.rm = T) - quantile(b, 0.25, 
                                                                                         type = 1, na.rm = T))/(qnorm(0.75) - qnorm(0.25)))
    bSigma[bSigma <= sqrt(.Machine$double.eps) * 10] <- NA
    bT <- apply(bres, 1, function(b) max(abs(b/bSigma), 
                                         na.rm = TRUE))
    cval <- quantile(bT, 1 - alp, type = 1, na.rm = T)
    
    
    summary(fc_attgt)
    did::ggdid(fc_attgt)
    did::process_attgt(fc_attgt)
    
    test = did::tidy(fc_attgt)
    test = fc_attgt$inffunc
    did::att_gt
    
    df_fc_attgt = data.frame("treatment_year" = fc_attgt$group,
                             "year" = fc_attgt$t,
                             "att" = fc_attgt$att,
                             "se" = fc_attgt$se,
                             "n" = fc_attgt$n)
    
    fl_attgt = did::att_gt(yname = "FL_2000_cum",
                           gname = "treatment_year",
                           idname = "assetid",
                           tname = "year",
                           control_group = "nevertreated",
                           xformla = ~1,
                           data = df_did)
    
    summary(fl_attgt)
    did::ggdid(fl_attgt)
    
    
    
    
  
  #Average deforestation in control and treated groups
  df_defo = df_long_m %>%
    #First, compute deforestation relative to 2000 for each pixel (deforestation as computed in Wolf et al. 2021)
    group_by(assetid) %>%
    mutate(FL_2000_cum = (fc_ha-fc_ha[year == 2000])/fc_ha[year == 2000]*100) %>%
    ungroup() %>%
    #Then compute the average forest cover and deforestation in each year, for treated and control groups
    #Standard deviation and 95% confidence interval is also computed for each variable
    group_by(group, year) %>%
    summarise(n = n(),
              # avgFC = mean(fc_ha, na.rm=TRUE),
              # sdFC = sd(fc_ha, na.rm = TRUE),
              # ciFC_low = avgFC - qt(0.975,df=n-1)*sdFC/sqrt(n),
              # ciFC_up = avgFC + qt(0.975,df=n-1)*sdFC/sqrt(n),
              # avgFCper = mean(fper, na.rm=TRUE),
              # sdFCper = sd(fper, na.rm = TRUE),
              # ciFCper_low = avgFCper - qt(0.975,df=n-1)*sdFCper/sqrt(n),
              # ciFCper_up = avgFCper + qt(0.975,df=n-1)*sdFCper/sqrt(n),
              avgFL_2000_cum = mean(FL_2000_cum, na.rm = TRUE),
              sdFL_2000_cum = sd(FL_2000_cum, na.rm = TRUE),
              # ciFL_low = avgFL_2000_cum - qt(0.975,df=n-1)*sdFL_2000_cum/sqrt(n),
              # ciFL_up = avgFL_2000_cum + qt(0.975,df=n-1)*sdFL_2000_cum/sqrt(n),
              matched = TRUE) %>%
    mutate(year_treatment = treatment.year,
           year_funding = funding.years,
           .after = year) %>%
    ungroup()
  
  
}
  
  
  
  
  
  
  
