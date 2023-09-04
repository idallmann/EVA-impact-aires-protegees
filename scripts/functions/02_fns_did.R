
#Load the list of PA matched during the matchign process
##INPUTS :
### iso : the ISO code of the country considered
### yr_min : the minimum for treatment year
##OUTPUTS :
### list_pa : a dataframe with the PA matched
fn_did_list_pa = function(iso, load_dir)
{
  list_pa = s3read_using(data.table::fread,
                    bucket = "projet-afd-eva-ap",
                    object = paste(load_dir, iso, paste0("list_pa_matched_", iso, ".csv"), sep = "/"),
                    opts = list("region" = "")) 

  return(list_pa)
}


fn_did_load_df = function(iso, wdpaid, load_dir, ext_input)
{
  #Load matched datasets
  df_matched_wide = s3read_using(data.table::fread,
                                 object = paste0(load_dir, "/", iso, "/", wdpaid, "/", paste0("matched_wide", "_", iso, "_", wdpaid, ext_input)),
                                 bucket = "projet-afd-eva-ap",
                                 opts = list("region" = "")) %>%
    select(c(region, country_en, iso3, wdpaid, group, assetid, status_yr, year_funding_first, year_funding_all, res_m, starts_with("treecover"), starts_with("treeloss")))
  
  df_unmatched_wide = s3read_using(data.table::fread,
                                 object = paste0(load_dir, "/", iso, "/", wdpaid, "/", paste0("unmatched_wide", "_", iso, "_", wdpaid, ext_input)),
                                 bucket = "projet-afd-eva-ap",
                                 opts = list("region" = "")) %>%
    select(c(region, country_en, iso3, wdpaid, group, assetid, status_yr, year_funding_first, year_funding_all, res_m, starts_with("treecover"), starts_with("treeloss")))
  
  df_matched_long = s3read_using(data.table::fread,
                                 object = paste0(load_dir, "/", iso, "/", wdpaid, "/", paste0("matched_long", "_", iso, "_", wdpaid, ext_input)),
                                 bucket = "projet-afd-eva-ap",
                                 opts = list("region" = "")) %>%
    select(c(region, iso3, wdpaid, group, assetid, status_yr, year_funding_first, year_funding_all, res_m, year, var, fc_ha))
    #select(c(region, country_en, iso3, wdpaid, group, status_yr, year_funding_first, year_funding_all, year, var, fc_ha))
  
  df_unmatched_long = s3read_using(data.table::fread,
                                 object = paste0(load_dir, "/", iso, "/", wdpaid, "/", paste0("unmatched_long", "_", iso, "_", wdpaid, ext_input)),
                                 bucket = "projet-afd-eva-ap",
                                 opts = list("region" = "")) %>%
    select(c(region, iso3, wdpaid, group, assetid, status_yr, year_funding_first, year_funding_all, year, res_m, var, fc_ha))
    #select(c(region, country_en, iso3, wdpaid, group, status_yr, year_funding_first, year_funding_all, year, var, fc_ha))
  
  return(list("df_matched_wide" = df_matched_wide, "df_unmatched_wide" = df_unmatched_wide,
              "df_matched_long" = df_matched_long, "df_unmatched_long" = df_unmatched_long))

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

fn_did_att = function(iso, wdpaid, data_pa, alpha, is_m, load_dir, save_dir)
{
  
  if(is_m == TRUE)
  {
    df_long = s3read_using(data.table::fread,
                           object = paste0(load_dir, "/", iso, "/", wdpaid, "/", paste0("matched_long", "_", iso, "_", wdpaid, ext_input)),
                           bucket = "projet-afd-eva-ap",
                           opts = list("region" = "")) %>%
      select(c(region, iso3, wdpaid, group, assetid, status_yr, year_funding_first, year_funding_all, res_m, year, var, fc_ha))
    #select(c(region, country_en, iso3, wdpaid, group, status_yr, year_funding_first, year_funding_all, year, var, fc_ha))
  } else{df_long = s3read_using(data.table::fread,
                                object = paste0(load_dir, "/", iso, "/", wdpaid, "/", paste0("unmatched_long", "_", iso, "_", wdpaid, ext_input)),
                                bucket = "projet-afd-eva-ap",
                                opts = list("region" = "")) %>%
           select(c(region, iso3, wdpaid, group, assetid, status_yr, year_funding_first, year_funding_all, year, res_m, var, fc_ha))
         #select(c(region, country_en, iso3, wdpaid, group, status_yr, year_funding_first, year_funding_all, year, var, fc_ha))
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
  funding.years = df_long %>% 
    filter(group == 2) %>% 
    slice(1)
  funding.years = funding.years$year_funding_first
  #funding.years = as.numeric(unlist(strsplit(funding.years$year_funding_all, split = ",")))
  
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
  
  ##Area of the PA
  wdpa_id = wdpaid #Need to give a name to wdpaid (function argument) different from the varaible in the dataset (wdpaid)
  area_ha = data_pa[data_pa$wdpaid == wdpa_id,]$area_km2*100

    
  #Then modify the dataframe before DiD computations
  ## Set treatment year = 0 for controls (necessary for did package to consider "never treated" units)
  ## Compute deforestation relative to 2000 forest cover (outcome where TE is computed)
  df_did = df_long %>%
    mutate(treatment_year = case_when(group == 1 ~0,
                                      group == 2 ~status_yr), #Set treatment year to 0 for control units (required by did::att_gt)
           .after = status_yr) %>%
    group_by(assetid) %>%
    mutate(FL_2000_cum = (fc_ha-fc_ha[year == 2000])/fc_ha[year == 2000]*100,
           fc_2000 = fc_ha[year == 2000]) %>%
    ungroup()
  
  ##Average forest cover in 2000 in a pixel, and average share of forest cover in a pixel
  fc_2000_avg = mean(df_did[df_did$group == 2,]$fc_2000, na.rm = TRUE)
  per_fc_2000_avg = min(fc_2000_avg/res_ha, 1) #Take the min as in some cases, reported forest cover is higher than pixel area
    
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
      ## att_pa : the total avoided deforestation is the avoided deforestation in ha in a given pixel, multiplied by the number of pixel in the PA covered by forest. 
      ## the latter is the total surface of PA multiplied by the average forest cover share in the treated pixels matched. 
      ## att_per : avoided deforestation in terms of 2000 forest cover. The area of forest cover in 2000 in a pixel is the area of a pixel multiplied by the average share of forest in a pixel
      mutate(att_pa = att_pix*(area_ha*per_fc_2000_avg/res_ha),
             att_per = att_pix/(res_ha*per_fc_2000_avg)*100) %>%
      #Compute time relative to treatment year
      mutate(time = year - treatment_year,
             .before = year) %>%
      #Compute confidence intervals
    mutate(cband_lower_pix = round(att_pix-c*se, 4),
           cband_upper_pix = round(att_pix+c*se, 4),
           cband_lower_pa = cband_lower_pix*area_ha/res_ha,
           cband_upper_pa = cband_upper_pix*area_ha/res_ha,
           cband_lower_per = cband_lower_pix/res_ha*100,
           cband_upper_per = cband_upper_pix/res_ha*100,
           sig = sign(cband_lower_pix) == sign(cband_upper_pix)) %>%
      #Rename confidence interval variables to indicate its level from alpha argument
      rename_with(.cols = c(cband_lower_pix, cband_lower_pa, cband_lower_per, cband_upper_pix, cband_upper_pa, cband_upper_per, sig),
                  .fn = \(x) paste0(x, "_", gsub("0.", "", 1-alpha))) %>%
      #Add region, iso3 and wdpaid
      mutate(region = region.name,
             iso3 = country.iso,
             wdpaid = wdpaid,
             .before = "treatment_year")
    
    ###Plot results
    # fig_fc_att = did::ggdid(fc_attgt,
    #                         grtitle = NULL,
    #                         theming = TRUE) %>%
    #   + labs(title = "Average treatment effect on the treated (ATT) of the conservation program",
    #          subtitle = paste("WDPA ID", wdpaid, "in", country.iso, "implemented in", treatment.year),
    #          caption = "Treatment effect is interpreted as the deforestation avoided thanks to the conservation program, hectare (ha).\nA negative effect means the conservation program has caused higher deforestation.",
    #          y = "Avoided deforestation (ha)") %>%
    #   + theme(
    #     axis.text.x = element_text(angle = -20, hjust = 0.5, vjust = 0.5),
    #     axis.text=element_text(size=11, color = "black"),
    #     axis.title=element_text(size=14, color = "black", face = "plain"),
    #     
    #     plot.caption = element_text(hjust = 0),
    #     plot.title = element_text(size=16, color = "black", face = "plain", hjust = 0),
    #     plot.subtitle = element_text(size=12, color = "black", face = "plain", hjust = 0),
    #     
    #     strip.text = element_blank(),
    #     
    #     #legend.position = "bottom",
    #     legend.title = element_blank(),
    #     legend.text=element_text(size=10),
    #     #legend.spacing.x = unit(1.0, 'cm'),
    #     legend.spacing.y = unit(0.75, 'cm'),
    #     legend.key.size = unit(2, 'line'),
    #     
    #     panel.grid.major.x = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
    #     panel.grid.minor.x = element_line(color = 'grey80', linewidth = 0.2, linetype = 2),
    #     panel.grid.major.y = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
    #     panel.grid.minor.y = element_line(color = 'grey80', linewidth = 0.2, linetype = 2),
    #     
    #   )

    ## ATT : avoided deforestation at pixel level (in ha)
    fig_att_pix = ggplot(data = df_fc_attgt,
                         aes(x = time, y = att_pix)) %>%
      + geom_line(color = "#08519C") %>%
      + geom_point(color = "#08519C") %>%
      + geom_ribbon(aes(ymin = cband_lower_pix_95, ymax = cband_upper_pix_95),
                    alpha=0.1, fill = "#FB6A4A", color = "black", linetype = "dotted") %>%
      + labs(title = ifelse(is_m == TRUE, 
                            yes = "Average treatment effect on the treated (ATT) of the conservation program (matched)",
                            no = "Average treatment effect on the treated (ATT) of the conservation program (unmatched)"),
             subtitle = paste("WDPA ID", wdpaid, "in", country.iso, "implemented in", treatment.year),
             caption = "Treatment effect is interpreted as the deforestation avoided at pixel level in hectare, due to the conservation program.\nA negative effect means the conservation program has caused higher deforestation.",
             y = "Avoided deforestation in a pixel (ha)",
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
      + geom_ribbon(aes(ymin = cband_lower_per_95, ymax = cband_upper_per_95),
                    alpha=0.1, fill = "#FB6A4A", color = "black", linetype = "dotted") %>%
      + labs(title = ifelse(is_m == TRUE, 
                            yes = "Average treatment effect on the treated (ATT) of the conservation program (matched)",
                            no = "Average treatment effect on the treated (ATT) of the conservation program (unmatched)"),
             subtitle = paste("WDPA ID", wdpaid, "in", country.iso, "implemented in", treatment.year),
             caption = "Treatment effect is interpreted like the deforestation avoided as a share of 2000 forest cover.\nA negative effect means the conservation program has caused higher deforestation.",
             y = "Avoided deforestation relative to 2000 f.c. (%)",
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
      + geom_ribbon(aes(ymin = cband_lower_pa_95, ymax = cband_upper_pa_95),
                    alpha=0.1, fill = "#FB6A4A", color = "black", linetype = "dotted") %>%
      + labs(title = ifelse(is_m == TRUE, 
                            yes = "Average treatment effect on the treated (ATT) of the conservation program (matched)",
                            no = "Average treatment effect on the treated (ATT) of the conservation program (unmatched)"),
             subtitle = paste("WDPA ID", wdpaid, "in", country.iso, "implemented in", treatment.year),
             caption = "Treatment effect is interpreted as the total deforestation avoided in the PA, in hectare (ha).\nThis measure is an extrapolation of avoided deforestation at pixel level, knowing the surface of the PA and the average share of forest in a pixel in 2000.\nA negative effect means the conservation program has caused higher deforestation.",
             y = "Total avoided deforestation (ha)",
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
    
    
      
    
    
    ##For deforestation
    ### ATT computation
    # fl_attgt = did::att_gt(yname = "FL_2000_cum",
    #                        gname = "treatment_year",
    #                        idname = "assetid",
    #                        tname = "year",
    #                        control_group = "nevertreated", #Thsi corresponds to control pixels as defined in the matching , with treatment year set to 0
    #                        xformla = ~1,
    #                        alp = alpha, #For 95% confidence interval
    #                        allow_unbalanced_panel = TRUE, #Ensure no unit is dropped, though every pixel should have data for all years in the period
    #                        bstrap=TRUE, #Compute bootstrap CI
    #                        biters = 1000, #The number of bootstrap iteration, 1000 is default
    #                        cband = TRUE, #Compute CI
    #                        clustervars = NULL, #No clustering seems relevant to me 
    #                        base_period = "varying",
    #                        data = df_did,
    #                        print_details = F)
    # 
    # 
    # ### Report results in a dataframe
    # ### confidence intervals are computed from bootstrap standard errors after a coefficient is applied.
    # ### This computation takes the one from did:::summary.MP function, line 15 and 16.
    # df_fl_attgt = data.frame("treatment_year" = fl_attgt$group,
    #                          "year" = fl_attgt$t,
    #                          "att" = fl_attgt$att,
    #                          "c" = fl_attgt$c,
    #                          "se" = fl_attgt$se,
    #                          "n" = fl_attgt$n) %>%
    #   mutate(cband_lower = round(att-c*se, 4),
    #          cband_upper = round(att+c*se, 4),
    #          sig = sign(cband_lower) == sign(cband_upper)) %>%
    #   rename_with(.cols = c(cband_lower, cband_upper, sig),
    #               .fn = \(x) paste0(x, "_", gsub("0.", "", 1-alpha))) %>%
    #   #Compute time relative to treatment year
    #   mutate(time = year - treatment_year,
    #          .before = year) %>%
    #   mutate(region = region.name,
    #          iso3 = country.iso,
    #          wdpaid = wdpaid,
    #          .before = "treatment_year")
    # 
    # fig_fl_att = did::ggdid(fl_attgt,
    #                   grtitle = NULL,
    #                   theming = TRUE) %>%
    #   + labs(title = "Average treatment effect on the treated (ATT) of the conservation program",
    #          subtitle = paste("WDPA ID", wdpaid, "in", country.iso, "implemented in", treatment.year),
    #          caption = "Treatment effect is interpreted as the share of 2000 forest cover not deforested due to the conservation program.\nA negative effect means the conservation program is associated with higher deforestation.",
    #          y = "Avoided deforestation rel. to 2000 (%)") %>%
    #   + theme(
    #     axis.text.x = element_text(angle = -20, hjust = 0.5, vjust = 0.5),
    #     axis.text=element_text(size=11, color = "black"),
    #     axis.title=element_text(size=14, color = "black", face = "plain"),
    #     
    #     plot.caption = element_text(hjust = 0),
    #     plot.title = element_text(size=16, color = "black", face = "plain", hjust = 0),
    #     plot.subtitle = element_text(size=12, color = "black", face = "plain", hjust = 0),
    #     
    #     strip.text = element_blank(),
    #     
    #     #legend.position = "bottom",
    #     legend.title = element_blank(),
    #     legend.text=element_text(size=10),
    #     #legend.spacing.x = unit(1.0, 'cm'),
    #     legend.spacing.y = unit(0.75, 'cm'),
    #     legend.key.size = unit(2, 'line'),
    #     
    #     panel.grid.major.x = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
    #     panel.grid.minor.x = element_line(color = 'grey80', linewidth = 0.2, linetype = 2),
    #     panel.grid.major.y = element_line(color = 'grey80', linewidth = 0.3, linetype = 1),
    #     panel.grid.minor.y = element_line(color = 'grey80', linewidth = 0.2, linetype = 2),
    #     
    #   )
    # 
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
    # ggsave(paste(tmp, paste0("fig_fl_att_", iso, "_", wdpaid, ".png"), sep = "/"),
    #        plot = fig_fl_att,
    #        device = "png",
    #        height = 6, width = 9)
    
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
    #output = list("df_fc_att" = df_fc_attgt, "df_fl_att"  = df_fl_attgt)
    output = list("df_fc_att" = df_fc_attgt)
    
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
  
  
  
  
  
  
  
