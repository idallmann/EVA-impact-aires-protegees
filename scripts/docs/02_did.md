# Difference-in-difference

In this script are performed the last steps of the analysis. Using the datasets of matched treated and control units, it is possible to compute the effect of the conservation for each protected area. Then these results can be aggregated at country and region level. Some secondary metrics are also computed : the annual deforestation rates faced by treated and control units, before and after treatment (à la Wolf et al. 2021); the total and average forest loss across groups of protected areas on the period considered, compared to the value in control units, before and after matching.

The process consists of the following steps.

1.  For a given country, load the list of protected areas whose observational units (treated pixels composing this area) have been matched to control units.

2.  For each protected area in this list :

    1.  Compute annual deforestation rates : before and after treatment for treated units, on the whole period for control units.

    2.  Compute treatment effect from matched treated and control pixels. Note that two functions exist depending on the portfolio analyzed : for AFD supported protected areas, funding-related information are added to the analysis (e.g year of funding). For the others, a general script is used.

    3.  Compute treatment effect for unmatched treated and control pixels. This can be useful to assess the bias induced by the selection into treatment. In other words, the bias due to not using matching. Indeed protected areas tend to be located in region less prone to deforestation (e.g further away from cities or roads). Simply using non-protected areas as a counterfactual, without matching, would then overestimate the effect of the conservation program. Note that this step can be time and computationally intensive, because the number of unmatched units is higher than the number of matched ones. It is usually skipped.

    4.  Plot the total area deforested on the period considered, in the protected area and its counterfactual, before and after matching. This is useful to illustrate the bias described above. Note that ideally the deforestation estimated in the protected area before and after matching should be the same. If note, the matched treated units are not representative of the overall protected area and the a local treatment effect will be estimated.

3.  The treatment effects, annual deforestation rates and estimations of total deforested area computed for each protected areas are gathered in specific datasets. This makes it possible to compute metrics aggregated at country and region level. These datastes are saved to the storage.

4.  If relevant, results of the analysis at protected area level are aggregated at country and region level.

5.  Finally, figures and tables are created to display the results.

## Initial settings

Configuring the Rmarkdown.

#```{r setup, include=FALSE, eval = FALSE}
#knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())
#```

Downloading and importing relevant packages.


```r
#Install some libraries
install.packages(c("tictoc", "fixest", "cobalt", "future", "progressr", "did", "latex2exp", "janitor", "stringi"))

# Load Libraries
library(dplyr)
library(stringi)
library(tictoc) #For timing
library(xtable)
library(tidyr)
library(stringr)
library(ggplot2) # For plotting
library(RColorBrewer)
library(ggrepel)
library(aws.s3)
library(fixest) #For estimating the models
library(did)
library(latex2exp)
library(janitor)
```

To keep this document concise, each step calls a function defined in a R script. Interested reader can delve deeper into the data processing by looking at this script. The following chunck load the functions in the workspace.


```r
#Import functions
source("scripts/functions/02_fns_did.R")
```

## Load datasets and define critical parameters


```r
#Define working directories
##Temporary directory
tmp_did = paste(tempdir(), "did", sep = "/")
##Loading and saving directories on the storage. This is either define on today's date, or by a user-defined date.
#load_dir = paste("impact_analysis/matching", Sys.Date(), sep = "/")
load_dir = paste("impact_analysis/matching", "2023-09-21", sep = "/")
#save_dir = paste("impact_analysis/did", Sys.Date(), sep = "/")
save_dir = paste("impact_analysis/did", "2023-09-21", sep = "/")

## Dataset specific to the PAs portfolio to analyze. Only one is selected depending on the analysis one wants to perform. 

## AFD portfolio
# data_pa =
#   #fread("data_tidy/BDD_PA_AFD_ie.csv" , encoding = "UTF-8")
#   aws.s3::s3read_using(
#   FUN = data.table::fread,
#   encoding = "UTF-8",
#   object = "data_tidy/BDD_PA_AFD_ie.csv",
#   bucket = "projet-afd-eva-ap",
#   opts = list("region" = "")) %>%
#   #Sangha trinational (555547988) created in 2012 actually gathers three former PAs
#   #in CAF (31458), CMR (1245) and COG (72332) implemented in
#   #1990, 2001 and 1993 respectively.
#   # Evaluating the trinational PA is not relevant here : our method relies on pre-treatment obervsations (for matching and DiD) and the outcome is likely to be affected by the initial PAs. On the other hand, evaluating the three earlier PAs might be irrelevant for us : are they funded by the AFD ?? In a first approach, the trinational is removed.
#   filter(is.na(wdpaid) == TRUE | wdpaid != 555547988)

##FAPBM
data_fapbm =
  #fread("data_tidy/BDD_PA_AFD_ie.csv" , encoding = "UTF-8")
  aws.s3::s3read_using(
  FUN = data.table::fread,
  encoding = "UTF-8",
  object = "data_tidy/BDD_PA_FAPBM.csv",
  bucket = "projet-afd-eva-ap",
  opts = list("region" = ""))

##Madagascar (all PAs)
data_pa =
  #fread("data_tidy/BDD_PA_AFD_ie.csv" , encoding = "UTF-8")
  aws.s3::s3read_using(
  FUN = data.table::fread,
  encoding = "UTF-8",
  object = "data_tidy/BDD_PA_MDG.csv",
  bucket = "projet-afd-eva-ap",
  opts = list("region" = ""))

# The list of countries (ISO3 codes) to analyze. This can be define manually or from the the dataset loaded.

# list_iso = data_pa %>%
#   filter(region == "Africa") %>%
#   filter(!(iso3 %in% c("ZZ")))
# list_iso = unique(list_iso$iso3)

list_iso = "MDG"

## Information on funding from AFD internal datasets, on AFD funded projects related to protected areas.
data_fund = 
  #fread("data_tidy/BDD_PA_AFD_fund.csv")
  aws.s3::s3read_using(
  FUN = data.table::fread,
  encoding = "UTF-8",
  # Mettre les options de FUN ici
  object = "data_tidy/BDD_PA_AFD_fund.csv",
  bucket = "projet-afd-eva-ap",
  opts = list("region" = ""))
data_fund_nodupl = data_fund %>%
  group_by(id_projet) %>%
  slice(1) %>%
  ungroup() %>%
  dplyr::select(-c("mt_part_cofinancier_prevu_euro", "cofinanciers_siop")) %>%
  mutate(kfw = grepl("kfw", cofinanciers, ignore.case = TRUE),
         ffem = grepl("ffem", cofinanciers, ignore.case = TRUE))

## List of projects related to protected areas in AFD, reported by technical departments
data_pa_report = 
  #read_delim("data_raw/BDD_PA_AFD.csv", delim = ";")
  s3read_using(readr::read_delim,
                delim = ";",
               show_col_types = FALSE,
                object = "data_raw/BDD_PA_AFD.csv",
                bucket = "projet-afd-eva-ap",
                opts = list("region" = "")) %>%
  mutate(key = paste(id_projet, nom_ap, wdpaid, sep = "_")) %>%
  group_by(key) %>%
  slice(1)

#Specify the period of study to create the mapme.bidiversity portfolio
## Start year
yr_first = 2000
## End year
yr_last = 2021

#Specify the margin of error to define confidence interval (0.05 corresponds 95% confidence interval).
alpha = 0.05
```

## Computing treatment effects at protected area level


```r
#For each country in the list, the different steps of the processing are performed
count_i = 0 #Initialize counter
max_i = length(list_iso) #Max value of the counter
tic = tic() #Start timer
#Create empty dataframes that will store the treatment effects computed for each protected area in the portfolio. 
##Initialize a dataframe to store annual deforestation rate for each PA, à la Wolf et al. 2021
df_fl_annual_wolf = data.frame()
##Initialize a dataframe to store treatment effects of all PA analyzed
df_fc_att = data.frame() #Effect on forest cover for matched units
##df_fc_att_unm = data.frame() #Effect on forest cover for unmacthed units
df_fl_att = data.frame() #Effect on deforestation relative to 2000
##Initialize a dataframe to store forest loss for visual evidence before/after matching
df_plot_forest_loss = data.frame()

for (i in list_iso)
{
  #Update counter and show progress
  count_i = count_i+1
  print(paste0(i, " : country ", count_i, "/", max_i))
  
  #Load the matching frame
  print("--Loading the list of PAs in the country considered")
  output_pa_i = fn_did_list_pa(iso = i, load_dir = load_dir)
  if(output_pa_i$is_ok == FALSE) {next} else list_pa_i = output_pa_i$list_pa
  
  count_j = 0
  max_j = length(list_pa_i)
  
  for(j in list_pa_i)
  {
    
    count_j = count_j+1
    print(paste0("WDPA ID ", j, " : ", count_j, "/", max_j))
    
    #Compute annual deforestation rates in treated and control matched areas, à la Wolf et al. 2021.
    print("--Compute average deforestation rates à la Wolf et al. 2021")
    output_wolf_m_j = fn_fl_wolf(iso = i, 
                                wdpaid = j, 
                                alpha = alpha, 
                                load_dir = load_dir,
                                ext_input = ".csv")
    
    if(output_wolf_m_j$is_ok == FALSE) 
      {
      next
      } else df_fl_wolf_m_j = output_wolf_m_j$df_fl_wolf_m_j
    df_fl_annual_wolf = rbind(df_fl_annual_wolf, df_fl_wolf_m_j)
    
    #Compute treatment effects
    print("--Compute treatment effects")
    print("----For matched units")
    
    #For AFD projects (funding info)
    # output_att_m_j = fn_did_att_afd(iso = i, wdpaid = j, 
    #                       is_m = TRUE,
    #                   data_pa = data_pa,
    #                   data_fund = data_fund_nodupl,
    #                   data_report = data_pa_report,
    #                   alpha = alpha,
    #                   load_dir = load_dir,
    #                   ext_input = ".csv",
    #                   save_dir = save_dir)
    
    #For PAs in general (no funding info)
      output_att_m_j = fn_did_att_general(iso = i, wdpaid = j, 
                        is_m = TRUE,
                    data_pa = data_pa,
                    alpha = alpha,
                    load_dir = load_dir,
                    ext_input = ".csv",
                    save_dir = save_dir)
    
    if(output_att_m_j$is_ok == FALSE) {next} else df_att_m_j = output_att_m_j
    
    df_fc_att = rbind(df_fc_att, df_att_m_j$df_fc_att)
    df_fl_att = rbind(df_fl_att, df_att_m_j$df_fl_att)
        
    print("----For unmatched units")
    
    #For AFD PAs (funding info known)
    # df_att_unm_j = fn_did_att_afd(iso = i, wdpaid = j, 
    #                       is_m = FALSE,
    #                   data_pa = data_pa,
    #                   alpha = 0.05,
    #                   load_dir = load_dir,
    #                   ext_input = ".csv",
    #                   save_dir = save_dir)
    # df_fc_att_unm = rbind(df_fc_att_unm, df_att_unm_j$df_fc_att)
    # df_fl_att_unm = rbind(df_fl_att, df_att_unm_j$df_fl_att)
    
    #For general PAs (funding info unknown)
    # df_att_unm_j = fn_did_att_general(iso = i, wdpaid = j, 
    #                       is_m = FALSE,
    #                   data_pa = data_pa,
    #                   alpha = 0.05,
    #                   load_dir = load_dir,
    #                   ext_input = ".csv",
    #                   save_dir = save_dir)    
    # df_fc_att_unm = rbind(df_fc_att_unm, df_att_unm_j$df_fc_att)
    # df_fl_att_unm = rbind(df_fl_att, df_att_unm_j$df_fl_att)
    
    #Plot visual evidence before-after matching
    print("--Plot visual evidence before-after matching")
    df_plot_forest_loss_j = fn_plot_forest_loss(iso = i, 
                                                wdpaid = j, 
                                                alpha = alpha,
                                                data_pa = data_pa, 
                                                load_dir = load_dir, 
                                                ext_input = ".csv", 
                                                save_dir = save_dir)
    df_plot_forest_loss = rbind(df_plot_forest_loss, df_plot_forest_loss_j)

  }  

  
  
}

#Finally save the treatment effects computed for every protected areas analyzed
## Treatment effect expressed in forest cover loss avoided (ha)
aws.s3::s3write_using(
FUN = data.table::fwrite,
df_fc_att,
# Mettre les options de FUN ici
object = paste(save_dir, "data_fc_att.csv", sep = "/"),
bucket = "projet-afd-eva-ap",
opts = list("region" = ""))
## Treatment effect expressed in change of deforestation rate (percentage points)
aws.s3::s3write_using(
FUN = data.table::fwrite,
df_fl_att,
# Mettre les options de FUN ici bucket = , iso, wdpaid, sep = "/")
object = paste(save_dir, "data_fl_att.csv", sep = "/"), 
bucket = "projet-afd-eva-ap",
opts = list("region" = ""))

toc = toc()
```

## Compute aggregated metrics at country and region level

The aggregation of results at country and regional level is not necessarily relevant when the number of protected areas by country or region is relatively small and results heterogenous across protected areas. Instead, a figure displaying the results for all protected areas in the sample might be mor relevant. See next section.

### Aggregation of treatment effects


```r
#Treatment effects are aggregated by region and country
#For total avoided deforestation in ha, treatment effects are summed and so are the confidence intervals (CI).
## If the CI of a treatment effect is NA, then the CI for total treatment effects is NA also. Otherwise CI will be downward biased.
#For avoided deforestation in % of 2000 forest cover, treatment effects are averaged and so are CI. CI being NA is less a problem here as we use a mean, not a sum

avg_att_fc_ctry = df_fc_att %>%
  group_by(region, iso3, time) %>%
  summarize(n_obs = n(),
            att_per_mean = mean(att_per, na.rm = TRUE),
            cband_lower_per = mean(cband_lower_per, na.rm = TRUE),
            cband_upper_per = mean(cband_upper_per, na.rm = TRUE),
            att_pa_tot = sum(att_pa, na.rm = TRUE),
            cband_lower_pa = sum(cband_lower_pa, na.rm = FALSE),
            cband_upper_pa = sum(cband_upper_pa, na.rm = FALSE))

avg_att_fc_region = df_fc_att %>%
  group_by(region, time) %>%
  summarize(n_obs = n(),
            att_per_mean = mean(att_per, na.rm = TRUE),
            cband_lower_per = mean(cband_lower_per, na.rm = TRUE),
            cband_upper_per = mean(cband_upper_per, na.rm = TRUE),
            att_pa_tot = sum(att_pa, na.rm = TRUE),
            cband_lower_pa = sum(cband_lower_pa, na.rm = FALSE),
            cband_upper_pa = sum(cband_upper_pa, na.rm = FALSE))

avg_att_fl_ctry = df_fl_att %>%
  group_by(region, iso3, time) %>%
  summarize(n_obs = n(),
            att_mean = mean(att, na.rm = TRUE),
            cband_lower = mean(cband_lower, na.rm = TRUE),
            cband_upper = mean(cband_upper, na.rm = TRUE))

avg_att_fl_region = df_fl_att %>%
  group_by(region, time) %>%
  summarize(n_obs = n(),
            att_mean = mean(att, na.rm = TRUE),
            cband_lower = mean(cband_lower, na.rm = TRUE),
            cband_upper = mean(cband_upper, na.rm = TRUE))
```

### Aggregation of annual deforestation rates


```r
#Aggregate at region level
## Compute for each region, for treated units, average annual deforestation rate before treatment, after treatment and on the full period. Confidence intervals are also computed.
avg_fl_annual_wolf_region = df_fl_annual_wolf %>%
  filter(group == "Treated") %>%
  group_by(region) %>%
  summarize(avg_FL_annual_wolf_pre = mean(avgFL_annual_wolf_pre, na.rm = TRUE),
            avg_FL_annual_wolf_pre_ci_up = mean(avgFL_annual_wolf_pre, na.rm = TRUE) +  qt((1-alpha)/2,df=21-1)*sd(avgFL_annual_wolf_pre, na.rm = TRUE)/sqrt(21),
            avg_FL_annual_wolf_pre_ci_lower = mean(avgFL_annual_wolf_pre, na.rm = TRUE) -  qt((1-alpha)/2,df=21-1)*sd(avgFL_annual_wolf_pre, na.rm = TRUE)/sqrt(21),
            med_FL_annual_wolf_pre_ci_up = median(avgFL_annual_wolf_pre, na.rm = TRUE),
            avg_FL_annual_wolf_tot = mean(avgFL_annual_wolf_tot, na.rm = TRUE),
            avg_FL_annual_wolf_tot_ci_up = mean(avgFL_annual_wolf_tot, na.rm = TRUE) +  qt((1-alpha)/2,df=21-1)*sd(avgFL_annual_wolf_tot, na.rm = TRUE)/sqrt(21),
            avg_FL_annual_wolf_tot_ci_lower = mean(avgFL_annual_wolf_tot, na.rm = TRUE) -  qt((1-alpha)/2,df=21-1)*sd(avgFL_annual_wolf_tot, na.rm = TRUE)/sqrt(21),
            med_FL_annual_wolf_tot_ci_up = median(avgFL_annual_wolf_tot, na.rm = TRUE)) 

#Aggregate at country level
## Same that at region level.
avg_fl_annual_wolf_country = df_fl_annual_wolf %>%
  filter(group == "Treated") %>%
  group_by(iso3) %>%
  summarize(avg_FL_annual_wolf_pre = mean(avgFL_annual_wolf_pre, na.rm = TRUE),
            avg_FL_annual_wolf_pre_ci_up = mean(avgFL_annual_wolf_pre, na.rm = TRUE) +  qt((1-alpha)/2,df=21-1)*sd(avgFL_annual_wolf_pre, na.rm = TRUE)/sqrt(21),
            avg_FL_annual_wolf_pre_ci_lower = mean(avgFL_annual_wolf_pre, na.rm = TRUE) -  qt((1-alpha)/2,df=21-1)*sd(avgFL_annual_wolf_pre, na.rm = TRUE)/sqrt(21),
            med_FL_annual_wolf_pre_ci_up = median(avgFL_annual_wolf_pre, na.rm = TRUE),
            avg_FL_annual_wolf_tot = mean(avgFL_annual_wolf_tot, na.rm = TRUE),
            avg_FL_annual_wolf_tot_ci_up = mean(avgFL_annual_wolf_tot, na.rm = TRUE) +  qt((1-alpha)/2,df=21-1)*sd(avgFL_annual_wolf_tot, na.rm = TRUE)/sqrt(21),
            avg_FL_annual_wolf_tot_ci_lower = mean(avgFL_annual_wolf_tot, na.rm = TRUE) -  qt((1-alpha)/2,df=21-1)*sd(avgFL_annual_wolf_tot, na.rm = TRUE)/sqrt(21),
            med_FL_annual_wolf_tot_ci_up = median(avgFL_annual_wolf_tot, na.rm = TRUE)) %>%
  ungroup() 
```

### Average and total forest loss on the period, at country and region level

For each protected area, the total deforestation is estimated on the period in the protected area and a counterfactual, before and after matching. This metric can be aggregated at country and region level : the sum of total deforestation or its average. This can be computed for all protected areas in the sample, a specific subset of protected areas, but also at country or region level.


```r
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
## for each country
df_plot_forest_loss_agg_ctry = df_plot_forest_loss %>%
  group_by(region, iso3, country_en, matched, group, year) %>%
  summarize(tot_fc_rel00_ha = sum(fc_tot_rel00_ha, na.rm = TRUE),
            tot_fc_rel00_ha_ci_lower = sum(fc_tot_rel00_ha_ci_lower, na.rm = FALSE),
            tot_fc_rel00_ha_ci_upper = sum(fc_tot_rel00_ha_ci_upper, na.rm = FALSE),
              avg_fc_rel00_ha = mean(fc_tot_rel00_ha, na.rm = TRUE),
              avg_fc_rel00_ha_ci_lower = mean(fc_tot_rel00_ha_ci_lower, na.rm = TRUE),
              avg_fc_rel00_ha_ci_upper = mean(fc_tot_rel00_ha_ci_upper, na.rm = TRUE)) %>%
  ungroup() 
## for each region
df_plot_forest_loss_agg_all = df_plot_forest_loss %>%
  group_by(region, matched, group, year) %>%
  summarize(tot_fc_rel00_ha = sum(fc_tot_rel00_ha, na.rm = TRUE),
            tot_fc_rel00_ha_ci_lower = sum(fc_tot_rel00_ha_ci_lower, na.rm = FALSE),
            tot_fc_rel00_ha_ci_upper = sum(fc_tot_rel00_ha_ci_upper, na.rm = FALSE),
              avg_fc_rel00_ha = mean(fc_tot_rel00_ha, na.rm = TRUE),
              avg_fc_rel00_ha_ci_lower = mean(fc_tot_rel00_ha_ci_lower, na.rm = TRUE),
              avg_fc_rel00_ha_ci_upper = mean(fc_tot_rel00_ha_ci_upper, na.rm = TRUE)) %>%
  ungroup() 

## For a specific subset of protected areas (here, protected areas funded by the FAPBM for instance)
df_plot_forest_loss_agg_fapbm = df_plot_forest_loss %>%
  filter(wdpaid %in% unique(data_fapbm$wdpaid)) %>%
  group_by(matched, group, year) %>%
  summarize(tot_fc_rel00_ha = sum(fc_tot_rel00_ha, na.rm = TRUE),
            tot_fc_rel00_ha_ci_lower = sum(fc_tot_rel00_ha_ci_lower, na.rm = TRUE),
            tot_fc_rel00_ha_ci_upper = sum(fc_tot_rel00_ha_ci_upper, na.rm = TRUE),
              avg_fc_rel00_ha = mean(fc_tot_rel00_ha, na.rm = TRUE),
              avg_fc_rel00_ha_ci_lower = mean(fc_tot_rel00_ha_ci_lower, na.rm = TRUE),
              avg_fc_rel00_ha_ci_upper = mean(fc_tot_rel00_ha_ci_upper, na.rm = TRUE)) %>%
  ungroup()

df_plot_forest_loss_agg_nofapbm = df_plot_forest_loss %>%
  filter(!(wdpaid %in% unique(data_fapbm$wdpaid))) %>%
  group_by(matched, group, year) %>%
  summarize(tot_fc_rel00_ha = sum(fc_tot_rel00_ha, na.rm = TRUE),
            tot_fc_rel00_ha_ci_lower = sum(fc_tot_rel00_ha_ci_lower, na.rm = TRUE),
            tot_fc_rel00_ha_ci_upper = sum(fc_tot_rel00_ha_ci_upper, na.rm = TRUE),
              avg_fc_rel00_ha = mean(fc_tot_rel00_ha, na.rm = TRUE),
              avg_fc_rel00_ha_ci_lower = mean(fc_tot_rel00_ha_ci_lower, na.rm = TRUE),
              avg_fc_rel00_ha_ci_upper = mean(fc_tot_rel00_ha_ci_upper, na.rm = TRUE)) %>%
  ungroup()


# Plot the results : total and average deforestation
## The maximum year to consider. The period where total deforestation is computed is then 2000-year.max.
year.max = 2021
## Titles to display
fct.labs <- c("Before Matching", "After Matching")
names(fct.labs) <- c(FALSE, TRUE)
## The number of protected areas (all and in a given subsample, for instance)
n_pa  = length(unique(df_plot_forest_loss$wdpaid))
  fct.labs <- c("Before Matching", "After Matching")
  names(fct.labs) <- c(FALSE, TRUE)
#n_fapbm  = length(unique(data_pa_fapbm[data_pa_fapbm$wdpaid %in% unique(df_plot_forest_loss$wdpaid),]$wdpaid))
  n_nofapbm  = length(unique(data_pa[data_pa$wdpaid %in% unique(df_plot_forest_loss$wdpaid) & !(data_pa$wdpaid %in% unique(data_fapbm$wdpaid)),]$wdpaid))
## The total surface of protected areas (all and in a given subsample, for instance)
area_tot_ha = sum(data_pa[data_pa$wdpaid %in% unique(df_plot_forest_loss$wdpaid),]$area_km2, na.rm = TRUE) *100
#area_fapbm_ha = sum(data_pa_fapbm[data_pa_fapbm$wdpaid %in% unique(df_plot_forest_loss$wdpaid),]$area_km2, na.rm = TRUE) *100
area_nofapbm_ha = sum(data_pa[data_pa$wdpaid %in% unique(df_plot_forest_loss$wdpaid) & !(data_pa$wdpaid %in% unique(data_fapbm$wdpaid)),]$area_km2, na.rm = TRUE) *100

## Define the figures
### For a given subsample
#### Total deforestation
fig_forest_loss_agg_tot_fapbm = ggplot(data = filter(df_plot_forest_loss_agg_fapbm, year == year.max),
               aes(y = abs(tot_fc_rel00_ha), fill = as.factor(group), x = group)) %>%
    + geom_bar(position =  position_dodge(width = 0.8), stat = "identity", show.legend = FALSE) %>%
    + geom_errorbar(aes(ymax=abs(tot_fc_rel00_ha_ci_upper), ymin=abs(tot_fc_rel00_ha_ci_lower)), width=0.3, colour="grey70", alpha=0.9, size=1) %>%
    + geom_label(aes(label = format(round(abs(tot_fc_rel00_ha), 0), big.mark = ","), y = 0),
                 vjust = -0.5,
                 color = "black",
                 show.legend = FALSE) %>%
    + scale_fill_brewer(name = "Group", palette = "Blues") %>%
    + labs(x = "",
           y = "Forest cover loss (ha)",
           title = paste("Total area deforested between 2000 and", year.max),
           subtitle = paste0("Sample : FAPBM protected areas in the analysis (" , n_fapbm, " areas covering ", format(area_fapbm_ha, big.mark = ","), "ha)"),
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
fig_forest_loss_agg_tot_fapbm

#### Average deforestation
fig_forest_loss_agg_avg_nofapbm = ggplot(data = filter(df_plot_forest_loss_agg_nofapbm, year == year.max),
               aes(y = abs(avg_fc_rel00_ha), fill = as.factor(group), x = group)) %>%
    + geom_bar(position =  position_dodge(width = 0.8), stat = "identity", show.legend = FALSE) %>%
    + geom_errorbar(aes(ymax=abs(avg_fc_rel00_ha_ci_upper), ymin=abs(avg_fc_rel00_ha_ci_lower)), width=0.3, colour="grey70", alpha=0.9, size=1) %>%
    + geom_label(aes(label = format(round(abs(avg_fc_rel00_ha), 0), big.mark = ","), y = 0),
                 vjust = -0.5,
                 color = "black",
                 show.legend = FALSE) %>%
    + scale_fill_brewer(name = "Group", palette = "Blues") %>%
    + labs(x = "",
           y = "Forest cover loss (ha)",
           title = paste("Area deforested in protected areas on average, between 2000 and", year.max),
           subtitle = paste0("Sample : non FAPBM protected areas in the analysis (" , n_nofapbm, " areas covering ", format(area_nofapbm_ha, big.mark = ","), " ha)"),
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
fig_forest_loss_agg_avg_nofapbm
```

Eventually the plots are saved in the remote storage.


```r
##Saving plots
tmp = paste(tempdir(), "fig", sep = "/")

ggsave(paste(tmp, "fig_forest_loss_agg_tot_fapbm.png", sep = "/"),
       plot = fig_forest_loss_agg_tot_fapbm,
       device = "png",
       height = 6, width = 9)

ggsave(paste(tmp, "fig_forest_loss_agg_avg_nofapbm.png", sep = "/"),
       plot = fig_forest_loss_agg_avg_nofapbm,
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
```

## Display treatment effects in figures and tables

The treatment effects computed for each protected areas can be displayed in figures or tables. Again, a function is used for protected areas supported by the AFD to include information on funding (funding year for instance), and an other for non-supported ones.

Note some protected areas can be removed from the display, because the matching is not satisfying or pre-treatment trends are too different between treated and control units (thus the matched control risk to be an irrelevant counterfactual). Also, one might want to highlight some protected areas. For instance when comparing a subset of protected areas in a given country to the others (protected areas in Madagascar supported by the AFD versus the others for instance).


```r
# Define a list of protected areas that will not be displayed
list_wdpa_bad = c(352240, #matching
                  352240, #matching
                  555542728, #pre-treatment parallel trend
                  555548846 #no confidence intervals
                  )
# Define a list of protected areas where a focus is needed
list_wdpa_focus = unique(data_fapbm$wdpaid)

# Get rid of some protected areas in the dataset used to plotting figures and tables
df_fc_att_tidy = df_fc_att %>%
  filter(!wdpaid %in% list_wdpa_bad)
df_fl_att_tidy = df_fl_att %>%
  filter(!wdpaid %in% list_wdpa_bad)

# A function to create figures and tables
##For AFD supported protected areas (funding info)
# fn_plot_att_afd(df_fc_att = df_fc_att_tidy,
#             df_fl_att = df_fl_att_tidy, 
#             alpha = alpha,
#             save_dir = save_dir)
##For other protected areas (no funding info)
fn_plot_att_general(df_fc_att = df_fc_att_tidy,
            df_fl_att = df_fl_att_tidy, 
            list_focus = list_wdpa_focus,
            alpha = alpha,
            save_dir = save_dir)
```

## 