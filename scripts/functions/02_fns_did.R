
#Load the list of PA matched during the matchign process
##INPUTS :
### iso : the ISO code of the country considered
### yr_min : the minimum for treatment year
##OUTPUTS :
### list_pa : a dataframe with the PA matched
fn_did_list_pa = function(iso)
{
  list_pa = s3read_using(data.table::fread,
                    bucket = "projet-afd-eva-ap",
                    object = paste("data_tidy/mapme_bio_data/matching", iso, paste0("list_pa_matched_", iso, ".csv"), sep = "/"),
                    opts = list("region" = "")) 

  return(list_pa)
}


fn_did_load_df_matched_wide = function(iso, wdpaid, ext_output)
{
  df_matched_wide = s3read_using(data.table::fread,
                                 object = paste0("data_tidy/mapme_bio_data/matching", "/", iso, "/", wdpaid, "/", paste0("matched_wide", "_", iso, "_", wdpaid, ext_output)),
                                 bucket = "projet-afd-eva-ap",
                                 opts = list("region" = "")) %>%
    select(c(wdpaid, group, status_yr, starts_with("treecover"), starts_with("treeloss")))
  
  return(df_matched_wide)

}
