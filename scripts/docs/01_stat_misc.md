---
output:
  html_document:
    code_folding: show
---

# Miscellaneous statistics

In this document are performed and plotted statistics for particular needs (analysis of a particular portfolio, some specific group of PAs, etc.)

## Initial settings

Configuring Rmarkdown.

#```{r setup, include=FALSE, eval = FALSE}
#knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())
#```

Installing and importing relevant packages.


```r
install.packages(c("stargazer", "janitor", "questionr", "countrycode", "WDI"))
library(tidyverse)
library(stargazer)
library(dplyr)
library(sf)
library(ggplot2)  
library(ggrepel)
library(RColorBrewer)
library(countrycode)
library(data.table)
#library(readxl)
#library(splitstackshape) 
library(janitor)
library(xtable)
library(questionr)
library(aws.s3)
library(WDI)
library(countrycode)
```

## Importing datasets

A dataset with information for each protected area funded by the AFD, and datasets on aggregated size at country/region/world level and by year. The latter takes into account potential overlap between PAs reported in the World Database on Protected Areas (WDPA).


```r
#Dataset of AFD supported protected area, with one line per protected area
data_stat_nodupl = 
  #fread("data_tidy/BDD_PA_AFD_nofund_nodupl.csv" , encoding = "UTF-8")
  aws.s3::s3read_using(
  FUN = data.table::fread,
  encoding = "UTF-8",
  # Mettre les options de FUN ici
  object = "data_tidy/BDD_PA_AFD_nofund_nodupl.csv",
  bucket = "projet-afd-eva-ap",
  opts = list("region" = ""))

# Portfolio of protected areas supported by the FAPBM
data_pa_fapbm = 
  #fread("data_tidy/BDD_PA_AFD_ie.csv" , encoding = "UTF-8")
  aws.s3::s3read_using(
  FUN = data.table::fread,
  encoding = "UTF-8",
  object = "data_tidy/BDD_PA_FAPBM.csv",
  bucket = "projet-afd-eva-ap",
  opts = list("region" = ""))

#Import WDPA data
## Download and save data
# data_wdpa = wdpa_fetch(x = "global", wait = TRUE, download_dir = "data_raw",
#                        page_wait = 2, verbose = TRUE)
# st_write(wdpa,
#          dsn = "data_raw/wdpa/wdpa_shp_global_raw.gpkg",
#          delete_dsn = TRUE)
## Load data
data_wdpa = 
  #st_read("data_raw/wdpa/wdpa_shp_global_raw.gpkg") %>%
  s3read_using(sf::st_read,
              object = "data_raw/wdpa/wdpa_shp_global_raw.gpkg",
              bucket = "projet-afd-eva-ap",
              opts = list("region" = "")) %>%
  #st_drop_geometry() %>%
  clean_names() %>%
  #Add region, sub-region and country names 
  mutate(region = countrycode(sourcevar = iso3,
                              origin = "iso3c",
                              destination = "un.region.name"),
         sub_region = countrycode(sourcevar = iso3,
                              origin = "iso3c",
                              destination = "un.regionsub.name"),
         country_en = countrycode(sourcevar = iso3,
                              origin = "iso3c",
                              destination = "un.name.en"),
         country_fr = countrycode(sourcevar = iso3,
                              origin = "iso3c",
                              destination = "un.name.fr"),
         .after = "iso3") 

#Subset of WDPA data for Madagascar, with IUCN category description (English and French)
data_wdpa_mdg = data_wdpa %>%
  filter(iso3 == "MDG") %>%
  #Add the description of IUCN from its category
    mutate(iucn_des_fr = case_when(
  !is.na(wdpaid) & iucn_cat == "Ia" ~ "Réserve naturelle intégrale",
  !is.na(wdpaid) & iucn_cat == "Ib" ~ "Zone de nature sauvage",
  !is.na(wdpaid) & iucn_cat == "II" ~ "Parc national", 
  !is.na(wdpaid) & iucn_cat == "III" ~ "Monument naturel",
  !is.na(wdpaid) & iucn_cat == "IV" ~ "Gest. des habitats/espèces",
  !is.na(wdpaid) & iucn_cat == "V" ~ "Paysage protégé",
  !is.na(wdpaid) & iucn_cat == "VI" ~ "Gest. de ress. protégées",
  !is.na(wdpaid) & iucn_cat == "Not Applicable" ~ "Non catégorisée",
  !is.na(wdpaid) & iucn_cat == "Not Reported" ~ "Non catégorisée",
  !is.na(wdpaid) & iucn_cat == "Not Assigned" ~ "Non catégorisée",
  TRUE ~ "Non référencée"), .after = iucn_cat) %>%
      mutate(iucn_des_en = case_when(
  !is.na(wdpaid) & iucn_cat == "Ia" ~ "Strict nature reserve",
  !is.na(wdpaid) & iucn_cat == "Ib" ~ "Wilderness area",
  !is.na(wdpaid) & iucn_cat == "II" ~ "National park",
  !is.na(wdpaid) & iucn_cat == "III" ~ "Natural monument or feature",
  !is.na(wdpaid) & iucn_cat == "IV" ~ "Habitat or species management area",
  !is.na(wdpaid) & iucn_cat == "V" ~ "Protected landscape or seascape",
  !is.na(wdpaid) & iucn_cat == "VI" ~ "Protected area with sust. use of nat. res.",
  !is.na(wdpaid) & iucn_cat == "Not Applicable" ~ "Not categorized",
  !is.na(wdpaid) & iucn_cat == "Not Reported" ~ "Not categorized",
  !is.na(wdpaid) & iucn_cat == "Not Assigned" ~ "Not categorized",
  TRUE ~ "Not referenced"), .after = iucn_cat) %>%
  st_drop_geometry()

#To perform statistics on the distribution of PAs reported by the WDPA across countries/regions, we focus on not high-income countries.
#Precisely, we restrict to countries defined as low-income, lower middle-income, upper-middle income, minus Russia, and including Chile, Uruguay New Caledonia and Panama
df_ctry_stat_wdpa = WDI(country = "all", start = "1960", end = "2022", extra = TRUE, language = "en") %>%
  group_by(iso3c) %>%
  slice(1) %>%
  ungroup() %>%
  select(c("iso3c", "income")) %>%
  rename("iso3" = "iso3c",
         "wb_inc_grp" = "income") %>%
  filter((wb_inc_grp %in% c("Low income",  "Lower middle income", "Upper middle income") & iso3 != "RUS") | iso3 %in% c("CHL", "URY", "PAN", "NCL"))

lst_ctry_stat_wdpa = df_ctry_stat_wdpa$iso3
```

## Performing descriptive statistics

### FAPBM portfolio and Madagascar

#### IUCN

FAPBM


```r
#Building the relevant dataset
##For all PAs ..
data_cat_iucn = data_pa_fapbm %>%
  filter(marine %in% c(0,1)) %>%
  group_by(iucn_des_en, iucn_des_fr) %>%
  #number of PAs per IUCN category
  summarize(n_iucn = n()) %>%
  ungroup() %>%
  #Frequency of IUCN categories
  mutate(n_pa = sum(n_iucn),
         freq_iucn = round(n_iucn/n_pa*100, 2)) %>%
  arrange(desc(iucn_des_en)) %>%
  mutate(ypos_iucn = cumsum(freq_iucn) - 0.5*freq_iucn) 


##... and for referenced PAs only
data_cat_iucn_ref = data_pa_fapbm %>%
  filter(marine %in% c(0,1)) %>%
  #Remove not referenced PAs
  subset(!(iucn_des_fr %in% c("Non catégorisée", "Non référencée"))) %>%
  group_by(iucn_des_en, iucn_des_fr) %>%
  #number of PAs per IUCN category
  summarize(n_iucn = n()) %>%
  ungroup() %>%
  #Frequency of IUCN categories
  mutate(n_pa = sum(n_iucn),
         freq_iucn = round(n_iucn/n_pa*100, 2)) %>%
  arrange(freq_iucn) %>%
  mutate(ypos_iucn = cumsum(freq_iucn) - 0.5*freq_iucn) 

#Pie charts
pie_cat_iucn_en = ggplot(data_cat_iucn, 
                      aes(x="", y= freq_iucn, fill = iucn_des_en)) %>%
  + geom_bar(width = 1, stat = "identity", color="white") %>%
  + coord_polar("y", start=0) %>%
  + geom_label_repel(aes(x=1.2, label = paste0(round(freq_iucn, 1), "%")), 
             color = "white", 
             position = position_stack(vjust = 0.55), 
             size=2.5, show.legend = FALSE) %>%
  # + geom_label(aes(x=1.4, label = paste0(freq_iucn, "%")), 
  #              color = "white", 
  #              position = position_stack(vjust = 0.7), size=2.5, 
  #              show.legend = FALSE) %>%
  + labs(x = "", y = "",
         title = "Distribution of non-marine protected areas by IUCN categories (%)",
         subtitle = paste("Sample :", sum(data_cat_iucn$n_iucn), "non-marine protected areas funded by FAPBM")) %>%
  #+ scale_fill_brewer(name = "Categories", palette = "Dark2") %>%
  + scale_fill_manual(name = "Categories",
                      values = c("Not categorized" = "#7570B3",
                                 "Habitat or species management area" = "#E7298A",
                                 "Protected landscape or seascape" = "#66A61E",
                                 "Protected area with sust. use of nat. res." = "#D95F02",
                                 "National park" = "#1B9E77",
                                 "Strict nature reserve" = "#E6AB02")) %>%
  + theme_void()
pie_cat_iucn_en

pie_cat_iucn_ref_en = ggplot(data_cat_iucn_ref, 
                      aes(x="", y= freq_iucn, fill = iucn_des_en)) %>%
  + geom_bar(width = 1, stat = "identity", color="white") %>%
  + coord_polar("y", start=0) %>%
  + geom_label_repel(aes(x=1.2, label = paste0(round(freq_iucn, 1), "%")), 
             color = "white", 
             position = position_stack(vjust = 0.55), 
             size=2.5, show.legend = FALSE) %>%
  # + geom_label(aes(x=1.4, label = paste0(freq_iucn, "%")), 
  #              color = "white", 
  #              position = position_stack(vjust = 0.7), size=2.5, 
  #              show.legend = FALSE) %>%
  + labs(x = "", y = "",
         title = "Distribution of non-marine protected areas by IUCN categories \nexcept for not categorized (%)",
         subtitle = paste("Sample :", sum(data_cat_iucn_ref$n_iucn), "out of", sum(data_cat_iucn$n_iucn),  "non-marine protected areas funded by FAPBM")) %>%
  #+ scale_fill_brewer(name = "Categories", palette = "Dark2") %>%
    + scale_fill_manual(name = "Categories",
                      values = c("Not categorized" = "#7570B3",
                                 "Habitat or species management area" = "#E7298A",
                                 "Protected landscape or seascape" = "#66A61E",
                                 "Protected area with sust. use of nat. res." = "#D95F02",
                                 "National park" = "#1B9E77",
                                 "Strict nature reserve" = "#E6AB02")) %>%
  + theme_void()
pie_cat_iucn_ref_en
```


```r
#Saving figures

tmp = paste(tempdir(), "fig", sep = "/")

ggsave(paste(tmp, "pie_cat_iucn_en.png", sep = "/"),
       plot = pie_cat_iucn_en,
       device = "png",
       height = 6, width = 9)

ggsave(paste(tmp, "pie_cat_iucn_ref_en.png", sep = "/"),
       plot = pie_cat_iucn_ref_en,
       device = "png",
       height = 6, width = 9)

#Export to S3 storage

##List of files to save in the temp folder
files <- list.files(tmp, full.names = TRUE)
##Add each file in the bucket (same foler for every file in the temp)
for(f in files) 
  {
  cat("Uploading file", paste0("'", f, "'"), "\n")
  aws.s3::put_object(file = f, 
                     bucket = "projet-afd-eva-ap/descriptive_stats/IUCN/FAPBM/no_marine", 
                     region = "", show_progress = TRUE)
  }

#Erase the files in the temp directory

do.call(file.remove, list(list.files(tmp, full.names = TRUE)))
```

All protected areas in Madagascar (reported by the WDPA)


```r
#Building the relevant dataset
##For all PAs ..
data_cat_iucn = data_wdpa_mdg %>%
  filter(marine %in% c(0,1)) %>%
  group_by(iucn_des_en, iucn_des_fr) %>%
  #number of PAs per IUCN category
  summarize(n_iucn = n()) %>%
  ungroup() %>%
  #Frequency of IUCN categories
  mutate(n_pa = sum(n_iucn),
         freq_iucn = round(n_iucn/n_pa*100, 2)) %>%
  arrange(desc(iucn_des_en)) %>%
  mutate(ypos_iucn = cumsum(freq_iucn) - 0.5*freq_iucn) 


##... and for referenced PAs only
data_cat_iucn_ref = data_wdpa_mdg %>%
  filter(marine %in% c(0,1)) %>%
  #Remove not referenced PAs
  subset(!(iucn_des_fr %in% c("Non catégorisée", "Non référencée"))) %>%
  group_by(iucn_des_en, iucn_des_fr) %>%
  #number of PAs per IUCN category
  summarize(n_iucn = n()) %>%
  ungroup() %>%
  #Frequency of IUCN categories
  mutate(n_pa = sum(n_iucn),
         freq_iucn = round(n_iucn/n_pa*100, 2)) %>%
  arrange(freq_iucn) %>%
  mutate(ypos_iucn = cumsum(freq_iucn) - 0.5*freq_iucn) 

#Pie charts
pie_cat_iucn_en = ggplot(data_cat_iucn, 
                      aes(x="", y= freq_iucn, fill = iucn_des_en)) %>%
  + geom_bar(width = 1, stat = "identity", color="white") %>%
  + coord_polar("y", start=0) %>%
  + geom_label_repel(aes(x=1.2, label = paste0(round(freq_iucn, 1), "%")), 
             color = "white", 
             position = position_stack(vjust = 0.55), 
             size=2.5, show.legend = FALSE) %>%
  # + geom_label(aes(x=1.4, label = paste0(freq_iucn, "%")), 
  #              color = "white", 
  #              position = position_stack(vjust = 0.7), size=2.5, 
  #              show.legend = FALSE) %>%
  + labs(x = "", y = "",
         title = "Distribution of non-marine protected areas by IUCN categories (%)",
         subtitle = paste("Sample :", sum(data_cat_iucn$n_iucn), "non-marine protected areas in Madagascar")) %>%
  # + scale_fill_brewer(name = "Categories", palette = "Dark2") %>%
      + scale_fill_manual(name = "Categories",
                      values = c("Not categorized" = "#7570B3",
                                 "Habitat or species management area" = "#E7298A",
                                 "Protected landscape or seascape" = "#66A61E",
                                 "Protected area with sust. use of nat. res." = "#D95F02",
                                 "National park" = "#1B9E77",
                                 "Strict nature reserve" = "#E6AB02")) %>%
  + theme_void()
pie_cat_iucn_en

pie_cat_iucn_ref_en = ggplot(data_cat_iucn_ref, 
                      aes(x="", y= freq_iucn, fill = iucn_des_en)) %>%
  + geom_bar(width = 1, stat = "identity", color="white") %>%
  + coord_polar("y", start=0) %>%
  + geom_label_repel(aes(x=1.2, label = paste0(round(freq_iucn, 1), "%")), 
             color = "white", 
             position = position_stack(vjust = 0.55), 
             size=2.5, show.legend = FALSE) %>%
  # + geom_label(aes(x=1.4, label = paste0(freq_iucn, "%")), 
  #              color = "white", 
  #              position = position_stack(vjust = 0.7), size=2.5, 
  #              show.legend = FALSE) %>%
  + labs(x = "", y = "",
         title = "Distribution of non-marine protected areas by IUCN categories \nexcept for not categorized (%)",
         subtitle = paste("Sample :", sum(data_cat_iucn_ref$n_iucn), "out of", sum(data_cat_iucn$n_iucn),  "non-marine protected areas in Madagascar")) %>%
  # + scale_fill_brewer(name = "Categories", palette = "Dark2") %>%
    + scale_fill_manual(name = "Categories",
                      values = c("Not categorized" = "#7570B3",
                                 "Habitat or species management area" = "#E7298A",
                                 "Protected landscape or seascape" = "#66A61E",
                                 "Protected area with sust. use of nat. res." = "#D95F02",
                                 "National park" = "#1B9E77",
                                 "Strict nature reserve" = "#E6AB02")) %>%
  + theme_void()
pie_cat_iucn_ref_en
```


```r
#Saving figures

tmp = paste(tempdir(), "fig", sep = "/")

ggsave(paste(tmp, "pie_cat_iucn_en.png", sep = "/"),
       plot = pie_cat_iucn_en,
       device = "png",
       height = 6, width = 9)

ggsave(paste(tmp, "pie_cat_iucn_ref_en.png", sep = "/"),
       plot = pie_cat_iucn_ref_en,
       device = "png",
       height = 6, width = 9)

#Export to S3 storage

##List of files to save in the temp folder
files <- list.files(tmp, full.names = TRUE)
##Add each file in the bucket (same foler for every file in the temp)
for(f in files) 
  {
  cat("Uploading file", paste0("'", f, "'"), "\n")
  aws.s3::put_object(file = f, 
                     bucket = "projet-afd-eva-ap/descriptive_stats/IUCN/MDG/no_marine", 
                     region = "", show_progress = TRUE)
  }

#Erase the files in the temp directory

do.call(file.remove, list(list.files(tmp, full.names = TRUE)))
```

Madagascar protected areas without FAPBM funded protected areas.


```r
#Building the relevant dataset
##For all PAs ..
data_cat_iucn = data_wdpa_mdg %>%
  filter(marine %in% c(0,1)) %>%
  filter(!(wdpaid %in% data_pa_fapbm$wdpaid)) %>%
  group_by(iucn_des_en, iucn_des_fr) %>%
  #number of PAs per IUCN category
  summarize(n_iucn = n()) %>%
  ungroup() %>%
  #Frequency of IUCN categories
  mutate(n_pa = sum(n_iucn),
         freq_iucn = round(n_iucn/n_pa*100, 2)) %>%
  arrange(desc(iucn_des_en)) %>%
  mutate(ypos_iucn = cumsum(freq_iucn) - 0.5*freq_iucn) 


##... and for referenced PAs only
data_cat_iucn_ref = data_wdpa_mdg %>%
  filter(marine %in% c(0,1)) %>%
  filter(!(wdpaid %in% data_pa_fapbm$wdpaid)) %>%
  #Remove not referenced PAs
  subset(!(iucn_des_fr %in% c("Non catégorisée", "Non référencée"))) %>%
  group_by(iucn_des_en, iucn_des_fr) %>%
  #number of PAs per IUCN category
  summarize(n_iucn = n()) %>%
  ungroup() %>%
  #Frequency of IUCN categories
  mutate(n_pa = sum(n_iucn),
         freq_iucn = round(n_iucn/n_pa*100, 2)) %>%
  arrange(freq_iucn) %>%
  mutate(ypos_iucn = cumsum(freq_iucn) - 0.5*freq_iucn) 

#Pie charts
pie_cat_iucn_en = ggplot(data_cat_iucn, 
                      aes(x="", y= freq_iucn, fill = iucn_des_en)) %>%
  + geom_bar(width = 1, stat = "identity", color="white") %>%
  + coord_polar("y", start=0) %>%
  + geom_label_repel(aes(x=1.2, label = paste0(round(freq_iucn, 1), "%")), 
             color = "white", 
             position = position_stack(vjust = 0.55), 
             size=2.5, show.legend = FALSE) %>%
  # + geom_label(aes(x=1.4, label = paste0(freq_iucn, "%")), 
  #              color = "white", 
  #              position = position_stack(vjust = 0.7), size=2.5, 
  #              show.legend = FALSE) %>%
  + labs(x = "", y = "",
         title = "Distribution of non-marine protected areas by IUCN categories (%)",
         subtitle = paste("Sample :", sum(data_cat_iucn$n_iucn), "non-marine, not FAPBM funded protected areas in Madagascar")) %>%
  # + scale_fill_brewer(name = "Categories", palette = "Dark2") %>%
      + scale_fill_manual(name = "Categories",
                      values = c("Not categorized" = "#7570B3",
                                 "Habitat or species management area" = "#E7298A",
                                 "Protected landscape or seascape" = "#66A61E",
                                 "Protected area with sust. use of nat. res." = "#D95F02",
                                 "National park" = "#1B9E77",
                                 "Strict nature reserve" = "#E6AB02")) %>%
  + theme_void()
pie_cat_iucn_en

pie_cat_iucn_ref_en = ggplot(data_cat_iucn_ref, 
                      aes(x="", y= freq_iucn, fill = iucn_des_en)) %>%
  + geom_bar(width = 1, stat = "identity", color="white") %>%
  + coord_polar("y", start=0) %>%
  + geom_label_repel(aes(x=1.2, label = paste0(round(freq_iucn, 1), "%")), 
             color = "white", 
             position = position_stack(vjust = 0.55), 
             size=2.5, show.legend = FALSE) %>%
  # + geom_label(aes(x=1.4, label = paste0(freq_iucn, "%")), 
  #              color = "white", 
  #              position = position_stack(vjust = 0.7), size=2.5, 
  #              show.legend = FALSE) %>%
  + labs(x = "", y = "",
         title = "Distribution of non-marine protected areas by IUCN categories \nexcept for not categorized (%)",
         subtitle = paste("Sample :", sum(data_cat_iucn_ref$n_iucn), "out of", sum(data_cat_iucn$n_iucn),  "non-marine, not FAPBM funded protected areas in Madagascar")) %>%
  # + scale_fill_brewer(name = "Categories", palette = "Dark2") %>%
    + scale_fill_manual(name = "Categories",
                      values = c("Not categorized" = "#7570B3",
                                 "Habitat or species management area" = "#E7298A",
                                 "Protected landscape or seascape" = "#66A61E",
                                 "Protected area with sust. use of nat. res." = "#D95F02",
                                 "National park" = "#1B9E77",
                                 "Strict nature reserve" = "#E6AB02")) %>%
  + theme_void()
pie_cat_iucn_ref_en
```


```r
#Saving figures

tmp = paste(tempdir(), "fig", sep = "/")

ggsave(paste(tmp, "pie_cat_iucn_en.png", sep = "/"),
       plot = pie_cat_iucn_en,
       device = "png",
       height = 6, width = 9)

ggsave(paste(tmp, "pie_cat_iucn_ref_en.png", sep = "/"),
       plot = pie_cat_iucn_ref_en,
       device = "png",
       height = 6, width = 9)

#Export to S3 storage

##List of files to save in the temp folder
files <- list.files(tmp, full.names = TRUE)
##Add each file in the bucket (same foler for every file in the temp)
for(f in files) 
  {
  cat("Uploading file", paste0("'", f, "'"), "\n")
  aws.s3::put_object(file = f, 
                     bucket = "projet-afd-eva-ap/descriptive_stats/IUCN/MDG_noFAPBM/no_marine", 
                     region = "", show_progress = TRUE)
  }

#Erase the files in the temp directory

do.call(file.remove, list(list.files(tmp, full.names = TRUE)))
```

#### Ecosystem

FAPBM funded protected areas.


```r
#Build datasets
data_eco = data_pa_fapbm %>%
  #subset non-referencded PAs (have NA ecosysteme)
  subset(is.na(marine) == FALSE) %>%
  mutate(marine = as.factor(marine))
data_eco$ecosyst_en = fct_recode(data_eco$marine, 
                              "Terrestrial"="0", 
                              "Coastal"="1", 
                              "Marine"="2")
data_eco$ecosyst_fr = fct_recode(data_eco$marine, 
                              "Terrestre"="0", 
                              "Côtier"="1", 
                              "Marin"="2")

data_eco_hist = data_eco %>%
  group_by(ecosyst_en, ecosyst_fr) %>%
  summarize(n = n(),
            freq = round(n/nrow(data_eco), 2)*100) %>%
  ungroup()

#Histogram in number (in English)
pie_eco_en = ggplot(data_eco_hist, 
                     aes(x = "", y = n, fill = ecosyst_en)) %>%
+ geom_bar(width = 1, stat = "identity",color="white") %>%
+ geom_label(aes(x=1.3, label = paste0(freq, "%")), 
             color = "black", position = position_stack(vjust = 0.55), 
             size=2.5, show.legend = FALSE) %>%
+ coord_polar("y", start=0) %>%
+ labs(title = "Proportion of protected areas by ecosystem type",
       subtitle = paste("Sample :", sum(data_eco_hist$n), "protected areas funded by the FAPBM"),
         x = "Ecosystem type",
         y = "Proportion of protected areas") %>%
  #+ scale_fill_brewer(name = "Ecosystem", palette="Paired") %>%
  + scale_fill_manual(name = "Ecosystem",
                      values = c("Marine" = "#1F78B4",
                                 "Terrestrial" = "#B2DF8A",
                                 "Coastal" = "#A6CEE3")) %>%
  + theme_void()
pie_eco_en
```


```r
#Saving figures

tmp = paste(tempdir(), "fig", sep = "/")

ggsave(paste(tmp, "pie_eco_en.png", sep = "/"),
       plot = pie_eco_en,
       device = "png",
       height = 6, width = 9)

#Export to S3 storage

##List of files to save in the temp folder
files <- list.files(tmp, full.names = TRUE)
##Add each file in the bucket (same foler for every file in the temp)
count = 0
for(f in files) 
{
  count = count+1
  cat("Uploading file", paste0(count, "/", length(files), " '", f, "'"), "\n")
  aws.s3::put_object(file = f, 
                     bucket = "projet-afd-eva-ap/descriptive_stats/ecosysteme/FAPBM", 
                     region = "", show_progress = TRUE)
  }

#Erase the files in the temp directory

do.call(file.remove, list(list.files(tmp, full.names = TRUE)))
```

All protected areas in Madagascar (reported by the WDPA)


```r
#Build datasets
data_eco = data_wdpa_mdg %>%
  #subset non-referencded PAs (have NA ecosysteme)
  subset(is.na(marine) == FALSE) %>%
  mutate(marine = as.factor(marine))
data_eco$ecosyst_en = fct_recode(data_eco$marine, 
                              "Terrestrial"="0", 
                              "Coastal"="1", 
                              "Marine"="2")
data_eco$ecosyst_fr = fct_recode(data_eco$marine, 
                              "Terrestre"="0", 
                              "Côtier"="1", 
                              "Marin"="2")

data_eco_hist = data_eco %>%
  group_by(ecosyst_en, ecosyst_fr) %>%
  summarize(n = n(),
            freq = round(n/nrow(data_eco), 2)*100) %>%
  ungroup()

#Histogram in number (in English)
pie_eco_en = ggplot(data_eco_hist, 
                     aes(x = "", y = n, fill = ecosyst_en)) %>%
+ geom_bar(width = 1, stat = "identity",color="white") %>%
+ geom_label(aes(x=1.3, label = paste0(freq, "%")), 
             color = "black", position = position_stack(vjust = 0.55), 
             size=2.5, show.legend = FALSE) %>%
+ coord_polar("y", start=0) %>%
+ labs(title = "Proportion of protected areas by ecosystem type",
       subtitle = paste("Sample :", sum(data_eco_hist$n), "protected areas in Madagascar"),
         x = "Ecosystem type",
         y = "Proportion of protected areas") %>%
  # + scale_fill_brewer(name = "Ecosystem", palette="Paired") %>%
  + scale_fill_manual(name = "Ecosystem",
                      values = c("Marine" = "#1F78B4",
                                 "Terrestrial" = "#B2DF8A",
                                 "Coastal" = "#A6CEE3")) %>%
  + theme_void()
pie_eco_en
```


```r
#Saving figures

tmp = paste(tempdir(), "fig", sep = "/")

ggsave(paste(tmp, "pie_eco_en.png", sep = "/"),
       plot = pie_eco_en,
       device = "png",
       height = 6, width = 9)

#Export to S3 storage

##List of files to save in the temp folder
files <- list.files(tmp, full.names = TRUE)
##Add each file in the bucket (same foler for every file in the temp)
count = 0
for(f in files) 
{
  count = count+1
  cat("Uploading file", paste0(count, "/", length(files), " '", f, "'"), "\n")
  aws.s3::put_object(file = f, 
                     bucket = "projet-afd-eva-ap/descriptive_stats/ecosysteme/MDG", 
                     region = "", show_progress = TRUE)
  }

#Erase the files in the temp directory

do.call(file.remove, list(list.files(tmp, full.names = TRUE)))
```

Madagascar without FAPBM


```r
#Build datasets
data_eco = data_wdpa_mdg %>%
  #subset non-referencded PAs (have NA ecosysteme)
  subset(is.na(marine) == FALSE) %>%
  filter(!(wdpaid %in% data_pa_fapbm$wdpaid)) %>%
  mutate(marine = as.factor(marine))
data_eco$ecosyst_en = fct_recode(data_eco$marine, 
                              "Terrestrial"="0", 
                              "Coastal"="1", 
                              "Marine"="2")
data_eco$ecosyst_fr = fct_recode(data_eco$marine, 
                              "Terrestre"="0", 
                              "Côtier"="1", 
                              "Marin"="2")

data_eco_hist = data_eco %>%
  group_by(ecosyst_en, ecosyst_fr) %>%
  summarize(n = n(),
            freq = round(n/nrow(data_eco), 2)*100) %>%
  ungroup()

#Histogram in number (in English)
pie_eco_en = ggplot(data_eco_hist, 
                     aes(x = "", y = n, fill = ecosyst_en)) %>%
+ geom_bar(width = 1, stat = "identity",color="white") %>%
+ geom_label(aes(x=1.3, label = paste0(freq, "%")), 
             color = "black", position = position_stack(vjust = 0.55), 
             size=2.5, show.legend = FALSE) %>%
+ coord_polar("y", start=0) %>%
+ labs(title = "Proportion of protected areas by ecosystem type",
       subtitle = paste("Sample :", sum(data_eco_hist$n), "protected areas in Madagascar, not funded by the FAPBM"),
         x = "Ecosystem type",
         y = "Proportion of protected areas") %>%
  # + scale_fill_brewer(name = "Ecosystem", palette="Paired") %>%
  + scale_fill_manual(name = "Ecosystem",
                      values = c("Marine" = "#1F78B4",
                                 "Terrestrial" = "#B2DF8A",
                                 "Coastal" = "#A6CEE3")) %>%
  + theme_void()
pie_eco_en
```


```r
#Saving figures

tmp = paste(tempdir(), "fig", sep = "/")

ggsave(paste(tmp, "pie_eco_en.png", sep = "/"),
       plot = pie_eco_en,
       device = "png",
       height = 6, width = 9)

#Export to S3 storage

##List of files to save in the temp folder
files <- list.files(tmp, full.names = TRUE)
##Add each file in the bucket (same foler for every file in the temp)
count = 0
for(f in files) 
{
  count = count+1
  cat("Uploading file", paste0(count, "/", length(files), " '", f, "'"), "\n")
  aws.s3::put_object(file = f, 
                     bucket = "projet-afd-eva-ap/descriptive_stats/ecosysteme/MDG_noFAPBM", 
                     region = "", show_progress = TRUE)
  }

#Erase the files in the temp directory

do.call(file.remove, list(list.files(tmp, full.names = TRUE)))
```

#### Governance

FAPBM funded protected areas


```r
#Table of the governance type distribution
##English version
data_gov_en = data_pa_fapbm %>%
  filter(marine %in% c(0,1)) %>%
  mutate(gov_type = case_when(is.na(gov_type) == TRUE ~ "Not referenced",
                              TRUE ~ gov_type)) %>%
  group_by(gov_type) %>%
  summarize(n = n()) %>%
  mutate(n_tot = sum(n),
         freq = round(n/n_tot*100,1)) %>%
  select(-n_tot) %>%
  arrange(-freq)

tbl_gov_en = data_gov_en
names(tbl_gov_en) = c("Governance","Number of PAs","Share of PAs (%)")


#PAs with nureported or unreferenced governance types are removed
##Tables
###English
data_gov_knwn_en = data_pa_fapbm %>%
  filter(marine %in% c(0,1)) %>%
  mutate(gov_type = case_when(is.na(gov_type) == TRUE ~ "Not referenced",
                              TRUE ~ gov_type)) %>%
  filter(gov_type != "Not Reported" & gov_type != "Not referenced") %>%
  group_by(gov_type) %>%
  summarize(n = n()) %>%
  mutate(n_tot = sum(n),
         freq = round(n/n_tot*100,1)) %>%
  select(-n_tot) %>%
  arrange(-freq)

tbl_gov_knwn_en = data_gov_knwn_en
names(tbl_gov_knwn_en) = c("Governance","Number of PAs","Share of PAs (%)")


##Pie charts
###English
pie_gov_knwn_en = 
  ggplot(data_gov_knwn_en, 
       aes(x="", y= freq, fill= gov_type)) %>%
  + geom_bar(width = 1, stat = "identity", color="white") %>%
  + geom_label(aes(x=1.3, 
                   label = paste0(format(freq, digits = 2), "%")), 
               color = "black", 
               position = position_stack(vjust = 0.55), 
               size=2.5, show.legend = FALSE) %>%
  + coord_polar("y", start=0) %>%
  + labs(title = "Governance type of non-marine protected areas \nexcept for not reported governance",
         subtitle = paste("Sample :", sum(data_gov_knwn_en$n), "out of", sum(data_gov_en$n), "non-marine protected areas funded by FAPBM")) %>%
  # + scale_fill_brewer(name = "Governance", palette="Paired") %>%
  + scale_fill_manual(name = "Governance",
                      values = c("Not Reported" = "#A6CEE3",
                                 "Local communities" = "#1F78B4",
                                 "Government-delegated management" = "#B2DF8A",
                                 "Non-profit organisations" = "#33A02C",
                                 "Collaborative governance" = "#FB9A99",
                                 "Federal or national ministry or agency" = "#E31A1C")) %>%
  + theme_void()
pie_gov_knwn_en
```


```r
#Saving figures

tmp = paste(tempdir(), "fig", sep = "/")

print(xtable(tbl_gov_knwn_en, 
             caption = "Governance of non-marine protected areas funded by FAPBM (when known)",
             type = "latex"),
      file = paste(tmp, "tbl_gov_knwn_en.tex", sep  ="/"))

ggsave(paste(tmp, "pie_gov_knwn_en.png", sep = "/"),
       plot =  pie_gov_knwn_en,
       device = "png",
       height = 6, width = 9)

#Export to S3 storage

##List of files to save in the temp folder
files <- list.files(tmp, full.names = TRUE)
##Add each file in the bucket (same foler for every file in the temp)
count = 0
for(f in files) 
{
  count = count+1
  cat("Uploading file", paste0(count, "/", length(files), " '", f, "'"), "\n")
  aws.s3::put_object(file = f, 
                     bucket = "projet-afd-eva-ap/descriptive_stats/gouvernance/FAPBM/no_marine", 
                     region = "", show_progress = TRUE)
  }

#Erase the files in the temp directory

do.call(file.remove, list(list.files(tmp, full.names = TRUE)))
```

All protected areas in Madagascar (reported by the WDPA)


```r
#Table of the governance type distribution
##English version
data_gov_en = data_wdpa_mdg %>%
  filter(marine %in% c(0,1)) %>%
  mutate(gov_type = case_when(is.na(gov_type) == TRUE ~ "Not referenced",
                              TRUE ~ gov_type)) %>%
  group_by(gov_type) %>%
  summarize(n = n()) %>%
  mutate(n_tot = sum(n),
         freq = round(n/n_tot*100,1)) %>%
  select(-n_tot) %>%
  arrange(-freq)

tbl_gov_en = data_gov_en
names(tbl_gov_en) = c("Governance","Number of PAs","Share of PAs (%)")


#PAs with nureported or unreferenced governance types are removed
##Tables
###English
data_gov_knwn_en = data_wdpa_mdg %>%
  filter(marine %in% c(0,1)) %>%
  mutate(gov_type = case_when(is.na(gov_type) == TRUE ~ "Not referenced",
                              TRUE ~ gov_type)) %>%
  filter(gov_type != "Not Reported" & gov_type != "Not referenced") %>%
  group_by(gov_type) %>%
  summarize(n = n()) %>%
  mutate(n_tot = sum(n),
         freq = round(n/n_tot*100,1)) %>%
  select(-n_tot) %>%
  arrange(-freq)

tbl_gov_knwn_en = data_gov_knwn_en
names(tbl_gov_knwn_en) = c("Governance","Number of PAs","Share of PAs (%)")


##Pie charts
###English
pie_gov_knwn_en = 
  ggplot(data_gov_knwn_en, 
       aes(x="", y= freq, fill= gov_type)) %>%
  + geom_bar(width = 1, stat = "identity", color="white") %>%
  + geom_label(aes(x=1.3, 
                   label = paste0(format(freq, digits = 2), "%")), 
               color = "black", 
               position = position_stack(vjust = 0.55), 
               size=2.5, show.legend = FALSE) %>%
  + coord_polar("y", start=0) %>%
  + labs(title = "Governance type of non-marine protected areas \nexcept for not reported governance",
         subtitle = paste("Sample :", sum(data_gov_knwn_en$n), "out of", sum(data_gov_en$n), "protected areas in Madagascar")) %>%
  #+ scale_fill_brewer(name = "Governance", palette="Paired") %>%
  + scale_fill_manual(name = "Governance",
                      values = c("Not Reported" = "#A6CEE3",
                                 "Local communities" = "#1F78B4",
                                 "Government-delegated management" = "#B2DF8A",
                                 "Non-profit organisations" = "#33A02C",
                                 "Collaborative governance" = "#FB9A99",
                                 "Federal or national ministry or agency" = "#E31A1C")) %>%
  + theme_void()
pie_gov_knwn_en
```


```r
#Saving figures

tmp = paste(tempdir(), "fig", sep = "/")

print(xtable(tbl_gov_knwn_en, 
             caption = "Governance of non-marine protected areas in Madagascar (when known)",
             type = "latex"),
      file = paste(tmp, "tbl_gov_knwn_en.tex", sep  ="/"))

ggsave(paste(tmp, "pie_gov_knwn_en.png", sep = "/"),
       plot =  pie_gov_knwn_en,
       device = "png",
       height = 6, width = 9)

#Export to S3 storage

##List of files to save in the temp folder
files <- list.files(tmp, full.names = TRUE)
##Add each file in the bucket (same foler for every file in the temp)
count = 0
for(f in files) 
{
  count = count+1
  cat("Uploading file", paste0(count, "/", length(files), " '", f, "'"), "\n")
  aws.s3::put_object(file = f, 
                     bucket = "projet-afd-eva-ap/descriptive_stats/gouvernance/MDG/no_marine", 
                     region = "", show_progress = TRUE)
  }

#Erase the files in the temp directory

do.call(file.remove, list(list.files(tmp, full.names = TRUE)))
```

Madagascar without FAPBM funded protected areas


```r
#Table of the governance type distribution
##English version
data_gov_en = data_wdpa_mdg %>%
  filter(!(wdpaid %in% data_pa_fapbm$wdpaid)) %>%
  filter(marine %in% c(0,1)) %>%
  mutate(gov_type = case_when(is.na(gov_type) == TRUE ~ "Not referenced",
                              TRUE ~ gov_type)) %>%
  group_by(gov_type) %>%
  summarize(n = n()) %>%
  mutate(n_tot = sum(n),
         freq = round(n/n_tot*100,1)) %>%
  select(-n_tot) %>%
  arrange(-freq)

tbl_gov_en = data_gov_en
names(tbl_gov_en) = c("Governance","Number of PAs","Share of PAs (%)")


#PAs with nureported or unreferenced governance types are removed
##Tables
###English
data_gov_knwn_en = data_wdpa_mdg %>%
  filter(!(wdpaid %in% data_pa_fapbm$wdpaid)) %>%
  filter(marine %in% c(0,1)) %>%
  mutate(gov_type = case_when(is.na(gov_type) == TRUE ~ "Not referenced",
                              TRUE ~ gov_type)) %>%
  filter(gov_type != "Not Reported" & gov_type != "Not referenced") %>%
  group_by(gov_type) %>%
  summarize(n = n()) %>%
  mutate(n_tot = sum(n),
         freq = round(n/n_tot*100,1)) %>%
  select(-n_tot) %>%
  arrange(-freq)

tbl_gov_knwn_en = data_gov_knwn_en
names(tbl_gov_knwn_en) = c("Governance","Number of PAs","Share of PAs (%)")


##Pie charts
###English
pie_gov_knwn_en = 
  ggplot(data_gov_knwn_en, 
       aes(x="", y= freq, fill= gov_type)) %>%
  + geom_bar(width = 1, stat = "identity", color="white") %>%
  + geom_label(aes(x=1.3, 
                   label = paste0(format(freq, digits = 2), "%")), 
               color = "black", 
               position = position_stack(vjust = 0.55), 
               size=2.5, show.legend = FALSE) %>%
  + coord_polar("y", start=0) %>%
  + labs(title = "Governance type of non-marine protected areas \nexcept for not reported governance",
         subtitle = paste("Sample :", sum(data_gov_knwn_en$n), "out of", sum(data_gov_en$n), "protected areas in Madagascar, not funded by the FAPBM")) %>%
  #+ scale_fill_brewer(name = "Governance", palette="Paired") %>%
  + scale_fill_manual(name = "Governance",
                      values = c("Not Reported" = "#A6CEE3",
                                 "Local communities" = "#1F78B4",
                                 "Government-delegated management" = "#B2DF8A",
                                 "Non-profit organisations" = "#33A02C",
                                 "Collaborative governance" = "#FB9A99",
                                 "Federal or national ministry or agency" = "#E31A1C")) %>%
  + theme_void()
pie_gov_knwn_en
```


```r
#Saving figures

tmp = paste(tempdir(), "fig", sep = "/")

print(xtable(tbl_gov_knwn_en, 
             caption = "Governance of non-marine protected areas in Madagascar, not funded by the FAPBM (when known)",
             type = "latex"),
      file = paste(tmp, "tbl_gov_knwn_en.tex", sep  ="/"))

ggsave(paste(tmp, "pie_gov_knwn_en.png", sep = "/"),
       plot =  pie_gov_knwn_en,
       device = "png",
       height = 6, width = 9)

#Export to S3 storage

##List of files to save in the temp folder
files <- list.files(tmp, full.names = TRUE)
##Add each file in the bucket (same foler for every file in the temp)
count = 0
for(f in files) 
{
  count = count+1
  cat("Uploading file", paste0(count, "/", length(files), " '", f, "'"), "\n")
  aws.s3::put_object(file = f, 
                     bucket = "projet-afd-eva-ap/descriptive_stats/gouvernance/MDG_noFAPBM/no_marine", 
                     region = "", show_progress = TRUE)
  }

#Erase the files in the temp directory

do.call(file.remove, list(list.files(tmp, full.names = TRUE)))
```

#### Number of PAs


```r
#Statistics

## PAs in the WDAP
n_mdg = data_wdpa_mdg %>%
  filter(iso3 == "MDG") %>%
  nrow()
n_fapbm = data_pa_fapbm %>%
  filter(iso3 == "MDG") %>%
  nrow()

## PAs in the WDAP, not marine
n_mdg_nomarine = data_wdpa_mdg %>%
  filter(iso3 == "MDG" & marine %in% c(0,1)) %>%
  nrow()
n_fapbm_nomarine = data_pa_fapbm %>%
  filter(iso3 == "MDG" & marine %in% c(0,1)) %>%
  nrow()

## PA we can analyze with our methodology
yr_min = 2002
n_mdg_ie = data_wdpa_mdg %>%
  filter(iso3 == "MDG") %>%
  filter(status_yr >= yr_min & marine %in% c(0,1) & rep_area > 1 ) %>%
  nrow()
n_fapbm_ie = data_pa_fapbm %>%
  filter(iso3 == "MDG") %>%
  filter(status_yr >= yr_min & marine %in% c(0,1) & area_km2 > 1 ) %>%
  nrow()
```

#### Area of PAs

FAPBM funded protected areas


```r
tbl_area_fapbm = data_pa_fapbm %>%
  filter(marine %in% c(0,1)) %>%
  summarize(n = n(),
            tot = format(sum(area_km2), big.mark = ",", digits = 1, scientific = FALSE),
            min = format(min(area_km2), big.mark = ",", digits = 1, scientific = FALSE),
            max = format(max(area_km2), big.mark = ",", digits = 1, scientific = FALSE),
            mean = format(mean(area_km2), big.mark = ",", digits = 1, scientific = FALSE)
            )
names(tbl_area_fapbm) = c("Number of PAs", "Total area (km²)", "Min. area (km²)", "Max. area (km²)", "Average area (km²)")
```


```r
#Saving figures

tmp = paste(tempdir(), "fig", sep = "/")

print(xtable(tbl_area_fapbm, 
             caption = "Statistics on non-marine protected areas funded by FAPBM",
             type = "latex"), 
      file = paste(tmp, "tbl_area_fapbm.tex", sep = "/"))

#Export to S3 storage

##List of files to save in the temp folder
files <- list.files(tmp, full.names = TRUE)
##Add each file in the bucket (same foler for every file in the temp)
count = 0
for(f in files) 
{
  count = count+1
  cat("Uploading file", paste0(count, "/", length(files), " '", f, "'"), "\n")
  aws.s3::put_object(file = f, 
                     bucket = "projet-afd-eva-ap/descriptive_stats/surface/FAPBM/no_marine", 
                     region = "", show_progress = TRUE)
  }

#Erase the files in the temp directory 

do.call(file.remove, list(list.files(tmp, full.names = TRUE)))  
```

All protected areas in Madagascar (reported by the WDPA)


```r
tbl_area_mdg = data_wdpa_mdg %>%
  st_drop_geometry() %>%
  filter(marine %in% c(0,1)) %>%
  summarize(n = n(),
            tot = format(sum(rep_area), big.mark = ",", digits = 1, scientific = FALSE),
            min = format(min(rep_area), big.mark = ",", digits = 1, scientific = FALSE),
            max = format(max(rep_area), big.mark = ",", digits = 1, scientific = FALSE),
            mean = format(mean(rep_area), big.mark = ",", digits = 1, scientific = FALSE)
            )
names(tbl_area_mdg) = c("Number of PAs", "Total area (km²)", "Min. area (km²)", "Max. area (km²)", "Average area (km²)")
```


```r
#Saving figures

tmp = paste(tempdir(), "fig", sep = "/")

print(xtable(tbl_area_mdg, 
             caption = "Statistics on non-marine protected areas in Madagascar",
             type = "latex"), 
      file = paste(tmp, "tbl_area_mdg.tex", sep = "/"))


#Export to S3 storage

##List of files to save in the temp folder
files <- list.files(tmp, full.names = TRUE)
##Add each file in the bucket (same foler for every file in the temp)
count = 0
for(f in files) 
{
  count = count+1
  cat("Uploading file", paste0(count, "/", length(files), " '", f, "'"), "\n")
  aws.s3::put_object(file = f, 
                     bucket = "projet-afd-eva-ap/descriptive_stats/surface/MDG/no_marine", 
                     region = "", show_progress = TRUE)
  }

#Erase the files in the temp directory

do.call(file.remove, list(list.files(tmp, full.names = TRUE)))
```

Madagascar without FAPBM funded protected areas.


```r
tbl_area_mdg_nofapbm = data_wdpa_mdg %>%
  st_drop_geometry() %>%
  filter(marine %in% c(0,1)) %>%
  filter(!(wdpaid %in% data_pa_fapbm$wdpaid)) %>%
  summarize(n = n(),
            tot = format(sum(rep_area), big.mark = ",", digits = 1, scientific = FALSE),
            min = format(min(rep_area), big.mark = ",", digits = 1, scientific = FALSE),
            max = format(max(rep_area), big.mark = ",", digits = 1, scientific = FALSE),
            mean = format(mean(rep_area), big.mark = ",", digits = 1, scientific = FALSE)
            )
names(tbl_area_mdg_nofapbm) = c("Number of PAs", "Total area (km²)", "Min. area (km²)", "Max. area (km²)", "Average area (km²)")
```


```r
#Saving figures

tmp = paste(tempdir(), "fig", sep = "/")

print(xtable(tbl_area_mdg_nofapbm, 
             caption = "Statistics on non-marine protected areas in Madagascar, not funded by the FAPBM",
             type = "latex"), 
      file = paste(tmp, "tbl_area_mdg_nofapbm.tex", sep = "/"))


#Export to S3 storage

##List of files to save in the temp folder
files <- list.files(tmp, full.names = TRUE)
##Add each file in the bucket (same foler for every file in the temp)
count = 0
for(f in files) 
{
  count = count+1
  cat("Uploading file", paste0(count, "/", length(files), " '", f, "'"), "\n")
  aws.s3::put_object(file = f, 
                     bucket = "projet-afd-eva-ap/descriptive_stats/surface/MDG_noFAPBM/no_marine", 
                     region = "", show_progress = TRUE)
  }

#Erase the files in the temp directory

do.call(file.remove, list(list.files(tmp, full.names = TRUE)))
```

## Evolution of tree cover and deforestation

All protected areas in Madagascar (reported by the WDPA)


```r
##2000 tree cover 
data_treecover_2000 = 
  aws.s3::s3read_using(
  FUN = data.table::fread,
  encoding = "UTF-8",
  # Mettre les options de FUN ici
  object = "data_tidy/GFW/MDG/treecover_extent_2000__ha.csv",
  bucket = "projet-afd-eva-ap",
  opts = list("region" = ""))

##Deforestation 2001-2022
data_treeloss = 
  aws.s3::s3read_using(
  FUN = data.table::fread,
  encoding = "UTF-8",
  # Mettre les options de FUN ici
  object = "data_tidy/GFW/MDG/treecover_loss__ha.csv",
  bucket = "projet-afd-eva-ap",
  opts = list("region" = ""))

data_plot_mdg = data_treeloss %>%
  left_join(data_treecover_2000, by = "iso") %>%
  rename("year" = "umd_tree_cover_loss__year",
         "loss_ha" = "umd_tree_cover_loss__ha",
         "emissions_Mg" = "gfw_gross_emissions_co2e_all_gases__Mg",
         "area_ha" = "area__ha",
         "treecover_2000_ha" = "umd_tree_cover_extent_2000__ha" 
         ) %>%
  arrange(year) %>%
  mutate(cum_loss = cumsum(loss_ha),
         treecover_ha_mdg = treecover_2000_ha - cum_loss,
         treecover_rel00_mdg = treecover_ha_mdg/treecover_2000_ha*100)

fig_treeloss_mdg = ggplot(data = data_plot_mdg,
                      aes(x = year, y = treecover_rel00_mdg)) %>%
  + geom_line(color = "#3182BD") %>%
  + geom_point(color = "#3182BD") %>%
  + labs(x = "", y = "% of 2000 tree cover", 
         title = "Evolution of forest cover in Madagascar",
         subtitle = paste("Treecover in 2000 :", format(data_plot_mdg$treecover_2000_ha[1], digits = 1, big.mark = ",", scientific = F), "ha"),
         caption = "Data : Global Forest Watch") %>%
  + theme(legend.position = "bottom",
      legend.key = element_rect(fill = "white"),
      plot.title = element_text(size = 14, face = "bold"), 
      axis.text.x = element_text(angle = 45,size=10, hjust = .5, vjust = .6),
      axis.title.x = element_text(margin = margin(t = 10)),
      panel.background = element_rect(fill = 'white', colour = 'white', 
                                      linewidth = 0.5, linetype = 'solid'),
      panel.grid.major = element_line(colour = 'grey90', linetype = 'solid'),
      panel.grid.minor = element_line(colour = 'grey90', linetype = 'solid'),
      plot.caption = element_text(color = 'grey50', size = 8.5, face = 'plain', hjust = 0))
fig_treeloss_mdg
```


```r
#Saving figures

tmp = paste(tempdir(), "fig", sep = "/")

ggsave(paste(tmp, "fig_treeloss_mdg.png", sep = "/"),
       plot =  fig_treeloss_mdg,
       device = "png",
       height = 6, width = 9)


#Export to S3 storage

##List of files to save in the temp folder
files <- list.files(tmp, full.names = TRUE)
##Add each file in the bucket (same foler for every file in the temp)
count = 0
for(f in files) 
{
  count = count+1
  cat("Uploading file", paste0(count, "/", length(files), " '", f, "'"), "\n")
  aws.s3::put_object(file = f, 
                     bucket = "projet-afd-eva-ap/descriptive_stats/GFW/MDG/all", 
                     region = "", show_progress = TRUE)
  }

#Erase the files in the temp directory

do.call(file.remove, list(list.files(tmp, full.names = TRUE)))
```

FAPBM funded protected areas.


```r
#Filter the WDPA to non-marine PAs in MDG funded by the FAPBM
wdpa_fapbm = data_wdpa %>%
  filter(iso3 == "MDG" & marine %in% c(0,1) & st_geometry_type(geom) == "MULTIPOLYGON" & wdpaid %in% unique(data_pa_fapbm$wdpaid)) %>%
  st_make_valid() %>%
  st_cast(to = "POLYGON") %>%
  mutate()

#Download the data with the MAPME package 
data.tree.fapbm = init_portfolio(wdpa_fapbm,
                     years = 2000:2022,
                     add_resources = FALSE) %>%
  get_resources(resources = c("gfw_treecover", "gfw_lossyear")) %>%
  calc_indicators(indicators = "treecover_area",
                  min_size=1, # indicator-specific argument
                  min_cover=10) %>%
  unnest(treecover_area) %>%
  drop_na(treecover) %>% #get rid of units with NA values 
  mutate(across(c("treecover"), \(x) round(x, 3))) %>% # Round numeric columns
  pivot_wider(names_from = "years", values_from = "treecover", names_prefix = "treecover_") %>%
  st_drop_geometry() %>%
  group_by(wdpaid) %>%
  summarize(across(.cols = starts_with("treecover"),
                   .fns = ~sum(.x, na.rm = TRUE))) %>%
  ungroup()

#Build a plotting dataset
data_plot_fapbm = data.tree.fapbm %>%
  st_drop_geometry() %>%
  group_by(wdpaid) %>%
  summarize(across(.cols = starts_with("treecover"),
                   .fns = ~sum(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(across(.cols = starts_with("treecover"),
                   .fns = ~sum(.x, na.rm = TRUE))) %>%
  slice(1) %>%
  # mutate(across(.cols = starts_with("treecover"),
  #                .fns = ~.x/treecover_2000*100))  %>%
  pivot_longer(cols = c(starts_with("treecover")),
               names_to = c("var", "year"),
               names_sep = "_",
               values_to = "treecover_ha_fapbm") %>%
  mutate(treecover_rel00_fapbm = treecover_ha_fapbm/treecover_ha_fapbm[year == 2000]*100) %>%
  select(c(year, treecover_ha_fapbm, treecover_rel00_fapbm)) %>%
  mutate(year = as.numeric(year)) 
  

fig_treeloss_fapbm = ggplot(data = data_plot_fapbm,
                      aes(x = year, y = treecover_rel00_fapbm)) %>%
  + geom_line(color = "#3182BD") %>%
  + geom_point(color = "#3182BD") %>%
  + labs(x = "", y = "% of 2000 tree cover", 
         title = "Evolution of forest cover in FAPBM funded protected areas",
        subtitle = paste("Treecover in 2000 :", format(data_plot_fapbm[data_plot_fapbm$year == 2000,]$treecover_ha_fapbm, digits = 1, big.mark = ",", scientific = F), "ha"),
         caption = "Data : Global Forest Watch") %>%
  + theme(legend.position = "bottom",
      legend.key = element_rect(fill = "white"),
      plot.title = element_text(size = 14, face = "bold"), 
      axis.text.x = element_text(angle = 45,size=10, hjust = .5, vjust = .6),
      axis.title.x = element_text(margin = margin(t = 10)),
      panel.background = element_rect(fill = 'white', colour = 'white', 
                                      linewidth = 0.5, linetype = 'solid'),
      panel.grid.major = element_line(colour = 'grey90', linetype = 'solid'),
      panel.grid.minor = element_line(colour = 'grey90', linetype = 'solid'),
      plot.caption = element_text(color = 'grey50', size = 8.5, face = 'plain', hjust = 0))
fig_treeloss_fapbm

# aws.s3::s3write_using(
# FUN = data.table::fwrite,
# data.tree.fapbm,
# # Mettre les options de FUN ici
# object = "data_tidy/GFW/MDG/FAPBM/data_treecover_2000_2022.csv",
# bucket = "projet-afd-eva-ap",
# opts = list("region" = ""))
```


```r
#Saving figures

tmp = paste(tempdir(), "fig", sep = "/")

ggsave(paste(tmp, "fig_treeloss_fapbm.png", sep = "/"),
       plot =  fig_treeloss_fapbm,
       device = "png",
       height = 6, width = 9)


#Export to S3 storage

##List of files to save in the temp folder
files <- list.files(tmp, full.names = TRUE)
##Add each file in the bucket (same foler for every file in the temp)
count = 0
for(f in files) 
{
  count = count+1
  cat("Uploading file", paste0(count, "/", length(files), " '", f, "'"), "\n")
  aws.s3::put_object(file = f, 
                     bucket = "projet-afd-eva-ap/descriptive_stats/GFW/MDG/FAPBM", 
                     region = "", show_progress = TRUE)
  }

#Erase the files in the temp directory

do.call(file.remove, list(list.files(tmp, full.names = TRUE)))
```

Madagascar protected areas without FAPBM funded ones.


```r
wdpa_nofapbm = data_wdpa %>%
  filter(iso3 == "MDG" & marine %in% c(0,1) & st_geometry_type(geom) == "MULTIPOLYGON" & !(wdpaid %in% unique(data_pa_fapbm$wdpaid))) %>%
  st_make_valid() %>%
  st_cast(to = "POLYGON")

data.tree.nofapbm = init_portfolio(wdpa_nofapbm,
                     years = 2000:2022,
                     add_resources = FALSE) %>%
  get_resources(resources = c("gfw_treecover", "gfw_lossyear")) %>%
  calc_indicators(indicators = "treecover_area",
                  min_size=1, # indicator-specific argument
                  min_cover=10) %>%
  unnest(treecover_area) %>%
  drop_na(treecover) %>% #get rid of units with NA values 
  mutate(across(c("treecover"), \(x) round(x, 3))) %>% # Round numeric columns
  pivot_wider(names_from = "years", values_from = "treecover", names_prefix = "treecover_") %>%
  st_drop_geometry() %>%
  group_by(wdpaid) %>%
  summarize(across(.cols = starts_with("treecover"),
                   .fns = ~sum(.x, na.rm = TRUE))) %>%   
  ungroup()

aws.s3::s3write_using(
FUN = data.table::fwrite,
data.tree.nofapbm,
# Mettre les options de FUN ici
object = "data_tidy/GFW/MDG/noFAPBM/data_treecover_2000_2022.csv",
bucket = "projet-afd-eva-ap",
opts = list("region" = ""))

data_plot_nofapbm = data.tree.nofapbm %>%
  st_drop_geometry() %>%
  group_by(wdpaid) %>%
  summarize(across(.cols = starts_with("treecover"),
                   .fns = ~sum(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(across(.cols = starts_with("treecover"),
                   .fns = ~sum(.x, na.rm = TRUE))) %>%
  slice(1) %>%
  # mutate(across(.cols = starts_with("treecover"),
  #                .fns = ~.x/treecover_2000*100))  %>%
  pivot_longer(cols = c(starts_with("treecover")),
               names_to = c("var", "year"),
               names_sep = "_",
               values_to = "treecover_ha_nofapbm") %>%
  mutate(treecover_rel00_nofapbm = treecover_ha_nofapbm/treecover_ha_nofapbm[year == 2000]*100) %>%
  select(c(year, treecover_ha_nofapbm, treecover_rel00_nofapbm)) %>%
  mutate(year = as.numeric(year)) 
  

fig_treeloss_nofapbm = ggplot(data = data_plot_nofapbm,
                      aes(x = year, y = treecover_rel00_nofapbm)) %>%
  + geom_line(color = "#3182BD") %>%
  + geom_point(color = "#3182BD") %>%
  + labs(x = "", y = "% of 2000 tree cover", 
         title = "Evolution of forest cover, in protected areas not FAPBM funded",
        subtitle = paste("Treecover in 2000 :", format(data_plot_nofapbm[data_plot_nofapbm$year == 2000,]$treecover_ha_nofapbm, digits = 1, big.mark = ",", scientific = F), "ha"),
         caption = "Data : Global Forest Watch") %>%
  + theme(legend.position = "bottom",
      legend.key = element_rect(fill = "white"),
      plot.title = element_text(size = 14, face = "bold"), 
      axis.text.x = element_text(angle = 45,size=10, hjust = .5, vjust = .6),
      axis.title.x = element_text(margin = margin(t = 10)),
      panel.background = element_rect(fill = 'white', colour = 'white', 
                                      linewidth = 0.5, linetype = 'solid'),
      panel.grid.major = element_line(colour = 'grey90', linetype = 'solid'),
      panel.grid.minor = element_line(colour = 'grey90', linetype = 'solid'),
      plot.caption = element_text(color = 'grey50', size = 8.5, face = 'plain', hjust = 0))
fig_treeloss_nofapbm
```


```r
#Saving figures

tmp = paste(tempdir(), "fig", sep = "/")

ggsave(paste(tmp, "fig_treeloss_nofapbm.png", sep = "/"),
       plot =  fig_treeloss_nofapbm,
       device = "png",
       height = 6, width = 9)


#Export to S3 storage

##List of files to save in the temp folder
files <- list.files(tmp, full.names = TRUE)
##Add each file in the bucket (same foler for every file in the temp)
count = 0
for(f in files) 
{
  count = count+1
  cat("Uploading file", paste0(count, "/", length(files), " '", f, "'"), "\n")
  aws.s3::put_object(file = f, 
                     bucket = "projet-afd-eva-ap/descriptive_stats/GFW/MDG/noFAPBM", 
                     region = "", show_progress = TRUE)
  }

#Erase the files in the temp directory

do.call(file.remove, list(list.files(tmp, full.names = TRUE)))
```

Plotting the evolution of forest cover in Madagascar, FAPBM funded and not FAPMB funded protected areas in the same figure.


```r
data_plot_all = data_plot_fapbm %>%
  left_join(data_plot_nofapbm, by = "year") %>%
  left_join(select(data_plot_mdg, c(year, treecover_ha_mdg, treecover_rel00_mdg)), by = "year") %>%
  mutate(treecover_rel00_mdg = case_when(is.na(treecover_rel00_mdg) == TRUE ~ 100,
                   TRUE ~ treecover_rel00_mdg),
         treecover_ha_mdg = case_when(is.na(treecover_ha_mdg) == TRUE ~ data_plot_mdg$treecover_2000_ha[1],
                   TRUE ~ treecover_ha_mdg))
#CARFEUL NEED TO RENAME VARIABLES   

fig_treeloss_all =  ggplot(data_plot_all,
                           aes(x = year)) %>%
  + geom_line(aes(y = treecover_rel00_mdg, color = "Madagascar")) %>%
  + geom_point(aes(y = treecover_rel00_mdg, color = "Madagascar")) %>%
  + geom_line(aes(y = treecover_rel00_nofapbm, color = "Not FAPBM")) %>%
  + geom_point(aes(y = treecover_rel00_nofapbm, color = "Not FAPBM")) %>%
  + geom_line(aes(y = treecover_rel00_fapbm, color = "FAPBM")) %>%
  + geom_point(aes(y = treecover_rel00_fapbm, color = "FAPBM")) %>%
  + scale_color_manual(name = "", values = c("Madagascar" = "grey50",
                                  "Not FAPBM" = "#DE2D26",
                                  "FAPBM" = "#31A354")) %>%
  + labs(x = "", y = "% of 2000 tree cover", 
         title = "Evolution of forest cover",
        caption = paste("Data : Global Forest Watch\nTreecover in 2000 :", format(data_plot_all[data_plot_all$year == 2000,]$treecover_ha_mdg, digits = 1, big.mark = ",", scientific = F), "ha in Madagascar,", format(data_plot_all[data_plot_all$year == 2000,]$treecover_ha_fapbm, digits = 1, big.mark = ",", scientific = F), "ha of FAPBM funded protected areas and", format(data_plot_all[data_plot_all$year == 2000,]$treecover_ha_nofapbm, digits = 1, big.mark = ",", scientific = F), "ha of non-funded.")) %>%
  + theme(legend.position = "bottom",
      legend.key = element_rect(fill = "white"),
      plot.title = element_text(size = 14, face = "bold"), 
      axis.text.x = element_text(angle = 45,size=10, hjust = .5, vjust = .6),
      axis.title.x = element_text(margin = margin(t = 10)),
      panel.background = element_rect(fill = 'white', colour = 'white',  
                                      linewidth = 0.5, linetype = 'solid'),
      panel.grid.major = element_line(colour = 'grey90', linetype = 'solid'),
      panel.grid.minor = element_line(colour = 'grey90', linetype = 'solid'),
      plot.caption = element_text(color = 'grey50', size = 8.5, face = 'plain', hjust = 0))
fig_treeloss_all
```


```r
#Saving figures

tmp = paste(tempdir(), "fig", sep = "/")

ggsave(paste(tmp, "fig_treeloss_all.png", sep = "/"),
       plot =  fig_treeloss_all,
       device = "png",
       height = 6, width = 9)


#Export to S3 storage

##List of files to save in the temp folder
files <- list.files(tmp, full.names = TRUE)
##Add each file in the bucket (same foler for every file in the temp)
count = 0
for(f in files) 
{
  count = count+1
  cat("Uploading file", paste0(count, "/", length(files), " '", f, "'"), "\n")
  aws.s3::put_object(file = f, 
                     bucket = "projet-afd-eva-ap/descriptive_stats/GFW/MDG", 
                     region = "", show_progress = TRUE)
  }

#Erase the files in the temp directory

do.call(file.remove, list(list.files(tmp, full.names = TRUE)))
```