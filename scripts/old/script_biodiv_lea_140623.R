# call libs
# install.packages("mapme.biodiversity")
# install.packages("cli")
# install.packages("stargazer")
# install.packages("questionr")
# install.packages("tidyr")
# install.packages("janitor")
library(mapme.biodiversity)
library(sf)
library(tidyverse)
library(mapview)
library(sf)
library(magrittr)
library(tidyverse)
library(stargazer)
library(dplyr)
library(openxlsx)
library(writexl)
library(ggplot2)
library(questionr)
library(readxl)
library(sp)
library(raster)
library(terra)

# Look at package mapme indicators -----------------------------------------------------


resources <- names(available_resources())
indicators <- names(available_indicators())
cat(sprintf("Supported resources:/n- %s/n/nSupported indicators:/n- %s",
            paste(resources, collapse = "/n- "),
            paste(indicators, collapse = "/n- ")))

setwd("~/Volumes/disquelea/AP/")

getwd()



# load data

afd<-read_sf("/Users/lea/Desktop/AP/base/BDD_SHP_nodupl.shp")


# View data
mapView(afd)

# preprocess: transform multipolygon geometries to single polygon geometries
afd<-sf::st_cast(afd,to="POLYGON")


# initialize portfolio
afd_portfolio <- afd %>%
  init_portfolio(2000:2020,
                 cores = 4,
                 add_resources = TRUE,
                 verbose = TRUE)

# ----- Forest Area -----
# download relevant data ressources and link them to the portfolio
afd_portfolio1<-
  get_resources(afd_portfolio,
    resources = c("gfw_lossyear","gfw_treecover","gfw_emissions"), 
    vers_treecover = "GFC-2020-v1.8",
    vers_lossyear = "GFC-2020-v1.8"
  )

# calculate indicators
afd_portfolio1<-
  calc_indicators(x = afd_portfolio1,
                  indicators = "treecover_area",
                  min_cover = 10,
                  min_size = 1, 
                  overwrite=T) # FAO forest definition: Minimum treecover = 10%, minimum size =1 hectare

# unnest results data for further analysis
colnames(afd_portfolio1) # check the columns at the end to derive the relevant column names
afd_treeloss<-unnest(afd_portfolio1,
                     cols="treecover_area")


# view unnested data (only relevant columns are selected for the view and geometry is dropped)
colnames(afd_treeloss)  # check the columns at the end to derive the relevant column names
treeloss <- afd_treeloss %>%
  sf::st_drop_geometry() %>%
  dplyr::select(ID_pr,Nm_AP,Drct_,WDPAID,Ann_c,years,treecover) 

library(tidyverse)
treeloss <- treeloss %>% clean_names()

treeloss$treecover <- replace(treeloss$treecover,treeloss$treecover==0,NA)

treeloss <- treeloss %>%
  filter(!is.na(treecover))


# GRAPH

# Variation par AP (N-(N-1))

treeloss <- treeloss %>%
  group_by(nm_ap) %>% 
  mutate(lag_treecov = lag(treecover),
         loss = ((treecover - lag_treecov)/lag_treecov)*100)

# Intervalles de 5 ans

treeloss <- treeloss %>%
  mutate(years_regroup = case_when(
    years <= 2005 ~ "2000-2005",
    years <= 2010 ~ "2005-2010",
    years <= 2015 ~ "2010-2015",
    years <=2020 ~ "2015-2020",
    TRUE ~ "NA"
  ))

#Variation par intervalle de 5 ans

treeloss <- treeloss %>%
  filter(!is.na(loss))

treeloss <- treeloss %>%
  group_by(nm_ap, years_regroup) %>%
  mutate(
    moy_5 = mean(loss)
  ) %>%
  ungroup()



# 1 - Evolution de la forestloss sur 3 AP :

#Buba
#Variation par année
buba <- treeloss  %>% 
  dplyr::filter(wdpaid %in% c("317051"))



buba %>%
  ggplot(aes(x=years,y=loss)) +
  geom_line() +
  geom_vline(xintercept = 2007, color = "green" )+ 
  labs(
    title    = "Evolution du couvert forestier - Buba",
    x        = "Années",
    y        = "Couvert forestier")

#Variation par intervalle de 5 ans

buba %>%
  ggplot(aes(x=years_regroup,y=moy_5))+
  geom_point()+ 
  geom_vline(xintercept = 2007, color = "green" ) + 
  labs(
    title    = "Evolution du couvert forestier (5 ans) - Buba",
    x        = "Années",
    y        = "Couvert forestier") +
  theme(plot.title = element_text(size=9)) +
  theme(axis.title = element_text(size=9))



#Niumi
niumi <- treeloss  %>% 
  dplyr::filter(wdpaid %in% c("109037"))

niumi %>%
  ggplot(aes(x=years,y=loss)) +
  geom_line() +
  geom_vline(xintercept = 2008, color = "green" )+ 
  labs(
    title    = "Evolution du couvert forestier - AP Niumi",
    x        = "Années",
    y        = "Couvert forestier")


#Bamboung

bamboung <- treeloss  %>% 
  dplyr::filter(wdpaid %in% c("555651496"))

bamboung %>%
  ggplot(aes(x=years,y=loss)) +
  geom_line() +
  geom_vline(xintercept = 2007, color = "green" )+ 
  labs(
    title    = "Evolution du couvert forestier - AP Bamboung",
    x        = "Années",
    y        = "Couvert forestier")


# REGION


# Moyenne du couvert forestier par région par année

treeloss <- treeloss  %>%
  group_by(drct, years) %>%
  mutate(
    evol_reg = mean(treecover)
  ) %>%
  ungroup()

# Variation de la moyenne par région

treeloss <- treeloss %>%
  group_by(drct) %>% 
  mutate(lag_region = lag(evol_reg),
         loss_region = ((evol_reg - lag_region)/lag_region)*100)

# Retirer 2001 (soit ils sont NA, soit ils prennent la valeur du dessus lorsque 2 régions se succèdent dans le tableau car le lag par région n'est pas appliqué)


treeloss$loss_region <- replace(treeloss$loss_region,!is.na(treeloss$loss_region) & treeloss$years==2001,NA)


#2 - Graph sur le sahel

grand_sahel <- treeloss  %>% 
  dplyr::filter(drct %in% c("Dr Grand Sahel")) 

grand_sahel <- grand_sahel %>%
  filter(!is.na(loss_region))

grand_sahel %>% 
  ggplot(aes(x=years,y=loss_region, fill=drct))+
  geom_line()+ 
  labs(
    title    = "Evolution moyenne du couvert forestier au sein des AP du Sahel par année",
    x        = "Années",
    y        = "Couvert forestier") +
  theme(plot.title = element_text(size=7)) +
  theme(axis.title = element_text(size=7))







# # EVOL SUR 4 AP
# 
# AP_4 <- treeloss %>% 
#   dplyr::filter(nm_ap %in% c('Niumi',"Bamboung",'Rio grande de Buba','Azagny National Park')) 
# 
# AP_4 %>% 
#   ggplot(aes(x=years,y=treecover, fill=nm_ap, color=nm_ap))+
#   scale_color_manual(values=c('green','blue','red','orange'))+
#   geom_line()+
#   geom_vline(xintercept = 2008, color = "red")+
#   geom_vline(xintercept = 2007, color = "blue")+
#   geom_vline(xintercept = 2013, color = "green")+
#   geom_vline(xintercept = 2007.2, color = "orange") 
# 
# 
# AP_2 <- treeloss %>% 
#   dplyr::filter(nm_ap %in% c('Niumi','Bamboung')) 
# 
# AP_2%>% 
#   ggplot(aes(x=years,y=treecover, fill=nm_ap, color=nm_ap))+
#   scale_color_manual(values=c('green','blue'))+
#   geom_line()+
#   geom_vline(xintercept = 2008, color = "blue")+
#   geom_vline(xintercept = 2007, color = "green")


# treeloss_wide <- treeloss %>%
#   pivot_wider(names_from = years, values_from = treecover)





#Moyenne sur 5 ans

AP_4 <- AP_4 %>%
  mutate(years_regroups = case_when(
    years <= 2005 ~ "2000-2005",
    years <= 2010 ~ "2005-2010",
    years <= 2015 ~ "2010-2015",
    years <=2020 ~ "2015-2020",
    TRUE ~ "Autre"
  ))

AP_4 <- AP_4  %>%
  group_by(nm_ap, years_regroups) %>%
  mutate(
    moy_5 = mean(treecover)
    ) %>%
  ungroup()

AP_4 %>% 
  ggplot(aes(x=years_regroup,y=moy_5, fill=nm_ap))+
  geom_point()+ 
  labs(
    title    = "Moyenne sur 5 ans",
    x        = "Années",
    y        = "Couvert forestier")









# Evolution par région


#Golf de Guinee
Guinee <- treeloss  %>% 
  dplyr::filter(drct %in% c("Dr Golfe De Guinee")) 
glfguinee %>% 
  ggplot(aes(x=years,y=evol_reg, fill=drct))+
  geom_line()+ 
  labs(
    title    = "Golf de Guinee",
    x        = "Années",
    y        = "Couvert forestier")

#Dr Grand Sahel
grand_sahel <- treeloss  %>% 
  dplyr::filter(drct %in% c("Dr Grand Sahel")) 
grand_sahel %>% 
  ggplot(aes(x=years,y=evol_reg, fill=drct))+
  geom_line()+ 
  labs(
    title    = "Grand Sahel",
    x        = "Années",
    y        = "Couvert forestier")

# Dr Afrique De L'Est
afr_est <- treeloss  %>% 
  dplyr::filter(drct %in% c("Dr Afrique De L'Est")) 
afr_est %>% 
  ggplot(aes(x=years,y=evol_reg, fill=drct))+
  geom_line()+ 
  labs(
    title    = "Afrique De L'Est",
    x        = "Années",
    y        = "Couvert forestier")

# Dr Afrique Australe
afr_aus <- treeloss  %>% 
  dplyr::filter(drct %in% c("Dr Afrique Australe")) 
afr_aus %>% 
  ggplot(aes(x=years,y=evol_reg, fill=drct))+
  geom_line()+ 
  labs(
    title    = "Afrique Australe",
    x        = "Années",
    y        = "Couvert forestier")








# ----- Mangrove Area -----
# download relevant data
afd_portfolio<-
  get_resources(afd_portfolio,
                resources = c("gmw")
  )

# calculate indicators
afd_portfolio<-
  calc_indicators(x = afd_portfolio,
                  indicators = "mangroves_area",
                  overwrite=T)

# unnest results data for further analysis
colnames(afd_portfolio) # check the columns at the end to derive the relevant column names
afd_mangroves<-unnest(afd_portfolio,
                     cols="mangroves_area")

# view unnested data (only relevant columns are selected for the view and geometry is dropped)
colnames(afd_mangroves)  # check the columns at the end to derive the relevant column names
afd_mangroves %>%
  sf::st_drop_geometry() %>%
  dplyr::select(ID_pr,WDPAID,year,mangrove_extent) %>% 
  View()


# OU :
st_read("/Users/lea/Desktop/AP/base/BDD_SHP_nodupl.shp") %>% 
  st_cast("POLYGON") %>%
  init_portfolio(years = 2000:2020, 
                 cores = 4, 
                 add_resources = TRUE) %>%
  get_resources(resources = "gmw") %>%
  calc_indicators(indicators = "mangroves_area") %>% 
  dplyr::select(mangroves_area) %>% 
  tidyr::unnest(mangroves_area) %>% 
  data.frame()


read_sf("/Users/lea/Desktop/AP/base/BDD_SHP_nodupl.shp") %>%
  st_cast("POLYGON") %>%
  init_portfolio(
    years = c(2015, 2016),
    add_resources = FALSE,
    cores = 1,
    verbose = FALSE
  ) %>%
  get_resources("gmw") %>%
  calc_indicators("mangroves_area") %>%
  tidyr::unnest(mangroves_area) %>%
  View()



# create summary table 
mang_summary <-
  afd_mangroves %>%
  dplyr::group_by(WDPAID) %>%
  summarize(area_sqkm = sum(value))








# ----- check more relevant datasources and indicators -----
names(available_resources())
names(available_indicators())


afd_portfolio<-
  get_resources(afd_portfolio,
                resources = c("teow")
  )

afd_portfolio<-
  get_resources(afd_portfolio,
                resources = c("esalandcover")
  )



afd_portfolio<-
  get_resources(afd_portfolio,
                resources = c("worldclim_precipitation")
  )






# ----- Emissions -----
# calculate indicators
afd_portfolio<-
  calc_indicators(x = afd_portfolio,
                  indicators = "treecoverloss_emissions",
                  min_cover = 30,
                  min_size = 1, overwrite=T) 

colnames(afd_portfolio) 
afd_emissions<-unnest(afd_portfolio,
                     cols="treecoverloss_emissions")

colnames(afd_emissions)  
afd_emissions %>%
  sf::st_drop_geometry() %>%
  dplyr::select(ID_pr,WDPAID,years,emissions) %>% 
  View()


# create summary table 
emissions_summary <-
  afd_emissions %>%
  group_by(name) %>%
  summarize(area_sqkm = sum(value))







# ----- BIOME / TEOW -----
# calculate indicators
afd_portfolio<-
  calc_indicators(x = afd_portfolio,
                  indicators = "biome",
                  min_cover = 30,
                  min_size = 1, overwrite=T) 

colnames(afd_portfolio) 
afd_teow<-unnest(afd_portfolio,
                      cols="biome")

colnames(afd_emissions)  
afd_emissions %>%
  sf::st_drop_geometry() %>%
  dplyr::select(ID_pr,WDPAID,value) %>% 
  View()

treeloss_basin <- afd_treeloss %>%
  dplyr::select(c(""),c("Nm_AP","years","treecover","Ann_c"))







