#*****************************************************************************#
#  Project: Analyse des aires protégées                                                            
#  Goal of script:                                                      
#  Author: Léa Poulin                                                       
#  Last modification: 31/05/2023                                              
#                                                
#*****************************************************************************#


#*****************************************************************************#
# I - Préparation des données ----
#*****************************************************************************#



# A - Installation des packages   ------------------------------------------------





# B -  Chargement des packages   ----------------------------------------------------

lapply(packages, library, character.only = TRUE)




# C - Nettoyer la base d'extraction du SIOP  -----------------

# Importer la base extraite du siop -----------------------------------------------------
#
library(readxl)
base_siop <- read_excel("//frparnet30/irs/EVA/3 - Eval et capi/1 - Productions/6 - Appuis/Appuis collectifs/Data et satellitaires/Aires Protégées/Données/Extraction BO/AP_BO_2000-2017_V3.xlsx")

# Supprimer les variables "Id. cofinancier","Libellé cofinancier" si ces 2 items ont été inclus dans l'extraction du SIOP (comme c'était le cas avec la première extraction), et reshape large la variable "cofinancier"  -----------------------------------------------------------
base_siop <- base_siop %>%
dplyr::select(!c("Id. Cofinancier","Libellé cofinancier")) %>%
  pivot_wider(names_from = "Cofinancier", values_from ="Cofinancier")

#Exporter la base_siop sur Excel afin de regrouper les variables cofinanciers en 6 colonnes ("cofinancier 1", "Cofinancier 2", etc.)
#write_xlsx(data.joint, path = "//frparnet30/irs/EVA/3 - Eval et capi/1 - Productions/6 - Appuis/Appuis collectifs/Data et satellitaires/Aires Protégées/Données/Extraction BO/base_siop.xlsx")





# D - Matcher l'extraction BO du SIOP avec l'excel des AP de l'AFD -----------------

# Importer l'Excel des AP et la base extraite du siop -----------------------------------------------------

library(readxl)
Basededonnees_AP <- read_excel("//frparnet30/irs/EVA/3 - Eval et capi/1 - Productions/6 - Appuis/Appuis collectifs/Data et satellitaires/Aires Protégées/Données/Base initiale recensement concours AP/Basededonnees_AP.xlsx")

#Ne garder que les variables pertinentes suivantes :
Base_AP <- Basededonnees_AP %>%
select(c("ID_concours","maitrise_ouvrage","surface(km2)","nb_AP","détail","projet","commentaires","ID_WDPA"))

# library(readxl)
#base_siop <- read_excel("//frparnet30/irs/EVA/3 - Eval et capi/1 - Productions/6 - Appuis/Appuis collectifs/Data et satellitaires/Aires Protégées/Données/Extraction BO/base_siop.xlsx")


# Apparier les deux bases de données (fichier excel initial "Basededonnees_AP" + extraction du siop "base_siop") ----------------------------------------------------------------
data.joint <- base_siop %>%
left_join(Base_AP, by = c("ID_concours"))

#Exporter la base appariée sur Excel afin de remplir les colonnes, à l'aide du site de l'IUCN, "wdpaid" et "nom_ap" et nettoyer la base => output : full_base
#write_xlsx(data.joint, path = "//frparnet30/irs/EVA/3 - Eval et capi/1 - Productions/6 - Appuis/Appuis collectifs/Data et satellitaires/Aires Protégées/Données/full_base.xlsx")

#
#
# # E - Matcher les données internes avec la base WDPA de l'IUCN -------------
#
# Importer la base "full_base" complétée des identifiants WDPAID et des nom_ap ----------------------------------------------------
full_base <- read_excel("//frparnet30/irs/EVA/3 - Eval et capi/1 - Productions/6 - Appuis/Appuis collectifs/Data et satellitaires/Aires Protégées/Données/Base_identif_wdpaid/full_base.xlsx")

 # Importer la base de données complète WDPA de l'IUCN
WDPA_BDD <- read_excel("//frparnet30/irs/EVA/3 - Eval et capi/1 - Productions/6 - Appuis/Appuis collectifs/Data et satellitaires/Aires Protégées/Données/Donnees_IUCN/WDPA_BDD.xlsx")
#275791 obs
# 
# 
# # Matching WDPA_BDD et full_base----------------------------------------
# 
WDPA_BDD$wdpaid <- as.character(WDPA_BDD$wdpaid)

BDD_joint <- full_base %>%
  left_join(WDPA_BDD, by = c("wdpaid"))
# 
# 
#
# Data cleaning -----------------------------------------------------------
#
library(janitor)
BDD_joint <- BDD_joint %>% clean_names()

#Exporter la base "BDD_joint" sur Excel => modification effectuée directement sur Excel (suppression des WDPA_PID en double)
write_xlsx(BDD_joint, path = "//frparnet30/irs/EVA/3 - Eval et capi/1 - Productions/6 - Appuis/Appuis collectifs/Data et satellitaires/Aires Protégées/Données/R/BDD_joint.xlsx")

# 
# 

#*****************************************************************************#
# II - Analyse des indicateurs projets ----
#*****************************************************************************#

# Importer la Base après simplication de noms de variables directement sur excel --------
# Modification sur excel : 
# - appliquer le bon format à tous les nombres
# - modifier le nom de toutes les cellules surlignées en jaune du fichier "BDD_test2"
# - pour la variable "pays" : remplacer les valeurs "multipays" dans la variable "pays" par les valeurs de la variable "autres pays de réalisation" !! (fonction "si" sur excel)
# - mettre les nom des pays et DR en minuscule
# - fusionner colonne superficie BO avec superficie WDPA : MARIN et TERRESTRE


base <-read_excel("//frparnet30/irs/EVA/3 - Eval et capi/1 - Productions/6 - Appuis/Appuis collectifs/Data et satellitaires/Aires Protégées/Données/R/BDD_joint.xlsx")

class(base)


# Creation d'une nouvelle base supprimant les doublons afin d'obtenir 1 ligne par AP :
#  - On divise la base bdd_joint en 2 entre les AP répertoriées dans la WDPA (ayant un identifiant) et celles qui ne le sont pas (pas d'identifiant => NA) :
#     o Base avec identifiant "base_id" : on supprime les doublons à partir de la variable wdpaid
#     o Base sans identifiant "base_na" : on supprime les doublons à partir de la variable "nom_ap" afin de ne pas regrouper toutes les AP non répertoriées en une seule ligne "NA". On conserve donc toutes les lignes correspondant à des AP clairement identifiées tandis que celles n'ayant pas d'AP identifiée (sans nom) sont supprimées (celles-ci correspondent à des projets non fléchés sur des AP en particulier).
#  - Matcher les deux bases => "base_nodupl"
#NB : cette base ne pourra pas être utilisée pour certaines statistiques, telles que celles sur les financements octroyés car une seule AP a pu être recevoir plusieurs financements, il faudra donc conserver toutes les lignes de financement

base_na <- base %>% filter(is.na(wdpaid))
base_id <- base %>% filter(!is.na(wdpaid))

base_na <- base_na %>% distinct(nom_ap, .keep_all= TRUE)
base_id  <- base_id %>% distinct(wdpaid, .keep_all= TRUE)

base_nodupl <- rbind(base_na, base_id)
#143 observations : 106 AP ayant un WDPA ID / 37 NA


# write_csv(base_nodupl, path = "//frparnet30/irs/EVA/3 - Eval et capi/1 - Productions/6 - Appuis/Appuis collectifs/Data et satellitaires/Aires Protégées/Données/R/base_nodupl.csv")
# write_xlsx(base_nodupl, path = "//frparnet30/irs/EVA/3 - Eval et capi/1 - Productions/6 - Appuis/Appuis collectifs/Data et satellitaires/Aires Protégées/Données/R/base_nodupl.xlsx")



# A # CATEGORIE IUCN ----------------------------------------------------


# Renommer les modalités de la variable Categorie_IUCN dans les deux bases --------------------

base_nodupl$cat_iucn <- case_when(
  !is.na(base_nodupl$wdpaid) & base_nodupl$iucn_cat == "Ia" ~ "Réserve naturelle intégrale",
  !is.na(base_nodupl$wdpaid) & base_nodupl$iucn_cat == "Ib" ~ "Zone de nature sauvage",
  !is.na(base_nodupl$wdpaid) & base_nodupl$iucn_cat == "II" ~ "Parc national",
  !is.na(base_nodupl$wdpaid) & base_nodupl$iucn_cat == "III" ~ "Monument naturel",
  !is.na(base_nodupl$wdpaid) & base_nodupl$iucn_cat == "IV" ~ "Gest. des habitats/espèces",
  !is.na(base_nodupl$wdpaid) & base_nodupl$iucn_cat == "V" ~ "Paysage protégé",
  !is.na(base_nodupl$wdpaid) & base_nodupl$iucn_cat == "VI" ~ "Gest. de ress. protégées",
  !is.na(base_nodupl$wdpaid) & base_nodupl$iucn_cat == "Not Applicable" ~ "Non catégorisée",
  !is.na(base_nodupl$wdpaid) & base_nodupl$iucn_cat == "Not Reported" ~ "Non catégorisée",
  !is.na(base_nodupl$wdpaid) & base_nodupl$iucn_cat == "Not Assigned" ~ "Non catégorisée",
  TRUE ~ "Non référencée"
)

base$cat_iucn <- case_when(
  !is.na(base$wdpaid) & base$iucn_cat == "Ia" ~ "Réserve naturelle intégrale",
  !is.na(base$wdpaid) & base$iucn_cat == "Ib" ~ "Zone de nature sauvage",
  !is.na(base$wdpaid) & base$iucn_cat == "II" ~ "Parc national",
  !is.na(base$wdpaid) & base$iucn_cat == "III" ~ "Monument naturel",
  !is.na(base$wdpaid) & base$iucn_cat == "IV" ~ "Gest. des habitats/espèces",
  !is.na(base$wdpaid) & base$iucn_cat == "V" ~ "Paysage protégé",
  !is.na(base$wdpaid) & base$iucn_cat == "VI" ~ "Gest. de ress. protégées",
  !is.na(base$wdpaid) & base$iucn_cat == "Not Applicable" ~ "Non catégorisée",
  !is.na(base$wdpaid) & base$iucn_cat == "Not Reported" ~ "Non catégorisée",
  !is.na(base$wdpaid) & base$iucn_cat == "Not Assigned" ~ "Non catégorisée",
  TRUE ~ "Non référencée"
)



# Calculer la proportion d'AP par catégorie IUCN --------------------------
cat <- count(base_nodupl$cat_iucn)
cat <- cat %>% 
  mutate(
    total = sum(freq),
    prop  = round(freq/total, 3)*100
  )

cat <- cat %>% 
  dplyr::select(-c(total))
names(cat) <- c("Catégories","Nb", "Proportion")

# Exporter sur LaTex ------------------------------------------------------
print(xtable(cat, type = "latex"), file = "CATIUCN.tex")
print(xtable(cat))


# Histogramme -------------------------------------------------------------
ggplot(base_nodupl, aes(x = reorder(cat_iucn,+as.numeric(wdpaid)), fill = as.numeric(wdpaid))) + 
  geom_bar(width = 0.50, fill="blue") +
  ggtitle("Proportion d'aires protégées par catégorie IUCN") +
  theme(plot.title = element_text(size = 14, face = "bold",hjust=0.5)) +
  xlab("Catégories IUCN") +
  ylab("Nombre d'aires protégées") +
  theme(axis.text.x = element_text(angle = (0),size=11))


# Diagramme incluant les AP non référencées ---------------------------------------------------------------

ggplot(cat, aes(x="", y=Proportion, fill=Catégories))+
  geom_bar(width = 1, stat = "identity",color="white")+
  coord_polar("y", start=0) +
  ggtitle("Proportion d'aires protégées par catégorie IUCN (%)") +
  theme(plot.title = element_text(size = 9, face = "bold",hjust=0.5)) +
  scale_fill_brewer(palette="Dark2") +
  geom_label(aes(x=1.4, label = paste0(Proportion, "%")), color = "white", position = position_stack(vjust = 0.7), size=2.5, show.legend = FALSE) 


# Diagramme sans les AP non référencées et non catégorisées dans la BDD WDPA ------------------

cat_ref <- filter(base_nodupl, cat_iucn != "Non référencée" & cat_iucn != "Non catégorisée" )

cat_ref <- count(cat_ref$cat_iucn)

cat_ref <- cat_ref %>% 
  mutate(
    prop  = round(freq/sum(freq), 3)*100
  )

names(cat_ref) <- c("Categories","PA", "Proportion")

ggplot(cat_ref, aes(x="", y=Proportion, fill=Categories))+
  geom_bar(width = 1, stat = "identity",color="white")+
  coord_polar("y", start=0) +
  ggtitle("Proportion of protected areas by IUCN category (excluding unclassified PAs)") +
  theme(plot.title = element_text(size = 9, face = "bold",hjust=1)) +
  scale_fill_brewer(palette="Paired") +
  geom_label(aes(x=1.2, label = paste0(Proportion, "%")), color = "black", position = position_stack(vjust = 0.55), size=2.5, show.legend = FALSE) +
  theme_void()



# Catégorie IUCN par pays et région ------------------------------------

#prop.table(table(base_nodupl$Categorie_IUCN,base_nodupl$pays),2) => The value of each cell is divided by the sum of the column cells <=> somme par colonne : prop.table(m, 2)

# veiller à remplacer les valeurs "multipays" dans la variable "pays" par les valeurs de la variable "autres pays de réalisation" !! (fonction "Si" sur excel)

pays <- round(prop.table(table(base_nodupl$cat_iucn,base_nodupl$pays),2),3)*100
pays_iucn <- as.data.frame(pays)
pays_iucn <- pays_iucn %>% pivot_wider(names_from = Var2, values_from = Freq)
pays_iucn <- t(pays_iucn)

region <- round(prop.table(table(base_nodupl$cat_iucn,base_nodupl$direction_regionale),2),3)*100
region_iucn <- as.data.frame(region)
region_iucn <- region_iucn %>% pivot_wider(names_from = Var2, values_from = Freq)
region_iucn <- t(region_iucn)


# Format LaTex ------------------------------------------------------------

print(xtable(pays_iucn, type = "latex"), file = "paysIUCN.tex")
print(xtable(pays_iucn))

print(xtable(region_iucn, type = "latex"), file = "REGIONIUCN.tex")
print(xtable(region_iucn))


# B #ECOSYSTEMES (hors AP non référencées) ----------------------------------------------

## Proportions d'AP par zone marine/terrestre 
# 0 (à prédominance ou entièrement terrestre), 1 (côtier: marine et terrestre), et 2 (à prédominance ou entièrement marine). 
#La valeur '1' est seulement utilisée pour les polygones.


# renommer les modalités de la variable marine ----------------------------

base_nodupl$marine <- as.factor(base_nodupl$marine)

base_nodupl$ecosysteme <- fct_recode(base_nodupl$marine,
                                     "Terrestrial"="0",
                                     "Coastal"="1",
                                     "Marine"="2"
)


# tableau de la proportion d'AP par ecosystème ----------------------------
# 
# ecosysteme <- round(rprop(table(base_nodupl$wdpaid,base_nodupl$ecosysteme)),2)
# ecosys <- as.data.frame(ecosysteme)
# ecosys <- ecosys %>% pivot_wider(names_from = Var2, values_from = Freq)


# Histogramme ecosysteme en % d'AP (VFRANCAIS)------------------------------------------------

ecosys_nb <- data.frame(table(base_nodupl$ecosysteme))
ecosys_nb$Freq <- round(ecosys_nb$Freq/sum(ecosys_nb$Freq),3)*100
names(ecosys_nb) <- c("Ecosystème","Proportion")

ggplot(ecosys_nb, aes(x = Ecosystème, y=Proportion, fill=Ecosytème)) + 
  geom_bar(width = 0.50, fill="blue",stat="identity") +
  ggtitle("Proportion d'aires protégées par type d'écosystème") +
  theme(plot.title = element_text(size = 14, face = "bold",hjust=0.5)) +
  xlab("Type d'écosystème") +
  ylab("Proportion d'aires protégées(%)")

# Histogramme ecosysteme en % d'AP (VENGLISH) ------------------------------------------------

ecosys_nb <- data.frame(table(base_nodupl$ecosysteme))
ecosys_nb$Freq <- round(ecosys_nb$Freq/sum(ecosys_nb$Freq),3)*100
names(ecosys_nb) <- c("Ecosysteme","Proportion")

ggplot(ecosys_nb, aes(x = Ecosysteme, y=Proportion, fill=Ecosyteme)) + 
  geom_bar(width = 0.50, fill="blue",stat="identity") +
  ggtitle("Proportion of protected areas by ecosystem type") +
  theme(plot.title = element_text(size = 14, face = "bold",hjust=0.5)) +
  xlab("Ecosystem type") +
  ylab("Proportion of protected areas(%)")

##vérifions : 
ecosys_prop <-round(data.frame(ecosys_nb$Freq/sum(ecosys_nb$Freq)),3)*100
names(ecosys_prop)<-c("Proportion")
ecosys <- cbind(ecosys_nb,ecosys_prop)
names(ecosys) <- c("Ecosystème","Nombre d'AP","Proportion d'AP(%)")

print(xtable(ecosys, type = "latex"), file = "ECOSYS.tex")
print(xtable(ecosys))

# Histogramme ecosyteme en nombre d'AP -------------------------------------------------------------

ggplot(base_nodupl, aes(x = ecosysteme, fill = as.numeric(wdpaid))) + 
  geom_bar(width = 0.50, fill="blue") +
  ggtitle("Proportion d'aires protégées par type d'écosystème") +
  theme(plot.title = element_text(size = 14, face = "bold",hjust=0.5)) +
  xlab("Type d'écosystème") +
  ylab("Nombre d'aires protégées")


# Diagramme ---------------------------------------------------------------

ggplot(ecosys_nb, aes(x = "", y=Proportion, fill=Ecosysteme))+
  geom_bar(width = 1, stat = "identity",color="white")+
  coord_polar("y", start=0) +
  ggtitle("Proportion of protected areas by ecosystem type (excluding unclassified PAs)") +
  theme(plot.title = element_text(size = 9, face = "bold",hjust=0.5)) +
  scale_fill_brewer(palette="Paired") +
  geom_label(aes(x=1.3, label = paste0(Proportion, "%")), color = "black", position = position_stack(vjust = 0.55), size=2.5, show.legend = FALSE) +
  theme_void()



# C # PAYS/REGIONS  ------------------------------------------------------



# Proportion d'AP par pays  --------------------------------------------

# ap_pays <- round(addmargins(prop.table((table(base_nodupl$wdpaid,base_nodupl$pays))),1),2)
# ap_pays <- as.data.frame(ap_pays)
# ap_pays <- ap_pays %>% pivot_wider(names_from = Var2, values_from = Freq)

ap_pays2 <- data.frame(table(base_nodupl$pays))
ap_pays2$Freq <- round(ap_pays2$Freq/sum(ap_pays2$Freq),3)*100

#TOP 6 PAYS
ap_pays_6 = subset(ap_pays2, Freq >= 5)


# Histogramme AP par pays  ------------------------------------------------
## EN % : TOP 6 des PAYS
ggplot(ap_pays_6, aes(x = reorder(Var1,-Freq), y=Freq, fill=Var1)) + 
  geom_bar(width = 0.80, fill="blue",stat="identity") +
  ggtitle("Les 6 pays abritant le plus d'aires protégées") +
  theme(plot.title = element_text(size = 14, face = "bold",hjust=0.5)) +
  xlab("pays") +
  ylab("Proportion d'aires protégées(%)")

## EN NB d'AP
ggplot(base_nodupl, aes(x = pays, fill = pays)) + 
  geom_bar(width = 0.60, fill="blue") +
  ggtitle("Proportion d'aires protégées par pays") +
  theme(plot.title = element_text(size = 14, face = "bold",hjust=0.5)) +
  xlab("pays") +
  ylab("Nombre d'aires protégées")+
  theme(axis.text.x = element_text(angle = (19),size=8))


# Proportions d'AP par région ---------------------------------------------

# ap_region <- round(addmargins(prop.table((table(base_nodupl$wdpaid,base_nodupl$direction_regionale))),1),2)
# ap_region <- as.data.frame(ap_region)
# ap_region <- ap_region %>% pivot_wider(names_from = Var2, values_from = Freq)

ap_region2 <- data.frame(table(base_nodupl$direction_regionale))
ap_region2$Freq <- round(ap_region2$Freq/sum(ap_region2$Freq),3)*100

#TOP 6 REGION
ap_region_6 = subset(ap_region2, Freq >= 5)


# Histogramme AP pays région -------------------------------------------------------------

##EN % : TOP 6
ggplot(ap_region_6, aes(x = reorder(Var1,-Freq), y=Freq, fill=Var1)) + 
  geom_bar(width = 0.80, fill="blue",stat="identity") +
  ggtitle("Les 6 régions abritant le plus d'aires protégées") +
  theme(plot.title = element_text(size= 14, face = "bold",hjust=0.5)) +
  xlab("Région") +
  ylab("Proportion d'aires protégées(%)")  

##EN NB D'AP
ggplot(base_nodupl, aes(x = direction_regionale)) + 
  geom_bar(width = 0.60, fill="blue") +
  ggtitle("Proportion d'aires protégées par région") +
  theme(plot.title = element_text(size = 14, face = "bold",hjust=0.5)) +
  xlab("Régions") +
  ylab("Nombre d'aires protégées")+
  theme(axis.text.x = element_text(angle = (19),size=8))

# D # EVOLUTION TEMPORELLE -------------------------------------------------


# Créer des tranches  -----------------------------------------------------

#(Sur la table excel, créer une nouvelle colonne annee_octroi pour ne garder QUE l'année)

# base$date <- rep(0,length(base$Date_octroi))
# base$date[2000<=base$annee_octroi & 2005>=base$annee_octroi]="2000-2005"
# base$date[2005<base$annee_octroi & 2010>=base$annee_octroi]="2005-2010"
# base$date[2010<base$annee_octroi & 2015>=base$annee_octroi]="2010-2015"
# base$date[2015<base$annee_octroi]="2015-2017"
# table(base$date)


# Evolution du nombre d'AP dans le temps (graphique cumulé et histo) ----------------------------------

table(base_nodupl$annee_octroi)

cum_areas <- data.frame(table(base_nodupl$annee_octroi))

cum_areas %>%
  ggplot(aes(x=Var1, y=cumsum(Freq))) +
  geom_point()+
  #geom_line()+
  ggtitle("Evolution du nombre d'aires protégées appuyées par l'AFD")+
  theme(plot.title =element_text(size = 14, face = "bold", hjust=0.5))+
  xlab("Années")+
  ylab("Nombre d'aires protégées")     

ggplot(base_nodupl, aes(x = annee_octroi)) + 
  geom_bar(width = 0.50, fill="blue") +
  ggtitle("Nombre d'aires protégées appuyées par année") +
  theme(plot.title = element_text(size = 14, face = "bold",hjust=0.5)) +
  xlab("Années") +
  ylab("Nombre d'aires protégées")+
  theme(axis.text.x = element_text(size=8))


#  E # FINANCEMENT  -------------------------------------------------------

# Financement moyen par projet (euro/projet) --------------------------------------
#NB : doublons sont à prendre en compte : on part de la BDD "base" et on supprime les doublons par projet 

projet_nodupl <- base %>% distinct(id_projet, .keep_all= TRUE)




# Summary : financement moyen par projet -----------------------------------------------------------------


fin <- as.array(format(summary(as.numeric(projet_nodupl$montant_total_projet))),scientific= FALSE)
fin_proj <- data.frame(t(fin))  #t(fin) permet de transposer la matrice

fin_proj <- fin_proj %>% 
  dplyr::select(-c(X1st.Qu.,X3rd.Qu.))



print(xtable(fin_proj, type = "latex"), file = "FINAP.tex")
print(xtable(fin_proj))


# Boîte à moustache -------------------------------------------------------

ggplot() + boxplot(as.numeric(projet_nodupl$montant_total_projet))



# Financement moyen par concours ------------------------------------------
fin_concours <- base %>% group_by(id_concours) %>% slice(1) %>% summarize(Mean = mean(as.numeric(montant_total_projet)))
#NB : fonction group_by avec slice permet de supprimer les lignes en double par valeurs de colonnes (pays, région)
names(fin_concours) <- c("Concours")   

# Financement moyen par projet ------------------------------------------

fin_proj_moy <- aggregate(as.numeric(montant_total_projet) ~ id_projet, data = projet_nodupl, FUN = mean)
names(fin_proj_moy ) <- c("Projets","Montants octroyés")

print(xtable(fin_proj_moy, type = "latex"), file = "FINPROJMOY.tex")
print(xtable(fin_proj_moy))

##OU
## fin_projet <-base %>% group_by(id_projet) %>% slice(1) %>% summarize(Mean = mean(as.numeric(montant_total_projet)))
## names(fin_projet) <- c("projets")

fin_conc_proj <- cbind(fin_concours,fin_proj)
fin_conc_proj <- t(fin_conc_proj)


# Financement moyen par pays et région ------------------------------------

fin_pays <- aggregate(as.numeric(montant_total_projet) ~ pays, data = projet_nodupl, FUN = mean)
names(fin_pays) <- c("Pays","Montants")
attach(fin_pays)
fin_pays<- fin_pays[order(-Montants),]

fin_region <- aggregate(as.numeric(montant_total_projet) ~ direction_regionale, data = projet_nodupl, FUN = mean)
names(fin_region) <- c("Régions","Montants")
attach(fin_region)
fin_region<- fin_region[order(-Montants),]

print(xtable(fin_pays, type = "latex"), file = "FINpays.tex")
print(xtable(fin_pays))

print(xtable(fin_region, type = "latex"), file = "FINREG.tex")
print(xtable(fin_region))

# base %>% group_by(pays) %>% slice(1) %>% summarize(Mean = mean(as.numeric(montant_total_projet)))
# base %>% group_by(direction_regionale) %>% slice(1) %>% summarize(Mean = mean(as.numeric(montant_total_projet)))
# base %>% group_by(direction_regionale) %>% slice(1) %>% summarize(Median = format(median(as.numeric(montant_total_projet)),scientific=FALSE))



#  Financement moyen par superficie (Euros/km2) (marine ET TERRESTRE) ---------------------------


#Drop les NA de la variable superficie"

totalfinancement <- sum(as.numeric(projet_nodupl$montant_total_projet))
totalsuperficie <- sum(as.numeric(base_nodupl$superficie))
financementkm2 <- totalfinancement / totalsuperficie
finkm2 <- data.frame(financementkm2)
names(finkm2) <-c("Financement moyen par km2 (€/km2)")

print(xtable(finkm2, type = "latex"), file = "FINKM2.tex")
print(xtable(finkm2))



# Evolution du financement dans le temps (graph cumulé + histo) ----------------------------------


##créer un tableau des montants financés par année -> aggregate 

fin_annee <- aggregate(as.numeric(montant_total_projet) ~ annee_octroi, projet_nodupl, sum)
names(fin_annee)<-c("annee_octroi","Montant")

str(fin_annee)
#les deux var sont bien au format numérique

fin_annee %>%
  ggplot(aes(x=annee_octroi, y=cumsum(as.numeric(Montant)))) +
  list(
    geom_point(),
    ggtitle("Evolution du financement des aires protégées"),
    theme(plot.title =element_text(size = 14, face = "bold", hjust=0.5)),
    xlab("Années"),
    ylab("Montants octroyés"))


fin_annee %>%
  ggplot(mapping= aes(x=annee_octroi, y=as.numeric(Montant))) +
  list(
    geom_bar(width = 0.50, stat='identity',fill="blue"),
    ggtitle("Evolution du financement des aires protégées"),
    theme(plot.title =element_text(size = 14, face = "bold", hjust=0.5)),
    xlab("Années"),
    ylab("Montants octroyés"))


# Type de financement -------------------------------------------------


# % d'AP par type de financement 
# rprop(table(base_nodupl$wdpaid,base_nodupl$`Libellé produit`))

type_fin_freq <- data.frame(table(projet_nodupl$libelle_produit))

Proportion <-round(data.frame(type_fin_freq$Freq/sum(type_fin_freq$Freq)),3)*100
names(Proportion)<-c("Proportion")

type_fin <- cbind(type_fin_freq,Proportion)
names(type_fin) <- c("Type de financement","Projets","Proportion(%)")

print(xtable(type_fin, type = "latex"), file = "TYPEFIN.tex")
print(xtable(type_fin))


# % et Montants financés par type de financement 

montant_typefin <- aggregate(as.numeric(montant_total_projet) ~ libelle_produit, projet_nodupl, sum)
names(montant_typefin) <- c("Type de financement","Montants (€)")

Proportion_typefin <- round(data.frame(suppressWarnings(as.numeric(montant_typefin$Montant)))/sum(suppressWarnings(as.numeric(montant_typefin$Montant))),3)*100
names(Proportion_typefin)<-c("Proportion (%)")

montant_partype_fin <- cbind(montant_typefin,Proportion_typefin)

print(xtable(montant_partype_fin, type = "latex"), file = "MONTTYPEFIN.tex")
print(xtable(montant_partype_fin))


# rprop(table(base$montant_total_projet,base$`Libellé produit`))
# aggregate(as.numeric(base$montant_total_projet), by=list(`Libellé produit`=base$`Libellé produit`), FUN=sum)


# G # CO-FINANCIERS  ------------------------------------------------------

##nb de projets total 

projet <- data.frame(length(projet_nodupl$id_projet))
names(projet) <- c("Nombre total de projets AP")

##FFEM

projet_nodupl$FFEM <- ifelse(
  projet_nodupl$cofinancier_1 == "FFEM"| projet_nodupl$cofinancier_2 == "FFEM"|projet_nodupl$cofinancier_3 == "FFEM"|projet_nodupl$cofinancier_4 == "FFEM"|projet_nodupl$cofinancier_5 == "FFEM",
  1, 0)
projet_nodupl$FFEM[is.na(projet_nodupl$FFEM)] <- 0

ffem <- count(projet_nodupl$FFEM)
ffem <- ffem %>% 
  mutate(FFEM = round(freq/ sum(freq), 3)*100)

ffem <- filter(ffem, x != 0)
ffem <- ffem %>% 
  dplyr::select(-c(x))

##AFD
afd <- count(projet_nodupl$afd)
afd <- afd %>% 
  mutate( afd = round(freq/ sum(freq), 3)*100)

afd<- filter(afd, x != 0)
afd <- afd %>% 
  dplyr::select(-c(x, freq))


#Montant FFEM


fin_ffem<- aggregate(as.numeric(montant_total_projet) ~ FFEM==1, data = projet_nodupl, FUN = sum)
names(fin_ffem) <- c("x","Fin total projets FFEM")
fin_ffem<- filter(fin_ffem,x != FALSE)
fin_ffem <- fin_ffem %>% 
  dplyr::select(-c(x))


fin_ffem_moy<- aggregate(as.numeric(montant_total_projet) ~ FFEM==1, data = projet_nodupl, FUN = mean)
names(fin_ffem_moy) <- c("x","Fin moyen projets FFEM")
fin_ffem_moy<- filter(fin_ffem_moy,x != FALSE)
fin_ffem_moy <- fin_ffem_moy %>% 
  dplyr::select(-c(x))

## Tableau aggrégé
cofin <- cbind(projet,ffem,fin_ffem,fin_ffem_moy)
names(cofin)<-c("Nombre total projets","Nombre projets FFEM","Prop projets FFEM(%)","Montant total projets FFEM(€)","Montant moyen projets FFEM")

print(xtable(cofin, type = "latex"), file = "COFINFFEM.tex")
print(xtable(cofin))


## KFW

projet_nodupl$KFW <- ifelse(
  projet_nodupl$cofinancier_1 == "KFW"| projet_nodupl$cofinancier_2 == "KFW"|projet_nodupl$cofinancier_3 == "KFW"|projet_nodupl$cofinancier_4 == "KFW"|projet_nodupl$cofinancier_5 == "KFW",
  1, 0)
projet_nodupl$KFW[is.na(projet_nodupl$KFW)] <- 0

kfw <- count(projet_nodupl$KFW)
kfw <- kfw %>% 
  mutate(KFW = round(freq/ sum(freq), 3)*100)

kfw <- filter(kfw, x != 0)
kfw <- kfw %>% 
  dplyr::select(-c(x, freq))







# % financement par co-financeur sans  % AFD % Co-finance 



#  H # SUPERFICIE (MARIN ET TERRESTRE) ---------------------------------------------------------


# superficie totale-------------------------------------------------------

base_superf <- filter(base_nodupl, superficie != 0)

totalsuperficie <- sum(as.numeric(base_superf$superficie))


# Stat superficie globale -------------------------------------------------


superf <- as.array(format(summary(as.numeric(base_superf$superficie))),scientific= FALSE)
superficie<- as.data.frame(t(superf))

superficie <- superficie%>% 
  dplyr::select(-c("1st Qu.","3rd Qu."))

print(xtable(superficie, type = "latex"), file = "superficie.tex")
print(xtable(superficie))


# superficie moyenne par pays et région ------------------------------------

superf_pays <- aggregate(as.numeric(superficie) ~ pays, data = base_superf, FUN = mean)
names(superf_pays) <- c("pays","superficie moyenne")

superf_region <- aggregate(as.numeric(superficie) ~ direction_regionale, data = base_superf, FUN = mean)
names(superf_region) <- c("Régions","superficie moyenne")

print(xtable(superf_pays, type = "latex"), file = "SUPERF_pays.tex")
print(xtable(superf_pays))

print(xtable(superf_region, type = "latex"), file = "SUPERF_REG.tex")
print(xtable(superf_region))






# Histogramme de l'évolution de la superficie par année  -----------------

superf_annee <- aggregate(as.numeric(superficie) ~ annee_octroi, base_superf, sum)
names(superf_annee)<-c("annee_octroi","superficie")

superf_annee %>%
  ggplot(aes(x=annee_octroi, y=cumsum(superficie))) +
  list(
    geom_point(stat="identity"),
    ggtitle("Evolution de la superficie des aires protégées"),
    theme(plot.title =element_text(hjust=0.5)),
    xlab("Années"),
    ylab("Superficie (km2)"))


superf_annee %>%
  ggplot(mapping= aes(x=annee_octroi, y=superficie)) +
  list(
    geom_bar(width = 0.60, stat="identity",fill="blue"),
    ggtitle("Evolution de la superficie des aires protégées"),
    theme(plot.title =element_text(size = 14, face = "bold", hjust=0.5)),
    xlab("Années"),
    ylab("Superficie (km2)"))



# I# GOUVERNANCE ----------------------------------------------------------

#	% et nombre AP par type de gouvernance : idem que type de financement (aggregate !)

base_nodupl$gov_type2 <- fct_explicit_na(base_nodupl$gov_type, na_level = "Not referenced")

gouv_freq <- data.frame(table(base_nodupl$gov_type2))


prop_gouv <-round(data.frame(gouv_freq$Freq/sum(gouv_freq$Freq)),3)*100

type_gouv <- cbind(gouv_freq,prop_gouv)
names(type_gouv)<-c("Gouvernance","Nombre d'AP","Proportion d'AP (%)")

print(xtable(type_gouv, type = "latex"), file = "TYPE_GOUV.tex")
print(xtable(type_gouv))


# Diagramme gouvernance sans les AP non référencées dans la BDD WDPA ------------------

base_nodupl$gov_type3 <- fct_explicit_na(base_nodupl$gov_type, na_level = "NA")

gouv2 <- filter(base_nodupl, gov_type != "NA", gov_type != "Not Reported")

gouv2 <- count(gouv2$gov_type)

gouv2 <- gouv2 %>% 
  mutate(
    prop  = round(freq/sum(freq), 3)*100
  )

names(gouv2) <- c("Gouvernance","AP", "Proportion")


ggplot(gouv2, aes(x="", y=Proportion, fill=Gouvernance))+
  geom_bar(width = 1, stat = "identity",color="white")+
  coord_polar("y", start=0) +
  ggtitle("Types de gouvernance des AP (hors AP non répertoriées)") +
  theme(plot.title = element_text(size = 9, face = "bold",hjust=0.5)) +
  scale_fill_brewer(palette="Paired") +
  geom_label(aes(x=1.3, label = paste0(Proportion, "%")), color = "black", position = position_stack(vjust = 0.55), size=2.5, show.legend = FALSE) +
  theme_void()

