#*****************************************************************************#
#  Project: Uruguay                                                                       
#  Goal of script: Analysis of vulnerables zones                                                               
#  Author: Ingrid Dallmann                                                           
#  Last modification: 10/04/2019                                                  
#                                                
#*****************************************************************************#




#*****************************************************************************#
# I - SET UP ----
#*****************************************************************************#


# A - CLEAR ENVIRONMENT  ----
rm(list=ls())


# B -  INSTALL LIBRARIES   ----
x <- c("sf", "ggplot2", "ggmap", "rgdal", "rgeos", "maptools", "dplyr", "tidyr", "tmap", "foreign", "magrittr",  
       "reshape", "raster", "data.table", "spData", "spDataLarge", "leaflet", "mapview", "sp")
# install.packages(x) # warning: uncommenting this may take a number of minutes
# install.packages("sp") 


# C -  LOAD LIBRARIES   ----
lapply(x, library, character.only = TRUE)
# library(sp)


# D - SET PATHS ----

# D.1 Geo

# Polygons 
path_1 = "D:/Dropbox Ingrid/Dropbox/WorldBank/201903_Uruguay&disabilities/Analysis/1_Data/1_Input/MIDES/ZonasEtaf2018"
# Barrios
path_1.1 = "D:/Dropbox Ingrid/Dropbox/WorldBank/201903_Uruguay&disabilities/Analysis/1_Data/1_Input/barrios_ine_uy"
# Zonas Censales
path_1.2 = "D:/Dropbox Ingrid/Dropbox/WorldBank/201903_Uruguay&disabilities/Analysis/1_Data/1_Input/ZonasCensales"
# Segmentos Censales
path_1.3 = "D:/Dropbox Ingrid/Dropbox/WorldBank/201903_Uruguay&disabilities/Analysis/1_Data/1_Input/Segmentos"

# 7 zonas
path_2 = "D:/Dropbox Ingrid/Dropbox/WorldBank/201903_Uruguay&disabilities/Analysis/1_Data/1_Input/Zonas/SieteZonas"
path_2.1 = "D:/Dropbox Ingrid/Dropbox/WorldBank/201903_Uruguay&disabilities/Analysis/1_Data/1_Input/Zonas"

# D.2 Health

# Hospitales
path_3 = "D:/Dropbox Ingrid/Dropbox/WorldBank/201903_Uruguay&disabilities/Analysis/1_Data/1_Input/MIDES/Salud/Hospitales_UY"
# Policlinicas
path_4 = "D:/Dropbox Ingrid/Dropbox/WorldBank/201903_Uruguay&disabilities/Analysis/1_Data/1_Input/MIDES/Salud/Policlinicas"
# Centros de salud
path_5 = "D:/Dropbox Ingrid/Dropbox/WorldBank/201903_Uruguay&disabilities/Analysis/1_Data/1_Input/MIDES/Salud/Centros_de_salud_UY"

# D.3 Education

# Preescolar
# Educacion en primera infancia e inicial
path_6 = "D:/Dropbox Ingrid/Dropbox/WorldBank/201903_Uruguay&disabilities/Analysis/1_Data/1_Input/MIDES/Educacion/EducacionPreescolar/educacionenprimerainfanciaeinicial"

# Secundaria
# Ciclo basico tecnologico
path_7 = "D:/Dropbox Ingrid/Dropbox/WorldBank/201903_Uruguay&disabilities/Analysis/1_Data/1_Input/MIDES/Educacion/EducacionSecundariaGeneralYTecnica/ciclobasicotecnologico"
# Formacion profesional basica fpb plan 2007
path_8 = "D:/Dropbox Ingrid/Dropbox/WorldBank/201903_Uruguay&disabilities/Analysis/1_Data/1_Input/MIDES/Educacion/EducacionSecundariaGeneralYTecnica/formacionprofesionalbasicafpbplan2007"
# Liceos
path_9 = "D:/Dropbox Ingrid/Dropbox/WorldBank/201903_Uruguay&disabilities/Analysis/1_Data/1_Input/MIDES/Educacion/EducacionSecundariaGeneralYTecnica/liceos"

# Terciaria
# Carreras tecnicas de nivel terciario
path_10 = "D:/Dropbox Ingrid/Dropbox/WorldBank/201903_Uruguay&disabilities/Analysis/1_Data/1_Input/MIDES/Educacion/EducacionTerciaria/carrerastecnicasdenivelterciario"
# Ingeniero tecnologico
path_11 = "D:/Dropbox Ingrid/Dropbox/WorldBank/201903_Uruguay&disabilities/Analysis/1_Data/1_Input/MIDES/Educacion/EducacionTerciaria/ingenierotecnologico"

# D.4 Transport
# Lines
path_12 = "D:/Dropbox Ingrid/Dropbox/WorldBank/201903_Uruguay&disabilities/Analysis/1_Data/1_Input/Transporte/lineasOmnibus_UY" 
# Bus station
path_13 = "D:/Dropbox Ingrid/Dropbox/WorldBank/201903_Uruguay&disabilities/Analysis/1_Data/1_Input/Transporte/paradasOmnibus"
 
# D.5 Afro & disabilities 
path_14 = "D:/Dropbox Ingrid/Dropbox/WorldBank/201903_Uruguay&disabilities/Analysis/1_Data/2_Intermediate"

# E - Warnings ON/OFF ----
#options(warn=-1) # OFF
options(warn=0)   # ON




#*****************************************************************************#
# II - PLOT DATA SEPARATED ----
#*****************************************************************************#

#A - Uruguay map with "departamentos" polygons ----

#A.1 Open point SpataialPolygonDataFrame
UY_pol = readOGR(dsn = path_1, layer = "ZonasEtaf2018")
  #dsn stands for "data source name" and specifies the directory in which the file is stored, 
  #and layer which specifies the file name

#A.2 Explore data
UY_pol

head(UY_pol@data, n = 2)
sapply(UY_pol@data, class)
nrow(UY_pol)
ncol(UY_pol)

proj4string(UY_pol)
bbox(UY_pol)

# A.3 Convert spatial object to sf
UY_pol_sf = st_as_sf(UY_pol, "sf")
  # UY_pol_sf: data frame with 177 observations and 4 variables, with the 4h variable being the "geom" variable
  # UY_pol: SpatialPolygonsDataFrame
UY_pol_sf

# A.4 Plot polygons 
plot(UY_pol_sf["zonaetaf"], col="white",border="black")


# B - Barrios, Montevideo ----

# B.1 Open point SpataialPolygonDataFrame
Barrios = readOGR(dsn = path_1.1, layer = "barrios_ine_uy")

# B.2 Explore data
Barrios

head(Barrios@data, n = 2)
sapply(Barrios@data, class)
nrow(Barrios)
ncol(Barrios)

proj4string(Barrios)
bbox(Barrios)

# B.3 Convert spatial object to sf
Barrios_sf = st_as_sf(Barrios, "sf")
Barrios_sf
  # 62 polygons

#B.4 Plot polygons 
plot(Barrios_sf["nrobarrio"], col="white",border="black")


# C - Zonas Censales ----

# C.1 Open point SpataialPolygonDataFrame
ZonasCensales = readOGR(dsn = path_1.2, layer = "Zonas")

# C.2 Explore data
ZonasCensales

head(ZonasCensales@data, n = 2)
sapply(ZonasCensales@data, class)
nrow(ZonasCensales)
ncol(ZonasCensales)

proj4string(ZonasCensales)
bbox(ZonasCensales)

# C.3 Convert spatial object to sf, subset Montevideo
ZonasCensales_sf = st_as_sf(ZonasCensales, "sf")
ZonasCensales_sf

# C.4 subset Montevideo
ZonasCensales_sf$NOMBDEPTO

ZonasCensalesMont = ZonasCensales_sf[ZonasCensales_sf$NOMBDEPTO == "MONTEVIDEO",]
ZonasCensalesMont

# C.5 Plot polygons 
plot(ZonasCensalesMont["ZONA"], col="white",border="black")
  # 13621 observations


# D - Segmentos Censales ----

# D.1 Open point SpataialPolygonDataFrame
Segmentos = readOGR(dsn = path_1.3, layer = "Segmentos")

# D.2 Explore data
Segmentos

head(Segmentos@data, n = 2)
sapply(Segmentos@data, class)
nrow(Segmentos)
ncol(Segmentos)

proj4string(Segmentos)
bbox(Segmentos)

# D.3 Convert spatial object to sf
Segmentos_sf = st_as_sf(Segmentos, "sf")
Segmentos_sf

# D.4 subset Montevideo
Segmentos_sf$NOMBDEPTO

Segmentos_Mont = Segmentos_sf[Segmentos_sf$NOMBDEPTO == "MONTEVIDEO",]
Segmentos_Mont

# D.5 Plot polygons 
plot(Segmentos_Mont["SEGMENTO"], col="white",border="black")
#  observations


# E. - Commercial neighborhoods ---

# E.1. Select neighborhoods
Barrios_sf$nombbarr
comercial = Barrios_sf[Barrios_sf$nombbarr %in% c("CIUDAD VIEJA", "POCITOS"),]

# E.2 Excplore data
comercial
comercial$nombbarr

# E.3 Plot polygons
plot(comercial["nrobarrio"], col="cornsilk",border="cornsilk4")


# F. - Privileged neighborhoods ---

# F.1. Select neighborhoods
Barrios_sf$nombbarr
  # Centro
  # Barrio Sur
  # Palermo
  # Parque Rodo
  # Punta Carretas
  # Buceo
  # Malvin
  # Punta Gorda
  # Carrasco
  # Pocitos ??? already in commercial 
privileged = Barrios_sf[Barrios_sf$nombbarr %in% c("CENTRO", "BARRIO SUR", "PALERMO", "PARQUE RODO", 
                                                   "PUNTA CARRETAS", "BUCEO", "MALVIN", "PUNTA GORDA", 
                                                   "CARRASCO"),]

# F.2 Excplore data
privileged
privileged$nombbarr

# F.3 Plot polygons
plot(privileged["nrobarrio"], col="azure",border="azure4")


# G - Geolocalisation of vulnerable zones  ---- 

# G.1 7 zonas 

# G.1.1 Open SpataialPolygonDataFrame
SieteZonas = readOGR(dsn = path_2, layer = "SieteZonasPolygon")

# G.1.2 Explore data
SieteZonas

head(SieteZonas@data, n = 7)
  # zona_p depto            loc
  # 0         Chacarita     1     MONTEVIDEO
  # 1       Vista Linda     3       PROGRESO
  # 2         Ituzaing?     1     MONTEVIDEO
  # 3          Obelisco     3    LAS PIEDRAS
  # 4      Las 5 Villas     3 BARROS BLANCOS
  # 5           Marconi     1     MONTEVIDEO
  # 6 Cantera del Zorro     1     MONTEVIDEO
    # ??? depto 1 = Montevideo, depto 3 = Canelones

sapply(SieteZonas@data, class)
nrow(SieteZonas)
ncol(SieteZonas)

proj4string(SieteZonas)
bbox(SieteZonas)

# G.1.3 Convert spatial object to sf
SieteZonas_sf = st_as_sf(SieteZonas, "sf")
SieteZonas_sf

# G.1.4 Plot polygons
plot(SieteZonas_sf["zona_p"], col="red",border="black")

# G.2 Asentamiento "Aquiles Lanza" 
  # "Boix y Merino" does not appear in the MIDES database 

# G.2.1 Read the csv file 
setwd(path_2.1 )
Aquiles = read.csv("Aquiles_Lanza.csv", header = T, sep = ",", dec = ".") 
  #copy first the file imported from MIDES to .txt and then put the extension .csv 
Aquiles
str(Aquiles)
names(Aquiles)

# G.2.2 Close the polygon
Aquiles_closed = rbind(Aquiles, Aquiles[1,])

Aquiles #22 observations
Aquiles_closed #23 observations

# G.2.3 Inspect for CRS to apply
st_crs(UY_pol_sf)
utm21sCRS = st_crs(UY_pol_sf)
utm21sCRS
class(utm21sCRS)

# G.2.4 Transform in a sf polygon
Aquiles_pol <- Aquiles_closed %>%
  st_as_sf(coords = c("X", "Y"), crs = utm21sCRS) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
Aquiles_pol

# G.2.5 Plot
plot(Aquiles_pol, col="red", border="black")

# G.3 Asentamiento "17 de junio", "3 de enero", "5 de abril"

# G.3.1 Read the csv file 
setwd(path_2.1 )
JunioEneroAbril = read.csv("17Junio_3Enero_5Abril.csv", header = T, sep = ",", dec = ".") 
#copy first the file imported from MIDES to .txt and then put the extension .csv 
JunioEneroAbril
str(JunioEneroAbril)
names(JunioEneroAbril)

# G.3.2 Close the polygon
JunioEneroAbril_closed = rbind(JunioEneroAbril, JunioEneroAbril[1,])

JunioEneroAbril #16 observations
JunioEneroAbril_closed #17 observations

# G.3.3 Transform in a sf polygon
JunioEneroAbril_pol <- JunioEneroAbril %>%
  st_as_sf(coords = c("X", "Y"), crs = utm21sCRS) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
JunioEneroAbril_pol

# G.3.4 Plot
plot(JunioEneroAbril_pol, col="red", border="black")


# H - Bus lines ----

# H.1 Open point SpataialPolygonDataFrame
lineas = readOGR(dsn = path_12, layer = "lineasOmnibus_UY")

# H.2 Explore data
lineas

head(lineas@data, n = 2)
sapply(lineas@data, class)
nrow(lineas)
ncol(lineas)

proj4string(lineas)
bbox(lineas)

# H.3 Convert spatial object to sf
lineas_sf = st_as_sf(lineas, "sf")
lineas_sf

# H.4 Plot polygons 
plot(lineas_sf["cod_linea"], col="gray")


# I - Bus stops ----

# I.1 Open point SpataialPolygonDataFrame
paradas = readOGR(dsn = path_13, layer = "paradasOmnibus")

# I.2 Explore data
paradas

head(paradas@data, n = 2)
sapply(paradas@data, class)
nrow(paradas)
ncol(paradas)

proj4string(paradas)
bbox(paradas)

# I.3 Convert spatial object to sf
paradas_sf1 = st_as_sf(paradas, "sf")
paradas_sf1

# I.4 Assign an identifier
paradas_sf = paradas_sf1 %>% 
  mutate(id = row_number())

head(paradas_sf)

# I.5 Plot polygons 
plot(paradas_sf["id"], col="gray48")



#*****************************************************************************#
# III -  PLOT POLYGONEs TOGETHER ----
#*****************************************************************************#


# A - Explore attributes  ----
UY_pol_sf
Barrios_sf
SieteZonas_sf

UY_pol_sf
summary(Barrios_sf)
summary(SieteZonas_sf)


# B - Extract Montevideo, Canelones and a subset of Canelones ----

# B.1 Extract Montevideo and Canelones 
UY_pol_sf$depto

Montevideo_depto = UY_pol_sf[UY_pol_sf$depto == "MONTEVIDEO",]
Canelones_depto = UY_pol_sf[UY_pol_sf$depto == "CANELONES",]

# B.2 Subset zonas etaf of interest in Canelones
Canelones_sub = Canelones_depto[Canelones_depto$zonaetaf %in% c("0308", "0306", "0312", "0307", "0309", "0313"),]

# B.3 Plot 
plot(Montevideo_depto["zonaetaf"], col="white",border="black", reset = F)
plot(Canelones_depto["zonaetaf"], col="white",border="black", reset = F)
plot(Canelones_sub["zonaetaf"], col="white",border="black", reset = F)

# C - Bind Montevideo and Canelones ----

# C.1 Total Canelones
MontCan_depto = do.call(rbind, list(Montevideo_depto, Canelones_depto))
MontCan_depto

# C.2 Subset of Canelones 
MontCan_sub_depto = do.call(rbind, list(Montevideo_depto, Canelones_sub))
MontCan_sub_depto

# C.3 Plot
plot(MontCan_depto["zonaetaf"], col="white", border="black", reset = F)
plot(MontCan_sub_depto["zonaetaf"], col="white", border="black", reset = F)


# D - PLOT WITH 7 ZONAS  ----
plot(MontCan_sub_depto["zonaetaf"], col="white", border="black", reset = F)
plot(Barrios_sf["nrobarrio"], col="white", border="black", reset = F, add = TRUE)
plot(SieteZonas_sf["zona_p"], col="red",border="black", add = TRUE)
# text(MontCan_depto, MontCan_depto$zonaetaf, cex=0.75)




#*****************************************************************************#
# IV -  ADD HEALTH ACCESS POINTS ----
#*****************************************************************************#


# A - HOSPITALES  ----

# A.1 Open SpataialPolygonDataFrame
hospitales = readOGR(dsn = path_3, layer = "Hospitales_UY")

# A.2 Explore data
hospitales

head(hospitales@data, n = 2)
sapply(hospitales@data, class)
nrow(hospitales)
ncol(hospitales)

proj4string(hospitales)
bbox(hospitales)

# A.3 Convert spatial object to sf
hospitales_sf = st_as_sf(hospitales, "sf")
hospitales_sf

# A.4 Plot points
plot(hospitales_sf["id"], pch=19, col = "blue", cex = 1)

# A.5 Extract Montevideo and Canelones
hospitales_sf$departamen

Montevideo_hospi = hospitales_sf[hospitales_sf$departamen == "MONTEVIDEO", ]
Montevideo_hospi
  #14 hospitales

Canelones_hospi = hospitales_sf[hospitales_sf$departamen == "CANELONES", ]
Canelones_hospi
  #3 hospitales


# A.6 Bind Montevideo and Canelones
MontCan_hospi = do.call(rbind, list(Montevideo_hospi, Canelones_hospi))
MontCan_hospi


# B - POLICLINICAS  ----

# B.1 Open SpataialPolygonDataFrame
policlinicas = readOGR(dsn = path_4, layer = "Policlinicas")

# B.2 Explore data
policlinicas

head(policlinicas@data, n = 2)
sapply(policlinicas@data, class)
nrow(policlinicas)
ncol(policlinicas)

proj4string(policlinicas)
bbox(policlinicas)

# B.3 Convert spatial object to sf
policlinicas_sf = st_as_sf(policlinicas, "sf")
policlinicas_sf

# B.4 Plot points
plot(policlinicas_sf["id"], pch=19, col = "cornflowerblue", cex = 1)


# B.5 Extract Montevideo
policlinicas_sf$departamen

Montevideo_policlinica = policlinicas_sf[policlinicas_sf$departamen == "MONTEVIDEO", ]
Montevideo_policlinica
#103 policlinicas

# B.6 Canelones
Canelones_policlinica = policlinicas_sf[policlinicas_sf$departamen == "CANELONES", ]
Canelones_policlinica
#85 hospitales

# B.7 Bind Montevideo and Canelones
MontCan_policlinica = do.call(rbind, list(Montevideo_policlinica, Canelones_policlinica))
MontCan_policlinica


# C - CENTROS DE SALUD  ----

# C.1 Open SpataialPolygonDataFrame
CSalud = readOGR(dsn = path_5, layer = "Centros_de_salud_UY")

# C.2 Explore data
CSalud

head(CSalud@data, n = 2)
sapply(CSalud@data, class)
nrow(CSalud)
ncol(CSalud)

proj4string(CSalud)
bbox(CSalud)

# C.3 Convert spatial object to sf
CSalud_sf = st_as_sf(CSalud, "sf")
CSalud_sf

# C.4 Plot points
plot(CSalud_sf["id"], pch=19, col = "cyan3", cex = 1)


# C.5 Extract Montevideo
CSalud_sf$departamen

Montevideo_CSalud = CSalud_sf[CSalud_sf$departamen == "MONTEVIDEO", ]
Montevideo_CSalud
  #11 policlinicas

Canelones_CSalud = CSalud_sf[CSalud_sf$departamen == "CANELONES", ]
Canelones_CSalud
  #

# C.6 Bind Montevideo and Canelones
MontCan_CSalud = do.call(rbind, list(Montevideo_CSalud, Canelones_CSalud))
MontCan_CSalud




#*****************************************************************************#
# V -  ADD EDUCATION ACCESS POINTS ----
#*****************************************************************************#


# A - Educacion en primera infancia e inicial  ----

# A.1 Open SpataialPolygonDataFrame
preescolar = readOGR(dsn = path_6, layer = "educacionenprimerainfanciaeinicial")

# A.2 Explore data
preescolar

head(preescolar@data, n = 2)
sapply(preescolar@data, class)
nrow(preescolar)
ncol(preescolar)

proj4string(preescolar)
bbox(preescolar)

# A.3 Convert spatial object to sf
preescolar_sf = st_as_sf(preescolar, "sf")
preescolar_sf

# A.4 Plot points
plot(preescolar_sf["ID"], pch=19, col = "cyan3", cex = 1)


# A.5 Extract Montevideo
preescolar_sf$DEPARTAMEN

Montevideo_preescolar = preescolar_sf[preescolar_sf$DEPARTAMEN == "MONTEVIDEO", ]
Montevideo_preescolar
  #63 preescolares

Canelones_preescolar = preescolar_sf[preescolar_sf$DEPARTAMEN == "CANELONES", ]
Canelones_preescolar
  #32 preescolares

# A.6 Bind Montevideo and Canelones
MontCan_preescolar = do.call(rbind, list(Montevideo_preescolar, Canelones_preescolar))
MontCan_preescolar


# B - Ciclo basico tecnologico  ----

# B.1 Open SpataialPolygonDataFrame
CBT = readOGR(dsn = path_7, layer = "ciclobasicotecnologico")

# B.2 Explore data
CBT

head(CBT@data, n = 2)
sapply(CBT@data, class)
nrow(CBT)
ncol(CBT)

proj4string(CBT)
bbox(CBT)

# B.3 Convert spatial object to sf
CBT_sf = st_as_sf(CBT, "sf")
CBT_sf

# B.4 Plot points
plot(CBT_sf["ID"], pch=19, col = "coral2", cex = 1)

# B.5 Extract Montevideo
CBT_sf$DEPARTAMEN

Montevideo_CBT = CBT_sf[CBT_sf$DEPARTAMEN == "MONTEVIDEO", ]
Montevideo_CBT
  #150 CBT

Canelones_CBT = CBT_sf[CBT_sf$DEPARTAMEN == "CANELONES", ]
Canelones_CBT
  #19 CBT

# B.6 Bind Montevideo and Canelones
MontCan_CBT = do.call(rbind, list(Montevideo_CBT, Canelones_CBT))
MontCan_CBT


# C - Formacion profesional basica fpb plan 2007  ----

# C.1 Open SpataialPolygonDataFrame
FPB = readOGR(dsn = path_8, layer = "formacionprofesionalbasicafpbplan2007")

# C.2 Explore data
FPB

head(FPB@data, n = 2)
sapply(FPB@data, class)
nrow(FPB)
ncol(FPB)

proj4string(FPB)
bbox(FPB)

# C.3 Convert spatial object to sf
FPB_sf = st_as_sf(FPB, "sf")
FPB_sf

# C.4 Plot points
plot(FPB_sf["ID"], pch=19, col = "chocolate", cex = 1)


# C.5 Extract Montevideo
FPB_sf$DEPARTAMEN

Montevideo_FPB = FPB_sf[FPB_sf$DEPARTAMEN == "MONTEVIDEO", ]
Montevideo_FPB
#44 FPB

Canelones_FPB = FPB_sf[FPB_sf$DEPARTAMEN == "CANELONES", ]
Canelones_FPB
# FPB

# C.6 Bind Montevideo and Canelones
MontCan_FPB = do.call(rbind, list(Montevideo_FPB, Canelones_FPB))
MontCan_FPB


# D - Liceos  ----

# D.1 Open SpataialPolygonDataFrame
liceos = readOGR(dsn = path_9, layer = "liceos")

# D.2 Explore data
liceos

head(liceos@data, n = 2)
sapply(liceos@data, class)
nrow(liceos)
ncol(liceos)

proj4string(liceos)
bbox(liceos)

# D.3 Convert spatial object to sf
liceos_sf = st_as_sf(liceos, "sf")
liceos_sf

# D.4 Plot points
plot(liceos_sf["ID"], pch=19, col = "brown1", cex = 1)


# D.5 Extract Montevideo
liceos_sf$DEPARTAMEN

Montevideo_liceos = liceos_sf[liceos_sf$DEPARTAMEN == "MONTEVIDEO", ]
Montevideo_liceos
  #74 liceos

Canelones_liceos = liceos_sf[liceos_sf$DEPARTAMEN == "CANELONES", ]
Canelones_liceos
  # 51 liceos

# D.6 Bind Montevideo and Canelones
MontCan_liceos = do.call(rbind, list(Montevideo_liceos, Canelones_liceos))
MontCan_liceos


# E - Carreras tecnicas de nivel terciario  ----

# E.1 Open SpataialPolygonDataFrame
CTT = readOGR(dsn = path_10, layer = "carrerastecnicasdenivelterciario")

# E.2 Explore data
CTT

head(CTT@data, n = 2)
sapply(CTT@data, class)
nrow(CTT)
ncol(CTT)

proj4string(CTT)
bbox(CTT)

# E.3 Convert spatial object to sf
CTT_sf = st_as_sf(CTT, "sf")
CTT_sf

# E.4 Plot points
plot(CTT_sf["ID"], pch=19, col = "darkgoldenrod1", cex = 1)


# E.5 Extract Montevideo
CTT_sf$DEPARTAMEN

Montevideo_CTT = CTT_sf[CTT_sf$DEPARTAMEN == "MONTEVIDEO", ]
Montevideo_CTT
  #

Canelones_CTT = CTT_sf[CTT_sf$DEPARTAMEN == "CANELONES", ]
Canelones_CTT
  # 

# E.6 Bind Montevideo and Canelones
MontCan_CTT = do.call(rbind, list(Montevideo_CTT, Canelones_CTT))
MontCan_CTT


# F - Ingeniero tecnologico  ----

# F.1 Open SpataialPolygonDataFrame
IT = readOGR(dsn = path_11, layer = "ingenierotecnologico")

# F.2 Explore data
IT

head(IT@data, n = 2)
sapply(IT@data, class)
nrow(IT)
ncol(IT)

proj4string(IT)
bbox(IT)

# F.3 Convert spatial object to sf
IT_sf = st_as_sf(IT, "sf")
IT_sf

# F.4 Plot points
plot(IT_sf["ID"], pch=19, col = "darkgoldenrod4", cex = 1)


# F.5 Extract Montevideo
IT_sf$DEPARTAMEN

Montevideo_IT = IT_sf[IT_sf$DEPARTAMEN == "MONTEVIDEO", ]
Montevideo_IT
  # 44 

Canelones_IT = IT_sf[IT_sf$DEPARTAMEN == "CANELONES", ]
Canelones_IT
  # 0 




#*****************************************************************************#
# VI -  AFRO POPULATION ----
#*****************************************************************************#


# A - Polygon
Barrios

plot(Barrios)
  #62 polygones
head(Barrios@data, n=2)


# B - READ AFRO POP DATA ----
setwd(path_14)
AfroDisc = read.csv("Afro&Disc_pop_barrios_2011.csv", header = T, sep = ",", dec = ".", stringsAsFactors = FALSE) 
AfroDisc


# C - MERGE WITH BARRIOS POLYGONS ----
head(Barrios$nrobarrio)
head(AfroDisc$nrobarrio)

Barrios@data = left_join(Barrios@data, AfroDisc, by = 'nrobarrio')

head(Barrios@data, n = 2)

Barrios@data

summary(Barrios$afro_per)

# D - Convert spatial object to sf ----
BarriosAfroDisc_sf = st_as_sf(Barrios, "sf")
BarriosAfroDisc_sf

# E - Plot ---
plot(BarriosAfroDisc_sf["afro_per"],  nbreaks = 6, breaks = "equal", main = "Afro population, Montevideo, 2011")
qtm(shp = BarriosAfroDisc_sf, fill = "afro_per", fill.palette = "Purples", fill.title = "Afro population, Montevideo, 2011")




#*****************************************************************************#
# VII -  DISABILITY POPULATION ----
#*****************************************************************************#


# A - Explore data ----
head(Barrios@data, n = 2)
summary(Barrios$con_dif_per)


# B - Plot ---
plot(BarriosAfroDisc_sf["con_dif_per"],  nbreaks = 6, breaks = "equal", main = "Population with disabilities, Montevideo, 2011")
qtm(shp = BarriosAfroDisc_sf, fill = "con_dif_per", fill.palette = "Blues", fill.title = "Population with disabilities, Montevideo, 2011")


#*****************************************************************************#
# VIII -  PLOT ALL TOGETHER ----
#*****************************************************************************#


# A - Zonas ----

# Montevideo + subset Canelones
tm_shape(MontCan_sub_depto) + tm_borders() +
  # Barriospoligons  + afro population 
  tm_shape(BarriosAfroDisc_sf) + tm_borders() + tmap_style("white") +
  # Siete Zonas 
  tm_shape(SieteZonas_sf) + tm_borders() + tm_fill(col = "red") +
  tm_add_legend(type = "symbol", labels = "Zona vulnerable", col = "red", shape = 15) + 
  # Asentamientos 1
  tm_shape(Aquiles_pol) + tm_borders() + tm_fill(col = "red") + 
  # Asentamientos 2
  tm_shape(JunioEneroAbril_pol) + tm_borders() + tm_fill(col = "red") + 
  # Zona comercial
  tm_shape(comercial) + tm_borders(col = "cornsilk4") + tm_fill(col = "burlywood3") + 
  tm_add_legend(type = "symbol", labels = "Zona comercial", col = "burlywood3", shape = 15) + 
  # Zona privilegiada
  tm_shape(privileged) + tm_borders(col = "azure") + tm_fill(col = "azure2") + 
  tm_add_legend(type = "symbol", labels = "Zona privilediada", col = "azure2", shape = 15)

# Only Montevideo
tm_shape(Montevideo_depto) + tm_borders() +
  # Barriospoligons  + afro population 
  tm_shape(BarriosAfroDisc_sf) + tm_borders() + tmap_style("white") +
  # Siete Zonas 
  tm_shape(SieteZonas_sf) + tm_borders() + tm_fill(col = "red") +
  tm_add_legend(type = "symbol", labels = "Zona vulnerable", col = "red", shape = 15) + 
  # Asentamientos 1
  tm_shape(Aquiles_pol) + tm_borders() + tm_fill(col = "red") + 
  # Asentamientos 2
  tm_shape(JunioEneroAbril_pol) + tm_borders() + tm_fill(col = "red") + 
  # Zona comercial
  tm_shape(comercial) + tm_borders(col = "cornsilk4") + tm_fill(col = "burlywood3") + 
  tm_add_legend(type = "symbol", labels = "Zona comercial", col = "burlywood3", shape = 15) + 
  # Zona privilegiada
  tm_shape(privileged) + tm_borders(col = "azure") + tm_fill(col = "azure2") + 
  tm_add_legend(type = "symbol", labels = "Zona privilediada", col = "azure2", shape = 15)  
  

# B - Transporte ----
# Only Montevideo
tm_shape(Montevideo_depto) + tm_borders() +
  # Barriospoligons  + afro population 
  tm_shape(BarriosAfroDisc_sf) + tm_borders() + tmap_style("white") +
  # Siete Zonas 
  tm_shape(SieteZonas_sf) + tm_borders() + tm_fill(col = "red") +
    tm_add_legend(type = "symbol", labels = "Zona vulnerable", col = "red", shape = 15) + 
  # Asentamientos 1
  tm_shape(Aquiles_pol) + tm_borders() + tm_fill(col = "red") + 
  # Asentamientos 2
  tm_shape(JunioEneroAbril_pol) + tm_borders() + tm_fill(col = "red") + 
  # Zona comercial
  tm_shape(comercial) + tm_borders(col = "cornsilk4") + tm_fill(col = "burlywood3") + 
    tm_add_legend(type = "symbol", labels = "Zona comercial", col = "burlywood3", shape = 15) + 
  # Zona privilegiada
  tm_shape(privileged) + tm_borders(col = "azure") + tm_fill(col = "azure2") + 
    tm_add_legend(type = "symbol", labels = "Zona privilediada", col = "azure2", shape = 15) +  
  # Lineas
  tm_shape(lineas_sf) + tm_lines(col="gray") +
    tm_add_legend(type = "symbol", labels = "Linea de bus", col = "gray", shape = 45) + 
  # Paradas
  tm_shape(paradas_sf)  + tm_dots(size = 0.1, alpha = 0.5, col = "gray35",  shape = 1) +
    tm_add_legend(type = "symbol", labels = "Parada de bus", col = "gray35", shape = 1)  

# C - Health ----  
# Montevideo + subset Canelones
tm_shape(MontCan_sub_depto) + tm_borders() +
  # Barriospoligons  + afro population 
  tm_shape(BarriosAfroDisc_sf) + tm_borders() + tmap_style("white") +
  # Siete Zonas 
  tm_shape(SieteZonas_sf) + tm_borders() + tm_fill(col = "red") +
    tm_add_legend(type = "symbol", labels = "Zona vulnerable", col = "red", shape = 15) + 
  # Asentamientos 1
  tm_shape(Aquiles_pol) + tm_borders() + tm_fill(col = "red") + 
  # Asentamientos 2
  tm_shape(JunioEneroAbril_pol) + tm_borders() + tm_fill(col = "red") + 
  # Zona comercial
  tm_shape(comercial) + tm_borders(col = "cornsilk4") + tm_fill(col = "burlywood3") + 
    tm_add_legend(type = "symbol", labels = "Zona comercial", col = "burlywood3", shape = 15) + 
  # Zona privilegiada
  tm_shape(privileged) + tm_borders(col = "azure") + tm_fill(col = "azure2") + 
    tm_add_legend(type = "symbol", labels = "Zona privilediada", col = "azure2", shape = 15) + 
  # Lineas
  tm_shape(lineas_sf) + tm_lines(col="gray") +
    tm_add_legend(type = "symbol", labels = "Linea de bus", col = "gray", shape = 45) + 
  # Hospitales 
  tm_shape(MontCan_hospi) + tm_dots(size = 0.1, alpha = 0.5, col = "blue") + 
    tm_add_legend(type = "symbol", labels = "Hospital", col = "blue", shape = 19) + 
  # Policlinicas
  tm_shape(MontCan_policlinica) + tm_dots(size = 0.1, alpha = 0.5, col = "cornflowerblue") + 
    tm_add_legend(type = "symbol", labels = "Policlinica", col = "cornflowerblue", shape = 19) +
  # Centros de Salud
  tm_shape(MontCan_CSalud) + tm_dots(size = 0.1, alpha = 0.5, col = "cyan3") + 
    tm_add_legend(type = "symbol", labels = "Centro de Salud", col = "cyan3", shape = 19)

# Montevideo
tm_shape(Montevideo_depto) + tm_borders() +
  # Barriospoligons  + afro population 
  tm_shape(BarriosAfroDisc_sf) + tm_borders() + tmap_style("white") +
  # Siete Zonas 
  tm_shape(SieteZonas_sf) + tm_borders() + tm_fill(col = "red") +
  tm_add_legend(type = "symbol", labels = "Zona vulnerable", col = "red", shape = 15) + 
  # Asentamientos 1
  tm_shape(Aquiles_pol) + tm_borders() + tm_fill(col = "red") + 
  # Asentamientos 2
  tm_shape(JunioEneroAbril_pol) + tm_borders() + tm_fill(col = "red") + 
  # Zona comercial
  tm_shape(comercial) + tm_borders(col = "cornsilk4") + tm_fill(col = "burlywood3") + 
  tm_add_legend(type = "symbol", labels = "Zona comercial", col = "burlywood3", shape = 15) + 
  # Zona privilegiada
  tm_shape(privileged) + tm_borders(col = "azure") + tm_fill(col = "azure2") + 
  tm_add_legend(type = "symbol", labels = "Zona privilediada", col = "azure2", shape = 15) + 
  # Lineas
  tm_shape(lineas_sf) + tm_lines(col="gray") +
  tm_add_legend(type = "symbol", labels = "Linea de bus", col = "gray", shape = 45) + 
  # Hospitales 
  tm_shape(Montevideo_hospi) + tm_dots(size = 0.1, alpha = 0.5, col = "blue") + 
  tm_add_legend(type = "symbol", labels = "Hospital", col = "blue", shape = 19) + 
  # Policlinicas
  tm_shape(Montevideo_policlinica) + tm_dots(size = 0.1, alpha = 0.5, col = "cornflowerblue") + 
  tm_add_legend(type = "symbol", labels = "Policlinica", col = "cornflowerblue", shape = 19) +
  # Centros de Salud
  tm_shape(Montevideo_CSalud) + tm_dots(size = 0.1, alpha = 0.5, col = "cyan3") + 
  tm_add_legend(type = "symbol", labels = "Centro de Salud", col = "cyan3", shape = 19)


# D - Education ---- 
# Montevideo + subset Canelones
tm_shape(MontCan_sub_depto) + tm_borders() +
  # Barriospoligons  + afro population 
  tm_shape(BarriosAfroDisc_sf) + tm_borders() + tmap_style("white") +
  # Siete Zonas 
  tm_shape(SieteZonas_sf) + tm_borders() + tm_fill(col = "red") + 
    tm_add_legend(type = "symbol", labels = "Zona vulnerable", col = "red", shape = 15) +
  # Asentamientos 1
  tm_shape(Aquiles_pol) + tm_borders() + tm_fill(col = "red") + 
  # Asentamientos 2
  tm_shape(JunioEneroAbril_pol) + tm_borders() + tm_fill(col = "red") + 
  # Zona comercial
  tm_shape(comercial) + tm_borders(col = "cornsilk4") + tm_fill(col = "burlywood3") + 
    tm_add_legend(type = "symbol", labels = "Zona comercial", col = "burlywood3", shape = 15) + 
  # Zona privilegiada
  tm_shape(privileged) + tm_borders(col = "azure") + tm_fill(col = "azure2") + 
    tm_add_legend(type = "symbol", labels = "Zona privilediada", col = "azure2", shape = 15) + 
  # Lineas
  tm_shape(lineas_sf) + tm_lines(col="gray") +
  tm_add_legend(type = "symbol", labels = "Linea de bus", col = "gray", shape = 45) + 
  # Preescolar 
  tm_shape(MontCan_preescolar) + tm_dots(size = 0.1, alpha = 0.5, col = "darkslateblue") + 
  tm_add_legend(type = "symbol", labels = "Preescolar", col = "darkslateblue", shape = 19) +  
  # Ciclo Basico Tecnico 
  tm_shape(MontCan_CBT) + tm_dots(size = 0.1, alpha = 0.5, col = "darkviolet") + 
  tm_add_legend(type = "symbol", labels = "Ciclo Basico Tecnico", col = "darkviolet", shape = 19) + 
  # Formacion profesional basica
  tm_shape(MontCan_FPB) + tm_dots(size = 0.1, alpha = 0.5, col = "deeppink2") + 
  tm_add_legend(type = "symbol", labels = "Formacion profesional basica", col = "deeppink2", shape = 19) + 
  # Liceos
  tm_shape(MontCan_liceos) + tm_dots(size = 0.1, alpha = 0.5, col = "darkmagenta") + 
  tm_add_legend(type = "symbol", labels = "Liceos", col = "darkmagenta", shape = 19) + 
  # Carreras Tecnicas Terciario
  tm_shape(MontCan_CTT) + tm_dots(size = 0.1, alpha = 0.5, col = "darkorchid1") + 
  tm_add_legend(type = "symbol", labels = "Carreras Tecnicas Terciario", col = "darkorchid1", shape = 19) 
  
# Montevideo
tm_shape(Montevideo_depto) + tm_borders() +
  # Barriospoligons  + afro population 
  tm_shape(BarriosAfroDisc_sf) + tm_borders() + tmap_style("white") +
  # Siete Zonas 
  tm_shape(SieteZonas_sf) + tm_borders() + tm_fill(col = "red") + 
  tm_add_legend(type = "symbol", labels = "Zona vulnerable", col = "red", shape = 15) +
  # Asentamientos 1
  tm_shape(Aquiles_pol) + tm_borders() + tm_fill(col = "red") + 
  # Asentamientos 2
  tm_shape(JunioEneroAbril_pol) + tm_borders() + tm_fill(col = "red") + 
  # Zona comercial
  tm_shape(comercial) + tm_borders(col = "cornsilk4") + tm_fill(col = "burlywood3") + 
  tm_add_legend(type = "symbol", labels = "Zona comercial", col = "burlywood3", shape = 15) + 
  # Zona privilegiada
  tm_shape(privileged) + tm_borders(col = "azure") + tm_fill(col = "azure2") + 
  tm_add_legend(type = "symbol", labels = "Zona privilediada", col = "azure2", shape = 15) + 
  # Lineas
  tm_shape(lineas_sf) + tm_lines(col="gray") +
  tm_add_legend(type = "symbol", labels = "Linea de bus", col = "gray", shape = 45) + 
  # Preescolar 
  tm_shape(Montevideo_preescolar) + tm_dots(size = 0.1, alpha = 0.5, col = "darkslateblue") + 
  tm_add_legend(type = "symbol", labels = "Preescolar", col = "darkslateblue", shape = 19) +  
  # Ciclo Basico Tecnico 
  tm_shape(Montevideo_CBT) + tm_dots(size = 0.1, alpha = 0.5, col = "darkviolet") + 
  tm_add_legend(type = "symbol", labels = "Ciclo Basico Tecnico", col = "darkviolet", shape = 19) + 
  # Formacion profesional basica
  tm_shape(Montevideo_FPB) + tm_dots(size = 0.1, alpha = 0.5, col = "deeppink2") + 
  tm_add_legend(type = "symbol", labels = "Formacion profesional basica", col = "deeppink2", shape = 19) + 
  # Liceos
  tm_shape(Montevideo_liceos) + tm_dots(size = 0.1, alpha = 0.5, col = "darkmagenta") + 
  tm_add_legend(type = "symbol", labels = "Liceos", col = "darkmagenta", shape = 19) + 
  # Carreras Tecnicas Terciario
  tm_shape(Montevideo_CTT) + tm_dots(size = 0.1, alpha = 0.5, col = "darkorchid1") + 
  tm_add_legend(type = "symbol", labels = "Carreras Tecnicas Terciario", col = "darkorchid1", shape = 19) 


# E - With Afro population ----

# E.1 Health  
# Montevideo + subset Canelones
tm_shape(Montevideo_depto) + tm_borders() +
# Barriospoligons  + afro population 
  tm_shape(BarriosAfroDisc_sf) + tm_polygons("afro_per", title="Afro population, Montevideo, 2011 (%)") + 
    tmap_style("white") +
# Siete Zonas 
  tm_shape(SieteZonas_sf) + tm_borders() + tm_fill(col = "red") + 
# Asentamientos 1
  tm_shape(Aquiles_pol) + tm_borders() + tm_fill(col = "red") + 
# Asentamientos 2
  tm_shape(JunioEneroAbril_pol) + tm_borders() + tm_fill(col = "red") + 
# Hospitales 
  tm_shape(Montevideo_hospi) + tm_dots(size = 0.1, alpha = 0.5, col = "blue") + 
    tm_add_legend(type = "symbol", labels = "Hospital", col = "blue", shape = 19) + 
# Policlinicas
  tm_shape(Montevideo_policlinica) + tm_dots(size = 0.1, alpha = 0.5, col = "cornflowerblue") + 
    tm_add_legend(type = "symbol", labels = "Policlinica", col = "cornflowerblue", shape = 19) +
# Centros de Salud
  tm_shape(Montevideo_CSalud) + tm_dots(size = 0.1, alpha = 0.5, col = "cyan3") + 
    tm_add_legend(type = "symbol", labels = "Centro de Salud", col = "cyan3", shape = 19)

# E.2 Education  
# Montevideo + subset Canelones
tm_shape(Montevideo_depto) + tm_borders() +
  # Barriospoligons  + afro population 
  tm_shape(BarriosAfroDisc_sf) + tm_polygons("afro_per", title="Afro population, Montevideo, 2011 (%)") + 
  tmap_style("white") +
  # Siete Zonas 
  tm_shape(SieteZonas_sf) + tm_borders() + tm_fill(col = "red") + 
  # Asentamientos 1
  tm_shape(Aquiles_pol) + tm_borders() + tm_fill(col = "red") + 
  # Asentamientos 2
  tm_shape(JunioEneroAbril_pol) + tm_borders() + tm_fill(col = "red") + 
  # Preescolar 
  tm_shape(Montevideo_preescolar) + tm_dots(size = 0.1, alpha = 0.5, col = "darkslateblue") + 
  tm_add_legend(type = "symbol", labels = "Preescolar", col = "darkslateblue", shape = 19) +  
  # Ciclo Basico Tecnico 
  tm_shape(Montevideo_CBT) + tm_dots(size = 0.1, alpha = 0.5, col = "darkviolet") + 
  tm_add_legend(type = "symbol", labels = "Ciclo Basico Tecnico", col = "darkviolet", shape = 19) + 
  # Formacion profesional basica
  tm_shape(Montevideo_FPB) + tm_dots(size = 0.1, alpha = 0.5, col = "deeppink2") + 
  tm_add_legend(type = "symbol", labels = "Formacion profesional basica", col = "deeppink2", shape = 19) + 
  # Liceos
  tm_shape(Montevideo_liceos) + tm_dots(size = 0.1, alpha = 0.5, col = "darkmagenta") + 
  tm_add_legend(type = "symbol", labels = "Liceos", col = "darkmagenta", shape = 19) + 
  # Carreras Tecnicas Terciario
  tm_shape(Montevideo_CTT) + tm_dots(size = 0.1, alpha = 0.5, col = "darkorchid1") + 
  tm_add_legend(type = "symbol", labels = "Carreras Tecnicas Terciario", col = "darkorchid1", shape = 19) 


# F - With population with Disabilities ----

# F.1 Health  
# Montevideo 
tm_shape(Montevideo_depto) + tm_borders() +
  # Barriospoligons  + afro population 
  tm_shape(BarriosAfroDisc_sf) + tm_polygons("con_dif_per", title="Pop. with disabilities, Montevideo, 2011 (%)") + 
  tmap_style("white") +
  # Siete Zonas 
  tm_shape(SieteZonas_sf) + tm_borders() + tm_fill(col = "red") + 
  # Asentamientos 1
  tm_shape(Aquiles_pol) + tm_borders() + tm_fill(col = "red") + 
  # Asentamientos 2
  tm_shape(JunioEneroAbril_pol) + tm_borders() + tm_fill(col = "red") + 
  # Hospitales 
  tm_shape(Montevideo_hospi) + tm_dots(size = 0.1, alpha = 0.5, col = "blue") + 
  tm_add_legend(type = "symbol", labels = "Hospital", col = "blue", shape = 19) + 
  # Policlinicas
  tm_shape(Montevideo_policlinica) + tm_dots(size = 0.1, alpha = 0.5, col = "cornflowerblue") + 
  tm_add_legend(type = "symbol", labels = "Policlinica", col = "cornflowerblue", shape = 19) +
  # Centros de Salud
  tm_shape(Montevideo_CSalud) + tm_dots(size = 0.1, alpha = 0.5, col = "cyan3") + 
  tm_add_legend(type = "symbol", labels = "Centro de Salud", col = "cyan3", shape = 19)


# F.2 Education  
# Montevideo 
tm_shape(Montevideo_depto) + tm_borders() +
  # Barriospoligons  + afro population 
  tm_shape(BarriosAfroDisc_sf) + tm_polygons("con_dif_per", title="Pop. with disabilities, Montevideo, 2011 (%)") + 
  tmap_style("white") +
  # Siete Zonas 
  tm_shape(SieteZonas_sf) + tm_borders() + tm_fill(col = "red") + 
  # Asentamientos 1
  tm_shape(Aquiles_pol) + tm_borders() + tm_fill(col = "red") + 
  # Asentamientos 2
  tm_shape(JunioEneroAbril_pol) + tm_borders() + tm_fill(col = "red") + 
  # Preescolar 
  tm_shape(Montevideo_preescolar) + tm_dots(size = 0.1, alpha = 0.5, col = "darkslateblue") + 
  tm_add_legend(type = "symbol", labels = "Preescolar", col = "darkslateblue", shape = 19) +  
  # Ciclo Basico Tecnico 
  tm_shape(Montevideo_CBT) + tm_dots(size = 0.1, alpha = 0.5, col = "darkviolet") + 
  tm_add_legend(type = "symbol", labels = "Ciclo Basico Tecnico", col = "darkviolet", shape = 19) + 
  # Formacion profesional basica
  tm_shape(Montevideo_FPB) + tm_dots(size = 0.1, alpha = 0.5, col = "deeppink2") + 
  tm_add_legend(type = "symbol", labels = "Formacion profesional basica", col = "deeppink2", shape = 19) + 
  # Liceos
  tm_shape(Montevideo_liceos) + tm_dots(size = 0.1, alpha = 0.5, col = "darkmagenta") + 
  tm_add_legend(type = "symbol", labels = "Liceos", col = "darkmagenta", shape = 19) + 
  # Carreras Tecnicas Terciario
  tm_shape(Montevideo_CTT) + tm_dots(size = 0.1, alpha = 0.5, col = "darkorchid1") + 
  tm_add_legend(type = "symbol", labels = "Carreras Tecnicas Terciario", col = "darkorchid1", shape = 19) 




#*****************************************************************************#
#  - END OF SCRIPT ----
#*****************************************************************************#

