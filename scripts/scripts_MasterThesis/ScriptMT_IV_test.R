---
title: "ScriptMT_IV_test"
author: "Antoine Vuillot"
date: "2023-07-30"
output: pdf_document
editor_options: 
  chunk_output_type: console
---
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                              Packages                                    ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(ggplot2)
library(tidyverse)
library(data.table)
library(xlsx)
library(stringr)
library(ARTofR)
library(geojsonsf)
library(sf)
library(RColorBrewer)
library(dplyr)
library(terra)
library(raster)
library(tidyterra)
library(gridExtra)
library(grid)
library(cowplot)
library(scales)

#For TWFE
library(estimatr)
library(lfe)
library(fastDummies)
library(stargazer)
```
