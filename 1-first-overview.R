# test načtení, zobrazení výsledků
library(tidyverse)
library(RCzechia) # devtools::install_github("jlacko/RCzechia")
library(tmap)
library(readxl)
library(tmaptools)
library(raster)


#----- načtení dat
results <- read_xlsx(path = '~/Tomio/src/SPD-ORP.xlsx',
                     sheet = 'src')

wobce <- tmaptools::append_data(shp = obce_polygony,
                               data = results,
                               key.shp = 'KOD_ROZ', # ORP = obec s rozšířenou působností
                               key.data = 'kod',
                               ignore.na = T)

vObce <- c("Praha", "Brno", "Plzeň", "Ostrava") # hlavní města - mají obrys
velkaMesta <- obce_polygony[obce_polygony$Obec %in% vObce, ] 

#----- kreslení mapy

bbox <- extent(republika) # trochu víc místa nahoře a dole, aby se vešel nadpis & legenda
bbox@ymax <- bbox@ymax + 0.35
bbox@ymin <- bbox@ymin - 0.15

nadpis <- "Volební výsledky SPD - Tomio Okamura" # nadpis grafu
leyenda <- "Procento platných hlasů"  # nadpis legendy

tmTomioORP <- tm_shape(republika, bbox = bbox) + tm_borders("grey30", lwd = 1) +
  tm_shape(wobce) + tm_fill(col = "podil", palette = "YlOrBr", title = leyenda, textNA = "Jinak (vojenské újezdy)") +
  tm_shape(velkaMesta) + tm_borders("grey50", lwd = 0.5) +
  tm_style_white(nadpis, frame = F, legend.text.size = 0.7, legend.title.size = 1.5, legend.format = list(text.separator = "-", fun = function(x) paste0(formatC(x, digits = 0, format = "f"), " %")))

print(tmTomioORP)
