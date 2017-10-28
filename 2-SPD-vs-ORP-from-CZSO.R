# načtení výsledků a dat o obcích, srovnání proti ORP datům z CZSO

library(raster)
library(tidyverse)
library(RCzechia) # devtools::install_github("jlacko/RCzechia")
library(tmap)
library(readxl)
library(tmaptools)
library(caret)
library(RColorBrewer)
library(extrafont)

#----- načtení dat
results <- read_xlsx(path = '~/Tomio/src/SPD-ORP.xlsx',
                     sheet = 'src')

statak <- read_xlsx(path = '~/Tomio/src/ORP-CZSO.xlsx',
                     sheet = 'src')

results <- data.frame(kod = results$kod, # pouze sloupce, co potřebuju, ať v tom není bordel...
                      podil = results$podil)

# začištění datových typů
statak$ORP <- as.factor(statak$ORP)
statak$saldo_migrace <- as.integer(statak$saldo_migrace)

#spojení
wrkTomio <- results %>%
  inner_join(statak, by = c("kod" = "ORP"))


# ------ kreslení  
wobce <- append_data(shp = obce_polygony,
                     data = wrkTomio,
                     key.shp = 'KOD_ROZ', # ORP = obec s rozšířenou působností
                     key.data = 'kod',
                     ignore.na = T)

vObce <- c("Praha", "Brno", "Plzeň", "Ostrava") # hlavní města - mají obrys
velkaMesta <- obce_polygony[obce_polygony$Obec %in% vObce, ] 

bbox <- extent(republika) # trochu víc místa nahoře a dole, aby se vešel nadpis & legenda
bbox@ymax <- bbox@ymax + 0.35
bbox@ymin <- bbox@ymin - 0.15

nadpis <- "Relativní migrace 2016" # nadpis grafu
leyenda <- "Saldo migrace vztažené k populaci"  # nadpis legendy

tmTomioORP <- tm_shape(republika, bbox = bbox)+tm_borders("grey30", lwd = 1) +
  tm_shape(wobce) + tm_fill(col = "rel_migrace", palette = "RdYlGn", title = leyenda, textNA = "Jinak (vojenské újezdy)") +
  tm_shape(velkaMesta) + tm_borders("grey30", lwd = 0.5)+
  tm_style_white(nadpis, frame = F,  fontfamily = "Roboto", legend.text.size = 0.4, legend.title.size = 0.7, legend.format = list(text.separator = "-", fun=function(x) paste0(formatC(x, digits = 1, format="f"), " %")))

save_tmap(tmTomioORP, filename = "relativni-migrace.png", width = 1600, type = "cairo")
