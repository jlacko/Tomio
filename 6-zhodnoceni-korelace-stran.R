# zhodnocení napočtené korelace stran a veličin
library(DBI)
library(dbplyr)
library(RPostgreSQL)
library(tidyverse)
library(grid)
library(gridExtra)

# Připojení databáze ----
myDb <- dbConnect(dbDriver('PostgreSQL'),
                  host = "db.jla-data.net",
                  port = 5432,
                  user = "tinca", # read only, což stačí...
                  dbname = "dbase",
                  password = "tinca")

results <- tbl(myDb, dbplyr::in_schema('jla_blog','strany_korelace')) %>%
  as.data.frame()

vyznamne_korelace <- results %>%
  mutate(abs_kor = abs(korelace)) %>%
  arrange(desc(abs_kor)) %>%
  group_by(strana) %>%
  mutate(poradi = row_number()) %>%
  filter(poradi == 1) %>%
  select(strana, velicina, korelace)

message("významné korelace")
print(vyznamne_korelace)
  

results <- tbl(myDb, dbplyr::in_schema('jla_blog','strany_detail')) %>%
  as.data.frame() %>%
  spread(key = strana, value = podil)


ggpWRK <- ggplot(data = results, aes(x = stehovani_plus, y = ODS)) +
  geom_point(col = "darkgoldenrod1") +
  geom_smooth(col = "cornflowerblue", method = "lm", se = F, lty = 'dotdash') +
  theme_light() +
  theme(#axis.title = element_blank(),
    axis.ticks = element_blank(),
    legend.title = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    legend.position = 'bottom') +
  ylab("podíl hlasů pro KSČM (%)")+
  xlab("počet potratů na 100 živě narozených")

grid.arrange(textGrob("Volební úspěch KSČM podle potratů", 
                      gp = gpar(fontsize = 1.7*11, fontface = "bold")), 
             ggpWRK, 
             heights = c(0.2, 1))


dbDisconnect(myDb) # zavřít, zhasnout...