# zhodnocení napočtené korelace stran a veličin
library(DBI)
library(dbplyr)
library(RPostgreSQL)
library(tidyverse)

# Připojení databáze ----
myDb <- dbConnect(dbDriver('PostgreSQL'),
                  host = "db.jla-data.net",
                  port = 5432,
                  user = "jindra",
                  dbname = "dbase",
                  password = rstudioapi::askForPassword("Database password"))

results <- tbl(myDb, dbplyr::in_schema('jla_blog','strany_detail')) %>%
  as.data.frame()




dbDisconnect(myDb) # zavřít, zhasnout...