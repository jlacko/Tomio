# uložení dat pro použití na blogu
library(DBI)
library(dbplyr)
library(RPostgreSQL)
library(tidyverse)

# Připojení databáze ----

myDb <- dbConnect(dbDriver('PostgreSQL'),
                  host = "jla-postgres.c7ymi3y4c6gx.eu-central-1.rds.amazonaws.com",
                  port = 5432,
                  user = "jindra",
                  dbname = "dbase",
                  password = rstudioapi::askForPassword("Database password"))

dbWriteTable(myDb, c("jla_blog", "tomio_results_orp"), value = results)
dbWriteTable(myDb, c("jla_blog", "tomio_results_orp_statak"), value = wrkTomio)
dbWriteTable(myDb, c("jla_blog", "tomio_korelace"), value = korelace)

dbDisconnect(myDb) # zavřít, zhasnout...