# uložení dat pro použití na blogu
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

dbWriteTable(myDb, c("jla_blog", "tomio_results_orp"), value = results)
dbWriteTable(myDb, c("jla_blog", "tomio_results_orp_statak"), value = wrkTomio)
dbWriteTable(myDb, c("jla_blog", "tomio_okresy"), value = wrkTomio)
dbWriteTable(myDb, c("jla_blog", "strany_korelace"), value = frmStrany)
dbWriteTable(myDb, c("jla_blog", "strany_detail"), value = frmVeliciny)

dbDisconnect(myDb) # zavřít, zhasnout...