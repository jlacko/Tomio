# načtení výsledků všech stran po okresech, hledání korelace s daty
library(readxl)
library(tidyverse)

# inicializace dat ----
frmStrany <- data.frame() # prázdný data frame
strany <- c("ANO", "ODS", "STAN", "komunisti", "lidovci", "pirati", "socdem", "topka", "SPD") # devět stran v parlamentu

for (i in seq_along(strany)) {


# načtení dat ----
results <- read_xlsx(path = paste('~/Tomio/src/',strany[i], '-okresy.xlsx', sep = ''), # výsledky strany na i-tém místě
                     sheet = 'src')

cizinci <- read_xlsx(path = '~/Tomio/src/cizinci.xlsx',
                     sheet = 'src')

cukrovka <- read_xlsx(path = '~/Tomio/src/cukrovka.xlsx',
                     sheet = 'src')

potraty <- read_xlsx(path = '~/Tomio/src/potraty-umrtnost.xlsx',
                      sheet = 'src')

kriminalita <- read_xlsx(path = '~/Tomio/src/kriminalita.xlsx',
                     sheet = 'src')

obyvatel <- read_xlsx(path = '~/Tomio/src/obyvatel.xlsx',
                     sheet = 'src')

davky <- read_xlsx(path = '~/Tomio/src/pocet-davek.xlsx',
                     sheet = 'src')

pristehovani <- read_xlsx(path = '~/Tomio/src/pristehovani.xlsx',
                   sheet = 'src')

urbanizace <- read_xlsx(path = '~/Tomio/src/urbanizace.xlsx',
                   sheet = 'src')

zamestnanost <- read_xlsx(path = '~/Tomio/src/zamestanost.xlsx',
                   sheet = 'src')

# spojení dat ----
wrkTomio <- results %>%
  left_join(cizinci) %>%
  left_join(cukrovka) %>%
  left_join(potraty) %>%
  left_join(davky) %>%
  left_join(kriminalita) %>%
  left_join(obyvatel) %>%
  left_join(pristehovani) %>%
  left_join(urbanizace) %>%
  left_join(zamestnanost)

#kontrola úplnosti klíčů ----
if (0 == sum(is.na(wrkTomio)) & nrow(wrkTomio) == 77) {
  message(paste(strany[i]," spárování cajk!", sep = ""))
} else {
  message(paste(strayny[i], " stala se chyba :(", sep = ""))
}

# čištění dat

wrkTomio$diabetici <- gsub(" ", "", wrkTomio$diabetici, fixed = TRUE) # odstranit mezery
wrkTomio$diabetici <- as.numeric(wrkTomio$diabetici) # převézt z textu na číslo

wrkTomio <- wrkTomio %>%
  mutate(cizinci = cizinci / obyvatel,
         cizinci_mimo_eu = cizinci_mimo_eu / obyvatel,
         cizinci_mimo_SK = cizinci_mimo_SK / obyvatel,
         diabetici = diabetici / obyvatel,
         davky_celkem = davky_celkem / obyvatel,
         davky_bydleni = davky_bydleni / obyvatel,
         kriminalita = kriminalita / obyvatel,
         vloupani = vloupani / obyvatel,
         stehovani_plus = stehovani_plus / obyvatel,
         stehovani_minus = stehovani_minus / obyvatel,
         stehovani_saldo = stehovani_saldo / obyvatel)

asdf <- cor(wrkTomio[,3:ncol(wrkTomio)], wrkTomio[,2])

korelace <- data.frame(velicina = rownames(asdf),
                       korelace = asdf) %>%
            select(velicina,
                   korelace = podil) %>%
            mutate(strana = strany[i])

frmStrany <- rbind(frmStrany, # k data frame stran přilípnout další korelaci
                   korelace)

}