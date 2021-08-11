install.packages("tidyverse")
library("tidyverse")
install.packages("stringr")
library("stringr")

Path1 <-'03_Data/Fahrzeug/Bestandteile_Fahrzeuge_OEM1_Typ11.csv'
Path2 <-'03_Data/Fahrzeug/Bestandteile_Fahrzeuge_OEM1_Typ12.csv'
Path3 <-'03_Data/Fahrzeug/Bestandteile_Fahrzeuge_OEM2_Typ21.csv'
Path4 <-'03_Data/Fahrzeug/Bestandteile_Fahrzeuge_OEM2_Typ22.csv'
Path_zul <- '03_Data/Zulassungen/Zulassungen_alle_Fahrzeuge.csv'

#Laden der Komponentendaten
kom_1<- read_csv2(Path1, name_repair = "universal")
kom_2 <- read_csv2(Path2)
kom_3 <- read_csv2(Path3)
kom_4 <- read_csv2(Path4)

#Anpassen der Variablennamen, so dass alle ?bereinstimmen, nicht unbedingt notwendig
kom_1 <-rename(kom_1,X1=1)
kom_2 <-rename(kom_2,X1=1)
kom_3 <-rename(kom_3,X1=1)

#Zusammenf?gen in eine Komponentenliste
kom <- kom_1 %>%
  full_join(kom_2) %>%
  full_join(kom_3) %>%
  full_join(kom_4)

#alle Autos mit einem K7-Teil auswählen
Kom_k7 <- kom %>%
  filter(grepl("^K7",ID_Karosserie)) %>%
  select( ID_Fahrzeug)
#umbennen damit der semi_join funktioniert
Kom_k7 <-rename(Kom_k7,IDNummer=ID_Fahrzeug)
#zulassungsdaten laden
zul <- read_csv2(Path_zul)
#alle Zulassungen mit K7-Teil auswählen
Kenn <- zul %>%
  semi_join(Kom_k7)
#zusammenfassen der Zulassungsdaten mit Anzahl pro LK und dann nur Dahlem ausgewählt
Dahlem_k7 <- Kenn %>%
  group_by(Gemeinden) %>%
  summarise(n())%>%
  filter(Gemeinden=='Dahlem')

Dahlem_k7
#keine Zulassungen in Dahlem! Auch nicht im gesamten Datensatz!!! Prüfen!!!
