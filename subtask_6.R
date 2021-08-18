install.packages("tidyverse")
library("tidyverse")

#gesuchte Teile Nummer der Karosserie
part_nr <- 'K5-112-1122-79'

#alle n?tigen Dateipfade, !!! Nocha anpassen, damit es direkt l?uft!!!
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
#filtern nach der Karosserie ID um die Fahrzeug ID zu bekommen
car_id= kom %>%
  filter(ID_Karosserie==part_nr)
#Lesen der Zusallsungsdaten
zul <- read_csv2(Path_zul)
# Suche nach der passenden Fahrzeug ID
Kenn <- zul %>%
  filter(IDNummer== car_id$ID_Fahrzeug)
# Das Fahrzeug ist in Aschersleben registriert
Kenn