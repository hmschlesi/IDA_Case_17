#main task collect all car ids for failed cars/parts/components
#T2 component file is already fixed!!!
install.packages("readr")
library("readr")
install.packages("stringr")
library("stringr")
install.packages("tidyverse")
library("tidyverse")

Path_T2_1 <- '03_Data/Einzelteil/Einzelteil_T02_p1.txt'
Path_T2_2 <- '03_Data/Einzelteil/Einzelteil_T02_p2.txt'

T2_1 <- read_table(Path_T2_1)
T2_2 <- read_table(Path_T2_2)

T2_1 <- T2_1 %>%
  select(!ends_with(".y") )%>%
  select(c(-id, -X1)) %>%
  select(!starts_with("Fehlerhaft"))%>%
  rename_with(~ gsub(".x", ".T2", .x))

T2_2 <- T2_2 %>%
  select(!ends_with(".y") )%>%
  select(c(-X17,-id, -X1)) %>%
  select(!starts_with("Fehlerhaft"))%>%
  rename_with(~ gsub(".x", ".T2", .x))

T2<- T2_1%>%
  full_join(T2_2)


Path_K1BE1_kom <- '03_Data/Komponente/Bestandteile_Komponente_K1BE1.csv'
Path_K1BE2_kom <- '03_Data/Komponente/Bestandteile_Komponente_K1BE2.csv' 

K1BE1_kom <- read_csv2(Path_K1BE1_kom)
K1BE1_kom <- K1BE1_kom%>%
  rename(ID_Motor=ID_K1BE1)
K1BE2_kom <- read_csv2(Path_K1BE2_kom)
K1BE2_kom <- K1BE2_kom %>%
  rename(ID_Motor=ID_K1BE2)

Motors <- K1BE1_kom%>%
  full_join(K1BE2_kom)%>%
  select(c(-ID_T1,-ID_T7,-ID_T8,-ID_T3,-1,-ID_T4))

T2_mot <- T2%>%
  left_join(Motors, by=c("ID_T02.T2"="ID_T2")) 


Path_C_11 <- '03_Data/Fahrzeug/Bestandteile_Fahrzeuge_OEM1_Typ11.csv'
Path_C_12 <- '03_Data/Fahrzeug/Bestandteile_Fahrzeuge_OEM1_Typ12.csv'
Path_C_21 <- '03_Data/Fahrzeug/Bestandteile_Fahrzeuge_OEM2_Typ21.csv'
Path_C_22 <- '03_Data/Fahrzeug/Bestandteile_Fahrzeuge_OEM2_Typ22.csv'


C_11<- read_csv2(Path_C_11)
C_12<- read_csv2(Path_C_12)
C_21<- read_csv2(Path_C_21)
C_22<- read_csv2(Path_C_22)

cars<- C_11 %>%
  full_join(C_12)%>%
  full_join(C_21)%>%
  full_join(C_22) %>%
  select(ID_Motor,ID_Fahrzeug)

T2_cars<- cars%>%
  left_join(T2_mot)%>%
  mutate(affected= case_when(Produktionsdatum.T2 > "2008-4-30" & Produktionsdatum.T2 < "2010-12-31" & Herstellernummer.T2==202 & Werksnummer.T2 ==2022 ~ 1,
                             TRUE ~ 0))



