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
  rename(Motor_ID=ID_K1BE1)
K1BE2_kom <- read_csv2(Path_K1BE2_kom)
K1BE2_kom <- K1BE2_kom %>%
  rename(Motor_ID=ID_K1BE2)

Motors <- K1BE1_kom%>%
  full_join(K1BE2_kom)%>%
  select(c(-ID_T1,-ID_T7,-ID_T8,-ID_T3,-1,-ID_T4))

T2_mot <- T2%>%
  left_join(Motors, by=c("ID_T02.T2"="ID_T2")) 
T2_mot
