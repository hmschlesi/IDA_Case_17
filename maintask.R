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

Path_Zul <- '03_Data/Zulassungen/Zulassungen_alle_Fahrzeuge.csv'

Zul <- read_csv2(Path_Zul)

T2_cars<- cars%>%
  left_join(T2_mot)%>%
  mutate(affected= case_when(Produktionsdatum.T2 > "2008-4-30" & Produktionsdatum.T2 < "2010-12-31" & Herstellernummer.T2==202 & Werksnummer.T2 ==2022 ~ "affected",
                             TRUE ~ "not affected"))%>%
  left_join(Zul, by=c("ID_Fahrzeug"="IDNummer"))%>%
  select(-8)

T2_cars_group_by_oem <- T2_cars %>%
  mutate(OEM=str_sub(ID_Fahrzeug,4,4)) %>%
  mutate(affected=as_factor(affected))%>%
  group_by(OEM, affected)%>%
  summarise(n=n())

ggplot(T2_cars_group_by_oem, aes(x=OEM, y=n, fill=affected)) +
  geom_bar(stat="identity",position=position_dodge())+
  ggtitle("affected and unaffected vehicels by OEM")

T2_cars_group_by_plant <- T2_cars %>%
  mutate(Plant=str_sub(ID_Fahrzeug,6,7))%>%
  mutate(affected=as_factor(affected))%>%
  group_by(Plant, affected)%>%
  summarise(n=n())

ggplot(T2_cars_group_by_plant, aes(x=Plant, y=n, fill=affected)) +
  geom_bar(stat="identity",position=position_dodge())+
  ggtitle("affected and unaffected vehicels by factory")

T2_cars_by_region <- T2_cars %>%
  filter(affected=="affected")%>%
  group_by(Gemeinden)%>%
  summarise(n=n())%>%
  arrange(desc(n))%>%
  slice(1:10)

ggplot(T2_cars_by_region,aes(x=reorder(Gemeinden,-n),y=n))+
  geom_bar(stat = "identity", fill="darkseagreen3",position = position_stack(reverse = TRUE))+
  theme_bw()+
  ggtitle("The ten most affected regions with defect component T2")



Path_geo <- '03_Data/Geodaten/Geodaten_Gemeinden_v1.2_2017-08-22_TrR.csv'

geo<-read_csv2(Path_geo)

T2_cars_geo<- T2_cars %>%
  filter(affected=="affected")%>%
  group_by(Gemeinden)%>%
  summarise(n=n())%>%
  left_join(geo, by=c("Gemeinden"="Gemeinde"))



ggplot(T2_cars_geo,aes(Laengengrad, Breitengrad))+
  geom_point(fill="red",aes(size=n, alpha=n))+ 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  coord_fixed(ratio = 1.5)+
  scale_alpha(range = c(0.1, 1))
