#main task collect all car ids for failed cars/parts/components
#T2 component file is already fixed!!!
install.packages("readr")
library("readr")
install.packages("stringr")
library("stringr")
install.packages("tidyverse")
library("tidyverse")
library("readxl")
library("lubridate")

start_repair <- "2021-09-01"
n_day=1

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

#-----------------------------------------------#
#some plots for better understanding of the data ser

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

#---------------------------------------------------#
#End of general plots

#Connecting the geo data to vehicel data

Path_geo <- '03_Data/Geodaten/Geodaten_Gemeinden_v1.2_2017-08-22_TrR.csv'
Path_geo2 <- '03_Data/Geodaten/PLZ.csv'
Path_Bundes <- '03_Data//Geodaten/Liste-der-PLZ-in-Excel-Karte-Deutschland-Postleitzahlen (1).xlsx'
# A quick quiz about german state capitals :)
# four states dont have any afffected cars, i.g. Berlin, Bremen, ...
bundes_haupt <- data.frame(Bundesland=c("Sachsen","Nordrhein-Westfalen" ,"Brandenburg","Thüringen" ,"Sachsen-Anhalt", "Mecklenburg-Vorpommern", "Niedersachsen", "Schleswig-Holstein" ,"Hamburg", "Hessen", "Rheinland-Pfalz", "Bayern", "Baden-Württemberg", "Saarland" ),
                           Bundeshaupt=c("Dresden", "Düsseldorf", "Potsdam", "Erfurt", "Magdeburg", "Schwerin" , "Hannover" , "Kiel", "Hamburg","Wiesbaden", "Mainz", "München", "Stuttgart" , "Saarbrücken"))


bundes <- read_excel(Path_Bundes)

geo2<- read_table(Path_geo2)
geo2 <- geo2 %>%
  group_by(Ort)%>%
  summarise(lon=mean(lon),lat=mean(lat))

bundes_haupt <- bundes_haupt%>%
  left_join(geo2, by=c("Bundeshaupt"="Ort"))


geo<-read_csv2(Path_geo)
# Gemeinde Seeg taucht zwar in zuLassungsdaten auf, hat aber keine Referenz in den Geodaten. Seeg ist eine Gemeinede in Bayern mit der PLZ 87637 unter der PLZ ist 
#beid en Geo daten nur ein NA als Gemeinde eingetrage. Dies wird manuell korrigiert.
#Man hätte auch den original Datensatz von DBGeoPLz verwenden können :)
geo <- geo %>%
  mutate(Gemeinde=replace(Gemeinde, Postleitzahl==87637 & is.na(Gemeinde),"SEEG"))%>%
  select(c(-1,-X))
#euclidean Distance for distance to state capital
eu_dist <- function(p1,p2,q1,q2){
  d<- sqrt((q1-p1)^2+(q2-p2)^2)
  return(d)
}

schedule_rep <- function(d){
  ceiling(nrow(distance)/100)
}


#Geodaten und Bundesländer mit den Fahrzeug verknüpfen über die Zualssung und PLZ. 
# Amd alot of other stuff
T2_cars_bund  <- T2_cars %>%
  filter(affected=="affected")%>%
  left_join(geo, by=c("Gemeinden"="Gemeinde"))%>%
  left_join(bundes, by=c("Postleitzahl"="PLZ"))%>%
  select(c(-Kreis, -Typ))%>%
  left_join(bundes_haupt)%>%
  mutate(Bundesland=as_factor(Bundesland))%>%
  mutate(distance=eu_dist( Laengengrad, Breitengrad ,lon,lat))%>%
  group_by(Bundesland)%>%
  arrange(Bundesland,distance)%>%
  mutate(days=ceiling(row_number()/100))%>%
  mutate(rep_date= date(start_repair)+days)


T2_cars_bund_general <- T2_cars_bund %>%
  group_by(Bundesland)%>%
  summarise(n=n(), repair=max(days))




ggplot(T2_cars_geo,aes(Laengengrad, Breitengrad))+
  geom_point(fill="red",aes(size=n, alpha=n))+ 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  coord_fixed(ratio = 1.5)+
  scale_alpha(range = c(0.1, 1))
