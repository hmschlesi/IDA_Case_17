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

Path_K1BE1_kom <- '03_Data/Komponente/Bestandteile_Komponente_K1BE1.csv'
Path_K1BE2_kom <- '03_Data/Komponente/Bestandteile_Komponente_K1BE2.csv' 

K1BE1_kom <- read_csv2(Path_K1BE1_kom)
K1BE2_kom <- read_csv2(Path_K1BE2_kom)