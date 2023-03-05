
#Se cargan los paquetes

library(tidyverse)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DataExplorer)

M34_202103_eph <- read_csv("Documents/Diplomatura IDAES/TPs/TP Final/M34_202103_eph.csv", col_names = TRUE)
View(M34_202103_eph)

summary(M34_202103_eph)


#Creamos una base de exploración, quitando la variable código, filtrando los ingresos negativos ya que no debería haber ingresos negativos, 
#los casos que superan la cantidad de horas de la semana (168) ya que no podrían trabajar más de las horas existentes en una semana y 
#cambiando las edades individuales por rangos para una exporación que nos permita inferir algunas conclusiones antes de modelizar. 

Base_exploracion <- M34_202103_eph %>%
  select (NRO_HOGAR, AGLOMERADO, NIVEL_ED, CH03, CH04, CH06, CH07, PP04A, CAT_OCUP, INTENSI, PP3E_TOT, CATEGORIA, CALIFICACION, P21) %>%
  filter (P21 >= "0") %>%
  filter (PP3E_TOT != "999")  %>%
  mutate(edad_rango = case_when(CH06 %in% c(0:19)  ~ "0 a 19",
                                CH06 %in% c(20:29) ~ "20 a 29",
                                CH06 %in% c(30:39) ~ "30 a 39",
                                CH06 %in% c(40:49) ~ "40 a 49",
                                CH06 %in% c(50:59) ~ "50 a 59",
                                CH06 %in% c(60:74) ~ "60 a 74",
                                CH06 >= 75 ~ "Mayor a 75"))



#Vamos a explorar la relación entre los rangos de edad, ingreso y el género por
# considerarlas variables base a entender. Al igual que vincularemos los rangos de
# edad y el género con las horas trabajadas.



options(scipen = 999)

ggplot(Base_exploracion, aes(x = edad_rango, y = P21, fill = CH04)) +
  geom_col() +  
  labs (title = "Ingresos acumulados por rango de edad y género", 
        x = "Rango de edad",
        y = "Ingreso en pesos Ar",
        fill = "Género")

ggplot(Base_exploracion, aes(x = edad_rango, y = PP3E_TOT, fill = CH04)) +
  geom_col() +  
  labs (title = "Horas trabajadas acumuladas por rango de edad y género", 
        x = "Rango de edad",
        y = "Horas trabajadas",
        fill = "Género")



# En ambos casos vemos que los rangos de edad de entre 30 y 59 años son los que 
# perciben la mayor cantidad de ingresos, pero que al compararla con la cantidad de horas 
# trabajadas, las personas de entre 20 y 29 años trabajan más horas que las personas de 
# 50 a 59 pero perciben un menor ingreso.
# En ambos casos, tanto candidad de horas trabajadas como ingresos percibidos, 
# los hombres predominan en la proporción en todos los rangos etarios. 

#Para entender mejor la relación entre las horas trabajdas, los ingresos y los rangos 
# de edad, vamos a sacar los promedios de las horas y los ingresos. 

Promedio_ingreso <- Base_exploracion %>%
  select(edad_rango, P21, CH04) %>%
  group_by(edad_rango) %>%
  summarise(ingreso_prom = median(P21))

ggplot(Promedio_ingreso, aes(x = edad_rango, y = ingreso_prom)) +
  geom_col(fill = 2)  +  
  labs (title = "Medianas de ingreso por rango de edad", 
        x = "Rango de edad",
        y = "Ingresos promedio") + 
  theme_minimal() 

Promedio_horas <- Base_exploracion %>%
  select(edad_rango, PP3E_TOT) %>%
  group_by(edad_rango) %>%
  summarise(horas_prom = mean(PP3E_TOT))

ggplot(Promedio_horas, aes(x = edad_rango, y = horas_prom)) +
  geom_col(fill = 4) +  
  labs (title = "Horas trabajadas promedio por rango de edad", 
        x = "Rango de edad",
        y = "Horas trabajadas promedio")




#Comparativamente vemos que no hay una gran diferencia entre la cantidad de horas que 
# trabajan los distintos rangos de edad (aproximadamente 4 horas de diferencia promedio),
# comparativamente con sus ingresos cuya disperción es mayor. especialmente entre
# los 50 a 59 años y los menores de 19 años. Vemos que el valor de la hora llega a su 
# máximo entre las edades de 50 a 59 años. 

#Ahora exploremos las mismas relaciones entre horas trabajadas e ingresos pero en 
# relación al género. 


Ingreso_genero <- Base_exploracion %>%
  select(CH04, P21) %>%
  group_by(CH04) %>%
  summarise(ingreso_prom = mean(P21))

ggplot(Ingreso_genero, aes(x = CH04, y = ingreso_prom)) +
  geom_col()

Horas_genero <- Base_exploracion %>%
  select(CH04, PP3E_TOT) %>%
  group_by(CH04) %>%
  summarise(horas_prom = mean(PP3E_TOT))

ggplot(Horas_genero, aes(x = CH04, y = horas_prom)) +
  geom_col()


Ingresoyhoras_genero <- Base_exploracion %>%
  select(CH04, P21,PP3E_TOT) %>%
  group_by(CH04) %>%
  summarise(ingreso_prom = mean(P21), horas_prom = mean(PP3E_TOT))

ggplot(Ingresoyhoras_genero, aes(x = CH04, y = ingreso_prom, fill = horas_prom)) +
  geom_col()

#Estado civil y relación de parentezoco con el/la jefa de familia.

ggplot(Base_exploracion, aes(x = P21 , y = CH03, fill = CH07)) +
  geom_col() +
  labs (title = "Ingresos acumulados por relación de parentesco con el/la jefe 
                              de familia y su estado civil", 
        x = "Ingreso",
        y = "Relación de parentesco",
        fill = "Estado civil") + 
  theme_minimal()  

Estadocivil_ingreso <- Base_exploracion %>%
  select(CH07, P21) %>%
  group_by(CH07) %>%
  summarise(ingreso_prom = mean(P21))

ggplot(Estadocivil_ingreso, aes(x = CH07 , y = ingreso_prom)) +
  geom_col(fill = 2) +
  labs (title = "Ingresos promedio por estado civil", 
        x = "Estado civil",
        y = "Ingreso promedio") + 
  theme_minimal()  



#GEOGRAFIA 
Aglomerado_ingreso <- Base_exploracion %>%
  select(AGLOMERADO, P21) %>%
  group_by(AGLOMERADO) %>%
  summarise(ingreso_prom = mean(P21))

ggplot(Aglomerado_ingreso, aes(x = ingreso_prom, y = AGLOMERADO)) +
  geom_col(fill = 4) +
  labs (title = "Ingresos promedio por aglomerado", 
        x = "Ingreso promedio",
        y = "Aglomeradoo") + 
  theme_minimal()




# Exploración calificación, nivel educativo e ingreso.
#Quienes tienen mayores ingresos son 
# quienes tienen la universidad o el secundario completo. Entre esos dos niveles educativos, 
# priman los profesionales y técnicos en los universitarios completos y los operarios 
# en el secundario. Llamaticamente, tener un universitario incompleto y un secundario 
#incompleto tienen un nivel de ingreso similar y su composición de calificación laboral
# se diferencia por una proporción mayor de técnicos y profesionales en los universitarios incompletos. 


ggplot(Base_exploracion, aes(x = P21, y = NIVEL_ED, fill = CALIFICACION)) +
  geom_col(width = 0.5) +  
  labs (title = "Distribución de ingresos por nivel educativo y calificación del trabajador", 
        x = "Ingreso",
        y = "Nivel educativo",
        fill = "Calificación") + 
  theme_minimal()  +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(size = 6), axis.text.y = element_text(size = 6)) +
  theme(legend.position = "top")

#Exploración de categoría con cantidad de horas e ingreso. 

Categoria_horas_ingreso <- Base_exploracion %>%
  select(CATEGORIA, P21, PP3E_TOT) %>%
  group_by(CATEGORIA) %>%
  summarise(ingreso_prom = mean(P21), horas_prom = mean(PP3E_TOT))

ggplot(Categoria_horas_ingreso, aes(x = ingreso_prom, y = CATEGORIA, fill = horas_prom)) +
  geom_col()


#Exploración tipo de instutición con ingreso

Sector_ocupacion_ingreso <- Base_exploracion %>%
  select(PP04A, P21) %>%
  group_by(PP04A) %>%
  summarise(ingreso_prom = median(P21))

ggplot(Sector_ocupacion_ingreso, aes(x = PP04A, y = ingreso_prom)) +
  geom_col()

Categoria_ocupacion_ingreso <- Base_exploracion %>%
  select(P21, CAT_OCUP, PP04A) %>%
  group_by(CAT_OCUP) %>%
  reframe(ingreso_prom = mean(P21), PP04A)

ggplot(Base_exploracion, aes(x = P21, y = AGLOMERADO, fill = PP04A)) +
  geom_col()

Exploracion_genero_hogar <- Base_exploracion %>%
  select(CH04, CAT_OCUP)

table(Exploracion_genero_hogar$CH04, Exploracion_genero_hogar$CAT_OCUP)

Categoria_ocupacion_ingreso <- Base_exploracion %>%
  select(P21, CAT_OCUP) %>%
  group_by(CAT_OCUP) %>%
  summarise(ingreso_prom = mean(P21))

ggplot(Categoria_ocupacion_ingreso, aes(x = CAT_OCUP, y = ingreso_prom)) +
  geom_col()