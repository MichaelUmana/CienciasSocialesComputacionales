#Se cargan las librerias 

install.packages("git2r")
install.packages('DataExplorer')

#Se cargan las librerias 

library(tidyverse)
library(ggplot2)
library(git2r)
library(readr)
library(DataExplorer)

#Se cargan los datos

bbdd <- read.csv("./Data/M34_202103_eph.csv")
head(bbdd)
unique(bbdd)

#Visión de base de datos

glimpse(bbdd)
