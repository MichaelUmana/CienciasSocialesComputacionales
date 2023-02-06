#Tratamiento de datos 

#Visión de base de datos

glimpse(bbdd)

# Se hace el cambio de CHR a Factor por medio del siguiente codigo
bbdd$CH04 <- as.factor(bbdd$CH04)

# Se visualiza que la variable genero era CHR, por tanto, se deja como factor para poder hacer una visualización de datos posteriormente. 
str(bbdd$CH04)

#Se genera una visualización pre eliminar para poder esclarecer la situación 

barplot(table(bbdd$CH04))

#Se puede observar que la mayor cantindad de encuestados son varones posteriormente 

plot.intro(bbdd$CH04)


# Se hace una visualización de la base de datos para tener claridad de su composición.

plot_intro(bbdd)

#Se puede observar que no existe valores perdidos y posteriormente se hace una visualización de la variblae género de la base de datos-

#Visualización variable genero dentro de la base de datos y posterior al paso de CHR a Factor.

plot_bar(bbdd$CH04)

#Siguiendo con la visualización de datos, se hace sobre la variable aglomerado.

str(bbdd$AGLOMERADO)

bbdd$AGLOMERADO <- as.factor(bbdd$AGLOMERADO)

#Se hace una visualización de datos 

plot_bar(bbdd$AGLOMERADO)


#Para establecer las variables. 
bbdd %>% plot_bar()

glimpse(bbdd)
