#Borrar los objetos de la memoria
rm(list=ls())
# Unir tablas -------------------------------------------------------------

#Lectura de los datos de la tabla de suelos
dtsuelo <- read.csv("data/output/dtsuelos.csv",fileEncoding="latin1")

#Lectura de los datos de los departamentos
dtdpto <- read.csv("data/output/dtdpto.csv",fileEncoding="latin1")
dtdpto<- dtdpto[,c("archivos","valor")]

#Combinar tablas de dpto y de suelos (principal y secundaria)
dtglobal <- dplyr::left_join(dtdpto, dtsuelo, by="archivos")

dtglobal$cab_Campo_cab_NA <- as.character(dtglobal$cab_Campo_cab_NA)

#Guardar los datos en el archivo global
write.csv(dtglobal, file = "data/output/dtglobal.csv",fileEncoding = "latin1")

