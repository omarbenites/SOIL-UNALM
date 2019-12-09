library(dplyr)

# Funciones para obtener y limpiar datos del formulario de suelos ------------------


#' Funcion para obtener el campo del departamento
#' @param archivos lista de archivos
#' @description obtener los solicitantes de los archivos de suelos
#' 
get_departamento<- function(archivos){
  
  dfr <- readxl::read_excel(path = archivos, sheet=1)
  class(dfr) <- "data.frame" 
  if( any(grepl(pattern = "Departam", dfr[,1] , ignore.case = TRUE)==TRUE) ){
    pos      <- grep(pattern = "Departam", dfr[,1] , ignore.case = TRUE)
    etiqueta <- dfr[,1][pos]
    valor <- dfr[pos,3]   
  }
  else {
    etiqueta <- "OTRO"    
    valor <- "NULO"
    pos <- "NULO"
  }
  out<- list(archivos = basename(archivos), etiqueta=etiqueta, valor = valor, pos = pos)
  
}


#' Funcion para obtener el campo del solicitante
#' @param archivos lista de archivos
#' @description obtener los solicitantes de los archivos de suelos
#' 
get_solicitante <- function(archivos){
  
  dfr <- readxl::read_excel(path = archivos, sheet=1)
  class(dfr) <- "data.frame" 
  if( any(grepl(pattern = "Soli", dfr[,1] , ignore.case = TRUE)==TRUE) ){
    pos      <- grep(pattern = "Soli", dfr[,1] , ignore.case = TRUE)
    etiqueta <- dfr[,1][pos]
    valor <- dfr[pos,3]   
  }
  else {
    etiqueta <- "OTRO"    
    valor <- "NULO"
    pos <- "NULO"
  }
  out<- list(archivos = basename(archivos), etiqueta=etiqueta, valor = valor, pos = pos)
}

#' Obtener la tabla de los suelos
#' @param archivos 
#' @description obtener las tablas de suelos
#' 
get_dtsuelos <- function(archivos){
  
  dfr <- readxl::read_excel(path = archivos, sheet=1)
  class(dfr) <- "data.frame" 
 
  #Posicion inicial y final de las etiquetas
  pi <- grep(pattern = "Lab", dfr[,1] , ignore.case = TRUE) #pos. inicial
  pf <- grep(pattern = "arena", dfr[,1] , ignore.case = TRUE) #pos. final
  
  
  if(length(pi)>0L & length(pf)>0L){
    
    dfr <- dfr[pi:pf,] #Subconjunto de datos encasillando la tabla de suelos principal/grade.
    pos <- grep(pattern = "^[[:digit:]]+$",dfr[,1]) #detect integers
    cc <- get_cabesuelos(dfr)
    if(length(pos)!=0L){
      out <- dfr[pos,]
      names(out) <- cc
      out$archivos <- basename(archivos)
    } else {
      out <- data.frame(archivos=basename(archivos))
    }
  } else {
    print("no hay datos")
    #out <- data.frame()
    out <- data.frame(archivos=basename(archivos))
  }
  out
}

#' Obtener la tabla de los nutrientes
#' @param archivos 
#' @description obtener la tabla de los nutrientes ubicada dentro de la 
#' tabla de suelos.
#' 
get_dtnutrientes <- function(archivos){
  
  dfr <- readxl::read_excel(path = archivos, sheet=1)
  class(dfr) <- "data.frame" 
  #lw <- grep(pattern = "Lab", dfr[,1] , ignore.case = TRUE)
  #up <- grep(pattern = "arena", dfr[,1] , ignore.case = TRUE)
  
  pi <- grep(pattern = "Fr.Ar.L.", dfr[,1], ignore.case = TRUE)
  
  if(length(pi)>0L){
    
    #Primer Conjunto datos de nutrientes (primer conjunto)
    pf <- 100L #numero arbitrario
    sdfr <- dfr[pi:100, ] #extraer numero de filas
   
    #Segundo conjunto de los datos de nutrientes 
    spi <- grep(pattern = "Lab", sdfr[,1] , ignore.case = TRUE) #detectar etiqueta "Lab"
    spf <- grep(pattern = "^[[:digit:]]+$",sdfr[,1]) #detectar Nro. de muestra
    
    if(length(spi)>0 && length(spf)>0){
      out <- sdfr[c(spi, spf), ] #extraer numero de filas del segundo conjunto de datos
      names(out)<- paste0("cab_",1:ncol(sdfr))
      out <- janitor::remove_empty(out,"rows") #remover filas vacias
      out$archivos <- basename(archivos) #poner un nombre   
    } else {
      print(archivos)
      out <- data.frame()
    }
    
  } else {
    print("NO HAY ENTEROS")
    print(archivos)
    out <- data.frame()
  }
  out
}

#' Obtener cabezeras de la tabla de suelos principal
#' 
#' @param dfr data.frame datos leidos de los archivos de suelo
#' @description Obtener(get) las cabezeras de las tablas de suelo

get_cabesuelos <- function(dfr){
  
  #dfr <- readxl::read_excel(path = archivos, sheet=1)
  #class(dfr) <- "data.frame" 
  if( any(grepl(pattern = "Lab", dfr[,1] , ignore.case = TRUE)==TRUE) ){
    pos <- grep(pattern = "Lab", dfr[,1] , ignore.case = TRUE)
    rg <- c(pos-1,pos,pos+1) 
    cc <- lapply(1:ncol(dfr), function(x) paste("cab",dfr[rg,x],collapse="_"))
    cc <- stringr::str_replace_all(string = unlist(cc), pattern = "\\s+",replacement = "_")
  }
  else {
    cc<- paste0("cab_nula_",1:ncol(dfr))
  }
  cc
}


#' Obtener cabezeras de la tabla de nutrientes
#' 
#' @param ldfr list lista de data.frame datos combinados de la tabla de suelos
#' @description Obtener(get) las cabezeras de las tablas de suelo

get_cabnut <- function(ldfr){
    
  #Listado del numero de columnas por cada data.frame
  lnc <- lapply(1:length(ldfr), function(x) length( na.omit(as.character(as.vector(ldfr[[x]][1,])))))
  pos <- which.max(lnc)
  #Detectar el numero máximo de cabeceras  
  if(pos>1){
      pos<- pos[1]
  }
  out <- paste0(names(ldfr[[pos]]),"_",as.character(as.vector(ldfr[[pos]][1,])))
  out[length(out)]<-"archivos"
  out
}

#' Asignar nuevos nombres a las cabezeras
#'@param nuevo nombres de las nuevas cabezeras
#'@param dfr data.frame datos de los nutrientes

set_cabnut <- function(nuevo, dfr){
  
  names(dfr)[1:length(nuevo)] <- nuevo
  dfr
}


#' Limpiar data.frames de acuerdo a diferentes tipos de campo del formulario de suelos
#' @param df data frame table derived of combining multiple row-vectors
#' 
clean_dt <- function(dfr, campo ="solicitante"){

  if(campo=="solicitante"){
    dfr$archivos <- as.character(dfr$archivos)
    dfr$etiqueta <- stringr::str_trim(as.character(dfr$etiqueta),side = "both") %>% gsub(' +',' ',.) 
    dfr$pos <- as.character(dfr$pos)
    dfr$valor <- as.character(dfr$valor)  
  }
  dfr
} 

#' Combinar por filas(rows) una lista de multiples data.frames
#' @param ldf list of data.frames
#' 
rbind_ldf<- function(ldf){
  dt <- as.data.frame(do.call(rbind, ldf), stringsAsFactors = FALSE)
}




