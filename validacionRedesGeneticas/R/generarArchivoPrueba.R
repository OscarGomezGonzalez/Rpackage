#' @title Dibuja el histograma de una variable superponiendo la densidad normal ajustada
#' @description Función que dibuja el histograma de una variable x, superponiendo la densidad normal
#' ajustada. Si el usuario lo desea puede superponer también un estimador de núcleo de la densidad.
#' @param x vector de datos cuyo histograma se va a calcular
#' @param dens valor lógico: TRUE=Superponer estimador de núcleo de la densidad
#' @return el histograma con la densidad normal superpuesta
#' @export normalHist
#' @examples
#' u=rnorm(1000,100,12)
#' normalHist(u);
#' normalHist(u,dens=TRUE)
#' normalHist(u,dens=TRUE,col="lightcyan")
#'
generarArchivoPrueba.grafo = function(){
  print("**************************************************************")
  print("*                                                            *")
  print("*                                                            *")
  print('*     Introduzca los genes y pesos para la red de prueba     *')
  print("*                   (ext para parar)                         *")
  print("*                                                            *")
  print("**************************************************************")
  i=1
  vector=c()
  while(i!=-1){
    a = readline(prompt = "Indica genA : ")
    if(a=="ext"){
      i=-1
    }else{
      vector[i] = a
      a = readline(prompt = "Indica genB : ")
      if(a=="ext"){
        i=-1
      }else{
        vector[i+1] = a
        a = readline(prompt = "Indica peso : ")
        if(a=="ext"){
          i=-1
        }else{
          vector[i+2] = a
          i=i+3
        }
      }
    }
  }
  size=length(vector)
  if(size%%3==0 && size>0){
    config=validacionRedesGeneticas::configuracionRedesPruebas.estado()
    if(as.numeric(config)){
      config[1]=config[1]+1
      vectorFin=c("GenA","GenB","Peso",vector)
      ruta=paste0("/Users/Racso/Documents/RStudio/trabajoBio/Package/validacionRedesGeneticas/data/redes/prueba",config,".txt")
      write(vectorFin,ruta, ncolumns = 3)
      redGenes=validacionRedesGeneticas::lecturaRed.prueba(paste0("prueba",config,".txt"))
      print(paste0("La nueva red se llama prueba",config,".txt"))
      return(redGenes)
    }else{
      error=paste0("ERROR en el sistema de archivo, ",config)
      print(error)
    }
  }else{
    print("Error al crear la red")
  }
}

#' @title Dibuja el histograma de una variable superponiendo la densidad normal ajustada
#' @description Función que dibuja el histograma de una variable x, superponiendo la densidad normal
#' ajustada. Si el usuario lo desea puede superponer también un estimador de núcleo de la densidad.
#' @param x vector de datos cuyo histograma se va a calcular
#' @param dens valor lógico: TRUE=Superponer estimador de núcleo de la densidad
#' @return el histograma con la densidad normal superpuesta
#' @export normalHist
#' @examples
#' u=rnorm(1000,100,12)
#' normalHist(u);
#' normalHist(u,dens=TRUE)
#' normalHist(u,dens=TRUE,col="lightcyan")
#'
configuracionRedesPruebas.estado = function(){
  ficheros = dir(path = "~/RStudio/trabajoBio/Package/validacionRedesGeneticas/data/redes")
  return(length(ficheros))
}
