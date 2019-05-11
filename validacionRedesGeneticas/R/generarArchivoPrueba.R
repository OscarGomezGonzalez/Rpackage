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
  print('*        Introduzca  el vector para la red de prueba         *')
  print("*                   (ext para parar)                         *")
  print("*                                                            *")
  print("**************************************************************")
  i=1
  while(i==-1){
    a = readline(prompt = "Indica la red : ")
    if(a=="ext"){
      i=-1
    }else{
      vectorGenes[i] = a
    }
  }
  respuesta=validacionRedesGeneticas::configuracionData.estado()
  if(as.numeric(respuesta)){
  config <- read.delim2("~/RStudio/trabajoBio/Package/validacionRedesGeneticas/data/config.txt")
  ruta=paste0("~/RStudio/trabajoBio/Package/validacionRedesGeneticas/data/",config(1),".txt")
  write(vector,ruta, ncolumns = ncol(vector))
  }else{
    error=paste0("ERROR en el sistema de archivo, ",respuesta)
    print(error)
  }
}
