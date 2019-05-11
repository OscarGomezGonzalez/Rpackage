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
borrarRedActual = function(redGenes){
  if(exists("redGenes")){
    rm(redGenes)
    print("**************************************************************")
    print("*                                                            *")
    print("*                                                            *")
    print('*                   Se ha borrado la red.                    *')
    print("*                                                            *")
    print("*                                                            *")
    print("**************************************************************")
  }else{
    print("**************************************************************")
    print("*                                                            *")
    print("*                                                            *")
    print('*            Introduzca  el nombre de la red.                *')
    print("*                                                            *")
    print("*                                                            *")
    print("**************************************************************")
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
borrarBD = function(BDGenes){
  if(exists("BDGenes")){
    rm(BDGenes)
    print("**************************************************************")
    print("*                                                            *")
    print("*                                                            *")
    print('*                   Se ha borrado la BD.                     *')
    print("*                                                            *")
    print("*                                                            *")
    print("**************************************************************")
  }else{
    print("**************************************************************")
    print("*                                                            *")
    print("*                                                            *")
    print('*            Introduzca  el nombre de la BD.                 *')
    print("*                                                            *")
    print("*                                                            *")
    print("**************************************************************")
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
borrarTodo = function(){
  rm(list=ls())
  print("**************************************************************")
  print("*                                                            *")
  print("*                                                            *")
  print('*             Se ha borrado todo el workspace.               *')
  print("*                                                            *")
  print("*                                                            *")
  print("**************************************************************")
}


