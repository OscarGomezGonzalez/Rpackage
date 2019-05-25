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
elegirBD.lista = function(carpetaBD){
  ficherosBD = dir(path = carpetaBD)
  fin=FALSE
  while(!fin){
    print("**************************************************************")
    print("*              *** Lista Bases de Datos ***                  *")
    print("*                                                            *")
    print(ficherosBD)
    print("*                                                            *")
    print("*                                                            *")
    print("*     *** Eliga la Bases de Datos con el nombre completo *** *")
    print("*                   (ext para parar)                         *")
    print("*                                                            *")
    print("**************************************************************")
    select = readline(prompt = "Indica la BD : ")
    if(select=="ext"){
      fin=TRUE
    }else{
      comprobar = any(ficherosBD %in% select) #comprueba que existe lo leido por teclado con la lista de BD
      if(comprobar){
        BD = validacionRedesGeneticas::lecturaBD.BD(select,carpetaBD)
        fin=TRUE
        return(BD)
      }else{
        print("**************************************************************")
        print("*                                                            *")
        print("*                                                            *")
        print('*  Opcion por defecto, indique una opcion de las que tiene.  *')
        print("*                                                            *")
        print("*                                                            *")
        print("**************************************************************")
      }
    }
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
lecturaBD.BD = function(nombreBD,carpetaBD){
  BD <- read.delim2(paste0(carpetaBD,"/",nombreBD), sep="")
  return(BD)
}
