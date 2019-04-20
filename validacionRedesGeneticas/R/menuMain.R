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
menuMain <- function() {
  fin=FALSE
  while(!fin){
    print("**************************************************************")
    print("*               *** MENU PRINCIPAL ***                       *")
    print("*                  1) lectura de red                         *")
    print("*                  2) generar matrices de adyacencia         *")
    print("*                  3) generar datos de prueba                *")
    print("*                  4) validacion de red genetica             *")
    print("*                  5) borrar red actual                      *")
    print("*                  6) borrar todo                            *")
    print("*                  7) salir                                  *")
    print("*                                                            *")
    print("*                                                            *")
    print("*                                                            *")
    print("**************************************************************")
    select = readline(prompt = "Indica la opcion : ")
    if(select==1){
      validacionRedesGeneticas::lecturaRed.archivo()
    }else if(select==2){
      validacionRedesGeneticas::generarMatrizAdyacencia.matriz()
    }else if(select==3){
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
      validacionRedesGeneticas::generarArchivoPrueba.grafo(vectorGenes)
    }else if(select==4){
      validacionRedesGeneticas::validarRedGenetica()
    }else if(select==5){
      print("**************************************************************")
      print("*                                                            *")
      print("*                                                            *")
      print('*            Introduzca  el nombre de la red.                *')
      print("*                                                            *")
      print("*                                                            *")
      print("**************************************************************")
      redGenes = readline(prompt = "Indica la red : ")
      validacionRedesGeneticas::borrarRedActual(redGenes)
    }else if(select==6){
      validacionRedesGeneticas::borrarTodo()
    }else if(select==7){
      fin=TRUE
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
