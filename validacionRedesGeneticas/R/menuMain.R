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
    print("*                1) generar datos de prueba                  *")
    print("*                2) lectura de red                           *")
    print("*                3) seleccionar BD                           *")
    print("*                4) validacion de red genetica               *")
    print("*                5) borrar red actual                        *")
    print("*                6) borrar BD (deseleccionar)                *")
    print("*                7) borrar todo                              *")
    print("*                8) salir                                    *")
    print("*                                                            *")
    print("*                                                            *")
    print("*                                                            *")
    print("**************************************************************")
    select = readline(prompt = "Indica la opcion : ")
    if(select==1){#archivos de prueba
      validacionRedesGeneticas::generarArchivoPrueba.grafo()
    }else if(select==2){#menu de leer
      #validacionRedesGeneticas::lecturaRed.archivo()
    }else if(select==3){#elegir la BD con la que comparar
      validacionRedesGeneticas::elegirBD.lista()
    }else if(select==4){#validad red
      validacionRedesGeneticas::validarRedGenetica()
    }else if(select==5){#borrar red
      validacionRedesGeneticas::borrarRedActual()
    }else if(select==6){#borrar BD
      validacionRedesGeneticas::borrarRedBD()
    }else if(select==7){#borrar todo el workspace
      validacionRedesGeneticas::borrarTodo()
    }else if(select==8){#salir
      fin=TRUE
      print("**************************************************************")
      print("*                                                            *")
      print("*                                                            *")
      print('*                   Que tenga un buen dia.                   *')
      print("*                                                            *")
      print("*                                                            *")
      print("**************************************************************")
    }else{#opcion defecto, fallo
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
