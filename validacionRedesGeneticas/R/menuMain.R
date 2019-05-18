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
  redGenes
  BD
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
      redGenes=validacionRedesGeneticas::generarArchivoPrueba.grafo()
    }else if(select==2){#menu de leer
      lect=FALSE
      while(!lect){
        print("**************************************************************")
        print("*               *** MENU lectura ***                         *")
        print("*                1) lectura de prueba                        *")
        print("*                2) lectura de archivo                       *")
        print("*                3) lectura de carpeta                       *")
        print("*                4) salir                                    *")
        print("*                                                            *")
        print("*                                                            *")
        print("**************************************************************")
        select = readline(prompt = "Indica la opcion : ")
        if(select==1){#leer prueba
          ficherosPrueba = dir(path = "~/RStudio/trabajoBio/Package/validacionRedesGeneticas/data/redes")
          print("**************************************************************")
          print("*                                                            *")
          print("*                                                            *")
          print('*          Introduzca el nombre del archivo completo         *')
          print("*                   (ext para parar)                         *")
          print("*                                                            *")
          print(ficherosPrueba)
          print("*                                                            *")
          print("*                                                            *")
          print("**************************************************************")
          i=1
          while(i!=-1){
            a = readline(prompt = "Indica el archivo : ")
            comprobar = ficherosPrueba %in% a #comprueba que existe lo leido por teclado con la lista de BD
            result=which(comprobar)
            if(result>0){
              redGenes=validacionRedesGeneticas::lecturaRed.prueba(a)
              i=-1
              lect=TRUE
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
          lect=TRUE
        }else if(select==2){#leer archivo
          redGenes=validacionRedesGeneticas::lecturaRed.archivo()
          lect=TRUE
        }else if(select==3){#leer carpeta
          redGenes=validacionRedesGeneticas::lecturaRed.carpeta()
          lect=TRUE
        }else if(select==4){#salir
          lect=TRUE
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
    }else if(select==3){#elegir la BD con la que comparar
      BD=validacionRedesGeneticas::elegirBD.lista()
    }else if(select==4){#validad red
      validacionRedesGeneticas::validarRedGenetica(redGenes,BD)
    }else if(select==5){#borrar red
      validacionRedesGeneticas::borrarRedActual(redGenes)
    }else if(select==6){#borrar BD
      validacionRedesGeneticas::borrarBD(BD)
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
