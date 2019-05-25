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
  carpetaLectura="~/RedesGeneticas_Validacion"
  carpetaRedesPrueba="~/RStudio/trabajoBio/Package/validacionRedesGeneticas/data/redes"
  carpetaBD="~/RStudio/trabajoBio/Package/validacionRedesGeneticas/data/BD"
  fin=FALSE
  redGenes=0
  BD=0
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
      redGenes=validacionRedesGeneticas::generarArchivoPrueba.grafo(carpetaRedesPrueba,carpetaBD)
    }else if(select==2){#menu de leer
      if(dir.exists(carpetaLectura)){
        dir.create(carpetaLectura, showWarnings = TRUE, recursive = FALSE, mode = "0777")
        Sys.chmod(carpetaLectura, mode = "0777", use_umask = TRUE)
        Sys.umask(mode = NA)
      }
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
          ficherosPrueba = dir(path = carpetaRedesPrueba)
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
            if(a=="ext"){
              i=-1
            }else{
              comprobar = any(ficherosPrueba %in% a) #comprueba que existe lo leido por teclado con la lista de prueba
              if(comprobar){
                redGenes=validacionRedesGeneticas::lecturaRed.prueba(a,carpetaRedesPrueba)
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
          }
          lect=TRUE
        }else if(select==2){#leer archivo
          ficherosLectura = dir(path = carpetaLectura)
          print("**************************************************************")
          print("*                                                            *")
          print("*                                                            *")
          print('*          Introduzca el nombre del archivo completo         *')
          print("*                   (ext para parar)                         *")
          print("*                                                            *")
          print(ficherosLectura)
          print("*                                                            *")
          print("*                                                            *")
          print("**************************************************************")
          i=1
          while(i!=-1){
            a = readline(prompt = "Indica el archivo : ")
            if(a=="ext"){
              i=-1
            }else{
              comprobar = any(ficherosLectura %in% a) #comprueba que existe lo leido por teclado con la lista de lectura
              if(comprobar){
                redGenes=validacionRedesGeneticas::lecturaRed.archivo(a,carpetaLectura)
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
          }
          lect=TRUE
        }else if(select==3){#leer carpeta
          redGenes=validacionRedesGeneticas::lecturaRed.carpeta(carpetaLectura)
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
        #print(redGenes)        #para comprobar la lectura
      }
    }else if(select==3){#elegir la BD con la que comparar
      BD=validacionRedesGeneticas::elegirBD.lista(carpetaBD)
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
