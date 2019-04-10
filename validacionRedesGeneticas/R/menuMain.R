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
  While(fin)
  {
    print("**************************************************************")
    print("*               *** MENU PRINCIPAL ***                       *")
    print("*                  1) lectura de red                         *")
    print("*                  2) generar matrices de adyacencia         *")
    print("*                  3) generar datos de prueba                *")
    print("*                  4) validacion de red genetica             *")
    print("*                  5) borrar red actual                      *")
    print("*                  6) salir                                  *")
    print("*                                                            *")
    print("*                                                            *")
    print("*                                                            *")
    print("**************************************************************")
    select = readline(prompt = "Indica la opcion : ")
    switch(select,
           1={
             validacionRedesGeneticas::lecturaRed.archivo()
           },
           2={
             validacionRedesGeneticas::generarMatrizAdyacencia.matriz()
           },
           3={
             validacionRedesGeneticas::generarArchivoPrueba.grafo()
           },
           4={
             validacionRedesGeneticas::validarRedGenetica()
           },
           5={
             validacionRedesGeneticas::borrarRedActual()
           },
           6={
            fin=TRUE
           },
           {
            print("Opcion por defecto, repite el menu, indique una opcion de las que tiene.")
           }
           )
  }
}
