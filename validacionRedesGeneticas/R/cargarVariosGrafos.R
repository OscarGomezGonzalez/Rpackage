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
cargarVariosGrafos.fundirGrafos = function(listaArchivos){
  if(1<length(listaArchivos)){
    lista=listaArchivos[-1]
  for(i in lista){
    grafoFundido=rbind(grafoFundido,i)
  }
  }else{
    grafoFundido=listaArchivos[1]
  }
  return(grafoFundido)
}
