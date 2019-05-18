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
#'&& redGenes[i,2]==BD[i2,1]&& redGenes[i,2]==BD[i2,2]
validarRedGenetica = function(redGenes,BD){
  totalPorcentaje=0
#  print(redGenes)
#  print(BD)
  for(i in 1:nrow(redGenes)){
    for(i2 in 1:nrow(BD)){
      if(redGenes[i,1]==BD[i2,1] ){
        totalPorcentaje=totalPorcentaje+15
        if(i==i2){
          totalPorcentaje=totalPorcentaje+35
        }
        if(redGenes[i,3]<=(BD[i2,3])/2){
          totalPorcentaje=totalPorcentaje+10
          if(redGenes[i,3]<=(BD[i2,3])/4){
            totalPorcentaje=totalPorcentaje+10
            if(redGenes[i,3]<=(BD[i2,3])/8){
              totalPorcentaje=totalPorcentaje+10
              if(redGenes[i,3]<=(BD[i2,3])/16){
                totalPorcentaje=totalPorcentaje+10
              }
            }
          }
        }
        if(redGenes[i,3]==BD[i2,3]){
          totalPorcentaje=totalPorcentaje+50
        }
      }
    }
    for(i2 in 1:nrow(BD)){
      if(redGenes[i,2]==BD[i2,2] ){
        totalPorcentaje=totalPorcentaje+16
        if(i==i2){
          totalPorcentaje=totalPorcentaje+34
        }
        if(redGenes[i,3]<=(BD[i2,3])/2){
          totalPorcentaje=totalPorcentaje+10
          if(redGenes[i,3]<=(BD[i2,3])/4){
            totalPorcentaje=totalPorcentaje+10
            if(redGenes[i,3]<=(BD[i2,3])/8){
              totalPorcentaje=totalPorcentaje+10
              if(redGenes[i,3]<=(BD[i2,3])/16){
                totalPorcentaje=totalPorcentaje+10
              }
            }
          }
        }
        if(redGenes[i,3]==BD[i2,3]){
          totalPorcentaje=totalPorcentaje+50
        }
      }
    }
    aux=i*2
    fin=totalPorcentaje/aux
    print("**************************************************************")
    print(paste0("El porcentaje total acumulado hasta la fila ",i," es de ",totalPorcentaje,"%"))
    print("**************************************************************")
    print("**************************************************************")
    print(paste0("La media de hasta esta fila  es de ",fin,"%"))
    print("**************************************************************")
  }
  aux=nrow(redGenes)*2
  fin=totalPorcentaje/aux
  print("**************************************************************")
  print(paste0("El porcentaje total acumulado entre las ",aux," combinaciones es de ",totalPorcentaje,"%"))
  print("**************************************************************")
  print(paste0("La media de porcentaje es de ",fin,"%"))
  print("**************************************************************")
}
