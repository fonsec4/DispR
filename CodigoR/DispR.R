#' Grafico de Dispersao
#'
#' Foi criada uma funcao DispR que recebe como 
#' parametro: DataSet(arquivo csv), os eixos x e y, 
#' parâmetros para calcular a media, logaritmo e os outliers.
#'
#'
DispR <- function (data = NULL, AXIS_X, AXIS_Y, LineDesired_Average = NULL, 
                   Line_LOG = NULL, lineOutlierAbove = NULL, lineOutlierLow = NULL, 
                   labelx, labely, title_graph, GroupColor) 
{
  graph = NULL 
  trend = NULL
  media = NULL
  point1 = NULL 
  outlierA = NULL  
  outlierB = NULL
  
  library(ggplot2)
  graph = ggplot(dat, aes(AXIS_X, AXIS_Y ,color = GroupColor, stroke = 1.2)) 
  
  #calculando a media
  if (!is.null(LineDesired_Average) ) {
    mediaaux = mean(LineDesired_Average) 
    media = geom_hline(yintercept = mediaaux, colour = 'red', size = 1)
  }
  
  if (!is.null(lineOutlierAbove) ) {
    outlierA = geom_hline(yintercept = lineOutlierAbove, colour = 'red', size = 1)
  }
  
  if (!is.null(lineOutlierLow) ) {
    outlierB = geom_hline(yintercept = lineOutlierLow, colour = 'red', size = 1)
  }
  
  #definindo pontos
  point1 = geom_point(shape = 1, size = 4) 
  
  #definindo trend line/log
  if (!is.null(Line_LOG)) {
    trend = geom_smooth( method = "lm", formula = y ~ log(x), se = FALSE, color = "red")
  }
  
  
  #plot (desenhando o gráfico)
  graph + trend + media + point1 + outlierA + outlierB + xlab(labelx) + ylab(labely) + ggtitle(title_graph)
}
