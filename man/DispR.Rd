\name{DispR}
\alias{DispR}
\title{Grafico de Dispersao}
\description{
 Foi criada uma funcao DispR que recebe como 
 parametro: DataSet(arquivo csv), os eixos x e y, 
 parametros para calcular a media, logaritmo e os outliers.
 Com este pacote, eh possivel tracar uma linha media em relacaoo ao eixo y, 
 uma linha logaritmo do eixo y em relacao ao eixo x e 
 linhas que representam os outliers.
 Os outliers nao sao de fato calculados na funcao, somente a media e logaritmo
}
\usage{
DispR(data, AXIS_X, AXIS_Y, LineDesired_Average, 
      Line_LOG, lineOutlierAbove, lineOutlierLow, 
      labelx, labely, title_graph, GroupColor)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Recebe como parametro: DataSet
(arquivo csv), os eixos x e y, parametros para calcular a media, logaritmo, os
outliers, titulo, label x e y, GroupColor (como sera separado a cor no grafico).
}
}
\details{
A exibicoo da media, logaritmo e outliers sao opcionais, conforme definido na
funcao DispR, todos os campos que contem a opcao null sao opcionais.
Os outliers nao sao calculados na funcao.
}
\value{
Com este pacote, eh possivel tracar uma linha media em relacao ao eixo y,
uma linha logaritmo do eixo y em relacao ao eixo x e linhas que representam os
outliers.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## dir = "caminho dataSet, csv"

#definindo diretorio de trabalho
 setwd(dir)

dat = read.csv("EconomistData.csv")

#Chamando a funcao
 DispR(dat,dat$CPI, dat$HDI, dat$HDI, 1, 0.9, 0.286, "CPI", "HDI", "TCC", dat$Region)

#csv disponivel no site Tutorials Harvard

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
