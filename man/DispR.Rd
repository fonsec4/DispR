\name{DispR}
\alias{DispR}
\title{Grafico de Dispersao}
\description{
Foi criada uma funcao DispR que recebe como 
parametro: DataSet(arquivo csv), os eixos x e y, 
parametros para calcular a media, logaritmo e os outliers.
}
\usage{
DispR(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Recebe como parametro: DataSet
(arquivo csv), os eixos x e y, parametros para calcular a media, logaritmo, os
outliers, titulo, label x e y, legenda.
}
}
\details{
A exibicoo da media, logaritmo e outliers sao opcionais, conforme definido na
funcao DispR:
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x) 
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
