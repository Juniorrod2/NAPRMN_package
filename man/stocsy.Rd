% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/STOCSY.R
\name{stocsy}
\alias{stocsy}
\title{Produz um grafico de STOCSY determinando a correlacao entre um sinal de referencia (driver) e os demais
sinais no espectro.}
\usage{
stocsy(spectra, driver_peak, mode = "cor", ref_spectrum = 1)
}
\arguments{
\item{spectra}{Objeto contendo a matriz de espectros para calculo das correlacoes. O objeto deve ser uma
matriz gerada durante o processamento com o pacote PepsNMR ou estrutuda de dados equivalente.}

\item{driver_peak}{Valor numerico indicando o ponto a ser usado como referencia para o calculo de correlacao. A partir do valor
fornecido sera calculado o ponto do mais proximo no espectro e este ponto sera usado para realizar as correlacoes.}

\item{mode}{Afeta a visualizacao do espectro, onde o modo "cor"-Correlacao projeta as intensidades pesadas de acordo
com a correlacao para cada sinal e o modo "cov"-Covariancia projeta as covariancias entre o sinal driver e os demais.
O gradiente de cores e sempre determinado pela correlacao.}

\item{ref_spectrum}{Define o espectro de referencia usado para plotar o grafico. Recebe um escalar ao qual deve corresponder a posicao numerica
do espectro a ser utilizado como modelo para projetar os sinais no grafico de saida.}
}
\value{
Um objeto do tipo plotly na forma de grafico interativo
}
\description{
Diferente da stocsy_by_region, esta funcao recebe apenas um ponto do espectro no parametro driver_peak e traca a correlacao
com os demais.
}
\examples{
#stocsy(spec,driver_peak = 0.945,mode = "cov")
}
