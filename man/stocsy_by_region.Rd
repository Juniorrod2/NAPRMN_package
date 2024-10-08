% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/STOCSY.R
\name{stocsy_by_region}
\alias{stocsy_by_region}
\title{Produz um grafico de STOCSY determinando a correlacao entre um sinal de referencia (driver) e os demais
sinais no espectro.}
\usage{
stocsy_by_region(
  spectra,
  driver_peak,
  mode = "cor",
  spectrum_resolution = 0.5,
  ref_spectrum = 1
)
}
\arguments{
\item{spectra}{Objeto contendo a matriz de espectros para calculo das correlacoes. O objeto deve ser uma
matriz gerada durante o processamento com o pacote PepsNMR ou estrutuda de dados equivalente.}

\item{driver_peak}{Vetor de tamanho 2, contendo o deslocamento quimico inicial e final do sinal ao qual
as correlacoes serao calculadas para plotar o STOCSY.}

\item{mode}{Afeta a visualizacao do espectro, onde o modo "cor"-Correlacao projeta as intensidades pesadas de acordo
com a correlacao para cada sinal e o modo "cov"-Covariancia projeta as covariancias entre o sinal driver e os demais.
O gradiente de cores e sempre determinado pela correlacao.}

\item{spectrum_resolution}{Define a resolucao espectral a ser plotada, por padrao 50\%. Recebe um valor entre 0 e 1
relativo ao percentual de resolucao do espectro original que deve ser conservada no grafico.}

\item{ref_spectrum}{Define o espectro de referencia. Recebe um escalar ao qual deve corresponder a posicao numerica
do espectro a ser utilizado como modelo para projetar os sinais no grafico de saida.}
}
\value{
Grafico interativo plotly contendo o grafico de STOCSY.
}
\description{
Produz um grafico de STOCSY determinando a correlacao entre um sinal de referencia (driver) e os demais
sinais no espectro.
}
\examples{
\dontrun{
#Exemplo de uso no modo covariancia
stocsy(Spectrum_data.WS,c(1.15,1.23),mode = "cov")
}
}
