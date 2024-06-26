% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Normalization.R
\name{DataNormalization}
\alias{DataNormalization}
\title{Normalização de dados}
\usage{
DataNormalization(
  data,
  rowNorm = "",
  transNorm = "",
  scaleNorm = "",
  ref = NULL,
  ratio = FALSE,
  ratioNum = 20
)
}
\arguments{
\item{data}{Matriz contendo os valores das variáveis no formato, amostras nas linhas.
Esta função suporta apenas objetos do tipo matrix. Portanto as variaveis categoricas devem ser removidas antes da normalização}

\item{rowNorm}{Define a normalização:
Quantile: "QuantileNorm"; PQN: "SamplePQN"; PQN por grupo: "GroupPQN"; Norm. p/ metabolito de referencia: "CompNorm"; soma: "SumNorm"; Mediana: "MedianNorm"
Não definir para não normalizar}

\item{transNorm}{Define transformação logaritimica/exponencial:
log: "LogNorm"
sqrt root: "SrNorm"
cubic root: "CrNorm"
Não definir para não aplicar transformação}

\item{scaleNorm}{Define o escalonamento
Mean centering only: "MeanCenter"
Unit Variance: "AutoNorm"
Pareto scaling: "ParetoNorm"
Range scaling: "RangeNorm"
Não definir para não aplicar escalonamento}

\item{ref}{Define o espectro ou metabolito de referencia, necessário apenas se aplicados os metodos SamplePQN, GroupPQN ou CompNorm}
}
\value{
Matrix com as variaveis normalizadas
}
\description{
Permite a normalização pelos mesmos parametros do metaboanalist, abaixo segue os metodos possiveis para normalização, escalonamento e transformação
}
\examples{
\dontrun{
#Exemplo Quantile norm + Mean centering, sem transformação logartimica
Normalization_test <-  DataNormalization(bins[,-1],rowNorm = "Quantile",transNorm = "",scaleNorm = "MeanCenter")
}
}
