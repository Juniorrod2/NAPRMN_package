

#' Converte o objeto matrix gerado pelo PepsNMR em um dataframe formatado
#' @description
#' Esta função recebe o objeto retornado pelas funções de pre-processamento do pacote PepsNMR e retorna um objeto do tipo dataframe
#' formatado de maneira adequada para salvar a planilha ou ser utilizado em outras funções. Além disso, também faz a correção da ordem
#' alfabetica nos nomes das amostras.
#'
#' @param Pepsmatrix
#' Matrix gerada por alguma das etapas de processamento do PepsNMR
#' @param DirNames
#' Boleano, indicando se o nome das amostras equivale ao numero do experimento. Se verdadeiro (TRUE) realiza a correção
#' da ordem alfanumerica dos numeros das amostras.
#'
#' @return Returns a formated dataframe
#' @export
#'
#' @examples \dontrun{NMR_dataproc <- pepsMatrixToDF(Spectrum_data.N)}
pepsMatrixToDF <- function(Pepsmatrix,DirNames=F){

  Df_real <- as.data.frame(Re(Pepsmatrix))
  Df_real <- cbind("Sample"=rownames(Df_real),Df_real)

  if(DirNames==T){
    x <- vector(length = dim(Df_real)[1])

    for(i in 1:max(as.numeric(Df_real[[1]]))){
      if(i%in%Df_real[[1]]){
        x[i] <-  which(Df_real[,1]==i)
      }
    }
    Df_real <- Df_real[x,]
  }else{
    Df_real <- Df_real[order(Df_real$Sample),]
  }
}


#' Plota um grafico interativo a partir de qualquer um dos objetos gerados durante o processamento via PepsNMR
#'
#'
#' @param Spectrum_data Objeto contendo os dados espectrais gerados a partir do PepsNMR (Normalmente será uma matrix)
#' @param plot_resolution Caso o argumento limit_n_points seja TRUE, define para quantos porcento do número total de de pontos a resolução dos espectros será reduzida.
#' Recomendado usar valores multiplos de 0.25 para evitar serrilhamento do espectro.
#' @param limit_n_points Boleano, definindo se os espectros devem ou não sofre redução da resolução/numero de pontos antes de serem plotados.
#' Recomendado caso possua muitos espectros ou esteja rodando em uma maquina mais fraca. Por padrão, a resolução será limitada a 10000 pontos/espectro
#' @param plot_only Vetor numerico definindo os indices dos espectros a serem plotados. Se definido como zero, todos os espectros na matrix serao plotados (Padrao).
#'  Por conta da coersao automatica do tipo de dado, nao e possivel plotar apenas um espectro, de forma que se for fornecido apenas um valor o espectro seguinte sera plotado automaticamente.
#'
#' @return Retorna um objeto de grafico interativo do tipo plotly
#' @export
#' @importFrom magrittr `%>%`
#' @examples \dontrun{Plot_NMR <- plot_interactive_Spectra(Bins_data)}
plot_interactive_Spectra <- function(Spectrum_data,plot_resolution=0.25,limit_n_points=T,plot_only=0){

  if(length(plot_only)==1&&plot_only==0){
    plot_only <- 1:dim(Spectrum_data)[1]
  }

  if(length(plot_only)==1&&plot_only!=0){
    plot_only <- plot_only:(1+plot_only)
  }

  n_data_points <- round(dim(Spectrum_data)[2]*plot_resolution)

  if(limit_n_points==F&&is.matrix(Spectrum_data)){
    Spectrum_data <- pepsMatrixToDF(Spectrum_data[plot_only,])
  }
  if(limit_n_points==T&&is.matrix(Spectrum_data)){
    Spectrum_data<- PepsNMR::Bucketing(Spectrum_data[plot_only,],width = F,mb = n_data_points)%>%pepsMatrixToDF()
  }

  plt <- tidyr::gather(Spectrum_data,-Sample,value = "Values",key = "int")%>%ggplot2::ggplot(ggplot2::aes(as.numeric(int),Values,color=as.character(Sample),group=Sample))+
    ggplot2::geom_line() + ggplot2::scale_x_reverse(breaks=seq(-0.5,12,0.1)) + ggplot2::labs(y="Intensity (Relative)",x="Chemical shift (ppm)",color="Sample identification")
  plotly::ggplotly(plt)
}
