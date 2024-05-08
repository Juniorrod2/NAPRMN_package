
#' Plota os espectros na vertical
#' @description
#' Plota os espectros projetados com vista superior. Util para localizar sinais isolados/discrepantes em apenas uma amostras ou delimitar a
#' area de corte para as integrais. No momento, exige a opcao dir.names = T, na funcao ReadFids do pacote PepsNMR.
#' @param spec_data Matriz gerada pelo PepsNMR contendo os espectros de RMN processados
#' @param plot_region Recebe um vetor onde o primeiro valor se refere ao inicio do intervalo e o segundo ao final do intervalo a ser plotado.
#' Delimita a subregião do espectro a ser plotada, util para reduzir o tamanho do grafico.
#' @param spec_resolution Define a resolucao a ser conservada nos espectros. Recebe um valor entre 1 e 0, onde 1 representa a resolucao original
#' e qualquer valor entre 1 e 0 o percentual da resolucao a ser mantido. Ex: Uma resolucao de 0.5 (Valor padrao) ira manter 16000 pontos em um espectro contendo
#' 32000 pontos.
#' @param plot_resolution Define a resolucao do gradiente de cor. recebe valores entre 1 e 0, onde valores maiores favorecem a representação de sinais mais intensos
#' e promove maior supressao do ruido na representacao e valores menores favorece sinais menos intensos, mas aumenta o ruido observado. Padronizado em 0.02
#'
#' @return
#' Retorna um grafico interativo no formato de objeto da classe plotly.
#' @export
#'
#' @examples
#' \dontrun{
#' plot_raster_spectrum(spec_data = Spectrum_data.WS[-3,],plot_region = c(0.5,4),plot_resolution = 0.03,spec_resolution = 0.1)
#' }
#'

plot_raster_spectrum <- function(spec_data,plot_region=c(0,9),spec_resolution=0.5,plot_resolution=0.02){

  if(spec_resolution!=1){lower_res_data <- PepsNMR::Bucketing(spec_data,
                                                              mb=round(dim(spec_data)[2]*spec_resolution))}else{
                                                                lower_res_data <- spec_data
                                                              }
  spec <- NAPRMN::pepsMatrixToDF(lower_res_data)
  spec <- spec[complete.cases(spec),]
  spec <- tidyr::gather(spec,-Sample,key="ppm",value="intensity")
  spec$ppm <- as.numeric(spec$ppm)
  spec <- dplyr::filter(spec,ppm>plot_region[1],ppm<plot_region[2])

  plot_object <- ggplot2::ggplot(spec,ggplot2::aes(as.numeric(ppm),Sample,fill=intensity)) +
    ggplot2::geom_raster()+ggplot2::scale_fill_gradientn(colours = c("gray","blue","red"),values = c(0,plot_resolution,1))+
    ggplot2::labs(x="Chemical Shift",y="Sample")+ggplot2::scale_x_reverse(breaks = seq(plot_region[1],plot_region[2],0.2))


  plotly::ggplotly(plot_object)
}
