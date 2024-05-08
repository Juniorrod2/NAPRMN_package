#' Produz um grafico de STOCSY determinando a correlacao entre um sinal de referencia (driver) e os demais
#' sinais no espectro.
#'
#' @param spectra Objeto contendo a matriz de espectros para calculo das correlacoes. O objeto deve ser uma
#' matriz gerada durante o processamento com o pacote PepsNMR ou estrutuda de dados equivalente.
#' @param driver_peak Vetor de tamanho 2, contendo o deslocamento quimico inicial e final do sinal ao qual
#' as correlacoes serao calculadas para plotar o STOCSY.
#' @param mode Afeta a visualizacao do espectro, onde o modo "cor"-Correlacao projeta as intensidades pesadas de acordo
#' com a correlacao para cada sinal e o modo "cov"-Covariancia projeta as covariancias entre o sinal driver e os demais.
#' O gradiente de cores e sempre determinado pela correlacao.
#' @param spectrum_resolution Define a resolucao espectral a ser plotada, por padrao 50%. Recebe um valor entre 0 e 1
#' relativo ao percentual de resolucao do espectro original que deve ser conservada no grafico.
#' @param ref_spectrum Define o espectro de referencia. Recebe um escalar ao qual deve corresponder a posicao numerica
#' do espectro a ser utilizado como modelo para projetar os sinais no grafico de saida.
#'
#' @return Grafico interativo plotly contendo o grafico de STOCSY.
#' @export
#'
#' @examples
#' \dontrun{
#' #Exemplo de uso no modo covariancia
#' stocsy(Spectrum_data.WS,c(1.15,1.23),mode = "cov")
#' }

stocsy <- function(spectra,driver_peak,mode="cor",spectrum_resolution=0.5,ref_spectrum=1){

  driver_peak <- driver_peak
  mode <- mode
  spectrum_resolution <- spectrum_resolution
  ref_spectrum <- ref_spectrum



  n_plot_points <- round(dim(spectra)[2]*spectrum_resolution)
  region <- data.frame("signal"="Drive","ppm_1"=driver_peak[1],"ppm_f"=driver_peak[2])

  Spectrum <- PepsNMR::Bucketing(spectra,width = F,mb =n_plot_points,intmeth = "t")
  Spectrum <- NAPRMN::pepsMatrixToDF(Spectrum)
  integrated_driver <- NAPRMN::NMR_integration(Spectrum,region)

  cor <- cor(integrated_driver$Drive,Spectrum[-1])
  cov <- cov(integrated_driver$Drive,Spectrum[-1])

  ref <- Spectrum[ref_spectrum,]
  ref <- tidyr::gather(ref,-Sample,value = "int",key="ppm")
  cor <- tidyr::gather(as.data.frame(cor),value = "corr",key="ppm")
  cov <- tidyr::gather(as.data.frame(cov),value = "cov",key="ppm")
  ref$adjInt <- ref$int*((cor$corr^2)*(cor$corr/abs(cor$corr))+0.01)
  ref$corr <- round((cor$corr^2)*(cor$corr/abs(cor$corr)),digits = 2)
  ref$cov <- cov$cov

  plot_cov <- ggplot2::ggplot(ref,ggplot2::aes(as.numeric(ppm),cov,group=Sample)) +
    ggplot2::geom_path()+ggplot2::geom_point(ggplot2::aes(color=corr),size=0.05)+ggplot2::scale_color_gradient2(low="blue",high = "red")+ggplot2::scale_x_reverse()

  if (mode=="cov"){
    Stocsy_plot <- ggplot2::ggplot(ref,ggplot2::aes(as.numeric(ppm),cov,group=Sample,color=corr,
                                                    xend=dplyr::lead(as.numeric(ppm)),yend=dplyr::lead(cov))) +
      ggplot2::geom_segment()+ggplot2::scale_color_gradient2(low = "green",mid ="blue",
                                                             high = "red",midpoint = 0,limits=c(-1,1))+
      ggplot2::scale_x_reverse(breaks=seq(10,0,-1))+ggplot2::labs(x="Chemical Shift")

  }else{
    Stocsy_plot <- ggplot2::ggplot(ref,ggplot2::aes(as.numeric(ppm),adjInt,group=Sample,color=corr,
                                    xend=dplyr::lead(as.numeric(ppm)),yend=dplyr::lead(adjInt))) +
                  ggplot2::geom_segment()+ggplot2::scale_color_gradient2(low = "green",mid ="blue",
                                                        high = "red",midpoint = 0,limits=c(-1,1))+
                  ggplot2::scale_x_reverse(breaks=seq(10,0,-1))+ggplot2::labs(x="Chemical Shift")
  }

  plotly::ggplotly(Stocsy_plot)
}
