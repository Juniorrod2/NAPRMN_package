

#' Converte o objeto matrix gerado pelo PepsNMR em um dataframe formatado
#' @description
#' Esta função recebe o objeto retornado pelas funções de pre-processamento do pacote PepsNMR e retorna um objeto do tipo dataframe
#' formatado de maneira adequada para salvar a planilha ou ser utilizado em outras funções. Além disso, também faz a correção da ordem
#' alfabetica nos nomes das amostras.
#'
#' @param NMR_matrix
#' Matrix gerada por alguma das etapas de processamento do PepsNMR
#' @param DirNames
#' Boleano, indicando se o nome das amostras equivale ao numero do experimento. Se verdadeiro (TRUE) realiza a correção
#' da ordem alfanumerica dos numeros das amostras.
#'
#' @return Returns a formated dataframe
#' @export
#'
#' @examples \dontrun{NMR_dataproc <- pepsMatrixToDF(Spectrum_data.N)}
NMRMatrixAsDataframe <- function(NMR_matrix,DirNames=F){

  Df_real <- as.data.frame(Re(NMR_matrix))
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
#' @param Spectrum_data Objeto contendo os dados espectrais gerados a partir do PepsNMR. Pode estar na forma de matrix ou dataframe.
#' @param plot_resolution Caso o argumento limit_n_points seja TRUE, define para quantos porcento do número total de de pontos a resolução dos espectros será reduzida.
#' Recomendado usar valores multiplos de 0.25 para evitar serrilhamento do espectro.
#' @param limit_n_points Booleano, definindo se os espectros devem ou não sofre redução da resolução/numero de pontos antes de serem plotados.
#' Recomendado na visualização de multiplos espectros sem redução da janela espectral.
#' @param plot_only Vetor numerico definindo os indices dos espectros a serem plotados (Pode ser um unico indice ou vetor contendo multiplos indices)
#' . Se definido como zero, apenas os 10 primeiros
#' espectros serão representados (Padrão) ou o numero total de espectros se menor que 10.
#'
#' @param Spectrum_window Recebe um vetor de tamanho 2, contendo os limites superior e inferior da janela espectral a ser representada
#'
#' @return Retorna um objeto de grafico interativo do tipo plotly
#' @export
#' @importFrom magrittr `%>%`
#' @examples \dontrun{Plot_NMR <- plot_interactive_Spectra(Bins_data)}
plot_interactive_spectra <- function(Spectrum_data,plot_resolution=0.25,limit_n_points=F,plot_only=0,Spectrum_window=NULL){

#Caso apenas um espectro seja plotado, ele sera duplicado para evitar a coercao da matrix em vetor
  if(length(plot_only)==1&&plot_only!=0){
    plot_only <- c(plot_only,plot_only)
  }

#Por padrão apenas 10 espectros seram plotados, exceto se o usuario especificar a quantidade a ser plotada
  if(length(plot_only)==1&&plot_only==0){
    if(nrow(Spectrum_data)>10){
      plot_only=1:10
    }else plot_only=1:nrow(Spectrum_data)
  }

  if(!is.null(Spectrum_window)){
    Spectrum_data <- PepsNMR::WindowSelection(Spectrum_data,from.ws = Spectrum_window[1],to.ws = Spectrum_window[2])
  }

  n_data_points <- round(dim(Spectrum_data)[2]*plot_resolution)

  if(limit_n_points==F&&is.matrix(Spectrum_data)){
    Spectrum_data <- NMRMatrixAsDataframe(Spectrum_data[plot_only,])
  }
  if(limit_n_points==T&&is.matrix(Spectrum_data)){
    Spectrum_data<- PepsNMR::Bucketing(Spectrum_data[plot_only,],width = F,mb = n_data_points)%>%NMRMatrixAsDataframe()
  }

  plt <- tidyr::gather(Spectrum_data,-Sample,value = "Intensity",key = "ppm")%>%ggplot2::ggplot(ggplot2::aes(as.numeric(ppm),Intensity,color=as.character(Sample),group=Sample))+
    ggplot2::geom_line(ggplot2::aes(text=paste("Chemical Shift:",round(as.numeric(ppm),6)))) + ggplot2::scale_x_reverse(breaks=seq(-0.5,12,0.1)) + ggplot2::labs(y="Intensity (Relative)",x="Chemical shift (ppm)",color="Sample identification")
  plotly::ggplotly(plt,tooltip = c("text","y","group"))

}


#'  Plot NMR Spectra
#'
#' This function generates line plots for one or multiple NMR spectra stored in a
#' matrix or data frame format. It supports automatic selection of samples to plot,
#' optional bucketing, and cropping the spectrum to a defined chemical-shift window.
#'
#' @param Spectrum_data Matrix or data frame containing NMR spectral data.
#' Rows represent samples and columns represent chemical shifts (ppm).
#'   Must include a column named `"Sample"` if already converted to data frame.
#'
#' @param plot_resolution Numeric (default = 0.25).
#' Defines the resolution used when bucketing (i.e., proportion of total points kept).
#'
#' @param limit_n_points Logical (default = `FALSE`).
#' If `TRUE`, the spectra are bucketed using \code{PepsNMR::Bucketing}.
#'   If `FALSE`, the full resolution is retained.
#'
#' @param plot_only Integer vector or single integer (default = 0).
#' Specifies which samples to plot.
#'   - If `0`, the first 10 samples (or all, if <10) are plotted automatically.
#'   - If a single non-zero value is provided, that sample is plotted twice (internal handling).
#'   - If a vector is provided, the selected samples are plotted.
#'
#' @param Spectrum_window Numeric vector of length 2 (default = `NULL`).
#'Chemical shift window for cropping the spectra (ppm).
#'   Values are passed to \code{PepsNMR::WindowSelection(from.ws, to.ws)}.
#'
#' @returns A `ggplot2` object containing the plotted spectra.
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # Assuming `Spectra` is a matrix of NMR data with samples in rows:
#' plot_spectra(Spectra)
#'
#' # Plot only samples 1, 3 and 5:
#' plot_spectra(Spectra, plot_only = c(1, 3, 5))
#'
#' # Apply spectral window cropping:
#' plot_spectra(Spectra, Spectrum_window = c(0.5, 10))
#'
#' # Limit number of points via bucketing:
#' plot_spectra(Spectra, limit_n_points = TRUE, plot_resolution = 0.1)
#' }
plot_spectra <- function (Spectrum_data, plot_resolution = 0.25, limit_n_points = F,
                          plot_only = 0, Spectrum_window = NULL)
{
  if (length(plot_only) == 1 && plot_only != 0) {
    plot_only <- c(plot_only, plot_only)
  }
  if (length(plot_only) == 1 && plot_only == 0) {
    if (nrow(Spectrum_data) > 10) {
      plot_only = 1:10
    }
    else plot_only = 1:nrow(Spectrum_data)
  }
  if (!is.null(Spectrum_window)) {
    Spectrum_data <- PepsNMR::WindowSelection(Spectrum_data,
                                              from.ws = Spectrum_window[1], to.ws = Spectrum_window[2])
  }
  n_data_points <- round(dim(Spectrum_data)[2] * plot_resolution)
  if (limit_n_points == F && is.matrix(Spectrum_data)) {
    Spectrum_data <- NMRMatrixAsDataframe(Spectrum_data[plot_only,
    ])
  }
  if (limit_n_points == T && is.matrix(Spectrum_data)) {
    Spectrum_data <- PepsNMR::Bucketing(Spectrum_data[plot_only,
    ], width = F, mb = n_data_points) %>% NMRMatrixAsDataframe()
  }
  plt <- tidyr::gather(Spectrum_data, -Sample, value = "Intensity",
                       key = "ppm") %>% ggplot2::ggplot(ggplot2::aes(as.numeric(ppm),
                                                                     Intensity, color = as.character(Sample), group = Sample)) +
    ggplot2::geom_line(ggplot2::aes(text = paste("Chemical Shift:",
                                                 round(as.numeric(ppm), 6)))) + ggplot2::scale_x_reverse(breaks = seq(-0.5,
                                                                                                                      12, 0.1)) + ggplot2::labs(y = "Intensity (Relative)",
                                                                                                                                                x = "Chemical shift (ppm)", color = "Sample identification")
  return(plt)
}


#' Plot Full Spectra With Overlaid Binned Intensities
#'
#' This function creates an interactive plot combining raw NMR spectra
#' with overlaid binned spectral intensities.
#' The binned values are summarized across samples (default: mean) and displayed
#' as vertical bars on top of the full-resolution spectra.
#'
#' @param full_spectra Matrix or data frame containing full-resolution NMR spectra.
#' Passed to \code{plot_spectra()}. Rows represent samples; columns represent ppm values.
#'
#' @param bin_data Matrix or data frame containing already-binned spectra.
#' Columns must correspond to bins, whose names represent bin centers (ppm).
#'
#' @param bin_width Numeric. Width of each bin (in ppm) used for plotting the bar overlays.
#'
#' @param summary_function Function applied across samples to summarize each bin
#' Useful summary functions: Mean; median; sum; sd; max; min.
#'
#' @param plot_only Integer or vector
#' Samples to plot in the full spectra.
#'   Passed directly to \code{plot_spectra()}.
#'
#' @param bin_round_precision Integer. Number of decimal places used when rounding
#' Numeric precision used for plotting and attributing bins to metabolites
#'
#' @returns A \code{plotly} interactive plot showing the full spectra and the overlaid bins.
#' @export
#'
#' @examples
#' \dontrun{
#' # full_spectra: matrix of spectra (samples x ppm points)
#' # bin_data: binned spectra (samples x bins)
#'
#' overlaid_bins_plot(
#'   full_spectra = my_full_spectra,
#'   bin_data = my_bin_matrix,
#'   bin_width = 0.05
#' )
#'
#' # Using a custom summary function
#' overlaid_bins_plot(
#'   full_spectra = my_full_spectra,
#'   bin_data = my_bin_matrix,
#'   bin_width = 0.05,
#'   summary_function = median
#' )
#' }
#'
overlaid_bins_plot <- function(full_spectra,
                               bin_data,
                               bin_width,
                               summary_function=mean,
                               plot_only=0,
                               bin_round_precision=4){

  full_spectra_plot <- plot_spectra(full_spectra,plot_only = plot_only)

  median_binned_spectra <- dplyr::summarise_all(as.data.frame(bin_data),summary_function)

  median_binned_spectra <- tidyr::gather(median_binned_spectra,key="bin",value="intensity")


  binned_spectra <- full_spectra_plot + ggplot2::geom_col(data=median_binned_spectra,
                                                          mapping = ggplot2::aes(round(as.numeric(bin),bin_round_precision),
                                                                                 intensity),
                                                          inherit.aes = F,
                                                          width = bin_width,
                                                          just = 0.5,
                                                          color="red",
                                                          position = ggplot2::position_identity())

  plotly::ggplotly(binned_spectra)
}


#' Alinhamento espectral via metodo CluPA
#' @description
#' Aplica o metodo hierarchical Cluster-based Peak Alignment (CluPA) aos espectros, seguindo os parametros fonecidos pelo usuario.
#' Esta funcao foi otimizada para funcionar a partir dos espectros processados com o pacote PepsNMR, podendo ter comportamentos
#' diferentes do previsto se utilizada com dados de outras ferramentas.
#' Para detalhes extras, ver a documentacao do pacote "speaq" onde o algoritmo usado aqui foi implementado originalmente.
#'
#' @param spectra
#' Matriz de espectros gerada pelo PepsNMR, contendo os deslocamentos quimicos nas colunas e os espectros nas linhas.
#' OBS: Não realizar a conversao usando a funcao "PepsMatrixtoDF()"
#' @param nDivRange_ppm
#' Tamanho de bin (em ppm) utilizado para a segmentacao do espectro durante o processo de identificacao dos picos.
#' @param maxShifts_ppm
#' Deslocamento maximo permitido (em ppm) durante o alinhamento para cada sinal identificado. Pode ser utilizado o valor "auto"
#' para que o algoritmo determine automaticamente o deslocamento ideal.
#' @param baselineThreshold
#' Linha de corte de intesidade para a deteccao dos picos. Determinar com base na inspecao dos espectros.
#' @param SNR.Th
#' Razao sinal/Ruido considerada durante a deteccao dos picos. Pode receber o valor -1 onde o algoritmo determina automicaticamente a
#' razao sinal/ruido
#' @param show_info
#' Recebe True/False. Configura se as mensagens de progresso devem ser mostradas durante o processo.
#'
#' @return
#' Uma matriz no mesmo formato do dado de entrada (Porem contendo apenas a parte real do espectro) ja alinhada.
#' @export
#'
#' @examples
#' #aling_spec1 <- Spectra_align(spec)
Spectra_align <- function(spectra,nDivRange_ppm=0.04,maxShifts_ppm=0.015,baselineThreshold=5E6,SNR.Th = -1,show_info=T){

  ppm_resolution <- abs(stats::median(diff(as.numeric(colnames(spectra)))))

  nDivRange <- round(nDivRange_ppm/ppm_resolution)

  if(maxShifts_ppm=="auto"){
    maxShifts <- NULL
  }else{
  maxShifts <- round(maxShifts_ppm/ppm_resolution)
  }


  spectra_real <- Re(spectra)

  peakList <- speaq::detectSpecPeaks(spectra_real,
                              nDivRange = nDivRange,
                              scales = seq(1, 16, 2),
                              baselineThresh = baselineThreshold,
                              SNR.Th = SNR.Th,
                              verbose=show_info
  );

  refInd<- speaq::findRef(peakList)$refInd

  aligned_spectra <- speaq::dohCluster(spectra_real,
                  peakList = peakList,
                  refInd = refInd,
                  maxShift  = maxShifts,
                  acceptLostPeak = TRUE, verbose=show_info)

  return(aligned_spectra)
}


#' Calcula a resolucao dos spectros
#' @description
#' Funcao auxiliar criada para calcular a resolucao da matriz de espectros fonecida.
#'
#'
#' @param spectra
#' Matriz no formato PepsNMR contendo os espectros.
#' @param npoints
#' Se utilizado, a funcao passa a retornar o comprimento equivalente em deslocamento quimico
#' referente ao numero de pontos fornecido neste argumento. Para calcular apenas a resolucao, manter em NULL.
#' @param ppm
#' Se utilizado, a funcao passa a retornar o numero de pontos no espectro correspondente ao
#' comprimento em ppm forncecido. Para calcular apenas a resolucao, manter em NULL.
#'
#' @return
#' Um valor ou string informando a resolucao ou as conversoes entre ppm para numero de pontos e
#' numero de pontos para ppm.
#' @export
#'
#' @examples
#' #calculate_spectrum_resolution(spec)
#' #calculate_spectrum_resolution(spec,npoints = 128)
#' #calculate_spectrum_resolution(spec,ppm=0.04)
#'
calculate_spectrum_resolution <- function(spectra,npoints=NULL,ppm=NULL){

  ppm_resolution <- abs(stats::median(diff(as.numeric(colnames(spectra)))))

  if(is.null(npoints)&&is.null(ppm)){
    return(ppm_resolution)
  }else{
      if(is.null(ppm)){
      return(cat(npoints,"points equals",(npoints*ppm_resolution),"ppm"))
      }else{
        return(cat(ppm,"ppm equals",(ppm/ppm_resolution),"points"))
      }
  }

}
