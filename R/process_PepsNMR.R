

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
