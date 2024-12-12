



#' Calcula as integrais a partir de uma planilha processada de espectros de RMN, recomendada para uso com dados processados pelo pacote PepsNMR
#'
#' @description
#'  Integra as regiões fornecidas no argumento integration_intervals, preferencialmente deve ser um dataframe produzido a partir de uma planilha do excel.
#'  Deve obrigatoriamente conter 3 colunas, sendo a primeira a identificação dos metabolitos, a segunda o deslocamento quimico inicial de cada integral e a terceira o
#'  deslocamento quimico final de cada integral. Os nomes de cada coluna pode ser escolhido livremente, mas a ordem deve permancer a mesma.
#'
#' @param Spectrum_data Dataframe gerado do processamento dos dados espectrais, contendo a identificação das amostras na primeira coluna e
#' as intensidades dos deslocamentos químicos nas demais.
#' @param integration_intervals Dataframe formatado a partir do Excel (preferencialmente), contendo a identificação dos metabolitos, inicio do intervalo para integração
#' e fim do intervalo para integração, respectivamente.
#' @details
#' O dataframe utilizado como input deve ser gerado a partir da funcao pepsMatrixToDF() ou possuir uma estrutura compativel.
#'
#'
#' @return
#' Um data frame contendo o valor da area das integrais no formato amostras nas linhas e metabolitos nas colunas
#' @export
#'
#'
#' @examples
#' #Exemplo de dataframe no formato correto para o argumento "Integration_intervals"
#'  region <- data.frame(metabolito=c("Leucina","Etanol","VLDL","metilhistidina","teste"),ppm_i=c(0.94,1.1,0.81,3.685,3.30),ppm_f=c(0.97,1.3,0.94,3.7,3.31))

NMR_integration <- function(Spectrum_data,integration_intervals){

  if(is.matrix(Spectrum_data)){
    Spectrum_data <- NMRMatrixAsDataframe(Spectrum_data)
  }

  region <- integration_intervals
  integral <- Spectrum_data[1]
  full_data <- tidyr::gather(Spectrum_data,-1,key = "ppm",value = "int")


  integrais <- 0
  for(i in 1:dim(region)[1]){
    sel <- dplyr::filter(full_data,ppm>=region[[i,2]],ppm<=region[[i,3]])
    sel <- tidyr::spread(sel,key="ppm",value="int")
    assign(region[[i,1]],sel)
    for(j in 1:dim(sel)[1]){
      integrais[j] <- pracma::trapz(as.double(colnames(sel[-1])),as.double(sel[j,-1]))
    }

    integral <-  cbind(integral,integrais)
  }

  colnames(integral) <- c("Sample",region[[1]])

  return(integral)

}



#' Permite gerar uma matriz hibrida entre bins equidistantes e regioes de integracao personalizadas
#' @description
#' Esta funcao gera um hibrido entre um sistema de integrais definidas pelo usuario e bins classicos de larguras equidistantes
#' Para todas as regioes que estiverm compreendidas no intevalo das integrais fonecidas, estas serao identificadas de acordo com
#' o metabolito sinalizado no dataframe integrals, enquanto todas as demais regioes serao processadas com bins de largura definida
#' no argumento bin_width. Para facilitar a leitura, todas as integrais sao movidas para o inicio da matriz, alem disso, as regioes
#' integradas sao removidas dos bins equidistantes para evitar duplicatas de variaveis.
#'
#' @param Spectrum_data
#' Matrix contendo os espectros processados de acordo com o pacote PepsNMR.
#' @param integrals
#' Dataframe derivado da planilha contendo as regioes de integracao. Utilizar o mesmo modelo exigido para a funcao
#' NMR_integration()
#' @param bin_width
#' Largura de bin aplicada para as regioes nao inclusas nas regioes determinadas no dataframe "integrals"
#'
#' @return
#' Um dataframe contendo as integrais e bins gerados.
#' @export
#'
#' @examples
#' #NMR_hbins <- hibrid_bucketing(spectra = spec,integrals = regioes_integrais,bin_width = 0.01)
hibrid_bucketing <- function(Spectrum_data,integrals,bin_width=0.01){
  if(is.matrix(Spectrum_data)){
  real_spectra <- NMRMatrixAsDataframe(Spectrum_data)
  }else{
    real_spectra <- Spectrum_data
  }


  integrated_signals <- NMR_integration(real_spectra,integrals)

  integrated_regions <- list()
  for(i in 1:dim(regioes_integrais)[1]){
    integrated_regions[[i]] <- c(regioes_integrais[[i,2]],regioes_integrais[[i,3]])
  }
  names(integrated_regions) <- regioes_integrais[[1]]

  unintegrated_spectra <- PepsNMR::RegionRemoval(spec,fromto.rr = integrated_regions,
                                        typeofspectra = "manual")

  unintegrated_spectra <- PepsNMR::Bucketing(unintegrated_spectra,width = T,mb=bin_width,intmeth = "t")
  unintegrated_spectra <- NMRMatrixAsDataframe(unintegrated_spectra)

  unintegrated_spectra <- CleanDataMatrix(as.matrix(unintegrated_spectra[-1]))

  hibrid_binning_data <- cbind(integrated_signals,unintegrated_spectra)
}


