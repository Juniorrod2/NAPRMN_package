



#' Calcula as integrais a partir de uma planilha processada de espectros de RMN, recomendada para uso com dados processados pelo pacote PepsNMR
#'
#' @description
#'  Integra as regiões fornecidas no argumento integration_intervals, preferencialmente deve ser um dataframe produzido a partir de uma planilha do excel.
#'  Deve obrigatoriamente conter 3 colunas, sendo a primeira a identificação dos metabolitos, a segunda o deslocamento quimico inicial de cada integral e a terceira o
#'  deslocamento quimico final de cada integral. Os nomes de cada coluna pode ser escolhido livremente, mas a ordem deve permancer a mesma.
#'
#' @param spectral_matrix Dataframe gerado do processamento dos dados espectrais, contendo a identificação das amostras na primeira coluna e
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

NMR_integration <- function(spectral_matrix,integration_intervals){

  region <- integration_intervals
  integral <- spectral_matrix[1]
  full_data <- tidyr::gather(spectral_matrix,-1,key = "ppm",value = "int")


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





