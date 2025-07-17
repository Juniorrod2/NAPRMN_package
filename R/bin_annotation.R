#' Filtra e anota bins espectrais
#'
#' Esta função filtra um conjunto de dados espectrais com base em anotações fornecidas e pode retornar os dados em formato longo ou largo.
#'
#' @param bins_data Data frame contendo os dados espectrais, onde as colunas (exceto as especificadas em `ignore_columns`) representam os bins espectrais.
#' @param annotation_data Lista ou data frame contendo duas colunas: a primeira com os nomes dos metabólitos e a segunda com os bins correspondentes.
#' @param ignore_columns Vetor de caracteres especificando as colunas do `bins_data` que não devem ser consideradas como bins espectrais.
#' @param number_of_decimal_places Inteiro indicando o número de casas decimais a serem consideradas para arredondamento dos bins. Padrão é 4.
#' @param long_format Lógico. Se `TRUE`, retorna os dados em formato longo; se `FALSE`, retorna em formato largo. Padrão é `TRUE`.
#'
#' @return Um data frame contendo os dados filtrados e anotados. O formato do retorno depende do argumento `long_format`.
#'
#'
#' @examples
#' # Exemplo de uso:
#' bins_data <- data.frame(SampleID = c("S1", "S2"), "1.2345" = c(0.5, 0.6), "2.3456" = c(0.3, 0.4),check.names = FALSE)
#' annotation_data <- data.frame(Metabolito = c("Met1", "Met2"), bins = c(1.2345, 2.3456))
#' filter_annotated_bins(bins_data, annotation_data, ignore_columns = "SampleID")
#'
#' @export

filter_annotated_bins <- function(bins_data,annotation_data,ignore_columns,number_of_decimal_places=4,long_format=T){

  ignored_colums <- rlang::enquo(ignore_columns)
  annotation_dataframe <- data.frame("Metabolito"=annotation_data[[1]],"bins"=annotation_data[[2]])
  bins_long_format <- tidyr::gather(bins_data,-!!(ignored_colums),key="bins",value="relative_intensity")

  bins_long_format$bins <- round(as.numeric(bins_long_format$bins),number_of_decimal_places)

  bins_long_format <- dplyr::filter(bins_long_format,bins%in%annotation_dataframe$bins)
  bins_long_format <- dplyr::left_join(bins_long_format,annotation_dataframe,by="bins")
  bins_long_format$bins <- NULL

  if(long_format){
  return(bins_long_format)
  }else{
  bins_wide_format <- tidyr::spread(bins_long_format,key = "Metabolito",value = "relative_intensity")
  return(bins_wide_format)
  }

}



