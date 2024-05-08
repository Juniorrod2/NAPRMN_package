#' Executa a remocao de valores constantes e invalidos (NA) nas matrizes de preprocessamento ou dados para estatisitica
#'
#' @param ndata Recebe uma matriz do tipo numerica a ser "limpa". A planilha deve conter apenas as variaveis a serem
#' filtradas, nao contendo nenhum metadado ou variavel do tipo categorica/Factor/Character.
#'
#' @return Objeto do tipo Matrix, contendo o dado filtrado sem as colulas constantes/invalidas.
#' @export
#'
#' @examples
#' \dontrun{
#' planilha_filtrada <- CleanDataMatriz(planilha)
#' }
CleanDataMatrix <- function(ndata){
  # make sure no costant columns crop up
  varCol <- apply(data.frame(ndata), 2, var, na.rm=T); # getting an error of dim(X) must have a positive length, fixed by data.frame
  constCol <- (varCol == 0 | is.na(varCol));
  return(ndata[,!constCol, drop=FALSE]); # got an error of incorrect number of dimensions, added drop=FALSE to avoid vector conversion
}
