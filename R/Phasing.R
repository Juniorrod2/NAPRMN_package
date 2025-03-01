#' Aplicação Automática de Faseamento em Espectros de RMN
#'
#' Esta função realiza o faseamento automático de espectros de RMN utilizando o pacote NMRphasing.
#'
#' @param spectra Matriz numérica contendo os espectros de RMN a serem faseados. Cada linha representa um espectro individual.
#' @param phasingMethod Método de faseamento a ser utilizado. O valor padrão é `"SPC_AAM"`. Consulte a documentação do pacote NMRphasing para mais detalhes sobre os métodos disponíveis.
#' @param absorptionOnly Booleano indicando se apenas a componente de absorção deve ser considerada no faseamento. O valor padrão é `FALSE`.
#' @param withBC Booleano indicando se a correção de linha de base deve ser aplicada durante o faseamento. O valor padrão é `FALSE`.
#'
#' @return
#' Retorna uma matriz contendo os espectros faseados, mantendo o mesmo formato da entrada.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Supondo que "spectra_data" seja uma matriz contendo espectros de RMN
#' spectra_phased <- autophase(spectra_data)
#' }
#'
autophase <- function(spectra,phasingMethod="SPC_AAM",absorptionOnly = F,withBC = F){

  for(i in 1:dim(spectra)[1]){
    spec <- spectra[i,]
    phased_spec <-NMRphasing::NMRphasing(specDatIn = spec,method = phasingMethod,
                                         absorptionOnly = absorptionOnly,withBC = withBC)
    spectra[i,] <- phased_spec
  }

  return(spectra)
}


#' Comparação de Métodos de Faseamento para Espectros de RMN
#'
#' Esta função aplica e compara diferentes métodos de faseamento a um espectro de RMN de referência.
#'
#' @param spectra Matriz numérica contendo os espectros de RMN a serem analisados. Cada linha representa um espectro individual.
#' @param fid_info Informações do domínio do tempo (FID) necessárias para o referenciamento interno dos espectros faseados.
#' @param ref Índice da linha na matriz `spectra` que será utilizada como espectro de referência para a comparação dos métodos de faseamento. O valor padrão é `1`.
#' @param absorptionOnly Booleano indicando se apenas a componente de absorção deve ser considerada no faseamento. O valor padrão é `FALSE`.
#' @param withBC Booleano indicando se a correção de linha de base deve ser aplicada durante o faseamento. O valor padrão é `FALSE`.
#' @param phasingMethods Vetor de caracteres contendo os nomes dos métodos de faseamento a serem testados. Os métodos disponíveis incluem `"NLS"`, `"MPC_DAOM"`, `"MPC_EMP"`, `"MPC_AAM"`, `"MPC_DSM"`, `"MPC_ADSM"`, `"SPC_DAOM"`, `"SPC_EMP"`, `"SPC_AAM"`, `"SPC_DSM"`, e `"SPC_ADSM"`. O valor padrão inclui todos esses métodos.
#'
#' @return
#' Retorna uma matriz contendo os espectros de referência faseados por cada um dos métodos especificados, juntamente com o espectro faseado utilizando `PepsNMR::ZeroOrderPhaseCorrection`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Supondo que "spectra_data" seja uma matriz contendo espectros de RMN e "fid_info_data" as informações do FID
#' phased_spectra <- compare_phasing_methods(spectra_data, fid_info_data)
#' }
#'
compare_phasing_methods <- function(spectra,fid_info,ref=1,absorptionOnly = F,withBC = F,
                                    phasingMethods=c("NLS","MPC_DAOM","MPC_EMP", "MPC_AAM","MPC_DSM",
                                                     "MPC_ADSM", "SPC_DAOM", "SPC_EMP", "SPC_AAM",
                                                     "SPC_DSM", "SPC_ADSM")){
  spec <- spectra[ref,]
  phasingMatrix <- PepsNMR::ZeroOrderPhaseCorrection(spec)


  for(i in phasingMethods){
    assign(i,NMRphasing::NMRphasing(specDatIn = spec,method = i,absorptionOnly = absorptionOnly,withBC = withBC))
    phasingMatrix <- rbind(phasingMatrix,eval(parse(text = i)))
  }

  rownames(phasingMatrix) <- c("PepsNMR",phasingMethods)
  phasingMatrix <- PepsNMR::InternalReferencing(phasingMatrix,Fid_info = fid_info)
  return(phasingMatrix)
}
