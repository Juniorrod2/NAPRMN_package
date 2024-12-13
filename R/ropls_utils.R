#' Extrai no formato de planilha as informacoes de um modelo do pacote ropls
#'
#' @param ropls_object
#' Objeto da classe opls gerado pelo pacote ropls. Pode ser de qualquer um dos tipos: PCA, PLS-DA ou OPLS-DA.
#' @description
#' Extrai em um objeto os scores e loadings de um modelo de PCA, PLS-DA ou OPLS-DA gerados pelo
#' pacote ropls. A deteccao do tipo de modelo e executada automaticamente necessitando apenas fornecer
#' o objeto gerado pelo pacote ropls.
#'
#' @return
#' Retorna uma lista contendo dois dataframes. Um referente aos scores do modelo gerado e outro
#' referente aos loadings
#' @export
#'
#' @examples
#' #model <- opls(xdada,yvector, predI = 1, orthoI = 1)
#' #model_data <- extract_ropls_data(model)
extract_ropls_data <- function(ropls_object){

  if(ropls_object@typeC=="OPLS-DA"){
    op <- ropls_object
    #Extrai os scores e reinsere a identificacao das amostras e grupos
    scores_opls <- cbind(op@scoreMN,op@orthoScoreMN)
    scores_opls <- as.data.frame(scores_opls)
    scores_opls <- cbind("Samples"=rownames(op@scoreMN),"Group"=op@suppLs[["y"]],scores_opls)


    #Extrai os escores e os valores de vip
    loading_opls <- cbind("bins"=round(as.numeric(rownames(op@loadingMN)),4),
                          op@loadingMN,
                          op@orthoLoadingMN,
                          "Vip"=round(as.numeric(op@vipVn),2),
                          "OrthoVip"=round(as.numeric(op@orthoVipVn),2))
    loading_opls <- as.data.frame(loading_opls)

    extracted_data <- list("Scores"=scores_opls,"Loadings"=loading_opls)
    return(extracted_data)
  }

  if(ropls_object@typeC=="PLS-DA"){
    op <- ropls_object
    #Extrai os scores e reinsere a identificacao das amostras e grupos
    scores_opls <- cbind(op@scoreMN)
    scores_opls <- as.data.frame(scores_opls)
    scores_opls <- cbind("Samples"=rownames(op@scoreMN),"Group"=op@suppLs[["y"]],scores_opls)


    #Extrai os escores e os valores de vip
    loading_opls <- cbind("bins"=round(as.numeric(rownames(op@loadingMN)),4),
                          op@loadingMN,
                          "Vip"=round(as.numeric(op@vipVn),2))
    loading_opls <- as.data.frame(loading_opls)

    extracted_data <- list("Scores"=scores_opls,"Loadings"=loading_opls)
    return(extracted_data)
  }

  if(ropls_object@typeC=="PCA"){
    op <- ropls_object
    #Extrai os scores e reinsere a identificacao das amostras e grupos
    scores_opls <- cbind(op@scoreMN)
    scores_opls <- as.data.frame(scores_opls)
    scores_opls <- cbind("Samples"=rownames(op@scoreMN),scores_opls)


    #Extrai os escores e os valores de vip
    loading_opls <- cbind("bins"=round(as.numeric(rownames(op@loadingMN)),4),
                          op@loadingMN)
    loading_opls <- as.data.frame(loading_opls)

    extracted_data <- list("Scores"=scores_opls,"Loadings"=loading_opls)
    return(extracted_data)
  }

}

