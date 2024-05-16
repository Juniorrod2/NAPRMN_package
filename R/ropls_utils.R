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


#' Identifica os bins referentes a cada metabolito a partir de uma planilha de anotacao de metabolitos
#'
#' @param integrais
#' Planilha de integrais, no mesmo formato utilizado para a funcao de integracao de regioes.
#' @param loadings
#' planilha contendo os dados dos loadings de um objeto ropls. Gerada pela funcao extract_ropls_data(),
#' fornecer apenas a planilha de loadings presente no objeto do tipo list retornado.
#' @param use_mean
#' Define se deve ser realizada a media dos dados para bins referntes ao mesmo metabolito.
#' Se TRUE (valor padrao), sera retornado apenas um bin para cada metabolito, referente ao bin medio
#' calculado entre todos os bins dentro da regiao de integracao do sinal, alem dos valores de vip medio
#' e as coordenadas do bin medio. Se FALSE, todos os bins referentes ao mesmo sinal e seus devidos dados
#' sao fornecidos individualmente duplicando a identificacao do metabolito quantas vezes forem necessarias.
#'
#' @return
#' Planilha contendo apenas os bins referentes as areas dos sinais identificados e suas devidas informacoes
#'
#' @export
#'
#' @examples
#' #model_data <- extract_ropls_data(model)
#' #annotated_data <- annotate_bins(model_data$loadings)
annotate_bins <- function(integrais,loadings,use_mean=T,return_undentified=F){
  loading_opls <- loadings
  # Filtra apenas os bins referentes metabolitos identificados, juntamente com seu loading e vip no objeto "loading"
  loading <- data.frame()

  for(i in 1:dim(integrais)[1]){

    loading_filter <- dplyr::filter(loading_opls,bins>=as.numeric(integrais[i,2]),bins<=as.numeric(integrais[i,3]))
    loading_filter$metabolite <- rep(as.character(integrais[i,1]),dim(loading_filter)[1])
    if(dim(loading)[1]==0){
      loading <- loading_filter
    }else{
      loading <- rbind(loading,loading_filter)
    }

  }

  if(return_undentified==T){
    use_mean=F

    loading_undentified <- dplyr::filter(loading_opls,!bins%in%loading$bins)

    loading_undentified$metabolite <- "undentified"

    loading <- rbind(loading,loading_undentified)
  }

  if(use_mean==T){
    #Gera as medias entre os todos bins para cada metabolito
    loading_opls <- dplyr::group_by(loading,metabolite)%>%dplyr::summarise_all(mean)
  }else{
    return(loading)
  }



}

