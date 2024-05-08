


SumNorm<-function(x){
  1000*x/sum(x, na.rm=T);
}

# normalize by median
MedianNorm<-function(x){
  x/median(x, na.rm=T);
}

# normalize by a reference sample (probability quotient normalization)
# ref should be the name of the reference sample
ProbNorm<-function(x, ref.smpl){
  x/median(as.numeric(x/ref.smpl), na.rm=T)
}

# normalize by a reference reference (i.e. creatinine)
# ref should be the name of the cmpd
CompNorm<-function(x, ref){
  1000*x/x[ref];
}

# perform quantile normalization on the raw data (can be log transformed later by user)
# https://stat.ethz.ch/pipermail/bioconductor/2005-April/008348.html
QuantileNormalize <- function(data){
  return(t(preprocessCore::normalize.quantiles(t(data), copy=FALSE)));
}


# generalize log, tolerant to 0 and negative values
LogNorm<-function(x, min.val){
  log10((x + sqrt(x^2 + min.val^2))/2)
}

# square root, tolerant to negative values
SquareRootNorm<-function(x, min.val){
  ((x + sqrt(x^2 + min.val^2))/2)^(1/2);
}

# normalize to zero mean and unit variance
AutoNorm<-function(x){
  (x - mean(x))/sd(x, na.rm=T);
}

# normalize to zero mean but variance/SE
ParetoNorm<-function(x){
  (x - mean(x))/sqrt(sd(x, na.rm=T));
}

# normalize to zero mean but variance/SE
MeanCenter<-function(x){
  x - mean(x);
}

# normalize to zero mean but variance/SE
RangeNorm<-function(x){
  if(max(x) == min(x)){
    x;
  }else{
    (x - mean(x))/(max(x)-min(x));
  }
}

#' Normalização de dados
#' @description
#' Permite a normalização pelos mesmos parametros do metaboanalist, abaixo segue os metodos possiveis para normalização, escalonamento e transformação
#'
#'
#' @param data Matriz contendo os valores das variáveis no formato, amostras nas linhas.
#' Esta função suporta apenas objetos do tipo matrix. Portanto as variaveis categoricas devem ser removidas antes da normalização
#' @param rowNorm Define a normalização:
#' Quantile: "QuantileNorm"; PQN: "SamplePQN"; PQN por grupo: "GroupPQN"; Norm. p/ metabolito de referencia: "CompNorm"; soma: "SumNorm"; Mediana: "MedianNorm"
#' Não definir para não normalizar
#' @param transNorm Define transformação logaritimica/exponencial:
#' log: "LogNorm"
#' sqrt root: "SrNorm"
#' cubic root: "CrNorm"
#' Não definir para não aplicar transformação
#'
#' @param scaleNorm Define o escalonamento
#' Mean centering only: "MeanCenter"
#' Unit Variance: "AutoNorm"
#' Pareto scaling: "ParetoNorm"
#' Range scaling: "RangeNorm"
#' Não definir para não aplicar escalonamento
#'
#'
#' @param ref Define o espectro ou metabolito de referencia, necessário apenas se aplicados os metodos SamplePQN, GroupPQN ou CompNorm
#'
#'
#'
#' @return Matrix com as variaveis normalizadas
#' @export
#'
#' @examples
#' \dontrun{
#' #Exemplo Quantile norm + Mean centering, sem transformação logartimica
#' Normalization_test <-  DataNormalization(bins[,-1],rowNorm = "Quantile",transNorm = "",scaleNorm = "MeanCenter")
#' }
DataNormalization <- function(data, rowNorm="", transNorm="", scaleNorm="", ref=NULL, ratio=FALSE, ratioNum=20){

  colNames <- colnames(data);
  rowNames <- rownames(data);

  # row-wise normalization
  if(rowNorm=="QuantileNorm"){
    data<-QuantileNormalize(data);
    # this can introduce constant variables if a variable is
    # at the same rank across all samples (replaced by its average across all)

    varCol <- apply(data, 2, var, na.rm=T);
    constCol <- (varCol == 0 | is.na(varCol));
    constNum <- sum(constCol, na.rm=T);
    if(constNum > 0){
      print(paste("After quantile normalization", constNum, "features with a constant value were found and deleted."));
      data <- data[,!constCol, drop=FALSE];
      colNames <- colnames(data);
      rowNames <- rownames(data);
    }
    rownm<-"Quantile Normalization";
  }else if(rowNorm=="GroupPQN"){
    grp.inx <- cls == ref;
    ref.smpl <- apply(data[grp.inx, , drop=FALSE], 2, mean);
    data<-t(apply(data, 1, ProbNorm, ref.smpl));
    rownm<-"Probabilistic Quotient Normalization by a reference group";
  }else if(rowNorm=="SamplePQN"){
    ref.smpl <- data[ref, , drop=FALSE];
    data<-t(apply(data, 1, ProbNorm, ref.smpl));
    rownm<-"Probabilistic Quotient Normalization by a reference sample";
  }else if(rowNorm=="CompNorm"){
    data<-t(apply(data, 1, CompNorm, ref));
    rownm<-"Normalization by a reference feature";
  }else if(rowNorm=="SumNorm"){
    data<-t(apply(data, 1, SumNorm));
    rownm<-"Normalization to constant sum";
  }else if(rowNorm=="MedianNorm"){
    data<-t(apply(data, 1, MedianNorm));
    rownm<-"Normalization to sample median";
  }else if(rowNorm=="SpecNorm"){
    if(!exists("norm.vec")){
      norm.vec <- rep(1,nrow(data)); # default all same weight vec to prevent error
      print("No sample specific information were given, all set to 1.0");
    }
    rownm<-"Normalization by sample-specific factor";
    data<-data/norm.vec;
  }else{
    # nothing to do
    rownm<-"N/A";
  }

  # use apply will lose dimension info (i.e. row names and colnames)

  rownames(data)<-rowNames;
  colnames(data)<-colNames;


  # if the reference by feature, the feature column should be removed, since it is all 1
  if(rowNorm=="CompNorm" && !is.null(ref)){
    inx<-match(ref, colnames(data));
    data<-data[,-inx, drop=FALSE];
    colNames <- colNames[-inx];
  }



  # transformation
  # may not be able to deal with 0 or negative values
  if(transNorm=='LogNorm'){
    min.val <- min(abs(data[data!=0]))/10;
    data<-apply(data, 2, LogNorm, min.val);
    transnm<-"Log10 Normalization";
  }else if(transNorm=='SrNorm'){
    min.val <- min(abs(data[data!=0]))/10;
    data<-apply(data, 2, SquareRootNorm, min.val);
    transnm<-"Square Root Transformation";
  }else if(transNorm=='CrNorm'){
    norm.data <- abs(data)^(1/3);
    norm.data[data<0] <- - norm.data[data<0];
    data <- norm.data;
    transnm<-"Cubic Root Transformation";
  }else{
    transnm<-"N/A";
  }


  if(scaleNorm=='MeanCenter'){
    data<-apply(data, 2, MeanCenter);
    scalenm<-"Mean Centering";
  }else if(scaleNorm=='AutoNorm'){
    data<-apply(data, 2, AutoNorm);
    scalenm<-"Autoscaling";
  }else if(scaleNorm=='ParetoNorm'){
    data<-apply(data, 2, ParetoNorm);
    scalenm<-"Pareto Scaling";
  }else if(scaleNorm=='RangeNorm'){
    data<-apply(data, 2, RangeNorm);
    scalenm<-"Range Scaling";
  }else{
    scalenm<-"N/A";
  }

  return(data)
}


