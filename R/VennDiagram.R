


#' Title Cria um diagrama de Venn a partir dos de uma lista fornecida
#' @description Cria um diagrama de Venn a partir dos de uma lista fornecida com um objeto (Vetor) para cada grupo
#'
#' @param x Lista contendo o conteudo ao qual o Diagrama deve ser produzido. Cada objeto da lista deve ser um vetor de strings com os membros do grupo.
#' @param fill Vetor de cores a ser usado no preenchimento do diagrama. Deve ter o mesmo tamanho da lista com os grupos.
#' @param color Vetor de cores a ser usado no contornos do diagrama. Deve ter o mesmo tamanho da lista com os grupos.
#' @param alp Transparencia do grafico
#' @param heig Altura da figura gerada (Em pixels)
#' @param widt Largura da figura gerada (Em pixels)
#' @param title Titulo do grafico
#' @param cat_names Vetor de nomes a serem atribuidos aos grupos no diagrama
#' @param cat_font_size Tamanho da fonte do nome dos grupos
#' @param var_font_size Tamanho da fonte do nome dos membros dos grupos
#'
#' @return Um plot contendo o diagrama de Venn na forma de objeto de figura do R
#' @export
#'
#'
#' @examples
#'  x <- list()
#'  x$Male <- c("1-Methyl-Histidine","L-Lactic acid","D-Glucose","VLDL & LDL")
#'  x$Female <- c("1-Methyl-Histidine","L-Glutamic acid","Acetone","D-Glucose")
#'  x$Adults <- c("L-Lactic acid","VLDL & LDL","1-Methyl-Histidine","Acetone","Acetoacetic acid")
#'  x$Elderly <- c("D-Glucose","1-Methyl-Histidine","L-Glutamic acid","Acetone")
#'
#'  graph1 <- Venn_diagram(x)
#'
Venn_diagram <-  function(x,fill= RColorBrewer::brewer.pal(length(x),"Dark2"),color=RColorBrewer::brewer.pal(length(x),"Dark2"),alp =0.5,heig = 9000,widt = 9000,title = NULL,cat_names = names(x),cat_font_size=1,var_font_size=1)
{


  v0 <- VennDiagram::venn.diagram(x, height=heig, width=widt,
                    col = color[1:length(x)],
                    fill = fill[1:length(x)],
                    alpha = alp, filename = NULL, main = title,category.names = cat_names,cat.cex = cat_font_size, cex = var_font_size,disable.logging=T)

  overlaps <- VennDiagram::calculate.overlap(x)
  overlaps <- rev(overlaps)


  posOverlap = as.numeric (gsub ("a","", (names (overlaps))))
  for (i in 1:length(overlaps)){
    pos = posOverlap [i]
    v0[[pos+length(x)*2]]$label <- paste(overlaps[[i]], collapse = "\n")
  }
  grid::grid.draw(v0)
  return(v0)
}

