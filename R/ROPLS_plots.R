

plot_ropls_scores <- function(ropls_object){

  if(ropls_object@typeC=="OPLS-DA"){
    comp1_legend <- paste("t1 ","(",OPLS_test@modelDF[["R2X"]][1],")",sep = "")
    comp2_legend <- paste("to1 ","(",OPLS_test@modelDF[["R2X"]][2],")",sep = "")
  }else{
    comp1_legend <- paste("Comp1 ","(",OPLS_test@modelDF[["R2X"]][1],")",sep = "")
    comp2_legend <- paste("Comp2 ","(",OPLS_test@modelDF[["R2X"]][2],")",sep = "")
  }

extrated_opls_data <- extract_ropls_data(ropls_object)

  if (ropls_object@typeC=="OPLS-DA"){
  mean_scores <- dplyr::group_by(extrated_opls_data$Scores,Group)%>%
   dplyr::summarise(p1=mean(as.numeric(p1)),p2=mean(as.numeric(o1)))

  pls_scores_plot <- ggplot2::ggplot(extrated_opls_data$Scores,ggplot2::aes(p1,o1,color=Group))+
  ggplot2::geom_point(size=4)+
  ggplot2::theme_bw()+ggplot2::labs(title = ropls_object@typeC,x=comp1_legend,y=comp2_legend)

  }else{
  mean_scores <- dplyr::group_by(extrated_opls_data$Scores,Group)%>%
    dplyr::summarise(p1=mean(as.numeric(p1)),p2=mean(as.numeric(p2)))

  pls_scores_plot <- ggplot2::ggplot(extrated_opls_data$Scores,ggplot2::aes(p1,p2,color=Group))+
    ggplot2::geom_point(size=4)+
    ggplot2::theme_bw()+ggplot2::labs(title = ropls_object@typeC,x=comp1_legend,y=comp2_legend)
  }

  return(pls_scores_plot)
}



#
# stat_ellipse(fill=rgb(0,0,0,alpha = 0),geom="polygon")
# geom_point(mean_scores,mapping = aes(p1,p2),size=8,shape=17)
#
# PLS_sp_pos <- extract_ropls_data(PLS_sp_pos)
#
# PLS_sp_pos$Loadings$metabolites <- rownames(PLS_sp_pos$Loadings)
#
# pls_loading_plot <- ggplot(PLS_sp_pos$Loadings,aes(p1,p2,color=Vip,label=metabolites))+
#   geom_text_repel(seed = 123,fontface="bold",size=5,force_pull = 0.8)+geom_point(color="black",size=3)+
#   scale_color_gradient(high = "red",low = "darkgray")+xlim(-0.3,0.3)+theme_bw()
